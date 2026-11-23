use super::{InferredType as Type, *};
use crate::symbols::{Aggregate, TypeDef, TypeSchema};
use crate::types::TypeInterner;

fn hierarchy(interner: &TypeInterner) -> Symbols<'_> {
    let mut symbols = Symbols::with_default_types();
    for (name, base) in [
        ("Base", None),
        ("Middle", Some("Base")),
        ("Leaf", Some("Middle")),
    ] {
        let mut aggregate = Aggregate::default();
        aggregate.set_base(base.map(|name| TypeApp::nullary(interner.intern(name))));
        symbols.add_type(
            interner.intern(name),
            TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
        );
    }
    symbols
}

#[test]
fn applicability_respects_directional_and_partial_bounds_without_selecting() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let types =
        ["Leaf", "Middle", "Base"].map(|name| crate::types::Type::nullary(interner.intern(name)));
    for (lower, upper) in [
        (Some(0), Some(0)),
        (Some(1), Some(1)),
        (Some(0), Some(2)),
        (Some(0), None),
        (None, Some(1)),
        (None, None),
    ] {
        let lower_type = lower.map_or(InferredType::Nothing, |i| (&types[i]).into());
        let upper_type = upper.map(|i| (&types[i]).into());
        let inferred = PolyType::with_bounds(lower_type.clone(), upper_type.clone());
        for (i, candidate) in types.iter().enumerate() {
            for variance in [
                Variance::Covariant,
                Variance::Contravariant,
                Variance::Invariant,
            ] {
                let above_lower = lower.is_none_or(|lower| lower <= i);
                let below_upper = upper.is_none_or(|upper| i <= upper);
                let expected = match variance {
                    Variance::Covariant => above_lower,
                    Variance::Contravariant => below_upper,
                    Variance::Invariant => above_lower && below_upper,
                };
                assert_eq!(
                    inferred.is_subtype_compatible(candidate, variance, &symbols),
                    expected,
                    "{lower:?}..{upper:?}, {candidate}, {variance:?}"
                );
            }
        }
        let PolyType::Var(var) = &inferred else {
            unreachable!()
        };
        assert_eq!(var.lower(), lower_type);
        assert_eq!(var.upper(), upper_type);
        // Eligibility must not select an endpoint or add speculative bounds.
        inferred
            .constrain_invariant(
                &PolyType::from_type(&types[lower.unwrap_or(upper.unwrap_or(2))]),
                &symbols,
            )
            .unwrap();
    }
}

#[test]
fn applicability_composes_nested_variance_and_preserves_unknowns() {
    let interner = TypeInterner::default();
    let mut symbols = hierarchy(&interner);
    let leaf = crate::types::Type::nullary(interner.intern("Leaf"));
    let base = crate::types::Type::nullary(interner.intern("Base"));
    let fixed = PolyType::with_bounds((&leaf).into(), Some((&leaf).into()));
    for variance in [
        Variance::Covariant,
        Variance::Contravariant,
        Variance::Invariant,
    ] {
        let id = interner.intern(match variance {
            Variance::Covariant => "Producer",
            Variance::Contravariant => "Consumer",
            Variance::Invariant => "Cell",
        });
        symbols.add_type(
            id,
            TypeDef::new(
                [Shared::new(TypeParamVar::new("T", variance, None, None))],
                TypeSchema::Aggregate(Aggregate::default().into()),
                [],
            ),
        );
        let inferred = Type::app(id, [fixed.clone()]).into_poly();
        assert_eq!(
            inferred.is_subtype_compatible(
                &crate::types::Type::app(id, [base.clone()]),
                Variance::Covariant,
                &symbols,
            ),
            variance == Variance::Covariant
        );
        assert!(inferred.is_subtype_compatible(
            &crate::types::Type::app(id, [leaf.clone()]),
            Variance::Covariant,
            &symbols,
        ));
    }
    for unknown in [PolyType::fresh(), InferredType::Nothing.into_poly()] {
        for variance in [
            Variance::Covariant,
            Variance::Contravariant,
            Variance::Invariant,
        ] {
            assert!(unknown.is_subtype_compatible(&base, variance, &symbols));
            assert!(unknown.is_subtype_compatible(&leaf, variance, &symbols));
        }
    }
}

#[test]
fn invariant_constraints_reject_later_widening_and_narrowing() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let middle = PolyType::nullary(interner.intern("Middle"));
    fn wrap<'ctx>(mut typ: PolyType<'ctx>, depth: usize) -> PolyType<'ctx> {
        for _ in 0..depth {
            typ = Type::app(predef::ARRAY, [typ]).into_poly();
        }
        typ
    }
    for depth in [0, 2] {
        let concrete = wrap(middle.clone(), depth);
        for mono_first in [true, false] {
            for (name, widen) in [("Base", true), ("Leaf", false)] {
                let var = PolyType::fresh();
                let inferred = wrap(var.clone(), depth);
                let (lhs, rhs) = if mono_first {
                    (&concrete, &inferred)
                } else {
                    (&inferred, &concrete)
                };
                if depth == 0 {
                    lhs.constrain_invariant(rhs, &symbols).unwrap();
                } else {
                    lhs.constrain_base(rhs, &symbols).unwrap();
                }
                let incompatible = PolyType::nullary(interner.intern(name));
                let result = if widen {
                    incompatible.constrain::<ClassSupertype>(&var, &symbols)
                } else {
                    var.constrain::<ClassSupertype>(&incompatible, &symbols)
                };
                assert!(
                    result.is_err(),
                    "depth={depth}, mono_first={mono_first}, {name}"
                );
            }
        }
    }
}

#[test]
fn invariant_mixed_constraints_accept_compatible_bounds() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let base = PolyType::nullary(interner.intern("Base"));
    let middle = PolyType::nullary(interner.intern("Middle"));
    let leaf = PolyType::nullary(interner.intern("Leaf"));

    for mono_first in [true, false] {
        let var = PolyType::fresh();
        leaf.constrain::<ClassSupertype>(&var, &symbols).unwrap();
        var.constrain::<ClassSupertype>(&base, &symbols).unwrap();
        let (lhs, rhs) = if mono_first {
            (&middle, &var)
        } else {
            (&var, &middle)
        };
        lhs.constrain_invariant(rhs, &symbols).unwrap();

        middle.constrain::<ClassSupertype>(&var, &symbols).unwrap();
        var.constrain::<ClassSupertype>(&middle, &symbols).unwrap();
        leaf.constrain::<ClassSupertype>(&var, &symbols).unwrap();
        var.constrain::<ClassSupertype>(&base, &symbols).unwrap();
        rhs.constrain_invariant(lhs, &symbols).unwrap();
    }
}

#[test]
fn rigid_bounds_follow_lower_bounds_without_equating_upper_bounds() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let base = Type::nullary(interner.intern("Base"));
    let leaf = PolyType::nullary(interner.intern("Leaf"));
    let with_lower = Type::Ctx(Shared::new(TypeParamVar::new(
        "T",
        Variance::Invariant,
        Some(base.clone()),
        None,
    )))
    .into_poly();
    assert!(leaf.satisfies_bound(&with_lower, &symbols));

    let first = Type::Ctx(Shared::new(TypeParamVar::new(
        "T",
        Variance::Invariant,
        None,
        Some(base.clone()),
    )))
    .into_poly();
    let second = Type::Ctx(Shared::new(TypeParamVar::new(
        "U",
        Variance::Invariant,
        None,
        Some(base),
    )))
    .into_poly();
    assert!(!first.satisfies_bound(&second, &symbols));
    assert!(!second.satisfies_bound(&first, &symbols));
}

#[test]
fn rejected_constraints_clear_guards_and_remove_new_edges() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let base = Type::nullary(interner.intern("Base"));
    let leaf = Type::nullary(interner.intern("Leaf"));
    for lower in [false, true] {
        let var = Var::new(leaf.clone(), Some(leaf.clone()));
        let incompatible = if lower { &base } else { &Type::Nothing };
        let mut seen = SmallVec::new();
        assert!(
            var.update(incompatible, lower, &symbols, &mut seen)
                .is_err()
        );
        assert!(seen.is_empty());
        assert!(!var.0.borrow().updating);
        var.select(&leaf, &symbols).unwrap();
    }

    let lhs = Var::new(base.clone(), None);
    let rhs = Var::new(Type::Nothing, Some(leaf.clone()));
    let weak = [Shared::downgrade(&lhs.0), Shared::downgrade(&rhs.0)];
    assert!(lhs.subtype(&rhs, &symbols).is_err());
    assert!(lhs.0.borrow().supers.is_empty());
    assert!(rhs.0.borrow().subs.is_empty());
    assert!(!lhs.0.borrow().updating);
    assert!(!rhs.0.borrow().updating);
    lhs.select(&base, &symbols).unwrap();
    rhs.select(&leaf, &symbols).unwrap();
    drop((lhs, rhs));
    assert!(weak.iter().all(|var| var.upgrade().is_none()));
}

#[test]
fn directional_edges_propagate_before_and_after_linking() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let base = PolyType::nullary(interner.intern("Base"));
    let leaf = PolyType::nullary(interner.intern("Leaf"));
    for link_first in [false, true] {
        for reverse in [false, true] {
            let a = PolyType::fresh();
            let b = PolyType::fresh();
            let c = PolyType::fresh();
            let link = || {
                if reverse {
                    b.constrain_base(&c, &symbols).unwrap();
                    a.constrain_base(&b, &symbols).unwrap();
                } else {
                    a.constrain_base(&b, &symbols).unwrap();
                    b.constrain_base(&c, &symbols).unwrap();
                }
            };
            if link_first {
                link();
            }
            leaf.constrain_base(&a, &symbols).unwrap();
            c.constrain_base(&base, &symbols).unwrap();
            base.constrain_base(&c, &symbols).unwrap();
            if !link_first {
                link();
            }
            for var in [&a, &b] {
                assert_eq!(
                    var.coalesce(&symbols).unwrap(),
                    leaf.coalesce(&symbols).unwrap()
                );
            }
            assert_eq!(
                c.coalesce(&symbols).unwrap(),
                base.coalesce(&symbols).unwrap()
            );
        }
    }
}

#[test]
fn exact_variables_allow_directional_subtype_constraints() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let base = PolyType::nullary(interner.intern("Base"));
    let leaf = PolyType::nullary(interner.intern("Leaf"));
    for exact_lhs in [true, false] {
        let lhs = PolyType::fresh();
        let rhs = PolyType::fresh();
        if exact_lhs {
            lhs.constrain_invariant(&leaf, &symbols).unwrap();
            base.constrain_base(&rhs, &symbols).unwrap();
        } else {
            leaf.constrain_base(&lhs, &symbols).unwrap();
            rhs.constrain_invariant(&base, &symbols).unwrap();
        }
        lhs.constrain_base(&rhs, &symbols).unwrap();
        assert_eq!(
            lhs.coalesce(&symbols).unwrap(),
            crate::types::Type::nullary(interner.intern("Leaf"))
        );
        assert_eq!(
            rhs.coalesce(&symbols).unwrap(),
            crate::types::Type::nullary(interner.intern("Base"))
        );
    }
}

#[test]
fn directional_cycles_merge_only_the_strong_component() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for reverse in [false, true] {
        let a = PolyType::fresh();
        let b = PolyType::fresh();
        let c = PolyType::fresh();
        let outside = PolyType::fresh();
        let base = PolyType::nullary(interner.intern("Base"));
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        a.constrain_base(&b, &symbols).unwrap();
        b.constrain_base(&c, &symbols).unwrap();
        b.constrain_base(&outside, &symbols).unwrap();
        base.constrain_base(&outside, &symbols).unwrap();
        if reverse {
            leaf.constrain_base(&a, &symbols).unwrap();
        }
        c.constrain_base(&a, &symbols).unwrap();
        if !reverse {
            leaf.constrain_base(&a, &symbols).unwrap();
        }
        for var in [&a, &b, &c] {
            assert_eq!(
                var.coalesce(&symbols).unwrap(),
                crate::types::Type::nullary(interner.intern("Leaf"))
            );
        }
        assert_eq!(
            outside.coalesce(&symbols).unwrap(),
            crate::types::Type::nullary(interner.intern("Base"))
        );
    }
}

fn coalesce_var<'ctx>(var: &Var<'ctx>, symbols: &Symbols<'ctx>) -> crate::types::Type<'ctx> {
    PolyType::Var(var.clone()).coalesce(symbols).unwrap()
}

fn link_mixed_component<'ctx>(
    [x, y, z]: &[Var<'ctx>; 3],
    insertion: [usize; 3],
    symbols: &Symbols<'ctx>,
) {
    for step in insertion {
        match step {
            0 => x.subtype(z, symbols),
            1 => z.add_lower(
                &Type::app(predef::ARRAY, [PolyType::Var(y.clone())]),
                symbols,
            ),
            _ => y.add_lower(
                &Type::app(predef::ARRAY, [PolyType::Var(x.clone())]),
                symbols,
            ),
        }
        .unwrap_or_else(|err| panic!("{insertion:?}: {err:?}"));
    }
}

fn permutations<const N: usize>() -> Vec<[usize; N]> {
    fn visit<const N: usize>(values: &mut [usize; N], start: usize, result: &mut Vec<[usize; N]>) {
        if start == N {
            result.push(*values);
            return;
        }
        for i in start..N {
            values.swap(start, i);
            visit(values, start + 1, result);
            values.swap(start, i);
        }
    }
    let mut result = Vec::new();
    visit(&mut std::array::from_fn(|i| i), 0, &mut result);
    result
}

#[test]
fn nested_predecessors_remain_open_to_late_lower_bounds() {
    let interner = TypeInterner::default();
    let mut symbols = hierarchy(&interner);
    let p = interner.intern("P");
    let mut aggregate = Aggregate::default();
    aggregate.set_base(Some(TypeApp::nullary(predef::ISCRIPTABLE)));
    symbols.add_type(
        p,
        TypeDef::new(
            [Shared::new(TypeParamVar::new(
                "T",
                Variance::Covariant,
                None,
                None,
            ))],
            TypeSchema::Aggregate(aggregate.into()),
            [],
        ),
    );
    // The class hierarchy helper does not implicitly add IScriptable.
    let dog = interner.intern("Dog");
    let mut aggregate = Aggregate::default();
    aggregate.set_base(Some(TypeApp::nullary(predef::ISCRIPTABLE)));
    symbols.add_type(
        dog,
        TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
    );
    for insertion in permutations::<4>() {
        for solve in permutations::<2>() {
            let x = Var::fresh();
            let y = Var::fresh();
            let weak = [Shared::downgrade(&x.0), Shared::downgrade(&y.0)];
            for step in insertion {
                match step {
                    0 => x.subtype(&y, &symbols),
                    1 => y.add_upper(&Type::nullary(predef::ISCRIPTABLE), &symbols),
                    2 => y.add_lower(&Type::app(p, [PolyType::Var(x.clone())]), &symbols),
                    _ => x.add_lower(&Type::nullary(dog), &symbols),
                }
                .unwrap_or_else(|err| panic!("{insertion:?}: {err:?}"));
            }
            assert!(
                x.upper() != Some(Type::Nothing),
                "insertion must not select Nothing"
            );
            for index in solve {
                let (var, expected) = if index == 0 {
                    (&x, dog)
                } else {
                    (&y, predef::ISCRIPTABLE)
                };
                assert_eq!(
                    coalesce_var(var, &symbols),
                    crate::types::Type::nullary(expected),
                    "{insertion:?} {solve:?}"
                );
            }
            drop((x, y));
            assert!(weak.iter().all(|v| v.upgrade().is_none()));
        }
    }
}

#[test]
fn coalescing_rechecks_bounds_tightened_by_nested_dependencies() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for insertion in permutations::<2>() {
        for solve in permutations::<2>() {
            let x = Var::fresh();
            let y = Var::fresh();
            let weak = [Shared::downgrade(&x.0), Shared::downgrade(&y.0)];
            for step in insertion {
                match step {
                    0 => x.subtype(&y, &symbols),
                    _ => x.add_upper(
                        &Type::app(predef::ARRAY, [PolyType::Var(y.clone())]),
                        &symbols,
                    ),
                }
                .unwrap();
            }
            let vars = [&x, &y];
            for index in solve {
                // The first attempt must succeed, not merely repair bounds
                // enough for a subsequent call to succeed.
                assert_eq!(
                    coalesce_var(vars[index], &symbols),
                    crate::types::Type::Nothing,
                    "{insertion:?} {solve:?}"
                );
            }
            for var in vars {
                assert_eq!(var.lower(), Type::Nothing);
                assert_eq!(var.upper(), Some(Type::Nothing));
            }
            drop((x, y));
            assert!(weak.iter().all(|var| var.upgrade().is_none()));
        }
    }
}

#[test]
fn mixed_component_coalescing_is_independent_of_root_and_insertion_order() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for insertion in permutations::<3>() {
        for solve in permutations::<3>() {
            let vars = [Var::fresh(), Var::fresh(), Var::fresh()];
            let weak = vars.each_ref().map(|var| Shared::downgrade(&var.0));
            link_mixed_component(&vars, insertion, &symbols);
            let expected = [
                crate::types::Type::Nothing,
                crate::types::Type::app(predef::ARRAY, [crate::types::Type::Nothing]),
                crate::types::Type::app(
                    predef::ARRAY,
                    [crate::types::Type::app(
                        predef::ARRAY,
                        [crate::types::Type::Nothing],
                    )],
                ),
            ];
            for index in solve {
                assert_eq!(
                    coalesce_var(&vars[index], &symbols),
                    expected[index],
                    "{insertion:?} {solve:?}"
                );
            }
            drop(vars);
            assert!(weak.iter().all(|v| v.upgrade().is_none()));
        }
    }
}

#[test]
fn exported_nested_bounds_retain_their_inference_component() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let x = Var::fresh();
    let y = Var::fresh();
    let weak = [Shared::downgrade(&x.0), Shared::downgrade(&y.0)];
    x.subtype(&y, &symbols).unwrap();
    y.add_lower(
        &Type::app(predef::ARRAY, [PolyType::Var(x.clone())]),
        &symbols,
    )
    .unwrap();
    let bound = y.lower();
    drop((x, y));
    assert!(weak.iter().all(|var| var.upgrade().is_some()));
    assert_eq!(
        bound.into_poly().coalesce(&symbols).unwrap(),
        crate::types::Type::app(predef::ARRAY, [crate::types::Type::Nothing])
    );
    assert!(weak.iter().all(|var| var.upgrade().is_none()));
}

#[test]
fn mixed_component_survives_any_remaining_external_handle() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for survivor in 0..3 {
        let vars = [Var::fresh(), Var::fresh(), Var::fresh()];
        let weak = vars.each_ref().map(|var| Shared::downgrade(&var.0));
        link_mixed_component(&vars, [0, 1, 2], &symbols);
        let remaining = vars[survivor].clone();
        drop(vars);
        assert!(weak.iter().all(|var| var.upgrade().is_some()));
        assert!(PolyType::Var(remaining.clone()).coalesce(&symbols).is_ok());
        drop(remaining);
        assert!(weak.iter().all(|var| var.upgrade().is_none()));
    }
}

#[test]
fn mixed_directional_nested_cycles_have_finite_solutions_and_drop() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for nodes in [2, 3] {
        for edge_first in [false, true] {
            for finish in [false, true] {
                let vars: Vec<_> = (0..nodes).map(|_| Var::fresh()).collect();
                let weak: Vec<_> = vars.iter().map(|var| Shared::downgrade(&var.0)).collect();
                let x = &vars[0];
                let y = &vars[nodes - 1];
                let link = || {
                    for edge in vars.windows(2) {
                        edge[0].subtype(&edge[1], &symbols).unwrap();
                    }
                };
                if edge_first {
                    link();
                }
                let array_x = Type::app(predef::ARRAY, [PolyType::Var(x.clone())]);
                y.add_lower(&array_x, &symbols).unwrap();
                if !edge_first {
                    link();
                }
                if finish {
                    let array_nothing = Type::app(predef::ARRAY, [Type::Nothing.into_poly()]);
                    y.add_upper(&array_nothing, &symbols).unwrap();
                    assert_eq!(
                        coalesce_var(y, &symbols),
                        crate::types::Type::app(predef::ARRAY, [crate::types::Type::Nothing]),
                        "nodes={nodes}, edge_first={edge_first}"
                    );
                    assert_eq!(coalesce_var(x, &symbols), crate::types::Type::Nothing);
                }
                drop((array_x, vars));
                assert!(weak.iter().all(|var| var.upgrade().is_none()));
            }
        }
    }
}

#[test]
fn coalescing_guard_rejects_recursive_bounds_without_recursing() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let var = Var::fresh();
    // Simulate a malformed bound bypassing constraint insertion, so the
    // coalescing guard remains independently tested from the occurs check.
    var.0.borrow_mut().lower = Type::app(predef::ARRAY, [PolyType::Var(var.clone())]);
    let result = PolyType::Var(var.clone()).coalesce(&symbols);
    var.0.borrow_mut().lower = Type::Nothing;
    assert!(result.is_err());
    assert!(!var.0.borrow().updating);
    assert_eq!(
        PolyType::Var(var).coalesce(&symbols).unwrap(),
        crate::types::Type::Nothing
    );
}

#[test]
fn collapsing_cycles_preserves_external_predecessors() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for reverse in [false, true] {
        let x = Var::fresh();
        let a = Var::fresh();
        let b = Var::fresh();
        x.subtype(&b, &symbols).unwrap();
        a.subtype(&b, &symbols).unwrap();
        b.subtype(&a, &symbols).unwrap();
        let leaf = Type::nullary(interner.intern("Leaf"));
        if reverse {
            b.add_upper(&leaf, &symbols).unwrap();
        } else {
            a.add_upper(&leaf, &symbols).unwrap();
        }
        assert_eq!(x.upper(), Some(leaf.clone()));
        let later = Var::fresh();
        later.subtype(&b, &symbols).unwrap();
        assert_eq!(later.upper(), Some(leaf));
    }
}

#[test]
fn branching_cycles_propagate_and_release_the_component() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    for branches in [2, 3] {
        let a = Var::fresh();
        let d = Var::fresh();
        let middle: Vec<_> = (0..branches).map(|_| Var::fresh()).collect();
        let weak: Vec<_> = [&a, &d]
            .into_iter()
            .chain(&middle)
            .map(|var| Shared::downgrade(&var.0))
            .collect();
        for var in &middle {
            a.subtype(var, &symbols).unwrap();
            var.subtype(&d, &symbols).unwrap();
        }
        assert_eq!(a.0.borrow().supers.spilled(), branches > 2);
        assert_eq!(d.0.borrow().subs.spilled(), branches > 2);
        d.subtype(&a, &symbols).unwrap();
        let leaf = Type::nullary(interner.intern("Leaf"));
        middle[1].add_lower(&leaf, &symbols).unwrap();
        for var in [&a, &d].into_iter().chain(&middle) {
            assert_eq!(var.lower(), leaf);
            assert_eq!(var.key(), a.key());
        }
        drop((a, d, middle));
        assert!(weak.iter().all(|v| v.upgrade().is_none()));
    }
}

#[test]
fn rigid_lattice_bounds_preserve_identity_and_reject_unknown_meets() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let base = Type::nullary(interner.intern("Base"));
    let leaf = Type::nullary(interner.intern("Leaf"));
    let rigid = Type::Ctx(Shared::new(TypeParamVar::new(
        "T",
        Variance::Invariant,
        None,
        Some(base.clone()),
    )));
    for (l, r) in [(&rigid, &base), (&base, &rigid)] {
        assert_eq!(l.glb(r, &symbols).unwrap(), rigid);
        assert_eq!(l.lub(r, &symbols).unwrap(), base);
    }
    for (l, r) in [(&rigid, &leaf), (&leaf, &rigid)] {
        assert!(l.glb(r, &symbols).is_err());
        assert_eq!(l.lub(r, &symbols).unwrap(), base);
    }
    assert_eq!(rigid.lub(&rigid, &symbols).unwrap(), rigid);
    assert_eq!(rigid.glb(&rigid, &symbols).unwrap(), rigid);
    let lower_bounded = Type::Ctx(Shared::new(TypeParamVar::new(
        "U",
        Variance::Invariant,
        Some(base.clone()),
        None,
    )));
    for (l, r) in [(&lower_bounded, &leaf), (&leaf, &lower_bounded)] {
        assert_eq!(l.lub(r, &symbols).unwrap(), lower_bounded);
        assert_eq!(l.glb(r, &symbols).unwrap(), leaf);
    }
    let unbounded = Type::Ctx(Shared::new(TypeParamVar::new(
        "V",
        Variance::Invariant,
        None,
        None,
    )));
    assert!(unbounded.lub(&leaf, &symbols).is_err());
    assert!(leaf.lub(&unbounded, &symbols).is_err());
}

#[test]
fn application_lattice_operations_respect_all_variances() {
    let interner = TypeInterner::default();
    let mut symbols = hierarchy(&interner);
    let base = PolyType::nullary(interner.intern("Base"));
    let leaf = PolyType::nullary(interner.intern("Leaf"));
    for variance in [
        Variance::Covariant,
        Variance::Contravariant,
        Variance::Invariant,
    ] {
        let id = interner.intern(match variance {
            Variance::Covariant => "Producer",
            Variance::Contravariant => "Consumer",
            Variance::Invariant => "Cell",
        });
        symbols.add_type(
            id,
            TypeDef::new(
                [Shared::new(TypeParamVar::new("T", variance, None, None))],
                TypeSchema::Aggregate(Aggregate::default().into()),
                [],
            ),
        );
        let l = Type::app(id, [leaf.clone()]);
        let r = Type::app(id, [base.clone()]);
        for (l, r) in [(&l, &r), (&r, &l)] {
            if variance == Variance::Invariant {
                assert!(l.lub(r, &symbols).is_err());
                assert!(l.glb(r, &symbols).is_err());
            } else {
                let (joined, met) = if variance == Variance::Covariant {
                    (&base, &leaf)
                } else {
                    (&leaf, &base)
                };
                assert_eq!(l.lub(r, &symbols).unwrap(), Type::app(id, [joined.clone()]));
                assert_eq!(l.glb(r, &symbols).unwrap(), Type::app(id, [met.clone()]));
            }
        }
        let a = PolyType::fresh();
        let b = PolyType::fresh();
        let l = Type::app(id, [a.clone()]);
        let r = Type::app(id, [b.clone()]);
        let joined = l.lub(&r, &symbols).unwrap();
        a.constrain_invariant(&leaf, &symbols).unwrap();
        if variance == Variance::Invariant {
            assert!(b.constrain_invariant(&base, &symbols).is_err());
        } else {
            b.constrain_invariant(&base, &symbols).unwrap();
            assert_eq!(
                joined.into_poly().coalesce(&symbols).unwrap(),
                crate::types::Type::app(
                    id,
                    [crate::types::Type::nullary(interner.intern(
                        if variance == Variance::Covariant {
                            "Base"
                        } else {
                            "Leaf"
                        }
                    ))]
                )
            );
        }
    }
}

#[test]
fn inherited_application_meet_refines_arguments() {
    let interner = TypeInterner::default();
    let mut symbols = hierarchy(&interner);
    let parent = interner.intern("Parent");
    let child = interner.intern("Child");
    let param = Shared::new(TypeParamVar::new("T", Variance::Covariant, None, None));
    symbols.add_type(
        parent,
        TypeDef::new(
            [param.clone()],
            TypeSchema::Aggregate(Aggregate::default().into()),
            [],
        ),
    );
    let mut aggregate = Aggregate::default();
    aggregate.set_base(Some(TypeApp::new(
        parent,
        [crate::types::Type::Ctx(param.clone())],
    )));
    symbols.add_type(
        child,
        TypeDef::new([param], TypeSchema::Aggregate(aggregate.into()), []),
    );
    let lhs = Type::app(child, [PolyType::nullary(interner.intern("Base"))]);
    let rhs = Type::app(parent, [PolyType::nullary(interner.intern("Leaf"))]);
    for (l, r) in [(&lhs, &rhs), (&rhs, &lhs)] {
        assert_eq!(
            l.glb(r, &symbols)
                .unwrap()
                .into_poly()
                .coalesce(&symbols)
                .unwrap(),
            crate::types::Type::app(
                child,
                [crate::types::Type::nullary(interner.intern("Leaf"))]
            )
        );
    }
}

#[test]
fn coalescing_validates_preexisting_bounds() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let invalid = PolyType::with_bounds(
        Type::nullary(interner.intern("Base")),
        Some(Type::nullary(interner.intern("Leaf"))),
    );
    assert!(invalid.coalesce(&symbols).is_err());
}

#[test]
fn variable_lattice_operations_do_not_equate_operands() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let leaf = PolyType::nullary(interner.intern("Leaf"));
    let base = PolyType::nullary(interner.intern("Base"));
    for join in [false, true] {
        for reverse in [false, true] {
            let a = PolyType::fresh();
            let b = PolyType::fresh();
            a.constrain_invariant(&leaf, &symbols).unwrap();
            b.constrain_invariant(&base, &symbols).unwrap();
            let (l, r) = if reverse { (&b, &a) } else { (&a, &b) };
            let result = if join {
                l.lub(r, &symbols)
            } else {
                l.glb(r, &symbols)
            }
            .unwrap();
            assert_eq!(
                result.coalesce(&symbols).unwrap(),
                crate::types::Type::nullary(interner.intern(if join { "Base" } else { "Leaf" }))
            );
            assert_eq!(
                a.coalesce(&symbols).unwrap(),
                crate::types::Type::nullary(interner.intern("Leaf"))
            );
            assert_eq!(
                b.coalesce(&symbols).unwrap(),
                crate::types::Type::nullary(interner.intern("Base"))
            );
        }
    }
}

#[test]
fn nested_edges_retain_inference_and_reject_occurs_cycles() {
    let interner = TypeInterner::default();
    let symbols = hierarchy(&interner);
    let leaf = PolyType::nullary(interner.intern("Leaf"));
    let base = PolyType::nullary(interner.intern("Base"));
    for reverse in [false, true] {
        let a = PolyType::fresh();
        let b = PolyType::fresh();
        let x = PolyType::fresh();
        let y = PolyType::fresh();
        let array = |arg| Type::app(predef::ARRAY, [arg]).into_poly();
        a.constrain_base(&b, &symbols).unwrap();
        if reverse {
            b.constrain_base(&array(y.clone()), &symbols).unwrap();
            array(x.clone()).constrain_base(&a, &symbols).unwrap();
        } else {
            array(x.clone()).constrain_base(&a, &symbols).unwrap();
            b.constrain_base(&array(y.clone()), &symbols).unwrap();
        }
        x.constrain_invariant(&leaf, &symbols).unwrap();
        assert_eq!(
            y.coalesce(&symbols).unwrap(),
            crate::types::Type::nullary(interner.intern("Leaf"))
        );
        assert!(base.constrain_base(&x, &symbols).is_err());
    }
    let recursive = PolyType::fresh();
    assert!(
        Type::app(predef::ARRAY, [recursive.clone()])
            .into_poly()
            .constrain_base(&recursive, &symbols)
            .is_err()
    );
}
