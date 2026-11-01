//! REDmod rejects calling a non-`const` method on a `const` parameter ("Cannot call non-const,
//! non-static function 'M' using const scope"), which redscript 0.5 accepts, e.g. in a wrapper
//! of a game method declared `const stateContext : StateContext`:
//! `stateContext.SetTemporaryScriptableParameter(..)`.
//!
//! REDmod accepts the call on a local holding the same object, so such calls go through one:
//! `let stateContextMut: StateContext = stateContext;` at the top of the function, and
//! `stateContextMut.SetTemporaryScriptableParameter(..)`. The parameter refers to an object, so
//! the local is the same object. Only with type information, which says which methods are not
//! `const`. Parameters assigned to or declared again in the function are left alone.

use hashbrown::{HashMap, HashSet};
use redscript_ast::{Expr, FunctionBody, ParamQualifiers, SourceModule, Span, Stmt};

use crate::index::{LocalTypes, Names};
use crate::walk::{
    MutVisitor, SExpr, SStmt, SType, for_each_function, ident, let_stmt, strip_ref, synth,
    type_name, walk_body, walk_expr,
};
use crate::{Cx, TypeQuery};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let Some(types) = cx.opts.types else {
        return;
    };
    for_each_function(module, |_, func| {
        let locals = LocalTypes::of(func);
        let params: HashMap<&'src str, (&'src str, SType<'src>)> = func
            .params
            .iter()
            .filter(|(param, _)| param.qualifiers.contains(ParamQualifiers::CONST))
            .filter(|(param, _)| locals.declarations(param.name) == 1)
            .filter_map(|(param, _)| {
                let typ = param.typ.as_ref()?;
                let class = type_name(strip_ref(&typ.0))?;
                Some((param.name, (class, typ.clone())))
            })
            .collect();
        if params.is_empty() {
            return;
        }
        let names = Names::of(func, cx.arena);
        let Some(body @ FunctionBody::Block(_)) = &mut func.body else {
            return;
        };
        let mut assigned = Assigned(HashSet::new());
        walk_body(&mut assigned, body);
        let mut rebinder = Rebinder {
            types,
            cx: &mut *cx,
            params: params
                .into_iter()
                .filter(|(name, _)| !assigned.0.contains(name))
                .collect(),
            names,
            locals: vec![],
        };
        walk_body(&mut rebinder, body);
        if let FunctionBody::Block(block) = body
            && !rebinder.locals.is_empty()
        {
            let mut stmts = rebinder.locals;
            stmts.extend(std::mem::take(&mut block.stmts).into_vec());
            block.stmts = stmts.into();
        }
    });
}

/// Collects the variables assigned to as a whole.
struct Assigned<'src>(HashSet<&'src str>);

impl<'src> MutVisitor<'src> for Assigned<'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        if let Expr::Assign { lhs, .. } = &expr.0
            && let Expr::Ident(name) = lhs.0
        {
            self.0.insert(name);
        }
        walk_expr(self, expr);
    }
}

struct Rebinder<'a, 'b, 'src> {
    types: &'a dyn TypeQuery,
    cx: &'a mut Cx<'b, 'src>,
    /// The `const` parameters of the function by name, with their class and declared type.
    params: HashMap<&'src str, (&'src str, SType<'src>)>,
    names: Names<'src>,
    /// The declarations of the locals standing for the parameters, in order of first use.
    locals: Vec<SStmt<'src>>,
}

impl<'src> Rebinder<'_, '_, 'src> {
    /// The local standing for the parameter `param`, declared on first use.
    fn local_of(&mut self, param: &'src str, typ: &SType<'src>, span: Span) -> &'src str {
        for (stmt, _) in &self.locals {
            if let Stmt::Let {
                name: (name, _),
                value: Some(value),
                ..
            } = stmt
                && matches!(value.0, Expr::Ident(p) if p == param)
            {
                return name;
            }
        }
        let local = self.names.fresh(&format!("{param}Mut"));
        let syn = synth(span);
        self.locals.push(let_stmt(
            local,
            Some(typ.clone()),
            Some(ident(param, syn)),
            syn,
        ));
        local
    }
}

impl<'src> MutVisitor<'src> for Rebinder<'_, '_, 'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let span = expr.1;
        let Expr::Call { expr: callee, .. } = &mut expr.0 else {
            return;
        };
        let Expr::Member {
            expr: receiver,
            member,
        } = &mut callee.0
        else {
            return;
        };
        let Expr::Ident(param) = receiver.0 else {
            return;
        };
        let Some((class, typ)) = self.params.get(param).cloned() else {
            return;
        };
        if self.types.method_is_const(class, member) != Some(false) {
            return;
        }
        let member = *member;
        let local = self.local_of(param, &typ, span);
        receiver.0 = Expr::Ident(local);
        self.cx.info(
            span,
            "const-param-receiver",
            format!(
                "REDmod does not call the non-const method `{member}` on the const parameter \
                 `{param}`, the call goes through the local `{local}` holding the same object"
            ),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, squash, to_redmod, to_redmod_with_types};

    fn types() -> MapTypeQuery {
        MapTypeQuery::new()
            .with_method_const("StateContext", "SetTemporaryScriptableParameter", false)
            .with_method_const("StateContext", "GetFloatParameter", true)
            .with_method_const("Other", "Poke", false)
    }

    #[test]
    fn non_const_calls_on_const_params_go_through_a_local() {
        // Retrothrusters_QoL: REDmod's "Cannot call non-const, non-static function
        // 'SetTemporaryScriptableParameter' using const scope"
        let src = "@wrapMethod(AirThrustersDecisions)
protected final const func ToFall(const stateContext: ref<StateContext>, const other: ref<Other>, plain: ref<StateContext>) -> Bool {
  let f: Float = stateContext.GetFloatParameter(n\"a\", true);
  if f > 0.0 {
    stateContext.SetTemporaryScriptableParameter(n\"p\", null, true);
    stateContext.SetTemporaryScriptableParameter(n\"q\", null, true);
  }
  other.Poke();
  plain.SetTemporaryScriptableParameter(n\"r\", null, true);
  return wrappedMethod(stateContext, other, plain);
}
";
        let types = types();
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "{ let stateContextMut: StateContext = stateContext; let otherMut: Other = other; \
             let f: Float = stateContext.GetFloatParameter(n\"a\", true);",
            "stateContextMut.SetTemporaryScriptableParameter(n\"p\", NULL, true); \
             stateContextMut.SetTemporaryScriptableParameter(n\"q\", NULL, true);",
            "otherMut.Poke();",
            // not const: nothing to do
            "plain.SetTemporaryScriptableParameter(n\"r\", NULL, true);",
            "return wrappedMethod(stateContext, other, plain);",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["const-param-receiver"; 3]);
    }

    #[test]
    fn unknown_methods_reassigned_and_shadowed_params_are_left_alone() {
        let src = "func F(const a: ref<StateContext>, const b: ref<StateContext>, const c: ref<StateContext>) {
  a.Unknown();
  b = c;
  b.SetTemporaryScriptableParameter(n\"p\", null, true);
  if true {
    let c: ref<StateContext> = b;
    c.SetTemporaryScriptableParameter(n\"p\", null, true);
  }
}
";
        let types = types();
        let (out, diags) = to_redmod_with_types(src, &types, run);
        assert!(!out.contains("Mut"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");
        // without type information
        let (out, diags) = to_redmod(
            "func F(const a: ref<StateContext>) {\n  a.SetTemporaryScriptableParameter(n\"p\", null, true);\n}\n",
            run,
        );
        assert!(!out.contains("Mut"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn local_names_do_not_collide() {
        let src = "func F(const a: ref<StateContext>) {
  let aMut: Int32 = 1;
  a.SetTemporaryScriptableParameter(n\"p\", null, true);
}
";
        let types = types();
        let (out, _) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains("let aMut_1: StateContext = a; let aMut: Int32 = 1; aMut_1.Set"),
            "{out}"
        );
    }
}
