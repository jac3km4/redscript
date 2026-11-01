//! REDmod methods are final unless they are declared `virtual`, and a method overriding one needs
//! `override`:
//! - a method with the name of a non-static method of a base class gets `override`,
//! - a method of a class of the module that a class of the sources overrides gets `virtual`.
//!
//! The hierarchy comes from the type information when it's available, since it covers the game
//! and every file of the sources, and from the classes declared in the module otherwise.
//!
//! An override of a `const` method of the game must be `const` too: it gets `const` when the type
//! information says the overridden method is.
//!
//! When a base class is unknown (not declared in the module and no type information), methods get
//! neither qualifier and the class gets a note.
//!
//! REDmod also rejects body-less functions returning a value, methods (static or not) and free
//! functions alike: they get a body returning the default value of their type. Native functions
//! keep no body.
//!
//! REDmod cannot override some methods of the game at all: native methods ("has no virtual
//! function in base class to override") and methods taking a `TweakDBID` parameter ("parameter
//! has different type 'TweakDBID' than in base function"), even with the exact signature of the
//! game. Overrides of such methods get a warning when the type information knows about them.
//!
//! Callbacks (`event`s) need neither qualifier. Patches (`@wrapMethod`, `@replaceMethod`,
//! `@addMethod`) are left as they are: they keep the qualifiers of the method they patch.

use hashbrown::{HashMap, HashSet};
use redscript_ast::{
    Block, FunctionBody, Item, ItemQualifiers, SourceFunction, SourceModule, Span, Stmt,
};

use crate::index::Names;
use crate::walk::{annotation_target, ident, let_stmt, synth, type_name};
use crate::{BaseMethodInfo, Cx};

/// Classes without methods to override, as far as REDmod is concerned.
const ROOTS: [&str; 1] = ["IScriptable"];

/// Qualifiers of the methods that are never overridden.
const NOT_OVERRIDABLE: ItemQualifiers = ItemQualifiers::STATIC
    .union(ItemQualifiers::CALLBACK)
    .union(ItemQualifiers::NATIVE);

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let hierarchy = Hierarchy::new(module);
    let prefix: String = module
        .path
        .as_ref()
        .map(|path| path.segments.iter().map(|s| format!("{s}.")).collect())
        .unwrap_or_default();

    for (decl, span) in &mut module.items {
        if let Item::Function(func) = &mut decl.item {
            if annotation_target(&decl.annotations).is_none()
                && !decl.qualifiers.contains(ItemQualifiers::NATIVE)
            {
                add_default_body(func, *span, cx);
            }
            continue;
        }
        let Item::Class(agg) = &mut decl.item else {
            continue;
        };
        if decl
            .qualifiers
            .intersects(ItemQualifiers::NATIVE | ItemQualifiers::IMPORT_ONLY)
        {
            continue;
        }
        let class = Class {
            name: agg.name.0,
            qualified: format!("{prefix}{}", agg.name.0),
        };
        let mut unknown_base = None;
        for (member, member_span) in &mut agg.items {
            let Item::Function(func) = &mut member.item else {
                continue;
            };
            if annotation_target(&member.annotations).is_some()
                || member
                    .qualifiers
                    .intersects(ItemQualifiers::NATIVE | ItemQualifiers::CALLBACK)
            {
                continue;
            }
            if !member.qualifiers.intersects(NOT_OVERRIDABLE) {
                let method = func.name.0;
                let overrides = class.overrides(&hierarchy, method, cx);
                match overrides {
                    Ok(true) if !member.qualifiers.contains(ItemQualifiers::OVERRIDE) => {
                        member.qualifiers.insert(ItemQualifiers::OVERRIDE);
                        cx.info(
                            *member_span,
                            "override-added",
                            format!(
                                "`{method}` overrides a method of a base class, REDmod requires \
                                 `override`"
                            ),
                        );
                    }
                    Err(base) => unknown_base = unknown_base.or(Some(base)),
                    _ => {}
                }
                if overrides == Ok(true) {
                    class.check_supported(method, *member_span, cx);
                }
                if overrides == Ok(true)
                    && !member.qualifiers.contains(ItemQualifiers::CONST)
                    && class.overrides_const(method, cx)
                {
                    member.qualifiers.insert(ItemQualifiers::CONST);
                    cx.info(
                        *member_span,
                        "const-added",
                        format!(
                            "`{method}` overrides a `const` method, REDmod requires `const` on \
                             the override"
                        ),
                    );
                }
                // a method of a class with an unknown base gets neither qualifier
                if overrides == Ok(false)
                    && !member
                        .qualifiers
                        .intersects(ItemQualifiers::VIRTUAL | ItemQualifiers::OVERRIDE)
                    && class.is_overridden(&hierarchy, method, cx)
                {
                    member.qualifiers.insert(ItemQualifiers::VIRTUAL);
                    cx.info(
                        *member_span,
                        "virtual-added",
                        format!(
                            "`{method}` is overridden by a subclass, REDmod requires `virtual` \
                             on the overridden method"
                        ),
                    );
                }
            }
            add_default_body(func, *member_span, cx);
        }
        if let Some(base) = unknown_base {
            cx.info(
                *span,
                "override-unknown",
                format!(
                    "`{base}`, a base class of `{}`, is not declared in this file: add `override` \
                     to the methods that override one of its methods, or convert with \
                     `--bundle`",
                    class.name
                ),
            );
        }
    }
}

/// Gives a body-less function returning a value a body returning the default value of its type.
fn add_default_body<'src>(func: &mut SourceFunction<'src>, span: Span, cx: &mut Cx<'_, 'src>) {
    let Some(ret) = func.return_type.as_deref() else {
        return;
    };
    if func.body.is_some() || type_name(&ret.0) == Some("Void") {
        return;
    }
    let ret = ret.clone();
    let span = synth(span);
    let local = Names::of(func, cx.arena).fresh("result");
    let stmts = [
        let_stmt(local, Some(ret), None, span),
        (Stmt::Return(Some(Box::new(ident(local, span)))), span),
    ];
    func.body = Some(FunctionBody::Block(Block::new(stmts)));
    cx.info(
        span,
        "default-body",
        format!(
            "REDmod requires a body for `{}`, which returns a value: it now returns the default \
             value of its type",
            func.name.0
        ),
    );
}

/// The classes of the module and the methods added to the other classes.
struct Hierarchy<'src> {
    classes: HashMap<&'src str, LocalClass<'src>>,
}

#[derive(Default)]
struct LocalClass<'src> {
    /// Whether the class is declared in the module, rather than only patched.
    declared: bool,
    base: Option<&'src str>,
    /// Methods that can be overridden.
    methods: HashSet<&'src str>,
}

impl<'src> Hierarchy<'src> {
    fn new(module: &SourceModule<'src>) -> Self {
        let mut classes: HashMap<&'src str, LocalClass<'src>> = HashMap::new();
        for (decl, _) in &module.items {
            match &decl.item {
                Item::Class(agg) => {
                    let class = classes.entry(agg.name.0).or_default();
                    class.declared = true;
                    class.base = agg.extends.as_deref().and_then(|(typ, _)| type_name(typ));
                    for (member, _) in &agg.items {
                        if let Item::Function(func) = &member.item
                            && !member.qualifiers.intersects(NOT_OVERRIDABLE)
                        {
                            class.methods.insert(func.name.0);
                        }
                    }
                }
                Item::Function(func) if !decl.qualifiers.intersects(NOT_OVERRIDABLE) => {
                    if let Some(target) = annotation_target(&decl.annotations) {
                        classes
                            .entry(target)
                            .or_default()
                            .methods
                            .insert(func.name.0);
                    }
                }
                _ => {}
            }
        }
        Self { classes }
    }

    /// The bases of `class`, nearest first, as far as they are declared in the module. The last
    /// one is the first base not declared in the module, if any.
    fn bases(&self, class: &'src str) -> Vec<&'src str> {
        let mut bases = vec![];
        let mut base = self.classes.get(class).and_then(|c| c.base);
        while let Some(name) = base.filter(|name| *name != class && !bases.contains(name)) {
            bases.push(name);
            base = self
                .classes
                .get(name)
                .filter(|c| c.declared)
                .and_then(|c| c.base);
        }
        bases
    }
}

struct Class<'src> {
    name: &'src str,
    qualified: String,
}

impl<'src> Class<'src> {
    /// Whether `method` overrides a method of a base class. Fails with the first base class
    /// that is not declared in the module when it's unknown.
    fn overrides(
        &self,
        hierarchy: &Hierarchy<'src>,
        method: &str,
        cx: &Cx<'_, 'src>,
    ) -> Result<bool, &'src str> {
        if let Some(res) = cx
            .opts
            .types
            .and_then(|types| types.base_has_method(&self.qualified, method))
        {
            return Ok(res);
        }
        for base in hierarchy.bases(self.name) {
            let info = hierarchy.classes.get(base);
            if info.is_some_and(|c| c.methods.contains(method)) {
                return Ok(true);
            }
            if !info.is_some_and(|c| c.declared) && !ROOTS.contains(&base) {
                return Err(base);
            }
        }
        Ok(false)
    }

    /// Whether `method` overrides a `const` method of a base class, as far as the type
    /// information knows.
    fn overrides_const(&self, method: &str, cx: &Cx<'_, 'src>) -> bool {
        cx.opts
            .types
            .and_then(|types| types.base_method_is_const(&self.qualified, method))
            .unwrap_or(false)
    }

    /// Warns about an override of a method of the game that REDmod cannot override.
    fn check_supported(&self, method: &str, span: Span, cx: &mut Cx<'_, 'src>) {
        let Some(info) = cx
            .opts
            .types
            .and_then(|types| types.base_method_info(&self.qualified, method))
        else {
            return;
        };
        if let Some(reason) = unsupported_override(&info) {
            cx.warn(
                span,
                "redmod-unsupported-override",
                format!(
                    "`{method}` overrides {reason}, which REDmod cannot override: the class does \
                     not compile in REDmod, move this logic out of the override"
                ),
            );
        }
    }

    /// Whether a subclass declared in the sources overrides `method`.
    fn is_overridden(&self, hierarchy: &Hierarchy<'src>, method: &str, cx: &Cx<'_, 'src>) -> bool {
        let local = hierarchy.classes.iter().any(|(name, class)| {
            class.declared
                && class.methods.contains(method)
                && hierarchy.bases(name).contains(&self.name)
        });
        local
            || cx
                .opts
                .types
                .and_then(|types| types.is_overridden_in_sources(&self.qualified, method))
                .unwrap_or(false)
    }
}

/// Why REDmod cannot override the method described by `info`, if it cannot.
fn unsupported_override(info: &BaseMethodInfo) -> Option<&'static str> {
    if info.from_sources {
        None
    } else if info.is_native {
        Some("a native method of the game")
    } else if info.has_tweakdbid_param {
        Some("a method of the game taking a `TweakDBID` parameter")
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, squash, to_redmod, to_redmod_with_types};
    use crate::{BaseMethodInfo, Severity};

    #[test]
    fn same_file_hierarchy() {
        let src = "class A {
  public func F() -> Void {}
  public func G() -> Void {}
  public static func S() -> Void {}
  cb func OnE(evt: ref<Event>) -> Bool { return true; }
}
class B extends A {
  public func F() -> Void {}
  public static func S() -> Void {}
  cb func OnE(evt: ref<Event>) -> Bool { return true; }
}
class C extends B {
  public func G() -> Void {}
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        assert!(out.contains("public virtual function F() -> Void"), "{out}");
        assert!(out.contains("public virtual function G() -> Void"), "{out}");
        assert!(
            out.contains("public override function F() -> Void"),
            "{out}"
        );
        assert!(
            out.contains("public override function G() -> Void"),
            "{out}"
        );
        assert_eq!(out.matches("static function S()").count(), 2, "{out}");
        assert!(!out.contains("override static") && !out.contains("virtual static"));
        assert!(!out.contains("override event") && !out.contains("virtual event"));
        assert_eq!(
            codes(&diags),
            [
                "virtual-added",
                "virtual-added",
                "override-added",
                "override-added"
            ]
        );
    }

    #[test]
    fn unknown_base_without_types() {
        let src = "class A extends GameObject {
  public func F() -> Void {}
  public func G() -> Void {}
}
class B extends IScriptable {
  public func F() -> Void {}
}
@wrapMethod(GameObject)
public func H() -> Void { wrappedMethod(); }
";
        let (out, diags) = to_redmod(src, run);
        assert!(
            !out.contains("override") && !out.contains("virtual"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["override-unknown"]);
        assert!(diags[0].message.contains("`GameObject`"), "{}", diags[0]);
    }

    #[test]
    fn methods_added_to_a_base_class() {
        let src = "@addMethod(GameObject)
public func Added() -> Void {}
class A extends GameObject {
  public func Added() -> Void {}
}
";
        let (out, diags) = to_redmod(src, run);
        assert!(
            squash(&out).contains("public override function Added()"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["override-added"]);
    }

    #[test]
    fn game_base_from_types() {
        let src = "module Mod.M
class A extends GameObject {
  public func OnGameAttached() -> Void {}
  public func Mine() -> Void {}
}
";
        let types = MapTypeQuery::new()
            .with_class("GameObject", None, &["OnGameAttached"], true)
            .with_class(
                "Mod.M.A",
                Some("GameObject"),
                &["OnGameAttached", "Mine"],
                false,
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains("public override function OnGameAttached()"),
            "{out}"
        );
        assert!(out.contains("public function Mine()"), "{out}");
        assert_eq!(codes(&diags), ["override-added"]);
    }

    #[test]
    fn mod_subclass_from_types() {
        let src = "module Mod.M
public abstract class Hint extends IScriptable {
  public func Kind() -> Void {}
  public func Other() -> Void {}
}
";
        let types = MapTypeQuery::new()
            .with_class("IScriptable", None, &[], true)
            .with_class("Mod.M.Hint", Some("IScriptable"), &["Kind", "Other"], false)
            .with_class("Mod.N.Sub", Some("Mod.M.Hint"), &["Kind"], false);
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(out.contains("public virtual function Kind()"), "{out}");
        assert!(out.contains("public function Other()"), "{out}");
        assert_eq!(codes(&diags), ["virtual-added"]);
    }

    #[test]
    fn body_less_methods_return_a_default() {
        let src = "class A {
  public func Value() -> Int32;
  public func Nothing();
  public func Unit() -> Void;
}
class B extends A {
  public func Value() -> Int32 { return 1; }
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        assert!(
            out.contains(
                "public virtual function Value() -> Int32 { let result: Int32; return result; }"
            ),
            "{out}"
        );
        assert!(out.contains("public function Nothing();"), "{out}");
        assert!(out.contains("public function Unit() -> Void;"), "{out}");
        assert_eq!(
            codes(&diags),
            ["virtual-added", "default-body", "override-added"]
        );
    }

    #[test]
    fn native_classes_are_left_alone() {
        let src = "native class A {
  public native func Value() -> Int32;
}
class B extends A {
  public native func Value() -> Int32;
}
";
        let (out, diags) = to_redmod(src, run);
        assert!(
            !out.contains("override") && !out.contains("Int32 {"),
            "{out}"
        );
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn overrides_of_const_methods_are_const() {
        let src = "module Mod.M
class P extends IScriptable {
  public func IsFulfilled(game: GameInstance, context: ref<IScriptable>) -> Bool { return true; }
  public func Other() -> Bool { return true; }
}
";
        let types = MapTypeQuery::new()
            .with_class("IScriptable", None, &[], true)
            .with_class(
                "IPrereq",
                Some("IScriptable"),
                &["IsFulfilled", "Other"],
                true,
            )
            .with_const_method("IPrereq", "IsFulfilled")
            .with_class("Mod.M.P", Some("IPrereq"), &["IsFulfilled", "Other"], false);
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains("public const override function IsFulfilled("),
            "{out}"
        );
        assert!(out.contains("public override function Other()"), "{out}");
        assert_eq!(
            codes(&diags),
            ["override-added", "const-added", "override-added"]
        );
    }

    #[test]
    fn static_and_free_functions_get_default_bodies() {
        let src = "class A {
  public static func Make() -> Int32;
  public static func Unit();
  public static native func Native() -> Int32;
}
func Free() -> String;
native func NativeFree() -> Int32;
func FreeUnit() -> Void;
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        for expected in [
            "public static function Make() -> Int32 { let result: Int32; return result; }",
            "public static function Unit();",
            "function Free() -> String { let result: String; return result; }",
            "function FreeUnit() -> Void;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(!out.contains("Native() -> Int32 {"), "{out}");
        assert!(!out.contains("NativeFree() -> Int32 {"), "{out}");
        assert_eq!(codes(&diags), ["default-body"; 2]);
    }

    #[test]
    fn unknown_base_adds_no_virtual() {
        let src = "class A extends GameObject {
  public func F() -> Void {}
}
class B extends A {
  public func F() -> Void {}
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        assert!(!out.contains("virtual"), "{out}");
        assert!(out.contains("public override function F()"), "{out}");
        assert_eq!(codes(&diags), ["override-unknown", "override-added"]);
    }

    #[test]
    fn unsupported_overrides_of_game_methods() {
        let src = "module Mod.M
class P extends IScriptablePrereq {
  public func IsFulfilled(game: GameInstance, context: ref<IScriptable>) -> Bool { return true; }
  protected func Initialize(record: TweakDBID) -> Void {}
  protected func OnRegister(game: GameInstance) -> Bool { return false; }
}
class E extends Effector {
  protected func Initialize(record: TweakDBID, game: GameInstance, parentRecord: TweakDBID) -> Void {}
}
class S extends E {
  protected func Initialize(record: TweakDBID, game: GameInstance, parentRecord: TweakDBID) -> Void {}
}
";
        let native = BaseMethodInfo {
            is_native: true,
            class_is_native: true,
            ..BaseMethodInfo::default()
        };
        let tweak = BaseMethodInfo {
            class_is_native: true,
            has_tweakdbid_param: true,
            ..BaseMethodInfo::default()
        };
        let types = MapTypeQuery::new()
            .with_class("IScriptable", None, &[], true)
            .with_class("IPrereq", Some("IScriptable"), &["IsFulfilled"], true)
            .with_class(
                "IScriptablePrereq",
                Some("IPrereq"),
                &["Initialize", "OnRegister"],
                true,
            )
            .with_class("Effector", Some("IScriptable"), &["Initialize"], true)
            .with_method_info("IPrereq", "IsFulfilled", native)
            .with_method_info("IScriptablePrereq", "Initialize", tweak)
            .with_method_info(
                "IScriptablePrereq",
                "OnRegister",
                BaseMethodInfo {
                    class_is_native: true,
                    ..BaseMethodInfo::default()
                },
            )
            .with_method_info("Effector", "Initialize", tweak)
            .with_class(
                "Mod.M.P",
                Some("IScriptablePrereq"),
                &["IsFulfilled", "Initialize", "OnRegister"],
                false,
            )
            .with_class("Mod.M.E", Some("Effector"), &["Initialize"], false)
            .with_class("Mod.M.S", Some("Mod.M.E"), &["Initialize"], false)
            // an override of a method of the sources is fine, even with a TweakDBID parameter
            .with_method_info(
                "Mod.M.E",
                "Initialize",
                BaseMethodInfo {
                    has_tweakdbid_param: true,
                    from_sources: true,
                    ..BaseMethodInfo::default()
                },
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        // the qualifiers are added as usual
        assert!(
            out.contains("public override function IsFulfilled("),
            "{out}"
        );
        let unsupported: Vec<_> = diags
            .iter()
            .filter(|d| d.code == "redmod-unsupported-override")
            .collect();
        assert_eq!(unsupported.len(), 3, "{diags:?}");
        assert!(unsupported.iter().all(|d| d.severity == Severity::Warning));
        assert!(
            unsupported[0].message.contains("`IsFulfilled`")
                && unsupported[0].message.contains("native"),
            "{}",
            unsupported[0]
        );
        assert!(
            unsupported[1].message.contains("`Initialize`")
                && unsupported[1].message.contains("`TweakDBID`"),
            "{}",
            unsupported[1]
        );
        assert!(unsupported[2].message.contains("`Initialize`"));
        assert_eq!(
            codes(&diags)
                .iter()
                .filter(|c| **c == "redmod-unsupported-override")
                .count(),
            3
        );
    }

    #[test]
    fn no_unsupported_override_warning_without_types() {
        let src = "class P extends IScriptablePrereq {
  public func IsFulfilled(game: GameInstance, context: ref<IScriptable>) -> Bool { return true; }
}
";
        let (_, diags) = to_redmod(src, run);
        assert_eq!(codes(&diags), ["override-unknown"]);
    }
}
