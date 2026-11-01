//! Qualifiers REDmod rejects:
//! - the visibility of members added or patched with `@wrapMethod`, `@replaceMethod`,
//!   `@addMethod` and `@addField` is dropped,
//! - top-level functions and enums cannot have a visibility in REDmod, it is dropped,
//! - static methods cannot be `final` in REDmod, the qualifier is dropped.

use redscript_ast::{Item, ItemQualifiers, SourceModule};

use crate::Cx;
use crate::walk::{PATCH_ANNOTATIONS, for_each_decl};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_decl(module, |(decl, span)| {
        if decl
            .annotations
            .iter()
            .any(|(ann, _)| PATCH_ANNOTATIONS.contains(&ann.name))
        {
            decl.visibility = None;
        }
        if let Item::Function(func) = &decl.item
            && decl
                .qualifiers
                .contains(ItemQualifiers::STATIC | ItemQualifiers::FINAL)
        {
            decl.qualifiers.remove(ItemQualifiers::FINAL);
            cx.info(
                *span,
                "final-dropped",
                format!(
                    "REDmod does not allow static functions to be final, `final` was dropped \
                     from `{}`",
                    func.name.0
                ),
            );
        }
    });

    for (decl, span) in &mut module.items {
        let what = match &decl.item {
            Item::Function(func) => format!("function `{}`", func.name.0),
            Item::Enum(enum_) => format!("enum `{}`", enum_.name.0),
            _ => continue,
        };
        if decl.visibility.take().is_some() {
            cx.info(
                *span,
                "visibility-dropped",
                format!(
                    "REDmod does not allow a visibility on top-level functions and enums, it \
                     was dropped from {what}"
                ),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn patch_annotations_drop_visibility() {
        let (out, _) = to_redmod(
            "@wrapMethod(A)\npublic final func F() {}\n\n@replaceMethod(A)\nprotected func G() {}\n\n@addMethod(A)\nprivate static func H() {}\n\n@addField(A)\npublic let x: Int32;\n\nclass B {\n  public func Kept() {}\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("@wrapMethod(A) final function F()"), "{out}");
        assert!(out.contains("@replaceMethod(A) function G()"), "{out}");
        assert!(out.contains("@addMethod(A) static function H()"), "{out}");
        assert!(out.contains("@addField(A) let x: Int32;"), "{out}");
        assert!(out.contains("public function Kept()"), "{out}");
    }

    #[test]
    fn top_level_functions_and_enums_drop_visibility() {
        let (out, diags) = to_redmod(
            "public func F() {}\n\nprivate func G() {}\n\nfunc H() {}\n\npublic enum E {\n  A = 0,\n}\n\npublic class C {\n  private let x: Int32;\n  public func M() {}\n}\n\npublic struct S {}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.starts_with("function F()"), "{out}");
        assert!(out.contains("function G()"), "{out}");
        assert!(out.contains("function H()"), "{out}");
        assert!(!out.contains("public function F"), "{out}");
        assert!(!out.contains("private function G"), "{out}");
        assert!(out.contains("enum E {"), "{out}");
        assert!(!out.contains("public enum"), "{out}");
        assert!(out.contains("public class C"), "{out}");
        assert!(out.contains("private let x: Int32;"), "{out}");
        assert!(out.contains("public function M()"), "{out}");
        assert!(out.contains("public struct S"), "{out}");
        assert_eq!(codes(&diags), ["visibility-dropped"; 3]);
    }

    #[test]
    fn static_methods_drop_final() {
        let (out, diags) = to_redmod(
            "class C {\n  public final static func F() {}\n  public static final func G() {}\n  public final func H() {}\n  public static func I() {}\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("public static function F()"), "{out}");
        assert!(out.contains("public static function G()"), "{out}");
        assert!(out.contains("public final function H()"), "{out}");
        assert!(out.contains("public static function I()"), "{out}");
        assert_eq!(codes(&diags), ["final-dropped"; 2]);
    }
}
