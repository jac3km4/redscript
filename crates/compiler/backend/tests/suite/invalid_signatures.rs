use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, TypeFlagRegistry, TypeInterner};
use redscript_compiler_backend::{AssembleError, CompilationInputs};
use redscript_compiler_frontend::infer_from_sources;
use redscript_io::{Class, ClassFlags, ScriptBundle, Visibility};

#[test]
fn partial_units_fail_before_mutating_the_bundle() {
    for source in [
        "func Broken(id: Missing, value: Int32) {}",
        "native func Broken() -> Missing;",
        "class Broken { public func Test(id: Missing, value: Int32) {} }",
        "native class Broken { public native func Test() -> Missing; }",
        "func Broken(value: array<Int32, Int32>) {}",
        "native func Broken() -> array<Int32, Int32>;",
        "class Broken { public func Test(value: array<Int32, Int32>) {} }",
        "native class Broken { public native func Test() -> array<Int32, Int32>; }",
        "func Broken<T extends array<Int32, Int32>>(value: T) {}",
        "class Bound {} class Box<T extends Bound> {} func Broken(value: Box<Int32>) {}",
        "class Bound {} class Box<T extends Bound> {} native func Broken() -> Box<Int32>;",
    ] {
        let sources = SourceMap::new();
        sources.push_back("invalid.reds", source);
        let interner = TypeInterner::default();
        let mut bundle = ScriptBundle::default();
        let name = bundle.cnames_mut().add("IScriptable");
        bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
        let (symbols, mappings) =
            CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
                .unwrap()
                .into_inner();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
        assert!(!reporter.reported().is_empty());
        let before = format!("{bundle:?}");
        let error = mappings
            .into_monomorphizer(&sources)
            .monomorphize(&unit, &symbols, &mut bundle)
            .unwrap_err();
        assert!(
            matches!(error, AssembleError::InvalidFunctionSignature(Some(_))),
            "{source}: {error:?}"
        );
        assert_eq!(format!("{bundle:?}"), before);
    }
}
