use std::cell::RefCell;
use std::fmt;
use std::path::Path;

use indexmap::IndexSet;
use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{
    CompileErrorReporter, Diagnostics, SourceMapExt, TypeFlagRegistry, TypeInterner,
};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::infer_from_sources;
use redscript_io::{
    CNameIndex, Class, ClassFlags, ClassIndex, EnumIndex, EnumValueIndex, FieldIndex, Function,
    FunctionFlags, FunctionIndex, IndexedDefinition, Instr, LocalIndex, Parameter, ParameterFlags,
    ParameterIndex, ResourceIndex, ScriptBundle, StringIndex, TweakDbIndex, Type, TypeIndex,
    TypeKind, Visibility,
};

#[test]
fn bytecode() {
    insta::glob!("data/*.reds", |path| {
        insta::assert_snapshot!(compile(path, |_| {}));
    });
}

#[test]
fn short_circuit() {
    let path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data/short-circuit/operators.reds");
    let bytecode = compile(&path, |bundle| {
        let name = bundle.cnames_mut().add("Bool");
        let bool_type = bundle.define(Type::new(name, TypeKind::Primitive));
        for name in ["OperatorLogicAnd", "OperatorLogicOr"] {
            let name = bundle.cnames_mut().add(name);
            let flags = FunctionFlags::default()
                .with_is_native(true)
                .with_is_static(true)
                .with_is_final(true);
            let function = Function::new(name, Visibility::Public, flags)
                .with_source(None)
                .with_return_type(Some(bool_type));
            bundle.define_and_init(function, |bundle, index, function| {
                let parameters = [("lhs", false), ("rhs", true)].map(|(name, short_circuit)| {
                    let name = bundle.cnames_mut().add(name);
                    let flags = ParameterFlags::default().with_is_short_circuit(short_circuit);
                    bundle.define(Parameter::new(name, index, bool_type, flags))
                });
                function.with_parameters(parameters)
            });
        }
    });
    insta::assert_snapshot!(bytecode);
}

fn compile(path: &Path, seed: impl FnOnce(&mut ScriptBundle<'_>)) -> String {
    let sources = SourceMap::from_files([path]).unwrap();
    sources.populate_boot_lib();

    let interner = TypeInterner::default();
    let mut bundle = ScriptBundle::default();
    seed(&mut bundle);
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));

    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
    let diagnostics = reporter.into_reported();
    assert_eq!(
        diagnostics.iter().filter(|d| d.is_error()).count(),
        0,
        "{}: {}",
        Diagnostics::from(diagnostics),
        path.display()
    );

    let bundle_len = bundle.definitions().len();
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    BytecodePrinter::new(&bundle, bundle_len).to_string()
}

#[derive(Debug)]
struct BytecodePrinter<'a> {
    bundle: &'a ScriptBundle<'a>,
    offset: usize,
    indices: RefCell<Indices<'a>>,
}

impl<'a> BytecodePrinter<'a> {
    fn new(bundle: &'a ScriptBundle<'a>, offset: usize) -> Self {
        Self {
            bundle,
            offset,
            indices: RefCell::new(Indices::new(bundle)),
        }
    }
}

impl fmt::Display for BytecodePrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut indices = self.indices.borrow_mut();

        for def in self.bundle.indexed_definitions().skip(self.offset) {
            let IndexedDefinition::Function(i, func) = def else {
                continue;
            };

            write!(f, "// ")?;
            if let Some(class) = func.class() {
                write!(f, "{}::", self.bundle[self.bundle[class].name()])?;
            }
            writeln!(
                f,
                "{} (f{})",
                self.bundle[func.name()],
                indices.functions.insert_full(i).0
            )?;

            for instr in func.body().code_iter() {
                match instr.unwrap() {
                    Instr::CNameConst(idx) => {
                        writeln!(
                            f,
                            "cname.const {} {:?}",
                            indices.names.insert_full(idx).0,
                            self.bundle[idx]
                        )?;
                    }
                    Instr::EnumConst { enum_, value } => {
                        writeln!(
                            f,
                            "enum.const {} {}",
                            indices.enum_(enum_),
                            indices.enum_value(value)
                        )?;
                    }
                    Instr::StringConst(idx) => {
                        writeln!(
                            f,
                            "string.const {} {:?}",
                            indices.string(idx),
                            self.bundle[idx]
                        )?;
                    }
                    Instr::TweakDbIdConst(idx) => {
                        writeln!(
                            f,
                            "tweakdb.const {} {:?}",
                            indices.tweakdb(idx),
                            self.bundle[idx]
                        )?;
                    }
                    Instr::ResourceConst(idx) => {
                        writeln!(
                            f,
                            "resource.const {} {:?}",
                            indices.resource(idx),
                            self.bundle[idx]
                        )?;
                    }
                    Instr::Local(idx) => {
                        writeln!(f, "local {}", indices.local(idx))?;
                    }
                    Instr::Param(idx) => {
                        writeln!(f, "param {}", indices.param(idx))?;
                    }
                    Instr::ObjectField(idx) => {
                        writeln!(f, "object.field {}", indices.field(idx))?;
                    }
                    Instr::Switch(switch) => {
                        writeln!(
                            f,
                            "{} j{}",
                            Instr::Switch(switch.clone()),
                            switch.first_case()
                        )?;
                    }
                    Instr::Construct { arg_count, class } => {
                        writeln!(f, "struct.new {} {}", arg_count, indices.class(class))?;
                    }
                    Instr::InvokeStatic {
                        exit,
                        line,
                        function,
                        flags,
                    } => {
                        writeln!(
                            f,
                            "invoke.static j{} l{} f{} {}",
                            exit.target(),
                            line,
                            indices.function(function),
                            flags
                        )?;
                    }
                    Instr::InvokeVirtual {
                        exit,
                        line,
                        function,
                        flags,
                    } => {
                        writeln!(
                            f,
                            "invoke.virtual j{} l{} f{} {}",
                            exit.target(),
                            line,
                            indices.name(function),
                            flags
                        )?;
                    }
                    Instr::StructField(idx) => {
                        writeln!(f, "struct.field {}", indices.field(idx))?;
                    }
                    Instr::Equals(idx) => {
                        writeln!(f, "eq {}", indices.type_(idx))?;
                    }
                    Instr::RefStringEqualsString(idx) => {
                        writeln!(f, "refstr.eq {}", indices.type_(idx))?;
                    }
                    Instr::StringEqualsRefString(idx) => {
                        writeln!(f, "str.eq {}", indices.type_(idx))?;
                    }
                    Instr::NotEquals(idx) => {
                        writeln!(f, "neq {}", indices.type_(idx))?;
                    }
                    Instr::RefStringNotEqualsString(idx) => {
                        writeln!(f, "refstr.neq {}", indices.type_(idx))?;
                    }
                    Instr::StringNotEqualsRefString(idx) => {
                        writeln!(f, "str.neq {}", indices.type_(idx))?;
                    }
                    Instr::New(idx) => {
                        writeln!(f, "object.new {}", indices.class(idx))?;
                    }
                    Instr::ArrayClear(idx) => writeln!(f, "array.clear {}", indices.type_(idx))?,
                    Instr::ArraySize(idx) => writeln!(f, "array.size {}", indices.type_(idx))?,
                    Instr::ArrayResize(idx) => writeln!(f, "array.resize {}", indices.type_(idx))?,
                    Instr::ArrayFindFirst(idx) => {
                        writeln!(f, "array.find_first {}", indices.type_(idx))?;
                    }
                    Instr::ArrayFindFirstFast(idx) => {
                        writeln!(f, "array.find_first_fast {}", indices.type_(idx))?;
                    }
                    Instr::ArrayFindLast(idx) => {
                        writeln!(f, "array.find_last {}", indices.type_(idx))?;
                    }
                    Instr::ArrayFindLastFast(idx) => {
                        writeln!(f, "array.find_last_fast {}", indices.type_(idx))?;
                    }
                    Instr::ArrayContains(idx) => {
                        writeln!(f, "array.contains {}", indices.type_(idx))?;
                    }
                    Instr::ArrayContainsFast(idx) => {
                        writeln!(f, "array.contains_fast {}", indices.type_(idx))?;
                    }
                    Instr::ArrayCount(idx) => writeln!(f, "array.count {}", indices.type_(idx))?,
                    Instr::ArrayCountFast(idx) => {
                        writeln!(f, "array.count_fast {}", indices.type_(idx))?;
                    }
                    Instr::ArrayPush(idx) => writeln!(f, "array.push {}", indices.type_(idx))?,
                    Instr::ArrayPop(idx) => writeln!(f, "array.pop {}", indices.type_(idx))?,
                    Instr::ArrayInsert(idx) => writeln!(f, "array.insert {}", indices.type_(idx))?,
                    Instr::ArrayRemove(idx) => writeln!(f, "array.remove {}", indices.type_(idx))?,
                    Instr::ArrayRemoveFast(idx) => {
                        writeln!(f, "array.remove_fast {}", indices.type_(idx))?;
                    }
                    Instr::ArrayGrow(idx) => writeln!(f, "array.grow {}", indices.type_(idx))?,
                    Instr::ArrayErase(idx) => writeln!(f, "array.erase {}", indices.type_(idx))?,
                    Instr::ArrayEraseFast(idx) => {
                        writeln!(f, "array.erase_fast {}", indices.type_(idx))?;
                    }
                    Instr::ArrayLast(idx) => writeln!(f, "array.last {}", indices.type_(idx))?,
                    Instr::ArrayElement(idx) => {
                        writeln!(f, "array.element {}", indices.type_(idx))?;
                    }
                    Instr::ArraySort(idx) => writeln!(f, "array.sort {}", indices.type_(idx))?,
                    Instr::ArraySortByPredicate(idx) => {
                        writeln!(f, "array.sort_by {}", indices.type_(idx))?;
                    }
                    Instr::StaticArraySize(idx) => {
                        writeln!(f, "static_array.size {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayFindFirst(idx) => {
                        writeln!(f, "static_array.find_first {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayFindFirstFast(idx) => {
                        writeln!(f, "static_array.find_first_fast {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayFindLast(idx) => {
                        writeln!(f, "static_array.find_last {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayFindLastFast(idx) => {
                        writeln!(f, "static_array.find_last_fast {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayContains(idx) => {
                        writeln!(f, "static_array.contains {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayContainsFast(idx) => {
                        writeln!(f, "static_array.contains_fast {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayCount(idx) => {
                        writeln!(f, "static_array.count {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayCountFast(idx) => {
                        writeln!(f, "static_array.count_fast {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayLast(idx) => {
                        writeln!(f, "static_array.last {}", indices.type_(idx))?;
                    }
                    Instr::StaticArrayElement(idx) => {
                        writeln!(f, "static_array.element {}", indices.type_(idx))?;
                    }
                    Instr::EnumToI32 { enum_type, size } => {
                        writeln!(f, "enum.to_int {} {}", indices.type_(enum_type), size)?;
                    }
                    Instr::I32ToEnum { enum_type, size } => {
                        writeln!(f, "enum.from_int {} {}", indices.type_(enum_type), size)?;
                    }
                    Instr::DynamicCast { class, is_weak } if is_weak => {
                        writeln!(f, "wref.dyncast {}", indices.class(class))?;
                    }
                    Instr::DynamicCast { class, .. } => {
                        writeln!(f, "ref.dyncast {}", indices.class(class))?;
                    }
                    Instr::ToString(idx) => {
                        writeln!(f, "to_string {}", indices.type_(idx))?;
                    }
                    Instr::ToVariant(idx) => {
                        writeln!(f, "variant.new {}", indices.type_(idx))?;
                    }
                    Instr::FromVariant(idx) => {
                        writeln!(f, "variant.extract {}", indices.type_(idx))?;
                    }
                    Instr::AsRef(idx) => {
                        writeln!(f, "as_ref {}", indices.type_(idx))?;
                    }
                    Instr::Deref(idx) => {
                        writeln!(f, "deref {}", indices.type_(idx))?;
                    }
                    other => writeln!(f, "{other}")?,
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
struct Indices<'a> {
    bundle: &'a ScriptBundle<'a>,
    names: IndexSet<CNameIndex>,
    enums: IndexSet<EnumIndex>,
    enum_values: IndexSet<EnumValueIndex>,
    strings: IndexSet<StringIndex>,
    tweakdb_ids: IndexSet<TweakDbIndex>,
    resouces: IndexSet<ResourceIndex>,
    locals: IndexSet<LocalIndex>,
    params: IndexSet<ParameterIndex>,
    fields: IndexSet<FieldIndex>,
    classes: IndexSet<ClassIndex>,
    functions: IndexSet<FunctionIndex>,
    types: IndexSet<TypeIndex>,
}

impl<'a> Indices<'a> {
    fn new(bundle: &'a ScriptBundle<'a>) -> Self {
        Self {
            bundle,
            names: IndexSet::default(),
            enums: IndexSet::default(),
            enum_values: IndexSet::default(),
            strings: IndexSet::default(),
            tweakdb_ids: IndexSet::default(),
            resouces: IndexSet::default(),
            locals: IndexSet::default(),
            params: IndexSet::default(),
            fields: IndexSet::default(),
            classes: IndexSet::default(),
            functions: IndexSet::default(),
            types: IndexSet::default(),
        }
    }

    fn name(&mut self, index: CNameIndex) -> String {
        format!(
            "{} {:?}",
            self.names.insert_full(index).0,
            self.bundle[index]
        )
    }

    fn enum_(&mut self, index: EnumIndex) -> String {
        format!(
            "{} {:?}",
            self.enums.insert_full(index).0,
            self.bundle[self.bundle[index].name()]
        )
    }

    fn enum_value(&mut self, index: EnumValueIndex) -> String {
        let member = &self.bundle[index];
        format!(
            "{} {:?}::{} = {}",
            self.enum_values.insert_full(index).0,
            self.bundle[self.bundle[member.enum_()].name()],
            self.bundle[member.name()],
            member.value()
        )
    }

    fn string(&mut self, index: StringIndex) -> usize {
        self.strings.insert_full(index).0
    }

    fn tweakdb(&mut self, index: TweakDbIndex) -> usize {
        self.tweakdb_ids.insert_full(index).0
    }

    fn resource(&mut self, index: ResourceIndex) -> usize {
        self.resouces.insert_full(index).0
    }

    fn local(&mut self, index: LocalIndex) -> String {
        let local = &self.bundle[index];
        format!(
            "{} {:?}: {:?}",
            self.locals.insert_full(index).0,
            self.bundle[local.name()],
            self.bundle[self.bundle[local.type_()].name()]
        )
    }

    fn param(&mut self, index: ParameterIndex) -> String {
        let param = &self.bundle[index];
        format!(
            "{} {:?}: {:?}",
            self.params.insert_full(index).0,
            self.bundle[param.name()],
            self.bundle[self.bundle[param.type_()].name()]
        )
    }

    fn field(&mut self, index: FieldIndex) -> String {
        let field = &self.bundle[index];
        format!(
            "{} {}::{}: {:?}",
            self.fields.insert_full(index).0,
            self.bundle[self.bundle[field.class()].name()],
            self.bundle[field.name()],
            self.bundle[self.bundle[field.type_()].name()]
        )
    }

    fn class(&mut self, index: ClassIndex) -> String {
        format!(
            "{} {:?}",
            self.classes.insert_full(index).0,
            self.bundle[self.bundle[index].name()]
        )
    }

    fn function(&mut self, index: FunctionIndex) -> String {
        let function = &self.bundle[index];
        let owner = function
            .class()
            .map(|class| format!("{}::", self.bundle[self.bundle[class].name()]))
            .unwrap_or_default();
        format!(
            "{} {}{}",
            self.functions.insert_full(index).0,
            owner,
            self.bundle[function.name()]
        )
    }

    fn type_(&mut self, index: TypeIndex) -> String {
        format!(
            "{} {:?}",
            self.types.insert_full(index).0,
            self.bundle[self.bundle[index].name()]
        )
    }
}
