//! Type information for the conversion of `.reds` sources to REDmod, provided by the redscript
//! 0.5 type checker.
//!
//! The sources are type-checked against the game scripts with the types of their expressions
//! recorded (see [`TypeRecords`]). The checker reads the same files, in the same order and with
//! the same contents as the source map of the conversion, so a position of the checker (global,
//! over all the files laid out one after another) maps to a file and an offset in that file.

use std::str::FromStr;

use hashbrown::{HashMap, HashSet};
use redscript::ast::{BinOp, Pos, UnOp};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{AnyDefinition, Class, Definition, Enum, Function, Type};
use redscript_ast::{FileId, SourceMap, Span};
use redscript_compiler::diagnostics::Diagnostic;
use redscript_compiler::error::{Cause, Error};
use redscript_compiler::parser::parse_file;
use redscript_compiler::scope::TypeId;
use redscript_compiler::source_map::Files;
use redscript_compiler::symbol::FunctionSignature;
use redscript_compiler::typechecker::{RecordKey, RecordedTypes, TypeRecords};
use redscript_compiler::unit::CompilationUnit;
use redscript_convert::driver::TypeCheckSummary;
use redscript_convert::{ConvertDiagnostic, RefKind, Severity, TypeHint, TypeQuery};

/// Type-checks the files `ids` of `map` against `pool` (the game scripts), which is modified.
/// Files the 0.5 parser rejects are left out, their expressions have no types.
pub fn type_check(
    map: &SourceMap,
    ids: &[FileId],
    mut pool: ConstantPool,
) -> Result<(TypeOracle, TypeCheckSummary), Error> {
    ensure_placeholder_type(&mut pool);
    // definitions from this index on are declared by the sources
    let first_source = u32::try_from(pool.definitions().count() + 1).unwrap_or(u32::MAX);

    let mut files = Files::new();
    for &id in ids {
        let Some(file) = map.get(id) else { continue };
        files.add(file.path().to_owned(), without_bom(file.source()));
    }
    let layout = Layout::new(map, &files, ids);

    let mut modules = vec![];
    for file in files.files() {
        match parse_file(file) {
            Ok(module) => modules.push(module),
            Err(err) => log::warn!(
                "redscript 0.5 cannot parse {} ({}), its types are unknown",
                file.path().display(),
                err.location
            ),
        }
    }

    let output = CompilationUnit::new(&mut pool, vec![])?
        .with_type_records()
        .typecheck(modules, &files, false, true)?;
    let (_, diagnostics, records) = output.into_parts();

    let mut types = HashMap::new();
    for (key, recorded) in records.iter().flat_map(TypeRecords::iter) {
        if let Some(span) = layout.key_span(key) {
            types.insert(span, Recorded::new(recorded, &pool));
        }
    }
    let summary = type_check_summary(&diagnostics, &layout);
    let oracle = TypeOracle::new(pool, types, first_source);
    Ok((oracle, summary))
}

/// Adds `ref<IScriptable>` to `pool` if missing (the game scripts have it): the type checker
/// replaces the types it cannot resolve with it.
fn ensure_placeholder_type(pool: &mut ConstantPool) {
    let find = |pool: &ConstantPool, name: &str| {
        pool.roots().find_map(|(index, def)| {
            (matches!(def.value, AnyDefinition::Type(_)) && pool.names.get(def.name).is_ok_and(|n| &*n == name))
                .then(|| index.cast::<Type>())
        })
    };
    if find(pool, "ref:IScriptable").is_some() {
        return;
    }
    let class = find(pool, "IScriptable").unwrap_or_else(|| {
        let name = pool.names.add("IScriptable".into());
        pool.add_definition(Definition::type_(name, Type::Class))
    });
    let name = pool.names.add("ref:IScriptable".into());
    pool.add_definition::<Type>(Definition::type_(name, Type::Ref(class)));
}

/// A byte order mark is replaced by spaces of the same length, which the 0.5 parser accepts.
fn without_bom(source: &str) -> String {
    match source.strip_prefix('\u{feff}') {
        Some(rest) => format!("   {rest}"),
        None => source.to_owned(),
    }
}

/// Where each file is in the positions of the type checker.
struct Layout<'a> {
    /// The start and end position of each file, with its id and source.
    files: Vec<(usize, usize, FileId, &'a str)>,
}

impl<'a> Layout<'a> {
    fn new(map: &'a SourceMap, files: &Files, ids: &[FileId]) -> Self {
        let files = files
            .files()
            .zip(ids.iter().filter(|&&id| map.get(id).is_some()))
            .filter_map(|(file, &id)| {
                let span = file.span();
                Some((usize::from(span.low), usize::from(span.high), id, map.get(id)?.source()))
            })
            .collect();
        Self { files }
    }

    /// The span in its file of the checker positions `low..high`.
    fn span(&self, low: Pos, high: Pos) -> Option<(Span, &'a str)> {
        let (low, high) = (usize::from(low), usize::from(high));
        let index = self.files.partition_point(|&(_, end, _, _)| end < low);
        let &(start, end, id, source) = self.files.get(index)?;
        if low < start || high > end || high < low {
            return None;
        }
        let local = |pos: usize| u32::try_from(pos - start).ok();
        Some((Span::new(local(low)?, local(high)?, id), source))
    }

    /// The span of the expression identified by `key`. The span of a prefix operation is the
    /// span of its operand, the operators are found in the source: the expression has no span
    /// when they are not right before the operand, e.g. `!(a)`.
    fn key_span(&self, key: &RecordKey) -> Option<Span> {
        let (mut span, source) = self.span(key.low, key.high)?;
        for _ in 0..key.prefixes {
            let before = source.get(..span.start as usize)?.trim_end();
            if !before.ends_with(['!', '-', '~']) {
                return None;
            }
            span.start = u32::try_from(before.len() - 1).ok()?;
        }
        Some(span)
    }
}

/// The types recorded for an expression.
#[derive(Debug, Default)]
struct Recorded {
    inferred: Option<TypeHint>,
    expected: Option<TypeHint>,
}

impl Recorded {
    fn new(recorded: &RecordedTypes, pool: &ConstantPool) -> Self {
        Self {
            inferred: recorded.inferred.as_ref().and_then(|typ| hint(typ, pool)),
            expected: recorded.expected.as_ref().and_then(|typ| hint(typ, pool)),
        }
    }
}

/// Describes a type of the 0.5 type checker. Strong references are implicit, like in the types
/// of the 1.x compiler.
fn hint(typ: &TypeId, pool: &ConstantPool) -> Option<TypeHint> {
    let name = |index: PoolIndex<()>| pool.def_name(index).ok().map(|name| name.to_string());
    let res = match typ {
        TypeId::Prim(index) => TypeHint::Primitive(name(index.cast())?),
        TypeId::Class(index) => TypeHint::Class(name(index.cast())?),
        TypeId::Struct(index) => TypeHint::Struct(name(index.cast())?),
        TypeId::Enum(index) => TypeHint::Enum(name(index.cast())?),
        TypeId::Ref(inner) => hint(inner, pool)?,
        TypeId::WeakRef(inner) => TypeHint::Ref(RefKind::Weak, hint(inner, pool)?.into()),
        TypeId::ScriptRef(inner) => TypeHint::Ref(RefKind::Script, hint(inner, pool)?.into()),
        TypeId::Array(inner) => TypeHint::Array(hint(inner, pool)?.into()),
        TypeId::StaticArray(inner, size) => TypeHint::StaticArray(hint(inner, pool)?.into(), *size as usize),
        TypeId::Variant => TypeHint::Variant,
        TypeId::Void => TypeHint::Primitive("Void".to_owned()),
        TypeId::Null => return None,
    };
    Some(res)
}

/// A named type of the constant pool.
#[derive(Debug, Clone, Copy)]
enum NamedType {
    Class(PoolIndex<Class>),
    Enum(PoolIndex<Enum>),
    Other,
}

/// A [`TypeQuery`] answering from the types recorded while type-checking the sources and from
/// the constant pool they were checked against.
pub struct TypeOracle {
    pool: ConstantPool,
    types: HashMap<Span, Recorded>,
    /// Definitions from this index on are declared by the sources.
    first_source: u32,
    /// The types by qualified name, in the order of the pool.
    named: Vec<(String, NamedType)>,
    by_name: HashMap<String, NamedType>,
}

impl TypeOracle {
    fn new(pool: ConstantPool, types: HashMap<Span, Recorded>, first_source: u32) -> Self {
        let named: Vec<_> = pool
            .roots()
            .filter_map(|(index, def)| {
                let typ = match &def.value {
                    AnyDefinition::Class(_) => NamedType::Class(index.cast()),
                    AnyDefinition::Enum(_) => NamedType::Enum(index.cast()),
                    AnyDefinition::Type(Type::Prim) => NamedType::Other,
                    _ => return None,
                };
                Some((pool.names.get(def.name).ok()?.to_string(), typ))
            })
            .collect();
        let mut by_name = HashMap::new();
        for (name, typ) in &named {
            by_name.entry(name.clone()).or_insert(*typ);
        }
        Self {
            pool,
            types,
            first_source,
            named,
            by_name,
        }
    }

    fn is_from_sources<A>(&self, index: PoolIndex<A>) -> bool {
        u32::from(index) >= self.first_source
    }

    /// A class or struct by its qualified name.
    fn class_id(&self, name: &str) -> Option<PoolIndex<Class>> {
        match self.by_name.get(name)? {
            NamedType::Class(index) => Some(*index),
            NamedType::Enum(_) | NamedType::Other => None,
        }
    }

    /// A class or struct by its qualified name, or else by its name without the module path.
    fn class_named(&self, name: &str) -> Option<PoolIndex<Class>> {
        if let Some(typ) = self.by_name.get(name) {
            return match typ {
                NamedType::Class(index) => Some(*index),
                NamedType::Enum(_) | NamedType::Other => None,
            };
        }
        self.named.iter().find_map(|(qualified, typ)| match typ {
            NamedType::Class(index) if qualified.strip_suffix(name).is_some_and(|module| module.ends_with('.')) => {
                Some(*index)
            }
            _ => None,
        })
    }

    /// `class` and its bases, from the most derived.
    fn hierarchy(&self, class: PoolIndex<Class>) -> impl Iterator<Item = (PoolIndex<Class>, &Class)> {
        let mut current = class;
        let mut seen = HashSet::new();
        std::iter::from_fn(move || {
            if current.is_undefined() || !seen.insert(current) {
                return None;
            }
            let index = current;
            let class = self.pool.class(index).ok()?;
            current = class.base;
            Some((index, class))
        })
    }

    fn methods<'a>(&'a self, class: &'a Class, name: &'a str) -> impl Iterator<Item = &'a Function> + 'a {
        class.functions.iter().filter_map(move |&index| {
            let raw = self.pool.def_name(index).ok()?;
            (FunctionSignature::from_raw(&raw).name() == name)
                .then(|| self.pool.function(index).ok())
                .flatten()
        })
    }

    /// Whether `class` declares a method `name` that can be overridden.
    fn has_instance_method(&self, class: &Class, name: &str) -> bool {
        self.methods(class, name)
            .any(|func| !func.flags.is_static() && !func.flags.is_callback())
    }

    /// Whether `typ` is the class `class`, possibly through a strong reference.
    fn is_type_of(&self, typ: PoolIndex<Type>, class: PoolIndex<Class>) -> bool {
        let typ = match self.pool.type_(typ) {
            Ok(Type::Ref(inner)) => *inner,
            Ok(_) => typ,
            Err(_) => return false,
        };
        matches!(self.pool.type_(typ), Ok(Type::Class))
            && self.pool.def_name(typ).ok() == self.pool.def_name(class).ok()
    }
}

impl TypeQuery for TypeOracle {
    fn type_of(&self, span: Span) -> Option<TypeHint> {
        self.types.get(&span)?.inferred.clone()
    }

    fn expected_type_of(&self, span: Span) -> Option<TypeHint> {
        self.types.get(&span)?.expected.clone()
    }

    fn is_declared_in_sources(&self, type_name: &str) -> bool {
        match self.by_name.get(type_name) {
            Some(NamedType::Enum(index)) => self.is_from_sources(*index),
            _ => true,
        }
    }

    fn has_static_method(&self, type_name: &str, method: &str) -> bool {
        let Some(id) = self.class_id(type_name) else {
            return false;
        };
        let Ok(class) = self.pool.class(id) else {
            return false;
        };
        self.methods(class, method).any(|func| {
            let [param] = func.parameters[..] else {
                return false;
            };
            let returns_string = func.return_type.is_some_and(|ret| {
                matches!(self.pool.type_(ret), Ok(Type::Prim))
                    && self.pool.def_name(ret).is_ok_and(|name| &*name == "String")
            });
            func.flags.is_static()
                && returns_string
                && self
                    .pool
                    .parameter(param)
                    .is_ok_and(|param| self.is_type_of(param.type_, id))
        })
    }

    fn class_has_field(&self, class: &str, field: &str) -> Option<bool> {
        let id = self.class_named(class)?;
        let found = self.hierarchy(id).any(|(_, class)| {
            class
                .fields
                .iter()
                .any(|&index| self.pool.def_name(index).is_ok_and(|name| &*name == field))
        });
        Some(found)
    }

    fn base_has_method(&self, class: &str, method: &str) -> Option<bool> {
        let id = self.class_id(class)?;
        let res = self
            .hierarchy(id)
            .skip(1)
            .any(|(_, class)| self.has_instance_method(class, method));
        Some(res)
    }

    fn is_overridden_in_sources(&self, class: &str, method: &str) -> Option<bool> {
        let id = self.class_id(class)?;
        let res = self.named.iter().any(|(_, typ)| match typ {
            NamedType::Class(sub) => {
                self.is_from_sources(*sub)
                    && self
                        .pool
                        .class(*sub)
                        .is_ok_and(|class| self.has_instance_method(class, method))
                    && self.hierarchy(*sub).skip(1).any(|(base, _)| base == id)
            }
            NamedType::Enum(_) | NamedType::Other => false,
        });
        Some(res)
    }
}

/// The name of the symbol a diagnostic reports as missing, `Type.member` for members.
fn unresolved_name(cause: &Cause) -> Option<String> {
    let name = match cause {
        Cause::FunctionNotFound(name) if BinOp::from_str(name).is_ok() || UnOp::from_str(name).is_ok() => {
            return None;
        }
        Cause::FunctionNotFound(name)
        | Cause::UnresolvedReference(name)
        | Cause::UnresolvedType(name)
        | Cause::ClassNotFound(name)
        | Cause::UnresolvedImport(name) => name.to_string(),
        Cause::UnresolvedModule(path) => format!("{path}.*"),
        // a member of the missing base of a class
        Cause::MemberNotFound(_, typ) | Cause::MethodNotFound(_, typ) if &**typ == "None" => return None,
        Cause::MemberNotFound(member, typ) | Cause::MethodNotFound(member, typ) => {
            let simple = typ.rsplit_once('.').map_or(&**typ, |(_, name)| name);
            format!("{simple}.{member}")
        }
        _ => return None,
    };
    Some(name)
}

/// Collects the type errors: names that do not resolve against the game scripts and the
/// sources are reported as unsupported APIs, other errors are only counted. A diagnostic
/// reported more than once (arguments are checked again after a failed overload resolution) is
/// only taken into account once.
fn type_check_summary(diagnostics: &[Diagnostic], layout: &Layout<'_>) -> TypeCheckSummary {
    let mut summary = TypeCheckSummary::default();
    let mut seen = HashSet::new();
    for diagnostic in diagnostics {
        if !diagnostic.is_fatal() || matches!(diagnostic, Diagnostic::SyntaxError(..)) {
            continue;
        }
        let span = diagnostic.span();
        let Some((span, _)) = layout.span(span.low, span.high) else {
            continue;
        };
        let unresolved = match diagnostic {
            // an unresolved name is replaced by `null`, this error follows the one reported for it
            Diagnostic::CompileError(Cause::InvalidMemberAccess(typ), _) if &**typ == "Null" => continue,
            Diagnostic::CompileError(cause, _) => unresolved_name(cause),
            _ => None,
        };
        if !seen.insert((span, diagnostic.to_string())) {
            continue;
        }
        match unresolved {
            Some(name) => summary
                .diagnostics
                .entry(span.file)
                .or_default()
                .push(ConvertDiagnostic {
                    span,
                    severity: Severity::Warning,
                    code: "unsupported-api",
                    message: format!(
                        "`{name}` is not available in the game scripts; REDmod does not load \
                         third-party native plugins"
                    ),
                }),
            None => summary.untyped += 1,
        }
    }
    summary
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use redscript::bundle::ScriptBundle;
    use redscript_compiler::parser::parse_str;

    use super::*;

    const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

    /// Stands for the game scripts.
    const GAME: &str = "public abstract native class IScriptable {
  public native func Root();
}

public native class GameObject extends IScriptable {
  public native func Base();
}

public native struct GameTime {
  public static native func ToString(self: GameTime) -> String;
}

public enum GameMode { A = 0 }
";

    /// Type-checks `sources` against the predefined types and [`GAME`], returns the oracle, the
    /// summary and the ids of the files.
    fn check(sources: &[&str]) -> (TypeOracle, TypeCheckSummary, Vec<FileId>) {
        let mut bundle = ScriptBundle::load(&mut Cursor::new(PREDEF)).unwrap();
        let game = parse_str(GAME).unwrap();
        let output = CompilationUnit::new(&mut bundle.pool, vec![])
            .unwrap()
            .compile(vec![game], &Files::default())
            .unwrap();
        assert!(
            !output.diagnostics().iter().any(Diagnostic::is_fatal),
            "{:?}",
            output.diagnostics()
        );
        // the implicit base of a class is IScriptable, including IScriptable itself
        let root = bundle
            .pool
            .roots()
            .find(|(_, def)| {
                matches!(def.value, AnyDefinition::Class(_))
                    && bundle.pool.names.get(def.name).is_ok_and(|n| &*n == "IScriptable")
            })
            .map(|(index, _)| index.cast())
            .unwrap();
        bundle.pool.class_mut(root).unwrap().base = PoolIndex::UNDEFINED;

        let map = SourceMap::new();
        let ids: Vec<_> = sources
            .iter()
            .enumerate()
            .map(|(i, src)| map.push_back(format!("file{i}.reds"), *src))
            .collect();
        let (oracle, summary) = type_check(&map, &ids, bundle.pool).unwrap();
        (oracle, summary, ids)
    }

    /// The span of the `nth` occurrence of `needle` in `src`.
    fn span_at(file: FileId, src: &str, needle: &str, nth: usize) -> Span {
        let (start, _) = src.match_indices(needle).nth(nth).unwrap();
        let start = start as u32;
        Span::new(start, start + needle.len() as u32, file)
    }

    fn span_of(file: FileId, src: &str, needle: &str) -> Span {
        span_at(file, src, needle, 0)
    }

    /// The names reported as unsupported APIs in `file`, in source order.
    fn unsupported(summary: &TypeCheckSummary, file: FileId) -> Vec<String> {
        let mut diagnostics = summary.diagnostics.get(&file).cloned().unwrap_or_default();
        diagnostics.sort_by_key(|d| d.span.start);
        diagnostics
            .iter()
            .map(|d| {
                assert_eq!(d.code, "unsupported-api");
                d.message.split('`').nth(1).unwrap().to_owned()
            })
            .collect()
    }

    fn prim(name: &str) -> Option<TypeHint> {
        Some(TypeHint::Primitive(name.to_owned()))
    }

    fn class(name: &str) -> Option<TypeHint> {
        Some(TypeHint::Class(name.to_owned()))
    }

    const OPERATORS: &str = "\
native func OperatorLogicNot(a: Bool) -> Bool;
native func OperatorAdd(a: Int32, b: Int32) -> Int32;
";

    const HELPER: &str = "\
class Helper {
  public let items: array<Float>;
  public func Count() -> Int32 = 1;
}

enum Mode { A = 0, B = 1 }

func MakeHelper() -> ref<Helper> = new Helper();

func TakeF(x: Float) {}
";

    const MAIN: &str = "\
import Codeware.UI.*

func Main(w: wref<Helper>, flag: Bool) -> Int32 {
  let h = MakeHelper();
  let n = h.Count();
  let m = Mode.B;
  let i = h.items;
  let b = !flag;
  let weak = w;
  CodewareOnly(n);
  Helper.Missing(n);
  TakeF(7);
  let f: Float = 2.5;
  f = 3.5;
  return n + 10;
}
";

    #[test]
    fn fixture_is_typed_against_its_own_declarations() {
        let (oracle, summary, ids) = check(&[OPERATORS, HELPER, MAIN]);
        let (helper, main) = (ids[1], ids[2]);

        assert_eq!(oracle.type_of(span_of(main, MAIN, "MakeHelper()")), class("Helper"));
        assert_eq!(oracle.type_of(span_of(main, MAIN, "h.Count()")), prim("Int32"));
        assert_eq!(
            oracle.type_of(span_of(main, MAIN, "Mode.B")),
            Some(TypeHint::Enum("Mode".to_owned()))
        );
        assert_eq!(
            oracle.type_of(span_of(main, MAIN, "h.items")),
            Some(TypeHint::Array(TypeHint::Primitive("Float".to_owned()).into()))
        );
        assert_eq!(
            oracle.type_of(span_of(main, MAIN, "w;").into_head(1)),
            Some(TypeHint::Ref(
                RefKind::Weak,
                TypeHint::Class("Helper".to_owned()).into()
            ))
        );
        // the span of a prefix operation includes its operator
        assert_eq!(oracle.type_of(span_of(main, MAIN, "!flag")), prim("Bool"));
        assert_eq!(oracle.type_of(span_at(main, MAIN, "flag", 1)), prim("Bool"));

        // a returned value, in another file
        assert_eq!(
            oracle.expected_type_of(span_of(helper, HELPER, "new Helper()")),
            class("Helper")
        );
        // a call argument
        assert_eq!(oracle.expected_type_of(span_of(main, MAIN, "7")), prim("Float"));
        // an assignment and a typed local
        assert_eq!(oracle.expected_type_of(span_of(main, MAIN, "3.5")), prim("Float"));
        assert_eq!(oracle.expected_type_of(span_of(main, MAIN, "2.5")), prim("Float"));
        // the operands of an operator, and a returned value
        assert_eq!(oracle.expected_type_of(span_of(main, MAIN, "10")), prim("Int32"));
        assert_eq!(oracle.expected_type_of(span_of(main, MAIN, "n + 10")), prim("Int32"));
        assert_eq!(oracle.type_of(span_of(main, MAIN, "n + 10")), prim("Int32"));
        // spans of something else than an expression
        assert_eq!(oracle.type_of(span_of(main, MAIN, "MakeHelper")), None);
        assert_eq!(oracle.type_of(span_of(helper, HELPER, "MakeHelper()")), None);

        assert_eq!(summary.untyped, 0, "{:?}", summary.diagnostics);
        assert_eq!(
            unsupported(&summary, main),
            ["Codeware.UI.*", "CodewareOnly", "Helper.Missing"]
        );
        let codeware = summary.diagnostics[&main]
            .iter()
            .find(|d| d.message.contains("`CodewareOnly`"))
            .unwrap();
        assert_eq!(codeware.span.start, span_of(main, MAIN, "CodewareOnly").start);
        assert_eq!(
            codeware.message,
            "`CodewareOnly` is not available in the game scripts; REDmod does not load \
             third-party native plugins"
        );
        assert!(!summary.diagnostics.contains_key(&helper));
    }

    trait Head {
        fn into_head(self, len: u32) -> Self;
    }

    impl Head for Span {
        /// The first `len` bytes of the span.
        fn into_head(self, len: u32) -> Self {
            Span::new(self.start, self.start + len, self.file)
        }
    }

    #[test]
    fn locals_of_failed_statements_are_not_unsupported_apis() {
        let src = "func TakeI(i: Int32) {}

func T() {
  let ok: Int32 = \"str\";
  TakeI(ok);
  let ok2 = Missing();
  TakeI(ok2);
  let after = 1;
}
";
        let (oracle, summary, ids) = check(&[src]);
        assert_eq!(unsupported(&summary, ids[0]), ["Missing"]);
        assert_eq!(summary.untyped, 1, "{:?}", summary.diagnostics);
        // the checking goes on after a failed statement, without guessing the types it declares
        assert_eq!(oracle.type_of(span_of(ids[0], src, "1")), prim("Int32"));
        assert_eq!(oracle.type_of(span_at(ids[0], src, "ok2", 1)), None);
        assert_eq!(oracle.expected_type_of(span_at(ids[0], src, "ok2", 1)), prim("Int32"));
    }

    #[test]
    fn types_that_do_not_resolve_are_unknown() {
        let src = "func Make() -> ref<Missing> = null;

func Take(m: ref<Missing>) -> ref<Missing> {
  let a = Make();
  let b = m;
  Take(a);
  return m;
}
";
        let (oracle, summary, ids) = check(&[src]);
        let file = ids[0];
        assert_eq!(oracle.type_of(span_of(file, src, "Make()")), None);
        let arg = span_of(file, src, "a);").into_head(1);
        let returned = span_at(file, src, "m;", 1).into_head(1);
        assert_eq!(oracle.type_of(span_of(file, src, "m;").into_head(1)), None);
        assert_eq!(oracle.type_of(arg), None);
        assert_eq!(oracle.expected_type_of(arg), None);
        assert_eq!(oracle.type_of(returned), None);
        assert_eq!(oracle.expected_type_of(returned), None);
        // one per declaration, which is the span of the types it uses
        assert_eq!(unsupported(&summary, file), ["Missing"; 2]);
        assert_eq!(summary.untyped, 0, "{:?}", summary.diagnostics);
    }

    #[test]
    fn operator_mismatches_are_not_unsupported_apis() {
        let src = "func T() {
  let fl: Float = 1.0;
  fl += 4;
}
";
        let (_, summary, _) = check(&[src]);
        assert!(summary.diagnostics.is_empty(), "{:?}", summary.diagnostics);
        assert_eq!(summary.untyped, 1);
    }

    #[test]
    fn class_fields_are_looked_up_in_bases() {
        let src = "module Mod.Sub
class Base {
  let owner: Int32;
}

class Derived extends Base {
  let own: Int32;
}

@addField(Base)
let added: Int32;
";
        let (oracle, _, _) = check(&[src]);
        assert_eq!(oracle.class_has_field("Derived", "own"), Some(true));
        assert_eq!(oracle.class_has_field("Derived", "owner"), Some(true));
        assert_eq!(oracle.class_has_field("Mod.Sub.Derived", "added"), Some(true));
        assert_eq!(oracle.class_has_field("Base", "own"), Some(false));
        assert_eq!(oracle.class_has_field("Sub.Derived", "own"), Some(true));
        assert_eq!(oracle.class_has_field("Missing", "own"), None);
        assert_eq!(oracle.class_has_field("Derive", "own"), None);
    }

    #[test]
    fn hierarchy_spans_files_and_the_game() {
        let base = "module M
public class Base extends IScriptable {
  func F() {}
  func G() {}
  static func S() {}
}
";
        let sub = "module N
import M.Base

class Sub extends Base {
  func F() {}
  static func S() {}
}
";
        let (oracle, summary, _) = check(&[base, sub]);
        assert!(summary.diagnostics.is_empty(), "{:?}", summary.diagnostics);
        assert_eq!(summary.untyped, 0);
        assert_eq!(oracle.base_has_method("N.Sub", "F"), Some(true));
        assert_eq!(oracle.base_has_method("N.Sub", "S"), Some(false));
        assert_eq!(oracle.base_has_method("M.Base", "F"), Some(false));
        assert_eq!(oracle.is_overridden_in_sources("M.Base", "F"), Some(true));
        assert_eq!(oracle.is_overridden_in_sources("M.Base", "G"), Some(false));
        assert_eq!(oracle.is_overridden_in_sources("M.Base", "S"), Some(false));
        assert_eq!(oracle.is_overridden_in_sources("IScriptable", "F"), Some(true));
        assert_eq!(oracle.is_overridden_in_sources("GameObject", "F"), Some(false));
        assert_eq!(oracle.base_has_method("GameObject", "Root"), Some(true));
        assert_eq!(oracle.base_has_method("GameObject", "Base"), Some(false));
        assert_eq!(oracle.base_has_method("Missing", "F"), None);
    }

    #[test]
    fn static_methods_and_enums_are_found() {
        let src = "struct Time {
  let seconds: Int32;
  public static func ToString(self: Time) -> String = \"\";
  public static func Other(self: Time) -> Int32 = 0;
}

enum Mode { A = 0 }
";
        let (oracle, _, _) = check(&[src]);
        assert!(oracle.has_static_method("Time", "ToString"));
        assert!(!oracle.has_static_method("Time", "Other"));
        assert!(!oracle.has_static_method("Time", "Missing"));
        assert!(!oracle.has_static_method("Missing", "ToString"));
        assert!(oracle.has_static_method("GameTime", "ToString"));
        assert!(oracle.is_declared_in_sources("Mode"));
        assert!(!oracle.is_declared_in_sources("GameMode"));
        assert!(oracle.is_declared_in_sources("Unknown"));
    }

    #[test]
    fn byte_order_marks_are_skipped() {
        let src = "\u{feff}func F() -> Int32 = 1 + 2;
native func OperatorAdd(a: Int32, b: Int32) -> Int32;
";
        let (oracle, summary, ids) = check(&[src]);
        assert_eq!(summary.untyped, 0);
        assert_eq!(oracle.type_of(span_of(ids[0], src, "1 + 2")), prim("Int32"));
    }
}
