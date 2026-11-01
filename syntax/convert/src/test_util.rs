use hashbrown::{HashMap, HashSet};
use redscript_ast::{Dialect, FileId, SourceMap, SourceModule, Span};
use redscript_formatter::{
    FormatSettings, ParseOptions, parse_document_with_options, print_document,
};

use crate::{
    BaseMethodInfo, ConvertDiagnostic, ConvertOptions, Cx, NameArena, SourceFields, TypeHint,
    TypeQuery,
};

/// The id of the file the test sources are parsed as.
pub fn test_file_id() -> FileId {
    SourceMap::new().push_back("test", "")
}

/// The span of the `n`-th occurrence of `needle` in the test source `src`.
pub fn span_of(src: &str, needle: &str, n: usize) -> Span {
    let (start, _) = src
        .match_indices(needle)
        .nth(n)
        .unwrap_or_else(|| panic!("`{needle}` not found in the source"));
    let start = u32::try_from(start).unwrap();
    let end = start + u32::try_from(needle.len()).unwrap();
    Span::new(start, end, test_file_id())
}

/// The span of the first occurrence of `needle` inside the `n`-th occurrence of `context` in the
/// test source `src`.
pub fn span_in(src: &str, context: &str, n: usize, needle: &str) -> Span {
    let outer = span_of(src, context, n);
    let offset = context
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` not found in `{context}`"));
    let start = outer.start + u32::try_from(offset).unwrap();
    let end = start + u32::try_from(needle.len()).unwrap();
    Span::new(start, end, outer.file)
}

/// A [`TypeQuery`] answering from fixed maps, standing in for the compiler in tests.
#[derive(Debug, Default)]
pub struct MapTypeQuery {
    types: HashMap<Span, TypeHint>,
    expected: HashMap<Span, TypeHint>,
    static_methods: HashSet<(String, String)>,
    game_types: HashSet<String>,
    class_fields: HashMap<String, HashSet<String>>,
    /// Classes known to the oracle, with their base class and non-static methods.
    classes: HashMap<String, (Option<String>, HashSet<String>)>,
    /// Classes declared by the converted sources.
    source_classes: HashSet<String>,
    /// `const` methods, by class and name.
    const_methods: HashSet<(String, String)>,
    /// What [`TypeQuery::base_method_info`] says about methods, by declaring class and name.
    method_infos: HashMap<(String, String), BaseMethodInfo>,
    /// What [`TypeQuery::is_import_only`] says about classes, by name.
    import_only: HashMap<String, bool>,
    /// What [`TypeQuery::struct_fields`] says about structs, by name.
    struct_fields: HashMap<String, Vec<String>>,
    /// What [`TypeQuery::method_is_const`] says about methods, by class and name.
    method_consts: HashMap<(String, String), bool>,
}

impl MapTypeQuery {
    pub fn new() -> Self {
        Self::default()
    }

    /// Answers `hint` as the type of the expression at `span`.
    pub fn with_type(mut self, span: Span, hint: TypeHint) -> Self {
        self.types.insert(span, hint);
        self
    }

    /// Answers `hint` as the expected type of the expression at `span`.
    pub fn with_expected(mut self, span: Span, hint: TypeHint) -> Self {
        self.expected.insert(span, hint);
        self
    }
}

impl MapTypeQuery {
    /// Declares that `type_name` comes from the game scripts rather than the converted sources.
    pub fn with_game_type(mut self, type_name: &str) -> Self {
        self.game_types.insert(type_name.to_owned());
        self
    }

    /// Declares that `class` is known and has a field `field`, possibly inherited.
    pub fn with_class_field(mut self, class: &str, field: &str) -> Self {
        self.class_fields
            .entry(class.to_owned())
            .or_default()
            .insert(field.to_owned());
        self
    }

    /// Declares a class with its base class and non-static methods. `game` classes come from the
    /// game scripts rather than the converted sources.
    pub fn with_class(
        mut self,
        name: &str,
        base: Option<&str>,
        methods: &[&str],
        game: bool,
    ) -> Self {
        let methods = methods.iter().map(|&m| m.to_owned()).collect();
        self.classes
            .insert(name.to_owned(), (base.map(str::to_owned), methods));
        if !game {
            self.source_classes.insert(name.to_owned());
        }
        self
    }

    /// Declares that the method `method` of `class` is `const`.
    pub fn with_const_method(mut self, class: &str, method: &str) -> Self {
        self.const_methods
            .insert((class.to_owned(), method.to_owned()));
        self
    }

    /// Declares what [`TypeQuery::base_method_info`] says about the method `method` of `class`,
    /// for the overrides in its subclasses.
    pub fn with_method_info(mut self, class: &str, method: &str, info: BaseMethodInfo) -> Self {
        self.method_infos
            .insert((class.to_owned(), method.to_owned()), info);
        self
    }

    /// Declares whether the class `class` is `importonly`.
    pub fn with_import_only(mut self, class: &str, import_only: bool) -> Self {
        self.import_only.insert(class.to_owned(), import_only);
        self
    }

    /// Declares a struct with its fields, in declaration order.
    pub fn with_struct(mut self, name: &str, fields: &[&str]) -> Self {
        let fields = fields.iter().map(|&f| f.to_owned()).collect();
        self.struct_fields.insert(name.to_owned(), fields);
        self
    }

    /// Declares whether the method `method` of `class` is `const`.
    pub fn with_method_const(mut self, class: &str, method: &str, is_const: bool) -> Self {
        self.method_consts
            .insert((class.to_owned(), method.to_owned()), is_const);
        self
    }

    /// The bases of `class`, nearest first, if it's known.
    fn bases(&self, class: &str) -> Option<Vec<&str>> {
        let mut base = self.classes.get(class)?.0.as_deref();
        let mut bases = vec![];
        while let Some(name) = base.filter(|name| !bases.contains(name)) {
            bases.push(name);
            base = self.classes.get(name).and_then(|(b, _)| b.as_deref());
        }
        Some(bases)
    }

    /// Declares that `type_name` has a static `method(self: type_name) -> String`.
    pub fn with_static_method(mut self, type_name: &str, method: &str) -> Self {
        self.static_methods
            .insert((type_name.to_owned(), method.to_owned()));
        self
    }
}

impl TypeQuery for MapTypeQuery {
    fn is_declared_in_sources(&self, type_name: &str) -> bool {
        !self.game_types.contains(type_name)
    }

    fn has_static_method(&self, type_name: &str, method: &str) -> bool {
        self.static_methods
            .contains(&(type_name.to_owned(), method.to_owned()))
    }

    fn class_has_field(&self, class: &str, field: &str) -> Option<bool> {
        self.class_fields
            .get(class)
            .map(|fields| fields.contains(field))
    }

    fn base_has_method(&self, class: &str, method: &str) -> Option<bool> {
        let bases = self.bases(class)?;
        Some(bases.iter().any(|base| {
            self.classes
                .get(*base)
                .is_some_and(|(_, methods)| methods.contains(method))
        }))
    }

    fn base_method_is_const(&self, class: &str, method: &str) -> Option<bool> {
        let bases = self.bases(class)?;
        Some(bases.iter().any(|base| {
            self.const_methods
                .contains(&((*base).to_owned(), method.to_owned()))
        }))
    }

    fn is_overridden_in_sources(&self, class: &str, method: &str) -> Option<bool> {
        self.classes.get(class)?;
        Some(self.source_classes.iter().any(|name| {
            self.bases(name).is_some_and(|bases| bases.contains(&class))
                && self.classes[name.as_str()].1.contains(method)
        }))
    }

    fn base_method_info(&self, class: &str, method: &str) -> Option<BaseMethodInfo> {
        self.bases(class)?.iter().find_map(|base| {
            self.method_infos
                .get(&((*base).to_owned(), method.to_owned()))
                .copied()
        })
    }

    fn is_import_only(&self, class: &str) -> Option<bool> {
        self.import_only.get(class).copied()
    }

    fn struct_fields(&self, name: &str) -> Option<Vec<String>> {
        self.struct_fields.get(name).cloned()
    }

    fn method_is_const(&self, class: &str, method: &str) -> Option<bool> {
        self.method_consts
            .get(&(class.to_owned(), method.to_owned()))
            .copied()
    }

    fn type_of(&self, span: Span) -> Option<TypeHint> {
        self.types.get(&span).cloned()
    }

    fn expected_type_of(&self, span: Span) -> Option<TypeHint> {
        self.expected.get(&span).cloned()
    }
}

/// Parses `src` in the `from` dialect, runs a single pass and prints the result in the `to`
/// dialect.
pub fn run_pass(
    src: &str,
    from: Dialect,
    to: Dialect,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    run_pass_with_types(src, from, to, None, pass)
}

/// Like [`run_pass`], with type information. Spans are relative to [`test_file_id`].
pub fn run_pass_with_types(
    src: &str,
    from: Dialect,
    to: Dialect,
    types: Option<&dyn TypeQuery>,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    let opts = ConvertOptions {
        types,
        ..ConvertOptions::default()
    };
    run_pass_impl(src, from, to, opts, pass)
}

/// Runs a pass converting community code to REDmod with the given options.
pub fn run_pass_with_options(
    src: &str,
    types: Option<&dyn TypeQuery>,
    fields: Option<&SourceFields>,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    let opts = ConvertOptions {
        types,
        fields,
        conditionals: None,
    };
    run_pass_with_convert_options(src, opts, pass)
}

/// Runs a pass converting community code to REDmod with the given conversion options.
pub fn run_pass_with_convert_options(
    src: &str,
    opts: ConvertOptions<'_>,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    run_pass_impl(src, Dialect::Community, Dialect::Redmod, opts, pass)
}

fn run_pass_impl(
    src: &str,
    from: Dialect,
    to: Dialect,
    opts: ConvertOptions<'_>,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    let arena = NameArena::new();
    let id = test_file_id();
    // like the driver, community sources are parsed in the 0.5.x compatibility mode
    let (doc, errors) = parse_document_with_options(src, id, ParseOptions::compat(from));
    assert!(errors.is_empty(), "{errors:?}");
    let mut doc = doc.unwrap();
    let mut diagnostics = vec![];
    let mut cx = Cx {
        arena: &arena,
        diagnostics: &mut diagnostics,
        opts: &opts,
    };
    pass(&mut doc.module, &mut cx);
    for d in &diagnostics {
        assert!(
            !d.message.contains("  "),
            "diagnostic message with a run of spaces: {d}"
        );
    }
    doc.module.dialect = to;
    let settings = FormatSettings {
        dialect: to,
        ..FormatSettings::default()
    };
    let printed = print_document(&doc, &settings).to_string();
    (printed, diagnostics)
}

/// Runs a pass converting community code to REDmod.
pub fn to_redmod(
    src: &str,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    run_pass(src, Dialect::Community, Dialect::Redmod, pass)
}

/// Runs a pass converting community code to REDmod with type information.
pub fn to_redmod_with_types(
    src: &str,
    types: &dyn TypeQuery,
    pass: impl for<'a, 'src> FnOnce(&mut SourceModule<'src>, &mut Cx<'a, 'src>),
) -> (String, Vec<ConvertDiagnostic>) {
    run_pass_with_types(src, Dialect::Community, Dialect::Redmod, Some(types), pass)
}

/// Collapses whitespace so that assertions do not depend on the printer's layout.
pub fn squash(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

pub fn codes(diagnostics: &[ConvertDiagnostic]) -> Vec<&'static str> {
    diagnostics.iter().map(|d| d.code).collect()
}
