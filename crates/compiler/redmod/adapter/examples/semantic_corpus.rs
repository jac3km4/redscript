//! Optional, boot-only semantic survey. See ../CORPUS.md.
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

use redscript_community_ast::{
    self as ast, AstNode, AstVisitor, FileId, SourceMap, Span, WithSpan,
};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::pass::UnusedLocals;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, LoweredFunction, QueryError, Symbols, Type,
    TypeInterner, expr_type, ir, parse_file,
};
use redscript_redmod_adapter::{Error, parse_file_with_metadata, process_sources};

const USAGE: &str = "Usage: cargo run -p redscript-redmod-adapter --locked --example semantic_corpus -- <corpus-directory> [output-directory]\nDependencies: embedded boot library and default symbols only; no game bundle.";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args_os().skip(1);
    let Some(root) = args.next() else {
        return Err(USAGE.into());
    };
    if root == "--help" || root == "-h" {
        println!("{USAGE}");
        return Ok(());
    }
    let output = args
        .next()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("target/redmod-semantic-corpus"));
    if args.next().is_some() {
        return Err(USAGE.into());
    }
    run(Path::new(&root), &output)?;
    Ok(())
}

/// Do not follow symlinks/junctions or honor ignore files: survey the explicit tree.
fn discover(root: &Path) -> io::Result<Vec<PathBuf>> {
    fn visit(path: &Path, paths: &mut Vec<PathBuf>) -> io::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let kind = entry.file_type()?;
            if kind.is_symlink() {
                continue;
            }
            if kind.is_dir() {
                visit(&entry.path(), paths)?;
            } else if kind.is_file()
                && entry
                    .path()
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("script"))
            {
                paths.push(entry.path());
            }
        }
        Ok(())
    }
    if !fs::symlink_metadata(root)?.is_dir() {
        return Err(io::Error::other(
            "corpus must be a directory, not a symlink",
        ));
    }
    let mut paths = Vec::new();
    visit(root, &mut paths)?;
    paths.sort_by_cached_key(|path| {
        let relative = portable(path.strip_prefix(root).expect("discovered below root"));
        (relative.to_ascii_lowercase(), relative)
    });
    if paths.is_empty() {
        return Err(io::Error::other("corpus contains no .script files"));
    }
    Ok(paths)
}

fn portable(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn tsv(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
        .replace('\n', "\\n")
}

#[derive(Debug)]
struct Record {
    stage: &'static str,
    code: &'static str,
    reason: String,
    level: String,
    path: String,
    line: usize,
    column: usize,
    start: u32,
    end: u32,
    message: String,
}

impl Record {
    fn new(
        sources: &SourceMap,
        stage: &'static str,
        code: &'static str,
        reason: String,
        level: String,
        span: Span,
        message: String,
    ) -> io::Result<Self> {
        let file = sources
            .get(span.file)
            .ok_or_else(|| io::Error::other("diagnostic has an unknown source file"))?;
        if span.start > span.end
            || !file.source().is_char_boundary(span.start as usize)
            || !file.source().is_char_boundary(span.end as usize)
        {
            return Err(io::Error::other(format!(
                "invalid diagnostic span in {}: {}..{}",
                file.path().display(),
                span.start,
                span.end
            )));
        }
        let loc = file.lookup(span.start);
        Ok(Self {
            stage,
            code,
            reason,
            level,
            path: portable(file.path()),
            line: loc.line + 1,
            column: loc.col + 1,
            start: span.start,
            end: span.end,
            message,
        })
    }

    fn adapter(sources: &SourceMap, stage: &'static str, error: &Error) -> io::Result<Self> {
        let reason = match error {
            Error::Parse(_) => "Syntax".into(),
            Error::Capability { kind, .. } => format!("{kind:?}"),
            Error::Semantic { kind, .. } => format!("{kind:?}"),
            Error::Condition { kind, .. } => format!("{kind:?}"),
        };
        Self::new(
            sources,
            stage,
            error.code(),
            reason,
            "Error".into(),
            error.span(),
            error.to_string(),
        )
    }
}

/// An inventory of the bridged AST, not of native syntax hidden by a first blocker.
#[derive(Default)]
struct Inventory {
    declarations: usize,
    functions: BTreeMap<Span, (Span, bool)>,
    expressions: BTreeSet<Span>,
}

impl<'src> AstVisitor<'src, WithSpan> for Inventory {
    type Error = std::convert::Infallible;

    fn visit_node(&mut self, node: AstNode<'_, 'src, WithSpan>) -> Result<(), Self::Error> {
        match node {
            AstNode::ItemDecl((decl, span)) => {
                if !matches!(decl.item, ast::Item::Import(_)) {
                    self.declarations += 1;
                }
                if let ast::Item::Function(function) = &decl.item {
                    self.functions
                        .insert(function.name.1, (*span, function.body.is_some()));
                }
            }
            AstNode::Expr((expr, span))
                if span.start < span.end
                    && !matches!(expr, ast::Expr::Omitted | ast::Expr::Error) =>
            {
                self.expressions.insert(*span);
            }
            _ => {}
        }
        Ok(())
    }
}

// Worst observed result wins for a shared written span; never let a successful
// synthetic/coercion observation hide another observation's failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum QueryOutcome {
    Resolved,
    Unresolved,
    Failure,
}

impl QueryOutcome {
    fn name(self) -> &'static str {
        match self {
            Self::Resolved => "resolved",
            Self::Unresolved => "unresolved",
            Self::Failure => "failure",
        }
    }
}

#[derive(Default)]
struct QueryObservation {
    outcomes: BTreeSet<QueryOutcome>,
    reasons: BTreeSet<&'static str>,
}

impl QueryObservation {
    fn outcome(&self) -> QueryOutcome {
        *self.outcomes.last().expect("at least one observation")
    }
}

fn query_result(result: Result<Type<'_>, QueryError<'_>>) -> (QueryOutcome, &'static str) {
    use QueryError as E;
    match result {
        Ok(Type::Nothing) => (QueryOutcome::Unresolved, "Nothing"),
        Ok(_) => (QueryOutcome::Resolved, "TypeReturned"),
        Err(E::UnknownNullType) => (QueryOutcome::Unresolved, "UnknownNullType"),
        Err(error) => (
            QueryOutcome::Failure,
            match error {
                E::Coalesce(_) => "Coalesce",
                E::Type(_) => "Type",
                E::MissingLocal(_) => "MissingLocal",
                E::MissingType(_) => "MissingType",
                E::MissingFunction(_) => "MissingFunction",
                E::MissingMethod(_) => "MissingMethod",
                E::MissingField(_) => "MissingField",
                E::MissingTypeArgument(_) => "MissingTypeArgument",
                E::TypeArgumentCount { .. } => "TypeArgumentCount",
                E::InvalidReceiver { .. } => "InvalidReceiver",
                E::InvalidArray(_) => "InvalidArray",
                E::InvalidClosure(_) => "InvalidClosure",
                E::AbsentArgument => "AbsentArgument",
                E::UnknownNullType => unreachable!(),
            },
        ),
    }
}

struct QuerySurvey<'a, 'ctx> {
    function: &'a LoweredFunction<'ctx>,
    symbols: &'a Symbols<'ctx>,
    written: &'a BTreeSet<Span>,
    observations: &'a mut BTreeMap<Span, QueryObservation>,
}

impl<'ctx> ir::Visitor<'ctx> for QuerySurvey<'_, 'ctx> {
    fn visit_expr(&mut self, expr: &ir::Expr<'ctx>) {
        let span = expr.span();
        if !matches!(expr, ir::Expr::Nop(_)) && self.written.contains(&span) {
            let (outcome, reason) = query_result(expr_type(expr, self.function, self.symbols));
            let observation = self.observations.entry(span).or_default();
            observation.outcomes.insert(outcome);
            observation.reasons.insert(reason);
        }
        // Use the public visitor's child traversal, with this hook on every child.
        match expr {
            ir::Expr::Construct { args, .. } => args.iter().for_each(|arg| self.visit_expr(arg)),
            ir::Expr::NewClosure { closure, .. } => self.visit_block(&closure.block),
            ir::Expr::Call { call, span } => self.visit_call(call, *span),
            ir::Expr::Assign { place, expr, .. } => {
                self.visit_expr(place);
                self.visit_expr(expr);
            }
            ir::Expr::Field { receiver, .. } => self.visit_expr(receiver),
            ir::Expr::Index { array, index, .. } => {
                self.visit_expr(array);
                self.visit_expr(index);
            }
            ir::Expr::Conditional {
                condition,
                then,
                else_,
                ..
            } => {
                self.visit_expr(condition);
                self.visit_expr(then);
                self.visit_expr(else_);
            }
            ir::Expr::DynCast { expr, .. } => self.visit_expr(expr),
            ir::Expr::NewInstance { .. }
            | ir::Expr::Nop(_)
            | ir::Expr::Local(..)
            | ir::Expr::Capture(..)
            | ir::Expr::Const(..)
            | ir::Expr::Null { .. } => {}
        }
    }
}

const METRICS: &[&str] = &[
    "ast_available",
    "declarations",
    "function_declarations",
    "signatures_valid",
    "signatures_invalid",
    "signatures_mixed",
    "signatures_unavailable",
    "bodies_parsed",
    "bodies_retained",
    "bodies_unavailable",
    "expressions_written",
    "expressions_mapped",
    "expressions_unmapped",
    "queries_resolved",
    "queries_unresolved",
    "queries_failure",
    "queries_mixed",
];

fn coverage_reports<'ctx>(
    sources: &SourceMap,
    inventories: &BTreeMap<FileId, Option<Inventory>>,
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
) -> (String, String, String) {
    use ir::Visitor;
    let mut signatures = BTreeMap::<Span, BTreeSet<bool>>::new();
    for entry in symbols.free_functions() {
        if let Some(span) = entry.func().span() {
            signatures
                .entry(span)
                .or_default()
                .insert(entry.func().type_().is_valid());
        }
    }
    for (_, typ) in symbols.types() {
        if let Some(aggregate) = typ.schema().as_aggregate() {
            for entry in aggregate.methods().iter() {
                if let Some(span) = entry.func().span() {
                    signatures
                        .entry(span)
                        .or_default()
                        .insert(entry.func().type_().is_valid());
                }
            }
        }
    }
    let bodies: BTreeSet<_> = unit.all_functions().map(|function| function.span).collect();
    let mut observations = BTreeMap::<Span, QueryObservation>::new();
    for function in unit.all_functions() {
        if let Some(Some(inventory)) = inventories.get(&function.span.file) {
            QuerySurvey {
                function,
                symbols,
                written: &inventory.expressions,
                observations: &mut observations,
            }
            .visit_block(&function.block);
        }
    }
    let mut coverage = format!("path\t{}\n", METRICS.join("\t"));
    let mut queries = String::from("path\tstart\tend\toutcome\tmixed\treasons\n");
    let mut totals = [0usize; 17];
    // File IDs follow the deterministic discovery order (boot is not in inventories).
    for (id, inventory) in inventories {
        let path = tsv(&portable(sources.get(*id).expect("corpus file").path()));
        let Some(inventory) = inventory else {
            writeln!(coverage, "{path}\t0\t{}", ["NA"; 16].join("\t")).unwrap();
            continue;
        };
        let mut counts = [0usize; 17];
        counts[0] = 1;
        counts[1] = inventory.declarations;
        counts[2] = inventory.functions.len();
        for (name, (span, has_body)) in &inventory.functions {
            let status = match signatures.get(name) {
                Some(valid) if valid.len() > 1 => 5,
                Some(valid) if valid.contains(&true) => 3,
                Some(_) => 4,
                None => 6,
            };
            counts[status] += 1;
            if *has_body {
                counts[7] += 1;
                counts[if bodies.contains(span) { 8 } else { 9 }] += 1;
            }
        }
        counts[10] = inventory.expressions.len();
        for span in &inventory.expressions {
            if let Some(observation) = observations.get(span) {
                counts[11] += 1;
                let outcome = observation.outcome();
                counts[match outcome {
                    QueryOutcome::Resolved => 13,
                    QueryOutcome::Unresolved => 14,
                    QueryOutcome::Failure => 15,
                }] += 1;
                counts[16] += usize::from(observation.outcomes.len() > 1);
                writeln!(
                    queries,
                    "{path}\t{}\t{}\t{}\t{}\t{}",
                    span.start,
                    span.end,
                    outcome.name(),
                    observation.outcomes.len() > 1,
                    observation
                        .reasons
                        .iter()
                        .copied()
                        .collect::<Vec<_>>()
                        .join(",")
                )
                .unwrap();
            }
        }
        counts[12] = counts[10] - counts[11];
        for (total, count) in totals.iter_mut().zip(counts) {
            *total += count;
        }
        writeln!(
            coverage,
            "{path}\t{}",
            counts.map(|count| count.to_string()).join("\t")
        )
        .unwrap();
    }
    let mut summary = format!(
        "\n## Observed coverage (not correctness)\n\nFiles without a bridged AST: {} (declaration/body/expression totals unavailable).\n\n| Metric | Count |\n| --- | ---: |\n",
        inventories.len() - totals[0]
    );
    for (metric, total) in METRICS.iter().zip(totals) {
        writeln!(summary, "| {metric} | {total} |").unwrap();
    }
    summary.push_str("\nDeclarations exclude imports and count only available bridged ASTs. Signature validity is the compiler's function-signature flag, not semantic validity. Unavailable signatures have no matching symbol name span. Parsed bodies are explicit AST bodies; retained bodies have an exact declaration-span match in LoweredCompilationUnit (including recovery IR), not accepted/diagnostic-free bodies. Unavailable bodies have no such match; reasons are not inferred. Prototypes and synthesized bodies are outside this denominator.\n\nExpression denominator: distinct nonempty bridged AST expression spans visited in bodies and field/default values, excluding Omitted/Error placeholders. Mapped means an exact-span, non-Nop IR expression was visited in a retained function body. Unmapped is not a query failure (e.g. callee names, folded expressions, defaults, excluded bodies). Boot and IR-only spans are excluded; generated nodes reusing written spans cannot reliably be distinguished. Multiple observations per span are deduplicated; failure takes precedence over unresolved over resolved, and mixed outcomes are counted. Resolved means only that expr_type returned a type other than Nothing; unresolved means Nothing or UnknownNullType; failure means another QueryError. These are query availability observations, NOT type accuracy, official-compiler parity, or runtime guarantees.\n\nSee [per-file coverage](coverage.tsv) and [mapped expression queries](expression_queries.tsv). No partial-analysis mode is enabled.\n");
    (coverage, queries, summary)
}

fn run(root: &Path, output: &Path) -> io::Result<()> {
    let paths = discover(root)?;
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let mut bytes = 0;
    for path in &paths {
        let source = fs::read_to_string(path)
            .map_err(|err| io::Error::other(format!("{}: {err}", path.display())))?;
        bytes += source.len();
        // Relative names keep reports portable; the corpus root is recorded separately.
        sources.push_back(
            path.strip_prefix(root).expect("discovered below root"),
            source,
        );
    }
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let mut dependencies = Vec::new();
    let mut modules = Vec::new();
    let mut records = Vec::new();
    let mut inventories = BTreeMap::new();
    let mut counts = BTreeMap::<&str, usize>::new();
    for stage in [
        "syntax",
        "capability",
        "validation",
        "missing-module",
        "processed",
    ] {
        counts.insert(stage, 0);
    }
    let mut files = String::from("path\tbytes\tstage\tparse_diagnostics\n");
    for (index, (id, file)) in sources.files().enumerate() {
        if index == 0 {
            let module = parse_file(id, file, &mut reporter)
                .ok_or_else(|| io::Error::other("embedded boot library did not parse"))?;
            dependencies.push(module);
            continue;
        }
        let (parsed, errors) = parse_file_with_metadata(file.source(), id);
        let inventory = parsed.as_ref().map(|parsed| {
            let mut inventory = Inventory::default();
            for item in &parsed.module.items {
                inventory
                    .visit_item_decl(item)
                    .expect("infallible inventory");
            }
            inventory
        });
        inventories.insert(id, inventory);
        let stage = if errors.iter().any(|error| matches!(error, Error::Parse(_))) {
            "syntax"
        } else if errors
            .iter()
            .any(|error| matches!(error, Error::Capability { .. }))
        {
            "capability"
        } else if !errors.is_empty() {
            "validation"
        } else if parsed.is_none() {
            "missing-module"
        } else {
            "processed"
        };
        *counts.get_mut(stage).expect("known stage") += 1;
        writeln!(
            files,
            "{}\t{}\t{stage}\t{}",
            tsv(&portable(file.path())),
            file.source().len(),
            errors.len()
        )
        .unwrap();
        for error in &errors {
            records.push(Record::adapter(&sources, stage, error)?);
        }
        // Match the IDE: retain ownership even for empty modules, and exclude
        // every module with parse-stage diagnostics (including validation errors).
        if errors.is_empty()
            && let Some(parsed) = parsed
        {
            modules.push(parsed);
        }
    }
    if !reporter.reported().is_empty() {
        return Err(io::Error::other(
            "embedded boot library has syntax diagnostics",
        ));
    }
    let (unit, symbols, errors) = process_sources(
        modules,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    unit.run_diagnostics(&[&UnusedLocals], &mut reporter);
    let adapter_errors = errors.len();
    for error in &errors {
        records.push(Record::adapter(&sources, "adapter-processing", error)?);
    }
    let mut shared_errors = 0;
    let mut shared_warnings = 0;
    let mut shared_pedantic = 0;
    for diagnostic in reporter.reported() {
        use redscript_compiler_frontend::DiagnosticLevel;
        match diagnostic.level() {
            DiagnosticLevel::Error | DiagnosticLevel::ErrorAllowedAtRuntime => shared_errors += 1,
            DiagnosticLevel::Warning => shared_warnings += 1,
            DiagnosticLevel::Pedantic => shared_pedantic += 1,
        }
        records.push(Record::new(
            &sources,
            "shared",
            diagnostic.code(),
            // Shared typed diagnostic codes, not message text, are the grouping key.
            String::new(),
            format!("{:?}", diagnostic.level()),
            diagnostic.span(),
            diagnostic.to_string(),
        )?);
    }
    records.sort_by(|a, b| {
        (
            &a.stage, &a.code, &a.reason, &a.level, &a.path, a.start, a.end, &a.message,
        )
            .cmp(&(
                &b.stage, &b.code, &b.reason, &b.level, &b.path, b.start, b.end, &b.message,
            ))
    });
    let mut summary = format!(
        "# REDmod semantic corpus (boot only)\n\nCorpus: `{}`\n\nDependencies: embedded `boot.reds` + `Symbols::with_default_types()`. No game bundle.\n\nFiles: {}; UTF-8 bytes: {}.\n\n| Parse-stage outcome | Files |\n| --- | ---: |\n",
        portable(root),
        paths.len(),
        bytes
    );
    for (stage, count) in &counts {
        writeln!(summary, "| {stage} | {count} |").unwrap();
    }
    write!(
        summary,
        "\nAdapter processing errors: {adapter_errors}.\n\nShared errors: {shared_errors}; warnings: {shared_warnings}; pedantic: {shared_pedantic}.\n\nAll eligible modules were processed together, with the IDE's unused-local pass.\nCapability counts are first blockers per file; validation may report several errors.\nExcluded declarations and absent engine definitions cause unresolved-symbol cascades.\nReturned IR and diagnostic-free files are NOT semantic passes or runtime guarantees.\n\nSee [grouped diagnostics](aggregate.tsv), [per-file outcomes](files.tsv),\n[all diagnostics](diagnostics.tsv), and [full log](diagnostics.log).\nReduce findings to authored fixtures under `crates/compiler/redmod/adapter/tests/data/`;\nsee the adapter CORPUS.md for reproduction guidance and fixture links.\n"
    )
    .unwrap();

    let (coverage, queries, coverage_summary) =
        coverage_reports(&sources, &inventories, &unit, &symbols);
    summary.push_str(&coverage_summary);
    let (diagnostics, aggregate, log) = reports(&records);
    fs::create_dir_all(output)?;
    for (name, contents) in [
        ("summary.md", summary.as_str()),
        ("files.tsv", files.as_str()),
        ("diagnostics.tsv", diagnostics.as_str()),
        ("aggregate.tsv", aggregate.as_str()),
        ("diagnostics.log", log.as_str()),
        ("coverage.tsv", coverage.as_str()),
        ("expression_queries.tsv", queries.as_str()),
    ] {
        fs::write(output.join(name), contents)?;
    }
    println!("{summary}\nReports: {}", output.display());
    Ok(())
}

fn reports(records: &[Record]) -> (String, String, String) {
    let mut diagnostics =
        String::from("stage\tcode\treason\tlevel\tpath\tline\tcolumn\tstart\tend\tmessage\n");
    let mut log = String::new();
    let mut groups = BTreeMap::<_, Vec<_>>::new();
    for record in records {
        let Record {
            stage,
            code,
            reason,
            level,
            path,
            line,
            column,
            start,
            end,
            message,
        } = record;
        writeln!(
            diagnostics,
            "{stage}\t{code}\t{}\t{level}\t{}\t{line}\t{column}\t{start}\t{end}\t{}",
            tsv(reason),
            tsv(path),
            tsv(message)
        )
        .unwrap();
        writeln!(log, "{path}:{line}:{column}: {level}[{code}] ({stage}/{reason}, bytes {start}..{end}): {message}").unwrap();
        groups
            .entry((stage, code, reason, level))
            .or_default()
            .push(record);
    }
    let mut aggregate = String::from(
        "stage\tcode\treason\tlevel\tdiagnostics\tfiles\texample_locations\texample_message\n",
    );
    for ((stage, code, reason, level), group) in groups {
        let files: BTreeSet<_> = group.iter().map(|record| &record.path).collect();
        let examples = group
            .iter()
            .take(3)
            .map(|record| format!("{}:{}:{}", record.path, record.line, record.column))
            .collect::<Vec<_>>()
            .join("; ");
        writeln!(
            aggregate,
            "{stage}\t{code}\t{}\t{level}\t{}\t{}\t{}\t{}",
            tsv(reason),
            group.len(),
            files.len(),
            tsv(&examples),
            tsv(&group[0].message)
        )
        .unwrap();
    }
    (diagnostics, aggregate, log)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovery_is_case_insensitive_sorted_and_recursive() -> io::Result<()> {
        let root =
            env::temp_dir().join(format!("redmod-semantic-discovery-{}", std::process::id()));
        fs::create_dir(&root)?;
        let result = (|| {
            fs::create_dir(root.join("nested"))?;
            fs::write(root.join(".gitignore"), "*.script\n")?;
            for path in [
                "z.script",
                "A.Script",
                "nested/b.SCRIPT",
                "ignore.reds",
                ".hidden.script",
            ] {
                fs::write(root.join(path), "")?;
            }
            let paths = discover(&root)?;
            let relative: Vec<_> = paths
                .iter()
                .map(|path| portable(path.strip_prefix(&root).unwrap()))
                .collect();
            assert_eq!(
                relative,
                [".hidden.script", "A.Script", "nested/b.SCRIPT", "z.script"]
            );
            assert_eq!(paths, discover(&root)?);
            assert!(discover(&root.join("missing")).is_err());
            assert!(discover(&root.join("z.script")).is_err());
            fs::create_dir(root.join("empty"))?;
            assert!(discover(&root.join("empty")).is_err());
            Ok(())
        })();
        fs::remove_dir_all(&root)?;
        result
    }

    #[test]
    fn survey_filters_invalid_modules_and_resolves_valid_modules_together() -> io::Result<()> {
        let root = env::temp_dir().join(format!("redmod-semantic-survey-{}", std::process::id()));
        fs::create_dir(&root)?;
        let result = (|| {
            for (name, source) in [
                ("a.Script", "function Caller(): Int32 { return Callee(); }"),
                ("b.script", "function Callee(): Int32 { return 1; }"),
                ("empty.script", ""),
                ("syntax.script", "function {"),
                (
                    "capability.script",
                    "class Host { [category = \"Test\"] mutable var value: Int32; }",
                ),
                (
                    "validation.script",
                    "function Invalid(x: MissingType, x: MissingType) {}",
                ),
            ] {
                fs::write(root.join(name), source)?;
            }
            let output = root.join("reports");
            run(&root, &output)?;
            let files = fs::read_to_string(output.join("files.tsv"))?;
            for (name, stage) in [
                ("a.Script", "processed"),
                ("b.script", "processed"),
                ("empty.script", "processed"),
                ("syntax.script", "syntax"),
                ("capability.script", "capability"),
                ("validation.script", "validation"),
            ] {
                assert!(
                    files.lines().any(|line| {
                        let fields: Vec<_> = line.split('\t').collect();
                        fields[0] == name && fields[2] == stage
                    }),
                    "missing {name}/{stage}: {files}"
                );
            }
            let diagnostics = fs::read_to_string(output.join("diagnostics.tsv"))?;
            assert!(!diagnostics.contains("UNRESOLVED_FN"), "{diagnostics}");
            assert!(!diagnostics.contains("UNRESOLVED_TYPE"), "{diagnostics}");
            Ok(())
        })();
        fs::remove_dir_all(&root)?;
        result
    }

    #[test]
    fn coverage_tracks_invalid_signatures_missing_bodies_and_query_outcomes() -> io::Result<()> {
        let root = env::temp_dir().join(format!("redmod-semantic-coverage-{}", std::process::id()));
        fs::create_dir(&root)?;
        let result = (|| {
            fs::write(
                root.join("a.script"),
                "function Good(): Int32 { return 1; } function Bad(x: Missing) { MissingCall(); } function NullValue(): IScriptable { return NULL; } function Recovery() { MissingCall(); }",
            )?;
            fs::write(root.join("blocked.script"), "function {")?;
            fs::write(root.join("empty.script"), "")?;
            let output = root.join("reports");
            run(&root, &output)?;
            let coverage = fs::read_to_string(output.join("coverage.tsv"))?;
            let row: Vec<_> = coverage
                .lines()
                .find(|line| line.starts_with("a.script\t"))
                .unwrap()
                .split('\t')
                .collect();
            let metric = |name| {
                row[1 + METRICS.iter().position(|metric| *metric == name).unwrap()]
                    .parse::<usize>()
                    .unwrap()
            };
            assert_eq!(metric("function_declarations"), 4);
            assert_eq!(metric("signatures_valid"), 3);
            assert_eq!(metric("signatures_invalid"), 1);
            assert_eq!(metric("bodies_parsed"), 4);
            assert_eq!(metric("bodies_retained"), 3);
            assert_eq!(metric("bodies_unavailable"), 1);
            assert!(metric("queries_resolved") > 0);
            assert_eq!(metric("queries_unresolved"), 1);
            assert!(metric("expressions_unmapped") > 0);
            assert!(coverage.contains("blocked.script\t0\tNA"));
            assert!(coverage.contains("empty.script\t1\t0\t0"));
            let queries = fs::read_to_string(output.join("expression_queries.tsv"))?;
            let summary = fs::read_to_string(output.join("summary.md"))?;
            assert!(summary.contains("NOT type accuracy"));
            run(&root, &output)?;
            assert_eq!(coverage, fs::read_to_string(output.join("coverage.tsv"))?);
            assert_eq!(
                queries,
                fs::read_to_string(output.join("expression_queries.tsv"))?
            );
            assert_eq!(summary, fs::read_to_string(output.join("summary.md"))?);
            Ok(())
        })();
        fs::remove_dir_all(&root)?;
        result
    }

    #[test]
    fn queries_deduplicate_with_conservative_precedence() {
        let mut observation = QueryObservation::default();
        for result in [
            Ok(Type::Nothing),
            Ok(Type::nullary(redscript_compiler_frontend::predef::INT32)),
            Err(QueryError::UnknownNullType),
            Err(QueryError::AbsentArgument),
        ] {
            let (outcome, reason) = query_result(result);
            observation.outcomes.insert(outcome);
            observation.reasons.insert(reason);
        }
        assert_eq!(observation.outcome(), QueryOutcome::Failure);
        assert_eq!(observation.outcomes.len(), 3);
        assert_eq!(observation.reasons.len(), 4);
    }

    #[test]
    fn locations_are_one_based_unicode_columns_and_groups_keep_counts() -> io::Result<()> {
        let sources = SourceMap::new();
        let id = sources.push_back("example.Script", "a\néx");
        let record = Record::new(
            &sources,
            "shared",
            "TEST",
            String::new(),
            "Error".into(),
            Span {
                file: id,
                start: 4,
                end: 5,
            },
            "test\nmessage".into(),
        )?;
        assert_eq!((record.line, record.column), (2, 2));
        let (diagnostics, aggregate, log) = reports(&[record]);
        assert!(diagnostics.contains("test\\nmessage"));
        assert!(aggregate.contains("\t1\t1\texample.Script:2:2\t"));
        assert!(log.contains("example.Script:2:2:"));
        Ok(())
    }
}
