//! Conservative source-signature grouping before shared declaration collection.
//! Types are span-free AST spellings; generic/alias equivalence is not inferred here.
use std::collections::{BTreeMap, HashSet};

use redscript_community_ast::{self as ast, Span};
use redscript_compiler_frontend::Evaluator;

use crate::{ConditionError, Error, ParsedModule};

pub fn select<'src>(
    modules: &mut [ParsedModule<'src>],
    dependencies: &[ast::SourceModule<'src>],
    errors: &mut Vec<Error>,
) {
    let evaluator = Evaluator::from_modules(
        dependencies
            .iter()
            .chain(modules.iter().map(|parsed| &parsed.module)),
    );
    let mut selection = Selection {
        evaluator,
        versions: BTreeMap::new(),
        selected: HashSet::new(),
        errors,
    };
    for parsed in modules {
        let namespace = parsed
            .module
            .path
            .as_ref()
            .map(|path| path.segments.join("."))
            .unwrap_or_default();
        selection.select_items(&mut parsed.module.items, &namespace);
    }
    for versions in selection.versions.values() {
        let limit = if versions.iter().any(|(_, added, _)| *added) {
            1
        } else {
            2
        };
        for &(_, _, span) in versions.iter().skip(limit) {
            selection.errors.push(Error::Condition {
                kind: ConditionError::TooManyVersions { limit },
                span,
            });
        }
        if let Some((_, _, span)) = versions.iter().filter(|(positive, _, _)| *positive).nth(1) {
            selection.errors.push(Error::Condition {
                kind: ConditionError::MultiplePositiveVersions,
                span: *span,
            });
        }
    }
}

struct Selection<'a, 'src> {
    evaluator: Evaluator<'src>,
    versions: BTreeMap<String, Vec<(bool, bool, Span)>>,
    selected: HashSet<String>,
    errors: &'a mut Vec<Error>,
}

impl<'src> Selection<'_, 'src> {
    fn select_items(&mut self, items: &mut [(ast::SourceItemDecl<'src>, Span)], namespace: &str) {
        for (decl, _) in items {
            if let ast::Item::Class(aggregate) | ast::Item::Struct(aggregate) = &mut decl.item {
                self.select_items(
                    &mut aggregate.items,
                    &format!("{namespace}.{}", aggregate.name.0),
                );
            }
            let ast::Item::Function(function) = &decl.item else {
                continue;
            };
            let Some((condition, span)) = decl
                .annotations
                .iter()
                .find(|(annotation, _)| annotation.name == "if")
            else {
                continue;
            };
            let [arg] = &*condition.args else {
                continue;
            };
            let Some((positive, path)) = module_condition(&arg.0) else {
                self.errors.push(Error::Condition {
                    kind: ConditionError::InvalidCondition,
                    span: *span,
                });
                // Prevent malformed conditions from becoming active through shared recovery.
                set_condition_false(&mut decl.annotations);
                continue;
            };
            if let Err(error) = self.evaluator.eval(arg) {
                self.errors.push(Error::Condition {
                    kind: ConditionError::Evaluation(error.to_string()),
                    span: error.span(),
                });
                set_condition_false(&mut decl.annotations);
                continue;
            }
            let patch = decl
                .annotations
                .iter()
                .find(|(ann, _)| matches!(ann.name, "addMethod" | "wrapMethod" | "replaceMethod"));
            let target = patch
                .map(|(ann, _)| {
                    format!(
                        "{}:{:?}",
                        ann.name,
                        ann.args
                            .iter()
                            .map(|(expr, _)| expr.clone().unwrapped())
                            .collect::<Vec<_>>()
                    )
                })
                .unwrap_or_default();
            let params: Vec<_> = function
                .params
                .iter()
                .map(|(param, _)| {
                    (
                        param.qualifiers,
                        param.typ.as_ref().map(|typ| typ.0.clone().unwrapped()),
                    )
                })
                .collect();
            let key = format!("{namespace}:{target}:{}:{params:?}", function.name.0);
            self.versions.entry(key.clone()).or_default().push((
                positive,
                patch.is_some_and(|(ann, _)| ann.name == "addMethod"),
                function.name.1,
            ));
            let present = self.evaluator.module_exists(path);
            let active = present == positive && self.selected.insert(key);
            if !active {
                set_condition_false(&mut decl.annotations);
            }
        }
    }
}

fn module_condition<'a>(expr: &'a ast::SourceExpr<'_>) -> Option<(bool, &'a str)> {
    let (positive, expr) = match expr {
        ast::Expr::UnOp {
            op: ast::UnOp::Not,
            expr,
        } => (false, &expr.0),
        _ => (true, expr),
    };
    match expr {
        ast::Expr::Call {
            expr,
            type_args,
            args,
        } if matches!(expr.0, ast::Expr::Ident("ModuleExists")) && type_args.is_empty() => {
            if let [(ast::Expr::Constant(ast::Constant::String(path)), _)] = &**args
                && path.split('.').all(|segment| !segment.is_empty())
            {
                Some((positive, path))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn set_condition_false(annotations: &mut [(ast::SourceAnnotation<'_>, Span)]) {
    for (annotation, _) in annotations {
        if annotation.name == "if" {
            for (expr, _) in &mut annotation.args {
                *expr = ast::Expr::Constant(ast::Constant::Bool(false));
            }
        }
    }
}
