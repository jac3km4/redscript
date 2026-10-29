//! Target access checks on resolved fields, including reads nested in calls/places.
use std::collections::HashSet;

use redscript_community_ast::{FileId, Span};
use redscript_compiler_frontend::ir::Visitor;
use redscript_compiler_frontend::{FieldId, LoweredCompilationUnit, RefType, Symbols, TypeId, ir};

use crate::{Error, SemanticError};

pub fn validate<'ctx>(
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    files: &HashSet<FileId>,
    errors: &mut Vec<Error>,
) {
    let mut validator = Validator {
        owner: None,
        symbols,
        files,
        errors,
    };
    for (id, class) in &unit.classes {
        validator.owner = Some(*id);
        for function in class.methods.values() {
            validator.visit_block(&function.block);
        }
    }
    validator.owner = None;
    for function in unit.functions.values() {
        validator.visit_block(&function.block);
    }
    let patches = unit
        .added_methods
        .iter()
        .filter_map(|(id, function)| function.as_ref().map(|function| (id, function)))
        .chain(unit.method_replacements.iter())
        .chain(
            unit.method_wrappers
                .iter()
                .flat_map(|(id, functions)| functions.iter().map(move |function| (id, function))),
        );
    for (id, function) in patches {
        validator.owner = Some(id.parent());
        validator.visit_block(&function.block);
    }
}

struct Validator<'a, 'ctx> {
    owner: Option<TypeId<'ctx>>,
    symbols: &'a Symbols<'ctx>,
    files: &'a HashSet<FileId>,
    errors: &'a mut Vec<Error>,
}

impl<'ctx> Visitor<'ctx> for Validator<'_, 'ctx> {
    fn visit_field(
        &mut self,
        receiver: &ir::Expr<'ctx>,
        _receiver_type: &ir::TypeApp<'ctx>,
        _receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
        span: Span,
    ) {
        if self.files.contains(&span.file)
            && self.symbols[field].flags().is_persistent()
            && self.symbols[field.parent()]
                .span()
                .is_some_and(|span| self.files.contains(&span.file))
            && self.owner != Some(field.parent())
        {
            self.errors
                .push(Error::semantic(SemanticError::PersistentFieldAccess, span));
        }
        self.visit_expr(receiver);
    }

    // REDmod rejects closures before lowering. Preserve the existing boundary
    // for community dependency closures rather than changing access semantics.
    fn visit_new_closure(&mut self, _closure: &ir::Closure<'ctx>, _span: Span) {}
}
