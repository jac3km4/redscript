//! Target access checks on resolved fields, including reads nested in calls/places.
use std::collections::HashSet;

use redscript_community_ast::{FileId, Span};
use redscript_compiler_frontend::ir::Visitor;
use redscript_compiler_frontend::{
    FieldId, FunctionId, LoweredCompilationUnit, RefType, Symbols, TypeId, ir,
};

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
    for (id, function) in unit.functions_with_ids() {
        validator.owner = match id {
            FunctionId::Free(_) => None,
            FunctionId::Method(id) | FunctionId::Replacement(id) | FunctionId::Wrapper(id) => {
                Some(id.parent())
            }
        };
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
                .schema()
                .as_aggregate()
                .is_some_and(|owner| !owner.flags().is_native() && !owner.flags().is_struct())
            && self.symbols[field.parent()]
                .span()
                .is_some_and(|span| self.files.contains(&span.file))
            && !self.owner.is_some_and(|owner| {
                self.symbols
                    .base_iter_with_self(owner)
                    .any(|(id, _)| id == field.parent())
            })
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
