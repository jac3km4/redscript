use redscript_community_ast::Span;

use crate::ir::Visitor;
use crate::{
    CompileErrorReporter, Diagnostic, FunctionType, LoweredCompilationUnit, LoweredFunction,
    MethodId, RefType, Symbols, ir,
};

pub(super) fn validate<'ctx>(
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) {
    let mut functions: Vec<(&LoweredFunction<'ctx>, &FunctionType<'ctx>)> = Vec::new();
    for (id, function) in &unit.functions {
        functions.push((function, symbols[*id].type_()));
    }
    for (id, class) in &unit.classes {
        for (method, function) in &class.methods {
            functions.push((function, symbols[MethodId::new(*id, *method)].type_()));
        }
    }
    for (id, function) in unit
        .added_methods
        .iter()
        .filter_map(|(id, f)| f.as_ref().map(|f| (id, f)))
        .chain(unit.method_replacements.iter())
        .chain(
            unit.method_wrappers
                .iter()
                .flat_map(|(id, fs)| fs.iter().map(move |f| (id, f))),
        )
    {
        functions.push((function, symbols[*id].type_()));
    }

    let diagnostics = crate::parallel::map_ordered(functions, |(function, func_type)| {
        check_function(function, func_type, symbols)
    });
    for diagnostics in diagnostics {
        reporter.report_many(diagnostics);
    }
}

fn check_function<'ctx>(
    function: &LoweredFunction<'ctx>,
    func_type: &FunctionType<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Vec<Diagnostic<'ctx>> {
    let mut reporter = CompileErrorReporter::default();
    if func_type
        .return_type()
        .strip_ref()
        .is_some_and(|(rt, _)| rt == RefType::Script)
    {
        reporter.report(Diagnostic::ScriptRefReturn(function.span));
    }
    ScriptRefValidator {
        symbols,
        reporter: &mut reporter,
    }
    .visit_block(&function.block);
    reporter.into_reported()
}

struct ScriptRefValidator<'a, 'ctx> {
    symbols: &'a Symbols<'ctx>,
    reporter: &'a mut CompileErrorReporter<'ctx>,
}

impl<'ctx> Visitor<'ctx> for ScriptRefValidator<'_, 'ctx> {
    fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, span: Span) {
        if closure.typ.last_arg_ref_type(self.symbols) == Some(RefType::Script) {
            self.reporter.report(Diagnostic::ScriptRefReturn(span));
        }
        self.visit_block(&closure.block);
    }

    fn visit_call(&mut self, call: &ir::Call<'ctx>, span: Span) {
        let visit_call_arg = |this: &mut Self, arg: &ir::Expr<'ctx>| {
            if arg.is_prvalue_ref(this.symbols)
                && let ir::Expr::Call { call, .. } = arg
                && let ir::Call::FreeFunction { args, .. } = &**call
                && let [inner] = &args[..]
            {
                this.visit_expr(inner);
                return;
            }
            this.visit_expr(arg);
        };

        match call {
            ir::Call::FreeFunction { function, args, .. } => {
                if self.symbols[*function].intrinsic().is_none() {
                    for arg in args {
                        visit_call_arg(self, arg);
                    }
                    return;
                }
                if self.symbols[*function].intrinsic() == Some(ir::Intrinsic::AsRef)
                    && let [inner] = &args[..]
                    && inner.is_prvalue(self.symbols)
                {
                    self.reporter
                        .report(Diagnostic::TemporaryScriptRefEscape(span));
                }
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ir::Call::Static { args, .. } => {
                for arg in args {
                    visit_call_arg(self, arg);
                }
            }
            ir::Call::Instance { receiver, args, .. } => {
                self.visit_expr(receiver);
                for arg in args {
                    visit_call_arg(self, arg);
                }
            }
            ir::Call::Closure { closure, args, .. } => {
                self.visit_expr(closure);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
        }
    }
}
