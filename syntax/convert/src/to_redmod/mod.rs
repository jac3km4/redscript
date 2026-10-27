//! Passes converting community code (`.reds`) to REDmod code (`.script`).

use redscript_ast::SourceModule;

use crate::Cx;

mod array_methods;
mod empty_array;
mod event_return;
mod float_literals;
mod for_in;
mod hoist;
mod imports;
mod inline_body;
mod int_suffix;
mod interpolation;
mod intrinsics;
mod let_patterns;
mod let_types;
mod locals;
mod loop_locals;
mod overrides;
mod system_name;
mod temporaries;
pub(crate) mod to_string;
mod typed_intrinsics;
mod visibility;
mod wrapped_method;

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    // only rewrites the intrinsics the diagnostics would report, when types say how
    typed_intrinsics::run(module, cx);
    // diagnostics are reported on the original AST, before any other rewrite
    crate::diagnostics::community(module, cx);

    imports::run(module, cx);
    inline_body::run(module, cx);
    wrapped_method::run(module, cx);
    visibility::run(module, cx);
    event_return::run(module, cx);
    // before the passes that track locals, the bindings become plain locals
    let_patterns::run(module, cx);
    intrinsics::run(module, cx);
    int_suffix::run(module, cx);
    float_literals::run(module, cx);
    system_name::run(module, cx);
    // after the casts above, which make more types known
    let_types::run(module, cx);
    interpolation::run(module, cx);
    for_in::run(module, cx);
    // after for-in, whose bodies are loops too
    loop_locals::run(module, cx);
    array_methods::run(module, cx);
    // after the array methods, whose temporaries are handled there
    temporaries::run(module, cx);
    empty_array::run(module, cx);
    overrides::run(module, cx);
    // last, so that every local introduced by the passes above is accounted for
    locals::run(module, cx);
}
