use redscript_compiler::typer::InferType;

mod utils;

#[test]
fn function() {
    utils::check_file_as_seq("typeck/function.reds", InferType::TOP)
        .no_errors()
        .local_has_type("id", "Function1<'1, '1>")
        .local_has_type("apply", "Function1<Function1<'1, '2>, Function1<'1, '2>>")
        .local_has_type("flip", "Function1<Function2<'1, '2, '3>, Function2<'2, '1, '3>>")
        .no_more_locals();
}

#[test]
#[ignore]
fn control_flow() {
    utils::check_file_as_seq("typeck/control-flow.reds", InferType::TOP)
        .no_errors()
        .local_has_type("while_", "Function1<Bool, Nothing>")
        .local_has_type("if_", "Function1<Bool, Nothing>")
        .local_has_type("for_", "Function1<array<'1>, Nothing>")
        .local_has_type("switch_", "Function1<String, Nothing>")
        .no_more_locals();
}

#[test]
fn cast() {
    utils::check_file_as_seq("typeck/cast.reds", InferType::TOP)
        .no_errors()
        .local_has_type("x", "Uint8")
        .local_has_type("y", "Uint8")
        .local_has_type("z", "Int32")
        .no_more_locals();
}

#[test]
fn implicit_conv() {
    utils::check_file_as_seq("typeck/implicit-conv.reds", InferType::TOP)
        .no_errors()
        .local_has_type("x", "wref<IScriptable>")
        .local_has_type("y", "wref<IScriptable>")
        .local_has_type("z", "IScriptable")
        .local_has_type("w", "IScriptable")
        .no_more_locals();
}

#[test]
fn dyn_cast() {
    utils::check_file_as_seq("typeck/dyn-cast.reds", InferType::TOP)
        .no_errors()
        .local_has_type("x", "Function1<'1, '1>")
        .local_has_type("y", "IScriptable")
        .no_more_locals();
}

#[test]
fn closure() {
    utils::check_file_as_seq("typeck/closure.reds", InferType::TOP)
        .no_errors()
        .local_has_type("x", "Int32")
        .local_has_type("f", "Function0<Int32>")
        .no_more_locals();
}

#[test]
#[ignore]
fn lub() {
    utils::check_file_as_seq("typeck/lub.reds", InferType::TOP)
        .no_errors()
        .local_has_type("x", "array<IScriptable>")
        .no_more_locals();
}
