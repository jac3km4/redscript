use redscript_io::{Instr, ScriptBundle};
use support::function;

use crate::support;

fn compile(source: &str) -> ScriptBundle<'static> {
    support::compile(source, "selection.script", &[], support::empty_bundle())
}

fn calls(bundle: &ScriptBundle<'_>, name: &str) -> Vec<String> {
    function(bundle, name)
        .body()
        .code_iter()
        .filter_map(|instr| match instr.unwrap() {
            Instr::InvokeVirtual { function, .. } => Some(bundle[function].to_string()),
            Instr::InvokeStatic { function, .. } => {
                Some(bundle[bundle[function].name()].to_string())
            }
            _ => None,
        })
        .collect()
}

#[test]
fn overload_and_forwarder_calls_emit_the_selected_targets() {
    let bundle = compile(
        "class ProbeValueBase {} class ProbeValueChild extends ProbeValueBase {}
         class ProbeHost {
           public static function Pick(self: ProbeHost, value: Int32) -> Int32 { return 1; }
           public function Pick(value: Int32) -> Int32 { return 2; }
           public function Specific(value: ProbeValueBase) -> Int32 { return 1; }
           public function Specific(value: ProbeValueChild) -> Int32 { return 2; }
           public static function Get() -> ProbeHost { return NULL; }
           public static function Arg() -> Int32 { return 1; }
           public static function Alias(self: ProbeHost, value: Int32) -> Int32 { return value; }
         }
         function RunReal(host: ProbeHost) -> Int32 { return host.Pick(1); }
         function RunSpecific(host: ProbeHost, value: ProbeValueChild) -> Int32 { return host.Specific(value); }
         function RunAlias() -> Int32 { return ProbeHost.Get().Alias(ProbeHost.Arg()); }",
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    assert_eq!(calls(&bundle, "RunReal;ProbeHost"), ["Pick;Int32"]);
    assert_eq!(
        calls(&bundle, "RunSpecific;ProbeHostProbeValueChild"),
        ["Specific;ProbeValueChild"]
    );
    // Prefix bytecode: alias call, then exactly one receiver evaluation, then
    // exactly one argument evaluation. No synthetic instance target is emitted.
    assert_eq!(
        calls(&bundle, "RunAlias;"),
        ["Alias;ProbeHostInt32", "Get;", "Arg;"]
    );
}
