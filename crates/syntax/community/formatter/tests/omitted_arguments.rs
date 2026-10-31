mod common;

use common::assert_preserved;
use redscript_community_formatter::FormatSettings;

#[test]
fn positional_holes_roundtrip_without_changing_arity() {
    for max_width in [20, 100] {
        let settings = FormatSettings {
            max_width,
            ..FormatSettings::default()
        };
        for expression in [
            "Target()",
            "Target(,)",
            "Target(,,)",
            "Target(,42,)",
            "Target(42,,43)",
            "Target(42,)",
            "Target(42,,)",
            "Target(,42,,)",
            "Target(,VeryLongArgumentName,,)",
            "host.Target(,VeryLongArgumentName,,)",
            "host.Target<Int32>(,42,)",
            "Target(,Nested(42,),)",
            "Target(, /* slot */ 42,)",
            "Target(,42,, /* final slot */)",
            "Target(,VeryLongArgumentName,)",
            "host.Target(,VeryLongArgumentName,)",
        ] {
            assert_preserved(
                &format!("func Run() {{ {expression}; }}"),
                &settings,
                "omitted.reds",
            );
        }
    }
}
