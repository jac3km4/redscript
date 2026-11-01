mod common;

use common::assert_preserved;
use redscript_community_formatter::FormatSettings;

#[test]
fn ordered_default_labels_roundtrip() {
    for body in [
        "default: return; case 1: break;",
        "case 0: First(); default: Middle(); case 1: Last();",
        "case 0: case 1: default: break;",
    ] {
        assert_preserved(
            &format!("func Run(value: Int32) {{ switch value {{ {body} }} }}"),
            &FormatSettings::default(),
            "ordered-switch.reds",
        );
    }
}
