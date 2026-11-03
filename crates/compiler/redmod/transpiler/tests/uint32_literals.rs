//! Exact Uint32 expression constants, with opt-in official bundle evidence.
//! REDmod field defaults use unsigned decimal spelling instead of signed-bit casts.
use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::{TranspileDiagnostic, transpile_sources};

const VALUES: [u32; 6] = [
    0,
    1,
    2_147_483_647,
    2_147_483_648,
    3_670_998_070,
    4_294_967_295,
];

fn emit(source: &str) -> Result<String, Vec<TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let selected = sources.push_back("uint32.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .map(|output| output[0].module.text.clone())
}

fn fields() -> String {
    let mut source = String::from("class Uint32LiteralFields {\n");
    for (i, value) in VALUES.iter().enumerate() {
        source.push_str(&format!(" public let field{i}: Uint32 = {value}u;\n"));
    }
    source.push_str("}\n");
    source
}

fn expressions() -> String {
    VALUES
        .iter()
        .enumerate()
        .map(|(i, value)| format!("func Uint32Literal{i}() -> Uint32 {{ return {value}u; }}\n"))
        .collect()
}

#[test]
fn unsigned_boundaries_use_exact_signed_bits_without_widening() {
    let text = emit(&expressions()).unwrap();
    for value in [
        "Cast<Uint32>(0)",
        "Cast<Uint32>(1)",
        "Cast<Uint32>(2147483647)",
        "Cast<Uint32>(-2147483648)",
        "Cast<Uint32>(-623969226)",
        "Cast<Uint32>(-1)",
    ] {
        assert!(text.contains(&format!("return {value};")), "{text}");
    }
    assert!(!text.contains("Int64"), "{text}");
    assert!(!text.contains("Uint64"), "{text}");
    for (ty, literal) in [("Int64", "2147483648l"), ("Uint64", "2147483648ul")] {
        let errors = emit(&format!("func Wide() -> {ty} {{ return {literal}; }}")).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("unverified target range")),
            "{errors:?}"
        );
    }
}

#[test]
fn field_defaults_use_literal_only_unsigned_decimals() {
    let text = emit(&fields()).unwrap();
    for (i, value) in VALUES.iter().enumerate() {
        assert!(
            text.contains(&format!("var field{i}: Uint32 = {value};")),
            "{text}"
        );
    }
    assert!(!text.contains("Cast<"), "{text}");
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn exact_values_with_official_compiler() {
    use std::path::{Path, PathBuf};
    use std::process::{Command, Stdio};
    use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

    use byte::BytesExt as _;
    use redscript_io::{BundleReader, Definition, FunctionBody, Instr};

    let executable = PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE"));
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    assert!(executable.is_absolute() && root.is_absolute());
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "uint32-literals-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
    let directory = std::path::absolute(directory).unwrap();
    let mut source = emit(&expressions()).unwrap();
    source.push_str(&emit(&fields()).unwrap());
    std::fs::write(directory.join("r6/scripts/authored.script"), source).unwrap();
    // Unlike OfficialValidator, keep the process working directory under target
    // too: the official executable writes REDmodLog.txt in its working directory.
    let stdout = std::fs::File::create_new(directory.join("stdout.log")).unwrap();
    let stderr = std::fs::File::create_new(directory.join("stderr.log")).unwrap();
    let mut child = Command::new(executable)
        .current_dir(&directory)
        .args(["validate-mod-scripts", "-compile", "-root"])
        .arg(root)
        .arg("-directory")
        .arg(&directory)
        .stdin(Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .unwrap();
    let started = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if started.elapsed() >= Duration::from_secs(60) {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!(
                "official Uint32 validation timed out: {}",
                directory.display()
            );
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    let log = std::fs::read_to_string(directory.join("stdout.log")).unwrap();
    println!("{}\n{log}", directory.display());
    assert!(status.success(), "{status}: {log}");
    assert!(
        log.contains("Commandlet validate-mod-scripts has succeeded"),
        "{log}"
    );
    assert!(log.contains("v1.2.2"), "profiling layout requires v1.2.2");
    assert!(
        std::fs::metadata(directory.join("r6/cache/final.redscripts.h64"))
            .unwrap()
            .len()
            > 0
    );
    let bytes = std::fs::read(directory.join("r6/cache/final.redscripts")).unwrap();
    let reader = BundleReader::new(&bytes).unwrap();
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
    let definition = |name: &str, tag: u16| {
        let index = (1..word(84))
            .find(|i| {
                let at = word(80) as usize + *i as usize * 20;
                u16::from_le_bytes(bytes[at + 16..at + 18].try_into().unwrap()) == tag
                    && reader.cnames().get(word(at)).unwrap().split(';').next() == Some(name)
            })
            .unwrap_or_else(|| panic!("missing {name}"));
        reader.definitions().get(index).unwrap()
    };
    let mut listing = String::new();
    for (i, expected) in VALUES.iter().enumerate() {
        let Definition::Function(function) = definition(&format!("Uint32Literal{i}"), 5) else {
            panic!("expected function")
        };
        let FunctionBody::Raw { bytes: raw, .. } = function.body() else {
            panic!("expected raw body")
        };
        // The v1.2.2 profiling preamble is not decoded by the general reader.
        assert_eq!(raw[0], 0x33);
        let mut at = 14;
        let mut code = Vec::<Instr>::new();
        while at < raw.len() {
            code.push(raw.read(&mut at, byte::LE).unwrap());
        }
        listing.push_str(&format!("Uint32Literal{i} ({expected}): {code:?}\n"));
        let ops = code
            .iter()
            .filter(|op| !matches!(op, Instr::Breakpoint(_)))
            .collect::<Vec<_>>();
        assert!(
            matches!(ops.as_slice(), [Instr::Return, Instr::U32Const(actual), Instr::Nop] if actual == expected),
            "{code:?}"
        );
        let Definition::Field(field) = definition(&format!("field{i}"), 8) else {
            panic!("expected field")
        };
        assert_eq!(field.defaults().len(), 1, "{field:?}");
        assert_eq!(
            field.defaults()[0].value().parse::<u32>().unwrap(),
            *expected
        );
        listing.push_str(&format!("field{i}: {:?}\n", field.defaults()));
    }
    std::fs::write(directory.join("decoded.txt"), listing).unwrap();
}
