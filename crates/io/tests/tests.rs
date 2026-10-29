use redscript_io::{Definition, ScriptBundle, Type, TypeKind};

#[test]
fn reject_embedded_nul_in_all_string_pools() {
    for pool in 0..4 {
        for string in ["\0", "before\0after", "trailing\0"] {
            let mut bundle = ScriptBundle::default();
            match pool {
                0 => {
                    bundle.cnames_mut().add(string);
                }
                1 => {
                    bundle.tdb_ids_mut().add(string);
                }
                2 => {
                    bundle.resources_mut().add(string);
                }
                _ => {
                    bundle.strings_mut().add(string);
                }
            }
            let writeable = bundle.into_writeable();
            assert!(writeable.to_bytes().is_err());
            use redscript_io::byte::{Measure, TryWrite};
            let mut bytes = vec![0xAB; writeable.measure(())];
            assert!(
                writeable
                    .try_write(&mut bytes, redscript_io::byte::LE)
                    .is_err()
            );
            assert!(bytes.iter().all(|&byte| byte == 0xAB));
        }
    }
}

#[cfg(feature = "mmap")]
#[test]
fn failed_encoding_preserves_existing_file() {
    let path = std::env::temp_dir().join(format!(
        "redscript-encoding-{}-{}.redscripts",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::write(&path, b"unchanged").unwrap();
    let mut bundle = ScriptBundle::default();
    bundle.strings_mut().add("invalid\0string");
    let result = bundle.into_writeable().save(&path);
    let contents = std::fs::read(&path).unwrap();
    std::fs::remove_file(path).unwrap();
    assert!(matches!(result, Err(redscript_io::SaveError::Encoding(_))));
    assert_eq!(contents, b"unchanged");
}

#[test]
fn roundtrip_empty_and_unicode_pooled_strings() {
    let mut bundle = ScriptBundle::default();
    let empty = bundle.strings_mut().add("");
    let unicode = bundle.strings_mut().add("héllo");
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let decoded = ScriptBundle::from_bytes(&bytes).unwrap();
    assert_eq!(&decoded[empty], "");
    assert_eq!(&decoded[unicode], "héllo");
}

#[test]
fn roundtrip_def() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("whatever");
    let typ = Type::new(name, TypeKind::Class);
    let idx = bundle.define(typ.clone());

    assert_eq!(bundle[idx], typ);
}

#[test]
fn roundtrip_encode() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("whatever");
    let typ = Type::new(name, TypeKind::Class);
    bundle.define(typ.clone());

    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let defs = bundle.definitions().cloned().collect::<Vec<_>>();
    assert_eq!(&defs, &[Definition::Type(typ)]);
}
