//! Internal, opt-in official compiler probes. Evidence is retained even on failure.
#[cfg(test)]
mod tests;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use byte::BytesExt as _;
use redscript_io::{BundleReader, Definition, FunctionBody, Instr};
pub use redscript_redmod_transpiler::validate::OfficialValidator;

pub struct OfficialCompiler {
    pub validator: OfficialValidator,
    pub game_root: PathBuf,
}

impl OfficialCompiler {
    /// Requires REDMOD_EXE and REDMOD_GAME_ROOT; never discovers or downloads tools.
    pub fn from_env() -> Self {
        let executable = PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE"));
        let game_root =
            PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
        assert!(executable.is_absolute(), "REDMOD_EXE must be absolute");
        assert!(game_root.is_absolute(), "REDMOD_GAME_ROOT must be absolute");
        Self {
            validator: OfficialValidator {
                executable,
                timeout: Duration::from_secs(60),
            },
            game_root,
        }
    }

    pub fn compile(&self, name: &str, source: &str) -> OfficialOutput {
        self.compile_files(name, &[source])
    }

    pub fn reject(&self, name: &str, source: &str, diagnostic: &str) -> OfficialOutput {
        self.reject_files(name, &[source], diagnostic)
    }

    /// Compile unchanged sources as authored.script (one source), or
    /// authored-0.script, authored-1.script, ... (multiple sources).
    pub fn compile_files(&self, name: &str, sources: &[&str]) -> OfficialOutput {
        self.run(name, sources, None)
    }

    /// Require a completed compiler rejection with the expected diagnostic.
    /// File naming and source preservation match `compile_files`.
    pub fn reject_files(&self, name: &str, sources: &[&str], diagnostic: &str) -> OfficialOutput {
        assert!(
            !diagnostic.is_empty(),
            "rejection diagnostic must not be empty"
        );
        self.run(name, sources, Some(diagnostic))
    }

    fn run(&self, name: &str, sources: &[&str], diagnostic: Option<&str>) -> OfficialOutput {
        assert!(!sources.is_empty(), "at least one source is required");
        let directory = unique_directory(name);
        println!("official compiler evidence: {}", directory.display());
        fs::create_dir_all(directory.join("r6/scripts")).unwrap();
        for (index, source) in sources.iter().enumerate() {
            let filename = if sources.len() == 1 {
                "authored.script".to_owned()
            } else {
                format!("authored-{index}.script")
            };
            fs::write(directory.join("r6/scripts").join(filename), source).unwrap();
        }
        // Enforce the test harness bound even when callers customize the validator.
        let validator = OfficialValidator {
            executable: self.validator.executable.clone(),
            timeout: Duration::from_secs(60),
        };
        let invocation = validator
            .invoke_with_root(&directory, &self.game_root)
            .unwrap_or_else(|error| {
                panic!(
                    "official compiler infrastructure error in {}: {error}",
                    directory.display()
                )
            });
        let report = if let Some(diagnostic) = diagnostic {
            assert!(
                invocation.status.code().is_some_and(|code| code != 0),
                "expected compiler rejection in {}: {invocation:?}",
                directory.display()
            );
            assert!(
                !invocation.has_success_marker(),
                "rejection reported success: {invocation:?}"
            );
            assert!(
                invocation.report.stdout.contains(diagnostic)
                    || invocation.report.stderr.contains(diagnostic),
                "missing rejection diagnostic {diagnostic:?} in {}: {invocation:?}",
                directory.display()
            );
            invocation.report
        } else {
            invocation.into_validation_report().unwrap_or_else(|error| {
                panic!(
                    "official compiler failed in {}: {error}",
                    directory.display()
                )
            })
        };
        OfficialOutput {
            directory,
            stdout: report.stdout,
            stderr: report.stderr,
        }
    }
}

/// Atomically reserve a unique absolute directory beneath workspace target/redmod-official.
/// It and all compiler side effects are deliberately never cleaned up.
pub fn unique_directory(name: &str) -> PathBuf {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    assert!(
        !name.is_empty()
            && name
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_')),
        "probe name must contain only ASCII letters, digits, '-' or '_'"
    );
    let parent = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target/redmod-official");
    fs::create_dir_all(&parent).unwrap();
    let parent = fs::canonicalize(parent).unwrap();
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    loop {
        let directory = parent.join(format!(
            "{name}-{}-{timestamp}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        match fs::create_dir(&directory) {
            Ok(()) => return directory,
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => panic!("cannot reserve {}: {error}", directory.display()),
        }
    }
}

pub struct OfficialOutput {
    pub directory: PathBuf,
    pub stdout: String,
    pub stderr: String,
}

impl OfficialOutput {
    pub fn bundle_bytes(&self) -> Vec<u8> {
        fs::read(self.directory.join("r6/cache/final.redscripts")).unwrap()
    }

    pub fn assert_version(&self, version: &str) {
        assert!(!version.is_empty(), "version must not be empty");
        assert!(
            self.stdout.contains(version) || self.stderr.contains(version),
            "expected official compiler version {version:?} in {}\n{}\n{}",
            self.directory.display(),
            self.stdout,
            self.stderr
        );
    }
}

/// Bounded REDS v14 inspection, not a general official bundle decoder.
/// Function bodies support only the official v1.2.2 14-byte Profile preamble.
/// All lookup failures, malformed records and unsupported layouts panic explicitly.
pub struct OfficialBundle<'a> {
    bytes: &'a [u8],
    reader: BundleReader<'a>,
}

impl<'a> OfficialBundle<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let reader = BundleReader::new(bytes).expect("expected bounded REDS v14 tables");
        let bundle = Self { bytes, reader };
        // Validate all string references and physical definition records without
        // decoding unrelated definition payloads (official layouts can differ).
        for table in [44, 56, 68, 92] {
            bundle.literal_pool(table);
        }
        for index in 1..bundle.word(84) {
            let header = bundle.header(index);
            bundle
                .reader
                .cnames()
                .get(bundle.word(header))
                .expect("invalid definition name");
            bundle.record(index);
        }
        bundle
    }

    fn word(&self, at: usize) -> u32 {
        u32::from_le_bytes(
            self.bytes
                .get(at..at.checked_add(4).unwrap())
                .expect("truncated REDS word")
                .try_into()
                .unwrap(),
        )
    }

    fn header(&self, index: u32) -> usize {
        assert!(
            index > 0 && index < self.word(84),
            "definition index out of bounds: {index}"
        );
        (self.word(80) as usize)
            .checked_add((index as usize).checked_mul(20).unwrap())
            .unwrap()
    }

    fn record(&self, index: u32) -> &'a [u8] {
        let header = self.header(index);
        let start = self.word(header + 8) as usize;
        let end = start
            .checked_add(self.word(header + 12) as usize)
            .expect("definition range overflow");
        self.bytes
            .get(start..end)
            .expect("definition exceeds bundle")
    }

    fn lookup(&self, name: &str, tag: u16, prefix: bool) -> u32 {
        let mut found = None;
        for index in 1..self.word(84) {
            let header = self.header(index);
            let actual_tag =
                u16::from_le_bytes(self.bytes[header + 16..header + 18].try_into().unwrap());
            if actual_tag != tag {
                continue;
            }
            let actual = self.definition_name(index);
            if if prefix {
                actual.starts_with(name)
            } else {
                actual == name
            } {
                assert!(
                    found.is_none(),
                    "ambiguous definition {name:?} (tag {tag}, prefix={prefix})"
                );
                found = Some(index);
            }
        }
        found.unwrap_or_else(|| panic!("missing definition {name:?} (tag {tag}, prefix={prefix})"))
    }

    pub fn definition(&self, name: &str, tag: u16) -> &'a [u8] {
        self.record(self.definition_index(name, tag))
    }

    pub fn definition_prefix(&self, prefix: &str, tag: u16) -> &'a [u8] {
        self.record(self.lookup(prefix, tag, true))
    }

    pub fn definition_index(&self, name: &str, tag: u16) -> u32 {
        self.lookup(name, tag, false)
    }

    pub fn definition_name(&self, index: u32) -> &'a str {
        self.reader
            .cnames()
            .get(self.word(self.header(index)))
            .expect("invalid definition name")
    }

    /// Uses the complete mangled name, including the signature after ';'.
    pub fn function_code(&self, name: &str) -> Vec<Instr> {
        self.decode_function(self.definition_index(name, 5))
    }

    pub fn function_code_prefix(&self, prefix: &str) -> Vec<Instr> {
        self.decode_function(self.lookup(prefix, 5, true))
    }

    fn decode_function(&self, index: u32) -> Vec<Instr> {
        let Definition::Function(function) = self
            .reader
            .definitions()
            .get(index)
            .expect("unsupported or malformed official function record")
        else {
            panic!("expected function record")
        };
        assert!(!function.body().is_empty(), "function has no body");
        let FunctionBody::Raw { bytes, .. } = function.body() else {
            panic!("expected serialized body")
        };
        // BundleReader limits this body to the physical definition size. Never
        // use virtual code length as a physical byte count, or scan other records.
        assert!(
            bytes.len() >= 14 && bytes[0] == 0x33,
            "expected official v1.2.2 14-byte Profile preamble"
        );
        let mut at = 14;
        let mut code = Vec::new();
        while at < bytes.len() {
            assert_ne!(bytes[at], 0x33, "unsupported nested Profile instruction");
            code.push(
                bytes
                    .read(&mut at, byte::LE)
                    .expect("invalid instruction within physical function record"),
            );
        }
        code
    }

    /// `table` is the REDS header byte offset: 44 CNames, 56 TweakDB IDs,
    /// 68 resources or 92 strings. Other table offsets are unsupported.
    pub fn literal_pool(&self, table: usize) -> Vec<&'a str> {
        let pool = match table {
            44 => self.reader.cnames(),
            56 => self.reader.tweakdb_ids(),
            68 => self.reader.resources(),
            92 => self.reader.strings(),
            _ => panic!("unsupported literal table offset: {table}"),
        };
        pool.into_iter()
            .map(|item| item.expect("invalid bounded literal pool entry"))
            .collect()
    }
}
