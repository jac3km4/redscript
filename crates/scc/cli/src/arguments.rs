use std::fmt;
use std::path::PathBuf;

use chumsky::prelude::*;

#[derive(Debug)]
#[allow(unused)]
pub struct Arguments {
    pub scripts_dirs: Vec<PathBuf>,
    pub optimize: bool,
    pub threads: u8,
    pub warnings: Vec<String>,
    pub no_testonly: bool,
    pub no_breakpoint: bool,
    pub profile: bool,
    pub script_paths_file: Option<PathBuf>,
    pub cache_file: Option<PathBuf>,
    pub no_exec: bool,
    pub no_debug: bool,
}

impl Arguments {
    pub fn from_env_args(args: impl IntoIterator<Item = String>) -> Result<Self, OptsError> {
        let joined = fixed_args(args.into_iter().skip(1));
        let slices = joined.iter().map(String::as_str).collect::<Vec<_>>();
        let parsed = arg_parser().repeated().collect::<Vec<_>>().parse(&slices);

        match parsed.into_result() {
            Err(errors) => {
                let errors = errors.iter().map(ToString::to_string).collect::<Vec<_>>();
                Err(OptsError::ParsingErrors(errors))
            }
            Ok(args) => {
                let mut scripts_dirs = Vec::new();
                let mut optimize = false;
                let mut threads = 1;
                let mut warnings = Vec::new();
                let mut no_testonly = false;
                let mut no_breakpoint = false;
                let mut profile = false;
                let mut script_paths_file = None;
                let mut cache_file = None;
                let mut no_exec = false;
                let mut no_debug = false;

                for arg in args {
                    match arg {
                        Arg::Named { name, value } => match name {
                            "compile" => scripts_dirs.push(PathBuf::from(value)),
                            "profile" => profile = value == "on",
                            "threads" => {
                                threads = value
                                    .parse()
                                    .map_err(|_| OptsError::InvalidArgument("threads"))?;
                            }
                            "compilePathsFile" => script_paths_file = Some(PathBuf::from(value)),
                            _ => {}
                        },
                        Arg::Flag { name } => match name {
                            "Wnone" => warnings.push("none".to_string()),
                            "optimize" => optimize = true,
                            "no-testonly" => no_testonly = true,
                            "no-breakpoint" => no_breakpoint = true,
                            "no-exec" => no_exec = true,
                            "no-debug" => no_debug = true,
                            _ => {}
                        },
                        Arg::Positional { value } => {
                            cache_file = Some(PathBuf::from(value));
                        }
                    }
                }

                if scripts_dirs.is_empty() {
                    return Err(OptsError::MissingArgument("compile"));
                }

                Ok(Self {
                    scripts_dirs,
                    optimize,
                    threads,
                    warnings,
                    no_testonly,
                    no_breakpoint,
                    profile,
                    script_paths_file,
                    cache_file,
                    no_exec,
                    no_debug,
                })
            }
        }
    }
}

fn fixed_args(args: impl IntoIterator<Item = String>) -> Vec<String> {
    let mut escaping = false;
    let mut processed = vec![];

    for mut arg in args {
        if let Some(idx) = arg.find('"') {
            escaping = true;

            let suffix = arg.split_off(idx + 1);
            arg.remove(idx);

            processed.push(arg);
            processed.push(suffix);
        } else if escaping {
            let mut parts = arg.split_whitespace();
            let (Some(first), Some(prev)) = (parts.next(), processed.last_mut()) else {
                processed.push(arg);
                continue;
            };
            *prev = format!("{} {first}", prev.trim_start());
            for part in parts {
                processed.push(part.to_owned());
            }
        } else {
            processed.push(arg);
        }
    }

    processed
}

fn arg_parser<'a>() -> impl Parser<'a, &'a [&'a str], Arg<'a>, extra::Err<Rich<'a, &'a str>>> {
    let switch = select! { str if str::starts_with(str, "-") => &str[1..] }.labelled("switch");
    let value = any().and_is(switch.not()).labelled("value");

    switch
        .then(value.or_not())
        .map(|(name, value)| match value {
            Some(value) => Arg::Named { name, value },
            None => Arg::Flag { name },
        })
        .or(value.map(|value| Arg::Positional { value }))
}

#[derive(Debug)]
enum Arg<'a> {
    Named { name: &'a str, value: &'a str },
    Flag { name: &'a str },
    Positional { value: &'a str },
}

#[derive(Debug)]
pub enum OptsError {
    ParsingErrors(Vec<String>),
    InvalidArgument(&'static str),
    MissingArgument(&'static str),
}

impl fmt::Display for OptsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParsingErrors(errors) => {
                write!(f, "parsing errors: ")?;
                errors.iter().enumerate().try_for_each(|(i, error)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{error}")
                })
            }
            Self::InvalidArgument(arg) => write!(f, "invalid argument for '{arg}'"),
            Self::MissingArgument(arg) => write!(f, "missing argument for '{arg}'"),
        }
    }
}

impl std::error::Error for OptsError {}

#[cfg(test)]
mod tests {
    use similar_asserts::assert_eq;

    use super::*;

    #[test]
    fn from_env_args_works_for_correct_input() {
        let args = [
            "D:\\Games\\Cyberpunk 2077\\engine\\tools\\scc.exe",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\r6\\scripts",
            "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts",
            "-Wnone",
            "-threads",
            "4",
            "-no-testonly",
            "-no-breakpoint",
            "-no-exec",
            "-no-debug",
            "-profile=off",
            "-optimize",
            "-compilePathsFile D:\\Games\\Cyberpunk 2077\\red4ext\\redscript_paths.txt",
        ]
        .into_iter()
        .map(ToOwned::to_owned);

        let opts = Arguments::from_env_args(args).unwrap();
        assert_eq!(
            opts.scripts_dirs,
            vec![PathBuf::from("D:\\Games\\Cyberpunk 2077\\r6\\scripts")]
        );
        assert_eq!(opts.optimize, true);
        assert_eq!(opts.threads, 4);
        assert_eq!(opts.warnings, vec!["none".to_string()]);
        assert_eq!(opts.no_testonly, true);
        assert_eq!(opts.no_breakpoint, true);
        assert_eq!(opts.profile, false);
        assert_eq!(
            opts.cache_file,
            Some(PathBuf::from(
                "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts"
            ))
        );
        assert_eq!(opts.no_exec, true);
        assert_eq!(opts.no_debug, true);
    }

    #[test]
    fn from_env_args_supports_multiple_compile_flags() {
        let args = [
            "D:\\Games\\Cyberpunk 2077\\engine\\tools\\scc.exe",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\r6\\scripts",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\mods\\mod1\\scripts",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\mods\\mod2\\scripts",
            "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts",
        ]
        .into_iter()
        .map(ToOwned::to_owned);

        let opts = Arguments::from_env_args(args).unwrap();
        assert_eq!(
            opts.scripts_dirs,
            vec![
                PathBuf::from("D:\\Games\\Cyberpunk 2077\\r6\\scripts"),
                PathBuf::from("D:\\Games\\Cyberpunk 2077\\mods\\mod1\\scripts"),
                PathBuf::from("D:\\Games\\Cyberpunk 2077\\mods\\mod2\\scripts"),
            ]
        );
        assert_eq!(
            opts.cache_file,
            Some(PathBuf::from(
                "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts"
            ))
        );
    }

    #[test]
    fn from_env_args_works_for_broken_input() {
        let args = [
            "D:\\Games\\Cyberpunk 2077\\engine\\tools\\scc.exe",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\r6\\scripts\" D:\\Games\\Cyberpunk",
            "2077\\r6\\cache\\final.redscripts -Wnone -threads 4 -no-testonly -no-breakpoint -no-exec -no-debug -profile=off -optimize  -compilePathsFile D:\\Games\\Cyberpunk",
            "2077\\red4ext\\redscript_paths.txt"
        ].into_iter().map(ToOwned::to_owned);

        let opts = Arguments::from_env_args(args).unwrap();
        assert_eq!(
            opts.scripts_dirs,
            vec![PathBuf::from("D:\\Games\\Cyberpunk 2077\\r6\\scripts")]
        );
        assert_eq!(opts.optimize, true);
        assert_eq!(opts.threads, 4);
        assert_eq!(opts.warnings, vec!["none".to_string()]);
        assert_eq!(opts.no_testonly, true);
        assert_eq!(opts.no_breakpoint, true);
        assert_eq!(opts.profile, false);
        assert_eq!(
            opts.cache_file,
            Some(PathBuf::from(
                "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts"
            ))
        );
        assert_eq!(opts.no_exec, true);
        assert_eq!(opts.no_debug, true);
    }

    #[test]
    fn fix_args_corrects_broken_args() {
        let broken_args = [
            "D:\\Games\\Cyberpunk 2077\\engine\\tools\\scc.exe",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\r6\\scripts\" D:\\Games\\Cyberpunk",
            "2077\\r6\\cache\\final.redscripts -Wnone -threads 4 -no-testonly -no-breakpoint -no-exec -no-debug -profile=off -optimize  -compilePathsFile D:\\Games\\Cyberpunk",
            "2077\\red4ext\\redscript_paths.txt"
        ].into_iter().map(ToOwned::to_owned);

        assert_eq!(
            fixed_args(broken_args),
            vec![
                "D:\\Games\\Cyberpunk 2077\\engine\\tools\\scc.exe",
                "-compile",
                "D:\\Games\\Cyberpunk 2077\\r6\\scripts",
                "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts",
                "-Wnone",
                "-threads",
                "4",
                "-no-testonly",
                "-no-breakpoint",
                "-no-exec",
                "-no-debug",
                "-profile=off",
                "-optimize",
                "-compilePathsFile",
                "D:\\Games\\Cyberpunk 2077\\red4ext\\redscript_paths.txt"
            ]
        );
    }

    #[test]
    fn fix_args_preserves_correct_args() {
        let args = [
            "D:\\Games\\Cyberpunk 2077\\engine\\tools\\scc.exe",
            "-compile",
            "D:\\Games\\Cyberpunk 2077\\r6\\scripts",
            "D:\\Games\\Cyberpunk 2077\\r6\\cache\\final.redscripts",
            "-Wnone",
            "-threads",
            "4",
            "-no-testonly",
            "-no-breakpoint",
            "-no-exec",
            "-no-debug",
            "-profile=off",
            "-optimize",
            "-compilePathsFile D:\\Games\\Cyberpunk 2077\\red4ext\\redscript_paths.txt",
        ]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

        assert_eq!(fixed_args(args.clone()), args);
    }
}
