use std::fmt::{self, Write};

use redscript_compiler_frontend::{Symbols, Type, TypeSchema, predef};

use crate::{Result, error};

pub fn ident(name: &str) -> Result<&str> {
    if !redscript_redmod_parser::is_target_identifier(name) {
        return Err(error(
            format!("invalid or reserved target identifier `{name}`"),
            None,
        ));
    }
    Ok(name)
}

pub fn path(name: &str) -> Result<&str> {
    name.split('.')
        .try_for_each(|part| ident(part).map(|_| ()))?;
    Ok(name)
}

pub fn typ<'ctx>(typ: &Type<'ctx>, symbols: &Symbols<'ctx>) -> Result<String> {
    let Type::Data(app) = typ else {
        return Err(error("unresolved/custom generic type is unsupported", None));
    };
    let id = app.id();
    let args = app.args();
    let def = symbols
        .get_type(id)
        .ok_or_else(|| error("missing resolved type", None))?;
    if id.is_fn() {
        return Err(error("lambdas and function values are unsupported", None));
    }
    if def
        .schema()
        .as_aggregate()
        .is_some_and(|a| a.flags().is_mixed_ref())
    {
        return Err(error("mixedRef types are unsupported", None));
    }
    if def
        .schema()
        .as_aggregate()
        .is_some_and(|a| a.flags().is_never_ref() && !a.flags().is_struct())
    {
        return Err(error(
            "neverRef classes have no verified target value semantics",
            None,
        ));
    }
    if let [inner] = args {
        let inner_text = self::typ(inner, symbols)?;
        if id == predef::REF {
            let Type::Data(inner) = inner else {
                return Err(error("invalid strong reference", None));
            };
            if !symbols
                .get_type(inner.id())
                .and_then(|d| d.schema().as_aggregate())
                .is_some_and(|a| !a.flags().is_struct() && !a.flags().is_never_ref())
            {
                return Err(error("strong ref requires a target reference class", None));
            }
            return Ok(inner_text);
        }
        if id == predef::WREF {
            return Ok(format!("wref<{inner_text}>"));
        }
        if id == predef::SCRIPT_REF {
            return Ok(format!("script_ref<{inner_text}>"));
        }
        if id == predef::ARRAY {
            return Ok(format!("array<{inner_text}>"));
        }
        if let Some(size) = id.static_array_size() {
            return Ok(format!("{inner_text}[{size}]"));
        }
    }
    if !args.is_empty() || !def.params().is_empty() {
        return Err(error("custom generic types are unsupported", None));
    }
    if matches!(def.schema(), TypeSchema::Primitive) && id == predef::NOTHING {
        return Err(error("Nothing has no target type", None));
    }
    path(id.as_str()).map(str::to_owned)
}

/// A concrete failure to spell a literal faithfully in the verified target subset.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralError {
    EmbeddedDelimiter,
    Nul,
    UnverifiedControl,
    UnverifiedResource,
}

impl fmt::Display for LiteralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::EmbeddedDelimiter => "embedded delimiter has no verified REDmod literal spelling",
            Self::Nul => "NUL has no verified REDmod literal spelling",
            Self::UnverifiedControl => "unverified control character in REDmod literal",
            Self::UnverifiedResource => {
                "raw backslashes or controls are unverified in R/T literals"
            }
        })
    }
}

impl std::error::Error for LiteralError {}

/// Validated raw contents; formatting cannot encounter a capability error.
#[derive(Debug)]
pub struct StringLiteral<'a> {
    value: &'a str,
    delimiter: char,
}

pub fn string(value: &str) -> Result<StringLiteral<'_>> {
    literal(value, '"', false)
}

pub fn cname(value: &str) -> Result<StringLiteral<'_>> {
    literal(value, '\'', false)
}

pub fn resource_string(value: &str) -> Result<StringLiteral<'_>> {
    literal(value, '"', true)
}

fn literal(value: &str, delimiter: char, resource: bool) -> Result<StringLiteral<'_>> {
    for c in value.chars() {
        let kind = if c == delimiter {
            Some(LiteralError::EmbeddedDelimiter)
        } else if c == '\0' {
            Some(LiteralError::Nul)
        } else if c.is_control() && c != '\n' {
            Some(LiteralError::UnverifiedControl)
        } else if resource && (c == '\\' || c.is_control()) {
            Some(LiteralError::UnverifiedResource)
        } else {
            None
        };
        if let Some(kind) = kind {
            let mut diagnostic = error(kind.to_string(), None);
            diagnostic.literal_error = Some(kind);
            return Err(diagnostic);
        }
    }
    Ok(StringLiteral { value, delimiter })
}

impl fmt::Display for StringLiteral<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.delimiter)?;
        f.write_str(self.value)?;
        f.write_char(self.delimiter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifiers_preserve_valid_names_and_reject_invalid_or_reserved_names() {
        for name in [
            "_", "_value2", "Run", "value123", "Override", "null", "Name",
        ] {
            assert_eq!(ident(name).unwrap(), name);
        }
        for name in [
            "", "1name", "a-b", "a.b", "a b", "café", "a\n", "class", "NULL", "override",
        ] {
            let error = ident(name).unwrap_err();
            assert_eq!(
                error.message,
                format!("invalid or reserved target identifier `{name}`")
            );
            assert_eq!(error.span, None);
            assert_eq!(error.declaration, None);
        }
    }

    #[test]
    fn paths_validate_each_identifier() {
        assert_eq!(
            path("Game._value2.Override").unwrap(),
            "Game._value2.Override"
        );
        for (name, invalid) in [
            ("", ""),
            (".Game", ""),
            ("Game.", ""),
            ("Game..Value", ""),
            ("Game.override", "override"),
            ("override.Value", "override"),
            ("Game.a-b", "a-b"),
        ] {
            assert_eq!(
                path(name).unwrap_err().message,
                format!("invalid or reserved target identifier `{invalid}`")
            );
        }
    }

    #[test]
    fn string_literals_preserve_raw_values() {
        for value in [
            r"assets\widgets",
            r"assets\",
            r"\n\t\u{22}\\",
            "first\n  second",
            "",
            "café",
        ] {
            let text = string(value).unwrap().to_string();
            assert_eq!(text, format!("\"{value}\""));
            let sources = redscript_community_ast::SourceMap::new();
            let file = sources.push_back("literal.script", text.clone());
            let (tokens, errors) = redscript_redmod_parser::lex(&text, file);
            assert!(errors.is_empty());
            assert_eq!(
                tokens.unwrap()[0].0,
                redscript_redmod_parser::Token::Str(value.into())
            );
            assert_eq!(cname(value).unwrap().to_string(), format!("'{value}'"));
        }
        assert_eq!(cname("say\"hello").unwrap().to_string(), "'say\"hello'");
    }

    #[test]
    fn string_literals_reject_unsupported_controls_before_formatting() {
        for (value, kind) in [
            ("\0", LiteralError::Nul),
            ("\r", LiteralError::UnverifiedControl),
            ("\t", LiteralError::UnverifiedControl),
            ("\u{7f}", LiteralError::UnverifiedControl),
            ("prefix\u{85}suffix", LiteralError::UnverifiedControl),
            ("say\"hello", LiteralError::EmbeddedDelimiter),
        ] {
            assert_eq!(string(value).unwrap_err().literal_error, Some(kind));
        }
        assert_eq!(
            cname("can't").unwrap_err().literal_error,
            Some(LiteralError::EmbeddedDelimiter)
        );
        for value in [r"path\", "first\nsecond"] {
            assert_eq!(
                resource_string(value).unwrap_err().literal_error,
                Some(LiteralError::UnverifiedResource)
            );
        }
    }
}
