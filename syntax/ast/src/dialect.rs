/// The flavor of the language a source file is written in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Dialect {
    /// The community flavor, stored in `.reds` files.
    #[default]
    Community,
    /// The REDmod flavor, stored in `.script` files.
    Redmod,
}

impl Dialect {
    /// Returns the dialect associated with the given file extension (without the leading dot).
    pub fn from_extension(ext: &str) -> Option<Self> {
        match ext {
            "reds" => Some(Self::Community),
            "script" => Some(Self::Redmod),
            _ => None,
        }
    }

    /// Returns the file extension (without the leading dot) associated with this dialect.
    pub fn extension(self) -> &'static str {
        match self {
            Self::Community => "reds",
            Self::Redmod => "script",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extension_mapping() {
        assert_eq!(Dialect::from_extension("reds"), Some(Dialect::Community));
        assert_eq!(Dialect::from_extension("script"), Some(Dialect::Redmod));
        assert_eq!(Dialect::from_extension("txt"), None);
        assert_eq!(Dialect::Community.extension(), "reds");
        assert_eq!(Dialect::Redmod.extension(), "script");
    }
}
