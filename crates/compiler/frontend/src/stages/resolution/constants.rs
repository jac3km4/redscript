pub mod annotation {
    pub const IF: &str = "if";
    pub const WRAP_METHOD: &str = "wrapMethod";
    pub const REPLACE_METHOD: &str = "replaceMethod";
    pub const ADD_METHOD: &str = "addMethod";
    pub const ADD_FIELD: &str = "addField";
    pub const INTRINSIC: &str = "intrinsic";
    pub const NEVER_REF: &str = "neverRef";
    pub const MIXED_REF: &str = "mixedRef";
    pub const NAME_IMPLEMENTATION: &str = "nameImplementation";
    pub const RUNTIME_PROPERTY: &str = "runtimeProperty";
    pub const HINT: &str = "hint";
    pub const DERIVE_NEW: &str = "deriveNew";
    pub const PRIVATE_CONSTRUCTOR: &str = "privateConstructor";
}

pub mod ident {
    pub const CAST: &str = "Cast";
    pub const THIS: &str = "this";
    pub const WRAPPED_METHOD: &str = "wrappedMethod";
    pub const NEW_METHOD: &str = "New";
}
