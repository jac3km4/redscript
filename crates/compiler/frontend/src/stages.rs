mod infer;
mod resolution;

pub use infer::{
    FunctionId, LoweredClass, LoweredCompilationUnit, LoweredFieldDefault, LoweredFunction,
    TypeInference,
};
pub use resolution::{FunctionAnnotation, NameResolution, Scope};
