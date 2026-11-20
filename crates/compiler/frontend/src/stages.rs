mod infer;
mod provenance;
mod resolution;

pub use infer::{
    FunctionId, LoweredClass, LoweredCompilationUnit, LoweredFieldDefault, LoweredFunction,
    TypeInference,
};
pub use provenance::{DeclarationPath, ResolvedDeclaration};
pub use resolution::{FunctionAnnotation, NameResolution, Scope};
