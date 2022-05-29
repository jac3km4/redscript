pub mod ast;
pub mod bundle;
pub mod bytecode;
pub mod decode;
pub mod definition;
pub mod encode;
pub mod mapper;

#[cfg(not(feature = "arc"))]
pub type Ref<A> = std::rc::Rc<A>;
#[cfg(feature = "arc")]
pub type Ref<A> = std::sync::Arc<A>;

#[cfg(not(feature = "arc"))]
pub type Str = flexstr::LocalStr;
#[cfg(feature = "arc")]
pub type Str = flexstr::SharedStr;

#[cfg(not(feature = "arc"))]
pub use flexstr::local_fmt as str_fmt;
#[cfg(feature = "arc")]
pub use flexstr::shared_fmt as str_fmt;
