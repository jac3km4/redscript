#![feature(result_cloned)]
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
