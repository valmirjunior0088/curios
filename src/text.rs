// Plicity is shared vocabulary with the kernel: the surface AST carries the
// same `@` marks `to_core` lowers into core's `FuncType`/`Apply`.
pub use crate::core::Plicity;

mod error;
pub use error::*;

mod names;
pub use names::*;

mod loader;
pub use loader::*;

mod nat;
pub use nat::*;

mod bin;
pub use bin::*;

mod prim;
pub use prim::*;

mod term;
pub use term::*;

mod to_core;
pub use to_core::*;

mod print;

mod prelude;
pub use prelude::*;

mod parse;

#[cfg(test)]
mod parse_tests;

mod module;
pub use module::*;
