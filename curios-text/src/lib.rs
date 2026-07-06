//! Surface syntax of the `.crs` language — the compiler pipeline's first stage (curios-text → curios-core → curios-ersd → curios-cont → curios-wasm): the parser, the printer that round-trips what was written, the module system (discovery, `use` resolution, visibility), and the `to_core` lowering that hands the rest of the pipeline a flat `curios_core::Module`.
//!
//! The tree is deliberately literal: sugar — function-definition signatures, infix operators, destructuring patterns, postfix `!`, string and spread literals — is kept verbatim so [`Term`]s print back as written, and is undone only during lowering. The usual path through the crate: parse an [`Entrypoint`] (or [`Module`]) via `FromStr`/`from_path`, pick a [`RootSource`] to serve file-backed `mod` declarations — normally wrapped by [`prelude`], which embeds the `sys`/`syn`/`std` roots in the binary — and call [`to_core`].

mod error;
pub use error::*;

mod names;
pub use names::*;

mod root_source;
pub use root_source::*;

#[cfg(test)]
mod root_source_tests;

mod nat;
pub use nat::*;

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
