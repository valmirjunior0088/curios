//! Surface syntax of the `.crs` language — the compiler pipeline's first stage (curios-text → curios-core → curios-ersd → curios-cont → curios-wasm): the parser, the printer that round-trips what was written, the module system (discovery, `use` resolution, visibility), and the `into_core` lowering that hands the rest of the pipeline a flat `curios_core::Module`.
//!
//! The tree is deliberately literal: sugar — function-definition signatures, infix operators, destructuring patterns, postfix `!`, string and spread literals — is kept verbatim so [`Term`]s print back as written, and is undone only during lowering. Bare stage tests call [`into_core`] with an explicit [`SyntaxRegistry`]. Product compilation restores [`PreparedPrelude`] from `curios-prelude`, supplies an optional [`RootSource`] only for the entry program's file-backed modules, and calls [`into_core_with_prelude`].

mod error;
pub use error::*;

#[cfg(feature = "archive")]
mod archive;
#[cfg(feature = "archive")]
pub use archive::*;

mod names;
pub use names::*;

mod syntax;
pub use syntax::*;

mod root_source;
pub use root_source::*;

mod nat;
pub use nat::*;

mod prim;
pub use prim::*;

mod term;
pub use term::*;

mod into_core;
pub use into_core::*;

mod print;
use print::*;

mod prelude;
pub use prelude::*;

mod parse;

mod module;
pub use module::*;
