//! The parser combinator DSL: `FnOnce`-based, with freely backtracking ordered choice that an alternative stops with [`commit`], packrat memoization via [`memoize`], and byte-offset errors rendered as caret snippets. The engine behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`).
//!
//! Why a parser is a function of the input, why choice backtracks until an alternative commits, and why memoization is packrat keyed by nonterminal and offset are `README.md`'s decisions; why this and `curios-print` are separate crates is `curios-print/README.md`'s.

mod state;
pub use state::*;

mod error;
pub use error::*;

mod parser;
pub use parser::*;

mod memo;
pub use memo::*;

mod primitive;
pub use primitive::*;

mod span;
pub use span::*;

mod repeat;
pub use repeat::*;
