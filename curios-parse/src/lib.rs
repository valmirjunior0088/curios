//! The parser combinator DSL: `FnOnce`-based, with ordered choice under progress-based commitment (an alternative that consumed input owns the error unless [`catch`]ed), packrat memoization via [`memoize`], and byte-offset errors rendered as caret snippets. The engine behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`).
//!
//! Why a parser is a single-use `FnOnce`, why choice commits on progress, and why memoization is packrat keyed by nonterminal and offset are `README.md`'s decisions. This and `curios-print` are separate crates rather than two modules of one, so that [`curios_parse::pure`](pure) and `curios_print::pure` are unambiguous at every use site and each crate stays flat like every other; `curios-print/README.md` states that decision and what it replaced.

mod state;
pub(crate) use state::*;

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
