//! The parser combinator DSL: `FnOnce`-based, with freely backtracking ordered choice that an alternative stops with [`commit`], packrat memoization via [`memoize`], and byte-offset errors rendered as caret snippets. The engine behind both the `.crs` surface grammar (`curios-module`) and the WAT parser (`curios-wasm`).
//!
//! Why a parser is a function of the input, why choice backtracks until an alternative commits, and why memoization is packrat keyed by nonterminal and offset are `README.md`'s decisions; why this and `curios-print` are separate crates is `curios-print/README.md`'s.

mod state;
pub use state::ParserState;

mod error;
pub use error::{ParserError, Stated};

mod parser;
pub use parser::{
    Boxed, Built, IntoParser, Parsed, Parser, Ready, commit, expecting, look_ahead, tagging,
    uncommit,
};

mod memo;
pub use memo::{memoize, run_parser};

mod primitive;
pub use primitive::{
    ahead, fail, fail_from, is_blank, not_ahead, obtained, preceded_by_space, pure, raise, refusal,
    settled, shown, take_eof, take_exact, take_n, take_while, unseen,
};

mod span;
pub use span::{Mark, mark, spanned};

mod repeat;
pub use repeat::{Recovered, Repeated, many0, many1, recover, sep_by0_trailing, sep_by1_trailing};
