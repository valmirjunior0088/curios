//! The pretty-printing combinator DSL: single-use [`Printer`] actions composed through `flat`/`sep_flat`/`indent` and run by [`run_printer`]. The `print.rs` in `curios-text`, `curios-core` and `curios-wasm` writes its `Display` impls in it; `curios-ersd` and `curios-cont` print their arenas directly and depend on nothing here. The layout decision it implements, with width as the mode and groups and lines as the only per-printer vocabulary, is `documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md`.
//!
//! The document algebra is defined over `std::fmt` alone, and the crate depends on nothing. Why, and why it is split from `curios-parse`, are `README.md`'s decisions.

mod combinator;
pub use combinator::*;

mod document;
pub use document::*;

mod run;
pub use run::*;

#[cfg(test)]
mod tests;
