//! The pretty-printing combinator DSL: single-use [`Printer`] actions composed through `flat`/`sep_flat`/`indent` and run by [`run_printer`]. The `print.rs` in `curios-text`, `curios-core` and `curios-wasm` writes its `Display` impls in it; `curios-ersd` and `curios-cont` print their arenas directly and depend on nothing here. The layout decision it implements, with width as the mode and groups and lines as the only per-printer vocabulary, is `documentation/design/toolchain/one-document-algebra-decides-layout-for-every-printer.md`.
//!
//! **This crate depends on nothing.** The document algebra is defined over `std::fmt` alone, so no Curios term reaches it — which is what lets every IR crate depend on it without any of them depending on each other.
//!
//! Split from `curios-parse` rather than sharing a module namespace with it, because both name their unit `pure`; `README.md` states the decision and what it replaced.

mod combinator;
mod document;
mod run;

pub use {combinator::*, document::*, run::*};

#[cfg(test)]
mod tests;
