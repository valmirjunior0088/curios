//! The pretty-printing combinator DSL: single-use [`Printer`] actions composed through `flat`/`sep_flat`/`indent` and run by [`run_printer`]. The `print.rs` in `curios-module`, `curios-core` and `curios-wasm` writes its `Display` impls in it; `curios-anf` and `curios-cps` print their arenas directly and depend on nothing here. The layout decision it implements, with width as the mode and groups and lines as the only per-printer vocabulary, is `documentation/design/tooling/a-printer-states-each-fact-once-where-it-is-bound.md`.
//!
//! The document algebra is defined over `std::fmt` alone, and the crate depends on nothing. Why, and why it is split from `curios-parse`, are `README.md`'s decisions.

mod combinator;
pub use combinator::{
    ends, fill, flat, flow, group, group_within, hang, hard_line, if_break, indent, line, named,
    named_at, own_line_if_broken, pure, reached, sep_flat, soft_line, word,
};

mod document;
pub use document::{FillItem, FillStyle, Printer};

mod run;
pub use run::{
    Annotated, Annotation, Original, Owed, each_named, render_annotated, run_printer,
    run_printer_placing, run_printer_within,
};

#[cfg(test)]
mod tests;
