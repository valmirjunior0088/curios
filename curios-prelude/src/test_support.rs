//! Where `/std` is authored, and its source as a package: what another crate's suites compile it from or ask about one of its files by.
//!
//! The build script compiles the same file by its path and makes the prelude from it; the library reads it nowhere else.

#[path = "std_module.rs"]
mod std_module;
pub use std_module::*;
