/// `FnOnce`-based parser combinators: ordered choice with progress-based commitment (an alternative that consumed input owns the error unless `catch`ed), packrat memoization via `memoize`, and byte-offset errors rendered as caret snippets. The engine behind both the `.crs` surface grammar (curios-text) and the WAT parser (curios-wasm).
pub mod parser;

/// Indentation-aware pretty-printing combinators over `std::fmt::Formatter`: single-use `Printer` actions composed with `flat`/`sep_flat`/`indent` and run by `run_printer`. Every IR crate's `Display` impls (`print.rs` in curios-text, -core, -ersd, -cont, -wasm) are written in it.
pub mod printer;
