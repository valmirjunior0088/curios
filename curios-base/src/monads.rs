//! The two combinator DSLs, and the one part of this crate that is *not*
//! flattened into the root. Both are monads and both name their unit `pure`,
//! so a flat re-export would leave `curios_base::pure` ambiguous at every use
//! site — while `printer::pure` and `parser::pure` are each unambiguous and
//! tell the reader which DSL a `Display` impl or a grammar rule is written in.
//! Keeping them namespaces is what lets the shared vocabulary coexist, for the
//! same reason `curios_core::kernel` stays a namespace beside the elaborator's
//! like-named judgments.

/// `FnOnce`-based parser combinators: ordered choice with progress-based commitment (an alternative that consumed input owns the error unless `catch`ed), packrat memoization via `memoize`, and byte-offset errors rendered as caret snippets. The engine behind both the `.crs` surface grammar (curios-text) and the WAT parser (curios-wasm).
pub mod parser;

/// Indentation-aware pretty-printing combinators over `std::fmt::Formatter`: single-use `Printer` actions composed with `flat`/`sep_flat`/`indent` and run by `run_printer`. Every IR crate's `Display` impls (`print.rs` in curios-text, -core, -ersd, -cont, -wasm) are written in it.
pub mod printer;
