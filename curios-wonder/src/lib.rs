//! `wonder`: what the compiler knows about a program, handed out as records.
//!
//! A query runs the compiler as far as a question needs and reads the answer off what the compiler already decided — a diagnostic is the [`Report`](curios_utilities::Report) the compile path would have printed, a goal is the entry of the batch elaboration already builds — and computes none of its own, as `README.md` decides.
//!
//! The command line renders a record for a reader (`ask`) and a language server adapts it to its protocol (`server`), each converting at its own edge. A location is a [`Span`](curios_utilities::Span): a source identity and a half-open UTF-8 byte range, with line and scalar-counted column derived beside it as the caret is; UTF-16 exists only in the server's adapter. Why the engine names no transport's types and no product's, and links no back end, are `README.md`'s decisions, so `cargo test -p curios-wonder` needs no `cargo xtask launcher build`, and `wasm-optm` is handed back as the module for the product that owns Binaryen to finish.
//!
//! [`diagnostics`](diagnostics()) reaches the store it is handed through one cache, which files a unit as `README.md`'s "A question files the units it compiled from disk" decides and keeps the rest for the session alone. A unit the store holds from an earlier text of the same sources is a baseline: the fold compiles the unit over it, and what it compiled is filed like any other unit. A caller that asks again — the language server — holds a [`Session`](curios_verdicts::Session) that keeps what each question compiled, a nearer baseline than anything filed, and dies with the process.

#[cfg(test)]
mod tests;

/// What a program read from standard input is called: in a status line, and — because the source it is parsed from is labelled with this too — in the `--> <stdin>:2:1` header of any diagnostic about it. One constant, so a reader is never told two names for one program. Angle brackets because no file is spelled that way, so neither line reads as naming something openable.
pub const STDIN_LABEL: &str = "<stdin>";

/// The prefix a module on standard input is mounted at. A loose file written as a module is mounted at its stem, and standard input has none, so it is mounted under the name of where it came from: the `one` a heredoc declares is `/stdin/one`.
pub const STDIN_MOUNT: &str = "stdin";

mod record;
pub use record::*;

mod declared;
pub use declared::*;

mod diagnostics;
pub use diagnostics::*;

mod document;
pub use document::*;

mod stage;
pub use stage::*;

mod named;
pub use named::*;

mod cost;
pub use cost::*;

mod ask;
pub use ask::*;

mod server;
pub use server::*;

mod lint;
pub use lint::*;
