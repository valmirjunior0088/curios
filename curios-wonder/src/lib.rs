//! `wonder`: what the compiler knows about a program, handed out as records.
//!
//! A query runs the compiler as far as a question needs and reads the answer off what the compiler already decided — a diagnostic is the [`Report`](curios_common::Report) the compile path would have printed, a goal is the entry of the batch elaboration already builds — and computes none of its own, as `README.md` decides.
//!
//! The command line renders a record for a reader (`ask`) and a language server adapts it to its protocol (`server`), each converting at its own edge. A location is a [`Span`](curios_common::Span): a source identity and a half-open UTF-8 byte range, with line and scalar-counted column derived beside it as the caret is; UTF-16 exists only in the server's adapter. Why the engine names no transport's types and no product's, and links no back end, are `README.md`'s decisions, so `cargo test -p curios-wonder` needs no `cargo xtask launcher build`, and `wasm-optm` is handed back as the module for the product that owns Binaryen to finish.
//!
//! [`diagnostics`](diagnostics()) reaches the store it is handed through one cache, which files a unit as `README.md`'s "A question files the units it compiled from disk" decides and keeps the rest for the session alone. A unit the store holds from an earlier text of the same sources is a baseline: the fold compiles the unit over it, and what it compiled is filed like any other unit. A caller that asks again — the language server — holds a [`Session`](curios_verdict::Session) that keeps what each question compiled, a nearer baseline than anything filed, and dies with the process.

#[cfg(test)]
mod tests;

/// What a program read from standard input is called: on the line that names a tree's subject, and — because the source it is parsed from is labelled with this too — in the place any diagnostic about it closes on, `<stdin>:2:1`. One constant, so a reader is never told two names for one program. Angle brackets because no file is spelled that way, so neither reads as naming something openable.
pub const STDIN_LABEL: &str = "<stdin>";

/// The prefix a module on standard input is mounted at. A loose file written as a module is mounted at its stem, and standard input has none, so it is mounted under the name of where it came from: the `one` a heredoc declares is `/stdin/one`.
pub const STDIN_MOUNT: &str = "stdin";

mod record;
use record::*;
pub use record::{Diagnosis, Severity};

mod declared;
use declared::*;

mod diagnostics;
use diagnostics::*;
pub use diagnostics::{Diagnosed, Origin, Subject, diagnostics};

mod document;
pub use document::std_documentation;

mod stage;
use stage::*;

mod named;
use named::*;
pub use named::{Declaration, Named, Written};

mod cost;
use cost::*;

mod ask;
use ask::*;
pub use ask::{
    Ended, Place, Staged, Telling, answer, file_subject, wonder_at, wonder_cost,
    wonder_declarations, wonder_diagnostics, wonder_stage, wonder_tests, written_as_a_module,
};

mod server;
pub use server::{SETTLE, serve};

mod lint;
pub use lint::{Linted, lint};

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_wonder::test_support` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;
