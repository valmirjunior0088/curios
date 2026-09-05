//! `wonder`: what the compiler knows about a program, handed out as records.
//!
//! **A query is a pure function of a compilation.** This crate runs the compiler as far as a question needs and reads the answer off what the compiler already decided — a diagnostic is the [`Report`](curios_utilities::Report) the compile path would have printed, a goal is the entry of the batch elaboration already builds. It resolves no name and types no term of its own, on the `Intrinsic::signature` principle: one source of truth that consumers walk, so what this crate says and what the compiler did cannot disagree.
//!
//! **Two consumers, one record.** The command line renders a record for a reader (`ask`) and a language server adapts it to its protocol (`server`); both are transports, and neither is secondary. What keeps them honest is that every rendering is computed from the record rather than from the compiler beside it — the CLI reads `wonder diagnostics` exactly as `curios run` would have reported the same program, because it is the same [`Report`](curios_utilities::Report) rendered.
//!
//! **Coordinates are the compiler's.** A location is a [`Span`](curios_utilities::Span): a source identity and a half-open UTF-8 byte range, with line and scalar-counted column derived beside it as the caret has always been. UTF-16 exists only in the server's adapter, converted at the boundary in both directions.
//!
//! **The engine names no transport's types, and no product's.** Nothing in the engine reads a file by a name it was not handed, encodes JSON, or spells an LSP type; a record is plain data over the compiler's coordinates, and a transport converts at its own edge. Nor does anything here link a back end: this crate sits under `curios` and reaches neither Binaryen nor Wasmtime nor the launcher the native compiler embeds — `cargo tree -p curios-wonder --edges normal` names none of them — so a question costs the compiler and nothing after it, and `cargo test -p curios-wonder` needs no `cargo x runtime`. The one rung the driver cannot render, `wasm-optm`, is handed back as the module for the product that owns Binaryen to finish, which is the one place a transport here takes something from above.
//!
//! **A query never writes the store.** Dependencies come from the store already built, and one that is not is compiled in memory and forgotten: the store addresses a unit by content, and a server that filed what it checked would file a unit per keystroke. [`diagnostics`](diagnostics()) wraps the store it is handed in one that files nothing — while still placing every unit the fold compiles, because a slot is addressed after the units before it and a chain with a gap in it misses for the whole tail — which is the rule stated in code rather than in a caller's discipline.

#[cfg(test)]
mod tests;

/// What a program read from standard input is called: in a status line, and — because the source it is parsed from is labelled with this too — in the `--> <stdin>:2:1` header of any diagnostic about it. One constant, so a reader is never told two names for one program. Angle brackets because no file is spelled that way, so neither line reads as naming something openable.
pub const STDIN_LABEL: &str = "<stdin>";

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

mod ask;
pub use ask::*;

mod server;
pub use server::*;

mod lint;
pub use lint::*;
