//! The native back end: [`optimize`] and [`to_cwasm`] from `compile`, and the [`engine`] a stored payload is addressed under. This is the only crate that brings both native backends together — Cranelift via `curios-cranelift`, Binaryen via `curios-binaryen` — so the CLI binary and the cross-stage suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on `curios-native`, which links neither, and the questions `wonder` answers and the pages `document` writes stay in `curios-wonder` and `curios-document`, under both.
//!
//! Compiling is not here. `curios_pipeline::compile_with_prelude` and its siblings put the fixed prelude in front of the fold, next to the fold they configure; this crate takes the `curios_wasm::Module` that comes out and is indifferent to where it came from.
//!
//! **What `curios test` compiles and runs is here too** (`testing`), with what answers a program's `foreign` rows (`link`), so a caller running a package's tests in process — `curios-prelude`'s own suite — goes through the pipeline the command line does, and the binary keeps only what it shows.

mod compile;
pub use compile::{engine, fault, optimize, to_cwasm};

mod link;
pub use link::{bindings_of, linked_of, unlinked};

mod testing;
pub use testing::{
    FailedCheck, Outcome, TestRun, TestSubject, Tested, opened, page_name, supplied, test_subjects,
};

use curios_profile::Tally;

/// The variable naming the cabinet every project a run sees files into, in place of each project's own.
///
/// **This and nothing else, and read by the command line alone — there is deliberately no divined default.** A toolchain that writes into a home directory nobody pointed it at is doing something the person who ran it did not ask for, and `.curios/` beside the project is the one generated directory this design admits to. So sharing is opt in: unset, every project keeps its own cabinet, which costs a re-fetch and a re-certification per project and surprises nobody; set, two projects pinning one revision materialize and compile it once.
///
/// Read once, where the command line starts, and handed to whatever opens a governing root, as `NO_COLOR` is read there: no library reads the environment, so a suite opening a cabinet opens the one it names whatever the shell running it holds, and a test that runs the command line clears the variable for the child it runs.
pub const CABINET: &str = "CURIOS_CABINET";

/// The counts a recorder this crate opens is handed beside the allocator's: the look count `curios-core` keeps, which `curios-profile` names nothing of. The command line hands them under `--profile`, and each measurement of the suite to the recorder it opens, so every stream of this crate's carries a span's looks.
pub const TALLIES: &[Tally] = &[Tally {
    name: "looks",
    read: curios_core::looks,
}];

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios::test_support` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;

#[cfg(test)]
mod tests;
