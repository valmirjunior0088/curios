//! What a recipe needs of a tool cargo does not bring: the oldest version it runs under, and the command that installs it.
//!
//! Each is spelled once, here, so the recipe that refuses and the message that says why read the same value.

/// The oldest Node the browser suite runs under: the first whose engine runs WebAssembly GC unflagged and whose test runner expands a pattern itself.
pub(crate) const NODE_FLOOR: u32 = 22;

/// The oldest nextest the suite runs under: the first whose `--partition` takes `slice:`, which is how the `test` recipe cuts a shard.
pub(crate) const NEXTEST_FLOOR: [u32; 3] = [0, 9, 127];

/// What fixes a nextest that is absent or too old, whichever it is. Cargo's own install rather than a prebuilt download, because it is one spelling on every platform and needs nothing a clone does not already have.
pub(crate) const NEXTEST_INSTALL: &str = "cargo install cargo-nextest --locked";
