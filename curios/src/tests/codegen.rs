//! End-to-end source-to-Wasm codegen parity tests.

mod census;
// The churn probe hears the engine's collection announcements through `curios-profile`'s log bridge, which exists only under its `enabled` feature — reached here through this crate's `profile` feature, which the probe's recorded command enables via `--all-features`.
#[cfg(feature = "profile")]
mod churn;
mod ladder;
mod parity;
mod structural;
