//! End-to-end source-to-Wasm codegen parity tests.
//!
//! **A fixture for an emitted operation taints an input.** A closed program folds in `curios-ersd`'s partial evaluator before Cont, so it proves the folder's answer and nothing about the backend. `Flt/rem` computed a different value at runtime than every folder for as long as its only fixtures were closed, and `Flt/of_str` was lossy and its emitted shape ran the Cont fixpoint to its backstop while every test fed it a literal string; each was found the day a runtime value reached it. `numeric.rs`'s `folded_matches_runtime` is the harness for a scalar operation — the same expression closed and over a host-read operand, asserted equal — and `Bytes/len(/std/rand/bytes(n)!)` is the taint for a program-shaped fixture.

mod big_nat_limb;
mod census;
#[cfg(feature = "profile")]
pub(super) use census::TOML_DRIVER;
// The churn probe hears the engine's collection announcements through `curios-profile`'s log bridge, which exists only under its `enabled` feature — reached here through this crate's `profile` feature, which the probe's recorded command enables via `--all-features`.
#[cfg(feature = "profile")]
mod churn;
mod ladder;
mod map_wall;
mod parity;
mod shapes;
mod structural;
