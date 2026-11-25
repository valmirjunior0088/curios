//! End-to-end source-to-Wasm codegen parity tests.
//!
//! **A fixture for an emitted operation taints an input.** A closed program folds in `curios-anf`'s partial evaluator before CPS, so it proves the folder's answer and nothing about the backend. An operation whose only fixtures are closed can compute a different value at runtime than every folder, or emit a shape that runs the CPS fixpoint to its backstop, and no closed fixture says so. `numeric.rs`'s `folded_matches_runtime` is the harness for a scalar operation — the same expression closed and over a host-read operand, asserted equal — and `Bytes/length(/std/rand/bytes(n)!)` is the taint for a program-shaped fixture.

mod alternation;
mod binaryen;

mod census;
// Only the fixpoint probe reads the driver the census keeps.
pub use census::*;

mod churn;
mod copying;
mod dead_loops;
mod dead_parameters;
mod entry;
mod fields;
mod frames;
mod inversion;
mod ladder;
mod leaves;
mod map_insert;
mod rebuild;
mod reconstruction;
mod specialize;
mod sweeps;
mod test_support;

mod parity;
mod shapes;

mod structural;
