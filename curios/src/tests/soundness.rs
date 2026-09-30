//! The two totality obligations, end to end.
//!
//! Erasure deletes types and it deletes `Prop`-sorted proofs, and both must be total: a divergent type breaks type formation, a divergent proof proves anything. What erasure *keeps* may diverge freely, which is why every rejection here is of a position and never of a definition.
//!
//! The size lattice and the classifier are unit-tested in `curios-analysis/src/totality/tests.rs`; these check what a user can observe, through the prelude-replay path a real program takes — where the analysis sees only the user suffix and reads the prelude's verdicts back from the archive.
//!
//! Each rejection asserts the *diagnostic*, not merely that compilation failed. A soundness test that accepts any error is worthless: a typo in the fixture would pass it while the hole stayed open.

mod proof_release_tests;
mod test_support;
mod totality_tests;
