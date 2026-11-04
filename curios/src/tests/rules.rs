//! The program tests of the rules that can admit a term: what fails when a rule stops holding, a file for each rule.
//!
//! What the soundness claim covers is `documentation/design/soundness/the-soundness-board.md`'s. The soundness board, a directory per ticket under `xboard/tickets/<part>/`, holds a ticket only for a proof of `False` that was seen admitted, so a rule nothing has broken has no witness there: these tests are what fails when such a rule stops holding. Each program is compiled as a user's is, through the prelude-replay path where every analysis sees only the user suffix and reads the prelude's verdicts back from the archive; the lattices and checkers themselves are unit-tested in their own crates.
//!
//! Strict positivity is what makes `induct` and `struct` sound: an inductive declaration claims its functor has an initial algebra, and without the gate `induct Bad | c(f : (Bad) -> False) end` inhabits `False`. The totality obligations are erasure's: it deletes types and `Prop`-sorted proofs, and both must be total, since a divergent type breaks type formation and a divergent proof proves anything, while what erasure keeps may diverge freely — which is why every totality rejection is of a position and never of a definition. Coherence is one witness per key.
//!
//! Each refusal is a pattern over the checker's error, by its variant and where it stands ([`test_support::refused`]), as [the soundness board](../../../documentation/design/soundness/the-soundness-board.md) places a held attack: a rule test that accepts any error, or any error whose message holds a fragment, is worse than none, since an invalid fixture passes it while the rule it names goes unchecked, as a probe refused with `unbound variable` passes having never reached the check at all.

mod arm_tests;
mod chain_tests;
mod coherence_tests;
mod coverage_tests;
mod effect_tests;
mod eta_tests;
mod fold_tests;
mod index_tests;
mod irrelevance_tests;
mod large_elimination_tests;
mod metavariable_tests;
mod mutation_tests;
mod one_inhabitant_tests;
mod plicity_tests;
mod positivity_composition_tests;
mod positivity_computed_type_tests;
mod positivity_index_tests;
mod positivity_occurrence_tests;
mod proposition_elimination_tests;
mod proposition_tests;
mod sequence_tests;
mod spine_tests;
mod subsumption_tests;
mod test_support;
mod totality_descent_tests;
mod totality_partiality_tests;
mod totality_proof_release_tests;
mod totality_tests;
