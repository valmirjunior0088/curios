//! Coverage for the soundness board entries that nothing else guards.
//!
//! The soundness board is `documentation/design/soundness/`, one entry per rule, each graded *probed*, *argued*, or *auditable only* (see `documentation/design/soundness/the-soundness-board.md`). "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A board test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked, as a probe refused with `unbound variable` passes having never reached the check at all.

mod arm_tests;
mod coverage_tests;
mod effect_tests;
mod eta_tests;
mod fold_tests;
mod index_tests;
mod metavariable_tests;
mod mutation_tests;
mod proposition_tests;
mod subsumption_tests;
mod test_support;
mod totality_tests;
