//! Coverage for the rules that can admit a term, where nothing else guards one.
//!
//! Each rule is argued in an entry under `documentation/design/soundness/` (see `documentation/design/soundness/the-soundness-board.md`). The soundness board, `xboard/src/board/`, holds a ticket only for a proof of `False` that was seen admitted, so a rule nothing has broken has no witness there: these tests are what fails when such a rule stops holding.
//!
//! The rules with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`.
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
