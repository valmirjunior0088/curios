//! No metavariable survives zonking into a position a checker reads.

//! End-to-end coverage for the soundness perimeter entries that nothing else guards.
//!
//! The soundness perimeter is `documentation/soundness/`, one entry per rule, each graded *probed*, *argued*, or *auditable only* (see `documentation/design/language/the-soundness-perimeter.md`). "Probed" is a claim about executable evidence, so it needs a test that fails when the rule stops holding — otherwise the grade records what someone once tried by hand and decays the moment nobody remembers doing it.
//!
//! The entries with their own homes are not repeated here: strict positivity lives in `tests::positivity`, the two totality obligations in `tests::soundness`, and witness coherence in `tests::concepts`. What is left is the large-elimination guard, `Prop` non-informativeness, coverage, and the foreign wire contract — four rules the claim rests on that had no regression test at all.
//!
//! Each rejection asserts its *own* diagnostic, following `tests::soundness`. A perimeter test that accepts any error is worse than none: an invalid fixture passes it while the rule it names goes unchecked. That is not hypothetical — the first draft of these probes "passed" on `unbound variable`, having never reached the check at all.

use crate::tests::run;

use super::test_support::*;

// `zonk_module`'s *extent*, which is where its soundness sits. Every program reaches this pass, and every "was not inferred" diagnostic is the rule firing, so what needed checking was how far it reaches rather than whether it runs. The assumption is that no unsolved metavariable survives into the module, and the module has exactly four term-bearing places for one to survive in: a definition's type, a definition's body (with the entrypoint body walked separately from both), an `induct` registry telescope, and a `struct` field telescope. The fields `zonk_module` deliberately skips carry `Vec<String>`, `Vec<(usize, Global)>` and `BTreeSet<Global>`, so its comment that concept metadata and witness markers hold no terms of their own is exact rather than approximate.
//
// The extent is where the soundness sits, because the assumption's second clause is that nothing can *later* be solved to a partial or negatively-occurring term. `check_positivity` and `record_totality` run after zonking and on the module zonking returned, and positivity reads a `Metavar` through `opaque`: its spine children at `Mixed`, and never its solution, which does not exist yet. A metavariable surviving into a registry telescope would therefore be analyzed as a hole while the term it is later solved to is analyzed not at all. Refusal before those passes run is what closes that, not the ordering by itself.
//
// Each fixture plants one unconstrained implicit in one of the four places; all four were refused. Null result, and the control is what keeps the row from being read as "declarations may not mention implicits" — it supplies the same argument in all four positions and requires the program to run.
#[test]
fn a_metavariable_does_not_survive_into_an_induct_telescope() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_AN_INDUCT_TELESCOPE}"),
        "was not inferred",
    );
}

#[test]
fn a_metavariable_does_not_survive_into_a_struct_field() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_A_STRUCT_FIELD}"),
        "was not inferred",
    );
}

#[test]
fn a_metavariable_does_not_survive_a_definitions_type() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_A_DEFINITIONS_TYPE}"),
        "was not inferred",
    );
}

#[test]
fn a_metavariable_does_not_survive_the_entrypoint_body() {
    rejected_by(
        &format!("{AN_UNCONSTRAINED_IMPLICIT}{A_METAVARIABLE_IN_THE_ENTRYPOINT_BODY}"),
        "was not inferred",
    );
}

#[test]
fn a_solved_metavariable_still_reaches_every_zonked_position() {
    assert_eq!(
        run(&format!(
            "{AN_UNCONSTRAINED_IMPLICIT}{A_SOLVED_METAVARIABLE_IN_EVERY_POSITION}"
        )),
        b"0"
    );
}
