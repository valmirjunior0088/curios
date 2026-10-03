//! No metavariable survives zonking into a position a checker reads.

use {super::test_support::*, crate::tests::run};

// **A solution keeps the universe instance its spelling had.** `k`'s `@x` is born at the call, outside the arm whose scrutinee, `h(Eq()(0, 0))`, is refined to the case, and solved inside it against `w`'s type — with that refinement suppressed, since it was not born under it, so its solution holds outside the arm. Born inside, it would be solved under the refinement and never reach the suppressed spelling this pins. Under suppression the elaborator's reducer keeps the probe as spelled rather than the refinement's key, which erases universe instances by design, being a spelling to look up by: a value committed from the key would hold a bare `/std/Eq/Eq`, which the kernel refuses as an occurrence stating no instance.
#[test]
fn an_inferred_value_under_a_refined_proof_keeps_its_universe_instance() {
    assert_eq!(
        run(AN_INFERRED_VALUE_UNDER_A_REFINED_PROOF_KEEPS_ITS_UNIVERSE_INSTANCE),
        b"1"
    );
}

// `zonk_module`'s *extent*, which is where its soundness sits. Every program reaches this pass, and every "was not inferred" diagnostic is the rule firing, so what matters is how far it reaches rather than whether it runs. The assumption is that no unsolved metavariable survives into the module, and the module has exactly four term-bearing places for one to survive in: a definition's type, a definition's body (with the entrypoint body walked separately from both), an `induct` registry telescope, and a `struct` field telescope. The fields `zonk_module` deliberately skips carry `Vec<String>`, `Vec<(usize, Global)>` and `BTreeSet<Global>`, so its comment that concept metadata and witness markers hold no terms of their own is exact rather than approximate.
//
// The extent is where the soundness sits, because the assumption's second clause is that nothing can *later* be solved to a partial or negatively-occurring term. `check_positivity` and `record_totality` run after zonking and on the module zonking returned, and positivity reads a `Metavar` through `opaque`: its spine children at `Mixed`, and never its solution, which does not exist yet. A metavariable surviving into a registry telescope would therefore be analyzed as a hole while the term it is later solved to is analyzed not at all. Refusal before those passes run is what closes that, not the ordering by itself.
//
// Each fixture plants one unconstrained implicit in one of the four places, and all four are refused; the control is what keeps the row from being read as "declarations may not mention implicits" — it supplies the same argument in all four positions and requires the program to run.
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
