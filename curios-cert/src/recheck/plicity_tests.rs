//! Marks: the ones a registry entry declares, and the written ones the kernel holds to them.

use {
    super::test_support::*,
    crate::{Counted, Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::Free,
    curios_utilities::Plicity,
};

/// A constructor's marks are read by one kernel rule, the arm's, which holds each binder an arm writes to the payload it binds ([`an_arm_binds_under_its_payloads_marks`]). No other rule reads them, and a constructor's *value* carries none: the same declaration under its honest mark and under a lying one, with a value of it and no match over it, must produce the same verdicts. That each binder has a mark is no one's to check: a mark is its telescope entry's own, so a signature a mark short of its binders cannot be written, and the same holds of the marks a declaration's parameters stand under in its arity.
///
/// That reason is an *inventory*: every consumer of a registry entry, read against the clauses. An inventory is exactly the kind of claim that goes stale as code moves, and the polarity vector beside it on the same entry has `curios-analysis`'s `a_carried_polarity_vector_is_recomputed_rather_than_believed` holding its own version of this. This holds the plicity half executably instead.
///
/// The lie is merely wrong — an `Implicit` mark where the declaration says `Explicit`. What must not happen is this kernel quietly deciding a value of the family *differently* because of it, which is what a nominal rule reading the marks outside an arm would introduce without any clause noticing.
///
/// The control is [`a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector`]: the same lie beside a genuine error. Without it, "the kernel ignores the mark" and "the kernel ignores this module" read alike.
#[test]
fn a_constructors_mark_decides_nothing_of_its_value() {
    assert_eq!(
        fixture_verdicts(
            &plicity_module(true, 1),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        fixture_verdicts(
            &plicity_module(false, 1),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        "a mark no rule reads outside an arm changed a verdict",
    );
    assert_eq!(
        fixture_verdicts(
            &plicity_module(false, 1),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "both sides must be accepted, or the equality above is two refusals agreeing",
    );
}

/// The control for the fixture above: under the same lying plicity vector, an ordinary error is still caught.
#[test]
fn a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector() {
    assert!(
        !fixture_verdicts(
            &plicity_module(false, 0),
            1_000_000,
            &Globals::default(),
            SYNTAX
        )
        .is_empty(),
        "the kernel accepted a constructor application at the wrong payload count",
    );
}

/// An argument stands under its parameter's mark. The application rule holds each written mark to the binder it fills, where the binder is at hand, so a spine's marks are a function of its head's type — which is what lets conversion compare two spines by their arguments alone, in this checker as in the elaborator.
///
/// The control is the same application under the parameter's own mark: refusing every application would pass the refusal alone.
#[test]
fn an_argument_stands_under_its_parameters_mark() {
    assert_eq!(
        fixture_verdicts(
            &marked_apply_module(Plicity::Explicit),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "an argument under its parameter's own mark was refused",
    );

    let refused = fixture_verdicts(
        &marked_apply_module(Plicity::Implicit),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        refused.iter().any(|verdict| matches!(
            verdict.error,
            Error::Mark {
                counted: Counted::Arguments,
                position: 1,
                declared: Plicity::Explicit,
                written: Plicity::Implicit,
            }
        )),
        "an argument written `@` at a plain parameter was not refused by its mark: {refused:?}",
    );
}

/// An arm binds each payload under the payload's own mark: the arm rule holds the marks an arm writes to its constructor's, as the application rule holds an argument's to its parameter's.
///
/// The control is `recheck::occurrence_tests`' `an_arm_matching_its_payload_still_reduces`, the same arm under the payload's own mark.
#[test]
fn an_arm_binds_under_its_payloads_marks() {
    let refused = fixture_verdicts(
        &arm_module(vec![(Plicity::Implicit, Free::local(996, Some("a")))]),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        refused.iter().any(|verdict| matches!(
            verdict.error,
            Error::Mark {
                counted: Counted::ArmBinders,
                position: 1,
                declared: Plicity::Explicit,
                written: Plicity::Implicit,
            }
        )),
        "an arm binding a plain payload under `@` was not refused by its mark: {refused:?}",
    );
}
