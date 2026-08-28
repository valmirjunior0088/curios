//! What a mismatch report owes its reader: two sides that do not read as the same thing.
//!
//! A report's spelling suppresses detail so a type reads the way source writes it. Every suppressed axis is a way for two distinct terms to arrive at one string, and a mismatch rendered that way states `X ≠ X` — a message carrying strictly less than the fact that something is wrong. These fix the escape hatch, not the suppression: the ordinary report keeps every abbreviation.

use {
    super::*,
    curios_core::{Global, Level, Term},
    curios_utilities::Qualifier,
};

fn family(name: &str, universes: Vec<Level>) -> Term {
    Term::induct_type_at(
        Global::Authored(Qualifier::from([name])),
        universes,
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    )
}

/// The reader's spelling, as `Error::reports` builds it: universes erased, metavariables anonymous.
fn rendered(inferred: Term, expected: Term) -> String {
    let error = Error::type_mismatch(inferred, expected);
    let spelling = Rc::new(
        Spelling::default()
            .with_erased_universes()
            .with_anonymous_metavars(),
    );
    Displayed(&error, spelling).to_string()
}

/// One family at two instances. Erasing the instances erases the whole of the disagreement, so the report has to put them back — the shape a struct's parameter telescope produced while its registry entry went unelaborated.
#[test]
fn a_disagreement_only_in_universe_instances_shows_them() {
    let shown = rendered(
        family("Carrier", vec![Level::zero()]),
        family("Carrier", vec![]),
    );

    let (inferred, expected) = shown
        .split_once("\n  expected: ")
        .expect("a mismatch renders both sides");
    assert_ne!(
        inferred, expected,
        "a mismatch may not state its two sides in identical words:\n{shown}"
    );
}

/// The ordinary case keeps every abbreviation: two genuinely different families already read differently, so nothing is un-suppressed and the reader is not handed core's spelling for no reason.
#[test]
fn a_disagreement_the_reader_can_already_see_keeps_its_spelling() {
    let shown = rendered(family("Bool", vec![]), family("Nat", vec![]));

    assert!(
        shown.contains("inferred: /Bool") && shown.contains("expected: /Nat"),
        "expected the reader's own spelling:\n{shown}"
    );
}
