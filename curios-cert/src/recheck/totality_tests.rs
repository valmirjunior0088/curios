//! Totality verdicts across walks: the certifier's record read where a unit is carried, and elaboration's stamp compared, never read, where an item is judged.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::{Erased, test_support::*},
    curios_common::Qualifier,
    curios_core::{Certification, Certified, Global, Totality},
};

/// A totality stamp asserts the *closure* — elaboration's classification closes over mentions as the kernel's does — so the cross-check compares it against the closed verdict, not the local half.
///
/// `reaches` is total in itself, stamped `Total`, and mentions the diverging `sink`: only the closure contradicts it. A comparison against the local half would pass it. Nothing reads the stamp afterwards — a later walk reads the certifier's record, which [`a_carried_totality_stamp_is_ignored_where_the_certifiers_record_classifies`] holds — so what this guards is the two checkers' disagreement being reported where they disagree.
///
/// No surface program reaches it: the only stamp writer is `curios-elab`'s `stamp_totality`, whose closure is correct, so the lie must be constructed — which is why this lives here and why nothing in the corpus could have found it. The control is [`an_honest_stamp_on_a_definition_reaching_a_partial_one_is_accepted`]: a `Partial` stamp on the same definition is a classification, not an error, and must stay accepted.
#[test]
fn a_totality_stamp_contradicted_only_by_the_closure_is_refused() {
    let verdicts = fixture_verdicts(
        &stamp_trial_definitions(Totality::Total, false),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| {
            verdict.name.as_ref() == Some(&Global::Authored(Qualifier::from(["reaches"])))
                && matches!(
                    &verdict.error,
                    Error::NotTotal {
                        erased: Erased::Proof,
                        reached: Some(name),
                    } if *name == Global::Authored(Qualifier::from(["sink"]))
                )
        }),
        "a `Total` stamp contradicted only by the closure over its mentions was not refused: {verdicts:?}",
    );
}

/// The control: the same two definitions honestly stamped stay accepted — partiality is a classification, and a rule refusing every mention of a partial definition would pass the witness above by breaking the language.
#[test]
fn an_honest_stamp_on_a_definition_reaching_a_partial_one_is_accepted() {
    assert_eq!(
        fixture_verdicts(
            &stamp_trial_definitions(Totality::Partial, false),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "an honestly stamped definition reaching a partial one was refused",
    );
}

/// The definition whose stamp is on trial.
fn reaches() -> Global {
    Global::Authored(Qualifier::from(["reaches"]))
}

/// Whether `verdicts` refuse `held : Vouched` for reaching `reaches` — the refusal a walk owes the proof whenever it knows `reaches` is partial.
fn refuses_the_proof_reaching_the_lie(verdicts: &[Verdict]) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name.as_ref() == Some(&Global::Authored(Qualifier::from(["held"])))
            && matches!(
                &verdict.error,
                Error::NotTotal {
                    erased: Erased::Proof,
                    reached: Some(name),
                } if *name == reaches()
            )
    })
}

/// The walk over the proof alone, the lying library mounted beneath it as the compile path mounts a unit — with `certification` as the record filed beside it.
fn carried_beneath_the_lie(certification: &Certification) -> Vec<Verdict> {
    fixture_verdicts(
        &carried_proof_definitions(),
        1_000_000,
        &AlreadyJudged::with(
            &stamp_trial_definitions(Totality::Total, false),
            certification,
        )
        .globals(),
        SYNTAX,
    )
}

/// A later walk reads a carried unit's totality from the certifier's record and never from the stamps elaboration wrote, so a proof reaching a lying stamp is refused carried exactly as it is refused judged fresh.
///
/// The library is certified with `reaches` honestly stamped `Partial`, and the record that walk leaves is mounted beside the same terms stamped `Total`. `held : Vouched` mentions only `reaches`, so its walk refuses it exactly when the environment's non-total set holds `reaches` — which the record says it does, whatever the stamp claims. Read off the stamp instead, the set would hold `sink` alone and the carried walk would certify the proof with no verdicts at all; judged in one module from an empty environment, the same proof is refused, which is the second half here.
#[test]
fn a_carried_totality_stamp_is_ignored_where_the_certifiers_record_classifies() {
    let honest = fixture_certified(
        &stamp_trial_definitions(Totality::Partial, false),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert_eq!(
        honest.verdicts,
        Vec::new(),
        "the honest library is certified"
    );
    assert_eq!(
        honest.certification.totality(&reaches()),
        Some(Totality::Partial)
    );

    let carried = carried_beneath_the_lie(&honest.certification);
    assert!(
        refuses_the_proof_reaching_the_lie(&carried),
        "carried beneath its record, the proof reaching the lying stamp must be refused: {carried:?}",
    );

    let judged = fixture_verdicts(
        &stamp_trial_definitions(Totality::Total, true),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        refuses_the_proof_reaching_the_lie(&judged),
        "judged fresh, the same proof must be refused for reaching the mis-stamped definition: {judged:?}",
    );
}

/// A unit mounted with an empty record — an environment built by hand, with no walk behind it — is classified by the walk that reads it, from its items, and the stamps on those items are not consulted: the lying library beneath the proof still has the proof refused.
#[test]
fn a_unit_mounted_with_an_empty_record_is_classified_by_the_reading_walk() {
    let carried = carried_beneath_the_lie(&Certification::default());

    assert!(
        refuses_the_proof_reaching_the_lie(&carried),
        "with an empty record, the reading walk must classify the library itself and refuse the proof: {carried:?}",
    );
}

/// Obligation (V) fires at an argument position, where the corpus never reaches it.
///
/// [The soundness board](../../../documentation/design/soundness/the-soundness-board.md)'s *Rejected* paragraph says the corpus "passes obligation (V) inert at argument positions", so a fixture reaching it has to be constructed. [`proof_at_an_argument_definitions`] is one: `held : Nat = ((p : Vouched) => 0)(proof)`, whose only recorded position naming `reaches` is the argument — `held`'s type and body sit at `Nat`, which the classification records nothing for. The control is the same term with `Vouched/qed(())` in the argument's place, which names nothing partial and must stay accepted.
#[test]
fn a_proof_at_an_argument_position_reaching_a_partial_definition_is_refused() {
    let verdicts = fixture_verdicts(
        &proof_at_an_argument_definitions(true),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        refuses_the_proof_reaching_the_lie(&verdicts),
        "a proof at an argument position reaching a partial definition was not refused: {verdicts:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &proof_at_an_argument_definitions(false),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "the same argument position holding a total proof was refused",
    );
}

/// A record is read only where it covers its unit. One naming `reaches` as `Total` and nothing else was not made by a walk over the library — `sink` is missing — so the unit is classified afresh and the forged entry admits nothing; a reader taking a record one name at a time would believe it and certify the proof. Mutation-checked: reading a record whatever its coverage — `Globals::mount` taking every record as covering — fails this test and no other in this file.
#[test]
fn a_record_that_does_not_cover_its_unit_is_not_read() {
    let forged = Certification::of(
        [(
            reaches(),
            Certified {
                totality: Totality::Total,
                ..Certified::default()
            },
        )],
        [],
        [],
        [],
    );
    assert!(!forged.covers(&stamp_trial_definitions(Totality::Total, false)));

    let carried = carried_beneath_the_lie(&forged);

    assert!(
        refuses_the_proof_reaching_the_lie(&carried),
        "a record not covering its unit must not be read, and the proof must be refused: {carried:?}",
    );
}

/// A record covering its unit, with every name it holds classified `Total`, and `extra` beside them: `held`, which the unit does not declare and the judged module does.
fn covering_the_library(extra: bool) -> Certification {
    let believed = |name: Global| {
        (
            name,
            Certified {
                totality: Totality::Total,
                ..Certified::default()
            },
        )
    };
    let mut entries = vec![
        believed(Global::Authored(Qualifier::from(["sink"]))),
        believed(reaches()),
    ];
    if extra {
        entries.push((
            Global::Authored(Qualifier::from(["held"])),
            Certified {
                totality: Totality::Partial,
                ..Certified::default()
            },
        ));
    }

    Certification::of(entries, [], [], [])
}

/// A record answers for the unit it was filed with and for no name beside it: an entry under a name the unit does not declare classifies nothing.
///
/// `Globals::partial` asks the record of each unit in scope for the name *and* asks that unit where it holds it, and `Certification::covers` ranges over the unit's names alone, so a record may carry an entry the unit answers for nowhere and still cover it. The record here covers the library and calls both its definitions total, so the proof beneath it is certified; the same record with `held` added as `Partial` — the judged module's own name, which the library does not declare — must certify it still. Read one name at a time, the entry would classify the judged module's `held` partial and have its own stamp refuse it, a verdict nothing in scope has any standing to raise.
///
/// Mutation-checked: with `Globals::partial` not asking where the unit holds the name, the proof is refused as `NotTotal` reaching nothing, and no other test in this file fails.
#[test]
fn a_record_answers_for_no_name_its_unit_does_not_declare() {
    let library = stamp_trial_definitions(Totality::Total, false);
    assert!(covering_the_library(false).covers(&library));
    assert!(
        covering_the_library(true).covers(&library),
        "an entry beside the unit's own names leaves the record covering it",
    );

    assert_eq!(
        carried_beneath_the_lie(&covering_the_library(false)),
        Vec::new(),
        "a record covering its unit and calling it total is believed",
    );
    assert_eq!(
        carried_beneath_the_lie(&covering_the_library(true)),
        Vec::new(),
        "an entry under a name the unit does not declare classified the judged module's own",
    );
}

/// A proof reaching two partial definitions is refused by the one written first, whichever spells first, as the elaborator refuses a type reaching two.
#[test]
fn a_proof_reaching_two_partial_definitions_names_the_first_written() {
    for (first, second) in [("zeta", "alpha"), ("alpha", "zeta")] {
        let verdicts = fixture_verdicts(
            &two_sinks_definitions(first, second),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );

        assert!(
            verdicts.iter().any(|verdict| matches!(
                &verdict.error,
                Error::NotTotal {
                    erased: Erased::Proof,
                    reached: Some(name),
                } if *name == Global::Authored(Qualifier::from([first]))
            )),
            "a proof reaching `{first}` and then `{second}` was not refused by `{first}`: {verdicts:?}",
        );
    }
}
