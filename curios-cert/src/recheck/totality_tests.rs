//! Totality verdicts across walks: the certifier's record read where a unit is carried, and a unit with none classified by the walk that reads it.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::{Erased, test_support::*},
    curios_common::Qualifier,
    curios_core::{
        Certification, Certified, Definitions, Global, Term, Totality, Universes, test_support::*,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Partiality is a classification: a definition reaching a partial one is accepted, and a rule refusing every mention of a partial definition would break the language.
#[test]
fn a_definition_reaching_a_partial_one_is_accepted() {
    assert_eq!(
        fixture_verdicts(
            &reaching_library(false),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "a definition reaching a partial one was refused",
    );
}

/// The definition a proof reaches partiality through.
fn reaches() -> Global {
    Global::Authored(Qualifier::from(["reaches"]))
}

/// Whether `verdicts` refuse `held : Vouched` for reaching `reaches` — the refusal a walk owes the proof whenever it knows `reaches` is partial.
fn refuses_the_proof_reaching_partiality(verdicts: &[Verdict]) -> bool {
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

/// The walk over `judged`, the library mounted beneath it as the compile path mounts a unit — with `certification` as the record filed beside it.
fn carried_beneath(judged: &Definitions, certification: &Certification) -> Vec<Verdict> {
    fixture_verdicts(
        judged,
        1_000_000,
        &AlreadyJudged::with(&reaching_library(false), certification).globals(),
        SYNTAX,
    )
}

/// A later walk reads a carried unit's totality from the certifier's record: the library certified alone leaves `reaches` partial in its record, and the proof reaching it, judged beneath that record, is refused exactly as it is judged fresh in one module with the library.
#[test]
fn a_carried_units_totality_is_read_from_the_certifiers_record() {
    let library = fixture_certified(
        &reaching_library(false),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert_eq!(library.verdicts, Vec::new(), "the library is certified");
    assert_eq!(
        library.certification.totality(&reaches()),
        Some(Totality::Partial)
    );

    let carried = carried_beneath(&carried_proof_definitions(), &library.certification);
    assert!(
        refuses_the_proof_reaching_partiality(&carried),
        "carried beneath its record, the proof reaching a partial definition must be refused: {carried:?}",
    );

    let judged = fixture_verdicts(
        &reaching_library(true),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        refuses_the_proof_reaching_partiality(&judged),
        "judged fresh, the same proof must be refused for reaching a partial definition: {judged:?}",
    );
}

/// A unit mounted with an empty record — an environment built by hand, with no walk behind it — is classified by the walk that reads it, from its items: the library beneath the proof still has the proof refused.
#[test]
fn a_unit_mounted_with_an_empty_record_is_classified_by_the_reading_walk() {
    let carried = carried_beneath(&carried_proof_definitions(), &Certification::default());

    assert!(
        refuses_the_proof_reaching_partiality(&carried),
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
        refuses_the_proof_reaching_partiality(&verdicts),
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
    assert!(!forged.covers(&reaching_library(false)));

    let carried = carried_beneath(&carried_proof_definitions(), &forged);

    assert!(
        refuses_the_proof_reaching_partiality(&carried),
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

/// [`carried_proof_definitions`] with `again : Vouched = held` beside the proof: a reader of `held` at a proof position, which a walk taking `held` for partial refuses.
fn carried_proof_and_its_reader() -> Definitions {
    let (vouched_name, declaration) = vouched_declaration();
    let again = authored(
        &Global::Authored(Qualifier::from(["again"])),
        Term::induct_type(vouched_name, Vec::<Term>::new(), Vec::<Term>::new()),
        Term::instance(
            Global::Authored(Qualifier::from(["held"])),
            Universes::default(),
        ),
    );

    declaring(
        Definitions {
            mounts: Vec::new(),
            items: vec![held_proof(), again].into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        BTreeMap::from([(vouched_name, declaration)]),
        [],
        [],
    )
}

/// A record answers for the unit it was filed with and for no name beside it: an entry under a name the unit does not declare classifies nothing.
///
/// `Globals::partial` asks the record of each unit in scope for the name *and* asks that unit where it holds it, and `Certification::covers` ranges over the unit's names alone, so a record may carry an entry the unit answers for nowhere and still cover it. The record here covers the library and calls both its definitions total, so the proof beneath it and `again`, which reads the proof, are certified; the same record with `held` added as `Partial` — the judged module's own name, which the library does not declare — must certify them still. Read one name at a time, the entry would classify the judged module's `held` partial and refuse `again` for reaching it, a verdict nothing in scope has any standing to raise.
///
/// Mutation-checked: with `Globals::partial` not asking where the unit holds the name, `again` is refused as `NotTotal` reaching `held`, and no other test in this file fails.
#[test]
fn a_record_answers_for_no_name_its_unit_does_not_declare() {
    let library = reaching_library(false);
    assert!(covering_the_library(false).covers(&library));
    assert!(
        covering_the_library(true).covers(&library),
        "an entry beside the unit's own names leaves the record covering it",
    );

    assert_eq!(
        carried_beneath(
            &carried_proof_and_its_reader(),
            &covering_the_library(false)
        ),
        Vec::new(),
        "a record covering its unit and calling it total is believed",
    );
    assert_eq!(
        carried_beneath(&carried_proof_and_its_reader(), &covering_the_library(true)),
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
