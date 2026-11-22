//! Totality verdicts across walks: the certifier's record read where a unit is carried, and a unit with none classified by the walk that reads it.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::{Erased, test_support::*},
    curios_common::{Qualifier, fill},
    curios_core::{Certification, Certified, Definitions, Global, Totality},
};

/// `Vouched`, a proposition of one case holding a unit, which is not defined.
const VOUCHED: &str = r#"
induct /Vouched: pub Prop
| qed(u: {})
end
"#;

/// `sink`, which does not terminate, and `reaches : (Nat) -> Nat`, which calls it: a library one of whose definitions reaches a partial one.
const LIBRARY: &str = r#"
let /sink: (n: #NatType) -> #NatType =
    #{node = Rec}
    (let x: (n: #NatType) -> #NatType = (n2: #NatType) => x(n2);
    x);

let /reaches: (m: #NatType) -> #NatType =
    (m: #NatType) => /sink(m);
"#;

/// `held : Vouched`, a proof applying a lambda to `reaches`, which it reads nothing of.
const HELD: &str = r#"
let /held: #{node = InductType} /Vouched() =
    ((g: (n: #NatType) -> #NatType) => #{node = Variant} /Vouched/qed(()))(/reaches);
"#;

/// [`LIBRARY`], with [`VOUCHED`] and the proof [`HELD`] beside it where `held` is.
fn reaching_library(held: bool) -> Definitions {
    match held {
        true => core_text_module(&fill!(
            "${VOUCHED}${LIBRARY}${HELD}",
            VOUCHED,
            LIBRARY,
            HELD
        )),
        false => core_text_module(LIBRARY),
    }
}

/// The proof [`HELD`], carried over a library it reads `reaches` of.
fn carried_proof_definitions() -> Definitions {
    core_text_module(&fill!("${VOUCHED}${HELD}", VOUCHED, HELD))
}

/// Partiality is a classification: a definition reaching a partial one is accepted, and a rule refusing every mention of a partial definition would break the language.
#[test]
fn a_definition_reaching_a_partial_one_is_accepted() {
    assert_eq!(
        text_refusals(LIBRARY),
        Vec::new(),
        "a definition reaching a partial one was refused",
    );
}

/// The definition a proof reaches partiality through.
fn reaches() -> Global {
    Global::Authored(Qualifier::from(["reaches"]))
}

/// Whether `verdicts` refuse `held : Vouched` for reaching `reaches` — the refusal a walk owes the proof whenever it knows `reaches` is partial.
fn refuses_the_proof_reaching_partiality(verdicts: &[Refusal]) -> bool {
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
fn carried_beneath(judged: &Definitions, certification: &Certification) -> Vec<Refusal> {
    fixture_refusals(
        judged,
        1_000_000,
        &AlreadyJudged::with(&reaching_library(false), certification).globals(),
        SYNTAX,
    )
}

/// A later walk reads a carried unit's totality from the certifier's record: the library certified alone leaves `reaches` partial in its record, and the proof reaching it, judged beneath that record, is refused exactly as it is judged fresh in one module with the library.
#[test]
fn a_carried_units_totality_is_read_from_the_certifiers_record() {
    let library = text_certified(LIBRARY);
    assert_eq!(library.refusals(), Vec::new(), "the library is certified");
    assert_eq!(
        library.certification().totality(&reaches()),
        Some(Totality::Partial)
    );

    let carried = carried_beneath(&carried_proof_definitions(), library.certification());
    assert!(
        refuses_the_proof_reaching_partiality(&carried),
        "carried beneath its record, the proof reaching a partial definition must be refused: {carried:?}",
    );

    let judged = fixture_refusals(&reaching_library(true), BUDGET, &Globals::default(), SYNTAX);
    assert!(
        refuses_the_proof_reaching_partiality(&judged),
        "judged fresh, the same proof must be refused for reaching a partial definition: {judged:?}",
    );
}

/// Proof totality fires at an argument position, where the corpus never reaches it.
///
/// [The soundness board](../../../documentation/design/soundness/the-soundness-board.md)'s *Rejected* paragraph says the corpus "passes proof totality inert at argument positions", so a fixture reaching it has to be constructed. This is one: `held : Nat = ((p : Vouched) => 0)(proof)`, whose only recorded position naming `reaches` is the argument — `held`'s type and body sit at `Nat`, which the classification records nothing for. The control is the same term with `Vouched/qed(())` in the argument's place, which names nothing partial and must stay accepted.
#[test]
fn a_proof_at_an_argument_position_reaching_a_partial_definition_is_refused() {
    let at_an_argument = |proof: &str| {
        text_refusals(&fill!(
            "${VOUCHED}${LIBRARY}\nlet /held: #NatType =\n    ((p: #{node = InductType} /Vouched()) => 0)(${proof});",
            VOUCHED,
            LIBRARY,
            proof
        ))
    };

    let verdicts = at_an_argument(
        "((g: (n: #NatType) -> #NatType) => #{node = Variant} /Vouched/qed(()))(/reaches)",
    );
    assert!(
        refuses_the_proof_reaching_partiality(&verdicts),
        "a proof at an argument position reaching a partial definition was not refused: {verdicts:?}",
    );
    assert_eq!(
        at_an_argument("#{node = Variant} /Vouched/qed(())"),
        Vec::new(),
        "the same argument position holding a total proof was refused",
    );
}

/// A record answering for every name the library holds, each classified `Total` though `sink` loops, and `extra` beside them: `held`, which the unit does not declare and the judged module does. Bytes no walk wrote, which a test mounts and nothing else does.
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
    core_text_module(&fill!(
        "${VOUCHED}${HELD}\nlet /again: #{node = InductType} /Vouched() = /held;",
        VOUCHED,
        HELD
    ))
}

/// A record answers for the unit it was filed with and for no name beside it: an entry under a name the unit does not declare classifies nothing.
///
/// `Globals::partial` asks the record of the unit in scope that holds the name, and no other, so a record may carry an entry its unit answers for nowhere and classify nothing by it. The record here answers for the library and calls both its definitions total, so the proof beneath it and `again`, which reads the proof, are certified; the same record with `held` added as `Partial` — the judged module's own name, which the library does not declare — must certify them still. Read off any record holding the name, the entry would classify the judged module's `held` partial and refuse `again` for reaching it, a verdict nothing in scope has any standing to raise.
///
/// Mutation-checked: with `Globals::partial` not asking which unit holds the name, `again` is refused as `NotTotal` reaching `held`, and no other test in this file fails.
#[test]
fn a_record_answers_for_no_name_its_unit_does_not_declare() {
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
        let verdicts = text_refusals(&fill!(
            r#"${VOUCHED}
let /${first}: (n: #NatType) -> #NatType =
    #{node = Rec}
    (let x: (n: #NatType) -> #NatType = (n2: #NatType) => x(n2);
    x);

let /${second}: (n: #NatType) -> #NatType =
    #{node = Rec}
    (let x: (n: #NatType) -> #NatType = (n2: #NatType) => x(n2);
    x);

let /held: #{node = InductType} /Vouched() =
    ((g: (n: #NatType) -> #NatType, h: (n: #NatType) -> #NatType) =>
        #{node = Variant} /Vouched/qed(()))(/${first}, /${second});
"#,
            VOUCHED,
            first,
            second
        ));

        assert!(
            refuses(&verdicts, Some("held"), |error| matches!(
                error,
                Error::NotTotal {
                    erased: Erased::Proof,
                    reached: Some(name),
                } if *name == global(first)
            )),
            "a proof reaching `{first}` and then `{second}` was not refused by `{first}`: {verdicts:?}",
        );
    }
}
