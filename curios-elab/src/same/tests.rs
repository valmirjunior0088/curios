//! Which parts of a definition elaborated again answer as its baseline's.

use {
    crate::*,
    curios_common::Qualifier,
    curios_core::{
        Definition, DefinitionKind, Global, Intrinsic, Item, Level, Nat, Place, Sharing, Term,
        Totality, UniverseConstraint, UniverseContext, UniverseParam,
    },
};

fn name() -> Global {
    Global::Authored(Qualifier::from(["stated"]))
}

/// `stated : Nat = <body>`, marked a proof where `proof`: the parts compared are read off the record, whatever a checker would make of the mark.
fn stated(body: usize, proof: bool) -> Item {
    Item::Let(Box::new(Definition {
        name: name(),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        proof,
        value_sort: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(body))),
    }))
}

/// What elaboration concludes of `stated`, which reaches nothing partial and has no universe parameter.
fn total() -> Classified {
    Classified {
        totality: Totality::Total,
        ..Classified::default()
    }
}

/// Whether what `made` unfolds to is what `held` does, and whether its signature is, both concluded total.
fn unfolds_alike(made: &Item, held: &Item) -> (bool, bool) {
    let same = Same::of((made, &total()), (held, &total()), 0);

    (
        same.answers(&Question::Body(name())),
        same.answers(&Question::Signature(name())),
    )
}

/// A proof unfolds to nothing whatever its body, so one proved another way answers a question about what it unfolds to as its baseline did, where a definition that is no proof answers it another way.
#[test]
fn a_proof_proved_another_way_unfolds_to_what_it_did() {
    assert_eq!(
        unfolds_alike(&stated(1, true), &stated(2, true)),
        (true, true)
    );
    assert_eq!(
        unfolds_alike(&stated(1, false), &stated(2, false)),
        (false, true)
    );
}

/// What a name unfolds to moves with its mark though no term does: a definition that starts or stops being a proof answers the question another way, at the signature it had.
#[test]
fn a_definition_whose_mark_moved_unfolds_to_something_else() {
    assert_eq!(
        unfolds_alike(&stated(1, true), &stated(1, false)),
        (false, true)
    );
    assert_eq!(
        unfolds_alike(&stated(1, false), &stated(1, true)),
        (false, true)
    );
}

/// `stated(1, false)` under a scheme of one constraint, `0 ≤ upper`.
fn constrained(upper: usize) -> Item {
    Item::Let(Box::new(Definition {
        universe_context: UniverseContext {
            parameter_count: 3,
            quality_count: 0,
            constraints: vec![UniverseConstraint {
                lower: Level::param(UniverseParam(0)),
                upper: Level::param(UniverseParam(upper)),
            }],
        },
        ..match stated(1, false) {
            Item::Let(definition) => *definition,
            Item::Rec(_) => unreachable!("a definition of its own"),
        }
    }))
}

/// A signature digests as it is compared: the definition elaborated now and the one restored from a store digest alike, and one with another inequality digests otherwise and compares otherwise.
#[test]
fn a_signature_digests_as_it_is_compared() {
    let made = constrained(1);
    let further = constrained(2);
    let digest = |item: &Item| {
        item_part_digest(item, Place::Let(0), &total(), &Question::Signature(name()))
            .expect("a signature digests")
    };
    let alike = |made: &Item, held: &Item| unfolds_alike(made, held).1;

    assert_eq!(digest(&made), digest(&made.shared(&Sharing::new())));
    assert_ne!(digest(&made), digest(&further));
    assert!(!alike(&made, &further));
}
