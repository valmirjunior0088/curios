//! Which parts of a definition elaborated again answer as its baseline's.

use {
    crate::{Question, Same},
    curios_core::{
        Definition, DefinitionKind, Global, Intrinsic, Item, Nat, Term, Totality, UniverseContext,
    },
    curios_utilities::Qualifier,
};

fn name() -> Global {
    Global::Authored(Qualifier::from(["stated"]))
}

/// `stated : Nat = <body>`, marked a proof where `proof`: the parts compared are read off the record, whatever a checker would make of the mark.
fn stated(body: usize, proof: bool) -> Item {
    Item::Let(Definition {
        name: name(),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::Total,
        proof,
        variances: Vec::new(),
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(body))),
    })
}

/// Whether what `made` unfolds to is what `held` does, and whether its signature is.
fn unfolds_alike(made: &Item, held: &Item) -> (bool, bool) {
    let same = Same::of(made, held, 0);

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
