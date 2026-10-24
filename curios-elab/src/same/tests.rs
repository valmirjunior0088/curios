//! Which parts of a definition elaborated again answer as its baseline's.

use {
    crate::{Question, Same, item_part_digest},
    curios_core::{
        Anchor, Anchored, Definition, DefinitionKind, Extent, Global, Intrinsic, Item, Level, Nat,
        Place, Sharing, Term, Totality, UniverseConstraint, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseParam,
    },
    curios_utilities::Qualifier,
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
        totality: Totality::Total,
        proof,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(body))),
    }))
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

/// `stated(1, false)` under a scheme of one constraint, `0 ≤ upper`, raised as `origin` says.
fn constrained(upper: usize, origin: UniverseConstraintOrigin) -> Item {
    Item::Let(Box::new(Definition {
        universe_context: UniverseContext {
            parameter_count: 3,
            quality_count: 0,
            constraints: vec![UniverseConstraint {
                lower: Level::param(UniverseParam(0)),
                upper: Level::param(UniverseParam(upper)),
                origin,
            }],
        },
        ..match stated(1, false) {
            Item::Let(definition) => *definition,
            Item::Rec(_) => unreachable!("a definition of its own"),
        }
    }))
}

/// A signature digests as it is compared: where its scheme's constraint was raised, and why, is no part of either, so the definition elaborated now, the one restored from a store and one whose constraint another reason raised digest alike and compare alike, and one with another inequality does neither.
#[test]
fn a_signature_digests_as_it_is_compared() {
    let placed = constrained(
        1,
        UniverseConstraintOrigin {
            extent: Some(Extent {
                anchor: Anchor::of(Anchored::Entry),
                start: 0,
                end: 4,
                stamp: 0,
            }),
            ..UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity)
        },
    );
    let otherwise = constrained(
        1,
        UniverseConstraintOrigin {
            declaration: Some("/lib/elsewhere".into()),
            ..UniverseConstraintOrigin::new(UniverseConstraintKind::WrittenType)
        },
    );
    let further = constrained(
        2,
        UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    );
    let digest = |item: &Item| {
        item_part_digest(item, Place::Let(0), &Question::Signature(name()))
            .expect("a signature digests")
    };
    let alike = |made: &Item, held: &Item| unfolds_alike(made, held).1;

    assert_eq!(digest(&placed), digest(&placed.shared(&Sharing::new())));
    assert_eq!(digest(&placed), digest(&otherwise));
    assert!(alike(&placed, &otherwise));
    assert_ne!(digest(&placed), digest(&further));
    assert!(!alike(&placed, &further));
}
