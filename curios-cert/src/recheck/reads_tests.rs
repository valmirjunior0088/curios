//! What each judged definition's record says it read of other items.

use {
    super::test_support::*,
    crate::Globals,
    curios_analysis::fixture::SYNTAX,
    curios_core::{Free, Global, Intrinsic, Module, Nat, Reads, Term},
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

fn global(symbol: &str) -> Global {
    Global::Authored(Qualifier::from([symbol]))
}

/// `alias : Type = Nat`, `three : alias = 3`, `identity : (Nat) -> Nat = (x) => x`, `five : Nat = identity(5)`, and `four : Nat = 4`.
fn reading_module() -> Module {
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let x = Free::local(10, Some("x"));

    Module {
        mounts: Vec::new(),
        items: vec![
            authored(&global("alias"), Term::type_ground(), nat()),
            authored(
                &global("three"),
                Term::free_var(&Free::from(&global("alias"))),
                literal(3),
            ),
            authored(
                &global("identity"),
                Term::func_type([(x, nat())], nat()),
                Term::func([(x, nat())], Term::free_var(&x)),
            ),
            authored(
                &global("five"),
                nat(),
                Term::apply(
                    Term::free_var(&Free::from(&global("identity"))),
                    [literal(5)],
                ),
            ),
            authored(&global("four"), nat(), literal(4)),
        ],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A definition's record holds what judging it read, of the kind it read, and nothing another judgment did. `five` applies `identity`, which types the head — its signature — and unfolds nothing. `three`'s declared type is `alias`, which is typed as written, so `alias`'s signature is read, and checking `3` against it unfolds it, so its body is read too. `four` names nothing, and `alias` reads nothing of its own.
#[test]
fn a_definition_reads_the_signature_it_types_and_the_body_it_unfolds() {
    let rechecked = fixture_certified(&reading_module(), 1_000_000, &Globals::default(), SYNTAX);
    assert_eq!(rechecked.verdicts, Vec::new());
    let reads = |symbol: &str| {
        rechecked
            .certification
            .reads(&global(symbol))
            .cloned()
            .expect("the record covers every judged definition")
    };

    assert_eq!(
        reads("five"),
        Reads {
            signatures: BTreeSet::from([global("identity")]),
            bodies: BTreeSet::new(),
        },
    );
    assert_eq!(
        reads("three"),
        Reads {
            signatures: BTreeSet::from([global("alias")]),
            bodies: BTreeSet::from([global("alias")]),
        },
    );
    assert_eq!(reads("four"), Reads::default());
    assert_eq!(reads("alias"), Reads::default());
}
