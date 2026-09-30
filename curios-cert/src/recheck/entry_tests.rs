//! The entrypoint: judged at the type it states, and refused when it states none.

use {
    crate::{Globals, KernelError},
    curios_analysis::fixture::SYNTAX,
    curios_core::{Entrypoint, Intrinsic, Module, Nat, Program, Term},
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

/// A program of nothing but `entry`.
fn entry_module(entry: Entrypoint) -> Program {
    Program {
        module: Module {
            mounts: Vec::new(),
            items: Vec::new(),
            universe_seeds: Vec::new(),
            induct_decls: BTreeMap::new(),
            struct_decls: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        entry,
    }
}

fn zero() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))
}

/// An entry stating no type is refused rather than inferred: elaboration writes the type it judged the body at, so an entry without one did not come through it, and inferring one would accept a program against a contract nobody checked. Mutation-checked: letting an untyped entry through accepts it.
#[test]
fn an_entry_stating_no_type_is_refused() {
    let verdicts = fixture_verdicts(
        &entry_module(Entrypoint {
            body: zero(),
            type_: None,
        }),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name.is_none()
                && matches!(verdict.error, KernelError::UntypedEntry)),
        "an entry with no type was certified: {verdicts:?}",
    );
}

/// An entry is checked against the type it states — the program contract a compile supplies, or an embedder's own — so a body that does not inhabit it is refused, where it used to be checked only when an author wrote the type. The control states the body's own type.
#[test]
fn an_entry_is_checked_against_the_type_it_states() {
    let stated = |type_: Intrinsic| {
        fixture_verdicts(
            &entry_module(Entrypoint {
                body: zero(),
                type_: Some(Term::intrinsic(type_)),
            }),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        )
    };

    assert!(
        stated(Intrinsic::BoolType)
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Mismatch { .. })),
        "a `Nat` entry stated at `Bool` was certified",
    );
    assert_eq!(stated(Intrinsic::NatType), Vec::new());
}
