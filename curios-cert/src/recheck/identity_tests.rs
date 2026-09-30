//! Identities a walk is handed: a free local is refused before the kernel mints a binder that could alias it.

use {
    crate::{Globals, KernelError},
    curios_analysis::fixture::SYNTAX,
    curios_core::{Entrypoint, Free, Global, Intrinsic, Module, Nat, Program, Term},
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

/// The indices a stray local is tried at: every one the kernel mints first, so a walk that let the local through would open a binder carrying the same identity at one of them.
const COLLIDING: std::ops::Range<u32> = 0..4;

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// `(x : Nat) => stray`, the function a capture turns into the identity.
fn constant_over(stray: &Free) -> Term {
    Term::func(
        [(Free::local(9_000, Some("x")), nat())],
        Term::free_var(stray),
    )
}

fn held() -> Global {
    Global::Authored(Qualifier::from(["held"]))
}

/// A module holding `items` and nothing else.
fn holding(items: Vec<curios_core::Item>) -> Module {
    Module {
        items,
        mounts: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A definition mentioning a local no binder of its opens is refused, at every index the kernel could mint for the binder it opens.
///
/// The kernel's counter starts at zero, so the local is one it can hand out again: opening `x` at the stray's index turns `(x : Nat) => stray` into the identity, which inhabits `(x : Nat) -> Nat`, and the judgment — which then sees a bound variable — accepts. The refusal is taken at the boundary for that reason. Mutation-checked: dropping it certifies the definition at the colliding index.
#[test]
fn a_definition_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let function = Term::func_type([(Free::local(9_001, Some("x")), nat())], nat());
        let module = holding(vec![authored(&held(), function, constant_over(&stray))]);

        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(held())
                && matches!(&verdict.error, KernelError::Unbound(found) if *found == stray)),
            "a definition mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}

/// The entry is held to the same boundary as the module's items, since it is judged in the same walk by the same counter. Mutation-checked as above.
#[test]
fn an_entry_mentioning_a_free_local_is_refused() {
    for index in COLLIDING {
        let stray = Free::local(index, Some("stray"));
        let program = Program {
            module: holding(Vec::new()),
            entry: Entrypoint {
                body: Term::apply(
                    constant_over(&stray),
                    [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
                ),
                type_: Some(nat()),
            },
        };

        let verdicts = fixture_verdicts(&program, 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name.is_none()
                && matches!(&verdict.error, KernelError::Unbound(found) if *found == stray)),
            "an entry mentioning free local {index} was not refused for it: {verdicts:?}",
        );
    }
}
