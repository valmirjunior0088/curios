//! A declaration's walk: the identities a restored budget opens binders at, and what a typing hit recalls.

use {
    super::test_support::*,
    curios_core::{Free, Intrinsic, Term},
};

/// A declaration's binders are opened at identities of its own: the counter the certifier mints them from starts over where the budget is restored, so how many binders the walk opened before a declaration decides nothing of it. The carriers' algebra ranks an atom by a hash that takes a local's identity in, and under a counter that ran on the certifier answered one term two ways by what it had judged before. A binder still open keeps its identity, and the counter starts over past it.
#[test]
fn a_restored_budget_starts_the_identities_over() {
    let mut certifier = certifier();
    certifier.restore_budget();
    let first = certifier.fresh(None);

    // What an earlier declaration opened.
    certifier.fresh(None);
    certifier.fresh(None);
    certifier.restore_budget();
    assert_eq!(certifier.fresh(None), first);

    let held = Free::local(first.index() + 7, Some("held"));
    certifier.assume(&held, &Term::intrinsic(Intrinsic::NatType));
    certifier.restore_budget();
    assert!(certifier.fresh(None).index() > held.index());
}

/// What a typing hit recalls lives as long as the typing it was filed with: a term typed under one arm, where its typing closed a group that does not descend, and typed again under another, where it closed none, recalls nothing once the first arm's entry is gone.
#[test]
fn a_typing_hit_recalls_what_its_own_typing_closed() {
    let mut certifier = certifier();
    let local = certifier.fresh(Some("x"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    certifier.assume(&local, &nat);
    let term = Term::free_var(&local);

    certifier.infer_store(term.clone(), nat.clone(), true);
    certifier.memos.begin_equations();
    certifier.infer_store(term.clone(), nat, false);

    assert_eq!(
        certifier.infer_hit(&term).map(|(_, closed)| closed),
        Some(false)
    );
}
