//! The evaluation memos: what a hit costs, what clears them, and what a budget restore forgets.

use {
    crate::Kernel,
    curios_core::{Cost, Intrinsic, Reducer, Term, UniverseContext},
};

use super::test_support::*;

/// A remembered reduct is the same answer the term would compute — including across a scope boundary, which a local-free key cannot observe.
#[test]
fn a_memoized_unfold_answers_the_same_across_scopes() {
    let mut kernel = kernel();
    let name = binder(0, "two");
    kernel.define(
        &name,
        &nat_type(),
        &Term::intrinsic(Intrinsic::nat_add(nat(1), nat(1))),
        &UniverseContext::default(),
    );

    let inside = kernel.scoped(|kernel| {
        let binder_ = binder(1, "x");
        kernel.assume(&binder_, &nat_type());

        kernel
            .reduce_forced(Term::free_var(&name))
            .expect("reduces")
    });

    let outside = kernel
        .reduce_forced(Term::free_var(&name))
        .expect("reduces");
    assert_eq!(inside, outside);
    assert_eq!(outside, nat(2));
}

/// Redefining a name clears every memo, so validity is by construction rather than by an append-only assumption.
#[test]
fn a_redefinition_clears_the_memos() {
    let mut kernel = kernel();
    let name = binder(0, "n");

    kernel.define(&name, &nat_type(), &nat(1), &UniverseContext::default());
    assert_eq!(
        kernel
            .reduce_forced(Term::free_var(&name))
            .expect("reduces"),
        nat(1),
    );

    kernel.define(&name, &nat_type(), &nat(2), &UniverseContext::default());
    assert_eq!(
        kernel
            .reduce_forced(Term::free_var(&name))
            .expect("reduces"),
        nat(2),
    );
}

/// A term-keyed memo hit spends nothing, so the same closed term reduced twice within one declaration costs its full price once and O(1) after.
///
/// This is what a hit charging the recorded cost of the computation it replaces got wrong. That charge prices what a memo-free evaluator would have spent rather than what this kernel did, and recorded costs compound — a subterm hit twice per level makes the charge exponential in a structure the memos evaluate linearly — which is how a 262 144-step budget came to be declared exhausted after 6 547 actual reduction steps.
#[test]
fn a_repeated_reduction_within_one_declaration_is_free() {
    let mut kernel = kernel();
    let term = chain(64);

    let first = spent(&mut kernel, term.clone());
    let second = spent(&mut kernel, term);

    assert!(first > 1, "the first reduction is the one that pays");
    assert_eq!(second, 0);
}

/// A free hit is only deterministic because the tables it reads live exactly as long as the budget does. Restoring one discards the other, so what a declaration spends is decided by the declaration rather than by which declarations were walked before it.
#[test]
fn restoring_the_budget_forgets_the_term_keyed_memos() {
    let mut kernel = kernel();
    let term = chain(64);

    let first = spent(&mut kernel, term.clone());
    kernel.restore_budget();
    let after_boundary = spent(&mut kernel, term);

    assert_eq!(after_boundary, first);
}

/// The name-keyed table is the one that is *not* cleared at a declaration boundary, and it stays charged for exactly that reason: an entry outliving a declaration may not also be free, or which declarations came first would decide what this one can afford.
///
/// So a hit costs what computing the body cost, and the second occurrence spends what the first did to within the peak-depth rule — a [`Cost::FRAME`] either way, since the boundary between them resets the peak — and the warmth of any term the first call remembered and the boundary did not clear. The equation is stated as the two bounds that survive either evaluator: the hit is charged the bulk of what it replaces, and the two never differ by more than a frame plus that warmth.
///
/// That near-equality is also why the table's *survival* cannot be asserted here: a charged hit and a recomputation are nearly the same number by construction, and only the wall clock separates them.
///
/// The two occurrences sit on either side of a declaration boundary, because that is where the unfold table is the only one left: within a declaration the occurrence itself is remembered, local-bearing or not, and the second look would be a free hit on *that* rather than a charged one on the name.
#[test]
fn an_unfold_hit_is_charged_what_it_replaces() {
    let mut kernel = kernel();
    let name = binder(0, "chain");
    kernel.define(&name, &nat_type(), &chain(64), &monomorphic());
    let occurrence = Term::free_var(&name);

    let first = spent(&mut kernel, occurrence.clone());
    kernel.restore_budget();
    let second = spent(&mut kernel, occurrence);

    assert!(first > 1, "computing the body is what the first call pays");
    assert!(
        second > first / 2,
        "the hit is charged what it replaces: {second} against {first}"
    );
    assert!(
        first.abs_diff(second) <= Cost::FRAME.get() + 64,
        "the two differ by at most a peak frame plus follow-on warmth: {second} against {first}"
    );
}

/// Memoization may only *reduce* what a judgment spends. That is what makes free hits monotone against the kernel that shipped before them — no program that certified then can stop certifying now — and it is the half of the old bit-identical invariant this design keeps: a semantic refusal is budget-independent, so only an exhaustion point can move, and it can only move later.
///
/// The subject reduces the same closed term twice in *separate* calls, so the inequality is strict: the memoized kernel's second call is a table hit where the uncached kernel runs the machine again. Repetition inside one call would no longer separate them, because the machine's own run-scoped values are a memo both kernels get.
#[test]
fn cached_spend_never_exceeds_uncached() {
    let repeated = chain(32);

    let mut cached = kernel();
    let mut uncached = Kernel::uncached(1_000_000, crate::SYNTAX);
    uncached.set_local_floor(1_000);

    let with_memos = spent(&mut cached, repeated.clone()) + spent(&mut cached, repeated.clone());
    let without = spent(&mut uncached, repeated.clone()) + spent(&mut uncached, repeated);

    assert!(with_memos < without, "{with_memos} against {without}");
}
