//! The evaluation memos: what a hit costs, what clears them, and what a budget restore forgets.

use {
    super::test_support::*,
    crate::{Kernel, Sort, check, infer},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Intrinsic, Reducer, Term, UniverseContext},
};

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
/// Charging a hit the recorded cost of the computation it replaces would price what a memo-free evaluator would have spent rather than what this kernel did, and recorded costs compound — a subterm hit twice per level makes the charge exponential in a structure the memos evaluate linearly.
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

/// What a declaration spends does not depend on what the declarations before it reduced. The first declaration here reduces a definition's body and then unfolds the definition by name, and the second unfolds it again: the second spends exactly what unfolding it costs a kernel that reduced nothing before it.
///
/// A table outliving its declaration would break it, whether its hits were free or charged at the price its first computation paid — which counts that computation's own free hits, so the declaration that unfolded a name first would decide what every later one is charged. The occurrence is local-bearing, so it takes the strategy's delta rather than the closed machine.
///
/// Mutation-checked: keeping any reduct table across [`Kernel::restore_budget`] fails it.
#[test]
fn what_a_declaration_spends_does_not_depend_on_what_was_reduced_before_it() {
    let name = binder(0, "chain");
    let occurrence = Term::free_var(&name);
    let defining = || {
        let mut kernel = kernel();
        kernel.define(&name, &nat_type(), &chain(64), &monomorphic());
        kernel
    };

    let alone = spent(&mut defining(), occurrence.clone());

    let mut after = defining();
    spent(&mut after, chain(64));
    spent(&mut after, occurrence.clone());
    after.restore_budget();
    let again = spent(&mut after, occurrence);

    assert!(alone > 1, "unfolding the name reduces its body");
    assert_eq!(
        again, alone,
        "the name costs what it costs a kernel that reduced nothing first"
    );
}

/// Memoization may only *reduce* what a judgment spends. That is what makes free hits monotone against an uncached kernel — no program it certifies stops certifying with the memos on — and it is the half of a bit-identical invariant this design keeps: a semantic refusal is budget-independent, so only an exhaustion point can move, and it can only move later.
///
/// The subject reduces the same closed term twice in *separate* calls, so the inequality is strict: the memoized kernel's second call is a table hit where the uncached kernel runs the machine again. Repetition inside one call would no longer separate them, because the machine's own run-scoped values are a memo both kernels get.
#[test]
fn cached_spend_never_exceeds_uncached() {
    let repeated = chain(32);

    let mut cached = kernel();
    let mut uncached = Kernel::uncached(1_000_000, SYNTAX);

    let with_memos = spent(&mut cached, repeated.clone()) + spent(&mut cached, repeated.clone());
    let without = spent(&mut uncached, repeated.clone()) + spent(&mut uncached, repeated);

    assert!(with_memos < without, "{with_memos} against {without}");
}

/// What classifying `type_` costs `kernel`, read off the remaining budget on either side.
fn spent_classifying(kernel: &mut Kernel, type_: &Term) -> u64 {
    let (before, _) = kernel.consumption();
    Sort::of(kernel, type_).expect("classifies");
    let (after, _) = kernel.consumption();

    before - after
}

/// A remembered sort lives as long as the budget does, as a reduct does: classified twice within one declaration a type pays once, and across a restore it pays again, so a sort one declaration filed never spares the next the reduction it rests on. Both lives are held to it — a closed type's, and that of a type naming a local.
///
/// Mutation-checked: with `Memos::begin_declaration` leaving the sorts alone, the closed type after the boundary is answered for nothing.
#[test]
fn restoring_the_budget_forgets_a_remembered_sort() {
    let x = binder(0, "x");
    let closed = Term::apply(
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
        [nat_type()],
    );
    let alias = binder(1, "alias");
    let scoped = Term::free_var(&alias);

    for type_ in [closed, scoped] {
        let mut kernel = kernel();
        kernel.define(&alias, &Term::type_ground(), &nat_type(), &monomorphic());

        let first = spent_classifying(&mut kernel, &type_);
        let second = spent_classifying(&mut kernel, &type_);
        kernel.restore_budget();
        let after_boundary = spent_classifying(&mut kernel, &type_);

        assert!(first > 0, "classifying it reduces it");
        assert_eq!(second, 0);
        assert_eq!(after_boundary, first);
    }
}

type Judge = fn(&mut Kernel, &Term);

/// What `judge` costs `kernel` on `term`, read off the remaining budget on either side.
fn spent_judging(kernel: &mut Kernel, judge: Judge, term: &Term) -> u64 {
    let (before, _) = kernel.consumption();
    judge(kernel, term);
    let (after, _) = kernel.consumption();

    before - after
}

/// A remembered type and a remembered check live as long as the budget does too, under both lives.
///
/// Mutation-checked: with `Memos::begin_declaration` leaving either table alone, the closed term after the boundary is judged for less than it costs.
#[test]
fn restoring_the_budget_forgets_a_remembered_type_and_a_remembered_check() {
    let inferring: Judge = |kernel, term| {
        infer(kernel, term).expect("is typed");
    };
    let checking: Judge = |kernel, term| check(kernel, term, &Term::type_ground()).expect("checks");

    let x = binder(0, "x");
    let closed = Term::apply(
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
        [nat_type()],
    );
    let alias = binder(1, "alias");
    let scoped = Term::free_var(&alias);

    for judge in [inferring, checking] {
        for term in [&closed, &scoped] {
            let mut kernel = kernel();
            kernel.define(&alias, &Term::type_ground(), &nat_type(), &monomorphic());

            let first = spent_judging(&mut kernel, judge, term);
            let second = spent_judging(&mut kernel, judge, term);
            kernel.restore_budget();
            let after_boundary = spent_judging(&mut kernel, judge, term);

            assert!(first > 0, "judging it spends");
            assert_eq!(second, 0);
            assert_eq!(after_boundary, first);
        }
    }
}
