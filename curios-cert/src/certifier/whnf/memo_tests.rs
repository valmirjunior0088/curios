//! The evaluation memos: what a hit costs, why none goes stale, and what a budget restore forgets.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        Global, Intrinsic, Level, Reducer, Term, UniverseContext, UniverseParam, Universes,
    },
};

/// A remembered reduct is the same answer the term would compute — including across a scope boundary, which a local-free key cannot observe.
#[test]
fn a_memoized_unfold_answers_the_same_across_scopes() {
    let mut certifier = certifier();
    let name = Global::Authored(Qualifier::from(["two"]));
    certifier
        .define(
            &name,
            &nat_type(),
            &Term::intrinsic(Intrinsic::nat_add(nat(1), nat(1))),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    let inside = certifier.scoped(|certifier| {
        let binder_ = binder(1, "x");
        certifier.assume(&binder_, &nat_type());

        certifier
            .reduce_forced(Term::instance(name, Universes::default()))
            .expect("reduces")
    });

    let outside = certifier
        .reduce_forced(Term::instance(name, Universes::default()))
        .expect("reduces");
    assert_eq!(inside, outside);
    assert_eq!(outside, nat(2));
}

/// A remembered reduct is served only to the spelling it carries to, in either order: a polymorphic declaration's bare occurrence stays its own normal form after its instance has unfolded, and its instance unfolds after the bare occurrence has been remembered stuck.
///
/// Asked in *one* certifier, which is what makes it about the memo rather than about the two rules. Every reference to a declaration is an `Instance` node, so the bare occurrence and the instance differ in the node's universe vector alone, and nothing but exact keying tells them apart: a door that erased the levels would hand `f@{3}` the stuck `f` it had remembered, or hand `f` a reduct of an instance nobody stated. `curios-elab`'s `polymorphic_definition_unfolds_only_through_an_explicit_universe_instance` asks the same of the reducer that does have such a door.
///
/// Mutation-checked: with `Memos::whnf` and its store keyed through `curios_core::project_erased_universes`, the first instance's reduct answers the bare occurrence and the second assertion fails.
#[test]
fn a_remembered_reduct_is_served_only_to_the_spelling_it_carries_to() {
    let polymorphic = UniverseContext {
        parameter_count: 1,
        ..Default::default()
    };
    let name = Global::Authored(Qualifier::from(["f"]));
    let define = |certifier: &mut Certifier| {
        certifier
            .define(
                &name,
                &nat_type(),
                &Term::type_at(Level::param(UniverseParam(0))),
                &polymorphic,
            )
            .expect("a name is declared once");
    };
    let bare = || Term::instance(name, Universes::default());
    let at = || Term::instance(name, vec![Level::constant(3)]);
    let reduce =
        |certifier: &mut Certifier, term: Term| certifier.reduce_forced(term).expect("reduces");

    let mut instance_first = certifier();
    define(&mut instance_first);
    assert_eq!(
        reduce(&mut instance_first, at()),
        Term::type_at(Level::constant(3))
    );
    assert_eq!(
        reduce(&mut instance_first, bare()),
        bare(),
        "a bare occurrence was served the reduct of an instance it does not state",
    );

    let mut bare_first = certifier();
    define(&mut bare_first);
    assert_eq!(reduce(&mut bare_first, bare()), bare());
    assert_eq!(
        reduce(&mut bare_first, at()),
        Term::type_at(Level::constant(3)),
        "an instance was served the stuck form of the bare occurrence",
    );
}

/// A name is written once: a second definition under it is refused and the first stands, so a remembered reduct is never about a definition the environment no longer holds.
#[test]
fn a_redefinition_is_refused() {
    let mut certifier = certifier();
    let name = Global::Authored(Qualifier::from(["n"]));

    certifier
        .define(&name, &nat_type(), &nat(1), &UniverseContext::default())
        .expect("a name is declared once");
    assert_eq!(
        certifier
            .reduce_forced(Term::instance(name, Universes::default()))
            .expect("reduces"),
        nat(1),
    );

    assert_eq!(
        certifier.define(&name, &nat_type(), &nat(2), &UniverseContext::default()),
        Err(Error::Redeclared(name)),
    );
    assert_eq!(
        certifier
            .reduce_forced(Term::instance(name, Universes::default()))
            .expect("reduces"),
        nat(1),
    );
}

/// A term-keyed memo hit spends nothing, so the same closed term reduced twice within one declaration costs its full price once and O(1) after.
///
/// Charging a hit the recorded cost of the computation it replaces would price what a memo-free evaluator would have spent rather than what this certifier did, and recorded costs compound — a subterm hit twice per level makes the charge exponential in a structure the memos evaluate linearly.
#[test]
fn a_repeated_reduction_within_one_declaration_is_free() {
    let mut certifier = certifier();
    let term = chain(64);

    let first = spent(&mut certifier, term.clone());
    let second = spent(&mut certifier, term);

    assert!(first > 1, "the first reduction is the one that pays");
    assert_eq!(second, 0);
}

/// A free hit is only deterministic because the tables it reads live exactly as long as the budget does. Restoring one discards the other, so what a declaration spends is decided by the declaration rather than by which declarations were walked before it.
#[test]
fn restoring_the_budget_forgets_the_term_keyed_memos() {
    let mut certifier = certifier();
    let term = chain(64);

    let first = spent(&mut certifier, term.clone());
    certifier.restore_budget();
    let after_boundary = spent(&mut certifier, term);

    assert_eq!(after_boundary, first);
}

/// What a declaration spends does not depend on what the declarations before it reduced. The first declaration here reduces a definition's body and then unfolds the definition by name, and the second unfolds it again: the second spends exactly what unfolding it costs a certifier that reduced nothing before it.
///
/// A table outliving its declaration would break it, whether its hits were free or charged at the price its first computation paid — which counts that computation's own free hits, so the declaration that unfolded a name first would decide what every later one is charged. The occurrence is local-bearing, so it takes the strategy's delta rather than the closed machine.
///
/// Mutation-checked: keeping any reduct table across [`Certifier::restore_budget`] fails it.
#[test]
fn what_a_declaration_spends_does_not_depend_on_what_was_reduced_before_it() {
    let name = Global::Authored(Qualifier::from(["chain"]));
    let occurrence = Term::instance(name, Universes::default());
    let defining = || {
        let mut certifier = certifier();
        certifier
            .define(&name, &nat_type(), &chain(64), &monomorphic())
            .expect("a name is declared once");
        certifier
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
        "the name costs what it costs a certifier that reduced nothing first"
    );
}

/// Memoization may only *reduce* what a judgment spends. That is what makes free hits monotone against an uncached certifier — no program it certifies stops certifying with the memos on — and it is the half of a bit-identical invariant this design keeps: a semantic refusal is budget-independent, so only an exhaustion point can move, and it can only move later.
///
/// The subject reduces the same closed term twice in *separate* calls, so the inequality is strict: the memoized certifier's second call is a table hit where the uncached certifier runs the machine again. Repetition inside one call would no longer separate them, because the machine's own run-scoped values are a memo both certifiers get.
#[test]
fn cached_spend_never_exceeds_uncached() {
    let repeated = chain(32);

    let mut cached = certifier();
    let mut uncached = Certifier::uncached(1_000_000, SYNTAX);

    let with_memos = spent(&mut cached, repeated.clone()) + spent(&mut cached, repeated.clone());
    let without = spent(&mut uncached, repeated.clone()) + spent(&mut uncached, repeated);

    assert!(with_memos < without, "{with_memos} against {without}");
}

/// A remembered reduct's hit mints nothing, as it spends nothing. Each level here adds the level below to itself, over a lambda applied to a local, whose reduction opens a binder to eta-contract the lambda; counted per hit, that identity doubles with every level as a memo-free certifier's would, and forty levels are past any fixed identity space. A hit opens nothing, so the sum mints what reducing each of its nodes once does, read off the identities minted on either side.
///
/// Mutation-checked: with every hit adding what its computation minted to the counter, the identity minted after the sum is past the identity space.
#[test]
fn a_remembered_reducts_hit_mints_nothing() {
    let n = binder(0, "n");
    let y = binder(1, "y");
    let added = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&y), Term::free_var(&n)));
    let mut term = Term::apply(Term::func([(y, nat_type())], added), [Term::free_var(&n)]);
    for _ in 0..40 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }

    let mut certifier = certifier();
    certifier.assume(&n, &nat_type());
    let before = certifier.fresh(None).index();
    certifier.reduce_forced(term).expect("reduces");
    let after = certifier.fresh(None).index();
    let minted = after - before - 1;
    assert!(minted <= 40, "minted {minted}");
}

/// What classifying `type_` costs `certifier`, read off what its declaration has consumed on either side.
fn spent_classifying(certifier: &mut Certifier, type_: &Term) -> u64 {
    let before = certifier.consumed().units();
    Sort::of(certifier, type_).expect("classifies");
    let after = certifier.consumed().units();

    after - before
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
    let alias = Global::Authored(Qualifier::from(["alias"]));
    let scoped = Term::instance(alias, Universes::default());

    for type_ in [closed, scoped] {
        let mut certifier = certifier();
        certifier
            .define(&alias, &Term::type_ground(), &nat_type(), &monomorphic())
            .expect("a name is declared once");

        let first = spent_classifying(&mut certifier, &type_);
        let second = spent_classifying(&mut certifier, &type_);
        certifier.restore_budget();
        let after_boundary = spent_classifying(&mut certifier, &type_);

        assert!(first > 0, "classifying it reduces it");
        assert_eq!(second, 0);
        assert_eq!(after_boundary, first);
    }
}

type Judge = fn(&mut Certifier, &Term);

/// What `judge` costs `certifier` on `term`, read off what its declaration has consumed on either side.
fn spent_judging(certifier: &mut Certifier, judge: Judge, term: &Term) -> u64 {
    let before = certifier.consumed().units();
    judge(certifier, term);
    let after = certifier.consumed().units();

    after - before
}

/// A remembered type and a remembered check live as long as the budget does too, under both lives.
///
/// Mutation-checked: with `Memos::begin_declaration` leaving either table alone, the closed term after the boundary is judged for less than it costs.
#[test]
fn restoring_the_budget_forgets_a_remembered_type_and_a_remembered_check() {
    let inferring: Judge = |certifier, term| {
        infer(certifier, term).expect("is typed");
    };
    let checking: Judge =
        |certifier, term| check(certifier, term, &Term::type_ground()).expect("checks");

    let x = binder(0, "x");
    let closed = Term::apply(
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
        [nat_type()],
    );
    let alias = Global::Authored(Qualifier::from(["alias"]));
    let scoped = Term::instance(alias, Universes::default());

    for judge in [inferring, checking] {
        for term in [&closed, &scoped] {
            let mut certifier = certifier();
            certifier
                .define(&alias, &Term::type_ground(), &nat_type(), &monomorphic())
                .expect("a name is declared once");

            let first = spent_judging(&mut certifier, judge, term);
            let second = spent_judging(&mut certifier, judge, term);
            certifier.restore_budget();
            let after_boundary = spent_judging(&mut certifier, judge, term);

            assert!(first > 0, "judging it spends");
            assert_eq!(second, 0);
            assert_eq!(after_boundary, first);
        }
    }
}
