//! What typing remembers: a node a term shares typed once, an answer kept no longer than the equations and binders it was read under, and a refused walk's typings taken back with the positions it recorded.

use {
    crate::certifier::infer::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{Global, Intrinsic, Nat, Term, UniverseContext, Universes},
};

/// A local-free term's type is remembered for the declaration, so a term whose tree is exponential in its depth — a sum adding a shared subterm to itself sixty times over, the shape a reduct takes when each step mentions the one before it more than once — is typed in the size of its graph. At a depth the uncached certifier can afford, both give the same type.
#[test]
fn a_shared_closed_term_is_typed_once_per_node() {
    let doubled = |depth: usize| {
        let mut term = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
        for _ in 0..depth {
            term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
        }
        term
    };

    assert!(infer(&mut certifier(), &doubled(60)).is_ok());

    let mut uncached = Certifier::uncached(100_000, SYNTAX);
    assert_eq!(
        infer(&mut certifier(), &doubled(8)),
        infer(&mut uncached, &doubled(8))
    );
}

/// Nor does a hit replay the identities the inference it remembers minted. Each level here applies a lambda, whose binder typing opens, to a sum sharing the level below twice; replayed per hit, those identities double with every level as a memo-free certifier's would, and forty levels are past the identity space. A hit opens nothing, so the graph mints what typing each of its nodes once does, read off the identities minted on either side.
#[test]
fn a_shared_closed_term_mints_once_per_node() {
    let x = binder(0, "x");
    let identity = Term::func([(x, nat_type())], Term::free_var(&x));
    let mut term = nat(1);
    for _ in 0..40 {
        term = Term::apply(
            identity.clone(),
            [Term::intrinsic(Intrinsic::nat_add(term.clone(), term))],
        );
    }

    let mut certifier = certifier();
    let before = certifier.fresh(None).index();
    assert_eq!(infer(&mut certifier, &term), Ok(nat_type()));
    let after = certifier.fresh(None).index();
    let minted = after - before - 1;
    assert!(minted <= 40, "minted {minted}");
}

/// A pair of the level below with itself, `depth` deep, over `base`, beside the record of its type over `base_type`.
fn doubled_pair(base: Term, base_type: Term, depth: u32) -> (Term, Term) {
    (0..depth).fold((base, base_type), |(value, type_), level| {
        (
            Term::tuple([value.clone(), value]),
            Term::tuple_type([
                (binder(2 * level + 100, "first"), type_.clone()),
                (binder(2 * level + 101, "second"), type_),
            ]),
        )
    })
}

/// A term naming a local is typed once per node too, for as long as what its typing read of the scope stands: sixty levels of a pair over a binder are sixty-one nodes, and inferring it answers where a walk per path does not. At a depth the uncached certifier affords, both give the same type.
///
/// Mutation-checked: with no type read off the scope answered, the sixty-level inference runs the budget out.
#[test]
fn a_shared_term_over_a_local_is_typed_once_per_node() {
    let n = binder(0, "n");
    let over = |mut certifier: Certifier<'static>| {
        certifier.assume(&n, &nat_type());
        certifier
    };
    let pair = |depth| doubled_pair(Term::free_var(&n), nat_type(), depth).0;

    assert!(infer(&mut over(certifier()), &pair(60)).is_ok());
    assert_eq!(
        infer(&mut over(certifier()), &pair(8)),
        infer(&mut over(Certifier::uncached(100_000, SYNTAX)), &pair(8)),
    );
}

/// And under an arm, where no type was remembered at all: the same pair is typed once per node while the arm's equation stands.
#[test]
fn a_shared_term_under_an_arm_is_typed_once_per_node() {
    let n = binder(0, "n");
    let m = binder(1, "m");
    let mut certifier = certifier();
    certifier.assume(&n, &nat_type());
    certifier.assume(&m, &nat_type());

    let inferred = certifier.scoped(|certifier| {
        certifier
            .refine(Term::free_var(&m), nat(0))
            .expect("the equation records");
        infer(
            certifier,
            &doubled_pair(Term::free_var(&n), nat_type(), 60).0,
        )
    });

    assert!(inferred.is_ok());
}

/// A tuple checked against a record reaches no inference — the Σ rule checks each field at its entry's type — so the check itself is remembered: a pair of the level below with itself, sixty deep, checks against the record of its type in the size of the two graphs.
///
/// Mutation-checked: with no check read off the scope answered, it runs the budget out.
#[test]
fn a_shared_tuple_is_checked_once_per_node() {
    let n = binder(0, "n");
    let mut certifier = certifier();
    certifier.assume(&n, &nat_type());
    let (value, type_) = doubled_pair(Term::free_var(&n), nat_type(), 60);

    assert_eq!(check(&mut certifier, &value, &type_), Ok(()));
}

/// A remembered type, and a remembered check, live as long as the equations they were taken under. `f(a)` is typed only where `a`'s type converts with `f`'s domain, which holds under `n = 0` and nowhere else: refused before the arm, accepted inside it, refused after it — and the uncached certifier agrees on all three, inferring and checking.
///
/// Mutation-checked: with either scoped table left standing where an equation moves, its judgment accepts the term after the arm.
#[test]
fn a_remembered_type_does_not_outlive_the_equations_it_was_taken_under() {
    type Judge = fn(&mut Certifier, &Term) -> bool;
    let inferring: Judge = |certifier, term| infer(certifier, term).is_ok();
    let checking: Judge = |certifier, term| check(certifier, term, &nat_type()).is_ok();

    let sequence = |mut certifier: Certifier, judge: Judge| {
        let n = binder(0, "n");
        let family = binder(1, "P");
        let f = binder(2, "f");
        let a = binder(3, "a");
        let x = binder(4, "x");
        let at = |index: Term| Term::apply(Term::free_var(&family), [index]);

        certifier.assume(&n, &nat_type());
        certifier.assume(
            &family,
            &Term::func_type([(x, nat_type())], Term::type_ground()),
        );
        certifier.assume(
            &f,
            &Term::func_type([(x, at(Term::free_var(&n)))], nat_type()),
        );
        certifier.assume(&a, &at(nat(0)));
        let term = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);

        let before = judge(&mut certifier, &term);
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(Term::free_var(&n), nat(0))
                .expect("the equation records");
            judge(certifier, &term)
        });
        let after = judge(&mut certifier, &term);

        [before, inside, after]
    };

    for judge in [inferring, checking] {
        let cached = sequence(certifier(), judge);

        assert_eq!(cached, [false, true, false]);
        assert_eq!(
            cached,
            sequence(Certifier::uncached(100_000, SYNTAX), judge)
        );
    }
}

/// Nor its binder: a name assumed again at another type, once its first binder is closed, is typed at the second.
///
/// Mutation-checked: answering a scoped type without asking whether its binders stand types the second `h` at `Nat`.
#[test]
fn a_remembered_type_does_not_outlive_its_binder() {
    let mut certifier = certifier();
    let h = binder(0, "h");
    let term = Term::free_var(&h);

    let first = certifier.scoped(|certifier| {
        certifier.assume(&h, &nat_type());
        infer(certifier, &term)
    });
    let second = certifier.scoped(|certifier| {
        certifier.assume(&h, &bool_type());
        infer(certifier, &term)
    });

    assert_eq!(first, Ok(nat_type()));
    assert_eq!(second, Ok(bool_type()));
}

/// A walk refused inside [`Certifier::tried`] leaves no position behind: what it recorded before the refusal is taken back, so the judgment after it records what that judgment records alone. The same walk in an ordinary bracket leaves the positions it recorded, and the judgment's are counted after them.
#[test]
fn a_refused_walk_takes_back_the_positions_it_recorded() {
    let refused = |certifier: &mut Certifier| {
        check(certifier, &nat_type(), &Term::type_ground())?;
        check(certifier, &nat(0), &bool_type())
    };
    let judged = |certifier: &mut Certifier| {
        check(certifier, &bool_type(), &Term::type_ground()).expect("a type is typed");
        let (positions, failure) = certifier.take_checked();
        assert!(failure.is_none());

        positions.len()
    };

    let alone = judged(&mut certifier());

    let mut tried = certifier();
    assert!(tried.tried(refused).is_err());
    assert_eq!(judged(&mut tried), alone);

    let mut scoped = certifier();
    assert!(scoped.scoped(refused).is_err());
    assert!(judged(&mut scoped) > alone);
}

/// A term first typed inside a refused [`Certifier::tried`] walk is recorded where it is typed again, as fully as a term typed once. A remembered typing records the term's own position and none inside it, which its first typing recorded; the refusal takes those back, and the typing with them. The control types the term twice in an ordinary bracket, where nothing is taken back and the two typings record what one does.
///
/// Mutation-checked: with the typings kept past a refusal, the lambda typed again records no position.
#[test]
fn a_term_typed_in_a_refused_walk_is_recorded_when_typed_again() {
    let x = binder(0, "x");
    let term = Term::func([(x, nat_type())], Term::free_var(&x));
    let judged = |certifier: &mut Certifier| {
        infer(certifier, &term).expect("the lambda types");
        let (positions, failure) = certifier.take_checked();
        assert!(failure.is_none());

        positions.len()
    };

    let alone = judged(&mut certifier());
    assert!(alone > 0, "the lambda's domain is a type position");

    let mut tried = certifier();
    let refused = tried.tried(|certifier| {
        infer(certifier, &term)?;
        check(certifier, &nat(0), &bool_type())
    });
    assert!(refused.is_err());
    assert_eq!(judged(&mut tried), alone);

    let mut scoped = certifier();
    scoped
        .scoped(|certifier| infer(certifier, &term))
        .expect("the lambda types");
    assert_eq!(judged(&mut scoped), alone);
}

/// `alias : Type 0 = ()`, defined into `certifier` as a refused definition stands: a type to `infer_type`, which reads its declared sort, and none to `Sort::of`, which reduces it to a value.
fn defining_alias(certifier: &mut Certifier) -> Global {
    let alias = Global::Authored(Qualifier::from(["alias"]));
    certifier
        .define(
            &alias,
            &Term::type_ground(),
            &Term::tuple(Vec::<Term>::new()),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    alias
}

/// `((z : alias) => 0)(())`, which checks `()` at `alias`: a position whose classification fails, recorded before the check refuses.
fn checked_at(alias: Global, z: u32) -> Term {
    Term::apply(
        Term::func(
            [(binder(z, "z"), Term::instance(alias, Universes::default()))],
            nat(0),
        ),
        [Term::tuple(Vec::<Term>::new())],
    )
}

/// `match not(true) | false => 0 | true => arm`: an arm no reduction takes, typed and refused, and withdrawn with what it recorded. Its scrutinee names no local, so no equation is in force inside it, and what it classifies is remembered for the declaration.
fn never_taken(arm: Term) -> Term {
    let c = binder(0, "c");
    let not = Term::func(
        [(c, bool_type())],
        Term::bool_match(
            Term::free_var(&c),
            None,
            bool_type(),
            Term::intrinsic(Intrinsic::Bool(true)),
            Term::intrinsic(Intrinsic::Bool(false)),
        ),
    );

    Term::bool_match(
        Term::apply(not, [Term::intrinsic(Intrinsic::Bool(true))]),
        None,
        nat_type(),
        nat(0),
        arm,
    )
}

/// A classification that failed is asked again where its type is met next: a withdrawn arm checks a value at `alias` first, and the same check outside the match records the failure again, with the memos on as with them off.
///
/// Mutation-checked: with a failed classification remembered, the check outside the arm records nothing and raises nothing.
#[test]
fn a_classification_that_failed_in_a_withdrawn_arm_fails_again_outside_it() {
    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        let alias = defining_alias(&mut certifier);
        let term = Term::tuple([never_taken(checked_at(alias, 1)), checked_at(alias, 2)]);

        assert!(
            infer(&mut certifier, &term).is_err(),
            "nothing inhabits `alias`"
        );
        assert!(matches!(
            certifier.take_checked().1,
            Some(Error::Unclassified(_))
        ));
    }
}

/// And a failure only a withdrawn arm met is withdrawn with it: an arm no reduction takes is one that was not typed, so nothing it classified, or could not, is the declaration's.
#[test]
fn a_classification_that_failed_only_in_a_withdrawn_arm_is_withdrawn_with_it() {
    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        let alias = defining_alias(&mut certifier);

        assert_eq!(
            infer(&mut certifier, &never_taken(checked_at(alias, 1))),
            Ok(nat_type())
        );
        assert!(certifier.take_checked().1.is_none());
    }
}

/// A check remembered under an arm's binding answers only while the binders it was read under stand. Binding `b` to `true`, as the arm of a `match b` at `true` does, records no equation, and no local's type names `b`, so retracting the binding clears no table: `0` checks at `match b | false => Bool | true => Nat` under the binding, and after it, where that type is stuck, the same check is refused, with the memos on as with them off.
///
/// Mutation-checked: with a scoped check answered without asking whether its binders stand, `0` checks after the binding.
#[test]
fn a_check_remembered_under_an_arms_binding_does_not_outlive_it() {
    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        let b = binder(1, "b");
        certifier.assume(&b, &bool_type());
        let chosen = Term::bool_match(
            Term::free_var(&b),
            None,
            Term::type_ground(),
            bool_type(),
            nat_type(),
        );

        let inside = certifier.scoped(|certifier| {
            certifier.bind(
                &b,
                &bool_type(),
                &Term::intrinsic(Intrinsic::Bool(true)),
                false,
            );
            check(certifier, &nat(0), &chosen)
        });
        let after = check(&mut certifier, &nat(0), &chosen);

        assert_eq!(inside, Ok(()));
        assert!(matches!(after, Err(Error::Mismatch { .. })), "{after:?}");
    }
}
