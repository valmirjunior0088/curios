//! What typing remembers: a node a term shares typed once, an answer kept no longer than the equations and binders it was read under, and a refused walk's typings taken back with the positions it recorded.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::Term,
};

/// `depth` levels of `${node}` over `base`, each level's operand written once and shared by its second occurrence under the share keys from `keys` on, the operand's two places written `${below}` and `${again}`: a graph whose tree is `2^depth`.
fn doubled(node: &str, base: &str, depth: usize, keys: usize) -> String {
    (keys..keys + depth).fold(base.to_string(), |operand, key| {
        fill!(
            node,
            below = fill!("#{share = ${key}} (${operand})", key, operand),
            again = fill!("#{shared = ${key}} _", key)
        )
    })
}

/// A local-free term's type is remembered for the declaration, so a term whose tree is exponential in its depth — a sum adding a shared subterm to itself sixty times over, the shape a reduct takes when each step mentions the one before it more than once — is typed in the size of its graph. At a depth the uncached certifier can afford, both give the same type.
#[test]
fn a_shared_closed_term_is_typed_once_per_node() {
    let sum = |depth| term(&doubled("#NatAdd(${below}, ${again})", "1", depth, 0));

    assert!(infer(&mut certifier(), &sum(60)).is_ok());

    let mut uncached = Certifier::uncached(100_000, SYNTAX);
    assert_eq!(
        infer(&mut certifier(), &sum(8)),
        infer(&mut uncached, &sum(8))
    );
}

/// Nor does a hit replay the identities the inference it remembers minted. Each level here applies a lambda, whose binder typing opens, to a sum sharing the level below twice; replayed per hit, those identities double with every level as a memo-free certifier's would, and forty levels are past the identity space. A hit opens nothing, so the graph mints what typing each of its nodes once does, read off the identities minted on either side.
#[test]
fn a_shared_closed_term_mints_once_per_node() {
    let applied = term(&doubled(
        "((x: #NatType) => x)(#NatAdd(${below}, ${again}))",
        "1",
        40,
        0,
    ));

    let mut certifier = certifier();
    let before = certifier.fresh(None).index();
    assert_eq!(infer(&mut certifier, &applied), Ok(term("#NatType")));
    let after = certifier.fresh(None).index();
    let minted = after - before - 1;
    assert!(minted <= 40, "minted {minted}");
}

/// A pair of the level below with itself, `depth` deep, over `n`, and the record of its type over `Nat`, as text.
fn doubled_pair(depth: usize) -> String {
    fill!(
        "${value}, ${type_}",
        value = doubled("(${below}, ${again})", "n", depth, 0),
        type_ = doubled(
            "{first: ${below}, second: ${again}}",
            "#NatType",
            depth,
            depth
        )
    )
}

/// A term naming a local is typed once per node too, for as long as what its typing read of the scope stands: sixty levels of a pair over a binder are sixty-one nodes, and inferring it answers where a walk per path does not. At a depth the uncached certifier affords, both give the same type.
///
/// Mutation-checked: with no type read off the scope answered, the sixty-level inference runs the budget out.
#[test]
fn a_shared_term_over_a_local_is_typed_once_per_node() {
    let inferred = |mut certifier: Certifier<'static>, depth| {
        let [pair, _] = under(&mut certifier, "n: #NatType", &doubled_pair(depth));

        infer(&mut certifier, &pair)
    };

    assert!(inferred(certifier(), 60).is_ok());
    assert_eq!(
        inferred(certifier(), 8),
        inferred(Certifier::uncached(100_000, SYNTAX), 8),
    );
}

/// And under an arm, where no type was remembered at all: the same pair is typed once per node while the arm's equation stands.
#[test]
fn a_shared_term_under_an_arm_is_typed_once_per_node() {
    let mut certifier = certifier();
    let [m, pair, _] = under(
        &mut certifier,
        "n: #NatType, m: #NatType",
        &fill!("m, ${pair}", pair = doubled_pair(60)),
    );

    let inferred = certifier.scoped(|certifier| {
        certifier
            .refine(m, term("0"))
            .expect("the equation records");
        infer(certifier, &pair)
    });

    assert!(inferred.is_ok());
}

/// A tuple checked against a record reaches no inference — the Σ rule checks each field at its entry's type — so the check itself is remembered: a pair of the level below with itself, sixty deep, checks against the record of its type in the size of the two graphs.
///
/// Mutation-checked: with no check read off the scope answered, it runs the budget out.
#[test]
fn a_shared_tuple_is_checked_once_per_node() {
    let mut certifier = certifier();
    let [value, type_] = under(&mut certifier, "n: #NatType", &doubled_pair(60));

    assert_eq!(check(&mut certifier, &value, &type_), Ok(()));
}

/// A remembered type, and a remembered check, live as long as the equations they were taken under. `f(a)` is typed only where `a`'s type converts with `f`'s domain, which holds under `n = 0` and nowhere else: refused before the arm, accepted inside it, refused after it — and the uncached certifier agrees on all three, inferring and checking.
///
/// Mutation-checked: with either scoped table left standing where an equation moves, its judgment accepts the term after the arm.
#[test]
fn a_remembered_type_does_not_outlive_the_equations_it_was_taken_under() {
    type Judge = fn(&mut Certifier, &Term) -> bool;
    let inferring: Judge = |certifier, term| infer(certifier, term).is_ok();
    let checking: Judge = |certifier, term| check(certifier, term, &self::term("#NatType")).is_ok();

    let sequence = |mut certifier: Certifier, judge: Judge| {
        let [n, applied] = under(
            &mut certifier,
            "n: #NatType, P: (x: #NatType) -> Type, f: (x: P(n)) -> #NatType, a: P(0)",
            "n, f(a)",
        );

        let before = judge(&mut certifier, &applied);
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(n.clone(), term("0"))
                .expect("the equation records");
            judge(certifier, &applied)
        });
        let after = judge(&mut certifier, &applied);

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

    let ((locals, [h]), first) = certifier.scoped(|certifier| {
        let opened = hypotheses(certifier, "h: #NatType", "h,");
        let first = infer(certifier, &opened.1[0]);

        (opened, first)
    });
    let second = certifier.scoped(|certifier| {
        certifier.assume(&locals[0], &term("#BoolType"));
        infer(certifier, &h)
    });

    assert_eq!(first, Ok(term("#NatType")));
    assert_eq!(second, Ok(term("#BoolType")));
}

/// A walk refused inside [`Certifier::tried`] leaves no position behind: what it recorded before the refusal is taken back, so the judgment after it records what that judgment records alone. The same walk in an ordinary bracket leaves the positions it recorded, and the judgment's are counted after them.
#[test]
fn a_refused_walk_takes_back_the_positions_it_recorded() {
    let refused = |certifier: &mut Certifier| {
        check(certifier, &term("#NatType"), &term("Type"))?;
        check(certifier, &term("0"), &term("#BoolType"))
    };
    let judged = |certifier: &mut Certifier| {
        check(certifier, &term("#BoolType"), &term("Type")).expect("a type is typed");
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
    let lambda = term("(x: #NatType) => x");
    let judged = |certifier: &mut Certifier| {
        infer(certifier, &lambda).expect("the lambda types");
        let (positions, failure) = certifier.take_checked();
        assert!(failure.is_none());

        positions.len()
    };

    let alone = judged(&mut certifier());
    assert!(alone > 0, "the lambda's domain is a type position");

    let mut tried = certifier();
    let refused = tried.tried(|certifier| {
        infer(certifier, &lambda)?;
        check(certifier, &term("0"), &term("#BoolType"))
    });
    assert!(refused.is_err());
    assert_eq!(judged(&mut tried), alone);

    let mut scoped = certifier();
    scoped
        .scoped(|certifier| infer(certifier, &lambda))
        .expect("the lambda types");
    assert_eq!(judged(&mut scoped), alone);
}

/// `alias : Type 0 = ()`, as a refused definition stands: a type to `infer_type`, which reads its declared sort, and none to `Sort::of`, which reduces it to a value.
const ALIAS: &str = "let /alias: Type = ();";

/// `((z : alias) => 0)(())`, which checks `()` at `alias`: a position whose classification fails, recorded before the check refuses.
const CHECKED_AT_ALIAS: &str = "((z: /alias) => 0)(())";

/// `match not(true) | false => 0 | true => ${arm} end`: an arm no reduction takes, typed and refused, and withdrawn with what it recorded. Its scrutinee names no local, so no equation is in force inside it, and what it classifies is remembered for the declaration.
const NEVER_TAKEN: &str = "match ((c: #BoolType) => match c: (_) => #BoolType | false => true | true => false end)(true): (_) => #NatType | false => 0 | true => ${arm} end";

/// A classification that failed is asked again where its type is met next: a withdrawn arm checks a value at `alias` first, and the same check outside the match records the failure again, with the memos on as with them off.
///
/// Mutation-checked: with a failed classification remembered, the check outside the arm records nothing and raises nothing.
#[test]
fn a_classification_that_failed_in_a_withdrawn_arm_fails_again_outside_it() {
    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        declared(&mut certifier, ALIAS);
        let both = term(&fill!(
            "(${withdrawn}, ${checked})",
            withdrawn = fill!(NEVER_TAKEN, arm = CHECKED_AT_ALIAS),
            checked = CHECKED_AT_ALIAS
        ));

        assert!(
            infer(&mut certifier, &both).is_err(),
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
        declared(&mut certifier, ALIAS);

        assert_eq!(
            infer(
                &mut certifier,
                &term(&fill!(NEVER_TAKEN, arm = CHECKED_AT_ALIAS))
            ),
            Ok(term("#NatType"))
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
        let (locals, [chosen]) = hypotheses(
            &mut certifier,
            "b: #BoolType",
            "match b: (_) => Type | false => #BoolType | true => #NatType end,",
        );
        let zero = term("0");

        let inside = certifier.scoped(|certifier| {
            certifier.bind(&locals[0], &term("#BoolType"), &term("true"), false);
            check(certifier, &zero, &chosen)
        });
        let after = check(&mut certifier, &zero, &chosen);

        assert_eq!(inside, Ok(()));
        assert!(matches!(after, Err(Error::Mismatch { .. })), "{after:?}");
    }
}
