//! What the certifier's reduction asks of its conversion: the guards and questions it answers through the shared analysis, asking nothing itself, and the reducts and verdicts it remembers by the reduction that reached them.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::{Env, test_support::*},
    curios_common::fill,
    curios_core::{Reducer, Term},
};

/// Assume `guard` holds, for the arm `inside` runs in.
fn guarded<T>(
    certifier: &mut Certifier,
    guard: &Term,
    inside: impl FnOnce(&mut Certifier) -> T,
) -> T {
    certifier.scoped(|certifier| {
        certifier
            .refine(guard.clone(), term("true"))
            .expect("the equation records");
        inside(certifier)
    })
}

/// A guard answers a term the carriers' readers hold equal to it: its sum commuted, and its dual with the sum commuted, neither a spelling the key is held under.
///
/// Conversion holds `a + b < 10` and `b + a < 10` equal wherever no arm stands, by the comparisons' linear views. Inside the guard's arm the written spelling is `true` by its equation, so while nothing answered the respelling, conversion refused inside the arm a pair it held outside it. `curios_analysis::answers` is the rule, the elaborator's reducer asks it at the same point, and `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
///
/// Mutation-checked: with `answers` holding only a term identical to its key, both respellings stay stuck.
#[test]
fn a_guard_answers_a_term_the_readers_hold_equal_to_it() {
    let mut certifier = certifier();
    let [guard, commuted, dual] = under(
        &mut certifier,
        "a: #NatType, b: #NatType",
        "#NatLt(#NatAdd(a, b), 10), #NatLt(#NatAdd(b, a), 10), #NatLe(10, #NatAdd(b, a))",
    );

    guarded(&mut certifier, &guard, |certifier| {
        assert_eq!(
            whnf(certifier, commuted.clone())
                .expect("reduces")
                .as_bool(),
            Some(true)
        );
        assert_eq!(
            whnf(certifier, dual).expect("reduces").as_bool(),
            Some(false)
        );
    });

    assert_eq!(
        whnf(&mut certifier, commuted).expect("reduces").as_bool(),
        None,
        "outside the arm the respelling is the stuck comparison it is"
    );
}

/// The hypotheses the tests of what reduction asks conversion share: `a`, `b`, `c` at `Nat`, `f: (Nat) -> Nat` and `h: (Bool) -> Nat`.
const ASKED: &str = "a: #NatType, b: #NatType, c: #NatType, f: (x: #NatType) -> #NatType, h: (flag: #BoolType) -> #NatType";

/// `f(a + b) < 10`, the guard.
const GUARD: &str = "#NatLt(f(#NatAdd(a, b)), 10)";

/// `f(b + a) < 10`, which conversion holds equal to [`GUARD`] and no reader of the carriers' algebra does: its one atom is another term.
const RESPELLED: &str = "#NatLt(f(#NatAdd(b, a)), 10)";

/// [`GUARD`], [`RESPELLED`] and then `terms`, each written over [`ASKED`]'s hypotheses opened in `certifier`.
fn asked<const N: usize>(certifier: &mut Certifier, terms: &str) -> [Term; N] {
    under(
        certifier,
        ASKED,
        &fill!(
            "${guard}, ${respelled}, ${terms}",
            guard = GUARD,
            respelled = RESPELLED,
            terms
        ),
    )
}

/// A guard answers a term the certifier's own conversion holds equal to its scrutinee: a call respelled in its argument, the guard's dual over that call, and — where the scrutinee is no operation of the algebra — the call itself. The control is a call on another argument.
///
/// The shared rule hands the question back and the certifier asks its conversion; `curios-elab`'s `reduce::question_tests` holds this proposition under this name. Mutation-checked: asked of the whole pair with the key's own equation left in force, the key reduces to its case value and the call is never the scrutinee.
#[test]
fn a_guard_answers_a_term_conversion_holds_equal_to_it() {
    let mut certifier = certifier();
    let [guard, respelled, dual, other, key, call, other_call] = asked(
        &mut certifier,
        "#NatLe(10, f(#NatAdd(b, a))), #NatLt(f(#NatAdd(a, c)), 10), f(#NatAdd(a, b)), f(#NatAdd(b, a)), f(#NatAdd(a, c))",
    );

    guarded(&mut certifier, &guard, |certifier| {
        assert_eq!(
            whnf(certifier, respelled).expect("reduces").as_bool(),
            Some(true)
        );
        assert_eq!(
            whnf(certifier, dual).expect("reduces").as_bool(),
            Some(false)
        );
        assert_eq!(whnf(certifier, other).expect("reduces").as_bool(), None);
    });

    certifier.scoped(|certifier| {
        certifier
            .refine(key, term("0"))
            .expect("the equation records");

        assert_eq!(whnf(certifier, call), Ok(term("0")));
        assert_eq!(whnf(certifier, other_call.clone()), Ok(other_call));
    });
}

/// A shared analysis reads a term by plain reduction, which asks the certifier's conversion nothing: under a guard, the respelling a judgment's reduction answers by asking stays stuck through `Env::force`. Totality and positivity read terms this way, and conversion's proof irrelevance is sound because acceptance waits on totality, so what totality reads may rest on no verdict of conversion's.
///
/// Mutation-checked: with `Env::force` reducing as a judgment does, the respelling is `true` to the analysis.
#[test]
fn a_shared_analysis_reads_a_term_by_reduction_that_asks_nothing() {
    let mut certifier = certifier();
    let [guard, respelled] = asked(&mut certifier, "");

    guarded(&mut certifier, &guard, |certifier| {
        let read = Env::force(certifier, &respelled).expect("reduces");
        assert_eq!(read.as_bool(), None);
        assert_eq!(
            whnf(certifier, respelled).expect("reduces").as_bool(),
            Some(true),
            "the judgment's own reduction answers it, or the reading above proves nothing"
        );
    });
}

/// A reduct is remembered by the reduction that took it: what plain reduction made of a term does not answer for it where a judgment reduces it, nor the other way, whichever came first. Both orders are run, each in an arm of its own so neither meets the other's reducts, on a certifier with its memos on and on one with them off, which must agree.
///
/// Mutation-checked both ways at each entry point's table: a hit read from the judgment's table whichever reduction asks hands plain reduction the judgment's `true` in the first order, and a store filed there whichever reduction took it hands the judgment plain reduction's stuck form in the second.
#[test]
fn a_reduct_is_remembered_by_the_reduction_that_took_it() {
    let orders = |certifier: &mut Certifier| {
        let [guard, respelled] = asked(certifier, "");
        // Through the forced entry point on both sides, which reads and writes the forced table and, beneath it, the unforced one.
        let judged = |certifier: &mut Certifier| {
            certifier
                .reduce_forced(respelled.clone())
                .expect("reduces")
                .as_bool()
        };
        let read = |certifier: &mut Certifier| {
            Env::force(certifier, &respelled)
                .expect("reduces")
                .as_bool()
        };

        let judged_first = guarded(certifier, &guard, |certifier| {
            [judged(certifier), read(certifier)]
        });
        let read_first = guarded(certifier, &guard, |certifier| {
            [read(certifier), judged(certifier)]
        });
        [judged_first, read_first]
    };

    let cached = orders(&mut certifier());
    assert_eq!(cached, [[Some(true), None], [None, Some(true)]]);
    assert_eq!(
        orders(&mut Certifier::uncached(1_000_000, SYNTAX)),
        cached,
        "the memos changed what one reduction or the other makes of the term"
    );
}

/// A question reduction puts to conversion is answered by plain reduction, so an answer that needs a question answered inside a question is refused. Under the guards `f(a + b) < 10` and `h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` is the second guard's scrutinee only to a conversion that asks the first guard about the respelled argument, and it stays stuck. The second guard names every binder the term names, so the filter in front of it passes the term and nothing but the question inside the question stands in the way. That is what ends the regress of reduction asking conversion asking reduction, and it leaves a question one conversion the budget bounds.
///
/// Mutation-checked: with the lookup asking under plain reduction too, the question decides the argument and the term is `true`.
#[test]
fn a_question_is_answered_by_reduction_that_asks_nothing() {
    let mut certifier = certifier();
    let [guard, respelled, over_guard, nested] = asked(
        &mut certifier,
        &fill!(
            "#NatLt(h(${guard}), 5), #NatLt(h(${respelled}), 5)",
            guard = GUARD,
            respelled = RESPELLED
        ),
    );

    guarded(&mut certifier, &guard, |certifier| {
        certifier
            .refine(over_guard, term("true"))
            .expect("the equation records");

        assert_eq!(
            whnf(certifier, respelled).expect("reduces").as_bool(),
            Some(true),
            "one question deep the argument is decided, or the refusal below proves nothing"
        );
        assert_eq!(whnf(certifier, nested).expect("reduces").as_bool(), None);
    });
}

/// The hypotheses the tests of a call under two proofs share: `a` at `Nat`, a proposition `bound`, two proofs of it, and `w: (n: Nat, at: bound) -> Nat`.
const PROVED: &str =
    "a: #NatType, bound: Prop, w: (n: #NatType, at: bound) -> #NatType, p1: bound, p2: bound";

/// A guard answers a call under another proof of its bound, and so does a match on the call itself. `w(a, p2) < 10` is the scrutinee `w(a, p1) < 10` to the certifier's conversion, which reads no proof, and it names a proof the key does not: so it is no reduct of the key. The filter that keeps settlement rare (`curios_analysis::could_reduce_to`) counts the binders a form names that the key does not, and passes over one that is itself a proof.
///
/// `curios-elab`'s `reduce::question_tests` holds this proposition under this name, and `curios`'s `tests::respelling` puts the programs through both checkers. Mutation-checked: with a proof counted as any other binder, both forms stay stuck.
#[test]
fn a_guard_answers_a_call_under_another_proof_of_its_bound() {
    let mut certifier = certifier();
    let [below_first, below_second, call_first, call_second] = under(
        &mut certifier,
        PROVED,
        "#NatLt(w(a, p1), 10), #NatLt(w(a, p2), 10), w(a, p1), w(a, p2)",
    );

    guarded(&mut certifier, &below_first, |certifier| {
        assert_eq!(
            whnf(certifier, below_second.clone())
                .expect("reduces")
                .as_bool(),
            Some(true)
        );
    });
    certifier.scoped(|certifier| {
        certifier
            .refine(call_first, term("0"))
            .expect("the equation records");
        assert_eq!(whnf(certifier, call_second), Ok(term("0")));
    });

    assert_eq!(
        whnf(&mut certifier, below_second)
            .expect("reduces")
            .as_bool(),
        None,
        "with no guard the form is the stuck comparison it is"
    );
}

/// A form naming a binder the key does not, outside a proof, is put to no equation: `f(b + 0 * c) < 10` converts with the guard `f(b) < 10` and names `c`, which is no proof, so the filter that keeps settlement rare counts it and the form stays stuck. That is what the filter still gives up, stated so that moving it is seen; the control is the conversion itself, which holds the two equal with no arm standing.
///
/// `curios-elab`'s `reduce::question_tests` holds this proposition under this name. Mutation-checked: with the filter passing every binder, the form is asked about and is `true`.
#[test]
fn a_form_naming_another_binder_outside_a_proof_is_put_to_no_equation() {
    let mut certifier = certifier();
    let [guard, respelled] = under(
        &mut certifier,
        ASKED,
        "#NatLt(f(b), 10), #NatLt(f(#NatAdd(b, #NatMul(0, c))), 10)",
    );

    assert_eq!(
        convert(&mut certifier, &term("#BoolType"), &guard, &respelled),
        Ok(true),
        "the two convert, or the miss below is no limit of the filter"
    );
    guarded(&mut certifier, &guard, |certifier| {
        assert_eq!(whnf(certifier, respelled).expect("reduces").as_bool(), None);
    });
}

/// A comparison's verdict is remembered by the reduction it was reached under: under a guard, the respelling a judgment's reduction answers by asking converts with `true` for a judgment and not for plain reduction, whichever compared first. Both orders are run, each in an arm of its own so neither meets the other's verdicts, and the uncached certifier agrees.
///
/// Mutation-checked both ways: a verdict read from the judgment's table whichever reduction asks hands plain reduction the judgment's `true` in the first order, and one filed there whichever reduction reached it hands the judgment plain reduction's `false` in the second.
#[test]
fn a_verdict_is_remembered_by_the_reduction_that_reached_it() {
    let orders = |certifier: &mut Certifier| {
        let [guard, respelled] = asked(certifier, "");
        let (bool_type, truth) = (term("#BoolType"), term("true"));
        let judged = |certifier: &mut Certifier| {
            convert(certifier, &bool_type, &respelled, &truth).expect("compares")
        };
        let read = |certifier: &mut Certifier| {
            certifier
                .plainly(|certifier| convert(certifier, &bool_type, &respelled, &truth))
                .expect("compares")
        };

        let judged_first = guarded(certifier, &guard, |certifier| {
            [judged(certifier), read(certifier)]
        });
        let read_first = guarded(certifier, &guard, |certifier| {
            [read(certifier), judged(certifier)]
        });
        [judged_first, read_first]
    };

    let cached = orders(&mut certifier());
    assert_eq!(cached, [[true, false], [false, true]]);
    assert_eq!(
        orders(&mut Certifier::uncached(1_000_000, SYNTAX)),
        cached,
        "the memos changed what one reduction or the other makes of the comparison"
    );
}

/// An equation's reduced spelling is settled by the reduction that asks for it, once for each. Under the guard `f(a + b) < 10`, the inner guard `f(b + a) < 10 && c < 3` reduces to `c < 3` for a judgment, which asks its conversion about the left operand, and stands as written for plain reduction; so `c < 3` is `true` to a judgment and stuck to an analysis, whichever asked first. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
///
/// Mutation-checked both ways: with the spelling read as a judgment's whichever reduction asks, plain reduction is handed `true` in the first order, and with it filed there whichever reduction settled it, the judgment meets plain reduction's spelling in the second and answers nothing.
#[test]
fn a_reduced_spelling_is_settled_by_the_reduction_that_asks_for_it() {
    let orders = |certifier: &mut Certifier| {
        let [guard, _, probe, inner] = asked(
            certifier,
            &fill!(
                "#NatLt(c, 3), #BoolAnd(${respelled}, #NatLt(c, 3))",
                respelled = RESPELLED
            ),
        );
        let judged = |certifier: &mut Certifier| {
            certifier
                .reduce_forced(probe.clone())
                .expect("reduces")
                .as_bool()
        };
        let read =
            |certifier: &mut Certifier| Env::force(certifier, &probe).expect("reduces").as_bool();
        let both = |certifier: &mut Certifier,
                    inside: &dyn Fn(&mut Certifier) -> [Option<bool>; 2]| {
            guarded(certifier, &guard, |certifier| {
                certifier
                    .refine(inner.clone(), term("true"))
                    .expect("the equation records");
                inside(certifier)
            })
        };

        [
            both(certifier, &|certifier| [judged(certifier), read(certifier)]),
            both(certifier, &|certifier| [read(certifier), judged(certifier)]),
        ]
    };

    let cached = orders(&mut certifier());
    assert_eq!(cached, [[Some(true), None], [None, Some(true)]]);
    assert_eq!(
        orders(&mut Certifier::uncached(1_000_000, SYNTAX)),
        cached,
        "the memos changed what one reduction or the other makes of the term"
    );
}

/// A stuck fold is taken again over the atoms the certifier's own conversion holds one: an equality, an ordering and a truncated difference over `f(a + b)` and `f(b + a)` are decided, where a fold that pairs atoms by identity left each stuck and conversion was no congruence under it. The control is a call on another argument, and a fold that decides no more is left as plain reduction spells it, with nothing respelled written back.
///
/// Plain reduction takes no fold again, which the reading through `Env::force` holds after the judgment's own reduct is remembered. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_fold_is_taken_again_over_atoms_conversion_holds_one() {
    let mut certifier = certifier();
    let [equal, at_most, difference, other, undecided] = under(
        &mut certifier,
        ASKED,
        &fill!(
            "#NatEql(${left}, ${right}), #NatLe(${left}, ${right}), #NatSub(${left}, ${right}), #NatEql(${left}, f(#NatAdd(a, c))), #NatLt(${left}, #NatMul(${right}, c))",
            left = "f(#NatAdd(a, b))",
            right = "f(#NatAdd(b, a))"
        ),
    );

    assert_eq!(
        whnf(&mut certifier, equal.clone())
            .expect("reduces")
            .as_bool(),
        Some(true)
    );
    assert_eq!(
        whnf(&mut certifier, at_most).expect("reduces").as_bool(),
        Some(true)
    );
    assert_eq!(whnf(&mut certifier, difference), Ok(term("0")));

    assert_eq!(
        whnf(&mut certifier, other).expect("reduces").as_bool(),
        None
    );

    assert_eq!(
        Env::force(&mut certifier, &equal)
            .expect("reduces")
            .as_bool(),
        None,
        "plain reduction takes no fold again"
    );

    let plain = Env::force(&mut certifier, &undecided).expect("reduces");
    assert_eq!(whnf(&mut certifier, undecided), Ok(plain));
}

/// A fold inside a question is not taken again, a question being answered by plain reduction (`a_question_is_answered_by_reduction_that_asks_nothing` holds the same of a guard's lookup): `h(f(a + b) == f(b + a)) == h(true)` stays stuck, its two calls being one only to a conversion that takes the fold in the first one's argument again.
///
/// Mutation-checked: with the fold taken again under plain reduction too, the question decides the argument and the comparison is `true`.
#[test]
fn a_fold_inside_a_question_is_not_taken_again() {
    let mut certifier = certifier();
    let [equal, nested] = under(
        &mut certifier,
        ASKED,
        &fill!(
            "${equal}, #NatEql(h(${equal}), h(true))",
            equal = "#NatEql(f(#NatAdd(a, b)), f(#NatAdd(b, a)))"
        ),
    );

    assert_eq!(
        whnf(&mut certifier, equal).expect("reduces").as_bool(),
        Some(true),
        "one question deep the fold is decided, or the refusal below proves nothing"
    );
    assert_eq!(
        whnf(&mut certifier, nested).expect("reduces").as_bool(),
        None
    );
}

/// A `Bool` match on `${scrutinee}` at `Nat`: `2` where it is false, `1` where it is true.
const ON_BOOL: &str = "#{goal = #NatType} match ${scrutinee} | false => 2 | true => 1 end";

/// A stuck elimination takes the arm the carriers' algebra reads its scrutinee as, in both reductions: a `Bool` match on `(a < b && c < b) && ¬(a < b)`, a contradiction across connectives no fold sees, is its false arm, and a structural `Nat` match on `(a + b + 1) * (b + a + 1)`, whose distribution has a floor, is its successor arm. Plain reduction takes both, the theory asking no judgment. The controls are a connective the decision diagram leaves open and a product with no floor, whose matches stand. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_elimination_takes_the_arm_the_algebra_reads() {
    let mut certifier = certifier();
    let on_bool = |scrutinee: &str| fill!(ON_BOOL, scrutinee);
    let on_product = |floor: usize| {
        fill!(
            "#{goal = #NatType} match #NatMul(#NatAdd(#NatAdd(a, b), ${floor}), #NatAdd(#NatAdd(b, a), ${floor})) | 0 => 10 | pred + 1 => 20 end",
            floor
        )
    };
    let [contradiction, successor, open_connective, open_product] = under(
        &mut certifier,
        ASKED,
        &[
            on_bool("#BoolAnd(#BoolAnd(#NatLt(a, b), #NatLt(c, b)), #BoolXor(#NatLt(a, b), true))"),
            on_product(1),
            on_bool("#BoolAnd(#NatLt(a, b), #NatLt(c, b))"),
            on_product(0),
        ]
        .join(", "),
    );

    for decided in [&contradiction, &successor] {
        let arm = whnf(&mut certifier, decided.clone()).expect("reduces");
        assert!(arm.as_nat().is_some(), "{decided:?} stands as {arm:?}");
        assert_eq!(
            Env::force(&mut certifier, decided),
            Ok(arm),
            "plain reduction takes the arm too"
        );
    }
    assert_eq!(whnf(&mut certifier, contradiction), Ok(term("2")));
    assert_eq!(whnf(&mut certifier, successor), Ok(term("20")));

    for open in [open_connective, open_product] {
        let stuck = whnf(&mut certifier, open.clone()).expect("reduces");
        assert!(stuck.as_nat().is_none(), "{open:?} takes an arm: {stuck:?}");
    }
}

/// A stuck elimination classes its scrutinee's atoms in a judgment's reduction alone: a `Bool` match on `(f(a + b) < c && b < c) && ¬(f(b + a) < c)`, a contradiction once the certifier's conversion holds the two calls one, is its false arm, and plain reduction, which asks nothing, leaves it standing. No fold law reads the two comparisons together, so the classing at the dispatch is what decides it. The control is the same connective over a call on another argument, which no classing makes a contradiction. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_stuck_elimination_classes_its_scrutinees_atoms_in_a_judgments_reduction_alone() {
    let mut certifier = certifier();
    let on = |other: &str| {
        fill!(
            ON_BOOL,
            scrutinee = fill!(
                "#BoolAnd(#BoolAnd(#NatLt(f(#NatAdd(a, b)), c), #NatLt(b, c)), #BoolXor(#NatLt(${other}, c), true))",
                other
            )
        )
    };
    let [classed, other] = under(
        &mut certifier,
        ASKED,
        &fill!(
            "${classed}, ${other}",
            classed = on("f(#NatAdd(b, a))"),
            other = on("f(#NatAdd(a, c))")
        ),
    );

    assert_eq!(whnf(&mut certifier, classed.clone()), Ok(term("2")));
    let plain = Env::force(&mut certifier, &classed).expect("reduces");
    assert!(
        plain.as_nat().is_none(),
        "plain reduction classed {classed:?}"
    );

    let other = whnf(&mut certifier, other).expect("reduces");
    assert!(
        other.as_nat().is_none(),
        "a call on another argument classed: {other:?}"
    );
}
