//! What the fold decides of a comparison a truncated difference stands in, held to the carrier's own values.

use {crate::reduce::intrinsic::test_support::*, crate::*};

/// The sides a difference can stand on, each with the spelling to name it by: bare, floored, nested, inside a sum, scaled, and against a side the bounds oracle or the lattice reads.
fn sides() -> Vec<(&'static str, Term)> {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let sub =
        |minuend: Term, subtrahend: Term| Term::intrinsic(Intrinsic::nat_sub(minuend, subtrahend));
    let rem = |dividend: Term, divisor: u32| {
        Term::intrinsic(Intrinsic::NatRem {
            dividend,
            divisor: lit(divisor),
            non_zero: qed(),
        })
    };
    let and = |left: Term, right: u32| Term::intrinsic(Intrinsic::NatAnd(left, lit(right)));
    let min = |left: Term, right: Term| Term::intrinsic(Intrinsic::NatMin(left, right));
    let max = |left: Term, right: Term| Term::intrinsic(Intrinsic::NatMax(left, right));

    vec![
        ("x", x.clone()),
        ("y", y.clone()),
        ("z", z.clone()),
        ("0", lit(0)),
        ("1", lit(1)),
        ("3", lit(3)),
        ("x + 1", add(x.clone(), lit(1))),
        ("y + z", add(y.clone(), z.clone())),
        ("2 * x", scaled(2, x.clone())),
        ("x - y", sub(x.clone(), y.clone())),
        ("y - x", sub(y.clone(), x.clone())),
        ("x - z", sub(x.clone(), z.clone())),
        ("z - y", sub(z.clone(), y.clone())),
        ("x - 3", sub(x.clone(), lit(3))),
        ("3 - x", sub(lit(3), x.clone())),
        ("(x - y) + 1", add(sub(x.clone(), y.clone()), lit(1))),
        ("(x + 1) - y", sub(add(x.clone(), lit(1)), y.clone())),
        ("x - (y + 1)", sub(x.clone(), add(y.clone(), lit(1)))),
        ("(x - y) - z", sub(sub(x.clone(), y.clone()), z.clone())),
        ("x - (y - z)", sub(x.clone(), sub(y.clone(), z.clone()))),
        ("(x - y) + z", add(sub(x.clone(), y.clone()), z.clone())),
        ("2 * (x - y)", scaled(2, sub(x.clone(), y.clone()))),
        (
            "2 * x - (2 * y + 1)",
            sub(scaled(2, x.clone()), add(scaled(2, y.clone()), lit(1))),
        ),
        ("x % 4 - y", sub(rem(x.clone(), 4), y.clone())),
        ("y - x % 4", sub(y.clone(), rem(x.clone(), 4))),
        ("x % 4 - 3", sub(rem(x.clone(), 4), lit(3))),
        (
            "x % 4 - (y + 4)",
            sub(rem(x.clone(), 4), add(y.clone(), lit(4))),
        ),
        ("(x - y) + y", add(sub(x.clone(), y.clone()), y.clone())),
        ("x - (x - y)", sub(x.clone(), sub(x.clone(), y.clone()))),
        ("and(x, 3) - y", sub(and(x.clone(), 3), y.clone())),
        ("min(x, y) - z", sub(min(x.clone(), y.clone()), z.clone())),
        ("max(x, y) - z", sub(max(x, y), z)),
    ]
}

/// The four comparisons the roster builds, each by the spelling to name it by.
fn comparisons() -> Vec<(&'static str, fn(Term, Term) -> Intrinsic)> {
    vec![
        ("<=", |left, right| Intrinsic::nat_lte(left, right)),
        ("<", |left, right| Intrinsic::nat_lt(left, right)),
        ("==", |left, right| Intrinsic::nat_eql(left, right)),
        ("!=", |left, right| Intrinsic::nat_neq(left, right)),
    ]
}

/// `comparison` with `x`, `y` and `z` at `values`, folded to the `Bool` a closed comparison is.
fn value_at(comparison: &Term, values: [u32; 3]) -> bool {
    let mut term = comparison.clone();
    for (index, value) in values.iter().enumerate() {
        let binder = Free::local(index as u64, None);
        term = at(term, &binder, lit(*value));
    }
    match &*fold(term) {
        Subterm::Intrinsic(Intrinsic::Bool(value)) => *value,
        other => panic!("a closed comparison folds to a `Bool`, not {other:?}"),
    }
}

// A verdict the fold reaches is a definitional equation congruence carries, so one it reaches of a comparison a difference stands in is held to the values of the carrier: every side a difference can stand on, at each of the four comparisons and on either side, folded open and then at every valuation of its three symbols past the literals it holds. The adjunction the comparison reads a bare difference through is the newest of those verdicts, and an over-reported one is a false equation — a strict bound carried strictly across a side that may be zero, an apartness carried across one, a floored or scaled difference read as a bare one. Mutation checks: `Comparison::through_residual`'s `(Lt, false)` arm answering `Lt` instead of `Le` fails this at `y <= x % 4 - (y + 4)`, decided false where both sides are zero at `x = 0, y = 0`; its `(Ne, false)` arm answering `Ne` instead of nothing fails it at `2 * (x - y) == 2 * x - (2 * y + 1)`, decided false where both sides are zero at `x = 0, y = 0`; and its `(Stuck, _)` arm answering `Le` — a verdict concluded from a comparison the oracle could not decide — fails it at `x < y - x`, decided false at `x = 0, y = 1`.
#[test]
fn a_verdict_the_fold_reaches_holds_at_every_valuation() {
    let sides = sides();
    let comparisons = comparisons();
    let mut decided = 0usize;
    for (left_name, left) in &sides {
        for (right_name, right) in &sides {
            for (symbol, spell) in &comparisons {
                let comparison = Term::intrinsic(spell(left.clone(), right.clone()));
                let folded = fold(comparison.clone());
                let Subterm::Intrinsic(Intrinsic::Bool(verdict)) = &*folded else {
                    continue;
                };
                decided += 1;
                for a in 0..5u32 {
                    for b in 0..5u32 {
                        for c in 0..5u32 {
                            assert_eq!(
                                *verdict,
                                value_at(&comparison, [a, b, c]),
                                "the fold decided `{left_name} {symbol} {right_name}` is {verdict}, which fails at x = {a}, y = {b}, z = {c}"
                            );
                        }
                    }
                }
            }
        }
    }
    assert!(
        decided > 100,
        "a grid that decided almost nothing would hold vacuously: {decided} decided"
    );
}

// The facts the bound prover reads of a truncated subtraction are true of the fold's own values, which is what a certificate summing them rests on: what both of its cases state — `a - b <= a`, which conversion decides, and `a <= b + (a - b)`, which `/std/Nat/le/add_sub` proves and conversion does not decide — and each case's own, `b + (a - b)` being `a` where `b <= a` and `a - b` being at most zero where `a < b`. A fact false at one valuation is a proof of `False` whatever a certificate does with it, and the lower half is the one the elaborator reads by a lemma rather than by a verdict, so nothing in the fold holds it.
#[test]
fn the_facts_read_of_a_difference_hold_at_every_valuation() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let difference = Term::intrinsic(Intrinsic::nat_sub(x.clone(), y.clone()));
    let raised = add(y.clone(), difference.clone());
    let within = Term::intrinsic(Intrinsic::nat_lte(difference.clone(), x.clone()));
    let lower = Term::intrinsic(Intrinsic::nat_lte(x.clone(), raised.clone()));
    let exact = Term::intrinsic(Intrinsic::nat_eql(raised, x.clone()));
    let truncated = Term::intrinsic(Intrinsic::nat_lte(difference, lit(0)));
    let guard = Term::intrinsic(Intrinsic::nat_lte(y, x));
    for a in 0..16u32 {
        for b in 0..16u32 {
            let at = |fact: &Term| value_at(fact, [a, b, 0]);
            assert!(at(&within), "`a - b <= a` fails at a = {a}, b = {b}");
            assert!(at(&lower), "`a <= b + (a - b)` fails at a = {a}, b = {b}");
            match at(&guard) {
                true => assert!(
                    at(&exact),
                    "`b + (a - b) == a` fails under `b <= a` at a = {a}, b = {b}"
                ),
                false => assert!(
                    at(&truncated),
                    "`a - b <= 0` fails under `a < b` at a = {a}, b = {b}"
                ),
            }
        }
    }
}

// What the fold leaves of a comparison it does not decide is a normal form, so two spellings of one proposition the fold answers alike are one term: the grid's undecided comparisons are rebuilt from their peeled operands, and a rebuilt one folds to itself. A fold that rebuilt a side it had read across would state a proposition the source does not.
#[test]
fn an_undecided_comparison_folds_to_a_fixed_point() {
    let sides = sides();
    let comparisons = comparisons();
    let mut open = 0usize;
    for (left_name, left) in &sides {
        for (right_name, right) in &sides {
            for (symbol, spell) in &comparisons {
                let comparison = Term::intrinsic(spell(left.clone(), right.clone()));
                let folded = fold(comparison);
                if matches!(&*folded, Subterm::Intrinsic(Intrinsic::Bool(_))) {
                    continue;
                }
                open += 1;
                assert_eq!(
                    fold(folded.clone()),
                    folded,
                    "`{left_name} {symbol} {right_name}` folds to a term the fold moves again"
                );
            }
        }
    }
    assert!(open > 100, "a grid with nothing open says nothing: {open}");
}
