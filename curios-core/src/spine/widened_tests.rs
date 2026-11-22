//! A widened natural's division: its `Int` remainder and quotient against the natural's own, the law they rest on over a grid of values, and the declining side of each.

use {
    super::{test_support::*, *},
    crate::*,
    curios_num::{Integer, Natural},
};

fn int(value: i32) -> Term {
    Term::intrinsic(Intrinsic::Int(Integer::from(value)))
}

fn nat(value: u32) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

fn widened(natural: Term) -> Term {
    Term::intrinsic(Intrinsic::NatToInt(natural))
}

fn qed() -> Term {
    sym(9_999, "qed")
}

fn int_rem(dividend: Term, divisor: Term) -> Intrinsic {
    Intrinsic::int_rem(dividend, divisor, qed())
}

fn nat_rem(dividend: Term, divisor: Term) -> Intrinsic {
    Intrinsic::nat_rem(dividend, divisor, qed())
}

fn int_div(dividend: Term, divisor: Term) -> Intrinsic {
    Intrinsic::int_div(dividend, divisor, qed())
}

/// `Nat/to_int(dividend / divisor)`.
fn widened_quotient(dividend: Term, divisor: Term) -> Intrinsic {
    Intrinsic::NatToInt(Term::intrinsic(Intrinsic::nat_div(
        dividend,
        divisor,
        qed(),
    )))
}

// The equation the peel states, held on values: Euclid's division of a dividend that is no negative number is the natural's, its remainder by `d` the natural's by `|d|` at either sign, and its quotient by a positive `d` the natural's.
#[test]
fn a_widened_naturals_euclidean_division_is_the_naturals_on_a_grid() {
    for a in 0..40u32 {
        for d in (-9..=9i32).filter(|d| *d != 0) {
            let (dividend, divisor) = (Integer::from(a), Integer::from(d));
            let (natural, magnitude) = (Natural::from(a), Natural::from(d.unsigned_abs()));
            assert_eq!(
                dividend.rem(&divisor),
                natural.rem(&magnitude).map(Integer::from),
                "{a} % {d}"
            );
            if d > 0 {
                assert_eq!(
                    dividend.div(&divisor),
                    natural.div(&magnitude).map(Integer::from),
                    "{a} / {d}"
                );
            }
        }
    }
}

// The remainder at a literal divisor of either sign, at a symbolic one, and over a dividend that is a widened sum, each in both orders; and the quotient at a positive literal and a symbolic divisor.
#[test]
fn peel_widened_division_holds_a_widened_naturals_division_the_naturals() {
    let (n, k) = (sym(0, "n"), sym(1, "k"));
    let one = |left: &Intrinsic, right: &Intrinsic| {
        matches!(peel_widened_division(left, right), Some(Deduction::Equal))
            && matches!(peel_widened_division(right, left), Some(Deduction::Equal))
    };

    assert!(one(
        &int_rem(widened(n.clone()), int(3)),
        &nat_rem(n.clone(), nat(3))
    ));
    assert!(one(
        &int_rem(widened(n.clone()), int(-3)),
        &nat_rem(n.clone(), nat(3))
    ));
    assert!(one(
        &int_rem(widened(n.clone()), widened(k.clone())),
        &nat_rem(n.clone(), k.clone())
    ));
    let sum = Term::intrinsic(Intrinsic::int_add(widened(n.clone()), int(1)));
    assert!(one(
        &int_rem(sum, int(3)),
        &nat_rem(
            Term::intrinsic(Intrinsic::Nat(nat_of(1, n.clone()))),
            nat(3)
        )
    ));
    assert!(one(
        &int_div(widened(n.clone()), int(3)),
        &widened_quotient(n.clone(), nat(3))
    ));
    assert!(one(
        &int_div(widened(n.clone()), widened(k.clone())),
        &widened_quotient(n, k)
    ));
}

// Each near miss is undecided, never impossible: another divisor, the quotient by a negative divisor, which is the natural's negated, another dividend, and a dividend that is no image of a natural, `Nat/to_int(n) - 1`, whose remainder is `2` where the natural's of `n - 1` is `0` at `n = 0`. A pair of no such shape is none of the peel's.
#[test]
fn peel_widened_division_declines_a_near_miss() {
    let (n, m) = (sym(0, "n"), sym(1, "m"));
    let undecided = |left: &Intrinsic, right: &Intrinsic| {
        matches!(
            peel_widened_division(left, right),
            Some(Deduction::Undecided)
        )
    };

    assert!(undecided(
        &int_rem(widened(n.clone()), int(3)),
        &nat_rem(n.clone(), nat(4))
    ));
    assert!(undecided(
        &int_div(widened(n.clone()), int(-3)),
        &widened_quotient(n.clone(), nat(3))
    ));
    assert!(undecided(
        &int_div(widened(n.clone()), int(3)),
        &widened_quotient(m.clone(), nat(3))
    ));
    let below = Term::intrinsic(Intrinsic::int_add(widened(n.clone()), int(-1)));
    assert!(undecided(
        &int_rem(below, int(3)),
        &nat_rem(
            Term::intrinsic(Intrinsic::NatSub(n.clone(), nat(1))),
            nat(3)
        )
    ));
    assert_eq!(
        peel_widened_division(
            &int_rem(widened(n.clone()), int(3)),
            &Intrinsic::NatToInt(m)
        ),
        None
    );
}
