//! The `Nat` peel: sums decided up to the order of their summands, a surviving floor clashed, and monomials paired by the identity of their factors.

use super::{test_support::*, *};

// The conclusion cancelling summands adds to the peel: two sums that differ only in the order of their addends are the same number, so peeling decides them equal instead of handing a pair to a structural comparison that compares spellings and refuses. Sound because `+` commutes; new, because nothing else in the peel normalises summand order.
#[test]
fn peel_nat_decides_a_commuted_sum_equal() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));

    let peel = peel_nat_pair(
        &Intrinsic::Nat(nat_of(1, add(x.clone(), y.clone()))),
        &Intrinsic::Nat(nat_of(1, add(y.clone(), x.clone()))),
    );

    assert!(
        matches!(peel, Some(Deduction::Equal)),
        "`x + y + 1` and `y + x + 1` are one number"
    );
}

// The clash the inverter reads as *impossible*, which is what excuses an omitted arm — so it must fire only where the two sides genuinely cannot be equal. A surviving positive floor against nothing is that case: whatever the symbolic residual takes, one side stays strictly larger.
#[test]
fn peel_nat_clashes_a_surviving_floor_against_the_identity() {
    let x = sym(0, "x");

    let peel = peel_nat_pair(
        &Intrinsic::Nat(nat_of(2, x.clone())),
        &Intrinsic::Nat(nat_of(1, x.clone())),
    );

    assert!(
        matches!(peel, Some(Deduction::Impossible)),
        "`x + 2` never equals `x + 1`"
    );
}

// The control against closing the clash above by clashing everything: a shared floor over *distinct* symbols cancels to a pair that may still be equal, so peeling must hand it on rather than decide it.
#[test]
fn peel_nat_continues_where_the_residuals_may_still_agree() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));

    let peel = peel_nat_pair(
        &Intrinsic::Nat(nat_of(1, x.clone())),
        &Intrinsic::Nat(nat_of(1, y.clone())),
    );

    assert!(
        matches!(peel, Some(Deduction::Equivalent(_))),
        "`x` and `y` are undecided, not unequal"
    );
}

fn mul(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_mul(left, right))
}

fn as_monomial(term: Term) -> Intrinsic {
    match &*term {
        Subterm::Intrinsic(intrinsic) => intrinsic.clone(),
        _ => unreachable!("a product is an intrinsic"),
    }
}

// Two monomials pair their factors by identity, not by position: `c · m · k` against `d · c · k` leaves `m` against `d` whichever order the hashes put the factors in — the pair a metavariable standing where `m` stands is solved from.
#[test]
fn peel_monomial_pairs_factors_by_identity_not_by_position() {
    let (c, d, k, m) = (sym(0, "c"), sym(1, "d"), sym(2, "k"), sym(3, "m"));
    let expected = as_monomial(mul(mul(c.clone(), m.clone()), k.clone()));
    let actual = as_monomial(mul(mul(d.clone(), c.clone()), k.clone()));

    assert!(matches!(
        peel_monomial(&expected, &actual),
        Some(Conclusion::Sufficient((left, right))) if left == m && right == d
    ));
}

// One multiset of factors under one coefficient is one monomial, however it is nested or ordered.
#[test]
fn peel_monomial_decides_a_reordered_product_equal() {
    let (c, d, k) = (sym(0, "c"), sym(1, "d"), sym(2, "k"));
    let one = as_monomial(mul(mul(c.clone(), d.clone()), k.clone()));
    let other = as_monomial(mul(k.clone(), mul(d.clone(), c.clone())));

    assert!(matches!(
        peel_monomial(&one, &other),
        Some(Conclusion::Equal)
    ));
}

// The controls: two factors left on each side have no forced pairing, and two coefficients are two monomials unless a factor is zero — so both decline to the caller's congruence rather than deciding anything, and neither ever clashes.
#[test]
fn peel_monomial_declines_where_no_pairing_is_forced() {
    let (c, d, k, m) = (sym(0, "c"), sym(1, "d"), sym(2, "k"), sym(3, "m"));
    let two = as_monomial(mul(c.clone(), d.clone()));
    let other_two = as_monomial(mul(k.clone(), m.clone()));
    assert!(peel_monomial(&two, &other_two).is_none());

    let doubled = as_monomial(mul(
        Term::intrinsic(Intrinsic::Nat(Nat::new(2u32))),
        c.clone(),
    ));
    let tripled = as_monomial(mul(
        Term::intrinsic(Intrinsic::Nat(Nat::new(3u32))),
        c.clone(),
    ));
    assert!(peel_monomial(&doubled, &tripled).is_none());
}

// The restriction inversion rests on, stated where it is enforced today: `x · f` against `x · g` gives conversion `f` against `g`, which is sufficient for the equation and not implied by it — at `x = 0` any `f` and `g` agree — so the entry inversion reads, `peel_intrinsic`, has no answer for the pair at all, and the residual is the converters' alone.
#[test]
fn a_shared_factor_leaves_conversion_a_residual_and_inversion_nothing() {
    let (x, f, g) = (sym(0, "x"), sym(1, "f"), sym(2, "g"));
    let this = as_monomial(mul(x.clone(), f.clone()));
    let that = as_monomial(mul(x.clone(), g.clone()));

    assert!(matches!(
        peel_monomial(&this, &that),
        Some(Conclusion::Sufficient((left, right))) if left == f && right == g
    ));
    assert!(peel_intrinsic(&this, &that).is_none());
}
