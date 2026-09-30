//! Peels that decide a value up to the order of its operands: a conjunction's leaf set, the symmetric comparisons and bitwise operations, and two comparisons' sides paired by identity.

use super::{test_support::*, *};

fn and(left: Term, right: Term) -> Intrinsic {
    Intrinsic::BoolAnd(left, right)
}

// `&&` is a semilattice on its leaves, so a commuted, reassociated or repeated conjunction is the same set of leaves and the same value. Decided as a set rather than by spelling the tree one way, which is what the `&&`/`||` cliff was.
#[test]
fn peel_bool_decides_a_commuted_and_reassociated_conjunction_equal() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    let nested = and(Term::intrinsic(and(x.clone(), y.clone())), z.clone());
    let commuted = and(z, Term::intrinsic(and(y.clone(), x.clone())));
    assert!(
        matches!(peel_bool(&nested, &commuted), Some(Deduction::Equal)),
        "`(x && y) && z` and `z && (y && x)` are one value"
    );

    let repeated = and(Term::intrinsic(and(x.clone(), y.clone())), x.clone());
    let once = and(y, x);
    assert!(
        matches!(peel_bool(&repeated, &once), Some(Deduction::Equal)),
        "a repeated leaf is the leaf"
    );
}

// The control: unlike leaf sets are undecided, not unequal, since the values may still agree — so the peel declines to the caller's congruence and never clashes.
#[test]
fn peel_bool_declines_unlike_leaf_sets_without_clashing() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    let this = and(x.clone(), y);
    let that = and(x.clone(), z);
    assert!(
        matches!(peel_bool(&this, &that), Some(Deduction::Undecided)),
        "`x && y` against `x && z` is the congruence's"
    );

    let disjunction = Intrinsic::BoolOr(x.clone(), x);
    assert!(
        peel_bool(&this, &disjunction).is_none(),
        "a conjunction against a disjunction is not this peel's"
    );
}

// A symmetric comparison is one value with its operands in either order, and it is decided here rather than respelled at the fold, since a guard's refinement is keyed on its written spelling.
#[test]
fn peel_symmetric_decides_a_swapped_comparison_equal() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    let this = Intrinsic::nat_eql(x.clone(), y.clone());
    let that = Intrinsic::nat_eql(y.clone(), x.clone());
    assert!(
        matches!(peel_symmetric(&this, &that), Some(Deduction::Equal)),
        "`x == y` and `y == x` are one value"
    );

    let unlike = Intrinsic::nat_eql(x.clone(), z);
    assert!(
        matches!(peel_symmetric(&this, &unlike), Some(Deduction::Undecided)),
        "`x == y` against `x == z` is the congruence's"
    );

    let ordered = Intrinsic::nat_lt(x.clone(), y.clone());
    let reversed = Intrinsic::nat_lt(y, x);
    assert!(
        peel_symmetric(&ordered, &reversed).is_none(),
        "an order relation is not symmetric and is not this peel's"
    );
}

// The bitwise lattice on ℕ commutes as `xor` on `Bool` does, and for the same reason it is a peel and not a spelling: a bitwise operand may be what a guard refines on, and its key is recorded as written.
#[test]
fn peel_symmetric_decides_a_swapped_bitwise_operation_equal() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));

    for (this, that) in [
        (
            Intrinsic::NatAnd(x.clone(), y.clone()),
            Intrinsic::NatAnd(y.clone(), x.clone()),
        ),
        (
            Intrinsic::NatOr(x.clone(), y.clone()),
            Intrinsic::NatOr(y.clone(), x.clone()),
        ),
        (
            Intrinsic::NatXor(x.clone(), y.clone()),
            Intrinsic::NatXor(y.clone(), x.clone()),
        ),
    ] {
        assert!(
            matches!(peel_symmetric(&this, &that), Some(Deduction::Equal)),
            "a bitwise operation is one value with its operands swapped"
        );
    }

    let shifted = Intrinsic::NatShl(x.clone(), y.clone());
    let reversed = Intrinsic::NatShl(y, x);
    assert!(
        peel_symmetric(&shifted, &reversed).is_none(),
        "a shift is not symmetric and is not this peel's"
    );
}

// Two comparisons' sides are paired by identity, not by the positions the linear views' rank order put them in: `y != w` against `x != y` leaves `w` against `x`, which is what solves a metavariable standing for `w` wherever its number sorts it.
#[test]
fn peel_comparison_pairs_sides_by_identity_not_by_position() {
    let (w, x, y) = (sym(0, "w"), sym(1, "x"), sym(2, "y"));
    assert_eq!(
        peel_comparison(
            &Intrinsic::NatNeq(y.clone(), w.clone()),
            &Intrinsic::NatNeq(x.clone(), y)
        ),
        Some(Conclusion::Sufficient((w, x))),
    );
}

#[test]
fn peel_comparison_decides_a_swapped_comparison_equal() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    assert_eq!(
        peel_comparison(
            &Intrinsic::IntEql(x.clone(), y.clone()),
            &Intrinsic::IntEql(y, x)
        ),
        Some(Conclusion::Equal),
    );
}

// No side in common is a pair the peel does not read, and neither is an equality against a disequality: the shape congruence keeps both.
#[test]
fn peel_comparison_declines_where_no_side_is_shared() {
    let (v, w, x, y) = (sym(0, "v"), sym(1, "w"), sym(2, "x"), sym(3, "y"));
    assert!(
        peel_comparison(
            &Intrinsic::NatEql(v, w),
            &Intrinsic::NatEql(x.clone(), y.clone())
        )
        .is_none()
    );
    assert!(
        peel_comparison(
            &Intrinsic::NatEql(x.clone(), y.clone()),
            &Intrinsic::NatNeq(x, y)
        )
        .is_none()
    );
}
