//! Peels that decide a value up to the order of its operands: a conjunction's leaf set, and the symmetric comparisons and bitwise operations.

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
        matches!(peel_bool(&nested, &commuted), Some(Peel::Equal)),
        "`(x && y) && z` and `z && (y && x)` are one value"
    );

    let repeated = and(Term::intrinsic(and(x.clone(), y.clone())), x.clone());
    let once = and(y, x);
    assert!(
        matches!(peel_bool(&repeated, &once), Some(Peel::Equal)),
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
        matches!(peel_bool(&this, &that), Some(Peel::Stuck)),
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
        matches!(peel_symmetric(&this, &that), Some(Peel::Equal)),
        "`x == y` and `y == x` are one value"
    );

    let unlike = Intrinsic::nat_eql(x.clone(), z);
    assert!(
        matches!(peel_symmetric(&this, &unlike), Some(Peel::Stuck)),
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
            matches!(peel_symmetric(&this, &that), Some(Peel::Equal)),
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
