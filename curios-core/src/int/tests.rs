//! The signed sum normal form: what folds to one term, what cancels, and what stays where it was written.

use super::*;

fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&crate::Free::local(index, Some(hint)))
}

fn int(value: i32) -> Term {
    Term::intrinsic(Intrinsic::Int(Integer::from(value)))
}

fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::IntAdd(left, right))
}

fn mul(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::IntMul(left, right))
}

// Like terms merge by coefficient and a subtraction is a negative coefficient, so `i + i - i` is `i` and `i - i` is the constant `0` — the group law `Nat`'s truncated subtraction cannot state.
#[test]
fn a_subtraction_cancels_through_the_coefficients() {
    let i = sym(0, "i");

    let twice_less_once = int_sum(&int_sum(&i, &i), &int_negate(&i));
    assert_eq!(twice_less_once, i, "`i + i - i` is `i`");

    let nothing = int_sum(&i, &int_negate(&i));
    assert!(int_is_zero(&nothing), "`i - i` is `0`");
}

// The constant rides last and vanishes at zero, and a literal met anywhere in a spine folds into it, so `(i + 1) + 1` and `i + 2` are one term.
#[test]
fn constants_fold_into_one_trailing_literal() {
    let i = sym(0, "i");

    let stepped = int_sum(&int_sum(&i, &int(1)), &int(1));
    assert_eq!(stepped, add(i.clone(), int(2)), "`(i + 1) + 1` is `i + 2`");

    let back = int_sum(&stepped, &int(-2));
    assert_eq!(back, i, "`i + 2 - 2` is `i`, with no trailing `0`");
}

// A monomial's factors are sorted into one order, so `i · j` and `j · i` are one term, and a literal factor is a coefficient on the left.
#[test]
fn a_product_of_symbols_has_one_factor_order() {
    let (i, j) = (sym(0, "i"), sym(1, "j"));

    let this = int_product(&i, &j);
    let that = int_product(&j, &i);
    assert_eq!(this, that, "`i · j` and `j · i` are one monomial");

    let scaled = int_product(&mul(i.clone(), int(3)), &int(2));
    assert_eq!(
        scaled,
        mul(int(6), i),
        "literal factors multiply into one left coefficient"
    );
}

// A single monomial distributes over a sum in the fold, and a product of two symbolic sums does not — that is `int_normalize`'s to do on demand.
#[test]
fn a_product_distributes_only_past_a_single_monomial() {
    let (i, j, k) = (sym(0, "i"), sym(1, "j"), sym(2, "k"));
    let sum = int_sum(&j, &k);

    let distributed = int_product(&i, &sum);
    assert_eq!(
        distributed,
        int_sum(&int_product(&i, &j), &int_product(&i, &k)),
        "`i · (j + k)` is `i · j + i · k`"
    );

    let stuck = int_product(&int_sum(&i, &j), &sum);
    assert!(
        int_has_stuck_product(&stuck),
        "`(i + j) · (j + k)` stays a stuck product until a comparison asks"
    );
}

// Cancellation moves every term to whichever side keeps its coefficient positive, so `i + 2 · j - k` against `j` becomes `i + j` against `k`, and a pair that differs in nothing becomes `0` against `0`.
#[test]
fn cancellation_splits_the_difference_by_sign() {
    let (i, j, k) = (sym(0, "i"), sym(1, "j"), sym(2, "k"));
    let left = int_sum(&int_sum(&i, &int_product(&int(2), &j)), &int_negate(&k));

    let (residual_left, residual_right) = int_cancel_common(&left, &j);
    assert_eq!(
        residual_left,
        int_sum(&i, &j),
        "`i + j` survives on the left"
    );
    assert_eq!(residual_right, k, "`k` crosses to the right");

    let (nothing_left, nothing_right) = int_cancel_common(&int_sum(&i, &j), &int_sum(&j, &i));
    assert!(int_is_zero(&nothing_left) && int_is_zero(&nothing_right));
}

// The split is taken whatever cancels, so two pairs with one difference are one pair — `0` against `j - i` and `i` against `j`, `-i` against `-j` and `j` against `i` — and a pair already split is its own split.
#[test]
fn the_split_spells_one_difference_one_way() {
    let (i, j) = (sym(0, "i"), sym(1, "j"));

    let j_less_i = int_sum(&j, &int_negate(&i));
    assert_eq!(
        int_split_by_sign(&int(0), &j_less_i),
        (i.clone(), j.clone())
    );
    assert_eq!(
        int_split_by_sign(&int_negate(&i), &int_negate(&j)),
        (j.clone(), i.clone())
    );

    let split = int_split_by_sign(&add(i.clone(), int(3)), &j);
    assert_eq!(
        int_split_by_sign(&split.0, &split.1),
        split,
        "a split pair is its own split"
    );
}

// A pair sharing no monomial and at most one nonzero constant comes back identically, not merely equivalently — the stability a stuck comparison's spelling rests on.
#[test]
fn cancellation_is_stable_when_nothing_is_shared() {
    let (i, j, k) = (sym(0, "i"), sym(1, "j"), sym(2, "k"));
    let left = add(i, add(j, int(3)));

    let (settled_left, settled_right) = int_cancel_common(&left, &k);
    assert_eq!(settled_left, left, "the left is handed back as written");
    assert_eq!(settled_right, k, "and so is the right");
}
