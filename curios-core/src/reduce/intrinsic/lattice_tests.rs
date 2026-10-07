//! `min` and `max` folded: two literals at both signs and past the machine word, a tree's leaves dropped by the plain comparison, and a node the fold changes nothing of left as written.

use {
    super::test_support::*,
    crate::{Intrinsic, Nat, Term},
    curios_num::{Integer, Natural},
};

fn nat_min(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::NatMin(left, right))
}

fn nat_max(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::NatMax(left, right))
}

/// Zero, the values either side of the i31's edge and of the machine word's, and one past 64 bits.
fn magnitudes() -> Vec<Natural> {
    [0u32, 1, 30, 31, 63, 64, 70]
        .into_iter()
        .map(|bits| {
            Natural::one()
                .shl_within(&Natural::from(bits), u64::MAX)
                .expect("a shift this small is built")
        })
        .chain([Natural::zero()])
        .collect()
}

// Two literals fold to the lesser and to the greater as `curios-num` orders them, on `Nat` and on `Int` at both signs, past the i31 and past 64 bits: the fold a running program's `min` and `max` have to agree with.
#[test]
fn two_literals_fold_to_the_lesser_and_the_greater() {
    let nat = |value: &Natural| Term::intrinsic(Intrinsic::Nat(Nat::new(value.clone())));
    let int = |value: &Integer| Term::intrinsic(Intrinsic::Int(value.clone()));
    let integers = magnitudes()
        .into_iter()
        .flat_map(|magnitude| {
            let value = Integer::from(magnitude);
            [value.clone(), -value]
        })
        .collect::<Vec<_>>();

    for left in magnitudes() {
        for right in magnitudes() {
            assert_eq!(
                fold(nat_min(nat(&left), nat(&right))),
                nat((&left).min(&right)),
                "min({left}, {right})"
            );
            assert_eq!(
                fold(nat_max(nat(&left), nat(&right))),
                nat((&left).max(&right)),
                "max({left}, {right})"
            );
        }
    }
    for left in &integers {
        for right in &integers {
            assert_eq!(
                fold(Term::intrinsic(Intrinsic::IntMin(int(left), int(right)))),
                int(left.min(right)),
                "min({left}, {right})"
            );
            assert_eq!(
                fold(Term::intrinsic(Intrinsic::IntMax(int(left), int(right)))),
                int(left.max(right)),
                "max({left}, {right})"
            );
        }
    }
}

// A node the fold drops no leaf of and hoists no offset out of stands as written: `min(y, x)` is not sorted into `min(x, y)`, and `min(min(x, y) + 1, z)` keeps its inner node under its offset. The operation's commutation and association are decided where two trees are compared, by their leaf sets, so nothing here respells a term.
//
// Mutation-checked: a fold that rebuilds every node from its leaves writes `min(min(x, y) + 1, z)` as `min(min(x + 1, y + 1), z)`.
#[test]
fn a_node_the_fold_changes_nothing_of_keeps_its_spelling() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let swapped = nat_min(y.clone(), x.clone());
    assert_eq!(fold(swapped.clone()), swapped);

    let nested = nat_min(
        Nat::rebuild(1u32.into(), nat_min(x.clone(), y.clone())),
        z.clone(),
    );
    assert_eq!(fold(nested.clone()), nested);

    let join = nat_max(nat_min(x, y), z);
    assert_eq!(fold(join.clone()), join);
}

// A leaf another answers for is dropped wherever it stands in the tree, and what is left is rebuilt in the order it was met: `min(min(x, z), x + y)` is `min(x, z)`, the third leaf at least the first, and `max(max(x, z), x + y)` is `max(z, x + y)`, the first leaf at most the third.
#[test]
fn a_leaf_another_answers_for_is_dropped_wherever_it_stands() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));
    let sum = add(x.clone(), y.clone());

    assert_eq!(
        fold(nat_min(nat_min(x.clone(), z.clone()), sum.clone())),
        nat_min(x.clone(), z.clone()),
    );
    assert_eq!(
        fold(nat_max(nat_max(x, z.clone()), sum.clone())),
        nat_max(z, sum),
    );
}
