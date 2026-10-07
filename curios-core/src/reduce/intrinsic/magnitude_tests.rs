//! `abs` and `sign` folded: two literals' worth of grid at both signs and past the machine word, and an operand nothing leaves, which stands as written.

use {
    super::test_support::*,
    crate::{Intrinsic, Nat, Term},
    curios_num::{Integer, Natural},
};

fn abs(operand: Term) -> Term {
    Term::intrinsic(Intrinsic::IntAbs(operand))
}

fn sign(operand: Term) -> Term {
    Term::intrinsic(Intrinsic::IntSign(operand))
}

fn int(value: &Integer) -> Term {
    Term::intrinsic(Intrinsic::Int(value.clone()))
}

/// Zero, and the values either side of the i31's edge and of the machine word's at both signs, with one past 64 bits.
fn integers() -> Vec<Integer> {
    [0u32, 1, 30, 31, 63, 64, 70]
        .into_iter()
        .flat_map(|bits| {
            let magnitude = Integer::from(
                Natural::one()
                    .shl_within(&Natural::from(bits), u64::MAX)
                    .expect("a shift this small is built"),
            );
            [magnitude.clone(), -magnitude]
        })
        .chain([Integer::from(0i32)])
        .collect()
}

// A literal folds to its magnitude and to its sign as `curios-num` computes them, at both signs, past the i31 and past 64 bits: the folds a running program's `abs` and `sign` have to agree with.
#[test]
fn a_literal_folds_to_its_magnitude_and_its_sign() {
    for value in integers() {
        assert_eq!(
            fold(abs(int(&value))),
            Term::intrinsic(Intrinsic::Nat(Nat::new(value.magnitude()))),
            "abs({value})"
        );
        assert_eq!(fold(sign(int(&value))), int(&value.sign()), "sign({value})");
    }
}

// A sum of both signs is read from one end, whichever it was written from: `abs(i - j)` and `abs(j - i)` are one term, the stuck node over one operand, and `sign(i - j)` is `sign(j - i)` negated, which is what makes `sign(+0 - n) = +0 - sign(n)` hold at a difference. An operand that already leads above zero is the node over it.
//
// Mutation-checked: a reading that negates only where every coefficient is below zero leaves `abs(i - j)` and `abs(j - i)` two terms.
#[test]
fn a_sum_of_both_signs_is_read_from_one_end() {
    let (i, j) = (sym(0, "i"), sym(1, "j"));
    let minus =
        |left: &Term, right: &Term| Term::intrinsic(Intrinsic::IntSub(left.clone(), right.clone()));
    let negated = |term: Term| {
        Term::intrinsic(Intrinsic::IntSub(
            Term::intrinsic(Intrinsic::Int(Integer::from(0i32))),
            term,
        ))
    };

    assert_eq!(fold(abs(minus(&i, &j))), fold(abs(minus(&j, &i))));
    assert_eq!(
        fold(sign(minus(&i, &j))),
        fold(negated(sign(minus(&j, &i)))),
    );
    assert_eq!(fold(sign(i.clone())), sign(i.clone()));
    assert_eq!(fold(abs(i.clone())), abs(i));
}
