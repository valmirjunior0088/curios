//! The right shifts folded at a count past the host word: the arithmetic answer, never a neutral term whose threshold is the host's.

use {
    crate::{Intrinsic, Nat, Term},
    curios_num::{Integer, Natural},
};

use super::test_support::*;

fn count(value: u128) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(Natural::from(value))))
}

fn int(value: i32) -> Term {
    Term::intrinsic(Intrinsic::Int(Integer::from(value)))
}

// Soundness gate: a definitional equation may not depend on the host. Both shifts narrowed their count to a host index before consulting the value's width, so `Nat/shr(1, 2³²)` folded natively and stayed neutral on wasm32, and `2⁶⁴` stayed neutral everywhere, where the theory's answer is zero for `Nat` and the sign for `Int` at every count past the width. Both word boundaries are held so a narrowing through either primitive would fail here.
#[test]
fn a_right_shift_by_a_count_past_the_host_word_folds_to_the_arithmetic() {
    let past_u32 = u128::from(u32::MAX) + 1;
    let past_u64 = u128::from(u64::MAX) + 1;

    for amount in [past_u32, past_u64] {
        assert_eq!(
            fold(Term::intrinsic(Intrinsic::NatShr(lit(1), count(amount)))),
            lit(0),
            "Nat/shr(1, {amount})"
        );
        assert_eq!(
            fold(Term::intrinsic(Intrinsic::IntShr(int(-1), count(amount)))),
            int(-1),
            "Int/shr(-1, {amount})"
        );
        assert_eq!(
            fold(Term::intrinsic(Intrinsic::IntShr(int(1024), count(amount)))),
            int(0),
            "Int/shr(+1024, {amount})"
        );
    }
}
