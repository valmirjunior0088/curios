//! Symbolic `Nat` comparison: shared addends, commuted sums, and the bound an indexed loop walks under.

use {
    super::{Comparison, compare_nat, from_ordering, reduce_intrinsic},
    crate::{Intrinsic, Nat, ReduceError, Subterm, Term},
    curios_num::Integer,
};

use super::test_support::*;

// What the cancellation buys the comparison family: a shared addend decides nothing, so removing it lets a stuck comparison stall on the operands that actually differ. `Le(x + a, x + b)` becoming `Le(a, b)` is what makes a decided proposition usable under a binder rather than only at literals.
#[test]
fn nat_sees_through_a_shared_addend() {
    let (x, a, b) = (sym(0, "x"), sym(1, "a"), sym(2, "b"));

    let (outcome, left, right) = compare_nat(
        &mut Inert,
        add(x.clone(), a.clone()),
        add(x.clone(), b.clone()),
    )
    .expect("reduces");

    assert_eq!(
        outcome,
        Comparison::Stuck,
        "two distinct symbols decide nothing"
    );
    assert_eq!(
        occurrences(&left, &x),
        0,
        "the shared `x` is gone from the left"
    );
    assert_eq!(occurrences(&right, &x), 0, "and from the right");
    assert_eq!(occurrences(&left, &a), 1);
    assert_eq!(occurrences(&right, &b), 1);
}

// Commutativity of `+` becomes definitional for the whole comparison family, which is the larger half of what cancellation buys: nothing else in the reducer normalises the order of a sum's summands.
#[test]
fn nat_decides_a_commuted_sum_equal() {
    let (a, b) = (sym(0, "a"), sym(1, "b"));

    let (outcome, _, _) = compare_nat(
        &mut Inert,
        add(a.clone(), b.clone()),
        add(b.clone(), a.clone()),
    )
    .expect("reduces");

    assert_eq!(
        outcome,
        Comparison::Eq,
        "`a + b` and `b + a` are the same number"
    );
}

// The bound every indexed loop in the standard library needs: walking `i` up to `n` under an invariant `i + k = n` asks for `i < i + kp + 1` at each step. Before cancellation that was three lemma applications in the prelude (`add_r`, `succ_of_ind`, and the transport); the comparison now decides it outright.
#[test]
fn nat_decides_the_bound_an_indexed_loop_walks_under() {
    let (i, kp) = (sym(0, "i"), sym(1, "kp"));
    let ceiling = Nat::rebuild(1u32.into(), add(i.clone(), kp.clone()));

    let (outcome, _, _) = compare_nat(&mut Inert, i.clone(), ceiling).expect("reduces");

    assert_eq!(
        outcome,
        Comparison::Lt,
        "`i < i + kp + 1` holds for every `kp`"
    );
}

// Soundness gate: the conversions preserve the number. `Nat/to_int` folds every literal — ℕ embeds in ℤ, both unbounded here — and `Int/to_nat` folds a non-negative to the same value and reports a negative like a zero divisor, never wrapping bits.
#[test]
fn conversion_folds_preserve_the_number() {
    for n in [
        0u64,
        1,
        0x3FFF_FFFF,
        0x4000_0000,
        0x7FFF_FFFF,
        0x8000_0000,
        0xFFFF_FFFF,
        0x1_0000_0000,
    ] {
        let nat = Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
        let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::NatToInt(nat)).expect("reduces");
        assert_eq!(
            reduced,
            Subterm::Intrinsic(Intrinsic::Int(Integer::from(n))),
            "Nat/to_int changed the number on {n}",
        );
    }
    for i in [0i64, 1, 0x3FFF_FFFF, 0x7FFF_FFFF, 0x1_0000_0000] {
        let int = Term::intrinsic(Intrinsic::Int(Integer::from(i)));
        let reduced = reduce_intrinsic(
            &mut Inert,
            &Intrinsic::IntToNat {
                int,
                non_neg: qed(),
            },
        )
        .expect("reduces");
        assert_eq!(
            reduced,
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(i as u64))),
            "Int/to_nat changed the number on {i}",
        );
    }
    for i in [-1i64, -0x4000_0000, i32::MIN as i64, i64::MIN] {
        let int = Term::intrinsic(Intrinsic::Int(Integer::from(i)));
        let reduced = reduce_intrinsic(
            &mut Inert,
            &Intrinsic::IntToNat {
                int,
                non_neg: qed(),
            },
        );
        assert!(
            matches!(reduced, Err(ReduceError::IntToNatNegative { .. })),
            "Int/to_nat failed to report the negative {i}",
        );
    }
}

// Soundness gate: the structural body agrees with the host ordering on every pair of literals — the decidable closed case where the two routes into a `Comparison` (the shared-inner shortcut vs. the host `cmp`) must coincide.
#[test]
fn nat_agrees_with_literal_ordering() {
    let mut reducer = Inert;
    let samples = [0u32, 1, 2, 5, 42, 128, 255, 256, 1000];
    for &m in &samples {
        for &n in &samples {
            assert_eq!(
                compare_nat(&mut reducer, lit(m), lit(n))
                    .expect("reduces")
                    .0,
                from_ordering(m.cmp(&n)),
                "compare_nat disagreed with the literal ordering on ({m}, {n})",
            );
        }
    }
}
