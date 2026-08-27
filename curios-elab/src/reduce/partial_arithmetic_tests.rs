//! Type-level partial arithmetic: a literal zero divisor reports through a `ReduceError`, never a panic.
//!
//! Runtime *range* limits, by contrast, never error here: `Nat`/`Int` are unbounded at the type level, folds compute exactly, and the 31-bit narrowing is enforced downstream (`ersd`'s carriers at the erase boundary, the i31 traps in `cont` → wasm).

use super::test_support::{context, qed};
use curios_core::*;
use {
    crate::*,
    curios_num::{Floating, Integer},
};

#[test]
fn nat_div_by_zero_reports() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::nat_div(
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/div",
            span: None,
        })
    );

    // The divisor alone forces the trap: a neutral dividend still reports.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::nat_div(
                Term::free_var(&x),
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/div",
            span: None,
        })
    );

    // A symbolic divisor is not a zero divisor: the term just stays stuck.
    let stuck = Term::intrinsic(Intrinsic::nat_div(
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        Subterm::Var(Var::free(y.clone())),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, stuck.clone()), Ok(stuck));
}

#[test]
fn nat_rem_by_zero_reports() {
    let mut context = context();
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::nat_rem(
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/rem",
            span: None,
        })
    );
}

#[test]
fn int_div_by_zero_reports() {
    let mut context = context();
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_div(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(0))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Int/div",
            span: None,
        })
    );

    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_rem(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(0))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Int/rem",
            span: None,
        })
    );
}

#[test]
fn int_arithmetic_is_unbounded() {
    let mut context = context();

    // Past the runtime's i31 range the type level keeps computing exactly — the limit is the runtime's, enforced downstream, not the checker's.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_add(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from((1i64 << 30) - 1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
            )),
        ),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 30))))
    );

    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_mul(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1i64 << 30))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1i64 << 30))),
            )),
        ),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 60))))
    );
}

#[test]
fn flt_to_int_answers_the_exact_integer() {
    let mut context = context();

    // The narrowing answers the *exact* unbounded integer, past what any runtime carrier holds: `2^31` is out of `i32` range, and that refusal belongs to the erasure boundary rather than here, where `Int` pretends ℤ.
    let exact = Term::intrinsic(Intrinsic::FltToInt {
        flt: Term::intrinsic(Intrinsic::Flt(Floating::from_f32(2147483648.0))),
        finite: qed(),
    });
    assert_eq!(
        reduce(&mut context, exact),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 31)))),
    );

    // Outside the domain `Finite` states there is no integer to answer, so the operation stays stuck rather than inventing one. A well-typed call cannot reach this — the bound excludes a NaN — and reduction does not rely on being handed only well-typed terms.
    let nan = Term::intrinsic(Intrinsic::FltToInt {
        flt: Term::intrinsic(Intrinsic::Flt(Floating::from_f32(f32::NAN))),
        finite: qed(),
    });
    assert_eq!(reduce(&mut context, nan.clone()), Ok(nan));
}
