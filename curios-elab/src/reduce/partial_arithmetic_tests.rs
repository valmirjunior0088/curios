//! Type-level partial arithmetic: a literal zero divisor leaves its division stuck, as a symbolic one does, never a panic: it is past the domain the division's bound states, which only a hypothesis stating the impossible reaches, and no value answers it.
//!
//! Range, by contrast, never errors here: `Nat`/`Int` are unbounded at the type level and in the running program alike, folds compute exactly, and only materialization in `curios-emit` refuses a value its envelope cannot box.

use {
    super::test_support::*,
    crate::test_support::*,
    crate::*,
    curios_core::{Intrinsic, Nat, Subterm, Term, Var},
    curios_num::{Floating, Integer},
};

#[test]
fn a_nat_division_by_a_literal_zero_stays_stuck() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    let by_zero = Term::intrinsic(Intrinsic::nat_div(
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, by_zero.clone()), Ok(by_zero));

    // A neutral dividend over a literal zero divisor is as stuck.
    let by_zero = Term::intrinsic(Intrinsic::nat_div(
        Term::free_var(&x),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, by_zero.clone()), Ok(by_zero));

    // A symbolic divisor leaves it stuck too.
    let stuck = Term::intrinsic(Intrinsic::nat_div(
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        Subterm::Var(Var::free(y)),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, stuck.clone()), Ok(stuck));
}

#[test]
fn a_nat_remainder_by_a_literal_zero_stays_stuck() {
    let mut context = context();
    let by_zero = Term::intrinsic(Intrinsic::nat_rem(
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, by_zero.clone()), Ok(by_zero));
}

#[test]
fn an_int_division_by_a_literal_zero_stays_stuck() {
    let mut context = context();
    let by_zero = Term::intrinsic(Intrinsic::int_div(
        Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
        Subterm::Intrinsic(Intrinsic::Int(Integer::from(0))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, by_zero.clone()), Ok(by_zero));

    let by_zero = Term::intrinsic(Intrinsic::int_rem(
        Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
        Subterm::Intrinsic(Intrinsic::Int(Integer::from(0))),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, by_zero.clone()), Ok(by_zero));
}

#[test]
fn int_arithmetic_is_unbounded() {
    let mut context = context();

    // Past the i31 range the type level keeps computing exactly, as the running program does with a boxed magnitude.
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

    // The narrowing answers the *exact* unbounded integer, past what any runtime carrier holds: `2^31` is past an i31, and `Int` pretends ℤ at the type level as the running program does with a boxed magnitude.
    let exact = Term::intrinsic(Intrinsic::FltToInt {
        flt: Term::intrinsic(Intrinsic::Flt(Floating::from(2147483648.0))),
        finite: qed(),
    });
    assert_eq!(
        reduce(&mut context, exact),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 31)))),
    );

    // Outside the domain `Finite` states there is no integer to answer, so the operation stays stuck rather than inventing one. A well-typed call cannot reach this — the bound excludes a NaN — and reduction does not rely on being handed only well-typed terms.
    let nan = Term::intrinsic(Intrinsic::FltToInt {
        flt: Term::intrinsic(Intrinsic::Flt(Floating::from(f64::NAN))),
        finite: qed(),
    });
    assert_eq!(reduce(&mut context, nan.clone()), Ok(nan));
}
