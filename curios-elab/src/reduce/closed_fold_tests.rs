//! The intrinsic folds over literal operands: what a closed carrier operation computes.

use {
    super::test_support::{context, qed},
    crate::*,
    curios_core::{Free, Intrinsic, Nat, ReduceError, Subterm, Term},
    curios_num::{Binary, Floating, Integer, Rounding},
};

#[test]
fn nat_to_byte_reflects_byte_to_nat() {
    let mut context = context();
    let byte_binder = context.fresh(Some("byte"));
    let byte = Term::free_var(&byte_binder);
    let term = Term::intrinsic(Intrinsic::nat_to_byte(
        Term::intrinsic(Intrinsic::ByteToNat(byte.clone())),
        qed(),
    ));

    assert_eq!(reduce(&mut context, term), Ok(byte.clone()));

    // The other direction, which the narrowing's domain is what buys: a `Byte` read back out of a `Nat` it was built from is that `Nat` again, so a bound established before the trip survives it.
    let number = Term::intrinsic(Intrinsic::Nat(Nat::new(7usize)));
    let round_trip = Term::intrinsic(Intrinsic::ByteToNat(Term::intrinsic(
        Intrinsic::nat_to_byte(number.clone(), qed()),
    )));

    assert_eq!(reduce(&mut context, round_trip), Ok(number));
}

#[test]
fn int_add_computes() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::int_add(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(2)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Int(Integer::from(3))).into())
    );
}

#[test]
fn int_eql_returns_true_or_false_bool() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::int_eql(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(4))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(4)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bool(true)).into())
    );
    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::int_eql(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(4))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(5)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bool(false)).into())
    );
}

#[test]
fn flt_folds_through_the_model() {
    let mut context = context();

    let flt = |value: f64| Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from(value))));

    // Two literals fold by calling the model, so the answer is a value rather than a normal form standing in for one.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_mul(Rounding::TiesToEven, flt(1.5), flt(2.0)))
        ),
        Ok(flt(3.0)),
    );

    // The cases the host would leave to itself, and the model does not: division by zero is a value, and `0.0 / 0.0` is the default NaN, positive, so `copysign` reads a `+`.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_div(Rounding::TiesToEven, flt(1.0), flt(0.0)))
        ),
        Ok(flt(f64::INFINITY)),
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::FltCopysign(
                flt(1.0),
                Term::intrinsic(Intrinsic::flt_div(Rounding::TiesToEven, flt(0.0), flt(0.0))),
            ))
        ),
        Ok(flt(1.0)),
    );

    // A direction folds as the model rounds in it: a tenth is inexact, so each direction lands on its own side of it, and ties away from zero parts from ties to even at a halfway integer.
    let mut tenth = |rounding| {
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_div(rounding, flt(1.0), flt(10.0))),
        )
    };
    assert_eq!(tenth(Rounding::TiesToEven), Ok(flt(0.1)));
    assert_eq!(
        tenth(Rounding::TowardNegative),
        Ok(flt(f64::from_bits(0.1f64.to_bits() - 1)))
    );
    assert_eq!(tenth(Rounding::TowardZero), tenth(Rounding::TowardNegative));
    assert_eq!(tenth(Rounding::TowardPositive), Ok(flt(0.1)));
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_round_integral(
                Rounding::TiesToAway,
                flt(2.5)
            ))
        ),
        Ok(flt(3.0)),
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_fma(
                Rounding::TiesToEven,
                flt(0.1),
                flt(10.0),
                flt(-1.0)
            ))
        ),
        Ok(flt(2f64.powi(-54))),
    );

    // A symbolic operand still rebuilds the neutral term.
    let symbolic = Term::intrinsic(Intrinsic::flt_mul(
        Rounding::TiesToEven,
        Term::free_var(&Free::local(1, Some("x"))),
        flt(2.0),
    ));
    assert_eq!(reduce(&mut context, symbolic.clone()), Ok(symbolic));
}

#[test]
fn list_get_returns_element_at_index() {
    let mut context = context();

    let list = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(20usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize))).into(),
        ],
    });

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_get(
                Subterm::Intrinsic(Intrinsic::NatType),
                list.clone(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                qed(),
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into())
    );
    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_get(
                Subterm::Intrinsic(Intrinsic::NatType),
                list,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))),
                qed(),
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize))).into())
    );
}

#[test]
fn list_get_errors_on_out_of_bounds() {
    let mut context = context();

    let list = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))).into()],
    });

    assert!(matches!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_get(
                Subterm::Intrinsic(Intrinsic::NatType),
                list,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                qed(),
            ))
            .into(),
        ),
        Err(ReduceError::ListGetOutOfBounds {
            len: 1,
            index: 1,
            ..
        })
    ));
}

#[test]
fn bytes_append_adds_byte() {
    let mut context = context();

    let bytes = Subterm::Intrinsic(Intrinsic::Bytes(Binary::from_bytes(vec![1, 2])));
    let byte: Subterm = Subterm::Intrinsic(Intrinsic::Byte(3));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::bytes_append(bytes, byte)).into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bytes(Binary::from_bytes(vec![1, 2, 3]))).into())
    );
}

#[test]
fn bytes_append_adds_the_full_byte_range() {
    let mut context = context();

    let bytes = Subterm::Intrinsic(Intrinsic::Bytes(Binary::from_bytes(vec![1, 2])));
    let byte: Subterm = Subterm::Intrinsic(Intrinsic::Byte(255));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::bytes_append(bytes, byte)).into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bytes(Binary::from_bytes(vec![1, 2, 255]))).into())
    );
}

#[test]
fn list_append_adds_element() {
    let mut context = context();

    let list = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(20usize))).into(),
        ],
    });

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_append(
                Subterm::Intrinsic(Intrinsic::NatType),
                list,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: vec![
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(20usize))).into(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize))).into(),
            ]
        })
        .into())
    );
}
