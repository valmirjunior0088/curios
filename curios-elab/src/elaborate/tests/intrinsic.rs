//! Raw intrinsic obligations, independently of the generated system wrappers.

use {
    crate::*,
    curios_analysis::test_support::*,
    curios_core::{Intrinsic, Nat, Term},
};

#[test]
fn a_box_is_made_holding_a_value_of_its_element() {
    let mut context = Context::with_default_budget(SYNTAX);
    let made = |value| {
        Term::intrinsic(Intrinsic::Box {
            element: Term::intrinsic(Intrinsic::NatType),
            value,
        })
    };
    let (_, type_) = elaborate(
        &mut context,
        &made(Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)))),
        Mode::Infer,
    )
    .expect("a box of a natural");
    assert_eq!(
        type_,
        Term::intrinsic(Intrinsic::IoType(Term::intrinsic(Intrinsic::BoxType(
            Term::intrinsic(Intrinsic::NatType)
        ))))
    );
    let error = elaborate(
        &mut context,
        &made(Term::intrinsic(Intrinsic::Bool(true))),
        Mode::Infer,
    )
    .expect_err("a box holds a value of its element")
    .shown(&mut context);
    assert!(format!("{error:?}").contains("Mismatch"), "{error:?}");
}
