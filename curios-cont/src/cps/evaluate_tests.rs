//! Compile-time evaluation of an intrinsic, and the traps it must not fold away.

use curios_num::Natural;

use {
    crate::cps::evaluate::evaluate,
    crate::{CpsAtom, CpsIntrinsic, CpsLiteral},
};

#[test]
fn preserves_traps_and_folds_exact_u32_nat_add() {
    assert_eq!(
        evaluate(
            CpsIntrinsic::NatAdd,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(Natural::from(20u32))),
                CpsAtom::Literal(CpsLiteral::Nat(Natural::from(22u32))),
            ],
        ),
        Some(CpsLiteral::Nat(Natural::from(42u32)))
    );
    // The numeric law: the folder computes in exact u32; the i31 envelope is the backend's problem (an out-of-range literal traps at materialization).
    assert_eq!(
        evaluate(
            CpsIntrinsic::NatAdd,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(Natural::from(0x7fff_ffffu32))),
                CpsAtom::Literal(CpsLiteral::Nat(Natural::from(1u32))),
            ],
        ),
        Some(CpsLiteral::Nat(Natural::from(0x8000_0000u32)))
    );
    assert_eq!(
        evaluate(
            CpsIntrinsic::NatDiv,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(Natural::from(1u32))),
                CpsAtom::Literal(CpsLiteral::Nat(Natural::from(0u32))),
            ],
        ),
        None
    );
}
