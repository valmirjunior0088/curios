//! Compile-time evaluation of an intrinsic, and the traps it must not fold away.

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
                CpsAtom::Literal(CpsLiteral::Nat(20)),
                CpsAtom::Literal(CpsLiteral::Nat(22)),
            ],
        ),
        Some(CpsLiteral::Nat(42))
    );
    // The numeric law: the folder computes in exact u32; the i31 envelope is the backend's problem (an out-of-range literal traps at materialization).
    assert_eq!(
        evaluate(
            CpsIntrinsic::NatAdd,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(0x7fff_ffff)),
                CpsAtom::Literal(CpsLiteral::Nat(1)),
            ],
        ),
        Some(CpsLiteral::Nat(0x8000_0000))
    );
    assert_eq!(
        evaluate(
            CpsIntrinsic::NatDiv,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
        ),
        None
    );
}
