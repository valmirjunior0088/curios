//! The chain both checkers run where two operations over neutral operands meet: what it holds one, and the near misses beside them it keeps apart.
//!
//! The chain is `curios-analysis`'s `convert_intrinsics`, and the rule is `documentation/design/soundness/conversion-is-one-relation-in-both-checkers.md`. The kernel's halves share each name in `curios_cert`'s `conversion_tests`.

use {super::test_support::*, crate::tests::run};

// A truncated difference does not commute where a sum does: at `a := 5`, `b := 3` one side is `2` and the other `0`, so a chain that read `-` as a commutative operation, pairing its operands as it pairs a sum's, would hold the two one and `Eq/subst` would carry `True` to `False`. The control states the sum, whose operands the chain pairs.
#[test]
fn a_difference_does_not_convert_with_its_operands_swapped() {
    assert_eq!(run(A_SUM_STATED_WITH_ITS_OPERANDS_SWAPPED), b"ok");
    rejected_by(
        A_DIFFERENCE_STATED_WITH_ITS_OPERANDS_SWAPPED,
        "expected: Eq(@Nat)(a - b, b - a)",
    );
}

// The decision diagram holds two connectives one only where they agree at every assignment of their atoms: `a && b` and `a || b` part at `a := true`, `b := false`, so a diagram that answered for one assignment, or read the two operations as one, would hold a false equation of `Bool`s. The control is a conjunction against its own operands swapped, which agree everywhere.
#[test]
fn a_conjunction_does_not_convert_with_the_disjunction_of_its_operands() {
    assert_eq!(run(A_CONJUNCTION_STATED_WITH_ITS_OPERANDS_SWAPPED), b"ok");
    rejected_by(
        A_CONJUNCTION_STATED_AS_THE_DISJUNCTION_OF_ITS_OPERANDS,
        "expected: Eq(@Bool)(a && b, a || b)",
    );
}
