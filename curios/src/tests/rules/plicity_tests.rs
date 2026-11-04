//! A binder's mark: part of a function type's identity, and no part of what a call, an arm or a projection reduces to.
//!
//! The rule is `documentation/design/theory/plicity-is-part-of-function-identity.md`. The kernel's halves are `curios_cert`'s `plicity_distinguishes_two_function_types`, `a_function_type_is_below_none_of_other_marks` and `a_lambda_of_other_marks_does_not_check_against_a_function_type`.
//!
//! Its kernel counterpart is `curios-cert/src/recheck/plicity_tests.rs`.

use {super::test_support::*, crate::tests::run, curios_elab::Error};

// A family computing a function type of one mark or the other, and a value of the first handed where the second is wanted. Wrapped in a call, the reference keeps its type and nothing is inserted, so what refuses it is conversion holding the two function types apart by their marks. The control hands the same value where its own type is wanted and calls it.
#[test]
fn a_function_type_is_not_aliased_across_a_mark() {
    assert_eq!(run(A_FUNCTION_TYPE_ALIASED_UNDER_ITS_OWN_MARK), b"3");
    refused(A_FUNCTION_TYPE_ALIASED_ACROSS_A_MARK).by_elaborator_in(
        "/g",
        |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "(@x: Nat) -> Nat"),
    );
}

// An equation between two functions apart in a mark cannot be stated, since `Eq`'s two sides share one type and no function type bears both marks: `(n) => n` against `(@n) => ((x) => x)(n)`, which a mark-blind η contraction would let `ground` hold. Under one mark, either one, the two are one function: η contracts the second only where its argument stands under its binder's mark, so under `@` it stays a function at its own type and its body reduces to the first's.
#[test]
fn an_equation_between_two_functions_apart_in_a_mark_is_not_stated() {
    assert_eq!(run(TWO_FUNCTIONS_EQUATED_UNDER_ONE_MARK), b"ok");
    assert_eq!(run(TWO_FUNCTIONS_EQUATED_UNDER_A_HIDDEN_MARK), b"ok");
    refused(TWO_FUNCTIONS_EQUATED_ACROSS_A_MARK).by_elaborator_in("/stated", |error| {
        matches!(error, Error::BinderPlicityMismatch { .. })
    });
}

// A hidden member stands at its own position in what a call, an arm and a projection reduce to: `f(1, @2, 3)` is `3`, `tag(x, z)` binds `z` to the last payload and `.1` over `Sized` reads `items`, so the sum is `9`. Read by mark or counted among the written positions, each reads the hidden `2` instead and the sum is `6`, which `Holds` would then make `True`.
#[test]
fn a_hidden_member_reduces_at_its_own_position() {
    assert_eq!(run(A_HIDDEN_MEMBER_REDUCES_AT_ITS_OWN_POSITION), b"9");
    refused(A_HIDDEN_MEMBER_CLAIMED_AT_ANOTHER_POSITION).by_elaborator_in(
        "/lie",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/False"),
    );
}
