//! What putting a term at a type the board states answers, through the real checkers.

use {
    super::*,
    crate::*,
    curios_core::{Intrinsic, Nat},
};

/// The `Nat` literal zero: a term with a type, and no proof of anything.
fn zero() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))
}

#[test]
fn a_program_whose_tail_proves_something_else_is_refused_at_false() {
    let witness = Witness {
        what: "a proof of `True`".to_string(),
        proof: Proof::Program("/std/Bool/True/qed()".to_string()),
        expect: vec![Expected {
            by: Checker::Elaborator,
            error: Some("TypeMismatch"),
            within: None,
        }],
    };

    assert_eq!(witness.unmet(&witness.proof.answer()), None);
}

#[test]
fn a_module_is_admitted_at_a_type_its_term_has() {
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    assert!(put_module(Definitions::default(), zero(), nat_type).admitted());
}

#[test]
fn a_module_whose_term_proves_nothing_is_refused_at_false() {
    let witness = Witness {
        what: "a number".to_string(),
        proof: Proof::Module("0".to_string()),
        expect: vec![Expected {
            by: Checker::Kernel,
            error: Some("Mismatch"),
            within: None,
        }],
    };

    assert_eq!(witness.unmet(&witness.proof.answer()), None);
}

/// A construction no type of the trusted base can hold still parses, and the reader refuses it by name, which a witness expects as it expects a checker's refusal.
#[test]
fn core_text_the_reader_refuses_is_refused_by_the_name_of_its_refusal() {
    let witness = Witness {
        what: "an intrinsic Core has no variant for".to_string(),
        proof: Proof::Module("#Banana".to_string()),
        expect: vec![Expected {
            by: Checker::Reader,
            error: Some("UnknownIntrinsic"),
            within: None,
        }],
    };
    let answer = witness.proof.answer();

    assert!(!answer.kernel_asked());
    assert_eq!(witness.unmet(&answer), None);
}

/// A text that does not parse has rotted, and no expectation meets it, so it fails whatever its witness expects.
#[test]
fn core_text_that_does_not_parse_is_refused_by_the_reader_with_the_kernel_not_asked() {
    let answer = put_core_text("let");

    assert!(!answer.kernel_asked());
    assert!(matches!(answer.refusals(), [refusal] if refusal.by() == Checker::Reader));
}

#[test]
fn core_text_that_states_the_type_of_its_term_is_refused_by_the_reader() {
    let answer = put_core_text("0\n: #NatType");

    assert!(!answer.kernel_asked());
    assert!(
        matches!(answer.refusals(), [Refusal::Unread(reported)] if reported.contains("states the type"))
    );
}
