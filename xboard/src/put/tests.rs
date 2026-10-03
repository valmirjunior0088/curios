//! What putting a term at a type the board states answers, through the real checkers.

use {
    super::*,
    crate::{Witness, refused},
    curios_core::{Intrinsic, Nat},
};

/// The `Nat` literal zero: a term with a type, and no proof of anything.
fn zero() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))
}

#[test]
fn a_program_whose_tail_proves_something_else_is_refused_at_false() {
    let witness = Witness {
        what: "a proof of `True`",
        proof: Proof::Program("/std/Bool/True/qed()"),
        expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. }],
    };

    assert_eq!(witness.unmet(&witness.proof.answer()), None);
}

#[test]
fn a_module_is_admitted_at_a_type_its_term_has() {
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    assert!(put_module(Module::default(), zero(), nat_type).admitted());
}

#[test]
fn a_module_whose_term_proves_nothing_is_refused_at_false() {
    let witness = Witness {
        what: "a number",
        proof: Proof::Module(|| (Module::default(), zero())),
        expect: refused![kernel: curios_cert::Error::Mismatch { .. }],
    };

    assert_eq!(witness.unmet(&witness.proof.answer()), None);
}
