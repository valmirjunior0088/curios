//! A foreign row inhabits its wire type and nothing more, whatever a term claims of it.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Global, Intrinsic, Term},
    curios_num::Grain,
    curios_utilities::Qualifier,
};

/// A forged ABI row cannot hand the guest an inhabitant of a proposition, because a wire signature cannot name one.
///
/// The foreign wire contract is enforced by the *grammar*, as `curios/src/tests/board`'s `a_foreign_declaration_is_confined_to_wire_types` shows: `parse_wire_type` is a closed keyword grammar, so `foreign bad : False` never parses and neither checker is asked — the rule is the parser's. As a statement about where the rule is enforced that is right. As a soundness statement it understates the position, and it leaves open the question that matters: a host call is the one place an *embedder* supplies a value the compiler never saw, so what happens if a module reaches the kernel with the rule already broken?
///
/// It cannot be broken, and the reason is representational rather than a check. `Subterm::Foreign` carries a `ForeignFunction`, whose `signature` is a `WireSignature` over `WireType`, a closed enum of wire carriers. No variant denotes a nominal type, so no row, forged by hand or not, can *say* its result is a proposition. And `infer`'s rule does not read a type off the term: it **constructs** the result through `foreign_signature` over that enum and checks each operand against its own wire type, so what a row claims about its name — the part a forgery controls — never reaches the type at all.
///
/// A null result, recorded as one: nothing here was found to be wrong. What the fixture pins is that the boundary holds from Core and not merely from the parser, which is the half no surface program can reach and no fixture covered. The row below is a forgery — a name no store would issue — and the kernel is then made to type its call.
///
/// The control is [`a_forged_foreign_row_still_inhabits_its_wire_type`], the same call at the type its signature does denote. A fixture that refused every foreign call would pin nothing about propositions.
#[test]
fn a_forged_foreign_row_cannot_inhabit_a_proposition() {
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());

    let verdicts = fixture_verdicts(
        &foreign_held_at(forged_row(), &false_type, &false_name),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Mismatch { .. })),
        "the kernel let a forged host row inhabit a proposition: {verdicts:?}",
    );
}

/// The control for the fixture above: the same forged row at the type its own signature names, wrapped in the description every host call returns. `wire_term` still reads `Nat` off the signature; `infer` wraps it, because a foreign call is an effect and an effect is an `Io`. Stating the control at the bare `Nat` would fail for a reason that has nothing to do with forgery.
#[test]
fn a_forged_foreign_row_still_inhabits_its_wire_type() {
    let false_name = Global::Authored(Qualifier::from(["False"]));

    assert_eq!(
        fixture_verdicts(
            &foreign_held_at(
                forged_row(),
                &Term::intrinsic(Intrinsic::io_type(Term::intrinsic(Intrinsic::NatType))),
                &false_name
            ),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "the boundary refused a host call at the type its own wire signature denotes",
    );
}

/// A row cannot be held at another wire type than its own: the type is built from the signature the row carries, so a claim that a row answering a `Nat` yields a list of bytes is a mismatch like any other.
#[test]
fn a_foreign_row_cannot_be_held_at_another_wire_type() {
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let claimed = Term::intrinsic(Intrinsic::io_type(Term::intrinsic(Intrinsic::ListType(
        Term::intrinsic(Intrinsic::BinType(Grain::X)),
    ))));

    let verdicts = fixture_verdicts(
        &foreign_held_at(forged_row(), &claimed, &false_name),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Mismatch { .. })),
        "the kernel let a row take another wire type than its own: {verdicts:?}",
    );
}
