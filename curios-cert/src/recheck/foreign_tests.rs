//! A foreign row inhabits its wire type and nothing more, whatever a term claims of it.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::{Qualifier, fill},
    curios_core::{Global, Intrinsic, Term},
};

/// A forged ABI row cannot hand the guest an inhabitant of a proposition, because a wire signature cannot name one.
///
/// The foreign wire contract is enforced by the *grammar*, as `curios-surface`'s `parse::tests::foreign` shows of `top_foreign_rejects_non_wire_type`: `parse_wire_type` is a closed keyword grammar, so `foreign bad : False` never parses and neither checker is asked — the rule is the parser's. As a statement about where the rule is enforced that is right. As a soundness statement it understates the position, and it leaves open the question that matters: a host call is the one place an *embedder* supplies a value the compiler never saw, so what happens if a module reaches the kernel with the rule already broken?
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
        Term::intrinsic(Intrinsic::BytesType),
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

/// A row that does not return, and its declaration written beside it stating that a call yields the proposition it is handed rather than a description of it: applied at `False`, the declaration would be a proof of it.
const ANSWERED_OUTSIDE_IO: &str = r#"
foreign /gone() -> Never;
let /gone : (A: Prop) -> A =
    (A2: Prop) =>
        #{node = Foreign}
        /gone(@A2);
/gone(/std/Bool/False)
"#;

/// A declaration stated beside a row cannot take the row's answer out of `Io`: the call is typed by the row it carries, whatever the declaration around it states, so the declaration's body is refused at its type, and the closing term, which reaches the refused declaration, as not known to terminate.
///
/// Mutation-checked at `curios_core::foreign_produced`: with a diverging row's result handed back bare rather than in `Io`, the declaration types and no mismatch is refused under `/gone`.
#[test]
fn a_declaration_beside_a_row_cannot_take_its_answer_out_of_io() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${ANSWERED_OUTSIDE_IO}",
        PROPOSITIONS,
        ANSWERED_OUTSIDE_IO
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("gone"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "the kernel let a declaration beside a row take its answer out of `Io`: {verdicts:?}",
    );
}

/// A call of a row that does not return holding none of its operands, and its declaration written beside it: the row demands the type a call yields first.
const A_DIVERGING_CALL_OF_NO_OPERAND: &str = r#"
foreign /gone() -> Never;
let /gone : #IoType(#BoolType) =
    #{node = Foreign}
    /gone();
/std/Bool/True/qed()
"#;

/// A call's operands are counted against its row before the row's result is read: a row that does not return reads its result off the call's first operand, so a call holding none is refused by its count where reading the result first aborted the walk.
#[test]
fn a_call_of_a_row_is_counted_before_its_result_is_read() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_DIVERGING_CALL_OF_NO_OPERAND}",
        PROPOSITIONS,
        A_DIVERGING_CALL_OF_NO_OPERAND
    ));

    assert!(
        matches!(
            &verdicts[..],
            [
                Verdict {
                    name: Some(gone),
                    error: Error::Arity {
                        counted: Counted::Arguments,
                        expected: 1,
                        actual: 0
                    }
                },
                Verdict { name: None, .. },
            ] if *gone == global("gone")
        ),
        "{verdicts:?}"
    );
}
