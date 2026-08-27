//! A forged foreign row inhabits its wire type and nothing more.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    super::recheck_module_verdicts,
    crate::{Globals, KernelError},
    curios_core::{Global, Intrinsic, Term},
    curios_utilities::Qualifier,
};

use super::test_support::*;

/// A forged ABI row cannot hand the guest an inhabitant of a proposition, because a wire signature cannot name one.
///
/// The foreign wire contract is the one perimeter row `curios/src/tests/perimeter.rs` records as enforced by the *grammar*: `parse_wire_type` is a closed keyword grammar, so `foreign bad : False` never parses and `both_checkers` returns `NotAsked` for both columns — which that file summarizes as "the rule is the parser's and neither checker backs it up". As a statement about where the rule is enforced that is right. As a soundness statement it understates the position, and it leaves open the question that matters: a host call is the one place an *embedder* supplies a value the compiler never saw, so what happens if a module reaches the kernel with the rule already broken?
///
/// It cannot be broken, and the reason is representational rather than a check. `Intrinsic::Foreign` carries a `ForeignFunction` whose `signature` is a `WireSignature` over `WireType` — a closed six-variant enum of `Nat`, `Int`, `Bool`, `Bytes`, `Handle` and `List`. No variant denotes a nominal type, so no row, forged by hand or not, can *say* its result is a proposition. And `infer`'s rule does not read a type off the term: it **constructs** the result from `wire_term` over that enum and checks each operand against its own wire type, so what the row claims about its namespace and name — the part a forgery controls — never reaches the type at all.
///
/// A null result, recorded as one: nothing here was found to be wrong. What the fixture pins is that the boundary holds from Core and not merely from the parser, which is the half no surface program can reach and no fixture covered. The row below is a forgery — a namespace and name no store would issue — and the kernel is then made to type its call.
///
/// The control is [`a_forged_foreign_row_still_inhabits_its_wire_type`], the same call at the type its signature does denote. A fixture that refused every foreign call would pin nothing about propositions.
#[test]
fn a_forged_foreign_row_cannot_inhabit_a_proposition() {
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let verdicts = recheck_module_verdicts(
        &forged_foreign(&false_type, &false_name),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Mismatch { .. })),
        "the kernel let a forged host row inhabit a proposition: {verdicts:?}",
    );
}

/// The control for the fixture above: the same forged row at the type its own signature names, wrapped in the description every host call now returns. `wire_term` still reads `Nat` off the signature; `infer` wraps it, because a foreign call is an effect and an effect is an `Io`. Stating the control at the bare `Nat` would fail for a reason that has nothing to do with forgery.
#[test]
fn a_forged_foreign_row_still_inhabits_its_wire_type() {
    let false_name = Global::Authored(Qualifier::from(["False"]));

    assert_eq!(
        recheck_module_verdicts(
            &forged_foreign(
                &Term::intrinsic(Intrinsic::io_type(Term::intrinsic(Intrinsic::NatType))),
                &false_name
            ),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX,
        ),
        Vec::new(),
        "the boundary refused a host call at the type its own wire signature denotes",
    );
}
