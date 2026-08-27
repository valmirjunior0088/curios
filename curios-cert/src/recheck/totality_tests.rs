//! Carried totality stamps: believed where they are read, judged where they are not.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    super::recheck_module_verdicts,
    crate::{Globals, KernelError},
    curios_analysis::Erased,
    curios_core::{Global, Totality},
    curios_utilities::Qualifier,
};

use super::test_support::*;

/// A totality stamp asserts the *closure* — it is what `Globals::of` seeds a later walk's non-total set from — so the cross-check must compare it against the closed verdict, not the local half.
///
/// It compared the local half. The disagreement check in `partial_definitions` fired only where `rejected || locally_partial(..)` held — a non-descending group, an inline `rec`, an exit — while the closure loop below it inserted a transitively-partial name into the set with no stamp comparison at all. So `reaches`, total in itself but stamped `Total` while mentioning the diverging `sink`, passed the very walk every account credited with refusing "a recorded verdict more generous than the kernel's own": while the hole was open, `recheck_module_verdicts` returned **zero refusals** for exactly this module. `Globals::of` then filed the lie — its non-total set is stamp-derived, so it held `sink` alone — and the compile-path walk certified a proof mentioning `reaches` with zero refusals too, which is [`a_lying_totality_stamp_is_believed_when_carried_and_refused_when_judged`]'s first half and was verified together with this. The elaborator seeds its `inherited` map from the same stamps, so a wrong one is invisible to the two-checker comparison; the transitive half of every carried stamp was the elaborator's `classify_module` conclusion, believed by the kernel and certified by nothing.
///
/// No surface program reaches it: the only stamp writer is `record_totality`, whose closure is correct, so the lie must be constructed — which is why this lives here and why nothing in the corpus could have found it. The control is [`an_honest_stamp_on_a_definition_reaching_a_partial_one_is_accepted`]: a `Partial` stamp on the same definition is a classification, not an error, and must stay accepted.
#[test]
fn a_totality_stamp_contradicted_only_by_the_closure_is_refused() {
    let verdicts = recheck_module_verdicts(
        &stamp_trial_module(Totality::Total, false),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| {
            verdict.name.as_ref() == Some(&Global::Authored(Qualifier::from(["reaches"])))
                && matches!(
                    &verdict.error,
                    KernelError::NotTotal {
                        erased: Erased::Proof,
                        reached: Some(name),
                    } if *name == Global::Authored(Qualifier::from(["sink"]))
                )
        }),
        "a `Total` stamp contradicted only by the closure over its mentions was not refused: {verdicts:?}",
    );
}

/// The control: the same two definitions honestly stamped stay accepted — partiality is a classification, and a rule refusing every mention of a partial definition would pass the witness above by breaking the language.
#[test]
fn an_honest_stamp_on_a_definition_reaching_a_partial_one_is_accepted() {
    assert_eq!(
        recheck_module_verdicts(
            &stamp_trial_module(Totality::Partial, false),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX,
        ),
        Vec::new(),
        "an honestly stamped definition reaching a partial one was refused",
    );
}

/// The pair the perimeter row asks for: a carried stamp more generous than the kernel's verdict is *believed* by the walk that carries it, and the same content judged fresh from an empty environment is refused — so the cross-check at a unit's filing is the whole of what holds the compile-path belief.
///
/// The first half is the compile path's exact shape: the environment is `Globals::of` over the lying library, its non-total set holds `sink` alone because the set is stamp-derived, and `held : Vouched` mentions only `reaches` — so the closure over the judged module never meets a partial name, (V) passes, and the walk returns no verdicts. That is by design and must stay: the belief is what keeps a compile from re-analyzing the standard library, and [`a_totality_stamp_contradicted_only_by_the_closure_is_refused`] is what makes it a belief in a verdict the kernel reached. The second half is the same three definitions in one module from an empty environment, where the closure runs over everything and the named route refuses the proof.
#[test]
fn a_lying_totality_stamp_is_believed_when_carried_and_refused_when_judged() {
    let carried = recheck_module_verdicts(
        &carried_proof_module(),
        1_000_000,
        &Globals::of(&stamp_trial_module(Totality::Total, false), 1_000),
        crate::SYNTAX,
    );
    assert_eq!(
        carried,
        Vec::new(),
        "a carried totality stamp is believed, so a walk seeded from a lying one must accept",
    );

    let judged = recheck_module_verdicts(
        &stamp_trial_module(Totality::Total, true),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );
    assert!(
        judged.iter().any(|verdict| {
            verdict.name.as_ref() == Some(&Global::Authored(Qualifier::from(["held"])))
                && matches!(
                    &verdict.error,
                    KernelError::NotTotal {
                        erased: Erased::Proof,
                        reached: Some(name),
                    } if *name == Global::Authored(Qualifier::from(["reaches"]))
                )
        }),
        "judged fresh, the same proof must be refused for reaching the mis-stamped definition: {judged:?}",
    );
}
