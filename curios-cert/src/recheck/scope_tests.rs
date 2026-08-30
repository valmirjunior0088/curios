//! A name already in scope: replaced rather than judged, and live but unchecked.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::{Globals, KernelError},
    curios_core::{Free, Level, Term},
};

use super::test_support::*;

/// The walk judges by name, and a name the handed environment already answers for is judged by nothing.
///
/// This is the rule `documentation/soundness/admission-without-judgment/judging-only-what-is-not-in-scope.md` rows. `fresh` is `!globals.in_scope(name)`, and an item every one of whose declared names the environment holds is dropped before `dependency_order` ever sees it — so what stands for that name is whatever `Kernel::seed` put there, and the module's own item is neither typed nor consulted. What keeps the collision unspellable is in another crate: mount-set disjointness, checked once in `curios-text`'s `into_core`. Nothing in this one asserts that declining is safe, and until this fixture no module in this file had been put to a non-empty environment at all — every other one is handed `Globals::default()`, where `fresh` is constantly true and the gate is inert.
///
/// So the collision is built here, to measure what the gate would cost if it ever became spellable. `shadowed` is `Nat` in the environment and `Bool` in the module, and `reader : Nat = shadowed` is judged in both configurations. Standing alone the module is refused — `reader` infers `Bool` where `Nat` was declared — and against the environment it is certified with **zero refusals**, because `reader` was typed against the environment's `shadowed` rather than the one its own module carries.
///
/// That is the sharper form of the row's claim, and it is what this fixture adds to it: the skipped item is not merely unjudged, it is *replaced*. The kernel certifies a module in which one name means the environment's definition, while erasure and every stage below it go on compiling the body the module actually holds.
///
/// The refusal standing alone is the control, and it is what proves the acceptance comes from the collision rather than from the module being harmless. Mutation-checked against the gate itself: forcing `fresh` to answer `true` fails the acceptance below with the same `Mismatch` the control asserts, so the gate is what admits and nothing beside it.
#[test]
fn a_definition_under_a_name_already_in_scope_is_replaced_rather_than_judged() {
    let module = shadowing_items();

    let alone = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);
    assert!(
        alone
            .iter()
            .any(|verdict| verdict.name == Some(reader_name())
                && matches!(verdict.error, KernelError::Mismatch { .. })),
        "the control stopped refusing: `reader : Nat` must not accept a `Bool`-typed `shadowed`: {alone:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &module,
            1_000_000,
            &already_judged(&judged_environment()),
            crate::SYNTAX
        ),
        Vec::new(),
        "the skip is what this measures: a verdict here means the walk no longer drops an item whose name the environment answers for",
    );
}

/// A registry entry resolves the same collision the other way round: the module's declaration is what every rule reads, and the one clause that would check it is what gets skipped.
///
/// `Kernel::declare_induct` runs over `module.induct_decls` unfiltered, after `Kernel::seed`, so a colliding entry *overwrites* the environment's and is live for `infer`, `eliminate` and conversion. `check_induct_decl` is then gated on `fresh` and passes over it. Two opposite resolutions of one collision inside one walk, and the registry's is the worse of the two: an unchecked declaration everything believes, rather than an unread one nothing reaches.
///
/// What that costs is measured rather than argued. A payload at `Type 5` under a family declared at `Type 0` is refused as `Oversized` standing alone and certified with **zero refusals** against an environment holding a benign `Shadowed`, so the size condition — the clause that keeps an inductive from containing the universe it lives in — is gone for exactly that entry.
///
/// **Strict positivity is not, and the row's own example of what an unjudged entry buys is the one thing it does not buy.** `positivity_vectors` runs unfiltered over `Declarations::extending`, whose lookup prefers the module's entry over the base, so `mk(f : (Shadowed) -> Shadowed)` is refused with the environment in place exactly as without it. The row reads that "a non-positive `induct` declared at `Prop` with whatever constructors it likes is a few steps from `False`"; that route is closed by a pass the gate does not reach, and the ones the paragraph above names are the open ones.
///
/// The two refusals — `Oversized` standing alone, `NotPositive` with the environment in place — are each other's controls: the first proves the acceptance in between comes from the gate, and the second proves the gate is narrower than the row claims. Mutation-checked the same way as the fixture above: forcing `fresh` to answer `true` fails the acceptance with the `Oversized` the control asserts.
#[test]
fn a_declaration_under_a_name_already_in_scope_is_live_but_unchecked() {
    let oversized = shadowing_registry(Term::type_at(Level::constant(5)));

    let alone = fixture_verdicts(&oversized, 1_000_000, &Globals::default(), crate::SYNTAX);
    assert!(
        alone
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Oversized { .. })),
        "the control stopped refusing: a `Type 5` payload exceeds a family declared at `Type 0`: {alone:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &oversized,
            1_000_000,
            &already_judged(&judged_environment()),
            crate::SYNTAX,
        ),
        Vec::new(),
        "the skip is what this measures: a verdict here means `check_induct_decl` no longer passes over a colliding entry",
    );

    let family = Term::induct_type(shadowed_family(), Vec::<Term>::new(), Vec::<Term>::new());
    let negative = shadowing_registry(Term::func_type(
        [(Free::local(911, Some("f")), family.clone())],
        family,
    ));

    let verdicts = fixture_verdicts(
        &negative,
        1_000_000,
        &already_judged(&judged_environment()),
        crate::SYNTAX,
    );
    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotPositive { .. })),
        "strict positivity stopped running over a declaration whose name the environment already holds: {verdicts:?}",
    );
}
