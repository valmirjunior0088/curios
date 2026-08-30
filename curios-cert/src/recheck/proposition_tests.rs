//! What a proposition may carry, and what a proof may reach.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::{Globals, KernelError},
    curios_analysis::Erased,
    curios_core::{Atom, Entrypoint, Global, InductParam, Intrinsic, Module, Telescope, Term},
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

/// The derivation a `Prop` carrying a type made possible, as a whole module.
///
/// `Box : Prop | mk(a : Type 0)` is a legal declaration — `Prop` is impredicative, so its payload carries no size condition, and the large-elimination guard is what is supposed to keep that sound. The guard admitted `unbox` because `carries_information` reported a universe-typed payload as carrying nothing, on the reasoning that erasure deletes a type either way. Every step after that is ordinary: irrelevance makes `mk(A)` and `mk(B)` convertible at `Box`, so `refl` inhabits `Eq(Box, mk(A), mk(B))`; congruence through `unbox` carries that to `Eq(Type 0, A, B)` for *any* two types; and transport — the licensed singleton case, `refl`'s payload being pinned by its own targets — turns `()` into a proof of `False`.
///
/// While the hole was open `recheck_module_verdicts` returned zero refusals for exactly this module, with the evaluation memos on and off, and `check_induct_decl` accepted the declaration. It never compiled and never ran: `curios-elab`'s `singleton_eliminable` refused `unbox` at every surface spelling, which is what kept the certifier's copy of the rule unobserved. The fixtures in `crate::kernel::infer::eliminate::tests` pin the predicate; this pins the consequence, and it is the reason the predicate's two call sites are worth guarding separately.
#[test]
fn a_derivation_through_a_type_carrying_proposition_is_refused() {
    let verdicts = fixture_verdicts(&forgery(), 1_000_000, &Globals::default(), crate::SYNTAX);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::LargeElimination(_))),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// (V) has two routes to a refusal and only one of them has ever fired. `check_positions` first asks whether a recorded position *reaches a definition known partial* — the named route, which blames a global — and failing that asks [`super::locally_partial`], which blames nothing: a term is partial in itself when it carries a non-descending `rec` group or an `Intrinsic::ProcExit`.
///
/// Instrumented across a kernel walk of the whole prelude and every program in `curios`'s test corpus, the named route refused 9 times — 8 at a proof position, 1 at a type position — and the anonymous route refused **zero**, with no test in this crate asserting a `NotTotal` verdict at all. The reason is the one this module documents: every surface spelling that would reach it is refused during elaboration, so no module carries it here. `rec b : False = b; b`, the shape three of `curios`'s `tests::soundness` fixtures use, never arrives.
///
/// `Intrinsic::ProcExit` is the trigger that isolates this route rather than merely reaching it. A non-descending `rec` at a proof type is refused by `check_group`'s own local gate before the position walk runs, so it demonstrates that gate instead; an exit meets no gate of its own. `exit` types at `{}` — deliberately, so that nothing is forged by a term that never returns — and `Held/qed(exit(0))` is therefore well typed at a proposition while carrying a computation that does not terminate. That is (V)'s whole subject: erasure deletes the proof, the exit never fires, and the program continues holding a certificate for something no total term established.
///
/// The control is the same module with `()` in place of the exit, which must stay accepted — a rule refusing every `Prop`-typed constructor application would satisfy the assertion above and nothing else here would notice.
#[test]
fn an_exit_inside_a_proof_is_refused_with_no_definition_to_blame() {
    let verdicts = fixture_verdicts(
        &proof_carrying_unit(true),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            KernelError::NotTotal {
                erased: Erased::Proof,
                reached: None,
            }
        )),
        "the kernel certified a proof carrying an exit: {verdicts:?}",
    );
}

/// The control for the fixture above: the same proposition built from the unit value stays accepted.
#[test]
fn a_proof_carrying_the_unit_value_is_accepted() {
    assert_eq!(
        fixture_verdicts(
            &proof_carrying_unit(false),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        Vec::new(),
        "an ordinary proof at a unit-carrying proposition was refused",
    );
}

/// A declaration's domains are typed whatever sort the declaration lands in, and until they were, a `Prop`-sorted one had none of them typed at all.
///
/// `check_signature` owes clause 6 two things — that every constructor or field domain is *well-sorted*, and that a `Type`-sorted one sits at or below the family's declared level — and it ran the first inside the second. The size half is vacuous at a `Prop`-sorted result, `Prop` being impredicative, so `infer_type` sat behind `if let Some(..) = &sized` and never ran for such a declaration. Nothing else covers those positions: `check_arity` reaches an `induct`'s parameters and indices but not its constructor telescopes, a `struct` reaches no `check_arity` at all, and `check_non_informative` only *classifies* each field with `Sort::of`, the lookup that reads a claim rather than checking one.
///
/// The claim it reads is what turns the gap into a forgery. `Sort::of` classifies a stuck type-valued `match` by its **motive** — the same reading the large-elimination guard was lied to through, closed there by `check_motive` typing the motive where `infer` meets the elimination. A field type is never met by `infer`, so the lie was available again: `struct Wrap(b : Bool) : Prop { held : match b : Prop | false => Nat | true => Nat end }` classifies `Prop`, so the non-informativeness rule excuses the field, while `Wrap(true)` really carries a `Nat`.
///
/// From there the derivation is the one `check_non_informative`'s own documentation predicts. `Wrap(true)` is `Prop`-sorted, so irrelevance identifies `Wrap(true){0}` with `Wrap(true){1}`, and `refl(Wrap(true), Wrap(true){0})` therefore inhabits `Eq(Wrap(true), Wrap(true){0}, Wrap(true){1})` — the indices being compared at a `Prop`-sorted domain, which is the rule working correctly. Transporting along it under the *honest* motive `(s, t, q) => (Held(s.0)) -> Held(t.0)` yields `(Held(0)) -> Held(1)`, and `Held`'s only constructor targets `0`, so the vacuous elimination coverage licenses proves `False`.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero** refusals for exactly this module, `let boom : False` included. No surface program reaches it, and the reason is that `curios-elab` keeps the two clauses apart where this crate had fused them: `check_telescope_entries` types every declaration domain through `check_is_sort`, and `add_declaration_sizing` is a separate walk that returns early at a non-`Type` result. So this is constructed by hand, and the second opinion was worth nothing here.
///
/// Its control is [`a_proposition_carrying_a_computed_proof_is_still_accepted`], the same computed field type with arms that really are propositions: a fix refusing every `Prop`-sorted declaration, or every field type it could not read off syntactically, would fail it.
///
/// The refusal is required to name `Wrap` and not merely to exist, because every later item in the derivation is built on the forged field and would mismatch for a downstream reason once anything at all went wrong. What has to be refused is the *declaration*, at the arm the motive lied about: a `Nat` at `Type 0` checked against the `Prop` the motive claims.
#[test]
fn a_proposition_may_not_carry_a_computed_relevant_field() {
    let verdicts = fixture_verdicts(
        &computed_field_forgery(),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name.as_ref() == Some(&wrap)
                && matches!(verdict.error, KernelError::Mismatch { .. })),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The control: the same computed field type with `Prop`-sorted arms still declares, so the guard above refuses a motive that lies rather than every field a declaration has to reduce to read.
#[test]
fn a_proposition_carrying_a_computed_proof_is_still_accepted() {
    let true_name = Global::Authored(Qualifier::from(["True"]));
    let true_type = Term::induct_type(true_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(
        true_name.clone(),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    );
    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam {
            telescope: Telescope::done(Vec::new()),
            plicities: Vec::new(),
        },
    )]);

    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));
    let module = Module {
        mounts: Vec::new(),
        items: vec![wrapped_at_true(&wrap_name, qed)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(true_name, true_decl)]),
        struct_decls: BTreeMap::from([(
            wrap_name,
            computed_field_wrapper(true_type.clone(), true_type),
        )]),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new()
    );
}
