//! Index targets the walk checks rather than believes, and the scheme a registry may declare apart from its type former.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    super::recheck_module_verdicts,
    crate::{Globals, KernelError},
    curios_core::{
        Definition, DefinitionKind, Global, InductDecl, Intrinsic, Item, Level, Module, Nat,
        Telescope, Term, Totality, UniverseContext, UniverseParam,
    },
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

/// A constructor's *index target* is registry data that no judgment in the walk reads.
///
/// The item walk's sizing check walks a constructor telescope's **domains** — each must sit at or below the family's declared level — and stops there. The terminal the telescope ends in, `Family(params, indices)`, is never visited, so the index targets a constructor states reach index inversion and the arm rule without any judgment having typed them. [`check_induct_decl`](crate::check_induct_decl) says so outright: the rest "falls out of the ordinary item walk, because a declaration lowers to a `rec` group of real definitions", and for a module the elaborator built that holds — the constructor wrapper's declared type ends in that terminal, so checking the wrapper's body against it types the targets.
///
/// The kernel never confirmed the lowering exists. The module below carries the registry entry and no items at all, so nothing types the terminal, and its index target is an unsolved metavariable — precisely what `zonk_module` promises has been eliminated, in the one position the kernel's walk did not re-derive. That made the guarantee the elaborator's word rather than the kernel's, which is the dependency the two-checker split exists to remove.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for this module. It is reachable from no surface program — the elaborator builds registry and bindings from one declaration — which is why it belongs here rather than in `curios/src/tests`, and why nothing in the corpus could have found it. The diagnostic is asserted rather than bare failure, since a module this small could fail for unrelated reasons and still look guarded.
///
/// The control is [`a_registry_index_target_of_a_real_term_is_accepted`], which is the same module with the metavariable replaced by a literal: the pass must refuse an elaboration-only node, not every registry entry.
#[test]
fn a_registry_index_target_is_checked_rather_than_believed() {
    let verdicts = recheck_module_verdicts(
        &indexed_module(Term::hole(7_usize)),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotCore(_))),
        "the kernel certified a module carrying an unsolved metavariable: {verdicts:?}",
    );
}

/// The control for the fixture above: a registry entry whose index target is a real term stays accepted.
#[test]
fn a_registry_index_target_of_a_real_term_is_accepted() {
    let target = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));

    assert_eq!(
        recheck_module_verdicts(
            &indexed_module(target),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        Vec::new(),
        "the boundary pass refused a registry entry that carries nothing elaboration-only",
    );
}

/// A constructor's index target may be a *proof*, and no judgment in the walk types it.
///
/// Definitional proof irrelevance accepts without inspecting either term, and `documentation/soundness/across-the-perimeter.md` states plainly what makes that correct: every inhabitant of a proposition is total, which is (V)'s job. The premise that argument needs is that **(V) inspects every `Prop`-typed term in the accepted module** — and for the kernel's own (V), seeded from its own typing rather than from the elaborator's hook, that reduces to whether the walk types every such term.
///
/// For a time it did not, and a constructor's index target was the gap. `partial_definitions` iterates `module.items` and nothing else, and the module below has none; `derived_binder_floor` is the only other pass that reads a registry entry, and it collects free variables rather than partiality. Nor did the item walk reach these terms — the sizing check walks a constructor telescope's domains and stops at the terminal — so a target was typed by nothing, and `check_group`'s local gate, which refuses a proof-typed member whose group does not descend, never fired on a group no judgment met.
///
/// So the module below states its constructor's index at `rec p : Held = p`, a closed non-descending inhabitant of a proposition, and `recheck_module_verdicts` returned **zero refusals** for it. Irrelevance identifies that target with any other proof of `Held`, which is the identification (V) exists to prevent. Reachable from no surface program — the elaborator builds registry and bindings from one declaration and types the targets through the constructor wrappers it lowers — which is why it is built here.
///
/// What closed it is clause 9 of `check_induct_decl`, in `check_constructed`: every index target must inhabit the index telescope at the constructor's own parameters, which *checks* the target instead of scanning past it. That is the shape this comment predicted while the hole was open — partiality is not syntactic, so establishing it takes typing the registry rather than reading it — and typing `rec p : Held = p` against `Held` is what puts `check_group`'s gate in front of it. Nothing about the fixture changed; the clause arrived and the verdict followed.
///
/// The control is [`a_real_proof_in_a_registry_index_target_is_accepted`], the same declaration aimed at a genuine `qed()`: what establishes the target must refuse a diverging proof without refusing an ordinary one. The assertion pins the refusing item as well as the diagnostic, because this module declares two families and "does not descend" reaching the verdict list from either would otherwise read as a pass.
#[test]
fn a_partial_proof_in_a_registry_index_target_is_refused() {
    let family = Global::Authored(Qualifier::from(["Indexed"]));
    let verdicts = recheck_module_verdicts(
        &indexed_by_proof(true),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| {
            verdict.name.as_ref() == Some(&family)
                && verdict.error.to_string().contains("does not descend")
        }),
        "the kernel certified a non-descending proof standing as a constructor's index target: {verdicts:?}",
    );
}

/// The control for the fixture above: an ordinary proof in the same position stays accepted.
#[test]
fn a_real_proof_in_a_registry_index_target_is_accepted() {
    assert_eq!(
        recheck_module_verdicts(
            &indexed_by_proof(false),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        Vec::new(),
        "a constructor aimed at a genuine proof was refused",
    );
}

/// An inductive's registry entry and its type-former definition may declare different universe schemes here, and that is not a hole.
///
/// `curios-elab`'s `validate_universes` refuses the disagreement outright — "inductive Foo and its type-former definition have different universe contexts" — and it is one of three such agreement clauses, the others pairing a `struct` with its type-former and a `concept` with its structure entry. All three are elaborator-only, and one concept stored twice with the agreement checked in one place is the shape that produced the three findings this file already records. So it was worth constructing rather than assuming.
///
/// The kernel certifies it, in both directions of the mismatch, and the reason it may is that it never reads the agreement: it reads each scheme where that scheme is *authoritative*. An `InductType` occurrence is checked and instantiated against the registry entry's context; the definition is checked under its own. Neither is ever consulted for the other's question, so there is nothing for a disagreement to corrupt.
///
/// The second fixture is what makes that a demonstration rather than a hope. It claims `Foo` at instance `[u, u]` inhabits the sort `Foo`'s registry gives instance `[u, v]`, and the kernel refuses with a `Mismatch` of `Type u` against `Type v` — the registry's `result_sort` applied faithfully to the levels the *occurrence* supplied. A kernel that read the definition's scheme here, or conflated the two, would accept it.
///
/// The verdict, stated plainly because the skill file asks for it: this is the elaborator being strict, not the certifier being permissive. The distinction is not that the elaborator is merely cautious — the agreement is load-bearing *for the elaborator*, which reads a scheme from whichever of its two copies is in hand. It is not load-bearing for a checker that reads each in its own place. No rule was added: making the kernel enforce an elaborator construction invariant it does not consult would be the kernel believing elaborator output, which is the thing this crate exists not to do.
///
/// Both routes by which a disagreement *could* have mattered are closed already, and by this same file: an ill-scoped level (`a_level_naming_an_undeclared_universe_parameter_is_refused`) and an instance of the wrong width (`a_universe_instance_narrower_than_its_scheme_is_refused`).
#[test]
fn a_registry_and_its_type_former_may_declare_different_schemes() {
    for (label, registry, definition) in
        [("registry narrower", 1, 2), ("definition narrower", 2, 1)]
    {
        assert_eq!(
            recheck_module_verdicts(
                &disagreeing_schemes(registry, definition),
                1_000_000,
                &Globals::default(),
                crate::SYNTAX,
            ),
            Vec::new(),
            "{label}: the kernel refused a disagreement it never consults",
        );
    }
}

/// The demonstration that each scheme is nonetheless applied where it is authoritative: a claim about the family's sort that the *registry's* `result_sort` refutes.
#[test]
fn a_family_takes_the_sort_its_registry_gives_the_levels_supplied() {
    let family = Global::Authored(Qualifier::from(["Foo"]));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        // The sort is the *second* parameter, so an occurrence's second level decides it.
        result_sort: Term::type_at(Level::param(UniverseParam(1))),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    // `Foo.{u, u}` claimed at `Type v`: its sort is `Type u`, and nothing may conflate the two.
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 2,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::induct_type_at(
            family.clone(),
            vec![Level::param(UniverseParam(0)); 2],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };

    let module = Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Some(Term::tuple(Vec::<Term>::new())),
    };

    let verdicts = recheck_module_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Mismatch { .. })),
        "the registry's result sort was not applied to the levels the occurrence supplied: {verdicts:?}",
    );
}
