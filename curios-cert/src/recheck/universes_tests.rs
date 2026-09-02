//! Universe contexts, levels, and the instance an occurrence must state.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::{Globals, KernelError},
    curios_core::{
        Definition, DefinitionKind, Entrypoint, Free, Global, Intrinsic, Item, Level, Module, Nat,
        Term, Totality, UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseMetaId, UniverseParam,
    },
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

/// A declaration's universe context is *assumed* while checking it, so an unsatisfiable one is a hypothesis set that proves anything.
///
/// `Kernel::assume_universes` takes the item's own constraints as given, and `entails` answers `≤` questions under them — so a context containing `u + 1 ≤ u` lets every level relation through, and `check_instance` stops discharging anything. Deciding satisfiability runs a solver and lives in `curios-elab`; this asks whether the kernel notices regardless.
#[test]
fn an_unsatisfiable_universe_context_is_refused() {
    let contradiction = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .succ()
            .expect("level has a successor"),
        upper: Level::param(UniverseParam(0)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };
    let universe_context = UniverseContext {
        parameter_count: 1,
        constraints: vec![contradiction],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    };

    let module = Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    assert!(
        !fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX).is_empty(),
        "the kernel assumed a contradiction as a hypothesis without noticing",
    );
}

/// A constraint may only mention parameters the context declares.
///
/// A context is closed: universe polymorphism belongs to declarations, so there is no enclosing scheme whose parameters a constraint could still reference. One that names `P3` while declaring a single parameter is not a stricter hypothesis but a meaningless one — instantiation substitutes an argument vector of the declared length, and a reference past its end has nothing to become. The elaborator refuses this as an escaping level; the kernel assumes the context, so it must refuse it too.
#[test]
fn a_constraint_naming_an_undeclared_parameter_is_refused() {
    let escaping = UniverseConstraint {
        lower: Level::param(UniverseParam(3)),
        upper: Level::param(UniverseParam(0)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };
    let universe_context = UniverseContext {
        parameter_count: 1,
        constraints: vec![escaping],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    };

    let module = Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    assert!(
        !fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX).is_empty(),
        "the kernel assumed a constraint about a parameter the declaration does not have",
    );
}

/// A *level* holding an unsolved universe metavariable is elaboration residue, and nothing in the walk refused one.
///
/// `validate_universes` is where the elaborator eliminates them, and its `validate_bound_universes` half is what walks a term's own levels and rejects a metavariable. The kernel's counterpart, [`closed`](crate::closed), inspects a [`UniverseContext`]'s *constraints* — the only place in this crate that looked for a meta level at all — and never a level sitting inside a term. So `Sort::of` read `Type(?u)` and answered `Type(?u + 1)`, and the walk carried the residue rather than refusing it.
///
/// Both positions below were certified. The registry one is the shape the metavariable pass beside it already covers: registry data no judgment types. The *definition type* one is sharper, because that position is fully walked — `check_definition` asks `Sort::of` for it and then checks the body against it — so this was not a coverage gap in which terms the walk reaches. It was the level algebra having no opinion about an unsolved level at all, which is why refusing it belongs at the boundary rather than inside a judgment.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for each of the two modules. Neither is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator — which is why they are built here and why nothing in the corpus could have found this. An unsolved level is not itself a closed inhabitant of `False`; what it is, is a level every cumulativity question is then decided against, with `entails` answering about a variable that no longer has a solver behind it. The refusal is the safe direction and the one the perimeter row already claims.
///
/// The control is [`a_ground_level_in_the_same_positions_is_accepted`], the same two modules at `Type 0`: the pass must refuse residue, not every level.
#[test]
fn a_level_holding_an_unsolved_universe_metavariable_is_refused() {
    let residue = Level::meta(UniverseMetaId::from(0usize));

    for (label, module) in [
        ("a definition's declared type", level_definition(&residue)),
        ("a registry entry's result sort", level_registry(&residue)),
    ] {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::NotCore(_))),
            "{label}: the kernel certified a module carrying an unsolved universe metavariable: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: the same two positions at a ground level stay accepted.
#[test]
fn a_ground_level_in_the_same_positions_is_accepted() {
    let ground = Level::zero();

    for (label, module) in [
        ("a definition's declared type", level_definition(&ground)),
        ("a registry entry's result sort", level_registry(&ground)),
    ] {
        assert_eq!(
            fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
            Vec::new(),
            "{label}: the boundary pass refused a level that holds no residue",
        );
    }
}

/// A level naming a universe parameter its declaration does not have, in the two positions the boundary pass walks.
///
/// A declaration's universe scheme is a promise that every level it mentions is either ground or one of the parameters it declares, so a use site fully determines it. `curios-elab`'s `validate_bound_universes` is what checks that promise, and this crate had no equivalent: `universe_residue` looks for an unsolved *metavariable* in a level and nothing looks for a parameter index past the declaration's own count.
///
/// What makes that a soundness question rather than a tidiness one is what instantiation does next. `instantiate_universe_levels_scoped` substitutes the indices an instance supplies and *renumbers* the rest down by the instance's width — the correct de Bruijn shift for a well-scoped term, where an index at or above the width refers to an enclosing binder. For an ill-scoped one it is a capture: `Type.{param 1}` and `Type.{param 0}` both instantiate at `[param 0]` to the same `Type.{u}`, so two levels that were distinct become one, and the hierarchy's questions are then decided about the wrong one.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for each of the two modules below, and the renumbering was confirmed directly against `instantiate_universe_levels_scoped`. Neither module is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator, so this is a rule the elaborator holds and the certifier did not, and the certifier was the permissive one. No closed inhabitant of `False` was built from it; what is demonstrated is the capture and the missing judgment.
///
/// The control is [`a_level_naming_a_declared_universe_parameter_is_accepted`], the same two positions with the parameter actually declared. It is not decoration: the prelude is universe-polymorphic throughout, so a check that refused every parameter-naming level would reject the standard library rather than this.
#[test]
fn a_level_naming_an_undeclared_universe_parameter_is_refused() {
    let escaping = Level::param(UniverseParam(0));

    for (label, module) in [
        (
            "a definition's declared type",
            scheme_definition(&escaping, 0),
        ),
        (
            "a registry entry's result sort",
            scheme_registry(&escaping, 0),
        ),
    ] {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::UnclosedUniverses)),
            "{label}: the kernel certified a level naming a parameter the declaration does not have: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: the same level in the same positions, with the declaration declaring the parameter it names.
#[test]
fn a_level_naming_a_declared_universe_parameter_is_accepted() {
    let declared = Level::param(UniverseParam(0));

    for (label, module) in [
        (
            "a definition's declared type",
            scheme_definition(&declared, 1),
        ),
        (
            "a registry entry's result sort",
            scheme_registry(&declared, 1),
        ),
    ] {
        assert_eq!(
            fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
            Vec::new(),
            "{label}: the boundary pass refused a parameter the declaration declares",
        );
    }
}

/// A universe instance supplying fewer levels than the scheme it instantiates has parameters.
///
/// `Kernel::check_instance` discharges a scheme's *constraints* at the levels an occurrence supplies, and that is all it does. A scheme with an empty constraint set therefore accepts an instance of any width — the loop body never runs — so nothing anywhere asked whether an occurrence supplies as many levels as the declaration declares. `curios-elab`'s `validate_instance_arities` asks exactly that, of every `Instance`, `InductType`, `Variant`, `StructType` and `Struct` in the module, and this crate had no equivalent.
///
/// The consequence is the capture [`a_level_naming_an_undeclared_universe_parameter_is_refused`] records, reached from the other side. That fixture is about a declaration naming a parameter it does not have; this one is about a declaration naming a parameter it *does* have, at an occurrence that does not supply it. `instantiate_universe_levels_scoped` renumbers whatever the instance leaves unsupplied down by the instance's width, so `Levelled`'s `Type.{param 1}` at the one-level instance `[param 0]` becomes `Type.{param 0}` — and `param 0` at the use site is the *use site's* own first parameter, not the declaration's second. Two levels that were distinct are now one, and cumulativity is decided about the wrong one.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for the module below, and the renumbering was confirmed directly against `instantiate_universe_levels_scoped`. Not reachable from a surface program — `validate_universes` runs before a module leaves the elaborator — so this is again a rule the elaborator holds and the certifier did not, with the certifier the permissive one. No closed inhabitant of `False` was built from it; the capture and the missing judgment are what is demonstrated.
///
/// The control is [`a_universe_instance_of_the_declared_width_is_accepted`], the same occurrence supplying both levels. It is load-bearing: every occurrence of a universe-polymorphic declaration in the prelude carries an instance, so a width check that got the bound wrong would reject the standard library rather than this.
#[test]
fn a_universe_instance_narrower_than_its_scheme_is_refused() {
    let verdicts = fixture_verdicts(
        &instance_of_width(1),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Arity { .. })),
        "the kernel certified an occurrence that leaves a declared universe parameter unsupplied: {verdicts:?}",
    );
}

/// The control for the fixture above: the same occurrence, supplying one level per declared parameter.
#[test]
fn a_universe_instance_of_the_declared_width_is_accepted() {
    assert_eq!(
        fixture_verdicts(
            &instance_of_width(2),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        Vec::new(),
        "the boundary refused an occurrence that supplies exactly the levels its scheme declares",
    );
}

/// An occurrence of a universe-polymorphic definition that states no instance was typed at the scheme's own type, discharging nothing.
///
/// A bare `Var` denotes no particular instance, and the rest of the codebase says so twice. `Globals::value` withholds such an occurrence's *body*, because a polymorphic definition unfolds only through an `Instance` that names which instance; `curios-elab` never builds one at all, rebuilding every polymorphic occurrence as an `Instance` at freshly minted levels, and its `Frames::var_reduct` withholds the body for the reason it states — letting a raw variable unfold "would leak those bound parameters into the ambient solver". `Globals::type_of` carried no such rule: it handed the definition's stored type back whole, scheme parameters and all.
///
/// Two things follow from that, and the second is what this asserts. The scheme's parameters are *captured* — `A`'s `Type v` is read as the ambient item's `v`, so a level belonging to one scheme becomes a level the using item quantifies over, which is the collapse at the neighbouring position (see `documentation/soundness/whole-module-passes/validate_universes-inside-zonk_module.md`). And `check_instance` never runs, so the scheme's own constraints are discharged by nothing. `A` here is well formed only where `u + 1 <= v`, and the second case below is that same occurrence with its instance stated, refused for exactly that reason — so the two cases differ in nothing but whether dropping the instance also drops the rule.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for the first module, while the second — the same use of `A`, in an item with the same universe context, differing only in that the occurrence states its instance — was refused as "this instance does not satisfy its scheme's `u+1 <= v`". Reachable from no surface program, since the elaborator rebuilds every such occurrence before the kernel is asked, which is why this belongs here and why nothing in the corpus could have found it.
///
/// The control is [`an_occurrence_stating_its_universe_instance_is_still_accepted`], and it holds the two spellings the rule must keep: an instance that does discharge the constraint, and a bare occurrence of a *monomorphic* definition, which is how every definition with no universe parameters is written.
#[test]
fn a_bare_occurrence_of_a_universe_scheme_is_refused() {
    let (bare, instance) = scheme_occurrences();

    for (label, body, refusal) in [
        ("the bare occurrence", bare, "states no universe instance"),
        (
            "the same occurrence with its instance stated",
            instance,
            "does not satisfy its scheme",
        ),
    ] {
        let module = universe_scheme_module(Some((open_context(), body)));
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.error.to_string().contains(refusal)),
            "{label}: the kernel read a universe scheme at the ambient item's parameters: {verdicts:?}",
        );
    }
}

/// The control: an occurrence that *states* its instance stays accepted, and so does a bare occurrence of a monomorphic definition.
///
/// The first half is what stops the witness above being closed by over-refusal — a rule refusing every occurrence of a universe scheme would pass it and take universe polymorphism with it. The second half is the larger one, and it is why the rule reads the scheme's *width* rather than the occurrence's shape: a bare `Var` is how every definition with no universe parameters is written, so refusing bare occurrences as a class would refuse the standard library rather than this fixture. The scheme is also checked standing alone, since `A` must remain well formed for the witness's refusals to be about its use.
#[test]
fn an_occurrence_stating_its_universe_instance_is_still_accepted() {
    let (_, instance) = scheme_occurrences();

    assert_eq!(
        fixture_verdicts(
            &universe_scheme_module(None),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX,
        ),
        Vec::new(),
        "the universe scheme was refused standing alone",
    );

    assert_eq!(
        fixture_verdicts(
            &universe_scheme_module(Some((scheme_context(), instance))),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX,
        ),
        Vec::new(),
        "an occurrence discharging its scheme's constraint was refused",
    );

    let zero = Global::Authored(Qualifier::from(["zero"]));
    let monomorphic = Module {
        mounts: Vec::new(),
        items: vec![
            authored(
                &zero,
                Term::intrinsic(Intrinsic::NatType),
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            ),
            authored(
                &Global::Authored(Qualifier::from(["echo"])),
                Term::intrinsic(Intrinsic::NatType),
                Term::free_var(&Free::from(&zero)),
            ),
        ],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    assert_eq!(
        fixture_verdicts(&monomorphic, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "a bare occurrence of a monomorphic definition was refused",
    );
}

/// An arm's case equation refined an occurrence that merely *projected* onto its scrutinee, so a coercion between two types the kernel itself calls distinct was certified.
///
/// The store keyed both sides through `project_erased_universes`, which rebuilds every `Type` payload at one canonical ground level — a projection written for the Core-to-Ersd hand-off, where levels really are irrelevant, and read here as if it were a quotient by definitional equality. It is not one: `Type 0` and `Type 1` are distinct terms, and the whole universe hierarchy is the claim that they are not interchangeable. So scrutinizing `f<0>(x)` recorded the equation under the key `f(x)`, and the *unrelated* stuck term `f<1>(x)` probed to that same key and was refined to the arm's `wrap(T)` — a case value it was never shown to have.
///
/// Level 1 is what makes the two indices genuinely different rather than merely differently spelled: `f`'s body carries its parameter into a constructor payload, so `f<0>(x)` and `f<1>(x)` reduce to `wrap(Type 0)` and `wrap(Type 1)`. [`Route::Direct`] below is that fact stated as an assertion — with no arm open, the kernel refuses the very same coercion — so the arm is the whole of what admitted it. That the premise "a universe argument cannot affect computation" is false is the finding: Core has no eliminator *over* levels, but `Type u` embeds one in a term, and a payload position is where it becomes a value difference.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for the [`Route::ConstantMotive`] module at level 1, while the [`Route::Direct`] module — the same coercion with the arm removed and nothing else changed — was refused as a `Mismatch` between `Q(f<0>(x))` and `Q(f<1>(x))`. Deleting the projection from both `Scope::refine` and `Scope::refinement_of` made the arm module refuse identically, which is what identified the key rather than some other rule as the admitting one. It is a constructed module and never a `.crs`: the elaborator mints its own levels, so no surface program spells the two instances this pair needs, which is why the second checker had never been put to it.
///
/// The control is [`a_case_equation_still_refines_the_occurrence_it_scrutinized`], and it is what proves the rule was not shut by disabling refinement outright.
#[test]
fn a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance() {
    let one = Level::zero().succ().expect("level zero has a successor");

    for (label, route) in [
        ("through the arm", Route::ConstantMotive),
        ("with no arm open", Route::Direct),
    ] {
        let module = universe_refinement_module(one.clone(), route);
        let verdicts = fixture_verdicts(&module, 10_000_000, &Globals::default(), crate::SYNTAX);

        assert!(
            verdicts.iter().any(|verdict| {
                verdict.name == Some(Global::Authored(Qualifier::from(["coerce"])))
                    && matches!(verdict.error, KernelError::Mismatch { .. })
            }),
            "{label}: the kernel certified a coercion between two types it calls distinct: {verdicts:?}",
        );
    }
}

/// The control: an equation still refines the occurrence the arm actually scrutinized.
///
/// [`Route::DependentMotive`] is the shape where the equation is load-bearing rather than incidental — the arm body is `q : Q(f<0>(x))` and the motive puts it at `Q(wrap(T))`, and nothing but `f<0>(x) ≡ wrap(T)` bridges those. So a key strict enough to refuse the witness above and *also* strict enough to miss its own scrutinee would fail here, which is the brick this fixture exists to catch: the convoy pattern is what the store is for, and refusing everything would take it with the fix.
///
/// Mutation-checked while the fix was written: emptying `Scope::refinement_of` to `None` leaves the witness above passing and fails this, so the two are not testing one thing twice.
#[test]
fn a_case_equation_still_refines_the_occurrence_it_scrutinized() {
    let module = universe_refinement_module(Level::zero(), Route::DependentMotive);

    assert_eq!(
        fixture_verdicts(&module, 10_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "the arm's own case equation stopped refining its scrutinee",
    );
}

/// A crafted module can spell what no elaborated term does: an instance whose head is a `let`-bound variable, which let-reduction then substitutes with an arbitrary value. `whnf` promises totality on arbitrary terms — `infer_type` reduces a declared type before anything types it — so the walk must return a verdict rather than abort: the substitution dissolves the instance to its head's value, the same levels-inert reading the sort fixtures pin for local heads. The typed head made the shape unrepresentable everywhere else; this is the one seam substitution can still drive, and the regression it pins is the walk surviving it.
#[test]
fn a_let_bound_instance_head_dissolves_under_reduction_rather_than_aborting_the_walk() {
    let alias = Free::local(960, Some("alias"));
    let declared = Term::let_(
        &alias,
        Term::type_ground(),
        Term::intrinsic(Intrinsic::NatType),
        Term::instance_of(&alias, vec![Level::zero()]),
    );
    let module = Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["dissolved"])),
            declared,
            Term::intrinsic(Intrinsic::Nat(Nat::new(5usize))),
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "the declared type reduces through the dissolving instance to `Nat`, which `5` inhabits",
    );
}
