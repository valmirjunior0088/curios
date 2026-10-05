//! Universe contexts, levels, and the instance an occurrence must state.

use {
    super::test_support::*,
    crate::{Error, Globals, Verdict},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Definition, DefinitionKind, Free, Global, Intrinsic, Item, Level, Module, Nat, Term,
        Totality, UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseMetaId, UniverseParam, Variance,
    },
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

/// A declaration's universe context is *assumed* while checking it, so an unsatisfiable one is a hypothesis set that proves anything.
///
/// `Kernel::assume_universes` takes the item's own constraints as given, and `entails` answers `≤` questions under them — so a context containing `u + 1 ≤ u` lets every level relation through, and `check_instance` stops discharging anything. Deciding satisfiability is `satisfiable`'s, a second implementation beside the elaborator's solver; this asks whether the walk refuses such a context before assuming it.
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
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert!(
        !fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
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
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert!(
        !fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
        "the kernel assumed a constraint about a parameter the declaration does not have",
    );
}

/// A *level* holding an unsolved universe metavariable is elaboration residue, which the walk refuses at its boundary.
///
/// `validate_universes` is where the elaborator eliminates them, and its `validate_bound_universes` half is what walks a term's own levels and rejects a metavariable. [`UniverseContext::is_closed`] inspects a context's *constraints*, never a level sitting inside a term, and `Sort::of` reads `Type(?u)` and answers `Type(?u + 1)` — so without a boundary pass the walk would carry the residue rather than refusing it.
///
/// Without that pass both positions below would be certified. The registry one is the shape the metavariable pass beside it covers: registry data no judgment types. The *definition type* one is sharper, because that position is fully walked — `check_definition` types it and then checks the body against it — so this is not a coverage gap in which terms the walk reaches but the level algebra having no opinion about an unsolved level at all, which is why refusing it belongs at the boundary rather than inside a judgment.
///
/// Neither is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator — which is why they are built here. An unsolved level is not itself a closed inhabitant of `False`; what it is, is a level every cumulativity question is then decided against, with `entails` answering about a variable that no longer has a solver behind it. The refusal is the safe direction and the one the board row already claims.
///
/// The control is [`a_ground_level_in_the_same_positions_is_accepted`], the same two modules at `Type 0`: the pass must refuse residue, not every level.
#[test]
fn a_level_holding_an_unsolved_universe_metavariable_is_refused() {
    let residue = Level::meta(UniverseMetaId::from(0usize));

    for (label, module) in [
        ("a definition's declared type", level_definition(&residue)),
        ("a registry entry's result sort", level_registry(&residue)),
    ] {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::NotCore(_))),
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
            fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
            Vec::new(),
            "{label}: the boundary pass refused a level that holds no residue",
        );
    }
}

/// A level naming a universe parameter its declaration does not have, in the two positions the boundary pass walks.
///
/// A declaration's universe scheme is a promise that every level it mentions is either ground or one of the parameters it declares, so a use site fully determines it. `curios-elab`'s `validate_bound_universes` checks that promise, and this crate's equivalent is `universe_escape`, beside `universe_residue`: where that one looks for an unsolved *metavariable* in a level, this one looks for a parameter index past the declaration's own count.
///
/// What makes that a soundness question rather than a tidiness one is what instantiation does next. `instantiate_universe_levels_scoped` substitutes the indices an instance supplies and *renumbers* the rest down by the instance's width — the correct de Bruijn shift for a well-scoped term, where an index at or above the width refers to an enclosing binder. For an ill-scoped one it is a capture: `Type.{param 1}` and `Type.{param 0}` both instantiate at `[param 0]` to the same `Type.{u}`, so two levels that were distinct become one, and the hierarchy's questions are then decided about the wrong one.
///
/// Neither module is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator — so only these fixtures put the certifier's copy of the rule to the test.
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
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::UnclosedUniverses)),
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
            fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
            Vec::new(),
            "{label}: the boundary pass refused a parameter the declaration declares",
        );
    }
}

/// A universe instance supplying fewer levels than the scheme it instantiates has parameters.
///
/// `Kernel::check_instance` discharges a scheme's *constraints* at the levels an occurrence supplies, and that is all it does. A scheme with an empty constraint set therefore accepts an instance of any width — the loop body never runs — so the width needs a check of its own, which `check_instance` makes first. `curios-elab`'s `validate_instance_arities` asks the same, of every `Instance`, `InductType`, `Variant`, `StructType` and `Struct` in the module.
///
/// The consequence is the capture [`a_level_naming_an_undeclared_universe_parameter_is_refused`] records, reached from the other side. That fixture is about a declaration naming a parameter it does not have; this one is about a declaration naming a parameter it *does* have, at an occurrence that does not supply it. `instantiate_universe_levels_scoped` renumbers whatever the instance leaves unsupplied down by the instance's width, so `Levelled`'s `Type.{param 1}` at the one-level instance `[param 0]` becomes `Type.{param 0}` — and `param 0` at the use site is the *use site's* own first parameter, not the declaration's second. Two levels that were distinct are now one, and cumulativity is decided about the wrong one.
///
/// Not reachable from a surface program — `validate_universes` runs before a module leaves the elaborator — so only this fixture puts the certifier's copy of the rule to the test.
///
/// The control is [`a_universe_instance_of_the_declared_width_is_accepted`], the same occurrence supplying both levels. It is load-bearing: every occurrence of a universe-polymorphic declaration in the prelude carries an instance, so a width check that got the bound wrong would reject the standard library rather than this.
#[test]
fn a_universe_instance_narrower_than_its_scheme_is_refused() {
    let verdicts = fixture_verdicts(
        &instance_of_width(1),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Arity { .. })),
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
            SYNTAX
        ),
        Vec::new(),
        "the boundary refused an occurrence that supplies exactly the levels its scheme declares",
    );
}

/// An occurrence of a universe-polymorphic definition that states no instance is refused rather than typed at the scheme's own type, which would discharge nothing.
///
/// A bare `Var` denotes no particular instance, and the rest of the codebase says so twice. `Globals::value` withholds such an occurrence's *body*, because a polymorphic definition unfolds only through an `Instance` that names which instance; `curios-elab` never builds one at all, rebuilding every polymorphic occurrence as an `Instance` at freshly minted levels, and its `Frames::var_reduct` withholds the body for the reason it states — letting a raw variable unfold "would leak those bound parameters into the ambient solver". `Kernel::type_of` applies the same rule to the type: handing the definition's stored type back whole would hand back the scheme's parameters with it.
///
/// Two things would follow from that, and the second is what this asserts. The scheme's parameters are *captured* — `A`'s `Type v` is read as the ambient item's `v`, so a level belonging to one scheme becomes a level the using item quantifies over, which is the collapse at the neighbouring position (see `documentation/design/soundness/formation/universe-instances-and-constraints.md`). And `check_instance` would never run, so the scheme's own constraints would be discharged by nothing. `A` here is well formed only where `u + 1 <= v`, and the second case below is that same occurrence with its instance stated, refused for exactly that reason — so the two cases differ in nothing but whether dropping the instance also drops the rule.
///
/// Reachable from no surface program, since the elaborator rebuilds every such occurrence before the kernel is asked, which is why this belongs here.
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
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

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
            SYNTAX,
        ),
        Vec::new(),
        "the universe scheme was refused standing alone",
    );

    assert_eq!(
        fixture_verdicts(
            &universe_scheme_module(Some((scheme_context(), instance))),
            1_000_000,
            &Globals::default(),
            SYNTAX,
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
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert_eq!(
        fixture_verdicts(&monomorphic, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "a bare occurrence of a monomorphic definition was refused",
    );
}

/// An arm's case equation does not refine an occurrence that merely *projects* onto its scrutinee, which would certify a coercion between two types the kernel itself calls distinct.
///
/// A store keying both sides through `project_erased_universes` — which rebuilds every `Type` payload at one canonical ground level, a projection written for the Core-to-Ersd hand-off where levels really are irrelevant — would read that projection as a quotient by definitional equality, and it is not one: `Type 0` and `Type 1` are distinct terms, and the whole universe hierarchy is the claim that they are not interchangeable. Scrutinizing `f<0>(x)` would record the equation under the key `f(x)`, and the *unrelated* stuck term `f<1>(x)` would probe to that same key and be refined to the arm's `wrap(T)` — a case value it was never shown to have.
///
/// Level 1 is what makes the two indices genuinely different rather than merely differently spelled: `f`'s body carries its parameter into a constructor payload, so `f<0>(x)` and `f<1>(x)` reduce to `wrap(Type 0)` and `wrap(Type 1)`. [`Route::Direct`] below is that fact stated as an assertion — with no arm open, the kernel refuses the very same coercion — so the arm is the whole of what admitted it. The premise "a universe argument cannot affect computation" is false: Core has no eliminator *over* levels, but `Type u` embeds one in a term, and a payload position is where it becomes a value difference.
///
/// It is a constructed module and never a `.crs`: the elaborator mints its own levels, so no surface program spells the two instances this pair needs.
///
/// The control is [`a_case_equation_still_refines_the_occurrence_it_scrutinized`], and it is what proves the rule is not shut by disabling refinement outright.
#[test]
fn a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance() {
    let one = Level::zero().succ().expect("level zero has a successor");

    for (label, route) in [
        ("through the arm", Route::ConstantMotive),
        ("with no arm open", Route::Direct),
    ] {
        let module = universe_refinement_module(one.clone(), route);
        let verdicts = fixture_verdicts(&module, 10_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts.iter().any(|verdict| {
                verdict.name == Some(Global::Authored(Qualifier::from(["coerce"])))
                    && matches!(verdict.error, Error::Mismatch { .. })
            }),
            "{label}: the kernel certified a coercion between two types it calls distinct: {verdicts:?}",
        );
    }
}

/// The control: an equation still refines the occurrence the arm actually scrutinized.
///
/// [`Route::DependentMotive`] is the shape where the equation is load-bearing rather than incidental — the arm body is `q : Q(f<0>(x))` and the motive puts it at `Q(wrap(T))`, and nothing but `f<0>(x) ≡ wrap(T)` bridges those. So a key strict enough to refuse the witness above and *also* strict enough to miss its own scrutinee would fail here, which is the brick this fixture exists to catch: the convoy pattern is what the store is for, and refusing everything would take it with the fix.
///
/// Mutation-checked: emptying `Scope::refinement_of` to `None` leaves the witness above passing and fails this, so the two are not testing one thing twice.
#[test]
fn a_case_equation_still_refines_the_occurrence_it_scrutinized() {
    let module = universe_refinement_module(Level::zero(), Route::DependentMotive);

    assert_eq!(
        fixture_verdicts(&module, 10_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "the arm's own case equation stopped refining its scrutinee",
    );
}

/// A crafted module can spell what no elaborated term does: an instance whose head is a `let`-bound variable, which let-reduction then substitutes with an arbitrary value. The declared type types — an instance of a local head reads levels-inert, as the sort fixtures pin for local heads — and checking `5` against it reduces it, where `whnf` promises totality on arbitrary terms, so the walk must return a verdict rather than abort: the substitution dissolves the instance to its head's value. A typed instance head makes the shape unrepresentable everywhere else; this is the one seam substitution can drive, and what it pins is the walk surviving it.
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
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "the declared type reduces through the dissolving instance to `Nat`, which `5` inhabits",
    );
}

fn level_one() -> Level {
    Level::zero().succ().expect("level zero has a successor")
}

fn refusals(module: &Module) -> Vec<Verdict> {
    fixture_verdicts(module, 1_000_000, &Globals::default(), SYNTAX)
}

/// Whether the identity put at two instances was itself refused: the walk's own verdict on the pair, apart from anything the reconciliation says of the declaration.
fn coercion_refused(verdicts: &[Verdict]) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(coercion_name()) && matches!(verdict.error, Error::Mismatch { .. })
    })
}

fn denied(verdicts: &[Verdict], family: Global, level: usize) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.error
            == Error::VarianceDenied {
                name: family,
                level,
            }
    })
}

// The level types `Wrap`'s parameter and nothing it holds, so the two instances are one type.
#[test]
fn two_instances_apart_in_an_irrelevant_level_convert() {
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let module = coercion_module(
        wrap,
        wrap_declaration(vec![Variance::Irrelevant]),
        vec![Term::intrinsic(Intrinsic::NatType)],
        Level::zero(),
        level_one(),
    );

    assert_eq!(refusals(&module), Vec::new());
}

// `Box` holds a `Type u`, carried honestly and carried with no vector at all.
#[test]
fn an_invariant_level_is_compared() {
    let boxed = Global::Authored(Qualifier::from(["Box"]));

    for variances in [vec![Variance::Invariant], Vec::new()] {
        let module = coercion_module(
            boxed,
            box_declaration(variances),
            Vec::new(),
            level_one(),
            Level::zero(),
        );

        assert!(coercion_refused(&refusals(&module)));
    }
}

// The lie that would read `box(Type 0) : Box.{1}` back at `Box.{0}`: the walk believes it and lets the coercion through, and the reconciliation refuses the module. An entry past the declaration's count is denied the same way.
#[test]
fn a_carried_irrelevance_the_recomputation_denies_refuses_the_module() {
    let boxed = Global::Authored(Qualifier::from(["Box"]));

    let believed = refusals(&coercion_module(
        boxed,
        box_declaration(vec![Variance::Irrelevant]),
        Vec::new(),
        level_one(),
        Level::zero(),
    ));
    assert!(!coercion_refused(&believed));
    assert!(denied(&believed, boxed, 0));

    let overlong = refusals(&coercion_module(
        boxed,
        box_declaration(vec![Variance::Invariant, Variance::Irrelevant]),
        Vec::new(),
        level_one(),
        level_one(),
    ));
    assert!(denied(&overlong, boxed, 1));
}

// `Wrap` carried as invariant in a level its declaration never mentions, which is what a family read before its payload was known carries: the reconciliation says nothing, and the walk compares the level it was told to.
#[test]
fn a_carried_invariance_the_recomputation_would_grant_is_compared_and_not_refused() {
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let carried = |to: Level| {
        refusals(&coercion_module(
            wrap,
            wrap_declaration(vec![Variance::Invariant]),
            vec![Term::intrinsic(Intrinsic::NatType)],
            Level::zero(),
            to,
        ))
    };

    assert_eq!(carried(Level::zero()), Vec::new());

    let apart = carried(level_one());
    assert!(coercion_refused(&apart));
    assert!(!denied(&apart, wrap, 0));
}

// `Fam`'s first level reaches no payload and no index type, and its constructor's target puts it at `Box`'s invariant position: two instances apart in it have different inhabitants at one index. Carried honestly the coercion is refused; carried as irrelevant the declaration is.
#[test]
fn a_level_only_an_index_target_mentions_is_invariant() {
    let (_, honest) = index_target_module(vec![Variance::Invariant, Variance::Invariant]);
    assert!(coercion_refused(&refusals(&honest)));

    let (family, lying) = index_target_module(vec![Variance::Irrelevant, Variance::Invariant]);
    assert!(denied(&refusals(&lying), family, 0));
}

// A concept-shaped struct: the level its method binds at is held by a field, and the level of its carrier's codomain by the parameter's type alone.
#[test]
fn a_concepts_method_level_is_compared() {
    let honest = || vec![Variance::Invariant, Variance::Irrelevant];

    let (_, codomain_apart) = method_level_module(honest(), [0, 0], [0, 1]);
    assert_eq!(refusals(&codomain_apart), Vec::new());

    let (_, method_apart) = method_level_module(honest(), [0, 1], [1, 1]);
    assert!(coercion_refused(&refusals(&method_apart)));

    let (name, lying) = method_level_module(vec![Variance::Irrelevant; 2], [0, 1], [1, 1]);
    assert!(denied(&refusals(&lying), name, 0));
}

// `Held` reaches a family a unit in scope declares, which answers from the vector it carries: that vector was held to its own declarations when its unit was certified, and the item walk reads it on the same word. So an irrelevance `Held` carries is granted over a scope's irrelevant `Wrap` and denied over its invariant `Box`, and over a scope whose `Box` carries a lie it is granted too, the lie being its own unit's to be refused for.
#[test]
fn a_family_of_a_unit_in_scope_answers_from_the_vector_it_carries() {
    let level = Level::param(UniverseParam(0));
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    let over_wrap = || {
        held_module(
            Term::induct_type_at(
                wrap,
                [level.clone()],
                [Term::intrinsic(Intrinsic::NatType)],
                Vec::<Term>::new(),
            ),
            Term::type_at(level.clone()),
            vec![Variance::Irrelevant],
        )
    };
    let over_box = || {
        held_module(
            Term::induct_type_at(
                boxed,
                [level.clone()],
                Vec::<Term>::new(),
                Vec::<Term>::new(),
            ),
            Term::type_at(level.succ().expect("a parameter has a successor")),
            vec![Variance::Irrelevant],
        )
    };
    let judged =
        |module: &Module, scope: Globals| fixture_verdicts(module, 1_000_000, &scope, SYNTAX);

    let (_, module) = over_wrap();
    assert_eq!(
        judged(
            &module,
            scope_of(wrap, wrap_declaration(vec![Variance::Irrelevant]))
        ),
        Vec::new()
    );

    let (held, module) = over_box();
    assert!(denied(
        &judged(
            &module,
            scope_of(boxed, box_declaration(vec![Variance::Invariant]))
        ),
        held,
        0
    ));
    assert_eq!(
        judged(
            &module,
            scope_of(boxed, box_declaration(vec![Variance::Irrelevant]))
        ),
        Vec::new()
    );
}

// `Uses` mentions its level only on `Alias.{u}(A)`, which unfolds to `A`: the recomputation reads the reduct and grants the irrelevance the entry carries.
#[test]
fn a_level_a_reduction_removes_is_carried_as_irrelevant() {
    let (_, module) = alias_module(vec![Variance::Irrelevant]);

    assert_eq!(refusals(&module), Vec::new());
}
