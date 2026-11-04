//! Universe contexts, levels, and the instance an occurrence must state.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        Definition, DefinitionKind, Definitions, Entrypoint, Free, Global, InductDecl, Intrinsic,
        Item, Level, Nat, Program, Quality, QualityMetaId, QualityParam, RecGroup, RecItem,
        ReduceError, StructDecl, Telescope, Term, UniverseConstraint, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseError, UniverseMetaId, UniverseParam,
        Universes, test_support::*,
    },
    curios_surface::Spelling,
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
        quality_count: 0,
        constraints: vec![contradiction],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    };

    let definitions = Definitions {
        items: vec![Item::Let(Box::new(definition))].into(),
    };

    assert!(
        !fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
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
        quality_count: 0,
        constraints: vec![escaping],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    };

    let definitions = Definitions {
        items: vec![Item::Let(Box::new(definition))].into(),
    };

    assert!(
        !fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX).is_empty(),
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

    for (label, definitions) in [
        ("a definition's declared type", level_definition(&residue)),
        ("a registry entry's result sort", level_registry(&residue)),
    ] {
        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::NotCore(_))),
            "{label}: the kernel certified a module carrying an unsolved universe metavariable: {verdicts:?}",
        );
    }
}

/// One position a universe context is assumed from: what to call it, the declaration a refusal there is named against, and how to build the program that carries the context.
type AssumedFrom<'a> = (
    &'a str,
    Option<Global>,
    &'a dyn Fn(Vec<UniverseConstraint>) -> Program,
);

/// Every position a universe context is assumed from refuses a set with no instance in the naturals.
///
/// `curios-cert/src/satisfy.rs` states what an unsatisfiable set buys once assumed: `entails` starts proving whatever it is asked, `check_instance` stops discharging anything, and the universe discipline that keeps the paradox out stops applying. So the clause has to cover every position, not the one it was written for. [`an_unsatisfiable_universe_context_is_refused`] above covers a definition's; these are the rest, each carrying the loop `param 0 + 1 ≤ param 0` and nothing else wrong, and each refused by name:
///
/// - an `induct` registry entry's, assumed by `check_induct_decl` through `at_every_sort`;
/// - a `struct` registry entry's, assumed by `check_struct_decl` the same way;
/// - a top-level `rec` group's, assumed by `check_rec_group`, which is the item's and not the group's: a group carries no context of its own, so a `rec` inside a term is no position at all and the entrypoint's closing term states none either.
/// - a definition's that leaves a sort to its caller, where `at_every_sort` assumes it once per assignment.
///
/// The control is each fixture with an empty constraint set, which stands.
#[test]
fn every_position_a_universe_context_is_assumed_from_refuses_a_loop() {
    let loop_ = || {
        vec![UniverseConstraint {
            lower: Level::param(UniverseParam(0))
                .succ()
                .expect("a parameter has a successor"),
            upper: Level::param(UniverseParam(0)),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }]
    };
    let context = |constraints: Vec<UniverseConstraint>, qualities: usize| UniverseContext {
        parameter_count: 1,
        quality_count: qualities,
        constraints,
    };
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let record = Global::Authored(Qualifier::from(["Record"]));
    let held = Global::Authored(Qualifier::from(["held"]));
    let definition = |universe_context: UniverseContext, type_: Term, body: Term| Definition {
        name: held,
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_,
        body,
    };
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let program = |items: Vec<Item>, body: Term| Program {
        definitions: Definitions {
            items: items.into(),
        },
        entrypoint: Entrypoint {
            body,
            type_: Some(nat()),
        },
    };
    let declared = |items: Vec<Item>,
                    inducts: Vec<(Global, InductDecl)>,
                    structs: Vec<(Global, StructDecl)>| Program {
        definitions: declaring(program(items, zero()).definitions, inducts, structs, []),
        entrypoint: Entrypoint {
            body: zero(),
            type_: Some(nat()),
        },
    };

    let entry_sites: [AssumedFrom<'_>; 4] = [
        ("an `induct` registry entry", Some(family), &|constraints| {
            declared(
                Vec::new(),
                vec![(
                    family,
                    InductDecl {
                        universe_context: context(constraints, 0),
                        arity: Telescope::done(Telescope::done(())),
                        constructors: Vec::new().into(),
                        result_sort: Term::type_ground(),
                        module: Qualifier::default(),
                        rep_public: true,
                        polarities: Vec::new(),
                        variances: Vec::new(),
                    },
                )],
                Vec::new(),
            )
        }),
        ("a `struct` registry entry", Some(record), &|constraints| {
            declared(
                Vec::new(),
                Vec::new(),
                vec![(
                    record,
                    StructDecl {
                        universe_context: context(constraints, 0),
                        arity: Telescope::done(Telescope::done(())),
                        result_sort: Term::type_ground(),
                        module: Qualifier::default(),
                        rep_public: true,
                        polarities: Vec::new(),
                        variances: Vec::new(),
                    },
                )],
            )
        }),
        ("a top-level `rec` group", Some(held), &|constraints| {
            program(
                vec![Item::Rec(RecItem::new(vec![(
                    fresh(None),
                    definition(context(constraints, 0), nat(), zero()),
                )]))],
                zero(),
            )
        }),
        (
            "a definition leaving a sort open",
            Some(held),
            &|constraints| {
                program(
                    vec![Item::Let(Box::new(definition(
                        context(constraints, 1),
                        nat(),
                        zero(),
                    )))],
                    zero(),
                )
            },
        ),
    ];

    for (label, refused, build) in entry_sites {
        let verdicts = fixture_verdicts(&build(loop_()), 1_000_000, &Globals::default(), SYNTAX);
        assert!(
            verdicts.iter().any(|verdict| verdict.name == refused
                && matches!(verdict.error, Error::UnsatisfiableUniverses)),
            "{label}: the walk assumed a set with no instance in the naturals: {verdicts:?}",
        );

        assert_eq!(
            fixture_verdicts(&build(Vec::new()), 1_000_000, &Globals::default(), SYNTAX),
            Vec::new(),
            "{label}: the same fixture stating no constraint was refused",
        );
    }
}

/// A sort whose quality is open, in the two positions the boundary pass walks: a metavariable elaboration left, and a parameter no context binds.
///
/// Every rule that reads a sort reads which of `Prop` and `Type` it is: irrelevance, the large-elimination guard, the totality an erased position owes. One that states neither has no answer any of them may take, so the kernel judges a term only once each of its sorts states one. Neither module is reachable from a surface program, so only these fixtures put the refusal to the test; the control is [`a_ground_level_in_the_same_positions_is_accepted`].
#[test]
fn a_sort_stating_neither_prop_nor_type_is_refused() {
    for (quality, open) in [
        ("a quality metavariable", Quality::Meta(QualityMetaId(0))),
        ("a quality parameter", Quality::Param(QualityParam(0))),
    ] {
        let sort = || Term::sort_at(open, Level::zero());

        for (label, definitions) in [
            ("a definition's declared type", sorted_definition(sort())),
            ("a registry entry's result sort", sorted_registry(sort())),
        ] {
            let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);

            assert!(
                verdicts
                    .iter()
                    .any(|verdict| matches!(verdict.error, Error::OpenSort(_))),
                "{label}: the kernel certified a module carrying {quality}: {verdicts:?}",
            );
        }
    }
}

/// The control for the fixture above: the same two positions at a ground level stay accepted.
#[test]
fn a_ground_level_in_the_same_positions_is_accepted() {
    let ground = Level::zero();

    for (label, definitions) in [
        ("a definition's declared type", level_definition(&ground)),
        ("a registry entry's result sort", level_registry(&ground)),
    ] {
        assert_eq!(
            fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX),
            Vec::new(),
            "{label}: the boundary pass refused a level that holds no residue",
        );
    }
}

/// A level naming a universe parameter its declaration does not have, in the two positions the boundary pass walks.
///
/// A declaration's universe scheme is a promise that every level it mentions is either ground or one of the parameters it declares, so a use site fully determines it. `curios-elab`'s `validate_bound_universes` checks that promise, and this crate's equivalent is `universe_escape`, beside `universe_residue`: where that one looks for an unsolved *metavariable* in a level, this one looks for a parameter index past the declaration's own count.
///
/// What makes that a soundness question rather than a tidiness one is what instantiation does next. `instantiate_universes` substitutes the indices an instance supplies and leaves any other as it stands, so an index past the scheme reaches the use site as the use site's own parameter of that index: a capture, a level of the reader's standing where the declaration meant one of its own, and the hierarchy's questions are then decided about the wrong one.
///
/// Neither module is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator — so only these fixtures put the certifier's copy of the rule to the test.
///
/// The control is [`a_level_naming_a_declared_universe_parameter_is_accepted`], the same two positions with the parameter actually declared. It is not decoration: the prelude is universe-polymorphic throughout, so a check that refused every parameter-naming level would reject the standard library rather than this.
#[test]
fn a_level_naming_an_undeclared_universe_parameter_is_refused() {
    let escaping = Level::param(UniverseParam(0));

    for (label, definitions) in [
        (
            "a definition's declared type",
            scheme_definition(&escaping, 0),
        ),
        (
            "a registry entry's result sort",
            scheme_registry(&escaping, 0),
        ),
    ] {
        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);

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

    for (label, definitions) in [
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
            fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX),
            Vec::new(),
            "{label}: the boundary pass refused a parameter the declaration declares",
        );
    }
}

/// A universe instance supplying fewer levels than the scheme it instantiates has parameters.
///
/// `Kernel::check_instance` discharges a scheme's *constraints* at the levels an occurrence supplies, and that is all it does. A scheme with an empty constraint set therefore accepts an instance of any width — the loop body never runs — so the width needs a check of its own, which `check_instance` makes first. `curios-elab`'s `validate_instance_arities` asks the same, of every `Instance`, `InductType`, `Variant`, `StructType` and `Struct` in the module.
///
/// The consequence is the capture [`a_level_naming_an_undeclared_universe_parameter_is_refused`] records, reached from the other side. That fixture is about a declaration naming a parameter it does not have; this one is about a declaration naming a parameter it *does* have, at an occurrence that does not supply it. `instantiate_universes` leaves whatever the instance does not supply as it stands, so `Levelled`'s `Type.{param 1}` at the one-level instance `[param 0]` stays `Type.{param 1}` — and `param 1` at the use site is the *use site's* own second parameter, not the declaration's. A level the declaration meant is now one of the reader's, and cumulativity is decided about the wrong one.
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

/// An instance at the top of the offset range, read through a scheme that adds to it, is refused as an overflow rather than wrapped or saturated.
///
/// A level is `max(constant, head + offset, …)` over `u32` offsets, and an occurrence's sort or type is its scheme's read at the levels it states, so a scheme's `u + 1` at `u = u32::MAX` asks for a successor no offset holds. Saturated, the successor would be its own level and `Type.{MAX} : Type.{MAX}` would be the collapse the hierarchy exists to refuse; wrapped, it would be `Type.{0}`, below what it types. Both are refused by `Level::checked_add` as `OffsetOverflow`, in a family's sort read at an instance and in a definition's type read at one. Reachable from no surface program, which spells no level.
///
/// The control is [`an_instance_below_the_top_of_the_offset_range_is_accepted`], the same two occurrences two below the top, where the definition's own type still has a sort.
#[test]
fn an_instance_at_the_top_of_the_offset_range_is_refused_as_an_overflow() {
    for (label, definitions) in [
        ("a family's sort", family_instance_at(u32::MAX, 0)),
        ("a definition's type", scheme_instance_at(u32::MAX, 0)),
    ] {
        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            matches!(
                verdicts.as_slice(),
                [Verdict {
                    name: Some(name),
                    error: Error::Reduce(ReduceError::Universe(UniverseError::OffsetOverflow)),
                }] if *name == Global::Authored(Qualifier::from(["held"]))
            ),
            "{label}: the kernel read a successor past the top of the offset range: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: the same occurrences two below the top, where every successor the walk takes has an offset to land on.
#[test]
fn an_instance_below_the_top_of_the_offset_range_is_accepted() {
    for (label, definitions) in [
        (
            "a family's sort",
            family_instance_at(u32::MAX - 2, u32::MAX - 1),
        ),
        (
            "a definition's type",
            scheme_instance_at(u32::MAX - 2, u32::MAX - 1),
        ),
    ] {
        assert_eq!(
            fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX),
            Vec::new(),
            "{label}: the kernel refused an instance whose successors all fit",
        );
    }
}

/// An occurrence of a universe-polymorphic definition that states no instance is refused rather than typed at the scheme's own type, which would discharge nothing.
///
/// An occurrence stating no level of a universe scheme denotes none of its instances. `curios-elab` never builds one, writing every reference to a declaration at the instance it chooses, and the kernel neither unfolds one — `Kernel::value_at` withholds the body of an instance short of its scheme, as `curios-elab`'s `Frames::value_at` does, for the reason that one states: unfolding it "would leak the scheme's parameters" — nor types it, refusing its instance's arity: handing the stored type back whole would hand back the scheme's parameters with it.
///
/// Two things would follow from that, and the second is what this asserts. The scheme's parameters are *captured* — `A`'s `Type v` is read as the ambient item's `v`, so a level belonging to one scheme becomes a level the using item quantifies over, which is the collapse at the neighbouring position. And `check_instance` would never run, so the scheme's own constraints would be discharged by nothing. `A` here is well formed only where `u + 1 <= v`, and the second case below is that same occurrence with its instance stated, refused for exactly that reason — so the two cases differ in nothing but whether dropping the instance also drops the rule.
///
/// Reachable from no surface program, since the elaborator states every occurrence's instance before the kernel is asked, which is why this belongs here.
///
/// The control is [`an_occurrence_stating_its_universe_instance_is_still_accepted`], and it holds the two spellings the rule must keep: an instance that does discharge the constraint, and a bare occurrence of a *monomorphic* definition, which is how every definition with no universe parameters is written.
#[test]
fn a_bare_occurrence_of_a_universe_scheme_is_refused() {
    let (bare, instance) = scheme_occurrences();

    for (label, body, refusal) in [
        (
            "the bare occurrence",
            bare,
            "universe levels: expected 2, found 0",
        ),
        (
            "the same occurrence with its instance stated",
            instance,
            "does not satisfy its scheme",
        ),
    ] {
        let definitions = universe_scheme_definitions(Some((open_context(), body)));
        let verdicts = fixture_verdicts(&definitions, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts.iter().any(|verdict| verdict
                .error
                .rendered(&Spelling::default())
                .contains(refusal)),
            "{label}: the kernel read a universe scheme at the ambient item's parameters: {verdicts:?}",
        );
    }
}

/// The control: an occurrence that *states* its instance stays accepted, and so does a bare occurrence of a monomorphic definition.
///
/// The first half is what stops the witness above being closed by over-refusal — a rule refusing every occurrence of a universe scheme would pass it and take universe polymorphism with it. The second half is the larger one, and it is why the rule reads the scheme's *width* rather than the occurrence's shape: an occurrence at no levels is how every definition with no universe parameters is written, so refusing bare occurrences as a class would refuse the standard library rather than this fixture. The scheme is also checked standing alone, since `A` must remain well formed for the witness's refusals to be about its use.
#[test]
fn an_occurrence_stating_its_universe_instance_is_still_accepted() {
    let (_, instance) = scheme_occurrences();

    assert_eq!(
        fixture_verdicts(
            &universe_scheme_definitions(None),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "the universe scheme was refused standing alone",
    );

    assert_eq!(
        fixture_verdicts(
            &universe_scheme_definitions(Some((scheme_context(), instance))),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "an occurrence discharging its scheme's constraint was refused",
    );

    let zero = Global::Authored(Qualifier::from(["zero"]));
    let monomorphic = Definitions {
        items: vec![
            authored(
                &zero,
                Term::intrinsic(Intrinsic::NatType),
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            ),
            authored(
                &Global::Authored(Qualifier::from(["echo"])),
                Term::intrinsic(Intrinsic::NatType),
                Term::instance(zero, Universes::default()),
            ),
        ]
        .into(),
    };

    assert_eq!(
        fixture_verdicts(&monomorphic, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "a bare occurrence of a monomorphic definition was refused",
    );
}

/// An arm's case equation does not refine an occurrence that merely *projects* onto its scrutinee, which would certify a coercion between two types the kernel itself calls distinct.
///
/// A store keying both sides through `project_erased_universes` — which rebuilds every `Type` payload at one canonical ground level, a projection written for the Core-to-ANF hand-off where levels really are irrelevant — would read that projection as a quotient by definitional equality, and it is not one: `Type 0` and `Type 1` are distinct terms, and the whole universe hierarchy is the claim that they are not interchangeable. Scrutinizing `f<0>(x)` would record the equation under the key `f(x)`, and the *unrelated* stuck term `f<1>(x)` would probe to that same key and be refined to the arm's `wrap(T)` — a case value it was never shown to have.
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
        let definitions = universe_refinement_definitions(one.clone(), route);
        let verdicts = fixture_verdicts(&definitions, 10_000_000, &Globals::default(), SYNTAX);

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
    let definitions = universe_refinement_definitions(Level::zero(), Route::DependentMotive);

    assert_eq!(
        fixture_verdicts(&definitions, 10_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "the arm's own case equation stopped refining its scrutinee",
    );
}

/// A definition whose declared type is a bare `rec` projection is read at the levels its own instance states.
///
/// `carrier@{p} : rec m : Type (p + 2) = Type (p + 1) end.0 = Type p`: the declared type is a projection whose member's body is `Type (p + 1)`, so a reader stating `carrier@{5}` is owed that type at `p := 5`, which forces `Type 6`. Claiming it at `Type 1` is a mismatch.
///
/// This is site one of the four `xboard/src/formation/a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers.rs` was admitted through. While a group bound universe parameters of its own, `p` inside the member scopes was the *group's* parameter, an instance of the declaration substituted nothing there, and the projection reduced under the reader's own `p` — `p ≤ 0`, stated below — so `Type 1` was admitted with no verdict. That half is unrepresentable here: it needed a universe context on the group, which [`RecGroup`] no longer carries. What stands is the refusal, with the honest claim as its control.
#[test]
fn a_declared_type_that_is_a_bare_projection_is_read_at_its_instances_levels() {
    let p = Level::param(UniverseParam(0));
    let plus_one = p.clone().succ().expect("a parameter has a successor");
    let plus_two = plus_one.clone().succ().expect("a level has a successor");
    let carrier = Global::Authored(Qualifier::from(["carrier"]));
    let reads = Global::Authored(Qualifier::from(["reads"]));
    let m = Free::local(760, Some("m"));
    let projection = Term::rec_proj(
        RecGroup::closing(vec![(m, Term::type_at(plus_two), Term::type_at(plus_one))]),
        0,
    );
    let definition = |name: Global, universe_context: UniverseContext, type_: Term, body: Term| {
        Item::Let(Box::new(Definition {
            name,
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_,
            body,
        }))
    };
    // The reader's own parameter 0 is pinned at zero: the level the shadowed reading handed the projection, and the one the instance's `5` must now override.
    let readers_context = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: p.clone(),
            upper: Level::zero(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let definitions = |claimed: Level| Definitions {
        items: vec![
            definition(
                carrier,
                UniverseContext {
                    parameter_count: 1,
                    quality_count: 0,
                    constraints: Vec::new(),
                },
                projection.clone(),
                Term::type_at(p.clone()),
            ),
            definition(
                reads,
                readers_context.clone(),
                Term::type_at(claimed),
                Term::instance(carrier, vec![Level::constant(5)]),
            ),
        ]
        .into(),
    };

    let verdicts = fixture_verdicts(
        &definitions(Level::constant(1)),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(reads)
            && matches!(verdict.error, Error::Mismatch { .. })),
        "a member's type was read at the reader's levels rather than at the instance's: {verdicts:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &definitions(Level::constant(6)),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "the same reader claiming the level the instance states was refused",
    );
}

/// A top-level `rec` group's member type is read at the levels the occurrence's instance states.
///
/// `rec carrier@{p} : Type (p + 1) = Type p end` with an occurrence stating `carrier@{5}` is owed `Type 6`, so claiming it at `Type 1` is a mismatch.
///
/// This is the kernel's half of site two of `xboard/src/formation/a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers.rs`, and unlike the three fixtures beside it this one discriminates nothing: `infer`'s `Rec` arm binds each member at `member_type` positionally, under the item's own constraints, which is the reading the fix keeps — the module below is refused the same way before the group stopped binding universes and after. What carried the cause at this site was the *elaborator's* export, `Places::member`, which paired `group.member_type(index)` with a term carrying the group and so handed an occurrence a type with the group's binder still over it; no surface syntax states a universe argument, so that spelling was reachable only from inside elaboration. It is closed by the group carrying no context to open. This fixture is here to hold the kernel's positional reading still, since the fix's correctness rests on it being the only one.
#[test]
fn an_exported_member_type_is_read_at_the_occurrences_instance() {
    let p = Level::param(UniverseParam(0));
    let plus_one = p.clone().succ().expect("a parameter has a successor");
    let carrier = Global::Authored(Qualifier::from(["carrier"]));
    let reads = Global::Authored(Qualifier::from(["reads"]));
    let carriers_context = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };
    let readers_context = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: p.clone(),
            upper: Level::zero(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let definition =
        |name: Global, universe_context: UniverseContext, type_: Term, body: Term| Definition {
            name,
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_,
            body,
        };
    let definitions = |claimed: Level| Definitions {
        items: vec![
            Item::Rec(RecItem::new(vec![(
                fresh(None),
                definition(
                    carrier,
                    carriers_context.clone(),
                    Term::type_at(plus_one.clone()),
                    Term::type_at(p.clone()),
                ),
            )])),
            Item::Let(Box::new(definition(
                reads,
                readers_context.clone(),
                Term::type_at(claimed),
                Term::instance(carrier, vec![Level::constant(5)]),
            ))),
        ]
        .into(),
    };

    let verdicts = fixture_verdicts(
        &definitions(Level::constant(1)),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(reads)
            && matches!(verdict.error, Error::Mismatch { .. })),
        "an exported member type was read at the reader's levels: {verdicts:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &definitions(Level::constant(6)),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "the same occurrence claiming the level its instance states was refused",
    );
}

/// A definition's stored body is read at the levels its own instance states, the group it carries included.
///
/// `carrier@{p} : Type (p + 2) = rec m : Type (p + 2) = Type (p + 1) end.0` stores the projection as its *value*. A reader whose declared type is `carrier@{5}` forces that value — the definition unfolds, the projection reduces — and is owed `Type 6`, so its body must be a type below level six: `Type 5` inhabits it and `Type 6` does not.
///
/// This is site four. The findings entry this fixture replaces held that the route stopped here, on the ground that shadowing leaves the group's levels free in the reading item; that is the attack rather than the stopper, because the reader's own constraints are the reader's to choose. While the group bound its own, this body reduced to `Type (p + 1)` at the reader's `p ≤ 0` — `Type 1` — so `Type 0` inhabited it and `Type 5` did not: both of the assertions below stood the other way round.
#[test]
fn a_stored_body_is_forced_at_its_instances_levels() {
    let p = Level::param(UniverseParam(0));
    let plus_one = p.clone().succ().expect("a parameter has a successor");
    let plus_two = plus_one.clone().succ().expect("a level has a successor");
    let carrier = Global::Authored(Qualifier::from(["carrier"]));
    let reads = Global::Authored(Qualifier::from(["reads"]));
    let m = Free::local(770, Some("m"));
    let projection = Term::rec_proj(
        RecGroup::closing(vec![(
            m,
            Term::type_at(plus_two.clone()),
            Term::type_at(plus_one),
        )]),
        0,
    );
    let definition = |name: Global, universe_context: UniverseContext, type_: Term, body: Term| {
        Item::Let(Box::new(Definition {
            name,
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_,
            body,
        }))
    };
    // A `let` around the projection, so unfolding takes the general substitution rather than the branch `step_instance` keeps for a stored value that *is* a projection, which hands the group the instance whole.
    let stored = Term::let_(
        &Free::local(771, Some("w")),
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        projection.clone(),
    );
    let readers_context = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: p.clone(),
            upper: Level::zero(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let definitions = |inhabitant: Level| Definitions {
        items: vec![
            definition(
                carrier,
                UniverseContext {
                    parameter_count: 1,
                    quality_count: 0,
                    constraints: Vec::new(),
                },
                Term::type_at(plus_two.clone()),
                stored.clone(),
            ),
            definition(
                reads,
                readers_context.clone(),
                Term::instance(carrier, vec![Level::constant(5)]),
                Term::type_at(inhabitant),
            ),
        ]
        .into(),
    };

    let verdicts = fixture_verdicts(
        &definitions(Level::constant(6)),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(reads)
            && matches!(verdict.error, Error::Mismatch { .. })),
        "a stored body was forced at the reader's levels rather than at the instance's: {verdicts:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &definitions(Level::constant(5)),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "the inhabitant the instance's levels admit was refused",
    );
}

/// The value sort the walk states of a definition is the sort its value has at its own instance's levels, so the shadowed reading cannot be laundered into a believed fact.
///
/// The walk states a definition's value sort as `Sort::of` of its value, and `Sort::of`'s `published` arm then hands that stated sort to every later reader with nothing run and no `check_instance`. That is what makes this position worse than the four sites it stands beside: each of those is a lie a reader has to reach for, while this one becomes a fact recorded on the definition.
///
/// `carrier@{p} : Type (p + 2) = let w : Nat = 0 in rec m : Type (p + 2) = Type (p + 1) end.0` has the value `Type 6` at `p := 5`, whose sort is `Type 7`. While the group bound its own levels the value reduced to `Type (q + 1)` under the reader's `q ≤ 0` — `Type 1` — and `Type 2` was the sort that reading reached. What the module says of the sort is read by nothing.
///
/// Beside this, `Judge::sort_below` through `curios-analysis`'s `fits_its_variable` is the cause's other consumer of a level, which a hand-written indexed family, match and inversion would reach; a `False` through it is another witness of the same ticket rather than a second one, and no fixture is built for it here.
#[test]
fn a_published_value_sort_is_the_sort_its_instance_reaches() {
    let p = Level::param(UniverseParam(0));
    let plus_one = p.clone().succ().expect("a parameter has a successor");
    let plus_two = plus_one.clone().succ().expect("a level has a successor");
    let carrier = Global::Authored(Qualifier::from(["carrier"]));
    let published = Global::Authored(Qualifier::from(["published"]));
    let m = Free::local(780, Some("m"));
    let projection = Term::rec_proj(
        RecGroup::closing(vec![(
            m,
            Term::type_at(plus_two.clone()),
            Term::type_at(plus_one),
        )]),
        0,
    );
    // A `let` around the projection, so unfolding takes the general substitution rather than the branch `step_instance` keeps for a stored value that *is* a projection, which hands the group the instance whole.
    let stored = Term::let_(
        &Free::local(781, Some("w")),
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        projection,
    );
    let definition = |name: Global,
                      universe_context: UniverseContext,
                      type_: Term,
                      body: Term,
                      value_sort: Option<Term>| {
        Item::Let(Box::new(Definition {
            name,
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort,
            type_,
            body,
        }))
    };
    let readers_context = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: p.clone(),
            upper: Level::zero(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let definitions = |stated: Level| Definitions {
        items: vec![
            definition(
                carrier,
                UniverseContext {
                    parameter_count: 1,
                    quality_count: 0,
                    constraints: Vec::new(),
                },
                Term::type_at(plus_two.clone()),
                stored.clone(),
                None,
            ),
            definition(
                published,
                readers_context.clone(),
                Term::type_at(Level::constant(7)),
                Term::instance(carrier, vec![Level::constant(5)]),
                Some(Term::type_at(stated)),
            ),
        ]
        .into(),
    };

    for said in [Level::constant(2), Level::constant(7)] {
        let rechecked = fixture_certified(
            &definitions(said.clone()),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );
        assert_eq!(rechecked.verdicts, Vec::new(), "{said:?}");
        let Item::Let(definition) = &rechecked.definitions.items[1] else {
            panic!("the module's second item is `published`");
        };
        assert_eq!(definition.name, published);
        assert_eq!(
            definition.value_sort,
            Some(Term::type_at(Level::constant(7))),
            "the walk stated the sort the shadowed reading gave the value, where the module said {said:?}",
        );
    }
}

/// A registry entry's field type is read at the levels the occurrence's instance states.
///
/// `struct R@{p} : Type (p + 2) { f : rec m : Type (p + 2) = Type (p + 1) end.0 }` has its field typed once, positionally, under the declaration's own constraints (`check_signature`), and `infer`'s projection rule then reads that field type at the occurrence's instance: `R.{5}{Type 0}.f` is owed `Type 6`, so claiming it at `Type 1` is a mismatch.
///
/// This is site three, and by the same code path it stands for the entry's arity, its result sort, its parameter telescope, its constructor telescopes and its index targets, each of which `check_signature` types the same way. The field's value is `Type 0`, which inhabits the field type under either reading, so the reader's claim is all that moves: while the group bound its own levels the field type arrived as `Type (q + 1)` under the reader's `q ≤ 0`, and `Type 1` was what the projection answered.
#[test]
fn a_registry_field_type_is_read_at_the_occurrences_instance() {
    let p = Level::param(UniverseParam(0));
    let plus_one = p.clone().succ().expect("a parameter has a successor");
    let plus_two = plus_one.clone().succ().expect("a level has a successor");
    let record = Global::Authored(Qualifier::from(["R"]));
    let reads = Global::Authored(Qualifier::from(["reads"]));
    let m = Free::local(790, Some("m"));
    let projection = Term::rec_proj(
        RecGroup::closing(vec![(
            m,
            Term::type_at(plus_two.clone()),
            Term::type_at(plus_one),
        )]),
        0,
    );
    let declaration = StructDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::build(
            [(Free::local(791, Some("f")), projection)],
            (),
        )),
        result_sort: Term::type_at(plus_two),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let readers_context = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: p.clone(),
            upper: Level::zero(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let definitions = |claimed: Level| {
        declaring(
            Definitions {
                items: vec![Item::Let(Box::new(Definition {
                    name: reads,
                    kind: DefinitionKind::Authored,
                    universe_context: readers_context.clone(),
                    island: Qualifier::default(),
                    proof: false,
                    value_sort: None,
                    type_: Term::type_at(claimed),
                    body: Term::proj_struct(
                        Term::struct_at(
                            record,
                            vec![Level::constant(5)],
                            Vec::<Term>::new(),
                            [Term::type_at(Level::zero())],
                        ),
                        record,
                        0,
                    ),
                }))]
                .into(),
            },
            [],
            [(record, declaration.clone())],
            [],
        )
    };

    let verdicts = fixture_verdicts(
        &definitions(Level::constant(1)),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(reads)
            && matches!(verdict.error, Error::Mismatch { .. })),
        "a registry field type was read at the reader's levels: {verdicts:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &definitions(Level::constant(6)),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "the field type at the occurrence's instance was refused",
    );
}

/// A top-level `rec` group's member names only the universe parameters its item declares.
///
/// A group binds no universe, so a level `param k` inside a member's scopes is the enclosing item's `k`th wherever it stands, and an instance of the item substitutes it with every other level the item binds. That is the reading `xboard/src/formation/a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers.rs` was admitted through the absence of; the fixtures beside it hold the four sites a *reader* reaches, and this one holds the boundary, which is what makes those four the only readings there are.
///
/// While a group bound parameters of its own, `param 1` inside a member of a one-parameter item was that item's own level, shifted past the group's binder — so the module below is the pre-collapse spelling of a legitimate group, and nothing about it is ill-formed on its face. The instance `held@{5}` states one level for a one-parameter scheme, so `Kernel::check_instance` passes on width and has no constraint to discharge; `instantiate_universes` then substitutes `param 0` and leaves `param 1` as it stands, and `param 1` at the reader is the *reader's* second parameter. Under the reader's own `param 1 ≤ 0` the member's body reads as `Type 1`, which the reader's `Type 0` inhabits. `universe_escape` is the whole of what refuses it, reaching a member through the opened view [`RecItem::definitions`](curios_core::RecItem::definitions) hands back, at the item's own count.
///
/// Reachable from no surface program — `validate_universes` holds every level of a module to its declaration's count before the module leaves the elaborator — so only this fixture puts the certifier's copy of the rule at this position.
///
/// Mutation-checked, which is what makes the claim above a demonstration: with `universe_escape` answering `None`, the walk returns **zero** verdicts over the first module, so the reader's `Type 0` stands at a level the member never meant.
///
/// The control is the same group with the item declaring the parameter its member names, read at an instance that states both.
#[test]
fn a_group_members_level_names_only_its_items_universe_parameters() {
    let second = Level::param(UniverseParam(1));
    let plus_one = second.clone().succ().expect("a parameter has a successor");
    let plus_two = plus_one.clone().succ().expect("a level has a successor");
    let held = Global::Authored(Qualifier::from(["held"]));
    let reads = Global::Authored(Qualifier::from(["reads"]));
    let member = |parameters: usize| Definition {
        name: held,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: parameters,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(plus_two.clone()),
        body: Term::type_at(plus_one.clone()),
    };
    // The reader pins its own second parameter at zero: the level a captured `param 1` would be read at, and the one the member's own instance must answer instead.
    let readers_context = |parameters: usize| UniverseContext {
        parameter_count: parameters,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: second.clone(),
            upper: Level::zero(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let definitions = |parameters: usize, instance: Vec<Level>, claimed: Level| Definitions {
        items: vec![
            Item::Rec(RecItem::new(vec![(fresh(None), member(parameters))])),
            Item::Let(Box::new(Definition {
                name: reads,
                kind: DefinitionKind::Authored,
                universe_context: readers_context(2),
                island: Qualifier::default(),
                proof: false,
                value_sort: None,
                type_: Term::instance(held, instance),
                body: Term::type_at(claimed),
            })),
        ]
        .into(),
    };

    let verdicts = fixture_verdicts(
        &definitions(1, vec![Level::constant(5)], Level::zero()),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(held)
            && matches!(verdict.error, Error::UnclosedUniverses)),
        "the kernel certified a group member naming a level its item does not bind: {verdicts:?}",
    );

    let stated = vec![Level::constant(5), Level::constant(6)];
    assert_eq!(
        fixture_verdicts(
            &definitions(2, stated, Level::constant(6)),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        Vec::new(),
        "the same member, at an item declaring the parameter it names, was refused",
    );
}

/// A universe context whose offsets sit at the top of the range is decided rather than saturated.
///
/// The soundness board states that level arithmetic is checked throughout and refuses with `OffsetOverflow`, "so the hierarchy cannot be collapsed by saturating it". `Level::checked_add` is one half of that, and [`an_instance_at_the_top_of_the_offset_range_is_refused_as_an_overflow`] holds it. The other half is the arithmetic the two level *decisions* run: `LevelModel::raise` adds a clause's shift to an offset and `ceiling` multiplies a gain by a head count, both saturating, and `satisfiable` and `entails` read the result. Saturated into agreement, a loop would answer *satisfiable* — a pass raising nothing is the least model — and the walk would assume a hypothesis set from which `entails` proves every level relation and `check_instance` discharges nothing (`curios-cert/src/satisfy.rs`).
///
/// Two readings are put here, the two the decisions are. `param 0 + u32::MAX ≤ param 0` is a loop whose every pass climbs by the largest offset a level holds, so the floor passes the ceiling rather than settling: refused as unsatisfiable. And under the satisfiable `param 0 + (u32::MAX - 1) ≤ param 1`, the bound `Type (param 0 + u32::MAX - 1) : Type (param 1)` asks `param 0 + u32::MAX ≤ param 1`, one rung above what the hypothesis states: the derivation must stop one short of it instead of saturating onto it.
///
/// The controls are the two sets with the same offsets the other way round, where the hypothesis does state the bound: both must stand, since a decision that refused every large offset would shut this with a brick.
#[test]
fn a_universe_context_at_the_top_of_the_offset_range_is_decided_rather_than_saturated() {
    let first = Level::param(UniverseParam(0));
    let second = Level::param(UniverseParam(1));
    let raised = |level: &Level, offset: u32| {
        level
            .checked_add(offset)
            .expect("an offset below the top of the range")
    };
    let bounded = |lower: Level, upper: Level| UniverseConstraint {
        lower,
        upper,
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };
    let held = Global::Authored(Qualifier::from(["held"]));
    let definitions =
        |parameters: usize, constraints: Vec<UniverseConstraint>, type_, body| Definitions {
            items: vec![Item::Let(Box::new(Definition {
                name: held,
                kind: DefinitionKind::Authored,
                universe_context: UniverseContext {
                    parameter_count: parameters,
                    quality_count: 0,
                    constraints,
                },
                island: Qualifier::default(),
                proof: false,
                value_sort: None,
                type_,
                body,
            }))]
            .into(),
        };
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));

    let looping = definitions(
        1,
        vec![bounded(raised(&first, u32::MAX), first.clone())],
        nat(),
        zero(),
    );
    let verdicts = fixture_verdicts(&looping, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::UnsatisfiableUniverses)),
        "a loop through the largest offset a level holds was called satisfiable: {verdicts:?}",
    );

    let short = definitions(
        2,
        vec![bounded(raised(&first, u32::MAX - 1), second.clone())],
        Term::type_at(second.clone()),
        Term::type_at(raised(&first, u32::MAX - 1)),
    );
    let verdicts = fixture_verdicts(&short, 1_000_000, &Globals::default(), SYNTAX);
    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Mismatch { .. })),
        "a bound one rung above the hypothesis was derived at the top of the offset range: {verdicts:?}",
    );

    assert_eq!(
        fixture_verdicts(
            &definitions(
                1,
                vec![bounded(first.clone(), raised(&first, u32::MAX))],
                nat(),
                zero(),
            ),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "a set with a model at every assignment was refused for its offsets",
    );

    assert_eq!(
        fixture_verdicts(
            &definitions(
                2,
                vec![bounded(raised(&first, u32::MAX), second.clone())],
                Term::type_at(second),
                Term::type_at(raised(&first, u32::MAX - 1)),
            ),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "a bound the hypothesis states at the top of the offset range was not derived",
    );
}
