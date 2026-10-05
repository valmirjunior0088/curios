//! Nominal occurrences: the arity a term may be applied at, the binders a set opens, and how a refusal spells them.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Definition, DefinitionKind, Free, Func, FuncType, Global, InductParam, Intrinsic,
        Item, Module, Nat, StructType, Subterm, Telescope, Term, Totality, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
    std::{
        collections::{BTreeMap, BTreeSet},
        panic::{AssertUnwindSafe, catch_unwind},
    },
};

/// A nominal occurrence's parameter and index counts must be the declaration's.
///
/// `Sort::of` reads an `InductType`'s declaration to answer for it — `check_instance` for the universe width, then `result_sort` instantiated at those levels — so the occurrence must supply as many *parameters* and *indices* as the declaration declares: an arity validated against the scheme it instantiates rather than against itself, as the universe width is.
///
/// Unchecked, both halves go wrong. Where the arity is merely carried, `let held : Type = F` for a one-parameter, one-index family would certify at no parameters, at no indices, and at two indices. Where the arity is *used*, eliminating over such a scrutinee reaches `InductDecl::indices_at`, whose `Telescope::open` asserts, and the process would panic. A panic refuses rather than admits, so it is inside what the soundness board permits of the Rust implementation (see `documentation/design/soundness/the-soundness-board.md`) — but a malformed occurrence is the *program's* fault, and the house rule is that a program's fault is an `Error`. The certifying half is the one that matters: a type the kernel blesses whose shape its own declaration contradicts.
///
/// Not reachable from a surface program — `curios-elab` builds a nominal occurrence saturated from the declaration it looked up — which is why this is constructed here.
///
/// The control is [`an_occurrence_at_its_declared_arity_is_accepted`], the same family at the parameters and indices it declares, which must keep passing: every nominal type in every program is such an occurrence.
#[test]
fn an_occurrence_whose_arity_is_not_its_declarations_is_refused() {
    for (label, params, indices) in arity_cases() {
        let verdicts = fixture_verdicts(
            &occurrence_module(params, indices),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::Arity { .. })),
            "{label}: the kernel accepted an occurrence its declaration contradicts: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: one parameter and one index, as declared.
#[test]
fn an_occurrence_at_its_declared_arity_is_accepted() {
    let module = occurrence_module(
        vec![Term::intrinsic(Intrinsic::NatType)],
        vec![Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
    );

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "the boundary refused an occurrence at exactly the arity its declaration states",
    );
}

/// A nominal *value*'s parameter count must be its declaration's, as its type's already must be.
///
/// [`an_occurrence_whose_arity_is_not_its_declarations_is_refused`] holds this for the two type formers, where `Sort::of` consults a declaration to answer for an occurrence. It does not reach the value forms: nothing calls `Sort::of` on a `Struct` or a `Variant`, so their carried parameter list needs a check of its own.
///
/// Unchecked, the two forms fail differently, and neither fails well. A `Struct` opens the declaration's arity with `Telescope::open`, which **asserts** — so a value at no parameters for a one-parameter structure, or at two, would abort the process. A panic refuses rather than admits, so it is inside what the soundness board permits of the Rust implementation (see `documentation/design/soundness/the-soundness-board.md`), but it is the wrong shape twice over: a malformed value is the *program's* fault, which the house rule says is an `Error`, and `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others — an abort takes every other verdict with it, which is what makes the disagreement count a count.
///
/// A `Variant` instead opens with `open_params`, which is tolerant: too few parameters leaves the declaration's own parameter binders unopened, so they read as *payload* slots and the payload-arity check compares against the wrong number, and only a downstream conversion that happens to reject the short parameter list would refuse it — sound by coincidence rather than by any rule about the value.
///
/// Not reachable from a surface program: `curios-elab` builds a nominal value saturated from the declaration it looked up.
///
/// The control is [`a_nominal_value_at_its_declared_arity_is_accepted`], both forms at exactly the parameters they declare, which must keep passing: every constructor application and every record literal in every program is one.
#[test]
fn a_nominal_value_whose_arity_is_not_its_declarations_is_refused() {
    for (label, module) in nominal_value_cases() {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::Arity { .. })),
            "{label}: the value's parameter count was not held to its declaration: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: one parameter each, as declared.
#[test]
fn a_nominal_value_at_its_declared_arity_is_accepted() {
    let nat = Term::intrinsic(Intrinsic::NatType);

    assert_eq!(
        fixture_verdicts(
            &struct_value_module(vec![nat.clone()]),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "a record literal at exactly its declared parameters was refused",
    );
    assert_eq!(
        fixture_verdicts(
            &variant_value_module(vec![nat]),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        ),
        Vec::new(),
        "a constructor application at exactly its declared parameters was refused",
    );
}

/// A nominal value's parameters are typed as an occurrence's are: its signature or field telescope is read at them, and its type carries them to every rule that meets it.
///
/// Each value here is handed `((b : Bool) => Nat)(3)` for its one `Type`-sorted parameter — an application at an argument of the wrong type, which β takes to `Nat` without looking. So the payload checks against the telescope at it, the value's type converts with the declared `S(Nat)` and `F(Nat)`, and nothing but typing the parameter itself can refuse it. Counted and never typed, both would be certified. Mutation-checked: counting a value's parameters without typing them accepts both. The control is [`a_nominal_value_at_its_declared_arity_is_accepted`].
#[test]
fn a_nominal_value_types_its_parameters() {
    let b = Free::local(990, Some("b"));
    let ill_typed = Term::apply(
        Term::func(
            [(b, Term::intrinsic(Intrinsic::BoolType))],
            Term::intrinsic(Intrinsic::NatType),
        ),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)))],
    );

    for (label, module) in [
        (
            "a record literal",
            struct_value_module(vec![ill_typed.clone()]),
        ),
        (
            "a constructor application",
            variant_value_module(vec![ill_typed]),
        ),
    ] {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, Error::Mismatch { .. })),
            "{label} was certified at a parameter its declaration does not admit: {verdicts:?}",
        );
    }
}

/// A count carried on a term and used to *index* must be checked, not assumed.
///
/// Reduction runs on a type position before typing has reached it, so the functions that walk it cannot rest on an invariant typing would establish.
///
/// **Reduction.** `step_apply` opens a lambda's telescope at the application's arguments. `Telescope::open` asserts, so an application that does not saturate its lambda would **abort the walk**; it is stuck instead, which is the conservative direction twice over: reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than killing every other verdict. `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others, and an abort is what makes that false.
///
/// **Synthesis needs no leg.** `synth_neutral`'s partial-application arm opens the head type's telescope at the arguments supplied, and each entry that remains is still under its own mark, so there is no vector beside the telescope to drift and this fixture keeps the lambda case alone.
///
/// It is not reachable from a surface program — `curios-elab` emits saturated applications — and what is at stake is a program's fault aborting the kernel where an `Error` belongs.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`]. It is the direction that matters: reduction must still fire on a well-formed application, and a guard that simply stopped reducing would pass every witness here while breaking every program.
///
/// With the declared type typed rather than classified, `infer` refuses the lambda case as `Arity { expected: 2, actual: 1 }` — naming the defect — where reduction going stuck alone would leave a generic `Unclassified`. Both verdicts are accepted here, since which rule reaches the fault first is not what the fixture pins.
#[test]
fn a_count_a_term_carries_is_refused_rather_than_indexed_with() {
    for (label, module) in unsaturated_cases() {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts.iter().any(|verdict| matches!(
                verdict.error,
                Error::Arity { .. } | Error::Unclassified(_)
            )),
            "{label}: the term was indexed at a count nothing checked: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: a lambda applied to exactly its binders still reduces, so the type position it stands in is classified.
#[test]
fn a_saturated_application_in_a_type_position_is_accepted() {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let former = Global::Authored(Qualifier::from(["f"]));

    let plicities = vec![Plicity::Explicit, Plicity::Explicit];
    let former_def = authored(
        &former,
        Subterm::FuncType(FuncType::new(
            Telescope::build([(a, nat.clone()), (b, nat.clone())], Term::type_ground()),
            plicities.clone(),
        ))
        .into(),
        Subterm::Func(Func::new(
            Telescope::build([(a, nat.clone()), (b, nat.clone())], nat.clone()),
            plicities,
        ))
        .into(),
    );

    // `f(3, 4)` reduces to `Nat`, so `held : f(3, 4) = 3` is an ordinary well-typed item.
    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::apply(
            Term::free_var(&Free::from(&former)),
            [
                three.clone(),
                Term::intrinsic(Intrinsic::Nat(Nat::new(4usize))),
            ],
        ),
        three,
    );

    let module = Module {
        mounts: Vec::new(),
        items: vec![former_def, held],
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "a saturated application in a type position was refused",
    );
}

/// The two further reduction steps that open a binder set at a count the term supplies.
///
/// [`a_count_a_term_carries_is_refused_rather_than_indexed_with`] holds the β step. `whnf` opens a binder set in four places, and these are two more — both reached the same way, by reduction running on a type position before typing has reached it.
///
/// **The arm of an elimination.** Reducing a `match` on a concrete constructor opens the matching arm at that constructor's payload. `Scope::open` asserts, so an arm binding two components of a one-component payload — or none — would **abort the walk**. The arm arity *is* checked, by `check_arm`, but only once typing reaches the elimination; reduction of a type position runs first.
///
/// **The recursive twin of the β step.** `unfold_rec_apply` unfolds a folded recursive application by opening its member's telescope at the arguments, exactly as `step_apply` does, so `rec f : (a, b) -> Type = …; f(3)` in a type position would abort there.
///
/// Both are stuck instead, the same conservative direction the β step takes — reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than aborting the walk and taking every other verdict with it.
///
/// `step_proj` needs nothing: every arm guards its index (`index < fields.len()`, `(1..=payload.len()).contains(&index)`) and falls through to stuck, the pattern these two follow.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`] together with [`an_arm_matching_its_payload_still_reduces`]: a guard that merely stopped reducing would pass every witness here while breaking every program.
#[test]
fn a_binder_set_is_not_opened_at_a_count_the_term_supplied() {
    for (label, module) in unguarded_opener_cases() {
        // The demonstrated defect is the *abort*: `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others, and a panic makes that false.
        let verdicts = catch_unwind(AssertUnwindSafe(|| {
            fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX)
        }))
        .unwrap_or_else(|_| panic!("{label}: reduction aborted the walk instead of refusing"));

        assert!(
            !verdicts.is_empty(),
            "{label}: the module was certified rather than refused",
        );
    }
}

/// The control for the arm half: an arm binding exactly its constructor's payload still reduces, so the type position it computes is classified.
#[test]
fn an_arm_matching_its_payload_still_reduces() {
    let module = arm_module(vec![(Plicity::Explicit, Free::local(996, Some("a")))]);

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new(),
        "an arm binding exactly its payload was refused",
    );
}

/// A nominal occurrence's parameters and indices are read by every rule that consults its declaration, so they are typed.
///
/// `at.rs` states the discipline: an occurrence is meaningful only once what it carries has been checked against what the declaration declares. The universe instance and the parameter and index *counts* are checked behind the handle; that each argument inhabits the domain the arity states it at is the third thing, and typing's. Counts are the boundary's job because no typing rule reads a length; a *shape* is typing's, and this one had no rule at all.
///
/// The forgery is what reading one unestablished buys. `Eq(@True)(0, 1)` is admitted as a type — `Sort::of` consults the declaration for its `result_sort` and hands back `Prop`, having checked two counts and nothing else — although `0` and `1` are `Nat`s standing in a domain the declaration says is `True`. It is then *inhabited*, and by the rule working correctly: `induct_type_args` compares the indices at the declared domain, that domain is `Prop`-sorted, and proof irrelevance discharges both without looking, so `refl(True, qed())` subsumes into it. From there every step is ordinary. Eliminating the forged equation under the motive `(s, t, q) => (Held(s)) -> Held(t)` — where the same gap lets `s` and `t`, typed `True`, stand in `Held`'s `Nat` index — yields `(Held(0)) -> Held(1)`, and `Held(1)` is uninhabited by construction, so the vacuous elimination coverage licenses (its only constructor targets `0`, which the `Nat` peel clashes against `1`) proves `False`.
///
/// No surface program reaches it — `curios-elab` elaborates a nominal occurrence as an application against the arity's telescope and checks every argument — which is why this is built by hand.
///
/// Its control is [`an_indexed_occurrence_at_a_well_typed_index_is_accepted`], which keeps the same family at an index that genuinely inhabits `Nat`: without it, refusing every indexed occurrence would pass this.
#[test]
fn a_nominal_occurrence_types_its_arguments() {
    let verdicts = fixture_verdicts(&index_forgery(), 1_000_000, &Globals::default(), SYNTAX);

    assert!(
        !verdicts.is_empty(),
        "the kernel certified a closed inhabitant of `False`",
    );
}

/// The control: an index that really is a `Nat` still types, so the guard above rejects a wrong argument rather than every argument.
#[test]
fn an_indexed_occurrence_at_a_well_typed_index_is_accepted() {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held_decl = indexed_family(
        Free::local(70, Some("n")),
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Term::type_ground(),
    );

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::induct_type(
            held_name,
            Vec::<Term>::new(),
            [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
        ),
        Term::variant(held_name, Vec::<Term>::new(), "yes", Vec::<Term>::new()),
    );

    let module = Module {
        mounts: Vec::new(),
        items: vec![held],
        induct_decls: BTreeMap::from([(held_name, held_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new()
    );
}

/// The same bogus occurrence, smuggled past [`a_nominal_occurrence_types_its_arguments`] through a Σ field.
///
/// Typing an occurrence's arguments closes the route only where something *types* the occurrence, so a type former's parts must be typed too: a `FuncType` or a `TupleType` answered with `Sort::of`, which classifies each domain — consulting a declaration for its sort and checking nothing else — would be the second, weaker way to accept a type `curios-cert/README.md` says this crate does not have.
///
/// Classified alone, `{Eq(@True)(0, 1)}` would be admitted, and the projection rule would hand the field's declared type straight back: `v.0` a scrutinee at the forged equation, and the rest of [`index_forgery`]'s derivation unchanged. The codomain half is the same shape — `(Nat) -> Eq(@True)(0, 1)`, with an application handing the codomain back — so both a `Proj` and an `Apply` reach it.
#[test]
fn a_bogus_occurrence_behind_a_tuple_field_is_refused() {
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let true_name = Global::Authored(Qualifier::from(["True"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let true_type = Term::induct_type(true_name, Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(true_name, Vec::<Term>::new(), "qed", Vec::<Term>::new());

    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    )]);

    let equality_decl = equality_declaration();

    // v : {Eq(True, 0, 1)} = (refl(True, qed()))
    let bogus = Term::induct_type(equality_name, [true_type.clone()], [nat(0), nat(1)]);
    let wrapped = authored(
        &Global::Authored(Qualifier::from(["v"])),
        Term::tuple_type(vec![(Free::local(30, Some("b")), bogus)]),
        Term::tuple([Term::variant(equality_name, [true_type], "refl", [qed])]),
    );

    let module = Module {
        mounts: Vec::new(),
        items: vec![wrapped],
        induct_decls: BTreeMap::from([(true_name, true_decl), (equality_name, equality_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

    assert!(
        !verdicts.is_empty(),
        "the kernel certified a bogus occurrence standing as a tuple field type",
    );
}

/// A refusal names the types the way the program that produced them wrote them.
///
/// `Error`'s own `Display` is faithful to Core — fully qualified paths, every parameter positional — which is right for a term printed in isolation and wrong for a message a reader has to recognize their own program in. `format_with` supplies the two axes that fix it: globals shortened against the module's symbol table, and a nominal family's implicit parameters marked from the type constructor's declared plicities.
///
/// Universe instances are deliberately left alone; see `Error::format_with`.
#[test]
fn a_refusal_shortens_names_and_marks_implicit_parameters() {
    let name = Global::Authored(Qualifier::from(["demo", "Box", "Box"]));
    let parameter = Free::local(0, Some("A"));

    // `struct Box(@A : Type)`: one implicit parameter, so a use site writes `Box(Nat)` and never supplies it positionally.
    let constructor = Definition {
        name,
        kind: DefinitionKind::StructType,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::func_type_marked(
            [(Plicity::Implicit, parameter, Term::type_ground())],
            Term::type_ground(),
        ),
        body: Term::type_ground(),
    };

    let module = Module {
        mounts: Vec::new(),
        items: vec![Item::Let(constructor)],
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    };

    let applied: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: vec![Term::intrinsic(Intrinsic::NatType)],
    })
    .into();
    let refusal = Error::Mismatch {
        inferred: Box::new(applied),
        expected: Box::new(Term::intrinsic(Intrinsic::NatType)),
    };

    assert_eq!(
        refusal.format_with(&module, &[], &SYNTAX),
        "expected `Nat`, found `Box(@Nat)`"
    );
    // The faithful rendering keeps the qualified path and drops the mark, which is what makes the axes worth supplying.
    assert_eq!(
        refusal.to_string(),
        "expected `Nat`, found `/demo/Box/Box(Nat)`"
    );
}
