//! Nominal occurrences: the arity a term may be applied at, the binders a set opens, and how a refusal spells them.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::{Globals, KernelError},
    curios_core::{
        Atom, Definition, DefinitionKind, Entrypoint, Free, Func, FuncType, Global, InductParam,
        Intrinsic, Item, Module, Nat, StructType, Subterm, Telescope, Term, Totality,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
    std::{
        collections::{BTreeMap, BTreeSet},
        panic::{AssertUnwindSafe, catch_unwind},
    },
};

use super::test_support::*;

/// A nominal occurrence's parameter and index counts must be the declaration's.
///
/// `Sort::of` reads an `InductType`'s declaration to answer for it — `check_instance` for the universe width, then `result_sort` instantiated at those levels. It never asked whether the occurrence supplies as many *parameters* and *indices* as the declaration declares. That is the same omission the universe width had before `check_instance` checked it: an occurrence's arity validated against itself rather than against the scheme it instantiates.
///
/// Verified while the hole was open, in both halves. Where the arity is merely carried, the module was **certified with zero refusals** — `let held : Type = F` for a one-parameter, one-index family, at no parameters, at no indices, and at two indices, all three accepted. Where the arity is *used*, the kernel **aborted**: eliminating over such a scrutinee reaches `InductDecl::indices_at`, whose `Telescope::open` asserts, and the process panicked with `telescope arity mismatch in open: expected 1, got 0`. A panic refuses rather than admits, so it is inside what the perimeter permits of the Rust implementation (see `documentation/design/language/the-soundness-perimeter.md`) — but a malformed occurrence is the *program's* fault, and the house rule is that a program's fault is a `KernelError`. The certified half is the one that matters: a type the kernel blessed whose shape its own declaration contradicts.
///
/// Not reachable from a surface program — `curios-elab` builds a nominal occurrence saturated from the declaration it looked up — which is why this is constructed here. No inhabitant of `False` was built from it; what is demonstrated is that the arity is unchecked and that both of its consumers are wrong when it is.
///
/// The control is [`an_occurrence_at_its_declared_arity_is_accepted`], the same family at the parameters and indices it declares, which must keep passing: every nominal type in every program is such an occurrence.
#[test]
fn an_occurrence_whose_arity_is_not_its_declarations_is_refused() {
    for (label, params, indices) in arity_cases() {
        let verdicts = fixture_verdicts(
            &occurrence_module(params, indices),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX,
        );

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::Arity { .. })),
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
        fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "the boundary refused an occurrence at exactly the arity its declaration states",
    );
}

/// A nominal *value*'s parameter count must be its declaration's, as its type's already must be.
///
/// [`an_occurrence_whose_arity_is_not_its_declarations_is_refused`] closed this for the two type formers, where `Sort::of` consults a declaration to answer for an occurrence. It does not reach the value forms: nothing calls `Sort::of` on a `Struct` or a `Variant`, so their carried parameter list was still taken on the term's own word.
///
/// The two forms failed differently, and neither failed well. A `Struct` opens the declaration's arity with `Telescope::open`, which **asserts** — so a value at no parameters for a one-parameter structure aborted the process with `telescope arity mismatch in open: expected 1, got 0`, and at two parameters with `expected 1, got 2`. A panic refuses rather than admits, so it is inside what the perimeter permits of the Rust implementation (see `documentation/design/language/the-soundness-perimeter.md`), but it is the wrong shape twice over: a malformed value is the *program's* fault, which the house rule says is a `KernelError`, and `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others — an abort takes every other verdict with it, which is what makes the disagreement count a count.
///
/// A `Variant` instead opens with `open_params`, which is tolerant: too few parameters leaves the declaration's own parameter binders unopened, so they read as *payload* slots and the payload-arity check compares against the wrong number. That was refused while the hole was open, but downstream and by accident — the resulting type carried the short parameter list into a conversion that happened to reject it — rather than by any rule about the value. Sound by coincidence is the pattern this class keeps producing.
///
/// Not reachable from a surface program: `curios-elab` builds a nominal value saturated from the declaration it looked up. No inhabitant of `False` was built from either; what is demonstrated is that the count is unchecked and that both consumers are wrong when it is.
///
/// The control is [`a_nominal_value_at_its_declared_arity_is_accepted`], both forms at exactly the parameters they declare, which must keep passing: every constructor application and every record literal in every program is one.
#[test]
fn a_nominal_value_whose_arity_is_not_its_declarations_is_refused() {
    for (label, module) in nominal_value_cases() {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::Arity { .. })),
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
            crate::SYNTAX,
        ),
        Vec::new(),
        "a record literal at exactly its declared parameters was refused",
    );
    assert_eq!(
        fixture_verdicts(
            &variant_value_module(vec![nat]),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX,
        ),
        Vec::new(),
        "a constructor application at exactly its declared parameters was refused",
    );
}

/// A count carried on a term and used to *index* must be checked, not assumed — twice, in reduction and in synthesis.
///
/// Both are reached the same way. `check_definition` calls `Sort::of` on a declared type and never infers it, so a type position holds a term nothing has typed, and the two functions that walk it were written against an invariant typing would have established.
///
/// **Reduction.** `step_apply` opens a lambda's telescope at the application's arguments. `Telescope::open` asserts, so an application that does not saturate its lambda **aborted the walk** — `telescope arity mismatch in open: expected 2, got 1`. It is now stuck instead, which is the conservative direction twice over: reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than killing every other verdict. `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others, and an abort is what makes that false.
///
/// **Synthesis, retired.** `synth_neutral`'s partial-application arm slices a spine's head-type `plicities` at the argument count, and a `FuncType` whose marks were not parallel to its telescope once aborted there, at `sort.rs`'s slice; a guard on the vector's length was added and a second fixture leg pinned it. On 2026-08-30 the pairing became a construction invariant — `FuncType::new` is the one door that builds a mark vector beside its telescope, the archived prelude restores exactly the constructor-built value its build wrote, and `curios-prelude-archive`'s `the_restored_prelude_pairs_every_mark_with_its_binder` checks that once per test run — so the drifted vector is unrepresentable, the guard retired with it, and this fixture keeps the lambda case alone.
///
/// Verified while the reduction hole was open: the lambda case aborted the process rather than producing a verdict. It is not reachable from a surface program — `curios-elab` emits saturated applications — and no inhabitant of `False` was built from it; what is demonstrated is that a program's fault aborted the kernel where a `KernelError` belongs.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`]. It is the direction that matters: reduction must still fire on a well-formed application, and a guard that simply stopped reducing would pass every witness here while breaking every program.
///
/// The lambda case's diagnostic *improved* when `infer_type` landed: with the declared type typed rather than classified, `infer` refuses it as `Arity { expected: 2, actual: 1 }` — naming the defect — where reduction going stuck had left it a generic `Unclassified`. Both verdicts are accepted here, since which rule reaches the fault first is not what the fixture pins.
#[test]
fn a_count_a_term_carries_is_refused_rather_than_indexed_with() {
    for (label, module) in unsaturated_cases() {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

        assert!(
            verdicts.iter().any(|verdict| matches!(
                verdict.error,
                KernelError::Arity { .. } | KernelError::Unclassified(_)
            )),
            "{label}: the term was indexed at a count nothing checked: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: a lambda applied to exactly its binders still reduces, so the type position it stands in is classified as it always was.
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
            Telescope::build(
                [(a.clone(), nat.clone()), (b.clone(), nat.clone())],
                Term::type_ground(),
            ),
            plicities.clone(),
        ))
        .into(),
        Subterm::Func(Func::new(
            Telescope::build(
                [(a.clone(), nat.clone()), (b.clone(), nat.clone())],
                nat.clone(),
            ),
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
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    };

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "a saturated application in a type position was refused",
    );
}

/// The two reduction steps that still opened a binder set at a count the term supplied.
///
/// [`a_count_a_term_carries_is_refused_rather_than_indexed_with`] closed the β step and the partial-spine slice. It closed one of two twins and one of two openers. `whnf` opens a binder set in four places, and an enumeration of them found these two still unguarded — both reached the same way, through `check_definition` sorting a declared type it never infers.
///
/// **The arm of an elimination.** Reducing a `match` on a concrete constructor opens the matching arm at that constructor's payload. `Scope::open` asserts, so an arm binding two components of a one-component payload — or none — **aborted the walk**. The arm arity *is* checked, by `check_arm`, but only once typing reaches the elimination; reduction of a type position runs first and had no such precondition.
///
/// **The recursive twin of the β step.** `unfold_rec_apply` unfolds a folded recursive application by opening its member's telescope at the arguments, exactly as `step_apply` did, and was left behind when `step_apply` was guarded. `rec f : (a, b) -> Type = …; f(3)` in a type position aborted there.
///
/// Verified while the holes were open: each of the three cases panicked at `Scope::open` or `Telescope::open` rather than producing a verdict, and each was confirmed independently reachable. Both are stuck now, which is the same conservative direction the β step took — reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than aborting the walk and taking every other verdict with it.
///
/// `step_proj` was enumerated alongside them and needed nothing: every arm already guards its index (`index < fields.len()`, `(1..=payload.len()).contains(&index)`) and falls through to stuck, which is what these two now do. It is the pattern, and it was already there to copy.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`] together with [`an_arm_matching_its_payload_still_reduces`]: a guard that merely stopped reducing would pass every witness here while breaking every program.
#[test]
fn a_binder_set_is_not_opened_at_a_count_the_term_supplied() {
    for (label, module) in unguarded_opener_cases() {
        // The demonstrated defect is the *abort*: `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others, and a panic makes that false.
        let verdicts = catch_unwind(AssertUnwindSafe(|| {
            fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX)
        }))
        .unwrap_or_else(|_| panic!("{label}: reduction aborted the walk instead of refusing"));

        assert!(
            !verdicts.is_empty(),
            "{label}: the module was certified rather than refused",
        );
    }
}

/// The control for the arm half: an arm binding exactly its constructor's payload still reduces, so the type position it computes is classified as it always was.
#[test]
fn an_arm_matching_its_payload_still_reduces() {
    let module = arm_module(vec![(Plicity::Explicit, Free::local(996, Some("a")))]);

    assert_eq!(
        fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX),
        Vec::new(),
        "an arm binding exactly its payload was refused",
    );
}

/// A nominal occurrence's parameters and indices are read by every rule that consults its declaration, and nothing typed them.
///
/// `at.rs` states the discipline this violated: an occurrence is meaningful only once what it carries has been checked against what the declaration declares, and three things had to hold. Two of them — the universe instance, and the parameter and index *counts* — moved behind the handle. The third was never written: that each argument inhabits the domain the arity states it at. Counts are the boundary's job because no typing rule reads a length; a *shape* is typing's, and this one had no rule at all.
///
/// The forgery is what reading one unestablished buys. `Eq(True, 0, 1)` is admitted as a type — `Sort::of` consults the declaration for its `result_sort` and hands back `Prop`, having checked two counts and nothing else — although `0` and `1` are `Nat`s standing in a domain the declaration says is `True`. It is then *inhabited*, and by the rule working correctly: `induct_type_args` compares the indices at the declared domain, that domain is `Prop`-sorted, and proof irrelevance discharges both without looking, so `refl(True, qed())` subsumes into it. From there every step is ordinary. Eliminating the forged equation under the motive `(s, t, q) => (Held(s)) -> Held(t)` — where the same gap lets `s` and `t`, typed `True`, stand in `Held`'s `Nat` index — yields `(Held(0)) -> Held(1)`, and `Held(1)` is uninhabited by construction, so the vacuous elimination coverage licenses (its only constructor targets `0`, which `peel_nat` clashes against `1`) proves `False`.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero** refusals for exactly this module, `let boom : False` included. No surface program reaches it — `curios-elab` elaborates a nominal occurrence as an application against the arity's telescope and checks every argument — which is why the certifier's copy of the rule went unwritten, and why the second opinion was worth nothing here.
///
/// Its control is [`an_indexed_occurrence_at_a_well_typed_index_is_accepted`], which keeps the same family at an index that genuinely inhabits `Nat`: without it, refusing every indexed occurrence would pass this.
#[test]
fn a_nominal_occurrence_types_its_arguments() {
    let verdicts = fixture_verdicts(
        &index_forgery(),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

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
            held_name.clone(),
            Vec::<Term>::new(),
            [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
        ),
        Term::variant(
            held_name.clone(),
            Vec::<Term>::new(),
            "yes",
            Vec::<Term>::new(),
        ),
    );

    let module = Module {
        mounts: Vec::new(),
        items: vec![held],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(held_name, held_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
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

/// The same bogus occurrence, smuggled past [`a_nominal_occurrence_types_its_arguments`] through a Σ field.
///
/// Typing an occurrence's arguments closes the route only where something *types* the occurrence. A type former's parts are not typed: `infer` answers for a `FuncType` or a `TupleType` with `Sort::of`, which classifies each domain — consulting a declaration for its sort and checking nothing else — and that is the second, weaker way to accept a type `curios-cert/README.md` says this crate no longer has.
///
/// So `{Eq(True, 0, 1)}` is admitted, and the projection rule hands the field's declared type straight back: `v.0` is a scrutinee at the forged equation, and the rest of [`index_forgery`]'s derivation is unchanged. The codomain half is the same shape — `(Nat) -> Eq(True, 0, 1)` is admitted, and an application hands the codomain back — so both a `Proj` and an `Apply` reach it.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero** refusals for this module.
#[test]
fn a_bogus_occurrence_behind_a_tuple_field_is_refused() {
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let true_name = Global::Authored(Qualifier::from(["True"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let true_type = Term::induct_type(true_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(
        true_name.clone(),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    );

    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    )]);

    let equality_decl = equality_declaration();

    // v : {Eq(True, 0, 1)} = (refl(True, qed()))
    let bogus = Term::induct_type(equality_name.clone(), [true_type.clone()], [nat(0), nat(1)]);
    let wrapped = authored(
        &Global::Authored(Qualifier::from(["v"])),
        Term::tuple_type(vec![(Free::local(30, Some("b")), bogus)]),
        Term::tuple([Term::variant(
            equality_name.clone(),
            [true_type],
            "refl",
            [qed],
        )]),
    );

    let module = Module {
        mounts: Vec::new(),
        items: vec![wrapped],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(true_name, true_decl), (equality_name, equality_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

    assert!(
        !verdicts.is_empty(),
        "the kernel certified a bogus occurrence standing as a tuple field type",
    );
}

/// A refusal names the types the way the program that produced them wrote them.
///
/// `KernelError`'s own `Display` is faithful to Core — fully qualified paths, every parameter positional — which is right for a term printed in isolation and wrong for a message a reader has to recognize their own program in. `format_with` supplies the two axes that fix it: globals shortened against the module's symbol table, and a nominal family's implicit parameters marked from the type constructor's declared plicities.
///
/// Universe instances are deliberately left alone; see `KernelError::format_with`.
#[test]
fn a_refusal_shortens_names_and_marks_implicit_parameters() {
    let name = Global::Authored(Qualifier::from(["demo", "Box", "Box"]));
    let parameter = Free::local(0, Some("A"));

    // `struct Box(@A : Type)`: one implicit parameter, so a use site writes `Box(Nat)` and never supplies it positionally.
    let constructor = Definition {
        name: name.clone(),
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

    let applied: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: vec![Term::intrinsic(Intrinsic::NatType)],
    })
    .into();
    let refusal = KernelError::Mismatch {
        inferred: Box::new(applied),
        expected: Box::new(Term::intrinsic(Intrinsic::NatType)),
    };

    assert_eq!(
        refusal.format_with(&module, &[], &crate::SYNTAX),
        "expected `Nat`, found `Box(@Nat)`"
    );
    // The faithful rendering keeps the qualified path and drops the mark, which is what makes the axes worth supplying.
    assert_eq!(
        refusal.to_string(),
        "expected `Nat`, found `/demo/Box/Box(Nat)`"
    );
}
