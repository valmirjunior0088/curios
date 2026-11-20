//! Nominal occurrences: the arity a term may be applied at, the binders a set opens, and how a refusal spells them.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_surface::Spelling,
    std::panic::{AssertUnwindSafe, catch_unwind},
};

/// A family of one parameter and one index, `F(A : Type) : (i : Nat) -> Type`, whose one case is not defined, and `held : Type` an occurrence of it at `${occurrence}`.
const OCCURRENCE: &str = r#"
induct /F(A: Type): (i: #NatType) -> pub Type
| mk(): (0)
end

let /held: Type =
    #{node = InductType} /F${occurrence};
"#;

/// A record of one parameter, `S(A : Type) { f : A }`, and a family of one, `F(A : Type)` with `mk(x : A)`, whose case is not defined.
const ONE_PARAMETER: &str = r#"
struct /S(A: Type): pub Type {
    f: A,
}

induct /F(A: Type): pub Type
| mk(x: A)
end
"#;

/// [`ONE_PARAMETER`]'s declarations and `held`, a record literal of `S` at `${record}`.
fn record_held(record: &str) -> String {
    fill!(
        "${ONE_PARAMETER}\nlet /held: #{node = StructType} /S(#NatType) =\n    ${record};",
        ONE_PARAMETER,
        record
    )
}

/// [`ONE_PARAMETER`]'s declarations and `held`, a value of `F`'s case at `${variant}`.
fn variant_held(variant: &str) -> String {
    fill!(
        "${ONE_PARAMETER}\nlet /held: #{node = InductType} /F(#NatType) =\n    #{node = Variant} /F/mk${variant};",
        ONE_PARAMETER,
        variant
    )
}

/// `True`, and `Eq(A : Prop) : (x : A, y : A) -> Prop` with `refl(z : A) : (z, z)`, neither case defined: a parameter `True` inhabits, whose indices are typed at it.
const EQUALITY: &str = r#"
induct /True: pub Prop
| qed()
end

induct /Eq(A: Prop): (x: A, y: A) -> pub Prop
| refl(z: A): (z, z)
end
"#;

/// Whether `verdicts` refuse `held` for a count, `actual` where `expected` was.
fn held_miscounted(verdicts: &[Refusal], counted: Counted, expected: usize, actual: usize) -> bool {
    refuses(
        verdicts,
        Some("held"),
        miscounted(counted, expected, actual),
    )
}

/// Whether `verdicts` refuse `name` for a term found at `Nat` where `True` was expected: an index the declaration types at `True`.
fn a_nat_index_at_true(verdicts: &[Refusal], name: &str) -> bool {
    refuses(verdicts, Some(name), |error| {
        matches!(error, Error::Mismatch { inferred, expected }
            if **inferred == term("#NatType")
                && matches!(&***expected, curios_core::Subterm::InductType(family) if family.name == global("True")))
    })
}

/// A nominal occurrence's parameter and index counts must be the declaration's.
///
/// `Sort::of` reads an `InductType`'s declaration to answer for it — `check_instance` for the universe width, then `result_sort` instantiated at those levels — so the occurrence must supply as many *parameters* and *indices* as the declaration declares: an arity validated against the scheme it instantiates rather than against itself, as the universe width is.
///
/// Unchecked, both halves go wrong. Where the arity is merely carried, `let held : Type = F` for a one-parameter, one-index family would certify at no parameters, at no indices, and at two indices. Where the arity is *used*, eliminating over such a scrutinee reaches `InductDecl::indices_at`, whose `Telescope::open` asserts, and the process would panic. A panic refuses rather than admits, so it is inside what the soundness board permits of the Rust implementation (see `documentation/design/soundness/the-soundness-board.md`) — but a malformed occurrence is the *program's* fault, and the house rule is that a program's fault is an `Error`. The certifying half is the one that matters: a type the certifier blesses whose shape its own declaration contradicts.
///
/// Not reachable from a surface program — `curios-elab` builds a nominal occurrence saturated from the declaration it looked up — which is why this is constructed here.
///
/// The control is [`an_occurrence_at_its_declared_arity_is_accepted`], the same family at the parameters and indices it declares, which must keep passing: every nominal type in every program is such an occurrence.
#[test]
fn an_occurrence_whose_arity_is_not_its_declarations_is_refused() {
    for (label, occurrence, counted, actual) in [
        ("no parameters", "()(0)", Counted::Parameters, 0),
        ("no indices", "(#NatType)", Counted::Indices, 0),
        ("two indices", "(#NatType)(0, 0)", Counted::Indices, 2),
    ] {
        let verdicts = text_refusals(&fill!(OCCURRENCE, occurrence));

        assert!(
            held_miscounted(&verdicts, counted, 1, actual),
            "{label}: the certifier accepted an occurrence its declaration contradicts: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: one parameter and one index, as declared.
#[test]
fn an_occurrence_at_its_declared_arity_is_accepted() {
    let occurrence = "(#NatType)(0)";

    assert_eq!(
        text_refusals(&fill!(OCCURRENCE, occurrence)),
        Vec::new(),
        "the boundary refused an occurrence at exactly the arity its declaration states",
    );
}

/// A nominal *value*'s parameter count must be its declaration's, as its type's already must be.
///
/// [`an_occurrence_whose_arity_is_not_its_declarations_is_refused`] holds this for the two type formers, where `Sort::of` consults a declaration to answer for an occurrence. It does not reach the value forms: nothing calls `Sort::of` on a `Struct` or a `Variant`, so their carried parameter list needs a check of its own.
///
/// Unchecked, either form would fail badly. A `Struct` opens the declaration's arity with `Telescope::open` and a `Variant` its case's payload with `InductParam::open`, both of which **assert** — so a value at no parameters for a one-parameter declaration, or at two, would abort the process. A panic refuses rather than admits, so it is inside what the soundness board permits of the Rust implementation (see `documentation/design/soundness/the-soundness-board.md`), but it is the wrong shape twice over: a malformed value is the *program's* fault, which the house rule says is an `Error`, and `certify_definitions` is documented as walking to the end with each verdict independent of the others — an abort takes every other verdict with it, which is what makes the disagreement count a count.
///
/// Not reachable from a surface program: `curios-elab` builds a nominal value saturated from the declaration it looked up.
///
/// The control is [`a_nominal_value_at_its_declared_arity_is_accepted`], both forms at exactly the parameters they declare, which must keep passing: every constructor application and every record literal in every program is one.
#[test]
fn a_nominal_value_whose_arity_is_not_its_declarations_is_refused() {
    for (label, text, actual) in [
        ("struct at no parameters", record_held("/S { 3 }"), 0),
        (
            "struct at two parameters",
            record_held("/S(#NatType, #NatType) { 3 }"),
            2,
        ),
        ("variant at no parameters", variant_held("(3)"), 0),
        (
            "variant at two parameters",
            variant_held("(@#NatType, @#NatType, 3)"),
            2,
        ),
    ] {
        let verdicts = text_refusals(&text);

        assert!(
            held_miscounted(&verdicts, Counted::Parameters, 1, actual),
            "{label}: the value's parameter count was not held to its declaration: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: one parameter each, as declared.
#[test]
fn a_nominal_value_at_its_declared_arity_is_accepted() {
    assert_eq!(
        text_refusals(&record_held("/S(#NatType) { 3 }")),
        Vec::new(),
        "a record literal at exactly its declared parameters was refused",
    );
    assert_eq!(
        text_refusals(&variant_held("(@#NatType, 3)")),
        Vec::new(),
        "a constructor application at exactly its declared parameters was refused",
    );
}

/// A nominal value's parameters are typed as an occurrence's are: its signature or field telescope is read at them, and its type carries them to every rule that meets it.
///
/// Each value here is handed `((b : Bool) => Nat)(3)` for its one `Type`-sorted parameter — an application at an argument of the wrong type, which β takes to `Nat` without looking. So the payload checks against the telescope at it, the value's type converts with the declared `S(Nat)` and `F(Nat)`, and nothing but typing the parameter itself can refuse it. Counted and never typed, both would be certified. Mutation-checked: counting a value's parameters without typing them accepts both. The control is [`a_nominal_value_at_its_declared_arity_is_accepted`].
#[test]
fn a_nominal_value_types_its_parameters() {
    let ill_typed = "((b: #BoolType) => #NatType)(3)";

    for (label, text) in [
        (
            "a record literal",
            record_held(&fill!("/S(${ill_typed}) { 3 }", ill_typed)),
        ),
        (
            "a constructor application",
            variant_held(&fill!("(@${ill_typed}, 3)", ill_typed)),
        ),
    ] {
        let verdicts = text_refusals(&text);

        assert!(
            refuses(&verdicts, Some("held"), |error| {
                matches!(error, Error::Mismatch { inferred, expected }
                    if **inferred == term("#NatType")
                        && **expected == term("#BoolType"))
            }),
            "{label} was certified at a parameter its declaration does not admit: {verdicts:?}",
        );
    }
}

/// A count carried on a term and used to *index* must be checked, not assumed.
///
/// Reduction runs on a type position before typing has reached it, so the functions that walk it cannot rest on an invariant typing would establish.
///
/// **Reduction.** `step_apply` opens a lambda's telescope at the application's arguments. `Telescope::open` asserts, so an application that does not saturate its lambda would **abort the walk**; it is stuck instead, which is the conservative direction twice over: reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than killing every other verdict. `certify_definitions` is documented as walking to the end with each verdict independent of the others, and an abort is what makes that false.
///
/// **Synthesis needs no leg.** `synth_neutral`'s partial-application arm opens the head type's telescope at the arguments supplied, and each entry that remains is still under its own mark, so there is no vector beside the telescope to drift and this fixture keeps the lambda case alone.
///
/// It is not reachable from a surface program — `curios-elab` emits saturated applications — and what is at stake is a program's fault aborting the certifier where an `Error` belongs.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`]. It is the direction that matters: reduction must still fire on a well-formed application, and a guard that simply stopped reducing would pass every witness here while breaking every program.
///
/// With the declared type typed rather than classified, `infer` refuses the lambda case as `Arity { expected: 2, actual: 1 }` — naming the defect — where reduction going stuck alone would leave a generic `Unclassified`. Both verdicts are accepted here, since which rule reaches the fault first is not what the fixture pins.
#[test]
fn a_count_a_term_carries_is_refused_rather_than_indexed_with() {
    let verdicts = text_refusals(
        r#"
let /f: (a: #NatType, b: #NatType) -> Type =
    (a: #NatType, b: #NatType) => #NatType;

let /held: /f(3) =
    3;
"#,
    );

    assert!(
        held_miscounted(&verdicts, Counted::Arguments, 2, 1),
        "a lambda applied to fewer arguments than it binds: the term was indexed at a count nothing checked: {verdicts:?}",
    );
}

/// The control for the fixture above: a lambda applied to exactly its binders still reduces, so the type position it stands in is classified.
#[test]
fn a_saturated_application_in_a_type_position_is_accepted() {
    // `f(3, 4)` reduces to `Nat`, so `held : f(3, 4) = 3` is an ordinary well-typed item.
    let verdicts = text_refusals(
        r#"
let /f: (a: #NatType, b: #NatType) -> Type =
    (a: #NatType, b: #NatType) => #NatType;

let /held: /f(3, 4) =
    3;
"#,
    );

    assert_eq!(
        verdicts,
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
    let (two, none) = ("mk(a, b)", "mk()");
    for (label, text, counted, expected, actual) in [
        (
            "an arm binding two components of a one-component payload",
            fill!(ARM, arm = two),
            Counted::ArmBinders,
            1,
            2,
        ),
        (
            "an arm binding none of a one-component payload",
            fill!(ARM, arm = none),
            Counted::ArmBinders,
            1,
            0,
        ),
        (
            "a recursive application short of its binders",
            r#"
let /held: (
    #{node = Rec}
    (let f: (a: #NatType, b: #NatType) -> Type = (a2: #NatType, b2: #NatType) => #NatType;
    f))(3) =
    3;
"#
            .to_string(),
            Counted::Arguments,
            2,
            1,
        ),
    ] {
        // The demonstrated defect is the *abort*: `certify_definitions` is documented as walking to the end with each verdict independent of the others, and a panic makes that false.
        let verdicts = catch_unwind(AssertUnwindSafe(|| text_refusals(&text)))
            .unwrap_or_else(|_| panic!("{label}: reduction aborted the walk instead of refusing"));

        assert!(
            held_miscounted(&verdicts, counted, expected, actual),
            "{label}: the module was not refused for its count: {verdicts:?}",
        );
    }
}

/// The control for the arm half: an arm binding exactly its constructor's payload still reduces, so the type position it computes is classified.
#[test]
fn an_arm_matching_its_payload_still_reduces() {
    let one = "mk(a)";

    assert_eq!(
        text_refusals(&fill!(ARM, arm = one)),
        Vec::new(),
        "an arm binding exactly its payload was refused",
    );
}

/// A nominal occurrence's parameters and indices are read by every rule that consults its declaration, so they are typed.
///
/// `at.rs` states the discipline: an occurrence is meaningful only once what it carries has been checked against what the declaration declares. The universe instance and the parameter and index *counts* are checked behind the handle; that each argument inhabits the domain the arity states it at is the third thing, and typing's. Counts are the boundary's job because no typing rule reads a length; a *shape* is typing's, and this one had no rule at all.
///
/// The forgery is what reading one unestablished buys. `Eq(@True)(0, 1)` is admitted as a type — `Sort::of` consults the declaration for its `result_sort` and hands back `Prop`, having checked two counts and nothing else — although `0` and `1` are `Nat`s standing in a domain the declaration says is `True`. It is then *inhabited*, and by the rule working correctly: `induct_type_args` compares the indices at the declared domain, that domain is `Prop`-sorted, and proof irrelevance discharges both without looking, so `refl(True, qed())` subsumes into it. From there every step is ordinary. Eliminating the forged equation at the goal `(Held(0)) -> Held(1)`, whose indices the arm reads at its case — where the same gap lets them, typed `True`, stand in `Held`'s `Nat` index — yields `(Held(0)) -> Held(1)`, and `Held(1)` is uninhabited by construction, so the vacuous elimination coverage licenses (its only constructor targets `0`, which the `Nat` peel clashes against `1`) proves `False`.
///
/// No surface program reaches it — `curios-elab` elaborates a nominal occurrence as an application against the arity's telescope and checks every argument — which is why this is built by hand.
///
/// `Eq`'s parameter is at `Prop`, which `True` inhabits, so what refuses the forgery is the indices' typing and nothing about the parameter: at `Type 1`, `True` itself would be refused, and the indices never asked.
///
/// Its control is [`an_indexed_occurrence_at_a_well_typed_index_is_accepted`], which keeps the same family at an index that genuinely inhabits `Nat`: without it, refusing every indexed occurrence would pass this.
#[test]
fn a_nominal_occurrence_types_its_arguments() {
    let verdicts = text_refusals(&fill!(
        r#"${EQUALITY}
induct /False: pub Prop
end

induct /Held: (n: #NatType) -> pub Type
| yes(): (0)
end

let /forged: #{node = InductType} /Eq(#{node = InductType} /True())(0, 1) =
    #{node = Variant} /Eq/refl(@#{node = InductType} /True(), #{node = Variant} /True/qed());

let /cast: (w: #{node = InductType} /Held()(0)) -> #{node = InductType} /Held()(1) =
    #{goal = (w: #{node = InductType} /Held()(0)) -> #{node = InductType} /Held()(1)}
    match /forged
    | refl(z) => (w2: #{node = InductType} /Held()(z)) => w2
    end;

let /held: #{node = InductType} /Held()(1) =
    /cast(#{node = Variant} /Held/yes());

let /boom: #{node = InductType} /False() =
    #{goal = #{node = InductType} /False()} match /held end;
"#,
        EQUALITY
    ));

    assert!(
        a_nat_index_at_true(&verdicts, "forged"),
        "the certifier accepted an occurrence whose indices do not inhabit their domain: {verdicts:?}",
    );
}

/// The control: an index that really is a `Nat` still types, so the guard above rejects a wrong argument rather than every argument.
#[test]
fn an_indexed_occurrence_at_a_well_typed_index_is_accepted() {
    let verdicts = text_refusals(
        r#"
induct /Held: (n: #NatType) -> pub Type
| yes(): (0)
end

let /held: #{node = InductType} /Held()(0) =
    #{node = Variant} /Held/yes();
"#,
    );

    assert_eq!(verdicts, Vec::new());
}

/// The same bogus occurrence, smuggled past [`a_nominal_occurrence_types_its_arguments`] through a Σ field.
///
/// Typing an occurrence's arguments closes the route only where something *types* the occurrence, so a type former's parts must be typed too: a `FuncType` or a `TupleType` answered with `Sort::of`, which classifies each domain — consulting a declaration for its sort and checking nothing else — would be the second, weaker way to accept a type `curios-cert/README.md` says this crate does not have.
///
/// Classified alone, `{Eq(@True)(0, 1)}` would be admitted, and the projection rule would hand the field's declared type straight back: `v.0` a scrutinee at the forged equation, and the rest of [`a_nominal_occurrence_types_its_arguments`]' derivation unchanged. The codomain half is the same shape — `(Nat) -> Eq(@True)(0, 1)`, with an application handing the codomain back — so both a `Proj` and an `Apply` reach it.
#[test]
fn a_bogus_occurrence_behind_a_tuple_field_is_refused() {
    // v : {Eq(True, 0, 1)} = (refl(True, qed()))
    let verdicts = text_refusals(&fill!(
        r#"${EQUALITY}
let /v: {b: #{node = InductType} /Eq(#{node = InductType} /True())(0, 1)} =
    (#{node = Variant} /Eq/refl(@#{node = InductType} /True(), #{node = Variant} /True/qed()),);
"#,
        EQUALITY
    ));

    assert!(
        a_nat_index_at_true(&verdicts, "v"),
        "the certifier accepted a bogus occurrence standing as a tuple field type: {verdicts:?}",
    );
}

/// A refusal names the types the way the program that produced them wrote them.
///
/// A refusal written faithfully keeps Core's own spelling — fully qualified paths, every parameter positional — which is right for a term printed in isolation and wrong for a message a reader has to recognize their own program in. The spelling a refusal is reported under (`curios-surface`'s `Spelling::for_refusal`) supplies the two axes that fix it: globals shortened against the module's symbol table, and a nominal family's implicit parameters marked from the type constructor's declared plicities.
///
/// Universe instances are deliberately left alone; see `Error::rendered`.
#[test]
fn a_refusal_shortens_names_and_marks_implicit_parameters() {
    // `struct Box(@A : Type)`: one implicit parameter, so a use site writes `Box(Nat)` and never supplies it positionally.
    let (definitions, _) = put()
        .read("struct /demo/Box/Box(@A: Type): pub Type {}")
        .unwrap_or_else(|unread| panic!("{unread:?}"));
    let (_, entry) = put()
        .read("#{node = StructType} /demo/Box/Box(#NatType)")
        .unwrap_or_else(|unread| panic!("{unread:?}"));
    let refusal = Error::Mismatch {
        inferred: Box::new(entry.expect("the text closes with its term").body),
        expected: Box::new(term("#NatType")),
    };

    assert_eq!(
        refusal.rendered(&Spelling::for_refusal(&definitions, &[], &SYNTAX)),
        "expected `#NatType`, found `#{node = StructType} Box(@#NatType)`"
    );
    // The faithful rendering keeps the qualified path and drops the mark, which is what makes the axes worth supplying.
    assert_eq!(
        refusal.rendered(&Spelling::default()),
        "expected `Nat`, found `/demo/Box/Box(Nat)`"
    );
}

/// The same forgery, smuggled in as a `List` split's carrier element type, which the split states and the walk reads by converting it with its scrutinee's: `Eq(@True)(v, w)` over two `Nat`s converts with `Eq(@True)(qed(), qed())`, so the cons arm's head stands at the forged equation, and eliminating it at `(Held(v)) -> Held(w)` reads both indices at its case. Every body states its binders, and the refl arm's `Held(z)` is a `Nat` index typed at `True`, which refuses it; typed where the split states it, the element type is refused first, a `Nat` at `True`. The empty arm is ill-typed and withdrawn, the scrutinee computing to a cons.
#[test]
fn a_bogus_occurrence_behind_a_carriers_element_type_is_refused() {
    let verdicts = text_refusals(&fill!(
        r#"${EQUALITY}
induct /False: pub Prop
end

induct /Held: (n: #NatType) -> pub Type
| yes(): (0)
end

let /idl: (l: #ListType(#{node = InductType} /Eq(#{node = InductType} /True())(#{node = Variant} /True/qed(), #{node = Variant} /True/qed()))) ->
    #ListType(#{node = InductType} /Eq(#{node = InductType} /True())(#{node = Variant} /True/qed(), #{node = Variant} /True/qed())) =
    (l: #ListType(#{node = InductType} /Eq(#{node = InductType} /True())(#{node = Variant} /True/qed(), #{node = Variant} /True/qed()))) => l;

let /cast: (v: #NatType, w: #NatType) -> (u: #{node = InductType} /Held()(v)) -> #{node = InductType} /Held()(w) =
    (v: #NatType, w: #NatType) =>
        #{
            goal = (u: #{node = InductType} /Held()(v)) -> #{node = InductType} /Held()(w),
            cases = {carrier = {elem = #{node = InductType} /Eq(#{node = InductType} /True())(v, w)}}}
        match /idl(#List(#{node = InductType} /Eq(#{node = InductType} /True())(#{node = Variant} /True/qed(), #{node = Variant} /True/qed()), [#{node = Variant} /Eq/refl(@#{node = InductType} /True(), #{node = Variant} /True/qed())]))
        | [] => 0
        | [h, ..t] =>
            #{goal = (u: #{node = InductType} /Held()(v)) -> #{node = InductType} /Held()(w)}
            match h
            | refl(z) => (u2: #{node = InductType} /Held()(z)) => u2
            end
        end;

let /held: #{node = InductType} /Held()(1) =
    /cast(0, 1)(#{node = Variant} /Held/yes());

let /boom: #{node = InductType} /False() =
    #{goal = #{node = InductType} /False()} match /held end;
"#,
        EQUALITY
    ));

    assert!(
        refuses(&verdicts, Some("cast"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "the certifier accepted a forged equation standing as a carrier's element type: {verdicts:?}",
    );
}
