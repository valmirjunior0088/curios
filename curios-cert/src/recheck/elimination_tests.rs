//! Goals and result sorts, and the large-elimination guard a vacuous elimination must not skip.

use {
    super::test_support::*,
    crate::*,
    curios_common::fill,
    curios_core::{Atom, Quality, Subterm},
};

/// `Two` at `${sort}`, with `a` and `b`, and `Held(t : Two) : Type` whose one case `mk` targets `Two/a()`; neither family's cases are defined. `False` is declared beside them.
const TWO_HELD: &str = r#"
induct /False: pub Prop
end

induct /Two: pub ${sort}
| a()
| b()
end

induct /Held: (t: #{node = InductType} /Two()) -> pub Type
| mk(): (#{node = Variant} /Two/a())
end
"#;

/// `held : Held(Two/b()) = Held/mk()` and `forged : False` its empty elimination, closing with `forged`.
const FORGED_FROM_HELD: &str = r#"
let /held: #{node = InductType} /Held()(#{node = Variant} /Two/b()) =
    #{node = Variant} /Held/mk();

let /forged: #{node = InductType} /False() =
    #{goal = #{node = InductType} /False()} match /held end;

/forged
: #{node = InductType} /False()
"#;

/// `False`, `Held` with `${cases}` all targeting `Two/a()`, and `vacuous : (x : Held(Two/b())) -> False` their empty elimination.
const SHADOWED: &str = r#"
induct /False: pub Prop
end

induct /Two: pub Type
| a()
| b()
end

induct /Held: (t: #{node = InductType} /Two()) -> pub Type
${cases}
end

let /vacuous: (x: #{node = InductType} /Held()(#{node = Variant} /Two/b())) ->
    #{node = InductType}
    /False() =
    (x: #{node = InductType} /Held()(#{node = Variant} /Two/b())) =>
        #{goal = #{node = InductType} /False()} match x end;
"#;

/// `False`, `Held : Prop` with `qed`, and `Pin(n : Nat) : Prop` with `yes() : (0)` and `no() : (1)`.
const PIN: &str = r#"
induct /False: pub Prop
end

induct /Held: pub Prop
| qed()
end

induct /Pin: (n: #NatType) -> pub Prop
| yes(): (0)
| no(): (1)
end
"#;

/// `P(i : Nat) : Prop` with two cases at `0`, and `extract : (p : P(0)) -> Nat` eliminating it into `Nat` at a goal that states `${sort}` as its sort, `match 0 | 0 => Nat | _ => Nat end`.
const EXTRACTED: &str = r#"
induct /P: (i: #NatType) -> pub Prop
| mk(): (0)
| mk2(): (0)
end

let /extract: (p: #{node = InductType} /P()(0)) -> #NatType =
    (p: #{node = InductType} /P()(0)) =>
        #{goal = #{goal = ${sort}} match 0 | 0 => #NatType | _ => #NatType end} match p
        | mk() => 7
        | mk2() => 9
        end;
"#;

/// [`EXTRACTED`]'s honest goal over `P` declared at one universe parameter it is irrelevant in, with the cases `${cases}`, eliminated at the instance `${level}`.
const LEVELLED: &str = r#"
#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /P: (i: #NatType) -> pub Prop
${cases}
end

let /extract: (p: #{node = InductType, universes = {levels = [${level}]}} /P()(0)) -> #NatType =
    (p: #{node = InductType, universes = {levels = [${level}]}} /P()(0)) =>
        #{goal = #{goal = Type} match 0 | 0 => #NatType | _ => #NatType end} match p
        ${arms}
        end;
"#;

/// `False`, and `Pinned(n : Nat, m : Nat) : Prop` whose one case `mk(x : Nat)` targets `(x, x)`.
const PINNED: &str = r#"
induct /False: pub Prop
end

induct /Pinned: (n: #NatType, m: #NatType) -> pub Prop
| mk(x: #NatType): (x, x)
end
"#;

/// [`PINNED`] and `bad : (p : Pinned(0, 1)) -> False`, its sole-arm elimination through the arm `${arm}`.
const PINNED_ARM: &str = r#"${PINNED}
let /bad: (p: #{node = InductType} /Pinned()(0, 1)) -> #{node = InductType} /False() =
    (p: #{node = InductType} /Pinned()(0, 1)) =>
        #{goal = #{node = InductType} /False(), cases = {stated = #{node = InductType} /Pinned()(0, 1)}}
        match p | ${arm} => Prop end;
"#;

/// `Fixed(i : Type 7) : Type 8` whose one case targets `Type 3`, and `read : (V : ${variable}, h : Fixed(V)) -> {}` whose arm binds `Type 2` at `V`: a refinement of `V` to `Type 3` fits where `V`'s type holds it. The goal names no `V`, so the arm reads `V` as its refinement leaves it.
const REFINED: &str = r#"
induct /Fixed: (i: #{level = 7} Type) -> pub #{level = 8} Type
| mk(): (#{level = 3} Type)
end

let /read: (V: ${variable}, h: #{node = InductType} /Fixed()(V)) -> {} =
    (V: ${variable}, h: #{node = InductType} /Fixed()(V)) =>
        #{goal = {}} match h | mk() => (let y: V = #{level = 2} Type; ()) end;
"#;

/// [`REFINED`] with `Fixed` at `Type 10`, `Universe.{u} : Type (u + 2) = Type (u + 1)`, and `read` declared at `u = 8`, its variable at `${variable}`.
const REFINED_AT: &str = r#"
induct /Fixed: (i: #{level = 10} Type) -> pub #{level = 11} Type
| mk(): (#{level = 3} Type)
end

#{universe_context = {parameter_count = 1}}
let /Universe: #{level = u + 2} Type =
    #{level = u + 1} Type;

#{universe_context = {parameter_count = 1, constraints = [8 <= u, u <= 8]}}
let /read: (V: ${variable}, h: #{node = InductType} /Fixed()(V)) -> {} =
    (V: ${variable}, h: #{node = InductType} /Fixed()(V)) =>
        #{goal = {}} match h | mk() => (let y: V = #{level = 2} Type; ()) end;
"#;

/// A declared result sort that merely *reduces* to `Prop` silences the index guard that reads it syntactically.
///
/// `Sort::of` decides irrelevance by reducing `result_sort`, and so do `check_signature` and `check_non_informative`. Index inversion does not: its `Prop`-valued guard — the rule that stops a proposition's constructors from being told apart, because irrelevance says they are the same value — matches `Subterm::Sort(curios_core::Sort { quality: Quality::Prop, level: _ })` on the nose. A family declared at `((s : Type 0) => s)(Prop)` is therefore a proposition to every consumer except the one whose silence is unsound.
///
/// The consequence below is the vacuous-elimination route. `Two` is that family, with constructors `a` and `b`; `Held(t : Two)` has one constructor targeting `Two/a()`. Irrelevance makes `Two/a()` and `Two/b()` convertible, so `Held/mk()` inhabits `Held(Two/b())` — and then eliminating that value with *no arms at all* is accepted, because inversion clashes `Two/b()` against `Two/a()` and reports the only constructor impossible. The goal is `False`.
///
/// Without the literal-sort clause the walk would certify `let forged : False` from a value that exists. No `.crs` file can spell the sort — the surface grammar admits only the literal keywords `Type` and `Prop` after a declaration's `:` — and the elaborator builds no such entry, so only a fixture reaches it.
///
/// The control is the same module with `Two` at `Type 0`, where the clash is genuine and the empty elimination must stay accepted — general vacuous elimination is how an indexed family rules its impossible cases out, and refusing it is how this hole would be shut with a brick.
#[test]
fn a_result_sort_that_only_reduces_to_a_sort_is_refused() {
    let sort = "(((s: Type) => s)(Prop))";
    let verdicts = text_verdicts(&fill!(
        "${TWO_HELD}${FORGED_FROM_HELD}",
        TWO_HELD = fill!(TWO_HELD, sort),
        FORGED_FROM_HELD
    ));

    assert!(
        refuses(&verdicts, Some("Two"), |error| matches!(
            error,
            Error::NotASort(_)
        )),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The same route with `Two` declared at the literal `Prop`, which the inversion's tag test reads: irrelevance makes `Two/a()` and `Two/b()` one value, so `Held/mk()` is at `Held(Two/b())` and its arm cannot be excused.
#[test]
fn an_empty_elimination_where_a_propositions_two_constructors_differ_is_refused() {
    let sort = "Prop";
    let verdicts = text_verdicts(&fill!(
        "${TWO_HELD}${FORGED_FROM_HELD}",
        TWO_HELD = fill!(TWO_HELD, sort),
        FORGED_FROM_HELD
    ));

    assert!(
        refuses(&verdicts, Some("forged"), |error| *error
            == Error::MissingArm {
                family: global("Held"),
                tag: Atom::from("mk"),
            }),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_at_a_relevant_index_is_still_accepted() {
    let sort = "Type";
    let verdicts = text_verdicts(&fill!(
        r#"${TWO_HELD}
let /vacuous: (h: #{node = InductType} /Held()(#{node = Variant} /Two/b())) ->
    #{node = InductType}
    /False() =
    (h: #{node = InductType} /Held()(#{node = Variant} /Two/b())) =>
        #{goal = #{node = InductType} /False()} match h end;
"#,
        TWO_HELD = fill!(TWO_HELD, sort)
    ));

    assert!(
        verdicts.is_empty(),
        "an arm the index targets genuinely clash with was refused: {verdicts:?}",
    );
}

/// Coverage holds the *sole-arm* elimination too, which is the shape reduction fires rather than leaves standing.
///
/// A closed match with one arm, no catch-all and no payload named is its arm's body with its scrutinee unevaluated (`Cases::unread`, which the certifier's `whnf`, the closed machine and `curios-elab`'s reducer each state), so the arm's body stands at the elimination's own type whatever the scrutinee is. The vacuous elimination above is the shape `check_induct_arms` deliberately skips the large-elimination guard for; this is the shape whose arm the guard does reach and whose omission coverage must still decide.
///
/// `Pin(1)` below is inhabited by `Pin/no()`, and the `yes` arm's target `0` clashes with the index, so `specialize` reports the case impossible and no rule types that arm. Only coverage is left: `no` has no arm and is not impossible at `Pin(1)`. Excused, `forged(Pin/no())` would reduce to the `yes` arm's body — the sort `Prop`, a term nothing typed standing at `False`.
///
/// Only a fixture reaches the certifier's own loop with this shape: `curios-elab` refuses the same sole-arm match with `MatrixUncovered` while the module is still being built, so no surface program leaves one behind and `curios`'s `tests::rules::index` reads the elaborator's matrix rather than this.
///
/// Mutation-checked, which is what makes this a forgery rather than a refusal: with the coverage loop's `MissingArm` suppressed, the walk returns **zero** verdicts over this module, so the entry is certified at `False`.
#[test]
fn a_sole_arm_elimination_must_still_rule_out_the_constructors_it_omits() {
    let verdicts = text_verdicts(&fill!(
        r#"${PIN}
let /forged: (p: #{node = InductType} /Pin()(1)) -> #{node = InductType} /False() =
    (p: #{node = InductType} /Pin()(1)) =>
        #{goal = #{node = InductType} /False(), cases = {stated = #{node = InductType} /Pin()(1)}}
        match p | yes() => Prop end;

/forged(#{node = Variant} /Pin/no())
: #{node = InductType} /False()
"#,
        PIN
    ));

    assert!(
        refuses(&verdicts, Some("forged"), |error| *error
            == Error::MissingArm {
                family: global("Pin"),
                tag: Atom::from("no"),
            }),
        "the certifier accepted a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The control: the same two constructors at `Pin(0)`, where `no` is impossible and `yes` is the arm reduction takes. The sole-arm elimination must stay accepted, since it is the form a cast along a proof is written in; refusing every match with one arm is how the refusal above would be reached with a brick.
#[test]
fn a_sole_arm_elimination_whose_omissions_are_ruled_out_is_still_accepted() {
    let verdicts = text_verdicts(&fill!(
        r#"${PIN}
let /cast: (p: #{node = InductType} /Pin()(0), h: #{node = InductType} /Held()) ->
    #{node = InductType}
    /Held() =
    (p: #{node = InductType} /Pin()(0), h: #{node = InductType} /Held()) =>
        #{goal = #{node = InductType} /Held(), cases = {stated = #{node = InductType} /Pin()(0)}}
        match p | yes() => h end;
"#,
        PIN
    ));

    assert!(
        verdicts.is_empty(),
        "a cast along a proof whose other case the index rules out was refused: {verdicts:?}",
    );
}

/// A family that declares one tag twice, where the shadowed entry is the one a coverage decision is about.
///
/// A tag is the elimination key and the runtime index, and every lookup resolves one by *first match*. So a repeat hides a constructor instead of adding one, and the rules that walk `constructors` entry by entry answer about the first one once per entry. Coverage is where that shows: `Held(t : Two)` below declares `mk() : (Two/a())` and then `mk() : (Two/b())`, and asked whether each constructor is impossible at `Two/b()`, a rule resolving both entries to the first would compare `Two/a()` against `Two/b()` twice and report the family empty at an index its own second entry constructs at.
///
/// What it would certify is `vacuous : (x : Held(Two/b())) -> False` — a refutation of a constructor the same declaration states. That is not a closed inhabitant of `False`: construction resolves by first match too, so `let h : Held(Two/b()) = Held/mk()` is a `Mismatch` of `Held(Two/a())` against `Held(Two/b())`, and the shadowed entry has no inhabitant to hand the refutation. Refusing the repeated tag keeps the elimination rule from being sound only because an unrelated rule is lossy in the same direction.
///
/// No `.crs` file reaches it: `curios-module` refuses both spellings with ``` `mk` is already declared in this module ```, whether or not the representation is public. Nor does a module: a family's cases are made under distinct tags ([`curios_core::Constructors`]), so Core text's reader refuses the second `mk` where it stands, and the certifier is never handed the family.
///
/// The control is the same shape at two *distinct* tags, both targeting `Two/a()`. It must stay accepted: ruling impossible cases out is what an indexed family's vacuous elimination is for, and a rule that refused every multi-constructor declaration, or every empty elimination over one, would shut this hole with a brick.
#[test]
fn a_family_that_declares_one_tag_twice_is_refused() {
    let cases = "| mk(): (#{node = Variant} /Two/a())\n| mk(): (#{node = Variant} /Two/a())";
    let read = put().read(&fill!(SHADOWED, cases)).map(|_| ());

    assert!(
        matches!(
            &read,
            Err(crate::test_support::Unread::Read(curios_surface::ReadError {
                refusal: curios_surface::Refusal::RepeatedCase(tag),
                span: Some(_),
            })) if tag == "mk"
        ),
        "a family was read with two cases under one tag: {read:?}",
    );
}

#[test]
fn a_vacuous_elimination_over_two_distinct_tags_is_still_accepted() {
    let cases = "| mk(): (#{node = Variant} /Two/a())\n| mk2(): (#{node = Variant} /Two/a())";
    let verdicts = text_verdicts(&fill!(SHADOWED, cases));

    assert!(
        verdicts.is_empty(),
        "an elimination whose every constructor genuinely clashes was refused: {verdicts:?}",
    );
}

/// A goal that lies about its own sort must not skip the large-elimination guard.
///
/// `Sort::of` classifies a stuck `match` used as a type by reading its **goal** — "its goal is the sort, which every arm shares", as the arm says. Nothing in `Sort::of` establishes that: the arms are checked against the goal only where a judgment meets the elimination, and the goal may claim whatever it likes.
///
/// The guard is what a lie buys. `guard_large_elimination` **returns immediately when the result is not relevant** — eliminating a proposition into a proposition needs no condition. So a goal `#{goal = Prop} match 0 | 0 => Nat | _ => Nat end` reads as `Prop`, the guard would be skipped, and the very same goal *reduces to `Nat`*, so the arms typecheck as data.
///
/// `P` below is a two-constructor proposition, the shape the guard exists to refuse: `mk()` and `mk2()` both inhabit `P(0)`, proof irrelevance identifies them, and `extract` maps them to `7` and `9`. This is the route reached through the classifier rather than through the guard's own condition.
///
/// Not reachable from a surface program: `curios-elab` builds a match's goal and checks the arms against it, so it never emits one that lies, which is why this is built here.
///
/// Closed by checking the goal itself: `goal_sort` types it and requires it to land in a sort, which is Coq's `type_of_case` clause, and hands the sort it derives to the guard so nothing re-reads the claim. The lying goal does not typecheck — its arms inhabit `Type` while it states `Prop` — so the refusal names that rule rather than the guard it would bypass.
///
/// The control is [`an_honest_goal_still_refuses_the_large_elimination`], the same module with the goal stating `Type` — where the goal is honest, the guard does fire, and the elimination is refused for the reason it should be. Together they pin that what a lie would skip is the guard, and that closing the bypass does not close the guard itself.
#[test]
fn a_goal_that_misreports_its_sort_does_not_skip_the_large_elimination_guard() {
    let sort = "Prop";
    let verdicts = text_verdicts(&fill!(EXTRACTED, sort));

    assert!(
        refuses(&verdicts, Some("extract"), |error| matches!(
            error,
            Error::NotAGoal { .. }
        )),
        "the certifier eliminated a two-constructor proposition into `Nat`: {verdicts:?}",
    );
}

/// The control: the identical module whose goal states the `Type` its arms actually have. The guard sees a relevant result and refuses, which is what proves the fixture above is about the *classifier* rather than about eliminations in general.
#[test]
fn an_honest_goal_still_refuses_the_large_elimination() {
    let sort = "Type";
    let verdicts = text_verdicts(&fill!(EXTRACTED, sort));

    assert!(
        refuses(&verdicts, Some("extract"), |error| *error
            == Error::LargeElimination(global("P"))),
        "the guard did not fire even on an honest goal: {verdicts:?}",
    );
}

/// A goal is checked even where the elimination cannot run.
///
/// `check_induct_arms` skips the large-elimination guard for a vacuous elimination — no arms, no catch-all — and the reason is good: the coverage loop must then prove *every* constructor impossible at the scrutinee's indices, so the eliminated instance is uninhabited and discharging it into a relevant result leaks nothing. The question this fixture settles is whether the *goal* clause inherits that skip.
///
/// It does not, and the reason is not an exploit: no route from a vacuous elimination to a forged term is known, and the honest reading of that is the weak one — an attempt failed, which is not a proof that none exists. What makes the clause unconditional is that `infer` reads the elimination's **type** off the goal and hands it to the caller whether or not the elimination can run — so a goal nothing validated means a term whose type nothing validated, and `Sort::of` will classify it downstream. The module below is a vacuous elimination at an uninhabited `Held(Two/b())`, whose goal states `Prop` over arms inhabiting `Type`. What keeps the fixture pointed at the goal is the order `check` and `infer` run in, not the shape of the definition around it — see the note at the definition.
///
/// It also costs nothing. The clause sits in [`check_cases`](crate::infer) above the dispatch, where every `Cases` form shares one `goal` binding, so *not* running it here would mean pushing it down into `check_induct_arms`, guarding it with the vacuous condition, and duplicating it into the three intrinsic-carrier arms. Unconditional is the cheap implementation; the skip would be the deliberate exception.
#[test]
fn a_vacuous_elimination_still_has_its_goal_checked() {
    // `Two : Type 0`, so `a` and `b` genuinely clash and the elimination really is vacuous; `Held : (t : Two) -> Prop | mk() : (Two/a())`, a proposition, so the guard would be in play. The codomain is honest, and the ordering is what keeps this about the goal: `check` reaches `infer` before it can subsume, and `infer` runs `check_cases` — hence the goal clause — before the result is compared.
    let verdicts = text_verdicts(
        r#"
induct /Two: pub Type
| a()
| b()
end

induct /Held: (t: #{node = InductType} /Two()) -> pub Prop
| mk(): (#{node = Variant} /Two/a())
end

let /vacuous: (
    n: #NatType,
    s: #{node = InductType} /Held()(#{node = Variant} /Two/b())) -> #NatType =
    (n: #NatType, s: #{node = InductType} /Held()(#{node = Variant} /Two/b())) =>
        #{goal = #{goal = Prop} match n | 0 => #NatType | _ => #NatType end} match s end;
"#,
    );

    assert!(
        refuses(&verdicts, Some("vacuous"), |error| matches!(
            error,
            Error::NotAGoal { .. }
        )),
        "a vacuous elimination carried a goal nothing validated: {verdicts:?}",
    );
}

/// Every position at which a term stands as a *type* reaches the judgment, not the classifier — asserted at each position rather than argued once.
///
/// This crate has two ways to answer what sort a type has, and only one of them checks anything: `infer_sort` types a former's parts, `Sort::of` classifies them. `curios-cert/README.md` says the lookup is reached only where typing has run, and that sentence is a grep rather than a claim — a position reaching the classifier where the judgment belongs is the defect, and it can arrive at any position: a nominal occurrence's arguments, a type former's parts, a `Prop`-sorted declaration's domains.
///
/// A stuck `match` is what tells the two apart, because it is the one shape in Core that *states* its own sort instead of having one derived: `Sort::of` reads the goal, and the goal is a claim the term makes about itself. Where the judgment runs, `infer` reaches `goal_sort` and the arms are checked against the goal, so `#{goal = Prop} match b | false => Nat | true => Nat end` is refused by the arm — `Nat` at `Type 0` against the `Prop` the goal claims. Where only the classifier runs, that same term reads as a proposition and carries a `Nat`, which is the forgery [`a_proposition_may_not_carry_a_computed_relevant_field`] derives `False` from.
///
/// So the table below is one probe per position, each refused by that arm mismatch and by nothing else. The diagnostic is asserted rather than the mere presence of a verdict, because most of these types are uninhabitable while the lie stands: a body-driven refusal would name the body's own type and would pass a test that only counted verdicts.
///
/// Mutation-checked, which is what separates this from a fixture that asserts nothing: replacing `infer_telescope`'s `infer_type` with `Sort::of` — the exact weakening this guards against — makes "a lambda's domain annotation" come back with **zero** verdicts while every other row stays refused.
///
/// A declaration's own domains are the seventh position and are not repeated here; [`a_proposition_may_not_carry_a_computed_relevant_field`] holds that one, and holds it with the derivation rather than with the position alone.
#[test]
fn no_type_position_admits_a_lying_goal() {
    // `match b | false => Nat | true => Nat end` states a proposition and computes `Nat`, a type.
    let lying = "#{goal = Prop} match b | false => #NatType | true => #NatType end";
    for (position, text) in [
        (
            "a function type's domain",
            "let /probe: (b: #BoolType, x: ${lying}) -> {} =\n    (b: #BoolType, x: ${lying}) => ();",
        ),
        (
            "a function type's codomain",
            "let /probe: (b: #BoolType) -> ${lying} =\n    (b: #BoolType) => 0;",
        ),
        (
            "a tuple type's component",
            "let /probe: (b: #BoolType) -> {y: ${lying}} =\n    (b: #BoolType) => (0,);",
        ),
        (
            "a lambda's domain annotation",
            "let /probe: {} =\n    ((b: #BoolType, x: ${lying}) => ())(true, 0);",
        ),
        (
            "a let binding's declared type",
            "let /probe: (b: #BoolType) -> {} =\n    (b: #BoolType) =>\n        let y: ${lying} = 0;\n        ();",
        ),
        (
            "a rec member's declared type",
            "let /probe: (b: #BoolType) -> {} =\n    (b: #BoolType) =>\n        #{node = Rec}\n        (let y: ${lying} = 0;\n        ());",
        ),
    ] {
        let verdicts = text_verdicts(&fill!(text, lying));

        assert!(
            refuses(&verdicts, Some("probe"), |error| matches!(
                error,
                Error::Mismatch { inferred, expected }
                    if matches!(&***inferred, Subterm::Sort(curios_core::Sort { quality: Quality::Type, level: _ }))
                        && matches!(&***expected, Subterm::Sort(curios_core::Sort { quality: Quality::Prop, level: _ }))
            )),
            "{position} classified a lying goal instead of typing it: {verdicts:?}",
        );
    }
}

// The guard reads a family's declared sort and its constructors, never the instance eliminated: a two-constructor proposition is refused into `Nat` at either instance of a level it is irrelevant in, and a one-constructor one is admitted at either.
#[test]
fn an_irrelevant_level_changes_no_elimination() {
    let (cases, arms) = (
        "| mk(): (0)\n| mk2(): (0)",
        "| mk() => 7\n        | mk2() => 9",
    );
    let (case, arm) = ("| mk(): (0)", "| mk() => 7");
    for level in [0, 1] {
        let two = text_verdicts(&fill!(LEVELLED, cases, arms, level));
        assert!(
            refuses(&two, Some("extract"), |error| *error
                == Error::LargeElimination(global("P"))),
            "{two:?}"
        );

        let one = text_verdicts(&fill!(LEVELLED, cases = case, arms = arm, level));
        assert_eq!(one, Vec::new());
    }
}

// The scrutinee's index is `wrap` at another instance than `Ixd`'s target states, which conversion calls the same value: inversion reads the tag and the payloads and no level, so the arm is solved and stays mandatory.
#[test]
fn an_arm_is_not_excused_by_an_irrelevant_level() {
    let excused = |arms: &str| {
        text_verdicts(&fill!(
            r#"
#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /Wrap(A: #{level = u} Type): pub #{level = u} Type
| wrap(a: A)
end

#{universe_context = {parameter_count = 1}, variances = [Irrelevant]}
induct /Ixd(A: #{level = u} Type): (
    w: #{node = InductType, universes = {levels = [u]}} /Wrap(A)) -> pub #{level = u} Type
| mk(a: A): (#{node = Variant, universes = {levels = [u]}} /Wrap/wrap(@A, a))
end

let /read: (
    x:
        #{node = InductType, universes = {levels = [0]}}
        /Ixd(#NatType)(
            #{node = Variant, universes = {levels = [1]}} /Wrap/wrap(@#NatType, 3))) -> #NatType =
    (
        x:
            #{node = InductType, universes = {levels = [0]}}
            /Ixd(#NatType)(
                #{node = Variant, universes = {levels = [1]}} /Wrap/wrap(@#NatType, 3))) =>
        #{goal = #NatType} match x${arms} end;
"#,
            arms
        ))
    };

    let vacuous = excused("");
    assert!(
        refuses(&vacuous, Some("read"), |error| *error
            == Error::MissingArm {
                family: global("Ixd"),
                tag: Atom::from("mk"),
            }),
        "{vacuous:?}"
    );
    assert_eq!(excused(" | mk(b) => b"), Vec::new());
}

/// A metavariable in the certifier's input must be refused, though the Match-arm rule's gate reads it as closed.
///
/// `whnf`'s gate is `!term.has_local_free()`, where the closed machine's `accelerable` and `curios-elab`'s own `closed` also require no metavariable. So a match whose scrutinee is a metavariable and whose other parts name no local is closed to this reducer and open to the other two, and the closed branch of the sole-arm rule is the one with no comparison in it: `stated_payload` hands the payload over where `unread_payload` would put the stated instance beside the one the constructor builds and decline. What keeps the two from parting over a verdict is not the gate but the boundary in front of it — `Zonked::project` refuses a program carrying elaboration's own syntax, and `infer` refuses a `Metavar` with `NotCore` where a judgment meets one — and this holds that boundary, the fixture reaching the walk with `Zonked` bypassed as every fixture here does.
#[test]
fn a_metavariable_the_match_gate_reads_as_closed_is_still_refused() {
    let verdicts = text_verdicts(&fill!(
        r#"${PINNED}
let /bad: #{node = InductType} /False() =
    #{goal = #{node = InductType} /False(), cases = {stated = #{node = InductType} /Pinned()(0, 1)}}
    match #{id = 0, origin = Hole} ?
    | mk(x) => Prop
    end;
"#,
        PINNED
    ));

    assert!(
        refuses(&verdicts, Some("bad"), |error| matches!(
            error,
            Error::NotCore(_)
        )),
        "elaboration's own syntax must be refused where a judgment meets it: {verdicts:?}",
    );
}

/// An arm whose arity its constructor's payload denies must be refused, which is what keeps the sole-arm rule's payload from ever being opened at the wrong count.
///
/// The open branch of the certifier's rule filters on `payload.len() == arm.arity()` before it opens the arm, where `curios-elab`'s fires `unread()` ahead of that comparison. The two can only part on a module the certifier refuses outright, and this is the refusal: the payload a stated instance yields is as long as the constructor's signature, and `check_induct_arms` holds every arm's arity to that signature. The scrutinee is a binder, so the open branch is the one reduction takes.
#[test]
fn an_arm_whose_arity_its_constructor_denies_is_refused() {
    let arm = "mk(x, y)";
    let verdicts = text_verdicts(&fill!(PINNED_ARM, PINNED, arm));

    assert!(
        refuses(
            &verdicts,
            Some("bad"),
            miscounted(Counted::ArmBinders, 1, 2)
        ),
        "an arm must be held to its constructor's payload: {verdicts:?}",
    );
}

/// The control: the same elimination with the arm at its constructor's arity stays refused for the stated instance alone, never for the arity.
///
/// `Pinned(0, 1)` is a proposition no value stands at, so the arm the indices rule out is what coverage decides and the arity is no part of it. Reading the refusal above as the arity's needs this beside it, or a clause that refused every sole-arm elimination would pass the one and the other alike.
#[test]
fn an_arm_at_its_constructors_arity_is_not_refused_for_its_arity() {
    let arm = "mk(x)";
    let verdicts = text_verdicts(&fill!(PINNED_ARM, PINNED, arm));

    assert!(
        !verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::Arity {
                counted: Counted::ArmBinders,
                ..
            }
        )),
        "an arm at its constructor's arity is held to nothing about its arity: {verdicts:?}",
    );
}

/// An index refinement is kept only where its target inhabits the variable's own type, so an arm reads an outer variable as it stands wherever the bound does not hold.
#[test]
fn an_index_refinement_is_kept_only_where_its_target_fits_its_variable() {
    let variable = "#{level = 7} Type";
    let fitting = text_verdicts(&fill!(REFINED, variable));
    assert!(
        fitting.is_empty(),
        "a refinement whose target inhabits the variable's own type was dropped: {fitting:?}",
    );

    let variable = "Type";
    let collapsing = text_verdicts(&fill!(REFINED, variable));
    assert!(
        refuses(&collapsing, Some("read"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "a `Type 3` was refined into a variable of type `Type 0`: {collapsing:?}",
    );
}

/// A refinement's fit is decided at the levels the variable's own instance states, not at the reader's.
///
/// `a_published_value_sort_is_the_sort_its_instance_reaches` names `Judge::sort_below` through `curios-analysis`'s `fits_its_variable` as the other consumer of the level the ticket's cause carried, and builds no fixture for it. This is that fixture, reached the way a hand-written indexed family, match and inversion reach it.
///
/// The third module is what makes the first a demonstration rather than an assertion: it is the same reader with the binder's type spelled as the capture would have left it, `Type (q + 1)`, and it is accepted. So the level being read at the instance is the whole of what refuses the first, and nothing else in the module stands in its way.
#[test]
fn a_refinements_fit_is_decided_at_the_levels_its_variables_instance_states() {
    let variable = "#{universes = {levels = [0]}} /Universe";
    let collapsing = text_verdicts(&fill!(REFINED_AT, variable));
    assert!(
        refuses(&collapsing, Some("read"), |error| matches!(
            error,
            Error::Mismatch { .. }
        )),
        "a `Type 3` was refined into a variable the instance types at `Type 1`: {collapsing:?}",
    );

    let variable = "#{universes = {levels = [6]}} /Universe";
    assert_eq!(
        text_verdicts(&fill!(REFINED_AT, variable)),
        Vec::new(),
        "a refinement whose target inhabits the type the instance states was dropped",
    );

    let variable = "#{level = u + 1} Type";
    assert_eq!(
        text_verdicts(&fill!(REFINED_AT, variable)),
        Vec::new(),
        "the reading a capture would have left was refused for some other reason",
    );
}
