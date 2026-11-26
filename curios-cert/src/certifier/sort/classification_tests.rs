//! The sort a type is classified at: intrinsics, universes, nominals, functions and records, a hypothesis and an instance, and the shapes refused rather than guessed.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::{Erased, test_support::*},
    curios_common::fill,
    curios_core::{Category, Cost, Level, ReduceError, UniverseContext},
};

/// `P : Prop` and `D : Type`, each a family with no case: a base type at a known sort, which only the registry can say a nominal type is.
const BASES: &str = r#"
induct /P: pub Prop
end

induct /D: pub Type
end
"#;

#[test]
fn an_intrinsic_type_sits_at_level_zero() {
    let mut certifier = certifier();

    assert_eq!(
        Sort::of(&mut certifier, &term("#NatType")),
        Ok(Sort::Type(Level::zero())),
    );
}

/// `Type u : Type (u + 1)`, and `Prop u : Type (u + 1)` beside it. Both are the sort of a universe, not the universe itself.
#[test]
fn a_universe_is_one_level_above_itself() {
    let mut certifier = certifier();

    assert_eq!(
        Sort::of(&mut certifier, &term("Type")),
        Ok(Sort::Type(
            Level::zero().succ().expect("level zero succeeds")
        )),
    );
    assert_eq!(
        Sort::of(&mut certifier, &term("Prop")),
        Ok(Sort::Type(
            Level::zero().succ().expect("level zero succeeds")
        )),
    );
}

#[test]
fn a_nominal_types_sort_is_the_one_its_declaration_states() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    assert_eq!(
        Sort::of(&mut certifier, &term("#{node = InductType} /P()")),
        Ok(Sort::Prop(Level::zero()))
    );
    assert_eq!(
        Sort::of(&mut certifier, &term("#{node = InductType} /D()")),
        Ok(Sort::Type(Level::zero())),
    );
}

/// Π into a proposition is a proposition however large its domain, which is what makes `(n : Nat) -> P(n)` erasable.
#[test]
fn a_function_into_a_proposition_is_a_proposition() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    let pi = term("(n: #NatType) -> #{node = InductType} /P()");

    assert_eq!(Sort::of(&mut certifier, &pi), Ok(Sort::Prop(Level::zero())));
}

#[test]
fn a_function_into_data_takes_the_join_of_its_parts() {
    let mut certifier = certifier();

    let pi = term("(n: #NatType) -> #NatType");

    assert_eq!(Sort::of(&mut certifier, &pi), Ok(Sort::Type(Level::zero())));
}

/// A record of nothing but propositions is a proposition — but the *empty* record is unit, not a proposition. It is what an effect returns, so calling it a proposition would erase a value the program still needs.
#[test]
fn a_record_of_propositions_is_a_proposition_but_the_empty_one_is_unit() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    let all_props = term("{a: #{node = InductType} /P(), b: #{node = InductType} /P()}");
    assert_eq!(
        Sort::of(&mut certifier, &all_props),
        Ok(Sort::Prop(Level::zero()))
    );

    assert_eq!(
        Sort::of(&mut certifier, &term("{}")),
        Ok(Sort::Type(Level::zero()))
    );
}

/// One relevant field is enough to make the whole record relevant: its inhabitants are distinguishable, so irrelevance must not apply.
#[test]
fn one_relevant_field_makes_a_record_relevant() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    let mixed = term("{a: #{node = InductType} /P(), b: #NatType}");

    assert_eq!(
        Sort::of(&mut certifier, &mixed),
        Ok(Sort::Type(Level::zero()))
    );
}

/// A list *of* proofs is not a proposition: it has a length, so two lists are distinguishable even when their elements are not.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    let list = term("#ListType(#{node = InductType} /P())");

    assert_eq!(
        Sort::of(&mut certifier, &list),
        Ok(Sort::Type(Level::zero()))
    );
}

/// A recursive group's claim about its own member is not what decides that member's sort.
///
/// [`Sort::of`] reduces before it classifies, and forcing unfolds a projection to the member's body — so the sort reported is the body's, read honestly, rather than the `member_type` the group asserts. That matters because the projection arm answers through [`synth_neutral`](super::synth_neutral), which is a *lookup*: it reads the group's claim and checks nothing, and it has to stay that way, since certifying there would re-enter the very group whose member types are being sorted.
///
/// So the group below claims its member is a proposition while its body is a `Type`-sorted family, and the honest answer is the body's. Reduction is the whole defense at this arm, which is why it is pinned here rather than left to be re-derived from the reduction rules.
#[test]
fn a_groups_claim_about_its_member_does_not_decide_that_members_sort() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    // The claim is that the member is a proposition; the body is a family the registry puts at `Type 0`.
    let projected = term("#{node = Rec} (let T: Prop = #{node = InductType} /D(); T)");

    assert_eq!(
        Sort::of(&mut certifier, &projected),
        Ok(Sort::Type(Level::zero())),
        "the group's `Prop` claim was trusted over the sort its member actually reduces to",
    );
}

/// The type of a hypothesis is read off the binder it was opened at, which is how a `Prop`-typed variable is recognized as a proof.
#[test]
fn a_hypothesis_takes_the_sort_of_the_type_it_was_opened_at() {
    let mut certifier = certifier();

    // `h : Prop`: the hypothesis names a proposition, so its sort is the universe it was opened at.
    let (_, hypothesis) = opened(&mut certifier, "(h: Prop) => h");
    let sort = Sort::of(&mut certifier, &hypothesis);

    assert_eq!(sort, Ok(Sort::Prop(Level::zero())));
}

/// Refusing beats guessing: an unregistered nominal type has no sort the certifier can determine, and inventing one is the unsound direction.
#[test]
fn an_unregistered_nominal_type_is_refused_rather_than_guessed() {
    let mut certifier = certifier();

    assert_eq!(
        Sort::of(&mut certifier, &term("#{node = InductType} /Missing()")),
        Err(Error::Undeclared(nominal("Missing"))),
    );
}

/// An intrinsic *value* is not a type, so nothing classifies it — again a refusal rather than a default.
#[test]
fn a_value_in_type_position_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        Sort::of(&mut certifier, &term("true")),
        Err(Error::Unclassified(_)),
    ));
}

// === The `Instance` arm, which reads its levels ==========================
//
// `Sort::of` classifies a universe instance as the neutral it is, at the levels the occurrence states. `step_instance` leaves an `Instance` stuck only over a global declared without a body, while every defined scheme instantiates its body at the instance before the arm can see it.

/// A defined scheme never reaches the arm: `step_instance` instantiates its body at the stated instance first, so the sort lands at the *instance's* level — the second clause of the entry's Assumes, probed at the sort. `A<u> : Type (u + 1) = Type u` at `u := 1` classifies as `Type 2`, with no scheme parameter surviving to be captured.
#[test]
fn a_universe_instance_over_a_defined_scheme_classifies_at_the_instances_level() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "#{universe_context = {parameter_count = 1}}\nlet /A: #{level = u + 1} Type = #{level = u} Type;",
    );

    let two = Level::constant(2);

    assert_eq!(
        Sort::of(&mut certifier, &term("#{universes = {levels = [1]}} /A")),
        Ok(Sort::Type(two)),
    );
}

/// The one route that brings a scheme head to the arm — a polymorphic global declared without a body, a permanent neutral — classifies at the instance's level, as [`synth_neutral`](super::synth_neutral) reads the same head inside a spine: `A<u> : Type (u + 1)` is `Type 1` at `u := 0` and `Type 2` at `u := 1`. A classification that dropped the levels would answer the scheme's own sort with its parameter still in it, one answer for both instances. Nothing on the compile path declares a bodiless global — the module walk `define`s every item with its real body, and a unit in scope answers with the body of every `Let` and `Rec` — so only [`Certifier::declare`] builds this state, and no text writes it.
#[test]
fn a_universe_instance_over_a_bodiless_scheme_classifies_at_the_instances_level() {
    let mut certifier = certifier();
    certifier
        .declare(
            &nominal("A"),
            &term("#{level = u + 1} Type"),
            &UniverseContext {
                parameter_count: 1,
                quality_count: 0,
                constraints: Vec::new(),
            },
        )
        .expect("a name is declared once");

    assert_eq!(
        Sort::of(&mut certifier, &term("#{universes = {levels = [0]}} /A")),
        Ok(Sort::Type(Level::constant(1))),
    );
    assert_eq!(
        Sort::of(&mut certifier, &term("#{universes = {levels = [1]}} /A")),
        Ok(Sort::Type(Level::constant(2))),
    );
}

// === A proposition known by its head ====================================
//
// `Sort::of` answers `Prop` for an application of a declaration that lands there before it reduces anything, and takes no other head's word. The fixtures below hold the answer, that nothing is run for it, and what it declines.

/// `F : (Nat) -> ${sort}`, defined as a function that decides something of its argument before it answers `${body}` either way: applied, it is no type former, and reducing the application evaluates the argument.
const FAMILY: &str = r#"
let /F: (n: #NatType) -> ${sort} =
    (n: #NatType) =>
        #{goal = ${sort}}
        match #NatLt(n, 10)
        | false => ${body}
        | true => ${body}
        end;
"#;

/// An argument no budget this file's certifiers hold could reduce: a tower of powers, which the family's application would evaluate before it applied.
const UNAFFORDABLE: &str = "#NatPow(10, #NatPow(10, 10))";

/// An application of a definition declared into `Prop` is a proposition, and nothing is run to learn it: its argument is one the budget cannot reduce, and the question is answered.
#[test]
fn an_application_of_a_declaration_into_prop_is_a_proposition_unreduced() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);
    declared(
        &mut certifier,
        &fill!(FAMILY, sort = "Prop", body = "#{node = InductType} /P()"),
    );
    let applied = term(&fill!("/F(${UNAFFORDABLE})", UNAFFORDABLE));

    assert_eq!(
        Sort::of(&mut certifier, &applied),
        Ok(Sort::Prop(Level::zero()))
    );
    assert_eq!(
        erased_half(&mut certifier, &applied),
        Ok(Some(Erased::Proof))
    );
}

/// A declaration into `Type` is reduced as before: its reduct says its level, and the argument is run to reach it.
#[test]
fn an_application_of_a_declaration_into_type_is_classified_by_its_reduct() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);
    declared(
        &mut certifier,
        &fill!(FAMILY, sort = "Type", body = "#{node = InductType} /D()"),
    );

    assert_eq!(
        Sort::of(&mut certifier, &term("/F(3)")),
        Ok(Sort::Type(Level::zero())),
    );
    assert!(
        Sort::of(
            &mut certifier,
            &term(&fill!("/F(${UNAFFORDABLE})", UNAFFORDABLE))
        )
        .is_err(),
        "the argument was not run",
    );
}

/// A group's claim about its member is not taken for an application of that member either: the group below claims `(Nat) -> Prop` over a body that answers a `Type`-sorted family, and the application is classified by what it reduces to. [`a_groups_claim_about_its_member_does_not_decide_that_members_sort`] holds the bare projection.
#[test]
fn a_groups_claim_does_not_decide_the_sort_of_its_members_application() {
    let mut certifier = certifier();
    declared(&mut certifier, BASES);

    let applied = term(
        "(#{node = Rec} (let F: (n: #NatType) -> Prop = (n: #NatType) => #{node = InductType} /D(); F))(3)",
    );

    assert_eq!(
        Sort::of(&mut certifier, &applied),
        Ok(Sort::Type(Level::zero())),
        "the group's `Prop` claim was trusted over the sort its member's application reduces to",
    );
}

/// Depth is bought with stack: a tower of negations five thousand deep is classified on the default test-thread stack, where the walk descends into a Π's domain to classify it and the memo is written only where each classification returns. Each level is a frame the budget pays for, so the certifier is given two frames a level.
///
/// What this guards is the shape `.claude/rules/rust.md` asks of every walk over data-shaped depth. A sort walk that recursed natively would die on the segment it was entered on rather than refuse, and the soundness board requires the implementation to fail closed — a stack overflow is acceptable only where it refuses.
///
/// Mutation-checked: with `Sort::of`'s `recurse` bracket removed, the run prints `has overflowed its stack` and aborts, which is a death and not a refusal.
#[test]
fn a_tower_of_negations_is_classified_on_the_default_stack() {
    const DEPTH: u32 = 5_000;
    let mut certifier = Certifier::new(2 * u64::from(DEPTH) * Cost::FRAME.get(), SYNTAX);
    declared(&mut certifier, "induct /False: pub Prop\nend");

    let falsehood = "#{node = InductType} /False()";
    let tower = (0..DEPTH).fold(falsehood.to_string(), |tower, _| {
        fill!("((x: ${tower}) -> ${falsehood})", tower, falsehood)
    });

    assert!(
        matches!(Sort::of(&mut certifier, &term(&tower)), Ok(Sort::Prop(_))),
        "a Π into a proposition is a proposition at every depth",
    );
}

/// A type that unfolds to a list of itself runs the budget out on the levels its classification descends: a list's sort is its element's, `b`'s reduct is `List(b)` again, and every reduction after the first is a memo hit, which spends nothing. Curios/Core meets such a member where a sibling is checked at it (`rec b: Type = #ListType(b) and c: b = #List(b, [])`), which the elaborator's own budget refuses before the certifier sees it.
///
/// Mutation-checked: with `Sort::of` entering no level, the walk grows its stack until the process is killed.
#[test]
fn a_type_that_unfolds_to_a_list_of_itself_runs_the_budget_out() {
    let mut certifier = certifier();

    assert!(matches!(
        Sort::of(
            &mut certifier,
            &term("#{node = Rec} (let b: Type = #ListType(b); b)")
        ),
        Err(Error::Reduce(ReduceError::Exhausted {
            category: Category::Depth,
            ..
        })),
    ));
}

/// An index no scope binds names no binder to read a type off, so the lookup a stuck spine's sort is read through answers nothing and the sort is refused, never aborted on.
#[test]
fn a_bound_index_is_refused_rather_than_aborted_on() {
    let mut certifier = certifier();

    assert!(matches!(
        Sort::of(&mut certifier, &term("#{binding = Bound(0)} _")),
        Err(Error::Unclassified(_)),
    ));
}
