//! The arm rule at a goal: one over a hypothesis the scrutinee types, a lying goal, and its two preconditions.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::ReduceError,
};

/// `F : (Nat) -> Type` with `a : F(0)` and `b(m : Nat) : F(m + 1)`, declared in `certifier` with `P : (n : Nat, x : F(n)) -> Type`.
fn family(certifier: &mut Certifier) {
    declare(
        certifier,
        "F",
        "i: #NatType",
        "Type",
        &["a(): (0)", "b(m: #NatType): (#Nat(Succ(1, m)))"],
    );
    scrutinee_family(certifier, "F");
}

/// `#{goal = goal} match scrutinee | a() => body | b(m) => body end`: the two arms of an elimination of `F`, each `body`, at an ambient goal.
fn ambient(scrutinee: &str, goal: &str, body: &str) -> String {
    fill!(
        "#{goal = ${goal}} match ${scrutinee} | a() => ${body} | b(m) => ${body} end",
        goal,
        scrutinee,
        body
    )
}

/// `w : Q(n, x, h)` with `h : P(n, x)`: stated at the goal, each arm inhabits `Q(n, x, h)` specialized — `Q(0, a(), h)` with `h` shadowed at `P(0, a())` — and `w` does so once shadowed itself. A family over the index and scrutinee binders, `(i, s) => Q(i, s, h)`, could not be typed under its own binders, since `h : P(n, x)` is not a `P(i, s)`, which is what would force a convoy.
///
/// `Q : (n : Nat, x : F(n), p : P(n, x)) -> Type` is opaque: a type former whose last parameter is typed by the scrutinee and its index, so a goal stating it about a hypothesis can be written where that hypothesis stands and nowhere else.
#[test]
fn a_goal_over_a_hypothesis_typed_by_the_scrutinee_needs_no_family() {
    let mut certifier = certifier();
    family(&mut certifier);
    opaque(
        &mut certifier,
        "Q",
        &fill!(
            "(n: #NatType, x: ${f}, p: /P(n, x)) -> Type",
            f = family_at("F", "n")
        ),
    );
    let [matched, goal] = under(
        &mut certifier,
        &fill!(
            "n: #NatType, x: ${f}, h: /P(n, x), w: /Q(n, x, h)",
            f = family_at("F", "n")
        ),
        &fill!(
            "${ambient}, /Q(n, x, h)",
            ambient = ambient("x", "/Q(n, x, h)", "w")
        ),
    );

    assert_eq!(
        infer(&mut certifier, &matched).expect("the goal is inhabited by each arm"),
        goal,
    );
}

/// An ambient goal is a claim like any other: an arm that does not inhabit it is the mismatch it is.
#[test]
fn a_lying_ambient_goal_is_refused() {
    let mut certifier = certifier();
    family(&mut certifier);
    let [lying] = under(
        &mut certifier,
        &fill!("n: #NatType, x: ${f}", f = family_at("F", "n")),
        &fill!("${lying},", lying = ambient("x", "/P(n, x)", "0")),
    );

    assert!(matches!(
        infer(&mut certifier, &lying),
        Err(Error::Mismatch { .. })
    ));
}

/// An ambient result over an expression scrutinee — what a `let`-bound scrutinee read by value, or an argument substituted into the variable form, leaves behind — reads the goal's occurrences of that expression as the case: `P(0, f(n))` over `match f(n)` is `P(0, a())` in the `a` arm, which `h : P(0, f(n))` inhabits there through the case equation the arm records for `f(n)`, exactly as it would under a family. A goal that never names the expression is the same goal in every arm.
#[test]
fn an_ambient_result_over_an_expression_replaces_its_occurrences() {
    let mut certifier = certifier();
    family(&mut certifier);
    // `f : (Nat) -> F(0)` applied to a parameter is an expression scrutinee typed at index `0`, so only the `a` arm is reachable, and `b`'s targets clash with the actual index.
    opaque(
        &mut certifier,
        "f",
        &fill!("(k: #NatType) -> ${f}", f = family_at("F", "0")),
    );
    let [naming, constant, goal] = under(
        &mut certifier,
        "n: #NatType, h: /P(0, /f(n))",
        &fill!(
            "${naming}, ${constant}, /P(0, /f(n))",
            naming = ambient("/f(n)", "/P(0, /f(n))", "h"),
            constant = ambient("/f(n)", "#NatType", "0")
        ),
    );

    // The goal names the expression, so the `a` arm is checked at `P(0, a())`, and the elimination's type is the goal as written.
    assert_eq!(
        infer(&mut certifier, &naming).expect("the occurrence stands for the case"),
        goal
    );

    // A goal that never names the expression is the same goal in every arm, and the elimination's type is that goal.
    assert_eq!(
        infer(&mut certifier, &constant).expect("a constant goal is inhabited by each arm"),
        term("#NatType")
    );
}

/// `#{goal = ${goal}} match k | 0 => 0 | pred + 1 => ${cons} end`, over a `Nat` `k`.
const AMBIENT_ON_K: &str = "#{goal = ${goal}} match k | 0 => 0 | pred + 1 => ${cons} end,";

/// A case split's arms are each checked at its case value, as a `Bool` arm is. The same match, its arms each inhabiting the goal, is inferred at the goal.
#[test]
fn an_ambient_case_split_is_checked_at_each_case() {
    let mut certifier = certifier();
    let [split] = under(
        &mut certifier,
        "k: #NatType",
        &fill!(AMBIENT_ON_K, goal = "#NatType", cons = "pred"),
    );

    assert_eq!(
        infer(&mut certifier, &split).expect("each arm inhabits the goal at its case"),
        term("#NatType")
    );
}

/// The split's arms are still held to the goal: an identity arm that does not inhabit it is refused as the mismatch it is.
#[test]
fn an_ambient_case_split_whose_arm_misses_the_goal_is_refused() {
    let mut certifier = certifier();
    let [split] = under(
        &mut certifier,
        "k: #NatType",
        &fill!(AMBIENT_ON_K, goal = "#BoolType", cons = "true"),
    );

    assert!(matches!(
        infer(&mut certifier, &split),
        Err(Error::Mismatch { .. })
    ));
}

/// A budget failure is not a malformed goal, on the way from a goal's type to its sort as on the way to the type. `T : U`, where `U` is a `Bool` elimination into `Type` over a count two nested list folds take as long as their lengths' product to reach, so the type of what the result states is in hand at once and reading *it* as a sort is what the budget cannot afford. Reporting `NotAGoal` whatever that read was refused for would send a reader to a goal with nothing wrong with it. The control is the same elimination under a budget that affords the count, which is what says the refusal was the budget's.
#[test]
fn a_result_whose_sort_the_budget_cannot_reach_is_refused_for_the_budget() {
    let attempt = |budget: u64| {
        let mut certifier = Certifier::new(budget, SYNTAX);
        let [elimination] = under(
            &mut certifier,
            &fill!(
                "b: #BoolType, T: #{goal = Type} match #NatEql(${count}, 0) | false => Type | true => Type end, x: T",
                count = nested_count(224)
            ),
            "#{goal = T} match b | false => x | true => x end,",
        );

        infer(&mut certifier, &elimination)
    };

    assert!(
        matches!(
            attempt(20_000),
            Err(Error::Reduce(ReduceError::Exhausted { .. }))
        ),
        "the sort read ran out of budget and was reported as something else",
    );
    attempt(1_000_000_000)
        .unwrap_or_else(|error| panic!("a budget that affords the count refused: {error:?}"));
}

/// Two list folds over `side` zeros each, the inner one summing into the outer's accumulator: a `Nat` closed term weak-head reduction takes as many steps as `side` squared to reach, zero.
fn nested_count(side: usize) -> String {
    let list = fill!(
        "#List(#NatType, [${zeros}])",
        zeros = vec!["0"; side].join(", ")
    );

    fill!(
        "#ListFold(#NatType, #NatType, ${list}, 0, (a: #NatType, b: #NatType) => #ListFold(#NatType, #NatType, ${list}, b, (c: #NatType, d: #NatType) => #NatAdd(d, c)))",
        list
    )
}

/// An arm reduction never takes is not typed: over the literal `false` an elimination is its `false` arm wherever it stands, and nothing reads the other. Here the other holds a `Bool` where the result is `Nat`, as the result an arm is checked at leaves a proof standing under a guard that is no longer one.
///
/// Mutation-checked: with every arm typed, the elimination is refused.
#[test]
fn an_arm_under_a_scrutinee_that_is_another_case_is_not_typed() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term(&nat_by("false", WRONG))),
        Ok(term("#NatType"))
    );
}

/// The arm a literal scrutinee does take is typed as every arm is: over `true`, a `Bool` in the `true` arm of a `Nat` result is refused.
#[test]
fn the_arm_a_literal_scrutinee_takes_is_typed() {
    let mut certifier = certifier();

    assert!(infer(&mut certifier, &term(&nat_by("true", WRONG))).is_err());
}

/// `not : (Bool) -> Bool`, as the function it is.
const NOT: &str =
    "((c: #BoolType) => #{goal = #BoolType} match c | false => true | true => false end)";

/// The `Nat` elimination of `scrutinee` whose `false` arm is `0` and whose `true` arm is `in_true`.
fn nat_by(scrutinee: &str, in_true: &str) -> String {
    fill!(
        "#{goal = #NatType} match ${scrutinee} | false => 0 | true => ${in_true} end",
        scrutinee,
        in_true
    )
}

/// A `Bool` where a `Nat` is wanted: what an arm holds that no rule types as it is written.
const WRONG: &str = "true";

/// A scrutinee that is no case as it stands has every arm typed, and a refused one is no arm of the elimination only where the scrutinee computes to another case. Over a variable the `Bool` in the `true` arm of a `Nat` result is refused. Over `not(true)`, which computes to `false`, that arm is one no reduction takes. Over `not(false)` it is the arm taken, and its refusal stands.
///
/// Mutation-checked: with no refused arm withdrawn, the second elimination is refused; with one withdrawn whatever its scrutinee computes to, the third is accepted.
#[test]
fn an_arm_refused_under_a_scrutinee_that_computes_to_another_case_is_not_typed() {
    let mut certifier = certifier();
    let applied = |argument: &str| fill!("${not}(${argument})", not = NOT, argument);

    let stuck = certifier.scoped(|certifier| {
        let [stuck] = under(
            certifier,
            "c: #BoolType",
            &fill!("${stuck},", stuck = nat_by("c", WRONG)),
        );
        infer(certifier, &stuck)
    });
    assert!(stuck.is_err());

    assert_eq!(
        infer(&mut certifier, &term(&nat_by(&applied("true"), WRONG))),
        Ok(term("#NatType"))
    );
    assert!(infer(&mut certifier, &term(&nat_by(&applied("false"), WRONG))).is_err());
}

/// An arm is typed under the equations around it as its own case restates them, and one of those may be what computes to another case. The outer arm assumes `not(c)` is `true`; inside it the `true` arm of a split on `c` restates that as `not(true)`, which names no local and computes to `false`, so the inner arm is one no reduction takes and the `Bool` it holds is not typed. The inner `false` arm restates it as `not(false)`, which holds, and the same `Bool` there is refused.
///
/// Mutation-checked: with the restated equations left out of what an arm assumed, the first elimination is refused.
#[test]
fn an_arm_refused_under_an_equation_its_case_closed_is_not_typed() {
    let nested = |in_false: &str, in_true: &str| {
        let mut certifier = certifier();
        let inner = fill!(
            "#{goal = #NatType} match c | false => ${in_false} | true => ${in_true} end",
            in_false,
            in_true
        );
        let [nested] = under(
            &mut certifier,
            "c: #BoolType",
            &fill!(
                "${nested},",
                nested = nat_by(&fill!("${not}(c)", not = NOT), &inner)
            ),
        );

        infer(&mut certifier, &nested)
    };

    assert_eq!(nested("0", WRONG), Ok(term("#NatType")));
    assert!(nested(WRONG, "0").is_err());
}

/// A scrutinee that names a local and computes to another case all the same leaves its arm one no reduction takes: `and(not(true), d)` is `false` whatever `d` is, and the `Bool` in its `true` arm is not typed. Its equation is recorded, naming `d`, and answers nothing the arm holds.
#[test]
fn an_arm_refused_under_a_scrutinee_that_names_a_local_and_computes_is_not_typed() {
    let mut certifier = certifier();
    let scrutinee = fill!(
        "((a: #BoolType, b: #BoolType) => #{goal = #BoolType} match a | false => false | true => b end)(${not}(true), d)",
        not = NOT
    );
    let [eliminated] = under(
        &mut certifier,
        "d: #BoolType",
        &fill!("${eliminated},", eliminated = nat_by(&scrutinee, WRONG)),
    );

    assert_eq!(infer(&mut certifier, &eliminated), Ok(term("#NatType")));
}

/// A budget that cannot afford to compute the scrutinee withdraws no refusal: the scrutinee counts a literal down to `false`, its `true` arm holds a `Bool` where the result is `Nat`, and the elimination is refused for the budget where the count is past it and accepted where it is not.
#[test]
fn a_budget_that_cannot_compute_the_scrutinee_withdraws_no_refusal() {
    let counted = "(#{node = Rec} (let down: (n: #NatType) -> #BoolType = (n: #NatType) => #{goal = #BoolType} match n | 0 => false | pred + 1 => down(pred) end; down(50000)))";
    let attempt = |budget: u64| {
        let mut certifier = Certifier::new(budget, SYNTAX);

        infer(&mut certifier, &term(&nat_by(counted, WRONG)))
    };

    assert!(matches!(
        attempt(20_000),
        Err(Error::Reduce(ReduceError::Exhausted { .. }))
    ));
    assert_eq!(attempt(1_000_000_000), Ok(term("#NatType")));
}

/// A term typed in an arm no reduction takes is recorded where it is typed again, as fully as a term typed once: the arm's refusal is withdrawn, and what its walk recorded is taken back with the typings remembered beside it. The `true` arm under `not(true)` applies `fun (x : Nat) => x` to a `Bool`, refused once the lambda is typed; its scrutinee names no local, so no equation is in force and the lambda's typing is one of the declaration's. Typed again after the elimination, the lambda records its domain, a type position, as it does typed alone.
///
/// Mutation-checked: with the typings kept past a refusal, the lambda typed again records no position.
#[test]
fn a_term_typed_in_an_arm_no_reduction_takes_is_recorded_when_typed_again() {
    let identity = "((x: #NatType) => x)";
    let judged = |certifier: &mut Certifier| {
        infer(certifier, &term(identity)).expect("the lambda types");
        let (positions, failure) = certifier.take_checked();
        assert!(failure.is_none());

        positions.len()
    };

    let mut withdrawn = certifier();
    let eliminated = nat_by(
        &fill!("${not}(true)", not = NOT),
        &fill!("${identity}(${wrong})", identity, wrong = WRONG),
    );
    assert_eq!(
        infer(&mut withdrawn, &term(&eliminated)),
        Ok(term("#NatType"))
    );
    withdrawn.take_checked();

    assert_eq!(judged(&mut withdrawn), judged(&mut certifier()));
}
