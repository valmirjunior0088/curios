//! What a goal reports — its solution, its pin, its scope and its batch. The candidates suggested to fill one are `suggestion_tests`'.

use {crate::*, curios_text::RootSource};

use super::test_support::*;

#[test]
fn a_goal_batch_classifies_as_incomplete_and_a_hard_error_as_failure() {
    // The typed split the CLI's exit codes rest on: a written-goal batch is incomplete development state, a type mismatch a hard failure.
    let goals = with_entrypoint_type("let m : /std/Nat = ?; m", Some("/std/Nat"));
    assert!(matches!(
        compile_with_prelude(DEFAULT_STEP_BUDGET, &goals, &RootSource::none(), |_| {}),
        Err(CompileError::Incomplete(_))
    ));

    let mismatch = with_entrypoint_type("let bad : /std/Nat = true; bad", Some("/std/Nat"));
    assert!(matches!(
        compile_with_prelude(DEFAULT_STEP_BUDGET, &mismatch, &RootSource::none(), |_| {}),
        Err(CompileError::Failure(_))
    ));
}

#[test]
fn solved_goal_reports_its_solution() {
    // `id ? 5`: the type argument `?` is solved to `Nat` from the value `5` (`id ? x`) — but a written goal never compiles: the module still elaborates fully, then zonk reports what it determined.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        id(?, 5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("? : Type"), "unexpected error: {error}");
    assert!(
        error.contains("? =") && error.contains("Nat"),
        "unexpected error: {error}"
    );
}

#[test]
fn pinned_through_the_expected_type_reports_the_pin() {
    // `id ? true` checked against `/std/Bool`: the turnaround pins the type argument `?` to `Bool` through the expected type (a type-level pin), and the goal report names that solution.
    let source = r#"
        use /std/{Bool};
        let id(A : Type, a : A) -> A = a;
        id(?, true)
    "#;

    let error = compile(source, Some("/std/Bool")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(
        error.contains("? =") && error.contains("Bool"),
        "unexpected error: {error}"
    );
}

#[test]
fn unconstrained_goal_reports_undetermined() {
    // `let m : Nat = ? in m`: nothing constrains the value of `?`, so the goal report shows its type but no solution.
    let source = r#"
        use /std/{Nat};
        let m : Nat = ?;
        m
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("? : Nat"), "unexpected error: {error}");
    // No `? =` clause: nothing determined the goal.
    assert!(!error.contains("? ="), "unexpected error: {error}");
}

#[test]
fn report_includes_the_local_scope() {
    // The goal sits under `x`'s binder, so the Γ frozen at its birth — just that binder — appears in the report.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        f(1)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("x : Nat"), "unexpected error: {error}");
}

#[test]
fn goal_in_synthesis_position_reports_a_meta_type() {
    // A bare `?` with nothing to check against: a fresh metavariable stands in as its type, so the goal still reaches zonk's report (instead of dying with `CannotInfer` during elaboration) and shows the undetermined stand-in.
    //
    // The synthesis position is a typeless local `let`, not the entrypoint tail: the tail is always *checked* now — against the fixture's stated type here, against `Io({})` in a real program — so it can no longer host a term with nothing to check against.
    let source = r#"
        let anything = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("? : ?"), "unexpected error: {error}");
    // No `? =` clause: nothing determined the goal.
    assert!(!error.contains("? ="), "unexpected error: {error}");
}

#[test]
fn several_goals_report_together_in_declaration_order() {
    // Two written goals in different declarations: one elaboration reports both — in declaration order — instead of stopping at the first.
    let source = r#"
        use /std/{Nat, Bool};
        let m : Nat = ?;
        let b : Bool = ?;
        m
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
    let nat = error.find("? : Nat").expect("the Nat goal is reported");
    let bool_ = error.find("? : Bool").expect("the Bool goal is reported");
    assert!(nat < bool_, "goals out of declaration order: {error}");
}

#[test]
fn item_and_entrypoint_tail_goals_share_one_batch() {
    // A goal in a declaration and a goal in the entrypoint tail: both land in the same report.
    let source = r#"
        use /std/{Nat};
        let m : Nat = ?;
        /std/Nat/add(m, ?)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
}

#[test]
fn solved_and_unsolved_goals_share_one_batch() {
    // An unconstrained goal and a solved one: the batch keeps each entry's own verdict — no solution line for the first, `? = Nat` for the second.
    let source = r#"
        use /std/{Nat};
        let id(A : Type, a : A) -> A = a;
        let m : Nat = ?;
        id(?, 5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
    assert!(error.contains("? : Nat"), "unexpected error: {error}");
    assert!(error.contains("? : Type"), "unexpected error: {error}");
    assert!(error.contains("? ="), "unexpected error: {error}");
}

#[test]
fn each_goal_in_a_batch_names_its_binders_as_written() {
    // Two items each bind `n`. The rename map is per report, so both goals say `n`; a batch-wide map suffixed the second `n2` — a collision with a binder from a goal this one cannot see.
    let source = r#"
        use /std/{Nat};
        let first(n : Nat) -> Nat = ?;
        let second(n : Nat) -> Nat = ?;
        /std/print("")
    "#;

    let error = compile(source, None).unwrap_err();

    assert_eq!(
        error.matches("  n : Nat").count(),
        2,
        "unexpected error: {error}"
    );
    assert!(!error.contains("n2"), "unexpected error: {error}");
}

#[test]
fn types_spell_operators_as_infix_not_witness_projections() {
    // The concept-dispatch rebuild (`a + b` ≙ a witness projection call — `elaborate_infix`) folds back to its source spelling in reports, nested operands parenthesized, and no anonymous witness name leaks.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq((1 + 2) * 3, 9) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("(1 + 2) * 3"), "unexpected error: {error}");
    assert!(!error.contains("witness"), "unexpected error: {error}");
}

#[test]
fn a_hole_where_a_congruences_function_belongs_reports_as_a_goal_with_its_obligation() {
    // Pasting the refinement above: `?f(double(p))` against `double(p) + 2` is a metavariable-headed application against a value, which has no imitation to try but no refutation either — a constant solution could exist. It used to fall through the structural match to a hard `type mismatch`, telling the author the program was wrong; then, parked, it survived the drain as a postponed-conversion error. Now a survivor held up by written goals alone is the goals' own report: the batch names the hole, its type, and — as `? such that` lines — the conversions it has to make true, which is what tells the author `f` sends `double(p)` to `double(p) + 2`. A program with a goal in it never compiles, so the surrendered conversion is never unchecked; the classification is incomplete, not failure.
    let source = r#"
        use /std/{Nat, Eq};
        let double(n : Nat) -> Nat = match n | 0 => 0 | p + 1 => double(p) + 2 end;
        let double_correct(n : Nat) -> Eq(double(n), n * 2) =
            match n : (m) => Eq(double(m), m * 2)
            | 0 => Eq/refl()
            | p + 1; ih => Eq/cong(?, ih)
            end;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(
        error.contains("? : (Nat) -> Nat"),
        "unexpected error: {error}"
    );
    assert!(
        error.contains("? such that ?(double(p)) \u{2261} double(p) + 2"),
        "unexpected error: {error}"
    );
    assert!(
        !error.contains("type mismatch"),
        "unexpected error: {error}"
    );
    assert!(
        !error.contains("cannot decide"),
        "unexpected error: {error}"
    );
    // `subst`'s result `P(y)` meets any goal once `ih` fills its proof slot; an undecided fit of a parameter-headed result is refused.
    assert!(!error.contains("Eq/subst("), "vacuous fit offered: {error}");

    let entrypoint = with_entrypoint_type(source, Some("/std/Nat"));
    assert!(matches!(
        compile_with_prelude(
            DEFAULT_STEP_BUDGET,
            &entrypoint,
            &RootSource::none(),
            |_| {}
        ),
        Err(CompileError::Incomplete(_))
    ));
}

#[test]
fn typecheck_rejects_a_goal() {
    // `zonk` is included in the fast path, so a written goal is still reported — type-checking is fully validated even though lowering is skipped.
    let error = typecheck(
        r#"
        use /std/{Nat};
        let m : Nat = ?;
        m
        "#,
        Some("/std/Nat"),
    )
    .unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
}

/// A refusal beside a written goal is the refusal's exit and both reports: the refused item's first, then the goal at its occurrence.
#[test]
fn a_goal_beside_a_refusal_classifies_as_mixed() {
    let mixed = with_entrypoint_type(
        "let bad : /std/Nat = true; let m : /std/Nat = ?; m",
        Some("/std/Nat"),
    );
    let Err(error) = compile_with_prelude(DEFAULT_STEP_BUDGET, &mixed, &RootSource::none(), |_| {})
    else {
        panic!("a refused item compiles nothing");
    };

    let CompileError::Mixed { reports, failures } = error else {
        panic!("mixed, got {error:?}");
    };
    assert_eq!(failures, 1);
    assert_eq!(reports.len(), 2, "{reports:?}");
    assert!(
        reports[0].message.contains("bad:"),
        "{}",
        reports[0].message
    );
    assert!(
        reports[1].message.starts_with("goal `?`"),
        "{}",
        reports[1].message
    );
}
