//! What a goal reports — its solution, its pin, its scope and its batch — and the candidates suggested to fill it.

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
fn a_computed_equality_goal_suggests_refl() {
    // The motivating base case: `? : Eq(0 + 0, 0 * 2)` — the indices unify through reduction, so the report suggests the complete candidate. The step case used to get none, its sides being distinct stuck terms; since a sum is a linear combination, `(p + 1) + (p + 1)` and `(p + 1) * 2` both reduce to `2 · p + 2`, so it is suggested there too — the whole theorem is computation now, which is what makes the fixture a probe of the suggestion and no longer of its filtering.
    let source = r#"
        use /std/{Nat, Eq};
        let double(n : Nat) -> Nat = n + n;
        let double_correct(n : Nat) -> Eq(double(n), n * 2) =
            match n : (m) => Eq(m + m, m * 2)
            | 0 => ?
            | p + 1; ih => ?
            end;
        double(21)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    // One refl line per arm. The step arm also offers `Eq/cong(?, ih)` from the imported `/std/Eq` — the hypothesis placed, the function open — which is why the count is of refl lines and not of every candidate line.
    assert_eq!(
        error.matches("\u{2248} Eq/refl()").count(),
        2,
        "unexpected error: {error}"
    );
}

#[test]
fn impossible_constructors_are_not_suggested() {
    // At `Vec(Nat, 0)` inversion refutes `cons` (a successor target clashes with `0`) and admits `nil` completely.
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
        end
        let v : Vec(Nat, 0) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} /Vec/nil()"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("cons"), "unexpected error: {error}");
}

#[test]
fn a_scope_binder_fitting_the_goal_is_suggested() {
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        f(1)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("? \u{2248} x"), "unexpected error: {error}");
}

#[test]
fn a_solved_goal_gets_no_suggestions() {
    // A suggestion beside a `? =` answer is noise; solved goals carry none.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        id(?, 5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("? ="), "unexpected error: {error}");
    assert!(!error.contains('\u{2248}'), "unexpected error: {error}");
}

#[test]
fn a_module_function_fitting_the_goal_is_suggested_with_pinned_arguments() {
    // The application-fit pool: `mk`'s output `Eq(n, n)` unifies with the goal `Eq(3, 3)`, pinning `n := 3` — and the pinned argument displays filled because the candidate is materialized before the transaction rolls back. Complete fits rank by pool order, so the constructor fit leads.
    let source = r#"
        use /std/{Nat, Eq};
        let mk(n : Nat) -> Eq(n, n) = Eq/refl();
        let claim : Eq(3, 3) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    assert!(error.contains("mk(3)"), "unexpected error: {error}");
    let refl = error.find("\u{2248} Eq/refl()").expect("refl suggested");
    let mk = error.find("mk(3)").expect("mk suggested");
    assert!(refl < mk, "complete pool order broken: {error}");
}

#[test]
fn an_application_fit_mentioning_a_scope_binder_is_suggested() {
    // The same fit as above, but the goal sits inside a function body and mentions its binder: `mk`'s output `Eq(n, n)` unifies with `Eq(k, k)` by `n := k`. The suggestion pass used to run on the bare context, so `n`'s metavariable was born closed and the solution `k` failed the solver's scope check — every application fit inside a function body silently vanished, and only closed goals like the one above ever saw one. The pass now assumes the goal's telescope into a frame first.
    let source = r#"
        use /std/{Nat, Eq};
        let mk(n : Nat) -> Eq(n, n) = Eq/refl();
        let claim(k : Nat) -> Eq(k, k) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("mk(k)"), "unexpected error: {error}");
}

#[test]
fn an_imported_lemma_is_suggested_with_its_proof_slot_filled_from_the_scope() {
    // Pool 5 and the scope fill together. `Eq/sym` is never mentioned by the program — it arrives through `use /std/{Eq}` — and its explicit slot is a proof the goal cannot pin; the output pins `x := k, y := 7`, and `h : Eq(k, 7)` is the one binder whose type then fits the slot. The complete fit leads; `Eq/cong(?, h)` follows as the refinement whose function is open. Spelled `Eq/sym`, the path the import resolves under, not the `/std/Eq/sym` Core holds.
    let source = r#"
        use /std/{Nat, Eq};
        let flip(k : Nat, h : Eq(k, 7)) -> Eq(7, k) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/sym(h)"),
        "unexpected error: {error}"
    );
    assert!(
        error.contains("? \u{2248} Eq/cong(?, h)"),
        "unexpected error: {error}"
    );
    let sym = error.find("Eq/sym(h)").expect("sym suggested");
    let cong = error.find("Eq/cong(?, h)").expect("cong suggested");
    assert!(sym < cong, "complete fit should lead: {error}");
}

#[test]
fn a_hypothesis_fills_an_imported_lemmas_proof_slot_under_an_open_function() {
    // The step case of a proof about a function the normalizer cannot unfold on a variable: `double(p + 1)` against `(p + 1) * 2` is not refl, and `Eq/cong`'s output `Eq(f(x), f(y))` is undecided on `f` alone once `ih` pins `x` and `y` — undecided on exactly the open slot, which is the advisory the report keeps. The vacuous `Eq/sym(?)` and `Eq/trans(?, ?)`, true of every equation, are not offered.
    let source = r#"
        use /std/{Nat, Eq};
        rec double(n : Nat) -> Nat = match n | 0 => 0 | p + 1 => double(p) + 2 end;
        let double_correct(n : Nat) -> Eq(double(n), n * 2) =
            match n : (m) => Eq(double(m), m * 2)
            | 0 => Eq/refl()
            | p + 1; ih => ?
            end;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/cong(?, ih)"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("Eq/sym("), "vacuous fit offered: {error}");
    assert!(!error.contains("Eq/trans("), "vacuous fit offered: {error}");
    assert!(
        !error.contains("Eq/refl()"),
        "refl cannot close this: {error}"
    );
}

#[test]
fn an_application_fit_nothing_pinned_is_not_suggested() {
    // `touch`'s output `Eq(y + 1, x + 1)` converts with `Eq(3, 3)` by pinning only hidden slots; its one explicit slot stays a hole with nothing in scope to fill it. `touch(?)` says no more than `touch` has an argument, so it is dropped — where `mk(3)`, whose explicit slot the goal pinned, is kept (see `a_module_function_fitting_the_goal_is_suggested_with_pinned_arguments`).
    let source = r#"
        use /std/{Nat, Eq};
        let touch(@x : Nat, @y : Nat, e : Eq(x, y)) -> Eq(y + 1, x + 1) = Eq/sym(Eq/cong((z) => z + 1, e));
        let claim : Eq(3, 3) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("touch("), "unpinned fit offered: {error}");
}

#[test]
fn a_suggested_imported_candidate_compiles_when_pasted() {
    // The paste-and-recheck contract for a pool-5 candidate: `Eq/sym(h)` as suggested in `an_imported_lemma_is_suggested_with_its_proof_slot_filled_from_the_scope`.
    let source = r#"
        use /std/{Nat, Eq};
        let flip(k : Nat, h : Eq(k, 7)) -> Eq(7, k) = Eq/sym(h);
        0
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());
}

#[test]
fn a_hole_where_a_congruences_function_belongs_reports_as_a_goal_with_its_obligation() {
    // Pasting the refinement above: `?f(double(p))` against `double(p) + 2` is a metavariable-headed application against a value, which has no imitation to try but no refutation either — a constant solution could exist. It used to fall through the structural match to a hard `type mismatch`, telling the author the program was wrong; then, parked, it survived the drain as a postponed-conversion error. Now a survivor held up by written goals alone is the goals' own report: the batch names the hole, its type, and — as `? such that` lines — the conversions it has to make true, which is what tells the author `f` sends `double(p)` to `double(p) + 2`. A program with a goal in it never compiles, so the surrendered conversion is never unchecked; the classification is incomplete, not failure.
    let source = r#"
        use /std/{Nat, Eq};
        rec double(n : Nat) -> Nat = match n | 0 => 0 | p + 1 => double(p) + 2 end;
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
fn an_import_is_offered_only_below_its_use() {
    // `use` binds from its own position to the end of its body. The goal above `use /std/{Eq}` cannot paste `Eq/sym(h)` — `Eq` is not a name there — so it is not offered; the same goal below it is.
    let source = r#"
        use /std/{Nat};
        let above(k : Nat, h : /std/Eq/Eq(k, 7)) -> /std/Eq/Eq(7, k) = ?;
        use /std/{Eq};
        let below(k : Nat, h : Eq(k, 7)) -> Eq(7, k) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();
    let reports: Vec<&str> = error.split("goal `?`").collect();
    let above = reports
        .iter()
        .find(|report| report.contains("let above("))
        .expect("the goal above its import reports");
    let below = reports
        .iter()
        .find(|report| report.contains("let below("))
        .expect("the goal below its import reports");

    assert!(
        !above.contains("Eq/sym("),
        "offered above its import: {error}"
    );
    assert!(
        below.contains("? \u{2248} Eq/sym(h)"),
        "unexpected error: {error}"
    );
}

#[test]
fn a_nested_modules_import_stays_in_its_body() {
    // A nested module body starts with no imports and its own `use` binds nothing outside it: the goal inside `M` is offered `Eq/sym(h)`, the goal in the root body — which never imported `Eq` — is not.
    let source = r#"
        use /std/{Nat};
        pub mod M
            use /std/{Nat, Eq};
            pub let inner(k : Nat, h : Eq(k, 7)) -> Eq(7, k) = ?;
        end
        let outer(k : Nat, h : /std/Eq/Eq(k, 7)) -> /std/Eq/Eq(7, k) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();
    let reports: Vec<&str> = error.split("goal `?`").collect();
    let inner = reports
        .iter()
        .find(|report| report.contains("let inner("))
        .expect("the goal inside the module reports");
    let outer = reports
        .iter()
        .find(|report| report.contains("let outer("))
        .expect("the goal in the root body reports");

    assert!(
        inner.contains("? \u{2248} Eq/sym(h)"),
        "unexpected error: {error}"
    );
    assert!(
        !outer.contains("Eq/sym("),
        "leaked out of the module: {error}"
    );
}

#[test]
fn the_goals_own_definition_is_not_suggested() {
    // Suggesting the definition a goal sits inside would be circular for a plain `let`; the pools exclude the owner. The scope binder still fits.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        f(1)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("? \u{2248} x"), "unexpected error: {error}");
    assert!(!error.contains("/f("), "own definition suggested: {error}");
}

#[test]
fn a_suggested_complete_candidate_compiles_when_pasted() {
    // The paste-and-recheck contract: the candidate suggested for the fixture in `a_computed_equality_goal_suggests_refl`'s base shape compiles.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq(1 + 2, 3) = Eq/refl();
        0
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());
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
