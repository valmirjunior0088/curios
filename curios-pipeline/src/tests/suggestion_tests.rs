//! The candidates suggested to fill a goal: which pools they come from, which are withheld, and that a complete one compiles when pasted.

use super::test_support::*;

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
        let double(n : Nat) -> Nat = match n | 0 => 0 | p + 1 => double(p) + 2 end;
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

/// A goal inside an arm is offered what fits it under the arm's guard, as a paste there would be checked: `Eq/refl()` fits `Eq(b, true)` only where the arm has `b` as `true`. Suggestions are computed after elaboration, with the arm long closed, so they reinstall the refinements the goal was born under beside its telescope; without them the fit was never offered.
#[test]
fn a_goal_in_an_arm_is_offered_what_fits_under_its_guard() {
    let source = r#"
        use /std/{Bool, Nat, Eq};
        let f(b : Bool) -> Nat =
            match b
            | true =>
                let _p : Eq(b, true) = ?;
                0
            | false => 1
            end;
        f(true)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
}
