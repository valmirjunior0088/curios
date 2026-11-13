//! Where a written `?` is reported from.
//!
//! A goal is never accepted in a compiled program, so what these check is the *report*: which scope it names and at which position, for each syntactic place a goal may be written.

use crate::tests::test_support::*;

#[test]
fn a_goal_in_a_local_let_annotation_is_reported() {
    // `let y : ? = e` inside a body lowers to the same bare metavariable the typeless `let y = e` does, marked as a goal. The goal takes the annotation path, where the check solves it and the report carries the solution, exactly as a top-level `let y : ? = e` is reported; the typeless let's inference path would discard it unelaborated, leaving nothing for zonk to report and a program with a `?` in it compiling.
    let source = r#"
        use /std/{Nat};

        let g(x : Nat) -> Nat =
            let y : ? = x + 1;
            y;

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("└─ [goal]"), "{error}");
    assert!(error.contains("`Nat` inferred"), "{error}");
}

#[test]
fn a_goal_as_a_lambda_domain_is_reported() {
    // `elaborate_func_infer` refuses a lambda whose domain nothing pins only where that domain is a silent hole: a written `?` domain is a question rather than a failure to infer, so the goal rides on, is solved by the application, and is reported with its solution.
    let source = r#"
        use /std/{Nat};

        let h = (x : ?) => x;
        let r = h(1);

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("└─ [goal]"), "{error}");
    assert!(error.contains("`Nat` inferred"), "{error}");
}

#[test]
fn a_goal_as_a_scrutinee_or_a_guard_is_reported_at_the_type_its_arms_name() {
    // A scrutinee is inferred, so a written goal there has a stand-in for its type that nothing has decided. The arms name the carrier — `true` and `false` a `Bool`, as a guard is one, `0` a `Nat`, `x[]` a `Bytes`, `[]` a list — and the stand-in is held to it, so the goal is reported at that type rather than the match refused for a scrutinee of no carrier.
    let source = r#"
        use /std/{Bool, Bytes, List, Nat, Option};

        let a(k: Nat) -> Nat = match ? | true => k | false => 0 end;
        let b(o: Option(Nat)) -> Nat = match o | some(k), ? => k | _ => 0 end;
        let c(o: Option(Nat)) -> Nat = match o | some(k), k > 2, ? => k | _ => 0 end;
        let d(k: Nat) -> Nat = match ? | 0 => k | _ + 1 => 0 end;
        let e(k: Nat) -> Nat = match ? | x[] => k | x[_, .._] => 0 end;
        let f(k: Nat) -> Nat = match ? | [] => k | [_, .._] => 0 end;

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");

    assert_eq!(error.matches("└─ [goal]").count(), 6, "{error}");
    assert!(!error.contains("[error]"), "{error}");
    for expected in [
        "`Bool` expected",
        "`Nat` expected",
        "`Bytes` expected",
        "`List(?)` expected",
    ] {
        assert!(error.contains(expected), "{error}");
    }
}

#[test]
fn a_goal_as_a_match_motive_is_reported() {
    // An elided motive is synthesized from the arms, but a written `?` motive does not count as elided, which would synthesize over it and leave it unreported. It is a user-misplaced motive the author is asking about: checked against the eliminator's motive type and reported.
    let source = r#"
        use /std/{Nat, Bool};

        let g(b : Bool) -> Nat =
            match b : ? | true => 1 | false => 0 end;

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("└─ [goal]"), "{error}");
}
