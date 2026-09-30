//! Where a written `?` is reported from.
//!
//! A goal is never accepted in a compiled program, so what these check is the *report*: which scope it names and at which position, for each syntactic place a goal may be written.

use crate::tests::typecheck;

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
    assert!(error.contains("goal `?`"), "{error}");
    assert!(error.contains("? = Nat"), "{error}");
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
    assert!(error.contains("goal `?`"), "{error}");
    assert!(error.contains("? = Nat"), "{error}");
}

#[test]
fn a_goal_as_a_match_motive_is_reported() {
    // An elided motive is synthesized from the arms, but a written `?` motive does not count as elided, which would synthesize over it and leave it unreported. It is a user-written motive the author is asking about: checked against the eliminator's motive type and reported.
    let source = r#"
        use /std/{Nat, Bool};

        let g(b : Bool) -> Nat =
            match b : ? | true => 1 | false => 0 end;

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("goal `?`"), "{error}");
}
