//! `unused-let`: a `let` whose value runs no action and none of whose binders anything reads, which does nothing, and the `let`s that are not one.

use crate::into_core::test_support::*;

#[test]
fn a_local_binding_nothing_reads_is_reported() {
    assert_eq!(
        lints(
            r#"
        let x = Type;
        Type
    "#
        ),
        ["unused-let: unused let\n  `x` is never used, and its value runs no action\n  delete it"]
    );
}

#[test]
fn a_binder_shadowed_before_any_use_is_reported() {
    assert_eq!(
        lints(
            r#"
        let x = Type;
        let x = Type;
        x
    "#
        ),
        ["unused-let: unused let\n  `x` is never used, and its value runs no action\n  delete it"]
    );
}

/// An arm's body is lowered once for each alternative, and a binder written in it is one binder however often it is lowered: reported once, and only where no lowering of it is read.
#[test]
fn a_binder_in_the_body_an_arm_of_alternatives_shares_is_reported_once() {
    assert_eq!(
        lints(
            r#"
        let f(o : Bool) -> Bool =
            match o
            | true | false =>
                let unused = o;
                o
            end;
        f
    "#
        ),
        [
            "unused-let: unused let\n  `unused` is never used, and its value runs no action\n  delete it"
        ]
    );
}

#[test]
fn a_let_of_a_wildcard_nothing_reads_is_reported() {
    assert_eq!(
        lints(
            r#"
        let _ = Type;
        Type
    "#
        ),
        [
            "unused-let: unused let\n  nothing reads this `let`, and its value runs no action\n  delete it"
        ]
    );
}

/// A pattern none of whose binders anything reads is one `let` that does nothing, reported once at its pattern, and none of its binders is `unused-binder`'s.
#[test]
fn a_let_none_of_whose_binders_is_read_is_reported_once() {
    assert_eq!(
        lints(
            r#"
        let (a, b) = Type;
        Type
    "#
        ),
        [
            "unused-let: unused let\n  nothing reads this `let`, and its value runs no action\n  delete it"
        ]
    );
}

/// A value that hoists a `!` runs an action where the `let` stands: a `_` keeps it, and a name nothing reads gives itself up to `_`.
#[test]
fn a_let_that_runs_an_action_is_none() {
    assert_eq!(
        lints(
            r#"
        let _ = Type!;
        let x = Type!;
        Type
    "#
        ),
        ["unused-binder: unused binder\n  `x` is never used\n  write `_` in its name's place"]
    );
}

#[test]
fn a_declaration_holding_a_goal_reports_no_let() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> Type =
            let _ = Type;
            ?;
        f
    "#
        ),
        Vec::<String>::new()
    );
}
