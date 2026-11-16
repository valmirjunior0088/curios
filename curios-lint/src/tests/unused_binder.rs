//! `unused-binder`: a parameter, local, pattern leaf or arm binder nothing reads, and the binders never reported.

use super::test_support::*;

#[test]
fn a_parameter_the_body_never_reads_is_reported_at_its_word() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> Type = Type;
        f
    "#
        ),
        ["unused-binder: unused binder\n  `n` is never used\n  write `_` in its name's place"]
    );
}

/// A sugar's parameter is one binder in its Π-type and in its lambda, so a mention in the result type is a use of it whatever the body does.
#[test]
fn a_parameter_only_the_result_type_mentions_is_used() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> n = Type;
        f
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_parameter_only_a_later_parameters_type_mentions_is_used() {
    assert_eq!(
        lints(
            r#"
        let f(A : Type, x : A) -> Type = Type;
        f
    "#
        ),
        ["unused-binder: unused binder\n  `x` is never used\n  write `_` in its name's place"]
    );
}

/// Only the sugar's own parameters are read against its result type; a like-named binder deeper in the body is its own.
#[test]
fn only_the_sugars_own_parameters_are_exempted_by_the_result_type() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> n = (m) => (n) => m;
        f
    "#
        ),
        ["unused-binder: unused binder\n  `n` is never used\n  write `_` in its name's place"]
    );
}

#[test]
fn a_pattern_leaf_nothing_reads_is_reported_alone() {
    assert_eq!(
        lints(
            r#"
        let (a, b) = Type;
        a
    "#
        ),
        ["unused-binder: unused binder\n  `b` is never used\n  write `_` in its name's place"]
    );
}

#[test]
fn an_implicit_parameter_is_a_binder_like_any_other() {
    assert_eq!(
        lints(
            r#"
        (@A : Type) => Type
    "#
        ),
        ["unused-binder: unused binder\n  `A` is never used\n  write `_` in its name's place"]
    );
}

#[test]
fn a_match_arm_binder_nothing_reads_is_reported() {
    assert_eq!(
        lints(
            r#"
        pub induct Option(A : Type) : pub Type
        | some(A)
        | none()
        end
        let f(o : Option(Type)) -> Type =
            match o
            | some(x) => Type
            | none() => Type
            end;
        f
    "#
        ),
        ["unused-binder: unused binder\n  `x` is never used\n  write `_` in its name's place"]
    );
}

#[test]
fn a_predecessor_and_a_cons_tail_nothing_reads_are_reported() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type, l : Type) -> Type =
            let a =
                match n
                | 0 => Type
                | p + 1 => Type
                end;
            match l
            | [] => a
            | [h, ..t] => h
            end;
        f
    "#
        ),
        [
            "unused-binder: unused binder\n  `p` is never used\n  write `_` in its name's place",
            "unused-binder: unused binder\n  `t` is never used\n  write `_` in its name's place",
        ]
    );
}

/// A `_` is the slot a binder nothing reads gives its name up to: a parameter's, an implicit one's and a part of a pattern whose other part is read. A whole `let _ = …;` that runs nothing is no slot, and `unused-let`'s.
#[test]
fn a_wildcard_binder_is_never_reported() {
    assert_eq!(
        lints(
            r#"
        let f(_ : Type, @_ : Type) -> Type = Type;
        let (a, _) = f;
        a
    "#
        ),
        Vec::<String>::new()
    );
}

/// A `_` prefix keeps no binder: a name nothing reads is reported whatever it begins with, and one the body reads is used.
#[test]
fn an_underscore_prefixed_binder_is_reported_where_nothing_reads_it() {
    assert_eq!(
        lints(
            r#"
        let f(_n : Type, _m : Type) -> Type = _m;
        f
    "#
        ),
        ["unused-binder: unused binder\n  `_n` is never used\n  write `_` in its name's place"]
    );
}

#[test]
fn a_declaration_holding_a_goal_reports_no_binder() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> Type = ?;
        f
    "#
        ),
        Vec::<String>::new()
    );
}

/// A binder is a count, not a reachability question: the local function's own body mentions it.
#[test]
fn a_local_function_that_calls_itself_is_used() {
    assert_eq!(
        lints(
            r#"
        (x : Type) =>
            let go(n : Type) -> Type = go(n);
            x
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_sugar_field_parameter_nothing_reads_is_reported() {
    assert_eq!(
        lints(
            r#"
        (f(x) = Type,)
    "#
        ),
        ["unused-binder: unused binder\n  `x` is never used\n  write `_` in its name's place"]
    );
}

/// A `use` member has no binder to report: its place is held by `_`, and resolution reads it whether or not the body does.
#[test]
fn a_use_lambda_member_is_never_reported() {
    assert_eq!(
        lints(
            r#"
        (use _, a : Type) => a
    "#
        ),
        Vec::<String>::new()
    );
}

/// Each alternative writes its own binder, which its own lowering of the body reads or leaves.
#[test]
fn each_alternative_reports_the_binder_it_writes() {
    assert_eq!(
        lints(
            r#"
        let f(o : Type) -> Bool =
            match o
            | a(x) | b(_, x) => true
            | _ => false
            end;
        f
    "#
        ),
        [
            "unused-binder: unused binder\n  `x` is never used\n  write `_` in its name's place",
            "unused-binder: unused binder\n  `x` is never used\n  write `_` in its name's place"
        ]
    );
}
