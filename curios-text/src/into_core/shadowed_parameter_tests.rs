//! A binder the result type names: used where the name the result reads is its own, and reported where a later binder of its name shadows it.

use super::test_support::*;

/// The sugar's telescope is the Π-type at the root of its type, not the first one finished: a parameter whose own type is a function type is lowered before the result, and must not stand in for the telescope.
#[test]
fn a_parameter_the_result_mentions_is_used_when_an_earlier_parameter_has_a_function_type() {
    assert_eq!(
        lints(
            r#"
        let cong(f : (Type) -> Type, x : Type, y : Type) -> f(x) = Type;
        cong
    "#
        ),
        ["unused-binder: unused binder `y`; write `_` in its name's place"]
    );
}

/// The sugar's lambda is the one its body lowers to, not the first one lowered: a lambda written in the result type is lowered before it, and must not stand in for the parameters.
#[test]
fn a_parameter_only_the_result_type_mentions_is_used_when_the_result_type_holds_a_lambda() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> ((m) => m)(n) = Type;
        f
    "#
        ),
        Vec::<String>::new()
    );
}

/// A local definition's value is lowered before its type, and its parameters' own types with it: a parameter whose type is a function type must not stand in for the telescope.
#[test]
fn a_local_parameter_only_the_result_type_mentions_is_used_beside_a_function_typed_one() {
    assert_eq!(
        lints(
            r#"
        let g : Type =
            let f(h : (Type) -> Type, x : Type) -> h(x) = Type;
            f;
        g
    "#
        ),
        Vec::<String>::new()
    );
}

/// A function type written in a parameter's own type binds its own names, and what its result mentions exempts nothing of the sugar's.
#[test]
fn a_local_parameter_nothing_mentions_is_reported_when_a_later_type_binds_its_name() {
    assert_eq!(
        lints(
            r#"
        let g : Type =
            let f(x : Type, _ : (x : Type) -> x) -> Type = Type;
            f;
        g
    "#
        ),
        ["unused-binder: unused binder `x`; write `_` in its name's place"]
    );
}

/// A mention reaches the innermost binder of its name, so a parameter a later one of its name shadows is used by nothing, whatever the result type says of the later one.
#[test]
fn a_parameter_a_later_one_of_its_name_shadows_is_reported_though_the_result_names_it() {
    assert_eq!(
        lints(
            r#"
        let f(x : Type, x : Type) -> x = Type;
        f
    "#
        ),
        ["unused-binder: unused binder `x`; write `_` in its name's place"]
    );
}

#[test]
fn a_local_parameter_a_later_one_of_its_name_shadows_is_reported_though_the_result_names_it() {
    assert_eq!(
        lints(
            r#"
        let g : Type =
            let f(x : Type, x : Type) -> x = Type;
            f;
        g
    "#
        ),
        ["unused-binder: unused binder `x`; write `_` in its name's place"]
    );
}

#[test]
fn every_parameter_but_the_last_of_a_name_the_result_mentions_is_reported() {
    assert_eq!(
        lints(
            r#"
        let f(x : Type, x : Type, x : Type) -> x = Type;
        f
    "#
        ),
        [
            "unused-binder: unused binder `x`; write `_` in its name's place",
            "unused-binder: unused binder `x`; write `_` in its name's place"
        ]
    );
}

/// A compound parameter's leaves are the body's alone: the result type's mention is of the plain parameter, so the leaf of its name is used only where the body uses it.
#[test]
fn a_pattern_leaf_is_reported_though_the_result_names_a_later_parameter_of_its_name() {
    assert_eq!(
        lints(
            r#"
        let f((x, y) : {Type, Type}, x : Type) -> x = y;
        f
    "#
        ),
        ["unused-binder: unused binder `x`; write `_` in its name's place"]
    );
}

/// The same pair the other way round: the result's mention reaches the plain parameter, which the leaf then shadows in the body, so the leaf is the one the body decides.
#[test]
fn a_pattern_leaf_shadowing_a_parameter_the_result_mentions_is_reported_when_the_body_leaves_it() {
    assert_eq!(
        lints(
            r#"
        let f(x : Type, (x, y) : {Type, Type}) -> x = y;
        f
    "#
        ),
        ["unused-binder: unused binder `x`; write `_` in its name's place"]
    );
}

#[test]
fn a_parameter_the_result_mentions_is_used_though_a_pattern_leaf_of_its_name_takes_the_body() {
    assert_eq!(
        lints(
            r#"
        let f(x : Type, (x, y) : {Type, Type}) -> x = (x, y);
        f
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_witness_parameter_a_later_one_of_its_name_shadows_is_reported() {
    assert_eq!(
        lints(
            r#"
        pub concept Show(A : Type) : pub Type { show : A }
        satisfy (@A : Type, @A : Type) => Show(A) { show = Type }
        Type
    "#
        ),
        ["unused-binder: unused binder `A`; write `_` in its name's place"]
    );
}
