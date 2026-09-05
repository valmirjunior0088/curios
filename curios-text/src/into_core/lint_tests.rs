//! The lints the lowering decides: an import nothing resolved through, a binder nothing referenced, a declaration nothing reaches, and what counts as a use of each.

use super::test_support::*;

#[test]
fn a_selector_nothing_resolved_through_is_reported_at_its_word() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        x
    "#
        ),
        ["unused-import: unused import `y`; delete it"]
    );
}

#[test]
fn a_glob_nothing_resolved_through_is_reported_as_its_path() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/*;
        Type
    "#
        ),
        ["unused-import: unused import `/Foo/*`; delete it"]
    );
}

#[test]
fn a_glob_one_reference_resolved_through_is_used() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/*;
        x
    "#
        ),
        Vec::<String>::new()
    );
}

/// A local binder shadows the import, so nothing resolves through it — exactly the case in which the import is dead.
#[test]
fn an_import_a_local_shadows_before_any_use_is_unused() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        let x = Type;
        x
    "#
        ),
        ["unused-import: unused import `x`; delete it"]
    );
}

#[test]
fn a_re_export_is_its_own_use() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub use /Foo/{x};
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_module_selector_a_qualified_path_walks_through_is_used() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub mod Bar
                pub let z : Type = Type;
            end
        end
        use /Foo/{Bar};
        Bar/z
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_module_selector_a_later_use_walks_through_is_used() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub mod Bar
                pub let z : Type = Type;
            end
        end
        use /Foo/{Bar};
        use Bar/{z};
        z
    "#
        ),
        Vec::<String>::new()
    );
}

/// A `use` binds from its position to the end of the body it is written in; a nested module body starts empty, so a use in the parent that only the child would have wanted is dead in the parent.
#[test]
fn an_import_used_only_in_a_type_annotation_is_used() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let T : Type = Type;
        end
        use /Foo/{T};
        let f(x : T) -> Type = x;
        f
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn lints_read_in_source_order() {
    assert_eq!(
        lints(
            r#"
        pub mod Foo
            pub let a : Type = Type;
            pub let b : Type = Type;
        end
        use /Foo/{b};
        use /Foo/{a};
        Type
    "#
        ),
        [
            "unused-import: unused import `b`; delete it",
            "unused-import: unused import `a`; delete it",
        ]
    );
}

#[test]
fn a_parameter_the_body_never_reads_is_reported_at_its_word() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type) -> Type = Type;
        f
    "#
        ),
        ["unused-binder: unused binder `n`; name it `_n` to keep it"]
    );
}

/// The sugar's telescope is lowered twice, as the Π-type's binders and the lambda's; a parameter the result type mentions is used by the declaration whatever the body does.
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
        ["unused-binder: unused binder `x`; name it `_x` to keep it"]
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
        ["unused-binder: unused binder `n`; name it `_n` to keep it"]
    );
}

#[test]
fn a_local_binding_nothing_reads_is_reported() {
    assert_eq!(
        lints(
            r#"
        let x = Type;
        Type
    "#
        ),
        ["unused-binder: unused binder `x`; name it `_x` to keep it"]
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
        ["unused-binder: unused binder `b`; name it `_b` to keep it"]
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
        ["unused-binder: unused binder `x`; name it `_x` to keep it"]
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
        ["unused-binder: unused binder `A`; name it `_A` to keep it"]
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
        ["unused-binder: unused binder `x`; name it `_x` to keep it"]
    );
}

#[test]
fn a_fold_hypothesis_and_a_cons_tail_nothing_reads_are_reported() {
    assert_eq!(
        lints(
            r#"
        let f(n : Type, l : Type) -> Type =
            let a =
                match n
                | 0 => Type
                | p + 1; ih => p
                end;
            match l
            | [] => a
            | [h, ..t] => h
            end;
        f
    "#
        ),
        [
            "unused-binder: unused binder `ih`; name it `_ih` to keep it",
            "unused-binder: unused binder `t`; name it `_t` to keep it",
        ]
    );
}

#[test]
fn an_underscore_prefixed_binder_and_a_wildcard_are_kept() {
    assert_eq!(
        lints(
            r#"
        let _x = Type;
        let _ = Type;
        let f(_n : Type, _ : Type) -> Type = Type;
        f
    "#
        ),
        Vec::<String>::new()
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
        ["unused-binder: unused binder `x`; name it `_x` to keep it"]
    );
}

#[test]
fn a_private_definition_nothing_reaches_is_reported_at_its_name() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        Type
    "#
        ),
        [
            "unused-declaration: unused declaration `helper`; name it `_helper` or make it `pub` to keep it"
        ]
    );
}

#[test]
fn a_definition_the_entry_tail_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        helper
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_a_public_one_reaches_is_used_and_the_public_one_is_a_root() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        pub let api : Type = helper;
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_only_a_test_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        let helper : Type = Type;
        test it() = helper;
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_only_a_witness_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        pub concept Show(A : Type) : pub Type { show : A }
        struct Foo : Type { }
        satisfy Show(Foo) { show = Foo { } }
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_type_only_its_constructor_reaches_is_used() {
    assert_eq!(
        lints(
            r#"
        induct Foo : Type
        | mk()
        end
        Foo/mk()
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn a_definition_used_only_by_itself_is_dead() {
    assert_eq!(
        lints(
            r#"
        let go : Type = go;
        Type
    "#
        ),
        ["unused-declaration: unused declaration `go`; name it `_go` or make it `pub` to keep it"]
    );
}

#[test]
fn two_dead_definitions_reaching_each_other_are_both_dead() {
    assert_eq!(
        lints(
            r#"
        let a : Type = b
        and b : Type = a;
        Type
    "#
        ),
        [
            "unused-declaration: unused declaration `a`; name it `_a` or make it `pub` to keep it",
            "unused-declaration: unused declaration `b`; name it `_b` or make it `pub` to keep it",
        ]
    );
}

#[test]
fn a_private_module_nothing_reaches_is_reported_once_at_the_mod() {
    assert_eq!(
        lints(
            r#"
        mod Internal
            pub let a : Type = Type;
            let b : Type = a;
            mod Deeper
                pub let c : Type = Type;
            end
        end
        Type
    "#
        ),
        [
            "unused-declaration: unused declaration `Internal`; name it `_Internal` or make it `pub` to keep it"
        ]
    );
}

#[test]
fn a_private_module_one_declaration_of_which_is_reached_reports_the_others() {
    assert_eq!(
        lints(
            r#"
        mod Internal
            pub let a : Type = Type;
            pub let b : Type = Type;
        end
        Internal/a
    "#
        ),
        ["unused-declaration: unused declaration `b`; name it `_b` or make it `pub` to keep it"]
    );
}

#[test]
fn a_facade_re_export_keeps_the_private_module_it_reaches_into() {
    assert_eq!(
        lints(
            r#"
        pub mod Api
            pub use /Impl/{f};
        end
        mod Impl
            pub let f : Type = Type;
        end
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

#[test]
fn an_underscore_prefixed_declaration_and_a_public_one_are_kept() {
    assert_eq!(
        lints(
            r#"
        let _scratch : Type = Type;
        pub let api : Type = Type;
        mod _Later
            pub let x : Type = Type;
        end
        Type
    "#
        ),
        Vec::<String>::new()
    );
}

/// The sugar's telescope is the Π-type entered first, not the first one finished: a parameter whose own type is a function type is lowered before the result, and must not stand in for the telescope.
#[test]
fn a_parameter_the_result_mentions_is_used_when_an_earlier_parameter_has_a_function_type() {
    assert_eq!(
        lints(
            r#"
        let cong(f : (Type) -> Type, x : Type, y : Type) -> f(x) = Type;
        cong
    "#
        ),
        ["unused-binder: unused binder `y`; name it `_y` to keep it"]
    );
}

/// A named `use` binder joins the instance scope: resolution reads it, whether or not the body names it.
#[test]
fn a_named_use_lambda_binder_is_never_reported() {
    assert_eq!(
        lints(
            r#"
        (use w, a : Type) => a
    "#
        ),
        Vec::<String>::new()
    );
}
