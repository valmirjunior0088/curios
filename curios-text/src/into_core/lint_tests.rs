//! The `unused-import` lint: a `use` selector or glob nothing resolved through, and what counts as resolving through one.

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
