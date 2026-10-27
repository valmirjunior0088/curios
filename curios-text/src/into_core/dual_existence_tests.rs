//! A module and a binding under one name: what a `use` of the name registers, from inside and outside, and what each side's visibility lets through.

use super::test_support::*;

#[test]
fn use_of_dual_existence_registers_both() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
            end
            pub use X/{y};
        end
        pub mod Bar
            use /Foo/{y};
            pub let direct : Type = y;
            pub let via_path : Type = y;
        end
        Type
    "#);
}

#[test]
fn module_member_is_not_classified_as_a_generated_nominal_member() {
    let module = elaborate_source(
        r#"
        struct Foo(A : Type, B : Type) : pub Type { a : A, b : B }
        pub mod Foo
            pub let bar : Type = Type;
        end
        Type
    "#,
    );
    let bar = module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition)
                if definition.name.faithful().to_string() == "/Foo/bar" =>
            {
                Some(definition)
            }
            _ => None,
        })
        .expect("module member definition");
    assert_eq!(bar.kind, curios_core::DefinitionKind::Authored);
}

#[test]
fn dual_use_lets_bare_name_resolve_to_binding() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
            end
            pub use X/{y};
        end
        use /Foo/{y};
        y
    "#);
}

#[test]
fn dual_use_lets_path_resolve_through_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{y};
        end
        Foo/y
    "#);
}

#[test]
fn public_child_with_private_binding_imports_only_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let z : Type = Type;
            end
            use X/{z};
            let X : Type = z;
        end
        use /Foo/{X};
        X/z
    "#);
}

#[test]
fn private_child_with_public_binding_imports_only_binding() {
    run(r#"
        pub mod Foo
            mod X
                pub let z : Type = Type;
            end
            use X/{z};
            pub let X : Type = z;
        end
        use /Foo/{X};
        X
    "#);
}

#[test]
fn use_of_dual_existence_from_outside_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/{X};
        X
    "#);
}

#[test]
fn use_of_dual_existence_from_outside_qualifier_path() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/{X};
        X/q
    "#);
}

#[test]
fn rejects_use_when_both_sides_private() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            mod X
                pub let z : Type = Type;
            end
            use X/{z};
            let X : Type = z;
        end
        use /Foo/{X};
        Type
    "#
        )
        .contains("`X` is private to `/Foo`")
    );
}

#[test]
fn glob_on_dual_existence_imports_once() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/*;
        X/q
    "#);
}
