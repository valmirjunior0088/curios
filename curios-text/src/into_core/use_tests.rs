//! Every `use` form: paths, globs, brace groups, and the dual existence of a module and a binding under one name.

use crate::{Entrypoint, RootSource};

use super::test_support::*;

#[test]
fn private_use_does_not_expose_qualifier() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        pub mod MyMod
            use /Foo/{Bar};
        end
        MyMod/Bar/f
    "#
        )
        .contains("child module not found")
    );
}

#[test]
fn imports_binding_by_path() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        x
    "#);
}

#[test]
fn rejects_use_of_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            let x : Type = Type;
        end
        use /Foo/{x};
        x
    "#
        )
        .contains("private binding: x")
    );
}

#[test]
fn pub_use_re_exports_binding() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub use /Foo/{x};
        end
        use /Bar/{x};
        x
    "#);
}

#[test]
fn pub_use_binding_aliases_to_canonical_path() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub use /Foo/{x};
        end
        Bar/x
    "#);
}

#[test]
fn rejects_use_followed_by_local_let_of_same_name() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        let x : Type = Type;
        x
    "#
        )
        .contains("binding conflicts with existing scope entry: x")
    );
}

#[test]
fn rejects_two_imports_of_same_name() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub let x : Type = Type;
        end
        use /Foo/{x};
        use /Bar/{x};
        x
    "#
        )
        .contains("binding conflicts with existing scope entry: x")
    );
}

#[test]
fn relative_use_imports_binding() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            use /{Foo};
            use Foo/{x};
            pub let y : Type = x;
        end
        Bar/y
    "#);
}

#[test]
fn rejects_use_of_unknown_item() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{nope};
        Type
    "#
        )
        .contains("no module or binding named nope")
    );
}

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
            curios_core::Item::Let(definition) if definition.name.symbol() == "/Foo/bar" => {
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
fn brace_group_imports_all_labels() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        x
    "#);
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        y
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
        .contains("private child module: X")
    );
}

#[test]
fn glob_imports_all_public_bindings() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
                pub let y : Type = Type;
            end
            use /Foo/*;
            x
        "#),
        curios_core::Term::let_(
            &global("/Foo/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::let_(
                &global("/Foo/y"),
                written_type(2),
                written_type(3),
                curios_core::Term::var(curios_core::Var::free(global("/Foo/x")))
            )
        ),
    );
}

#[test]
fn glob_imports_child_modules_as_qualifiers() {
    run(r#"
        pub mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        use /Foo/*;
        Bar/f
    "#);
}

#[test]
fn glob_skips_private_child_modules() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            mod Bar
                pub let f : Type = Type;
            end
        end
        use /Foo/*;
        Bar/f
    "#
        )
        .contains("unresolved qualifier")
    );
}

#[test]
fn relative_use_glob_imports_from_qualifier() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use Foo/*;
        x
    "#);
}

#[test]
fn pub_use_glob_re_exports_all_public_labels() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        pub mod Mine
            pub use /Foo/*;
        end
        use /Mine/{Bar};
        use /Mine/{x};
        Bar/f
    "#);
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

#[test]
fn imports_record_each_binding_under_the_spelling_it_resolves_by() {
    // The table goal suggestions draw imported candidates from, and spell them by. A module import spells the module's bindings through its label; a named binding import and a glob spell theirs bare; and when one binding arrives twice the shorter spelling is a second entry, so the display takes it while an item between the two still sees only the first.
    //
    // Scope is point-of-use, per body: `before` was written above every import and sees none; `after` sees the module import but not the binding import below it; the tail sees both; and `Outer/M/inner`, inside its own body, sees what that body imported and nothing the root did.
    let src = r#"
        pub mod Outer
            pub mod M
                use /Outer/{N};
                pub let f: Type = Type;
                pub let g: Type = Type;
                pub let inner: Type = N/h;
            end
            pub mod N
                pub let h: Type = Type;
            end
        end
        let before: Type = Type;
        use Outer/{M, N};
        let after: Type = Type;
        use Outer/M/{g};
        Type
    "#;
    let unit = super::into_core_unit(
        &super::UnitSource::entry(&src.parse::<Entrypoint>().unwrap(), &RootSource::none()),
        &[],
        syntax(),
    )
    .unwrap();
    let imports = unit.imports();
    let spellings = imports.spellings();
    let spelling = |path: &str| spellings.get(&global_name(path)).copied();
    let in_scope = |owner: Option<&str>| {
        imports
            .in_scope_at(owner.map(global_name).as_ref())
            .map(|import| format!("{}={}", import.global, import.spelling))
            .collect::<Vec<_>>()
    };

    assert_eq!(spelling("/Outer/M/f"), Some("M/f"));
    assert_eq!(spelling("/Outer/M/g"), Some("g"));
    assert_eq!(spelling("/Outer/N/h"), Some("N/h"));
    assert_eq!(spelling("/Outer/M"), None, "a module is not a binding");

    assert!(
        in_scope(Some("/before")).is_empty(),
        "{:?}",
        in_scope(Some("/before"))
    );
    assert_eq!(
        in_scope(Some("/after")),
        [
            "/Outer/M/f=M/f",
            "/Outer/M/g=M/g",
            "/Outer/M/inner=M/inner",
            "/Outer/N/h=N/h"
        ]
    );
    assert_eq!(
        in_scope(None),
        [
            "/Outer/M/f=M/f",
            "/Outer/M/g=M/g",
            "/Outer/M/inner=M/inner",
            "/Outer/N/h=N/h",
            "/Outer/M/g=g"
        ]
    );
    assert_eq!(in_scope(Some("/Outer/M/inner")), ["/Outer/N/h=N/h"]);
}
