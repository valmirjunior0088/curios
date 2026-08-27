//! Local and module bindings, and the type a let is pinned through.

use super::test_support::*;

#[test]
fn single_let_binding() {
    assert_eq!(
        run(r#"
            let x : Type = Type;
            x
        "#),
        curios_core::Term::let_(
            &global("/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/x")))
        ),
    );
}

#[test]
fn nested_module_binding_reference() {
    assert_eq!(
        run(r#"
            mod Foo
                pub let f : Type = Type;
            end
            Foo/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/f")))
        ),
    );
}

#[test]
fn module_named_after_type_resolves_by_qualified_path() {
    assert_eq!(
        run(r#"
            mod Nat
                pub let double : Type = Type;
            end
            Nat/double
        "#),
        curios_core::Term::let_(
            &global("/Nat/double"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Nat/double")))
        ),
    );
}

#[test]
fn use_shorthand_resolves_qualifier() {
    assert_eq!(
        run(r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use Foo/{Bar};
            Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
        ),
    );
}

#[test]
fn allows_pub_on_root_items() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub let g : Type = Type;
        Type
    "#);
}

// `/std/Async/block_on` pins an unannotated local binding through a *type alias* — `let Slot : Type = Cell(Option(Job));` — whose bare written `Type` mints a generalizable level while its value sits at one fixed level.
#[test]
fn an_unannotated_local_let_is_pinned_through_a_type_alias() {
    let module = elaborate_source(
        "induct Box(A : Type) : Type | empty() | wrap(A) end \
         induct Job : Type | job() end \
         let Slot : Type = Box(Job); \
         let outer(@A : Type, x : A) -> A = \
             let slot = Box/empty(); \
             let force(b : Slot) -> {} = (); \
             let _ = force(slot); \
             x; \
         outer",
    );
    assert!(
        module
            .items
            .iter()
            .any(|item| matches!(item, curios_core::Item::Let(d) if d.name.symbol() == "/outer")),
        "outer elaborated"
    );
}
