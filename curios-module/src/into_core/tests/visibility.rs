//! What a descendant, a sibling and a parent may read, and what a public signature may name.

use crate::into_core::test_support::*;

// === Subtree visibility ======================================================

// A declaration written without `pub` in `M` is visible within `M`'s subtree, so a descendant may name its ancestor's private binding.
#[test]
fn descendant_reads_its_ancestors_private_binding() {
    run(r#"
        pub mod Owner
            let helper : Type = Type;
            pub mod Worker
                use /Owner/{helper};
                pub let use_it : Type = helper;
            end
        end
        Type
    "#);
}

// The relaxation is downward only: a sibling is outside the declaring module's subtree, so it stays shut out.
#[test]
fn sibling_cannot_read_a_siblings_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            pub mod A
                let secret : Type = Type;
            end
            pub mod B
                use /Owner/A/{secret};
                pub let use_it : Type = secret;
            end
        end
        Type
    "#
        )
        .contains("`secret` is private to `/Owner/A`")
    );
}

// Nor upward: a parent may traverse its own private child, but not read that child's private bindings.
#[test]
fn parent_cannot_read_its_childs_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Impl
                let secret : Type = Type;
            end
            pub let use_it : Type = Impl/secret;
        end
        Type
    "#
        )
        .contains("`secret` is private to `/Owner/Impl`")
    );
}

// `pub` inside a private module means "wherever this module is visible", which is its declaring module's subtree — not the world.
#[test]
fn pub_inside_a_private_module_reaches_the_subtree_only() {
    run(r#"
        pub mod Owner
            mod Impl
                pub let helper : Type = Type;
            end
            pub mod Worker
                use /Owner/Impl/{helper};
                pub let use_it : Type = helper;
            end
        end
        Type
    "#);

    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Impl
                pub let helper : Type = Type;
            end
        end
        pub mod Outsider
            use /Owner/Impl/{helper};
        end
        Type
    "#
        )
        .contains("`Impl` is private to `/Owner`")
    );
}

// A glob imports the exported surface, never a subtree-private declaration: reaching one always requires naming it. The reference is left as a bare name for core to reject, rather than silently resolving to `/Owner/helper`.
#[test]
fn glob_does_not_import_subtree_private_bindings() {
    let (definitions, _) = lowered(
        r#"
        pub mod Owner
            let helper : Type = Type;
            pub mod Worker
                use /Owner/*;
                pub let use_it : Type = helper;
            end
        end
        Type
    "#,
    );

    // The reference is left for core to reject. What it must *not* be is any declaration: `/Owner/helper` would mean the glob leaked a private binding, and a root-level `/helper` would silently capture an entry-module definition of the same name. A binder identity can be neither.
    let (_, _, body) = definitions
        .iter()
        .find(|(name, _, _)| *name == global_name("/Owner/Worker/use_it"))
        .expect("`use_it` is lowered");
    assert!(
        matches!(
            &**body,
            curios_core::Subterm::Var(var)
                if var.as_free().is_some_and(|local| local.hint() == Some("helper"))
        ),
        "the glob leaked a private binding: {body:?}"
    );
}

#[test]
fn rejects_private_module_from_outside_its_declaring_subtree() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Foo
                pub let f : Type = Type;
            end
        end
        pub mod Bar
            use /Owner/Foo/{f};
        end
        Type
    "#
        )
        .contains("`Foo` is private to `/Owner`")
    );
}

// A private declaration written at the root belongs to the root's subtree, which is the whole program — so a sibling module may name it. The boundary is the declaring module, and the root declares no boundary above itself.
#[test]
fn allows_a_private_root_module_from_a_sibling() {
    run(r#"
        mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#);
}

#[test]
fn allows_pub_root_module_via_absolute_path() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#);
}

// === Interface audit =========================================================

// The facade pattern: a module re-exports a name out of its own private child and then uses it in a public signature. The audit follows the re-export, so the name is as visible as the facade makes it.
#[test]
fn facade_may_name_what_it_re_exports_in_a_public_signature() {
    run(r#"
        pub mod Facade
            mod Impl
                pub let Helper : Type = Type;
            end
            pub use Impl/{Helper};
            pub let build(h : Helper) -> Helper = h;
        end
        Type
    "#);
}

// Without the re-export the same signature is rejected: `Helper` reaches only `Facade`'s subtree, while `build` reaches the whole program. The refusal names the module that keeps it there, and offers the re-export above.
#[test]
fn public_signature_naming_an_unexported_subtree_item_is_rejected() {
    assert!(
        run_err(
            r#"
        pub mod Facade
            mod Impl
                pub let Helper : Type = Type;
            end
            use Impl/{Helper};
            pub let build(h : Helper) -> Helper = h;
        end
        Type
    "#
        )
        .contains(
            "names `Helper` in its signature\n  `Helper` is in the private module `/Facade/Impl`\n  mark `/Facade/Impl` `pub`, re-export `Helper` beside `build`, or make `build` private"
        ),
    );
}
