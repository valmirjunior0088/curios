//! What a descendant, a sibling and a parent may read, and what a public signature may name.

use super::test_support::*;

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
        .contains("private binding")
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
        .contains("private binding")
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
        .contains("private child module")
    );
}

// A glob imports the exported surface, never a subtree-private declaration: reaching one always requires naming it. The reference is left as a bare name for core to reject, rather than silently resolving to `/Owner/helper`.
#[test]
fn glob_does_not_import_subtree_private_bindings() {
    let term = run(r#"
        pub mod Owner
            let helper : Type = Type;
            pub mod Worker
                use /Owner/*;
                pub let use_it : Type = helper;
            end
        end
        Type
    "#);

    // The reference is left for core to reject. What it must *not* be is any global: `/Owner/helper` would mean the glob leaked a private binding, and a root-level `/helper` would silently capture an entry-module definition of the same name. A binder identity can be neither. The reference is left for core to reject. What it must *not* be is any global: `/Owner/helper` would mean the glob leaked a private binding, and a root-level `/helper` would silently capture an entry-module definition of the same name. A binder identity can be neither.
    let dumped = format!("{term:?}");
    assert!(
        dumped.contains("Local(Mint { index: 0, hint: Some(\"helper\") })"),
        "unexpected term: {dumped}"
    );
    // `/Owner/helper` occurs exactly once — as the binder the declaration introduces. A second occurrence would be the reference resolving to it.
    assert_eq!(
        dumped
            .matches("Authored(Qualifier([\"Owner\", \"helper\"]))")
            .count(),
        1,
        "the glob leaked a private binding: {dumped}"
    );
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

// Without the re-export the same signature is rejected: `Helper` reaches only `Facade`'s subtree, while `build` reaches the whole program.
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
        .contains("exposes private item"),
    );
}
