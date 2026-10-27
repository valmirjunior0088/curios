//! The representation-exposure audit: what a public representation or signature may name, what a dependency exposed apart admits, and what a private module's name is told.

use super::test_support::*;

#[test]
fn unexposed_public_representation_may_use_private_helpers() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { Hidden }
        end
        Type
    "#);
}

#[test]
fn audit_accepts_a_separately_exposed_dependency() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            struct Open : pub Type { Hidden }
            pub let PublicOpen : Type = Open;
        end
        Type
    "#);
}

#[test]
fn direct_representation_exposure_accepts_a_separately_exposed_dependency() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            pub struct Open : pub Type { Hidden }
        end
        Type
    "#);
}

// **The hint names the declaration written without `pub`.** `T` is `pub` where it stands, and what keeps it from the readers of `g` is the module it stands in: a reader told to mark `T` has nothing to change.
#[test]
fn a_public_name_in_a_private_module_is_told_of_the_module() {
    let error = run_err(
        r#"
        pub mod M
            mod Inner
                pub struct T : pub Type { Type }
            end
            use Inner/{T};
            pub let g(t : T) -> T = t;
        end
        Type
    "#,
    );

    assert!(
        error.contains(
            "public item 'g' exposes 'T' in its signature, and 'T' is in the private module '/M/Inner'\n  mark '/M/Inner' pub, re-export 'T' beside 'g', or make 'g' private"
        ),
        "unexpected error: {error}"
    );
}

// The module named is the one under the widest subtree the name reaches. Exported out of `Inner` into `Outer`, `T` reaches all of `M`: `pub` on `Inner` would change nothing, and `Outer` is what stops it there.
#[test]
fn a_name_exported_into_a_private_module_is_told_of_that_one() {
    let error = run_err(
        r#"
        pub mod M
            mod Outer
                mod Inner
                    pub struct T : pub Type { Type }
                end
                pub use Inner/{T};
            end
            use Outer/{T};
            pub let g(t : T) -> T = t;
        end
        Type
    "#,
    );

    assert!(
        error.contains("'T' is in the private module '/M/Outer'")
            && error.contains("mark '/M/Outer' pub"),
        "unexpected error: {error}"
    );
}

// A name written without `pub` is the declaration to mark, whatever its module is written with.
#[test]
fn a_private_name_in_a_private_module_is_told_to_mark_the_name() {
    let error = run_err(
        r#"
        pub mod M
            mod Inner
                struct T : Type { Type }
                pub let make(t : T) -> T = t;
            end
        end
        Type
    "#,
    );

    assert!(
        error.contains(
            "public item 'make' exposes private item 'T' in its signature\n  mark 'T' pub, or make 'make' private"
        ),
        "unexpected error: {error}"
    );
}

/// A name under an internal root is seen where an export shows it, and nowhere else past the units that may name the root: the fixture's `/std` shows `/sys/Handle` only from its private `Hidden`, so a public signature naming it is refused, and the refusal names the module that keeps it.
#[test]
fn a_closed_roots_name_shown_only_privately_stays_out_of_a_public_signature() {
    let error = prepare_with("pub let leak(h: Hidden/Handle) -> Type = Type;").unwrap_err();

    assert!(
        error.contains("exposes") && error.contains("Hidden"),
        "unexpected error: {error}"
    );
}

/// The unit that may name an internal root sees what it holds: a private signature names `/sys/Handle` though no export shows it.
#[test]
fn a_private_signature_may_name_a_closed_roots_name_no_export_shows() {
    prepare_with("let quiet(h: /sys/Handle) -> Type = Type;").unwrap();
}

/// A name under an internal root that a public export shows is seen by every reader: a later unit's public signature names `/sys/Bool/True` through the `/std` that shows it.
#[test]
fn a_closed_roots_name_shown_publicly_may_stand_in_any_signature() {
    lower_with_prelude(
        "pub mod M\n    pub let checked(p: /std/Bool/True) -> Type = Type;\nend\nType",
    )
    .unwrap();
}

/// A module an export shows carries its public members with it, as resolution walks into them: the `/std` that shows `/sys/Bool/True` shows its constructor `qed`, so a later unit's public signature may name it — as a string literal's proof of validity names it in every signature that holds one.
#[test]
fn a_constructor_of_a_type_shown_publicly_may_stand_in_any_signature() {
    lower_with_prelude(
        "pub mod M\n    pub let checked(p: /std/Bool/True/qed()) -> Type = Type;\nend\nType",
    )
    .unwrap();
}
