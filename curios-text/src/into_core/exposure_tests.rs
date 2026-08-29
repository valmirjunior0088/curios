//! The representation-exposure audit, and the direct type aliases that carry provenance through it.

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
fn transparent_alias_exposure_audits_the_complete_representation() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { Hidden }
            pub let Alias : Type = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn parameterized_transparent_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            pub let Alias(A : Type) -> Type = Open(A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn specialized_direct_type_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            pub let Alias : Type = Open(Type);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn reordered_direct_type_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type, B : Type) : pub Type { hidden : Hidden, a : A, b : B }
            pub let Alias(A : Type, B : Type) -> Type = Open(B, A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn chained_direct_type_aliases_preserve_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            let Middle(A : Type) -> Type = Open(A);
            pub let Alias(A : Type) -> Type = Middle(A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn constant_direct_type_family_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { hidden : Hidden }
            pub let Alias(_A : Type) -> Type = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn direct_type_alias_audits_private_body_dependencies() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias : Type = Open(Hidden);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn direct_type_alias_accepts_separately_exposed_body_dependencies() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias : Type = Open(Hidden);
        end
        Type
    "#);
}

#[test]
fn direct_type_alias_does_not_expose_an_opaque_nominals_fields() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Opaque(A : Type) : Type { hidden : Hidden, value : A }
            pub let Alias : Type = Opaque(Type);
        end
        Type
    "#);
}

#[test]
fn local_head_is_not_a_direct_type_alias() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias(F : (Type) -> Type) -> Type = F(Hidden);
        end
        Type
    "#);
}

#[test]
fn computed_heads_are_not_direct_type_aliases() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let LetHead : Type =
                let F : (Type) -> Type = Open;
                F(Hidden);
            pub let BetaHead : Type = ((A : Type) => Open(A))(Hidden);
            pub let ProjectionHead : Type = (Open, Open).0(Hidden);
            pub let MatchHead : Type =
                match true
                | true => Open(Hidden)
                | false => Open(Hidden)
                end;
        end
        Type
    "#);
}

#[test]
fn aliased_universe_annotation_is_not_a_direct_type_alias() {
    run(r#"
        mod M
            pub let Universe : Type = Type;
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { value : A }
            pub let Alias : Universe = Open(Hidden);
        end
        Type
    "#);
}

#[test]
fn cyclic_direct_type_aliases_terminate_without_nominal_exposure() {
    run(r#"
        mod M
            pub let A(T : Type) -> Type = B(T)
            and B(T : Type) -> Type = A(T);
        end
        Type
    "#);
}

#[test]
fn non_type_identity_alias_still_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { hidden : Hidden }
            pub let Alias : {} = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn standard_library_direct_type_aliases_pass_exposure_audit() {
    lower_with_prelude("use /std/Str/{Valid}; Valid").unwrap();
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

// A re-exports x from B; B re-exports x from A; nobody declares x. Following the chain returns to the start without a concrete target → cyclic, not missing.
#[test]
fn rejects_cyclic_re_export_with_no_concrete_target() {
    assert!(
        run_err(
            r#"
        pub mod A
            pub use /B/{x};
        end
        pub mod B
            pub use /A/{x};
        end
        Type
    "#
        )
        .contains("cyclic re-export")
    );
}

// Two public declarations of the same label in the same namespace conflict at phase 2, before any elaboration.
#[test]
fn rejects_duplicate_public_declaration() {
    assert!(
        run_err(
            r#"
        pub let x : Type = Type;
        pub let x : Type = Type;
        Type
    "#
        )
        .contains("duplicate public declaration")
    );
}
