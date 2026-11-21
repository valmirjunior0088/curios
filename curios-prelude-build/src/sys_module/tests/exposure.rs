//! The representation-exposure audit against the prelude's roots: what a signature may name of a root the prelude closes, and the standard library's own aliases through it.

use super::*;

#[test]
fn standard_library_direct_type_aliases_pass_exposure_audit() {
    lower_with_prelude("use /std/Str/{Valid}; Valid").unwrap();
}

/// A name under an internal root is seen where an export shows it, and nowhere else past the units that may name the root: the fixture's `/std` shows `/sys/Handle` only from its private `Hidden`, so a public signature naming it is refused, and the refusal names the module that keeps it.
#[test]
fn a_closed_roots_name_shown_only_privately_stays_out_of_a_public_signature() {
    let error = prepare_with("pub let leak(h: Hidden/Handle) -> Type = Type;").unwrap_err();

    assert!(
        error.contains("private item exposed") && error.contains("Hidden"),
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
