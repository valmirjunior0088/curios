//! Labels as part of a tuple's identity, projection by label, and named construction against them.

use super::test_support::*;

#[test]
fn let_bound_tuple_with_an_effectful_field_lowers() {
    // A `let` bound to a tuple one of whose fields is an opaque foreign call: the field cannot be lowered in a pure-name position, so the binding must take the CPS join-block path in `into_cont`. Head-only purity classification used to route the whole `let` through `lower_pure_name` and panic the compiler on the field's host intrinsic. End-to-end guard for `is_pure_term`. The field stays the call itself — a description the projection then forces — so the effectful term is still what the tuple carries.
    let source = r#"
        foreign frobnicate : (Nat) -> Nat;
        let t = (frobnicate(5), 2);
        let n = t.0!;
        /std/print(/std/Nat/to_str(n))
    "#;

    assert!(compile(source, None).is_ok());
}

// --- B2: named tuple fields ----------------------------------------------

#[test]
fn proj_by_label_resolves_to_its_position() {
    // `.label` is elaboration-time sugar for the positional projection, so both spellings typecheck identically.
    let source = r#"
        use /std/{Nat, Bytes, Handle};
        let r : { status : Nat, payload : Bytes } = (0, /std/Str/to_bytes("ok"));
        let by_label : Bytes = r.payload;
        let by_index : Bytes = r.1;
        by_index
    "#;

    assert!(typecheck(source, Some("/std/Bytes")).is_ok());
}

#[test]
fn proj_unknown_label_names_the_available_fields() {
    let source = r#"
        use /std/{Nat, Bytes};
        let r : { status : Nat, payload : Bytes } = (0, /std/Str/to_bytes("ok"));
        r.body
    "#;

    let error = typecheck(source, Some("/std/Bytes")).unwrap_err();
    assert!(
        error.contains("no field named 'body'") && error.contains("status"),
        "unexpected error: {error}"
    );
}

#[test]
fn duplicate_tuple_label_is_rejected() {
    let source = r#"
        use /std/{Nat};
        let r : { x : Nat, x : Nat } = (0, 1);
        r.x
    "#;

    let error = typecheck(source, Some("/std/Nat")).unwrap_err();
    assert!(
        error.contains("duplicate field label 'x'"),
        "unexpected error: {error}"
    );
}

#[test]
fn labels_are_part_of_type_identity() {
    // Same positional types, different label order: not convertible — this is what makes `.label` re-indexing impossible.
    let reordered = r#"
        use /std/{Nat};
        let p : { width : Nat, height : Nat } = (640, 480);
        let q : { height : Nat, width : Nat } = p;
        q.width
    "#;
    assert!(typecheck(reordered, Some("/std/Nat")).is_err());

    // Labeled and unlabeled spellings are distinct types too.
    let unlabeled = r#"
        use /std/{Nat};
        let p : { width : Nat, height : Nat } = (640, 480);
        let q : { Nat, Nat } = p;
        q.0
    "#;
    assert!(typecheck(unlabeled, Some("/std/Nat")).is_err());
}

#[test]
fn named_construction_checks_against_the_labels() {
    // Written names must match the expected type's labels positionally; bare fields are always accepted.
    let source = r#"
        use /std/{Nat, Bytes};
        let r : { status : Nat, payload : Bytes } = (status = 0, payload = /std/Str/to_bytes("ok"));
        let mixed : { status : Nat, payload : Bytes } = (status = 0, /std/Str/to_bytes("ok"));
        r.status
    "#;
    assert!(typecheck(source, Some("/std/Nat")).is_ok());

    let wrong_name = r#"
        use /std/{Nat, Bytes};
        let r : { status : Nat, payload : Bytes } = (code = 0, payload = /std/Str/to_bytes("ok"));
        r.status
    "#;
    let error = typecheck(wrong_name, Some("/std/Nat")).unwrap_err();
    assert!(
        error.contains("'code'") && error.contains("'status'"),
        "unexpected error: {error}"
    );

    let unlabeled_type = r#"
        use /std/{Nat, Bytes};
        let r : { Nat, Bytes } = (status = 0, /std/Str/to_bytes("ok"));
        r.0
    "#;
    assert!(typecheck(unlabeled_type, Some("/std/Nat")).is_err());
}

#[test]
fn dependent_record_projects_by_label() {
    // Labels bind dependently: a later field's type mentions an earlier label, and label projection re-types through the dependency.
    let source = r#"
        let p : { T : Type, x : T } = (T = /std/Nat, x = 3);
        let v : p.T = p.x;
        /std/print(/std/Nat/to_str(v))
    "#;

    assert!(typecheck(source, None).is_ok());
}
