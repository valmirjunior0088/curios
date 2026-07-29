use crate::*;

#[test]
fn display_unbound_variable() {
    let err = Error::unbound_variable(Subterm::Prim(Prim::NatType));
    assert_eq!(err.to_string(), "unbound variable: Nat");
}

#[test]
fn display_not_a_function() {
    let err = Error::not_a_function(Subterm::Prim(Prim::NatType));
    assert_eq!(
        err.to_string(),
        "applied a non-function\n  head has type: Nat"
    );
}

#[test]
fn display_type_mismatch_shows_both_types() {
    let err = Error::type_mismatch(Subterm::Prim(Prim::NatType), Subterm::Prim(Prim::BoolType));
    let s = err.to_string();
    assert!(s.contains("Nat"), "should contain inferred Nat: {s}");
    assert!(s.contains("Bool"), "should contain expected Bool: {s}");
}
