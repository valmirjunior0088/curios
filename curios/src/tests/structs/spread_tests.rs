//! Struct spread: identity copies, overrides, and every shape the update form rejects.

use crate::tests::{error, run};

// `T { ..base, f = x }` copies every unwritten field from `base`; a bare spread is the identity copy.
#[test]
fn struct_spread_identity_copy() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        let q : Pair(Nat, Nat) = Pair { ..p };
        /std/print(Nat/to_str(Nat/mul(q.fst, q.snd)))
        "#;

    assert_eq!(run(source), b"12");
}

// A single labeled override replaces its field; the rest copy across.
#[test]
fn struct_spread_single_override() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        let q : Pair(Nat, Nat) = Pair { ..p, snd = 9 };
        /std/print(Nat/to_str(Nat/add(q.fst, q.snd)))
        "#;

    assert_eq!(run(source), b"13");
}

// Overrides claim scattered positions (first and third), the gap copies — the order-preserving-subsequence rule with a hole in the middle.
#[test]
fn struct_spread_multi_override_with_gap() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Tri : pub Type { fst : Nat, snd : Nat, thd : Nat }
        let t : Tri = Tri { fst = 1, snd = 2, thd = 3 };
        let u : Tri = Tri { ..t, fst = 10, thd = 30 };
        /std/print(Nat/to_str(Nat/add(Nat/add(u.fst, u.snd), u.thd)))
        "#;

    assert_eq!(run(source), b"42");
}

// A dependent record updates when the override keeps the dependency consistent: `n` and `v : Vec(Nat, n)` replaced together.
#[test]
fn struct_spread_dependent_override_runs() {
    let source = r#"
        use /std/{Nat, Vec, Handle};
        pub struct Sized : pub Type { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())) };
        let t : Sized = Sized { ..s, n = 1, v = Vec/cons(42, Vec/nil()) };
        let total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : (_, _) => Nat
            | nil() => acc
            | cons(@m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        /std/print(Nat/to_str(total(t.v, 0)))
        "#;

    assert_eq!(run(source), b"42");
}

// Overriding a field that a copied field's type depends on is rejected: the copied `v` still has length 2, but the new telescope demands 3.
#[test]
fn struct_spread_dependent_field_mismatch_rejected() {
    let source = r#"
        use /std/{Nat, Vec, Handle};
        pub struct Sized : pub Type { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(1, Vec/cons(2, Vec/nil())) };
        let bad : Sized = Sized { ..s, n = 3 };
        /std/print("no")
        "#;

    error(source);
}

// The head may re-pin parameters, so an update can change them: the base is a `Pair(Nat, Nat)`, the result a `Pair(Str, Nat)` — the copied `snd` is checked against the new instantiation.
#[test]
fn struct_spread_parameter_changing_update() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 42 };
        let q : Pair(Str, Nat) = Pair { ..p, fst = "x" };
        /std/print(Nat/to_str(q.snd))
        "#;

    assert_eq!(run(source), b"42");
}

// A bare head with a spread and no annotation: the parameter metavariables are minted inside the base's frame and solved from the copied projections.
#[test]
fn struct_spread_bare_head_inference() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        let q = Pair { ..p, snd = 9 };
        /std/print(Nat/to_str(Nat/add(q.fst, q.snd)))
        "#;

    assert_eq!(run(source), b"13");
}

// The function-field definition sugar works as a spread override.
#[test]
fn struct_spread_function_field_override() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Api : pub Type { base : Nat, bump : (Nat) -> Nat }
        let api : Api = Api { base = 40, bump(x) = x };
        let api2 : Api = Api { ..api, bump(x) = Nat/add(x, 2) };
        /std/print(Nat/to_str(api2.bump(api2.base)))
        "#;

    assert_eq!(run(source), b"42");
}

// Overrides after a spread must be labeled: gaps make positions ambiguous.
#[test]
fn struct_spread_unlabeled_override_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, 5 };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("labeled"), "unexpected error: {error}");
}

// Overrides must follow the declared field order — the ordering law holds through a spread.
#[test]
fn struct_spread_out_of_order_override_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Tri : pub Type { fst : Nat, snd : Nat, thd : Nat }
        let t : Tri = Tri { fst = 1, snd = 2, thd = 3 };
        let bad = Tri { ..t, thd = 30, fst = 10 };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("order"), "unexpected error: {error}");
}

// A repeated override is caught by the same subsequence walk.
#[test]
fn struct_spread_duplicate_override_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, fst = 3, fst = 4 };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("order"), "unexpected error: {error}");
}

// An override naming no declared field is an unknown field.
#[test]
fn struct_spread_unknown_field_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, nope = 3 };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("no field"), "unexpected error: {error}");
}

// The spread must be the first entry.
#[test]
fn struct_spread_not_first_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { fst = 3, ..p };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("first"), "unexpected error: {error}");
}

// At most one spread per literal.
#[test]
fn struct_spread_multiple_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, ..p };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(error.contains("at most one"), "unexpected error: {error}");
}

// The base must be a value of the literal's own struct — a structurally matching tuple does not qualify.
#[test]
fn struct_spread_non_struct_base_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let bad = Pair { ..(fst = 1, snd = 2) };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("must itself be"),
        "unexpected error: {error}"
    );
}

// Nor does a same-shaped *other* record.
#[test]
fn struct_spread_wrong_struct_base_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        pub struct Dup(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let d : Dup(Nat, Nat) = Dup { fst = 1, snd = 2 };
        let bad = Pair { ..d };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("must itself be"),
        "unexpected error: {error}"
    );
}

// A spread is construction, so a private-representation struct cannot be spread-copied outside its declaring module either.
#[test]
fn struct_spread_private_outside_module_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius : Type { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        let bad = Celsius/Celsius { ..c };
        /std/print("no")
        "#;

    let error = error(source);
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
}
