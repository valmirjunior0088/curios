//! What a motive may name and bind over an indexed family, and the binder count it is checked against.

use super::super::{error, run};

// === Motives ================================================================
//
// A motive is a term checked against the eliminator's motive type, `(ī : Ī(p̄)) -> I(p̄, ī) -> Sort`. There is no motive grammar: what follows `:` is parsed by `parse_term` and checked like any other term.

// The motive need not be a lambda at all. A top-level family of the right type is eta-expanded into the motive scope, so an elimination can name the family it proves rather than restating it.
#[test]
fn a_motive_may_name_a_top_level_family() {
    let source = r#"
        use /std/{Nat, Eq};
        let discriminates(s : Nat, t : Nat, q : Eq(s, t)) -> Type = Eq(t, s);
        let flip(@x : Nat, @y : Nat, p : Eq(x, y)) -> Eq(y, x) =
            match p : discriminates
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq(4, 4) = flip(Eq/refl());
        /std/print(Nat/to_str(4))
        "#;

    assert_eq!(run(source), b"4");
}

// A motive that ignores every binder is written with `_`s, one per index and one for the scrutinee — a constant motive is a lambda like any other, not a separate rung.
#[test]
fn a_constant_motive_on_an_indexed_family_binds_placeholders() {
    let source = r#"
        use /std/{Nat, Vec};
        let len(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_, _) => Nat
            | nil() => 0
            | cons(@m, x, xs) => m + 1
            end;
        /std/print(Nat/to_str(len(Vec/cons(1, Vec/cons(2, Vec/nil())))))
        "#;

    assert_eq!(run(source), b"2");
}

// A motive binder's annotation is an ordinary type in an ordinary position, so the scrutinee binder's annotation may name the index binders written before it — recovering the eliminated family on the motive line. This is the dependent-lambda-telescope rule (`tests::binders`) applied to a motive.
#[test]
fn a_motive_binder_annotation_may_name_earlier_index_binders() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip(@A : Type, @x : A, @y : A, p : Eq(x, y)) -> Eq(y, x) =
            match p : (s : A, t : A, q : Eq(s, t)) => Eq(t, s)
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq(6, 6) = flip(Eq/refl());
        /std/print(Nat/to_str(6))
        "#;

    assert_eq!(run(source), b"6");
}

// Plicity is expressible because the annotation is a real application: `Eq` hides its type parameter, so `Eq(s, t)` is how it is written here, and the old flat slot list that spelled it `Eq(A, s, t)` has no counterpart.
#[test]
fn a_motive_binder_annotation_obeys_the_families_plicity() {
    let source = r#"
        use /std/{Nat, Eq};
        let flip(@x : Nat, @y : Nat, p : Eq(x, y)) -> Eq(y, x) =
            match p : (s, t, q : Eq(@Nat, s, t)) => Eq(t, s)
            | refl(@z) => Eq/refl()
            end;
        let _ : Eq(7, 7) = flip(Eq/refl());
        /std/print(Nat/to_str(7))
        "#;

    assert_eq!(run(source), b"7");
}

// A `| _ =>` catch-all on an indexed family. Every motive binds its indices whether or not the body uses them, so a default no longer collides with a "pattern motive": the enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones.
#[test]
fn a_default_arm_is_allowed_on_an_indexed_family() {
    let source = r#"
        use /std/{Nat, Vec};
        let head_or(@T : Type, @n : Nat, v : Vec(T, n), fallback : T) -> T =
            match v : (_, _) => T
            | cons(@m, x, xs) => x
            | _ => fallback
            end;
        let v : Vec(Nat, 2) = Vec/cons(8, Vec/cons(9, Vec/nil()));
        /std/print(Nat/to_str(head_or(v, 0)))
        "#;

    assert_eq!(run(source), b"8");
}

// The binder count is checked against the index telescope, not inferred, so an under-bound motive reports as itself instead of as a domain mismatch.
#[test]
fn an_under_bound_motive_reports_its_binder_count() {
    let source = r#"
        use /std/{Nat, Vec};
        let len(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_) => Nat
            | nil() => 0
            | cons(@m, x, xs) => m + 1
            end;
        /std/print(Nat/to_str(len(Vec/nil())))
        "#;

    let error = error(source);
    assert!(
        error.contains("motive binds 1 name(s)") && error.contains("needs 2"),
        "unexpected error: {error}"
    );
}
