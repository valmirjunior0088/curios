//! Plicity at a lambda and a bare reference: the hidden binders a lambda writes or has inserted, its surplus and short binders, and the hidden arguments a reference takes at its expectation.

use {
    crate::tests::{error, run},
    curios_utilities::fill,
};

// === Plicity: insertion of omitted hidden binders, and exact checking of written ones. ====

// A lambda checked against `(@A : Type, x : A) -> A` may omit the implicit binder entirely: elaboration inserts `@A` and binds the sole explicit slot.
#[test]
fn lambda_inserts_an_omitted_implicit_binder() {
    let source = r#"
        use /std/{Nat, Str};
        let id : (@A : Type, x : A) -> A = (x) => x;
        /std/print(Nat/to_str(id(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// The same definition may instead write the implicit binder explicitly; both spellings elaborate to the same canonical function.
#[test]
fn lambda_may_write_the_implicit_binder() {
    let source = r#"
        use /std/{Nat, Str};
        let id : (@A : Type, x : A) -> A = (@A, x) => x;
        /std/print(Nat/to_str(id(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// A lambda checked against a type with both an implicit and a witness binder may omit both. The inserted witness binder still joins witness resolution, so the body resolves `Show(A)` through it.
#[test]
fn lambda_inserts_an_omitted_witness_binder() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let showit : (@A : Type, use Show(A), x : A) -> Str = (x) => Show/show(x);
        /std/print(showit(7))
        "#;
    assert_eq!(run(source), b"7");
}

// The witness member may be written, as `use _`: it holds its place, and the body reaches the witness by resolution.
#[test]
fn lambda_may_write_the_witness_member() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let showit : (@A : Type, use Show(A), x : A) -> Str = (@A, use _, x) => Show/show(x);
        /std/print(showit(7))
        "#;
    assert_eq!(run(source), b"7");
}

// A plain binder can never bind a hidden slot: written against `(@A, x) -> A`, the first plain binder claims the sole explicit slot, so the second is surplus.
#[test]
fn lambda_plain_binder_never_binds_a_hidden_slot() {
    let source = r#"
        use /std/{Nat, Str};
        let bad : (@A : Type, x : A) -> A = (a, x) => x;
        /std/print(Nat/to_str(bad(5)))
        "#;
    // The implicit is inserted before `a`, so `a` binds `x` and `x` is surplus, which is what the refusal says — a count would say `2, 2`.
    assert!(
        error(source).contains("claims no parameter"),
        "{}",
        error(source)
    );
}

// The surplus refusal names the surplus, because no count pair can name this fault.
//
// Alignment is positional by plicity, so a lambda and its expected type can agree on their totals *and* on their explicit counts and still fail to align. Comparing totals would report `expected 3, got 3` for the first of these — self-contradictory, and silent about the rule that decides it.
#[test]
fn a_lambda_binder_that_claims_no_parameter_is_named_as_surplus() {
    let surplus = |source: &str| {
        let rendered = error(source);
        assert!(
            rendered.contains("claims no parameter") || rendered.contains("claim no parameter"),
            "{rendered}"
        );
        rendered
    };

    // syntax.md's own refused spelling: `A` binds the sole explicit slot and the rest are surplus.
    surplus(
        r#"
        use /std/{Str, Show};
        let bad : (@A : Type, use Show(A), value : A) -> Str = (A, show, value) => Show/show(value);
        /std/print(bad(5))
        "#,
    );

    // One explicit parameter, one explicit binder, and still a surplus: `@A` claims nothing.
    surplus(
        r#"
        use /std/{Nat};
        let bad : (x : Nat) -> Nat = (x, @A) => x;
        /std/print(Nat/to_str(bad(5)))
        "#,
    );
}

// A lambda short of an explicit parameter counts the explicit ones, which are the only parameters it may write.
//
// Counting the total would say `expected 3` for a type with two explicit parameters, and acting on it would mean writing the third — the spelling refused just above.
#[test]
fn a_lambda_short_of_a_parameter_counts_the_explicit_ones() {
    let source = r#"
        use /std/{Nat};
        let bad : (@A : Type, x : Nat, y : Nat) -> Nat = (x) => x;
        /std/print(Nat/to_str(bad(5, 6)))
        "#;
    assert!(
        error(source).contains("expected 2, got 1"),
        "{}",
        error(source)
    );
}

// A marked binder that reaches an explicit slot is a plicity mismatch: writing `@x` for a plain parameter is rejected, naming the required spelling.
#[test]
fn lambda_marked_binder_on_explicit_slot_is_rejected() {
    let source = r#"
        use /std/{Nat, Str};
        let bad : (x : Nat) -> Nat = (@x) => x;
        /std/print(Nat/to_str(bad(5)))
        "#;
    assert!(
        error(source).contains("explicit parameter"),
        "{}",
        error(source)
    );
}

// A bare reference whose type leads with an implicit binder, checked against a rigid non-arrow expectation, has the hidden prefix inserted at the reference. Plicity is part of function identity, so without insertion this configuration is a guaranteed mismatch — insertion only rescues errors.
#[test]
fn bare_reference_inserts_hidden_arguments_at_a_rigid_expectation() {
    let source = r#"
        use /std/{Nat, Str, Option};
        let none_of(@A : Type) -> Option(A) =
            Option/none();
        let x : Option(Nat) =
            match false
            | true => Option/some(1)
            | false => none_of
            end;
        let shown : Str =
            match x
            | some(n) => Nat/to_str(n)
            | none() => "none"
            end;
        /std/print(shown)
        "#;
    assert_eq!(run(source), b"none");
}

// The exemption: a bare reference assigned at its own hidden-headed function type keeps the polymorphic value — insertion fires only where that type could never convert.
#[test]
fn bare_reference_keeps_its_type_at_a_hidden_expectation() {
    let source = r#"
        use /std/{Nat, Str};
        let ident(@A : Type, a : A) -> A =
            a;
        let keep : (@A : Type, a : A) -> A =
            ident;
        /std/print(Nat/to_str(keep(9)))
        "#;
    assert_eq!(run(source), b"9");
}

/// A call handing `ident` where `F(b)` is expected, before and after the argument that solves `b`, with `F(true)` coming to `arrow`.
fn waiting_at(arrow: &str) -> String {
    fill!(
        r#"
        use /std/{Nat, Bool, Eq, print};
        let F(b: Bool) -> Type = match b: (_) => Type
            | true => ${arrow}
            | false => Nat
            end;
        let ident(@A: Type, a: A) -> A = a;
        let take(@b: Bool, f: F(b), e: Eq()(b, true)) -> Nat = 1;
        let early(@b: Bool, e: Eq()(b, true), f: F(b)) -> Nat = 1;
        print(Nat/to_str(take(ident, Eq/refl()) + early(Eq/refl(), ident)))
        "#,
        arrow,
    )
}

// `F(b)` at a `b` still to be solved is stuck on a metavariable, and so has not decided whether `ident`'s hidden binder is inserted: the reference waits until the implicit is solved and is read at the type `F(true)` comes to, so the call is admitted in either order of its arguments whether that type is hidden-headed or an explicit arrow.
#[test]
fn bare_reference_waits_at_an_expectation_stuck_on_a_metavariable() {
    assert_eq!(run(&waiting_at("(@A: Type, x: A) -> A")), b"2");
    assert_eq!(run(&waiting_at("(x: Nat) -> Nat")), b"2");
}

// A reference waiting at an expectation nothing decides is refused naming the implicit never solved, as the conversion it would otherwise have parked is.
#[test]
fn bare_reference_at_an_expectation_never_decided_names_what_was_never_solved() {
    let source = r#"
        use /std/{Nat, Bool, print};
        let F(b: Bool) -> Type = match b: (_) => Type
            | true => (x: Nat) -> Nat
            | false => Nat
            end;
        let ident(@A: Type, a: A) -> A = a;
        let take(@b: Bool, _: F(b)) -> Nat = 1;
        print(Nat/to_str(take(ident)))
        "#;
    let error = error(source);
    assert!(
        error.contains("never solved: the implicit argument 'b' of 'take'"),
        "{error}"
    );
}
