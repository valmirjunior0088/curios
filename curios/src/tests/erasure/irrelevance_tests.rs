//! The typing facts erasure rests on: two proofs of one proposition are interchangeable, data is not, and a proposition eliminates into data only when that cannot observe which proof it was.

use crate::tests::{error, run};

#[test]
fn prop_irrelevance_equates_distinct_proofs() {
    // Definitional proof irrelevance: `Le` is a strict proposition, so any two proofs of `Le(a, b)` are convertible — `refl` checks at `Eq(p, q)` even though `p` and `q` are distinct binders. Without the `Prop` short-circuit in `convert`, `refl : Eq(p, p)` would not check against `Eq(p, q)`.
    let source = r#"
        use /std/{Str, Eq, Nat, Io};
        induct Le: (Nat, Nat) -> pub Prop
        | z(@n : Nat): (0, n)
        | s(@a : Nat, @b : Nat, prev : Le(a, b)): (a + 1, b + 1)
        end
        let irrelevant(a : Nat, b : Nat, p : Le(a, b), q : Le(a, b))
            -> Eq(p, q) =
            Eq/refl();
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn data_is_not_proof_irrelevant() {
    // The bound on irrelevance: `Nat` is data, not a proposition, so distinct values are never equated — `refl` at `Eq(x, y)` for unequal `x`, `y` is rejected. Guards the `Prop` short-circuit against over-firing on non-props.
    let source = r#"
        use /std/{Str, Eq, Nat, Io};
        let bad(x : Nat, y : Nat) -> Eq(x, y) = Eq/refl();
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn large_elimination_of_a_prop_is_rejected() {
    // The large-elimination guard: `Le` is a multi-constructor proposition, so matching it into `Nat` (data) would observe which constructor it was, breaking irrelevance — rejected. The permitted cases (empty `False` via `absurd`, singleton `Eq` via `subst`, and prop→prop) are exercised by std.
    let source = r#"
        use /std/{Str, Nat, Io};
        induct Le: (Nat, Nat) -> pub Prop
        | z(@n : Nat): (0, n)
        | s(@a : Nat, @b : Nat, prev : Le(a, b)): (a + 1, b + 1)
        end
        let bad(a : Nat, b : Nat, p : Le(a, b)) -> Nat =
            match p : (_, _, _) => Nat
            | z(@_) => 0
            | s(@_, @_, _) => 1
            end;
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    let error = error(source);
    assert!(
        error.contains("cannot eliminate the proposition"),
        "unexpected error: {error}"
    );
}
