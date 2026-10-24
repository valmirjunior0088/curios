//! What the carriers' algebra decides beside the law grid: a law at an instance that changes its atoms, a metavariable solved through the cancellation and through the packed-literal view, and a type that eliminates what the algebra decides — each through both checkers.

use super::run;

// A law proved over bare atoms holds at an instance whose substitution reshapes them: `x + z <= y + z` at `x := a + b`, `y := b`, `z := a + 1` cancels a summand the bare statement never had, and both sides fall to `a <= 0`; `i - j + j` at `i := k · l`, `j := k + l` cancels a sum against itself. The instantiated type and the one written meet only through what the substitution created.
#[test]
fn a_law_holds_at_an_instance_that_changes_its_atoms() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Eq};
        let comm(x: Nat, y: Nat) -> Eq()(x + y, y + x) = Eq/refl();
        let at_sums(a: Nat, b: Nat) -> Eq()(2 * b + a + 1, (a + 1) + b * 2) = comm(b * 2, a + 1);
        let cancel(x: Nat, y: Nat, z: Nat) -> Eq()(x + z <= y + z, x <= y) = Eq/refl();
        let at_shared(a: Nat, b: Nat) -> Eq()(a <= 0, a + b <= b) = cancel(a + b, b, a + 1);
        let shift(i: Int, j: Int) -> Eq()(i - j + j, i) = Eq/refl();
        let at_products(k: Int, l: Int) -> Eq()(k * l, k * l - (k + l) + (k + l)) = Eq/sym(shift(k * l, k + l));
        /std/print("ok")
        "#),
        b"ok"
    );
}

// A metavariable is solved through the cancellation: `?n + 1` against `3` peels the shared floor and leaves `?n` against `2`, which solves it.
#[test]
fn a_metavariable_is_solved_through_the_cancellation() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq};
        let pred(@n: Nat, p: Eq()(n + 1, 3)) -> Nat = n;
        /std/print(Nat/to_str(pred(Eq/refl())))
        "#),
        b"2"
    );
}

// A metavariable is solved through the packed-literal view: `x[h]` is `append(x[], ?h)`, which no shape congruence relates to the folded literal `x[7]`, so the elaborator splits the literal at the spine's known lengths and solves `?h` from the last byte. The kernel then sees the solution, and the two spellings agree by reduction.
#[test]
fn a_metavariable_is_solved_through_the_packed_literal_view() {
    assert_eq!(
        run(r#"
        use /std/{Byte, Bytes, Eq, Nat};
        let head(@h: Byte, p: Eq()(x[7], x[h])) -> Byte = h;
        /std/print(Nat/to_str(Byte/to_nat(head(Eq/refl()))))
        "#),
        b"7"
    );
}

// The concatenation twin: `x[h, ..t]` is the one-byte `append(x[], ?h)` followed by `?t`. A symbolic head stops the prefix strip at once, so the view cuts the literal at the head's known length, the unknown tail taking the rest, and solves `?h` as `1` and `?t` as `x[2, 3]`.
#[test]
fn a_metavariable_is_solved_as_the_rest_of_a_split_packed_literal() {
    assert_eq!(
        run(r#"
        use /std/{Byte, Bytes, Eq, Nat};
        let tail(@h: Byte, @t: Bytes, p: Eq()(x[1, 2, 3], x[h, ..t])) -> Bytes = t;
        /std/print(Nat/to_str(Bytes/len(tail(Eq/refl()))))
        "#),
        b"2"
    );
}

// A type that eliminates a connective the algebra decides is its arm: a binder at `match b && Bool/not(b)` is the function its false arm states, applied; and `/sys`'s `Holds`, a `Bool` match, is `False` at a contradiction, which a match with no arm takes apart, and `True` at a tautology, which `True/qed()` proves.
#[test]
fn a_type_that_eliminates_a_decided_connective_is_its_arm() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Eq, Nat};
        use /std/Bool/{Holds, True};
        let applied(b: Bool, f: match b && Bool/not(b) | true => Nat | false => (Nat) -> Nat end) -> Nat = f(1);
        let absurd(b: Bool, h: Holds(b && Bool/not(b))) -> Eq()(0, 1) = match h end;
        let excluded(b: Bool) -> Holds(b || Bool/not(b)) = True/qed();
        /std/print(Nat/to_str(applied(true, (n: Nat) => n + 1)))
        "#),
        b"2"
    );
}

// A scrutinee is read under the equations in force: in the true arm of `x < y`, the right leaf of `c || (x < y)`, which the fold leaves as written behind its stuck left, is `true` by the arm's equation, so a match on the disjunction takes its true arm.
#[test]
fn a_scrutinee_is_read_under_an_arms_equation() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Eq, Nat};
        let in_arm(x: Nat, y: Nat, c: Bool) -> Nat =
            match x < y: (_) => Nat
            | true =>
                let e: Eq()(match c || (x < y) | true => 1 | false => 2 end, 1) = Eq/refl();
                1
            | false => 0
            end;
        /std/print(Nat/to_str(in_arm(1, 2, false)))
        "#),
        b"1"
    );
}
