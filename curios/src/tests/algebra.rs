//! What the carriers' algebra decides beside the law grid: a law at an instance that changes its atoms, and a metavariable solved through the cancellation and through the packed-literal view — each through both checkers.

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

// A metavariable is solved through the packed-literal view: `b[h]` is `append(b[], ?h)`, which no shape congruence relates to the folded literal `b[1]`, so the elaborator splits the literal at the spine's known lengths and solves `?h` from the last bit. The kernel then sees the solution, and the two spellings agree by reduction.
#[test]
fn a_metavariable_is_solved_through_the_packed_literal_view() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Bits, Eq};
        let head(@h: Bool, p: Eq()(b[1], b[h])) -> Bool = h;
        match head(Eq/refl()) | true => /std/print("true") | false => /std/print("false") end
        "#),
        b"true"
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
