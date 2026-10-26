//! A sequence fold answers alike at every spelling of its operands: what `Bytes` and `List` operations decide over a symbolic value, and what their rows let a bound or a validity discharge.

use super::test_support::*;

// The shapes a fold reads a `Bytes` value by: a fill of a successor opens with its atom, which is the cons `x[a, ..fill]`, and the same run spelled as a snoc ends with it. The two are one value at every count, so a strip or a byte split that told them apart would refute at the symbolic count what `n = 0` makes `true`, and `absurd(0, 7, qed)` would be a closed inhabitant of `False`. What holds them is that a chunk facing an element is handed on, never clashed: the comparison stays a stuck `==`.
#[test]
fn a_fill_and_its_snoc_spelling_are_never_told_apart() {
    rejected_by(
        r#"
        use /std/{Nat, Byte, Bytes};
        use /std/Bool/{True, False, Holds};

        let absurd(n: Nat, a: Byte, h: Holds(Bytes/eql(Bytes/replicate(n + 1, a), x[..Bytes/replicate(n, a), a]))) -> False =
            h;

        absurd(0, 7, True/qed())
        "#,
        "inferred: Holds(x[a, ..Bytes/replicate(n, a)] == x[..Bytes/replicate(n, a), a])",
    );
}

// A fill of a count nothing decides may be the empty value, so it is no clash against `x[]`, as a window of such a count is not (`a_window_that_may_be_empty_does_not_clash_with_the_empty_value`): the word strip reads it as one chunk of unknown length. Read as its leading atom, the arm would be excused and `absurd(0, 7, Eq/refl())` would prove `False`.
#[test]
fn a_fill_that_may_be_empty_does_not_clash_with_the_empty_value() {
    rejected_by(
        r#"
        use /std/{Eq, Nat, Byte, Bytes};
        use /std/Bool/{False};

        let absurd(n: Nat, a: Byte, h: Eq()(Bytes/replicate(n, a), x[])) -> False =
            match h end;

        absurd(0, 7, Eq/refl())
        "#,
        "is not provably impossible at the scrutinee's indices",
    );
}

// Two windows of one base fuse only where the second begins where the first ends; across a gap of one the concatenation skips a byte the single window holds. Fused anyway, `fused` would hold of every base, and at `x[1, 2, 3]` it states `x[1, 3] == x[1, 2]`, which is `False`.
#[test]
fn two_windows_across_a_gap_are_not_one_window() {
    rejected_by(
        r#"
        use /std/{Nat, Bytes};
        use /std/Bool/{True, False, Holds};

        let fused(
            b: Bytes,
            m: Nat,
            n: Nat,
            @ok: Holds(m + 1 + n <= Bytes/len(b)),
        ) -> Holds(Bytes/eql(x[..Bytes/slice(b, 0, m), ..Bytes/slice(b, m + 1, n)], Bytes/slice(b, 0, m + n))) =
            True/qed();

        fused(x[1, 2, 3], 1, 1)
        "#,
        "expected: Holds(x[..Bytes/slice(b, 0, m), ..Bytes/slice(b, m + 1, n)] == Bytes/slice(b, 0, m + n))",
    );
}

// An index into a concatenation lies inside its second operand once it has passed the first one's whole length, and the seam is read by cancelling that length off the index. The second operand's own length is no such seam: read as one, `get(b ++ c, len(c))` would be `get(c, 0)` everywhere, which at `x[1] ++ x[2, 3]` is `3` against `2`.
#[test]
fn an_index_past_the_wrong_operand_is_no_seam() {
    rejected_by(
        r#"
        use /std/{Nat, Bytes, Eq, Byte};
        use /std/Bool/{True, Holds};

        let shifted(
            b: Bytes,
            c: Bytes,
            @inside: Holds(Bytes/len(c) < Bytes/len(x[..b, ..c])),
            @first: Holds(0 < Bytes/len(c)),
        ) -> Eq()(Bytes/get(x[..b, ..c], Bytes/len(c)), Bytes/get(c, 0)) =
            Eq/refl();

        let wrong: Eq()(Byte/to_nat(Bytes/get(x[1, 2, 3], 2)), Byte/to_nat(Bytes/get(x[2, 3], 0))) =
            Eq/cong((y: Byte) => Byte/to_nat(y), shifted(x[1], x[2, 3]));

        Eq/subst((v: Nat) => Holds(v == 3), wrong, True/qed())
        "#,
        "expected: Eq(@Byte)(Bytes/get(x[..b, ..c], Bytes/len(c)), Bytes/get(c, 0))",
    );
}

// A bound is discharged by the folds its proposition reduces through, and a length past an operand that may be empty is not strictly past it: the sum's shared summand cancels, and `0 < Bytes/len(c)` is what is left. Discharged anyway, `longer(x[], x[])` would be a closed proof of `0 < 0`.
#[test]
fn a_length_past_an_operand_that_may_be_empty_is_no_strict_bound() {
    rejected_by(
        r#"
        use /std/{Bytes};
        use /std/Bool/{True, Holds};

        let longer(b: Bytes, c: Bytes) -> Holds(Bytes/len(b) < Bytes/len(x[..b, ..c])) =
            True/qed();

        longer(x[], x[])
        "#,
        "expected: Holds(0 < Bytes/len(c))",
    );
}

// `map` by a function whose body reduces to its binder is the list itself, and only then: `v - 1 + 1` is `v` everywhere but at `0`, where truncation makes it `1`. Read as the identity, `moved([0])` would hold `[1]` one value with `[0]`.
#[test]
fn a_map_by_a_function_that_is_the_identity_almost_everywhere_is_not_its_list() {
    rejected_by(
        r#"
        use /std/{Nat, List, Eq};
        use /std/Bool/{True, Holds};

        let moved(xs: List(Nat)) -> Eq()(List/map(xs, (v: Nat) => v - 1 + 1), xs) =
            Eq/refl();

        let sum(ys: List(Nat)) -> Nat = List/fold(ys, 0, (y: Nat, acc: Nat) => y + acc);

        let at_zero: Eq()(sum(List/map([0], (v: Nat) => v - 1 + 1)), sum([0])) =
            Eq/cong(sum, moved([0]));

        Eq/subst((v: Nat) => Holds(v == 1), at_zero, True/qed())
        "#,
        "expected: Eq(@List(Nat))(List/map(@Nat, @Nat, xs, (v) => v - 1 + 1), xs)",
    );
}

// A left fold over a concatenation threads its accumulator through the operands in order; read in the other order, the fold of `[1] ++ [2]` by a step that does not commute would be `21` where it is `12`.
#[test]
fn a_fold_over_a_concatenation_threads_its_operands_in_order() {
    rejected_by(
        r#"
        use /std/{Nat, List, Eq};
        use /std/Bool/{True, Holds};

        let step(x: Nat, acc: Nat) -> Nat = acc * 10 + x;

        let swapped(xs: List(Nat), ys: List(Nat)) -> Eq()(List/fold([..xs, ..ys], 0, step), List/fold(xs, List/fold(ys, 0, step), step)) =
            Eq/refl();

        Eq/subst((v: Nat) => Holds(v == 12), swapped([1], [2]), True/qed())
        "#,
        "List/fold(@Nat, @Nat, xs, List/fold(@Nat, @Nat, ys, 0,",
    );
}

// `Str` and `Char` carry their validity as a hidden field the literal fills only where the decision reduces to `True`. UTF-8's `ED A0 80` encodes the surrogate `U+D800`: `ED` admits only `80..=9F` after it, where every other three-byte lead but `E0` admits `80..=BF`, and a scan that let it take that range would hand `Str/Valid/decode_head` a `Char` whose own validity is `False`.
#[test]
fn a_surrogate_is_neither_text_nor_a_character() {
    rejected_by(
        r#"
        use /std/{Str};

        let forged: Str = Str { bytes = x[0xED, 0xA0, 0x80] };

        /std/print("FORGED")
        "#,
        "nothing discharged Str/Valid(x[0xED, 0xA0, 0x80]), which reduces to /std/Bool/False",
    );
    rejected_by(
        r#"
        use /std/{Char};

        let forged: Char = Char { code = 0xD800 };

        /std/print("FORGED")
        "#,
        "Char/Valid(55296), which reduces to /std/Bool/False",
    );
}
