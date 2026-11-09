//! A sequence fold answers alike at every spelling of its operands: what `Bytes` and `List` operations decide over a symbolic value, and what their rows let a bound or a validity discharge.

use {
    super::test_support::*,
    crate::tests::test_support::*,
    curios_cert::Verdict,
    curios_common::Qualifier,
    curios_core::{Global, Intrinsic, Item, Nat, Subterm, Term, Zonked},
    curios_module::RootSource,
    curios_num::Binary,
    curios_pipeline::{DEFAULT_BUDGET, test_support::*},
    curios_surface::Entrypoint,
    std::hash::{DefaultHasher, Hash, Hasher},
};

// The shapes a fold reads a `Bytes` value by: a fill of a successor opens with its atom, which is the cons `x[a, ..fill]`, and the same run spelled as a snoc ends with it. The two are one value at every count, so a strip or a byte split that told them apart would refute at the symbolic count what `n = 0` makes `true`, and `absurd(0, 7, qed)` would be a closed inhabitant of `False`. What holds them is that a chunk facing an element is handed on, never clashed: the comparison stays a stuck `==`. The control states the hypothesis as its own conclusion, which at `n = 0` holds.
#[test]
fn a_fill_and_its_snoc_spelling_are_never_told_apart() {
    control(
        r#"
        use /std/{Nat, Byte, Bytes};
        use /std/Bool/{True, False, Holds};

        let absurd(n: Nat, a: Byte, h: Holds(Bytes/eql(Bytes/replicate(n + 1, a), x[..Bytes/replicate(n, a), a]))) -> Holds(Bytes/eql(Bytes/replicate(n + 1, a), x[..Bytes/replicate(n, a), a])) =
            h;

        let at_zero = absurd(0, 7, True/qed());
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (") -> Holds(Bytes/eql(Bytes/replicate(n + 1, a), x[..Bytes/replicate(n, a), a])) =", ") -> False ="),
    ])
    .by_elaborator_in("/absurd", |error| matches!(error, curios_elab::Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/sys/Bool/Holds(x[a, ..Bytes/replicate(n, a)] == x[..Bytes/replicate(n, a), a])"));
}

// A fill of a count nothing decides may be the empty value, so it is no clash against `x[]`, as a window of such a count is not (`a_window_that_may_be_empty_does_not_clash_with_the_empty_value`): the word strip reads it as one chunk of unknown length. Read as its leading atom, the arm would be excused and `absurd(0, 7, Eq/refl())` would prove `False`. The control's fill is of a successor, which opens with its atom and clashes.
#[test]
fn a_fill_that_may_be_empty_does_not_clash_with_the_empty_value() {
    control(
        r#"
        use /std/{Eq, Nat, Byte, Bytes};
        use /std/Bool/{False};

        let absurd(n: Nat, a: Byte, h: Eq()(Bytes/replicate(n + 1, a), x[])) -> False =
            match h end;

        let bad = absurd;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "Bytes/replicate(n + 1, a), x[]",
            "Bytes/replicate(n, a), x[]",
        ),
        ("let bad = absurd;", "let bad = absurd(0, 7, Eq/refl());"),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(
            error,
            curios_elab::Error::MatrixUncovered { indexed: true, .. }
        )
    });
}

// Two windows of one base fuse only where the second begins where the first ends; across a gap of one the concatenation skips a byte the single window holds. Fused anyway, `fused` would hold of every base, and at `x[1, 2, 3]` it states `x[1, 3] == x[1, 2]`, which is `False`. The control's windows meet, and fuse.
#[test]
fn two_windows_across_a_gap_are_not_one_window() {
    control(
        r#"
        use /std/{Nat, Bytes};
        use /std/Bool/{True, False, Holds};

        let fused(
            b: Bytes,
            m: Nat,
            n: Nat,
            @ok: Holds(m + 1 + n <= Bytes/len(b)),
        ) -> Holds(Bytes/eql(x[..Bytes/slice(b, 0, m), ..Bytes/slice(b, m, n)], Bytes/slice(b, 0, m + n))) =
            True/qed();

        let at = fused(x[1, 2, 3], 1, 1);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("..Bytes/slice(b, m, n)]", "..Bytes/slice(b, m + 1, n)]"),
    ])
    .by_elaborator_in("/fused", |error| matches!(error, curios_elab::Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/Holds(x[..Bytes/slice(b, 0, m), ..Bytes/slice(b, m + 1, n)] == Bytes/slice(b, 0, m + n))"));
}

// An index into a concatenation lies inside its second operand once it has passed the first one's whole length, and the seam is read by cancelling that length off the index. The second operand's own length is no such seam: read as one, `get(b ++ c, len(c))` would be `get(c, 0)` everywhere, which at `x[1] ++ x[2, 3]` is `3` against `2`. The control cancels the first operand's length, and reads `2` at both ends.
#[test]
fn an_index_past_the_wrong_operand_is_no_seam() {
    control(
        r#"
        use /std/{Nat, Bytes, Eq, Byte};
        use /std/Bool/{True, Holds};

        let shifted(
            b: Bytes,
            c: Bytes,
            @inside: Holds(Bytes/len(b) < Bytes/len(x[..b, ..c])),
            @first: Holds(0 < Bytes/len(c)),
        ) -> Eq()(Bytes/get(x[..b, ..c], Bytes/len(b)), Bytes/get(c, 0)) =
            Eq/refl();

        let read: Eq()(Byte/to_nat(Bytes/get(x[1, 2, 3], 1)), Byte/to_nat(Bytes/get(x[2, 3], 0))) =
            Eq/cong((y: Byte) => Byte/to_nat(y), shifted(x[1], x[2, 3]));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Holds(Bytes/len(b) < Bytes/len(x[..b, ..c]))", "Holds(Bytes/len(c) < Bytes/len(x[..b, ..c]))"),
        ("Bytes/get(x[..b, ..c], Bytes/len(b))", "Bytes/get(x[..b, ..c], Bytes/len(c))"),
        ("Bytes/get(x[1, 2, 3], 1)", "Bytes/get(x[1, 2, 3], 2)"),
    ])
    .by_elaborator_in("/shifted", |error| matches!(error, curios_elab::Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Byte)(Bytes/get(x[..b, ..c], Bytes/len(c)), Bytes/get(c, 0))"));
}

// A bound is discharged by the folds its proposition reduces through, and a length past an operand that may be empty is not strictly past it: the sum's shared summand cancels, and `0 < Bytes/len(c)` is what is left. Discharged anyway, `longer(x[], x[])` would be a closed proof of `0 < 0`. The control's bound is not strict, and what is left, `0 <= Bytes/len(c)`, holds.
#[test]
fn a_length_past_an_operand_that_may_be_empty_is_no_strict_bound() {
    control(
        r#"
        use /std/{Bytes};
        use /std/Bool/{True, Holds};

        let longer(b: Bytes, c: Bytes) -> Holds(Bytes/len(b) <= Bytes/len(x[..b, ..c])) =
            True/qed();

        let at = longer(x[], x[]);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Bytes/len(b) <= Bytes/len(", "Bytes/len(b) < Bytes/len("),
    ])
    .by_elaborator_in("/longer", |error| matches!(error, curios_elab::Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/Holds(0 < Bytes/len(c))"));
}

// `map` by a function whose body reduces to its binder is the list itself, and only then: `v - 1 + 1` is `v` everywhere but at `0`, where truncation makes it `1`. Read as the identity, `moved([0])` would hold `[1]` one value with `[0]`. The control maps by `v + 1 - 1`, which is `v` everywhere.
#[test]
fn a_map_by_a_function_that_is_the_identity_almost_everywhere_is_not_its_list() {
    control(
        r#"
        use /std/{Nat, List, Eq};
        use /std/Bool/{True, Holds};

        let moved(xs: List(Nat)) -> Eq()(List/map(xs, (v: Nat) => v + 1 - 1), xs) =
            Eq/refl();

        let sum(ys: List(Nat)) -> Nat = List/fold(ys, 0, (y: Nat, acc: Nat) => y + acc);

        let at_zero: Eq()(sum(List/map([0], (v: Nat) => v + 1 - 1)), sum([0])) =
            Eq/cong(sum, moved([0]));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("List/map(xs, (v: Nat) => v + 1 - 1)", "List/map(xs, (v: Nat) => v - 1 + 1)"),
        ("List/map([0], (v: Nat) => v + 1 - 1)", "List/map([0], (v: Nat) => v - 1 + 1)"),
    ])
    .by_elaborator_in("/moved", |error| matches!(error, curios_elab::Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(List(Nat))(List/map(@Nat, @Nat, xs, (v) => v - 1 + 1), xs)"));
}

// A left fold over a concatenation threads its accumulator through the operands in order; read in the other order, the fold of `[1] ++ [2]` by a step that does not commute would be `21` where it is `12`. The control states the order the fold threads.
#[test]
fn a_fold_over_a_concatenation_threads_its_operands_in_order() {
    control(
        r#"
        use /std/{Nat, List, Eq};
        use /std/Bool/{True, Holds};

        let step(x: Nat, acc: Nat) -> Nat = acc * 10 + x;

        let swapped(xs: List(Nat), ys: List(Nat)) -> Eq()(List/fold([..xs, ..ys], 0, step), List/fold(ys, List/fold(xs, 0, step), step)) =
            Eq/refl();

        let at = swapped([1], [2]);
        /std/print(Nat/to_str(List/fold([1, 2], 0, step)))
        "#,
        b"12",
    )
    .refused(&[
        ("List/fold(ys, List/fold(xs, 0, step), step)", "List/fold(xs, List/fold(ys, 0, step), step)"),
    ])
    .by_elaborator_in("/swapped", |error| matches!(error, curios_elab::Error::TypeMismatch { inferred, expected, .. } if pinned(inferred) == "/std/Eq/Eq(Nat)(List/fold(@Nat, @Nat, ys, List/fold(@Nat, @Nat, xs, 0, (x, acc) => acc * 10 + x), (x2, acc2) => acc2 * 10 + x2), List/fold(@Nat, @Nat, ys, List/fold(@Nat, @Nat, xs, 0, (x3, acc3) => acc3 * 10 + x3), (x4, acc4) => acc4 * 10 + x4))" && pinned(expected) == "/std/Eq/Eq(Nat)(List/fold(@Nat, @Nat, ys, List/fold(@Nat, @Nat, xs, 0, (x, acc) => acc * 10 + x), (x2, acc2) => acc2 * 10 + x2), List/fold(@Nat, @Nat, xs, List/fold(@Nat, @Nat, ys, 0, (x3, acc3) => acc3 * 10 + x3), (x4, acc4) => acc4 * 10 + x4))"));
}

// A pointwise operation reads the bytes its operands' runs spell, wherever the runs past the fusion cap split them: `not` over a 65-byte run beside a 66-byte one flips the first 65 bytes of one value and the next 66 of the other. A fold that placed the seam at 64 would answer `misaligned`; carried along `grouping` to the same bytes grouped otherwise, the claim becomes `not(regrouped) == misaligned`, which the fold over the other grouping refutes, so the closing term would be a proof of `False`. The control claims the bytes the runs spell, flipped.
#[test]
fn a_pointwise_operation_answers_the_bytes_its_runs_spell_wherever_they_split() {
    control(
        r#"
        use /std/{Bytes, Eq};
        use /std/Bool/{True, Holds};

        let spelled : Bytes = x[..Bytes/replicate(65, 0x0F), ..x[], ..Bytes/replicate(66, 0x3C)];
        let regrouped : Bytes = x[..Bytes/replicate(60, 0x0F), ..x[..Bytes/replicate(5, 0x0F), ..Bytes/replicate(66, 0x3C)]];
        let flipped : Bytes = x[..Bytes/replicate(65, 0xF0), ..Bytes/replicate(66, 0xC3)];

        let claimed : Holds(Bytes/eql(Bytes/not(spelled), flipped)) = True/qed();
        let grouping : Eq()(spelled, regrouped) = Eq/refl();

        let carried = Eq/subst((z: Bytes) => Holds(Bytes/eql(Bytes/not(z), flipped)), grouping, claimed);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("x[..Bytes/replicate(65, 0xF0), ..Bytes/replicate(66, 0xC3)]", "x[..Bytes/replicate(64, 0xF0), ..Bytes/replicate(67, 0xC3)]"),
    ])
    .by_elaborator_in("/claimed", |error| matches!(error, curios_elab::Error::TypeMismatch { .. }));
}

// Runs past the fusion cap stay a concatenation, and two such values are compared by the bytes they spell, not by the node that holds them: `x[..run, 0x01]` and `x[..run, 0x02]` share 65 bytes and part at the last. Held one value, `merged` would carry a decided equality of the two to its negation. The control equates the run with the same bytes grouped otherwise.
#[test]
fn runs_past_the_fusion_cap_that_part_at_their_last_byte_are_two_values() {
    control(
        r#"
        use /std/{Bytes, Eq};
        use /std/Bool/{True, Holds};

        let run : Bytes = Bytes/replicate(65, 0x00);
        let merged : Eq()(x[..run, 0x01], x[..Bytes/replicate(64, 0x00), 0x00, 0x01]) = Eq/refl();

        let carried = Eq/subst((z: Bytes) => Holds(Bytes/eql(z, x[..run, 0x01])), merged, True/qed());
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("0x00, 0x01]) = Eq/refl()", "0x00, 0x02]) = Eq/refl()"),
    ])
    .by_elaborator_in("/merged", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    });
}

// A run's hash reads 32 bytes of it, so two 100-byte runs apart only at the byte with index 1, which none of those 32 is, hash alike, as the test asserts first. Every table a fold answers from keys a term by that hash and confirms it by the bytes, so `sum(marked)` is `1` in the declaration that met `sum(zeros)` first; a table that took the hash for the value would answer it `0` there and `1` everywhere else. The control states the sum, `1`.
#[test]
fn two_runs_whose_sampled_bytes_agree_are_still_two_values() {
    let hashed = |bytes: Vec<u8>| {
        let mut hasher = DefaultHasher::new();
        Binary::from_bytes(bytes).hash(&mut hasher);
        hasher.finish()
    };
    let mut marked = vec![0x00; 100];
    marked[1] = 0x01;
    assert_eq!(
        hashed(vec![0x00; 100]),
        hashed(marked),
        "the two runs hash alike, which is the case the program puts",
    );

    control(
        r#"
        use /std/{Bytes, Nat, Byte, Eq};
        use /std/Bool/{True, Holds};

        let zeros : Bytes = Bytes/replicate(100, 0x00);
        let marked : Bytes = Bytes/xor(zeros, x[..x[0x00, 0x01], ..Bytes/replicate(98, 0x00)]);
        let sum(z: Bytes) -> Nat = Bytes/fold(z, 0, (h: Byte, acc: Nat) => acc + Byte/to_nat(h));

        let conflated : Eq()(sum(zeros) + sum(marked), 1) = Eq/refl();

        /std/print(Nat/to_str(sum(zeros) + sum(marked)))
        "#,
        b"1",
    )
    .refused(&[
        ("+ sum(marked), 1)", "+ sum(marked), 0)"),
    ])
    .by_elaborator_in("/conflated", |error| matches!(error, curios_elab::Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(1, 0)"));
}

// `Str` and `Char` carry their validity as a hidden field the literal fills only where the decision reduces to `True`. UTF-8's `ED A0 80` encodes the surrogate `U+D800`: `ED` admits only `80..=9F` after it, where every other three-byte lead but `E0` admits `80..=BF`, and a scan that let it take that range would hand `Str/Valid/decode_head` a `Char` whose own validity is `False`. The controls are the last values before the surrogates, `ED 9F 80` and `U+D7FF`.
#[test]
fn a_surrogate_is_neither_text_nor_a_character() {
    control(
        r#"
        use /std/{Str};

        let edge: Str = Str { bytes = x[0xED, 0x9F, 0x80] };

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("0x9F", "0xA0"),
    ])
    .by_elaborator_in("/edge", |error| matches!(error, curios_elab::Error::UninferredImplicit { bound, reduct: Some(reduct), .. } if pinned(bound) == "/std/Str/Valid/Valid(x[0xED, 0xA0, 0x80])" && pinned(reduct) == "/sys/Bool/False"));
    control(
        r#"
        use /std/{Char};

        let edge: Char = Char { code = 0xD7FF };

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("0xD7FF", "0xD800"),
    ])
    .by_elaborator_in("/edge", |error| matches!(error, curios_elab::Error::UninferredImplicit { bound, reduct: Some(reduct), .. } if pinned(bound) == "/std/Char/Valid/Valid(55296)" && pinned(reduct) == "/sys/Bool/False"));
}

// The kernel's half of `a_surrogate_is_neither_text_nor_a_character`: the kernel reads no privacy and discharges nothing, so a value built past the elaborator is held by the field its literal states alone. `c` is elaborated at `A`, whose validity reduces to `True`, and its code is rewritten to the surrogate `U+D800` with the elaborated proof kept, which the field then states at `Valid(55296)`, reducing to `False`.
#[test]
fn a_surrogate_character_built_past_the_elaborator_is_refused_by_its_hidden_field() {
    let source = r#"
        use /std/{Char};

        pub let c: Char = Char { code = 65 };

        /std/print("A")
        "#;
    let entrypoint = source.parse::<Entrypoint>().expect("the subject parses");
    let mut program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the subject elaborates")
        .program;
    let control = Zonked::project(&program).expect("an elaborated program is zonked");
    assert_eq!(
        recheck_with_prelude(&control, DEFAULT_BUDGET),
        Vec::new(),
        "the kernel accepts `c` at `A`"
    );
    let name = Global::Authored(Qualifier::from(["c"]));
    let mut forged = 0;
    for item in program.definitions.items.iter_mut() {
        let Item::Let(definition) = item else {
            continue;
        };
        if definition.name != name {
            continue;
        }
        let Subterm::Struct(literal) = &*definition.body else {
            panic!("`c` elaborates to a literal: {:?}", definition.body);
        };
        let mut literal = literal.clone();
        literal.fields[0] = Term::intrinsic(Intrinsic::Nat(Nat::new(0xD800usize)));
        definition.body = Term::from(Subterm::Struct(literal));
        forged += 1;
    }
    assert_eq!(forged, 1, "the subject declares `c` once");

    let program = Zonked::project(&program).expect("a forged literal stays zonked");
    let verdicts = recheck_with_prelude(&program, DEFAULT_BUDGET);
    assert!(
        matches!(
            verdicts.as_slice(),
            [Verdict { name: Some(refused), error: curios_cert::Error::Mismatch { .. } }] if *refused == name
        ),
        "the kernel refuses the forged `c` alone, as a mismatch: {verdicts:?}"
    );
}

// A match and a projection force what they read, so where a fold answers a recursive call — its step returning the accumulator, its initial value the call — both checkers force the call before reading it: `f(1)` is `7`, `c3`'s type is `Bool`, `S` is `Bool` and `p`'s instance is `True`. Read at the fold's unforced answer, `f`'s step stands neutral and `y` is refused by both checkers, and the elaborator and the kernel part on `c3` and `s`. `curios_cert`'s `recheck::closed_computation_tests` puts the module to the kernel under this name.
#[test]
fn a_match_or_a_projection_forces_the_call_a_fold_answers() {
    assert_eq!(
        run(r#"
            use /std/{Nat, List, Bool};
            use /std/Bool/{True, Holds};

            pub let f(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => match List/fold([5], f(k), (_: Nat, b: Nat) => b) | 0 => 7 | _ => 9 end end;
            pub let y: Holds(f(1) == 7) = True/qed();

            pub let member(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => member(k) end;
            pub let c3: match List/fold([5], member(3), (_: Nat, b: Nat) => b) | 0 => Bool | _ => Nat end = true;
            pub let S: match List/fold([5], member(3), (_: Nat, b: Nat) => b) | 0 => Type | _ => Type end = Bool;
            pub let s: S = true;

            pub let pair(n: Nat) -> {Nat, Nat} = match n | 0 => (0, 1) | k + 1 => pair(k) end;
            pub let p: Holds(List/fold([5], pair(3), (_: Nat, b: {Nat, Nat}) => b).0 == 0) = True/qed();

            /std/print("ok")
            "#),
        b"ok"
    );
}

// Two byte runs are apart by their lengths only where those lengths differ: `x[c, d]` against `x[1, 2]` is one value at `c = 1` and `d = 2`, so the fold of `!=` stays stuck on the two bytes and `qed` does not inhabit it, where `forged` would be `False`. `curios_cert`'s `recheck::intrinsic_tests` puts the module to the kernel under this name. The control's literal run is three bytes long, which no two bytes meet.
#[test]
fn byte_runs_of_one_length_are_not_apart_by_their_lengths() {
    control(
        r#"
        use /std/{Byte};
        use /std/Bool/{True, False, Holds};

        pub let apart(c: Byte, d: Byte) -> Holds(x[c, d] != x[1, 2, 3]) = True/qed();

        let forged = apart;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("x[1, 2, 3]", "x[1, 2]"),
        ("let forged = apart;", "let forged: False = apart(1, 2);"),
    ])
    .by_elaborator_in("/apart", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    });
}
