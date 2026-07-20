use {super::run, curios_runtime::MockHost, std::time::Duration};

#[test]
fn utf8_slice_proof_aligns_with_byte_walk() {
    // The corrected `slice_closed` shape: a RELEVANT byte walk (`to_lead_bytes`) and
    // a MIRRORING proof walk (`to_lead_proof : Valid(to_lead_bytes(s, b))`). The proof
    // peels the derivation while the byte function reduces in lockstep — which only
    // works now that the `Bytes` eliminator decodes a *symbolic* cons (the new reduce
    // rule). The `cont`/`bad` arms reduce `to_lead_bytes(cont, cons(c,t))` to
    // `to_lead_bytes(step(c,cont), t)`, matching the recursive proof's index.
    let source = r#"
        use /std/{Io, Byte, Bytes, Nat, Bln};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        induct Scan : Type
        | lead()
        | cont(Nat, Nat, Nat)
        | bad()
        end

        let classify(c : Nat) -> Scan =
            match in_range(c, 0, 127)
            | true => Scan/lead()
            | false =>
                match in_range(c, 194, 223)
                | true => Scan/cont(1, 128, 191)
                | false => Scan/bad()
                end
            end;

        let step(c : Nat, s : Scan) -> Scan =
            match s
            | bad() => Scan/bad()
            | cont(rem, lo, hi) =>
                match in_range(c, lo, hi)
                | false => Scan/bad()
                | true =>
                    match Nat/eql(rem, 1)
                    | true => Scan/lead()
                    | false => Scan/cont(Nat/sub(rem, 1), 128, 191)
                    end
                end
            | lead() => classify(c)
            end;

        induct Utf8 : (s : Scan, b : Bytes) -> Type
        | stop() : (Scan/lead(), x\)
        | more(c : Byte, st : Scan, t : Bytes, rest : Utf8(step(Byte/to_nat(c), st), t))
            : (st, Bytes/concat(Bytes/append(x\, c), t))
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec to_lead_bytes(s : Scan, b : Bytes) -> Bytes =
            match s
            | lead() => b
            | cont(rem, lo, hi) =>
                match b
                | x\ => x\
                | x\h\..t; ih => to_lead_bytes(step(/std/Byte/to_nat(h), Scan/cont(rem, lo, hi)), t)
                end
            | bad() =>
                match b
                | x\ => x\
                | x\h\..t; ih => to_lead_bytes(step(/std/Byte/to_nat(h), Scan/bad()), t)
                end
            end;

        rec to_lead_proof(s : Scan, b : Bytes, d : Utf8(s, b)) -> Valid(to_lead_bytes(s, b)) =
            let go =
                match s : (s) => (p : Utf8(s, b)) -> Valid(to_lead_bytes(s, b))
                | lead() => (p) => p
                | cont(rem, lo, hi) => (p) =>
                    match p : (w : Utf8(q, x)) => Valid(to_lead_bytes(q, x))
                    | more(c, st, t, rest) => to_lead_proof(step(Byte/to_nat(c), st), t, rest)
                    end
                | bad() => (p) =>
                    match p : (w : Utf8(q, x)) => Valid(to_lead_bytes(q, x))
                    | more(c, st, t, rest) => to_lead_proof(step(Byte/to_nat(c), st), t, rest)
                    end
                end;
            go(d);

        Io/write(Io/stdout, x\6F\6B)
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn str_literal_prints_its_bytes() {
    let source = r#"
        use /std/{Str, Io};
        let s : Str = "hello";
        Io/print(s)
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"hello");
}

// `Str/of_bytes` is the checked constructor: it runs `is_utf8` and yields `some`
// for well-formed UTF-8. `é` is the bytes C3 A9, a valid 2-byte sequence.
#[test]
fn str_of_bytes_accepts_multibyte_utf8() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\c3\a9) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), [0xc3, 0xa9]);
}

// An invalid lead byte fails `is_utf8`, so `Str/of_bytes` returns `none`.
#[test]
fn str_of_bytes_rejects_invalid_utf8() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\ff) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("rejected")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"rejected");
}

// A truncated multi-byte sequence (a 2-byte lead with no continuation) fails the
// continuation-byte check, so `of_bin` returns `none`.
#[test]
fn str_of_bytes_rejects_truncated_multibyte() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\c3) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("rejected")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"rejected");
}

// The UTF-8 decode certification lemmas: naming them forces their bodies to
// elaborate (demand-driven checking). `cont_len` is the one that exercises the
// comparison intrinsic — `step` only reduces in `cont` state because
// `eql(succ(succ k''), 1)` now folds to `false`. `peel_byte`/`count_w`/
// `decode_head` are the cursor-free decode core: `peel_byte` advances the
// (prop) validity witness one byte without ever large-eliminating it,
// `count_w` is the codepoint count `len` is built on, and `decode_head`
// reads the head codepoint from the relevant bytes under that witness.
#[test]
fn utf8_decode_lemmas_type_check() {
    let source = r#"
        use /std/{Str, Nat, Io};
        let lemmas = (Str/bad_uninhabited, Str/cont_len, Str/peel_byte,
            Nat/lte_trans, Nat/lt_of_lte_succ, Nat/lte_add_mono_l, Str/count_w, Str/cont0_uninhabited, Str/take_conts, Str/decode_head);
        Io/print("ok")
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}

// `Str/len` and `Str/get` count and index by codepoint, not byte. The string is
// `a€😀` — a 1-byte, a 3-byte, and a 4-byte scalar — so its length is 3 and the
// codepoints decode to U+0061 (97), U+20AC (8364), and U+1F600 (128512).
#[test]
fn str_get_indexes_codepoints_of_every_width() {
    let source = r#"
        use /std/{Str, Char, Nat, Io, Option};
        match Str/of_bytes(x\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) =>
            Io/print(Str/flatten([
                Nat/to_str(Str/len(s)), ",",
                Nat/to_str(Char/to_nat(Option/unwrap_or(Str/get(s, 0), '?'))), ",",
                Nat/to_str(Char/to_nat(Option/unwrap_or(Str/get(s, 1), '?'))), ",",
                Nat/to_str(Char/to_nat(Option/unwrap_or(Str/get(s, 2), '?')))
            ]))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"3,97,8364,128512");
}

// `Str/at` is the proof-carrying indexer: `ok : Lt(i, len s)` flows (erased) into
// the `Bytes/at` bound, so it reads each codepoint with no fallback. Indexing `a€😀`
// at 0, 1, 2 yields U+0061, U+20AC, U+1F600 — same widths as `get`, but total.
#[test]
fn str_at_reads_codepoints_with_the_proof() {
    let source = r#"
        use /std/{Str, Char, Nat, Io, Option};
        match Str/of_bytes(x\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) =>
            let out =
                let r0 = Nat/try_lt(0, Str/len(s));
                let r1 = Nat/try_lt(1, Str/len(s));
                let r2 = Nat/try_lt(2, Str/len(s));
                match r0 : Option(Str)
                | none() => Option/none()
                | some(p0) => match r1 : Option(Str)
                | none() => Option/none()
                | some(p1) => match r2 : Option(Str)
                | none() => Option/none()
                | some(p2) => Option/some(Str/flatten([
                    Nat/to_str(Char/to_nat(Str/at(s, 0, p0))), ",",
                    Nat/to_str(Char/to_nat(Str/at(s, 1, p1))), ",",
                    Nat/to_str(Char/to_nat(Str/at(s, 2, p2)))]))
                end end end;
            Io/print(Option/unwrap_or(out, "oob"))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"97,8364,128512");
}

// `Str/slice` cuts at codepoint boundaries, so slicing `[1, 2)` out of `a€😀`
// yields the whole 3-byte euro sign — never a split sequence.
#[test]
fn str_slice_cuts_on_codepoint_boundaries() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) => Io/print(Str/slice(s, 1, 2))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), [0xe2, 0x82, 0xac]);
}

// An interior `Str/slice` over a mixed-width string exercises the single-pass
// O(n) cut: `drop_n` skips the leading `a` (1 byte) and `take_n` keeps the next
// three scalars (`é€😀`, of widths 2, 3, 4) as one window — never splitting a
// sequence. `aé€😀b` sliced `[1, 4)` yields `é€😀`.
#[test]
fn str_slice_spans_every_codepoint_width() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\61\c3\a9\e2\82\ac\f0\9f\98\80\62) : {}
        | some(s) => Io/print(Str/slice(s, 1, 4))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        [0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80]
    );
}

// `Str/trim` is string-typed and strips only the leading/trailing ASCII
// whitespace, leaving the interior multibyte scalar (`café`, with a 2-byte `é`)
// intact.
#[test]
fn str_trim_keeps_interior_multibyte() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\20\20\63\61\66\c3\a9\20\20) : {}
        | some(s) => Io/print(Str/trim(s))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), [0x63, 0x61, 0x66, 0xc3, 0xa9]);
}

// An all-whitespace string trims to empty: `trim_start` overshoots `trim_end`,
// and the `Nat/min` guard collapses the slice to nothing rather than trapping.
#[test]
fn str_trim_all_whitespace_is_empty() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bytes(x\20\09\20) : {}
        | some(s) => Io/print(Str/concat(Str/trim(s), "!"))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"!");
}

#[test]
fn char_of_nat_accepts_exact_unicode_scalar_boundaries() {
    let source = r#"
        use /std/{Char, Nat, Str, Option, Lst, Io};
        let render(n : Nat) -> Str =
            match Char/of_nat(n)
            | some(c) => Nat/to_str(Char/to_nat(c))
            | none() => "x"
            end;
        Io/print(Str/join(",", Lst/map(
            [0, 0xD7FF, 0xD800, 0xDFFF, 0xE000, 0x10FFFF, 0x110000], render)))
        "#;

    assert_eq!(run(source), b"0,55295,x,x,57344,1114111,x");
}

#[test]
fn char_to_utf8_matches_rust_across_widths_and_boundaries() {
    let scalars = [
        0x0, 0x7f, 0x80, 0x3bb, 0x7ff, 0x800, 0xd7ff, 0xe000, 0xffff, 0x10000, 0x1f600, 0x10ffff,
    ];
    let source = r#"
        use /std/{Char, Nat, Bytes, Option, Lst, Io};
        let encode(n : Nat) -> Bytes =
            Char/to_utf8(Option/unwrap_or(Char/of_nat(n), '?'));
        Io/write(Io/stdout, Bytes/flatten(Lst/map(
            [0, 0x7F, 0x80, 0x3BB, 0x7FF, 0x800, 0xD7FF, 0xE000, 0xFFFF,
             0x10000, 0x1F600, 0x10FFFF], encode)))
        "#;

    let expected = scalars
        .into_iter()
        .flat_map(|scalar| {
            char::from_u32(scalar)
                .expect("test scalar")
                .to_string()
                .into_bytes()
        })
        .collect::<Vec<_>>();
    assert_eq!(run(source), expected);
}

#[test]
fn str_logical_operations_use_certified_chars() {
    let source = r#"
        use /std/{Char, Str, Nat, Option, Show, Io};
        let s = "a€😀";
        let rebuilt = Str/fold(s, "", (c, acc) => Str/concat(acc, Show/show(c)));
        let second = Show/show(Option/unwrap_or(Str/get(s, 1), '?'));
        let euro = Option/unwrap_or(Str/find(s, '€'), 99);
        let supplementary = Option/unwrap_or(Str/find_index(s, (c) => Char/to_nat(c) > 0xFFFF), 99);
        let shown = Show/show('😀');
        let folded = Str/eql_ascii_ci("AbÉ", "aBÉ");
        let not_unicode_folded = Str/eql_ascii_ci("É", "é");
        Io/print(Str/flatten([
            rebuilt, "|", second, "|", Nat/to_str(euro), "|",
            Nat/to_str(supplementary), "|", shown, "|",
            /std/Bln/to_str(folded), "|", /std/Bln/to_str(not_unicode_folded), "|",
            Nat/to_str(Str/len(s)), "|", Str/slice(s, 1, 2)
        ]))
        "#;

    assert_eq!(run(source), "a€😀|€|1|2|😀|true|false|3|€".as_bytes());
}

#[test]
fn str_rejects_every_invalid_utf8_shape() {
    let source = r#"
        use /std/{Str, Bln, Lst, Bytes, Io};
        let rejected(bytes : Bytes) -> Bln =
            match Str/of_bytes(bytes)
            | some(_) => false
            | none() => true
            end;
        Io/print(Bln/to_str(Lst/fold([
            x\c0\af, x\e0\80\80, x\ed\a0\80, x\f4\90\80\80,
            x\80, x\c2, x\e2\82, x\f0\9f\98
        ], true, (bytes, ok) => ok && rejected(bytes))))
        "#;

    assert_eq!(run(source), b"true");
}

#[test]
fn json_unicode_escapes_require_well_formed_surrogate_pairs() {
    let source = r#"
        use /std/{Json, Parse, Result, Str, Io};
        use /std/Json/{str};
        let decoded(input : Str) -> Str =
            match Parse/run(Json/decode, Str/to_bytes(input)) : Str
            | success(value) =>
                match value : Str
                | str(s) => s
                | _ => "wrong"
                end
            | failure(_) => "rejected"
            end;
        Io/print(Str/join("|", [
            decoded("\"\\uD83D\\uDE00\""),
            decoded("\"\\uD83D\""),
            decoded("\"\\uD83D\\u0041\""),
            decoded("\"\\uDE00\"")
        ]))
        "#;

    assert_eq!(run(source), "😀|rejected|rejected|rejected".as_bytes());
}

#[test]
fn character_literals_do_not_coerce_to_numeric_domains() {
    for source in [
        "use /std/{Nat}; let n : Nat = 'a'; n",
        "use /std/{Byte}; let b : Byte = 'a'; b",
        "use /std/{Char, Nat}; let c : Char = 'a'; c == 97",
        "use /std/{Char, Byte}; let c : Char = 'a'; c == (0x61 : Byte)",
    ] {
        let (system, _io) = MockHost::builder().build();
        assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
    }
}

// A *non-productive* inner `rec` forced in a type position must degrade to the
// reduce deadline (an error), never hang or panic — the regression guard for
// inner-`rec` reduction at the type level (a `Subterm::Rec` demanded by an
// eliminator is now forced, not left stuck).
#[test]
fn utf8_inductive_spike() {
    // DE-RISKING PROBE (Str migration): a state-indexed inductive relation over a
    // native `Bytes` index, with `cons(c, t)` encoded as `concat(append(x\, c), t)`.
    // The point is `seq` (the concatenation lemma underlying `concat_closed`): a
    // 2-case induction on the derivation whose arms close ONLY if the native-Bytes
    // free-monoid laws hold *definitionally* — `concat(x\, b) ≡ b` (stop arm) and
    // `concat(concat(single c, t), b) ≡ concat(single c, concat(t, b))` (more arm).
    // If this typechecks, the inductive-`IsUtf8` approach is viable and the
    // cons-index inversion limit does not bite the proof path.
    let source = r#"
        use /std/{Io, Str, Nat, Bytes};

        induct Scan : Type
        | lead()
        | cont()
        | bad()
        end

        let step(c : Nat, s : Scan) -> Scan =
            match s
            | lead() => match Nat/lt(c, 128) | true => Scan/lead() | false => Scan/cont() end
            | cont() => Scan/lead()
            | bad() => Scan/bad()
            end;

        induct Utf8 : (s : Scan, b : Bytes) -> Type
        | stop() : (Scan/lead(), x\)
        | more(c : Nat, st : Scan, t : Bytes, rest : Utf8(step(c, st), t))
            : (st, Bytes/concat(Bytes/append(x\, Nat/to_byte(c)), t))
        end

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Utf8(Scan/lead(), b))
            -> Utf8(s, Bytes/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bytes/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bytes/concat(t, b), seq(rest, vb))
            end;

        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_construction_spike() {
    // DE-RISKING PROBE 2 (Str migration), the CONSTRUCTING side: the `of_bin`
    // checker must BUILD a derivation whose index matches the input `Bytes`, by
    // native-`Bytes` recursion. That needs the native eliminator's motive to be
    // DEPENDENT — refining `b` to `cons(h, t)` in the cons arm so the arm can
    // return `P(cons h t)` from `ih : P(t)`. Probe with the trivial all-accepting
    // relation `All` built by induction on `b`. If this typechecks, the checker is
    // expressible (real decision-procedure work, but no missing primitive).
    let source = r#"
        use /std/{Io, Str, Nat, Bytes};

        induct All : (b : Bytes) -> Type
        | empty() : (x\)
        | snoc(c : Nat, t : Bytes, rest : All(t)) : (Bytes/concat(Bytes/append(x\, Nat/to_byte(c)), t))
        end

        rec build(b : Bytes) -> All(b) =
            match b : (b) => All(b)
            | x\ => All/empty()
            | x\h\..t; ih => All/snoc(/std/Byte/to_nat(h), t, ih)
            end;

        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_concat_closed_holds_for_the_real_automaton() {
    // INCREMENT A of the Str migration: `concat_closed` against the ACTUAL UTF-8
    // `Scan`/`classify`/`step` automaton from std/Str.crs (not the spike's stub).
    // The spike showed `seq` is step-agnostic — it threads `step(c, s)` without
    // inspecting it — so swapping in the real, range-checking automaton changes
    // nothing in the proof: both arms still close by the definitional free-monoid
    // laws (`concat(x\, b) ≡ b`; associativity). This is the lemma that earns the
    // proof-carrying newtype: `Valid(a) -> Valid(b) -> Valid(concat a b)`.
    let source = r#"
        use /std/{Io, Str, Nat, Byte, Bytes, Bln};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        induct Scan : Type
        | lead()
        | cont(Nat, Nat, Nat)
        | bad()
        end

        let classify(c : Nat) -> Scan =
            match in_range(c, 0, 127)
            | true => Scan/lead()
            | false =>
                match in_range(c, 194, 223)
                | true => Scan/cont(1, 128, 191)
                | false =>
                    match in_range(c, 224, 239)
                    | true =>
                        let lo = match Nat/eql(c, 224) | true => 160 | false => 128 end;
                        let hi = match Nat/eql(c, 237) | true => 159 | false => 191 end;
                        Scan/cont(2, lo, hi)
                    | false =>
                        match in_range(c, 240, 244)
                        | true =>
                            let lo = match Nat/eql(c, 240) | true => 144 | false => 128 end;
                            let hi = match Nat/eql(c, 244) | true => 143 | false => 191 end;
                            Scan/cont(3, lo, hi)
                        | false => Scan/bad()
                        end
                    end
                end
            end;

        let step(c : Nat, s : Scan) -> Scan =
            match s
            | bad() => Scan/bad()
            | cont(rem, lo, hi) =>
                match in_range(c, lo, hi)
                | false => Scan/bad()
                | true =>
                    match Nat/eql(rem, 1)
                    | true => Scan/lead()
                    | false => Scan/cont(Nat/sub(rem, 1), 128, 191)
                    end
                end
            | lead() => classify(c)
            end;

        induct Utf8 : (s : Scan, b : Bytes) -> Type
        | stop() : (Scan/lead(), x\)
        | more(c : Byte, st : Scan, t : Bytes, rest : Utf8(step(Byte/to_nat(c), st), t))
            : (st, Bytes/concat(Bytes/append(x\, c), t))
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, Bytes/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bytes/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bytes/concat(t, b), seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(Bytes/concat(a, b)) =
            seq(va, vb);

        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_of_bin_checker_decides_and_builds_derivations() {
    // INCREMENT B of the Str migration: the `of_bin` decision procedure. It must
    // both DECIDE validity at runtime and BUILD a real `Utf8` derivation in the
    // `some` case. The native `Bytes` eliminator is a fold (its `ih` is the
    // fold-of-tail with fixed parameters), but the checker threads a changing
    // `Scan` state — so we fold `b` into a FUNCTION `(s) -> Option(Utf8(s, b))`
    // (foldl-as-foldr convoy), letting each step receive its state from the caller.
    // `of_bin_valid(b) = check(b)(lead)`. The runtime `decide` proves the automaton
    // actually runs: "hi" (ASCII) is accepted, a lone `x\80` continuation byte is
    // rejected — output "yesno".
    let source = r#"
        use /std/{Io, Str, Nat, Bytes, Bln, Option};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        induct Scan : Type
        | lead()
        | cont(Nat, Nat, Nat)
        | bad()
        end

        let classify(c : Nat) -> Scan =
            match in_range(c, 0, 127)
            | true => Scan/lead()
            | false =>
                match in_range(c, 194, 223)
                | true => Scan/cont(1, 128, 191)
                | false =>
                    match in_range(c, 224, 239)
                    | true =>
                        let lo = match Nat/eql(c, 224) | true => 160 | false => 128 end;
                        let hi = match Nat/eql(c, 237) | true => 159 | false => 191 end;
                        Scan/cont(2, lo, hi)
                    | false =>
                        match in_range(c, 240, 244)
                        | true =>
                            let lo = match Nat/eql(c, 240) | true => 144 | false => 128 end;
                            let hi = match Nat/eql(c, 244) | true => 143 | false => 191 end;
                            Scan/cont(3, lo, hi)
                        | false => Scan/bad()
                        end
                    end
                end
            end;

        let step(c : Nat, s : Scan) -> Scan =
            match s
            | bad() => Scan/bad()
            | cont(rem, lo, hi) =>
                match in_range(c, lo, hi)
                | false => Scan/bad()
                | true =>
                    match Nat/eql(rem, 1)
                    | true => Scan/lead()
                    | false => Scan/cont(Nat/sub(rem, 1), 128, 191)
                    end
                end
            | lead() => classify(c)
            end;

        induct Utf8 : (s : Scan, b : Bytes) -> Type
        | stop() : (Scan/lead(), x\)
        | more(c : Nat, st : Scan, t : Bytes, rest : Utf8(step(c, st), t))
            : (st, Bytes/concat(Bytes/append(x\, Nat/to_byte(c)), t))
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        let check(b : Bytes) -> ((s : Scan) -> Option(Utf8(s, b))) =
            match b : (b) => (s : Scan) -> Option(Utf8(s, b))
            | x\ => (s) =>
                match s : (s) => Option(Utf8(s, x\))
                | lead() => Option/some(Utf8/stop())
                | cont(rem, lo, hi) => Option/none()
                | bad() => Option/none()
                end
            | x\h\..t; ih => (s) =>
                match ih(step(/std/Byte/to_nat(h), s)) : Option(Utf8(s, Bytes/concat(Bytes/append(x\, h), t)))
                | some(rest) => Option/some(Utf8/more(/std/Byte/to_nat(h), s, t, rest))
                | none() => Option/none()
                end
            end;

        let of_bin_valid(b : Bytes) -> Option(Valid(b)) =
            check(b)(Scan/lead());

        let decide(b : Bytes) -> Bytes =
            match of_bin_valid(b)
            | some(_) => Str/to_bytes("yes")
            | none() => Str/to_bytes("no")
            end;

        Io/write(Io/stdout, Bytes/concat(decide(x\68\69), decide(x\80)))
        "#;
    assert_eq!(run(source), b"yesno");
}

#[test]
fn utf8_decimal_is_ascii_carries_its_proof() {
    // INCREMENT C of the Str migration: producers (`Nat/to_str`) must yield a
    // `Valid` Bytes without a bridge. The trick that avoids ALL Nat-comparison
    // arithmetic: `digit` emits each decimal digit as a CONCRETE byte literal per
    // branch, so `step(byte, lead)` *reduces* to `lead` and the per-digit proof is
    // just `refl`. `single` wraps one ASCII byte into a `Valid` via `subst` over
    // that proof; `decimal` recurses and combines the high digits with the low one
    // through the already-proven `concat_closed`. The result type — `decimal` returns
    // a dependent pair `{ b : Bytes, v : Valid(b) }` — IS `decimal_is_ascii`. Runtime
    // check: `decimal(255).b` renders "255", proving the bytes are real digits.
    let source = r#"
        use /std/{Io, Str, Nat, Bytes, Bln, Eq};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        induct Scan : Type
        | lead()
        | cont(Nat, Nat, Nat)
        | bad()
        end

        let classify(c : Nat) -> Scan =
            match in_range(c, 0, 127)
            | true => Scan/lead()
            | false =>
                match in_range(c, 194, 223)
                | true => Scan/cont(1, 128, 191)
                | false =>
                    match in_range(c, 224, 239)
                    | true =>
                        let lo = match Nat/eql(c, 224) | true => 160 | false => 128 end;
                        let hi = match Nat/eql(c, 237) | true => 159 | false => 191 end;
                        Scan/cont(2, lo, hi)
                    | false =>
                        match in_range(c, 240, 244)
                        | true =>
                            let lo = match Nat/eql(c, 240) | true => 144 | false => 128 end;
                            let hi = match Nat/eql(c, 244) | true => 143 | false => 191 end;
                            Scan/cont(3, lo, hi)
                        | false => Scan/bad()
                        end
                    end
                end
            end;

        let step(c : Nat, s : Scan) -> Scan =
            match s
            | bad() => Scan/bad()
            | cont(rem, lo, hi) =>
                match in_range(c, lo, hi)
                | false => Scan/bad()
                | true =>
                    match Nat/eql(rem, 1)
                    | true => Scan/lead()
                    | false => Scan/cont(Nat/sub(rem, 1), 128, 191)
                    end
                end
            | lead() => classify(c)
            end;

        induct Utf8 : (s : Scan, b : Bytes) -> Type
        | stop() : (Scan/lead(), x\)
        | more(c : Nat, st : Scan, t : Bytes, rest : Utf8(step(c, st), t))
            : (st, Bytes/concat(Bytes/append(x\, Nat/to_byte(c)), t))
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, Bytes/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bytes/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bytes/concat(t, b), seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(Bytes/concat(a, b)) =
            seq(va, vb);

        let single(c : Nat, ok : Eq(step(c, Scan/lead()), Scan/lead()))
            -> Valid(Bytes/append(x\, Nat/to_byte(c))) =
            let r : Utf8(step(c, Scan/lead()), x\) =
                Eq/subst((sc) => Utf8(sc, x\), Eq/sym(ok), Utf8/stop());
            Utf8/more(c, Scan/lead(), x\, r);

        let digit(d : Nat) -> { c : Nat, ok : Eq(step(c, Scan/lead()), Scan/lead()) } =
            match Nat/eql(d, 0) | true => (48, Eq/refl()) | false =>
            match Nat/eql(d, 1) | true => (49, Eq/refl()) | false =>
            match Nat/eql(d, 2) | true => (50, Eq/refl()) | false =>
            match Nat/eql(d, 3) | true => (51, Eq/refl()) | false =>
            match Nat/eql(d, 4) | true => (52, Eq/refl()) | false =>
            match Nat/eql(d, 5) | true => (53, Eq/refl()) | false =>
            match Nat/eql(d, 6) | true => (54, Eq/refl()) | false =>
            match Nat/eql(d, 7) | true => (55, Eq/refl()) | false =>
            match Nat/eql(d, 8) | true => (56, Eq/refl()) | false =>
            match Nat/eql(d, 9) | true => (57, Eq/refl()) | false =>
            (48, Eq/refl())
            end end end end end end end end end end;

        let single_digit(d : Nat) -> { b : Bytes, v : Valid(b) } =
            let g = digit(d);
            (Bytes/append(x\, Nat/to_byte(g.c)), single(g.c, g.ok));

        rec decimal(n : Nat) -> { b : Bytes, v : Valid(b) } =
            match Nat/lt(n, 10)
            | true => single_digit(n)
            | false =>
                let hi = decimal(Nat/div(n, 10));
                let lo = single_digit(Nat/rem(n, 10));
                (Bytes/concat(hi.b, lo.b), concat_closed(@hi.b, @lo.b, hi.v, lo.v))
            end;

        let decimal_is_ascii(n : Nat) -> Valid(decimal(n).b) =
            decimal(n).v;

        Io/write(Io/stdout, decimal(255).b)
        "#;
    assert_eq!(run(source), b"255");
}

#[test]
fn utf8_slice_closed_peels_codepoints() {
    // INCREMENT (slice_closed), the hard tail: prove codepoint slicing preserves
    // validity WITHOUT byte-offset reasoning. Walk the derivation, peeling one
    // codepoint at a time (a `more`-run from `lead` back to `lead`). The core lemma
    // `take_to_lead` walks from any state to the next `lead` boundary, returning the
    // consumed codepoint-fragment (with its derivation `midd : Utf8(s, mid)`) and the
    // valid remainder `tv : Valid(tail)`. `take1`/`drop1` then split the first
    // codepoint; iterating them (mechanical) gives `slice`, reassembled via
    // `concat_closed`. The `bad` state never reaches `lead`, but the arm still
    // elaborates for a general index (its `stop` prunes; it's just never hit at
    // runtime on valid input).
    let source = r#"
        use /std/{Io, Str, Nat, Byte, Bytes, Bln};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        induct Scan : Type
        | lead()
        | cont(Nat, Nat, Nat)
        | bad()
        end

        let classify(c : Nat) -> Scan =
            match in_range(c, 0, 127)
            | true => Scan/lead()
            | false =>
                match in_range(c, 194, 223)
                | true => Scan/cont(1, 128, 191)
                | false =>
                    match in_range(c, 224, 239)
                    | true => Scan/cont(2, 128, 191)
                    | false =>
                        match in_range(c, 240, 244)
                        | true => Scan/cont(3, 128, 191)
                        | false => Scan/bad()
                        end
                    end
                end
            end;

        let step(c : Nat, s : Scan) -> Scan =
            match s
            | bad() => Scan/bad()
            | cont(rem, lo, hi) =>
                match in_range(c, lo, hi)
                | false => Scan/bad()
                | true =>
                    match Nat/eql(rem, 1)
                    | true => Scan/lead()
                    | false => Scan/cont(Nat/sub(rem, 1), 128, 191)
                    end
                end
            | lead() => classify(c)
            end;

        induct Utf8 : (s : Scan, b : Bytes) -> Type
        | stop() : (Scan/lead(), x\)
        | more(c : Byte, st : Scan, t : Bytes, rest : Utf8(step(Byte/to_nat(c), st), t))
            : (st, Bytes/concat(Bytes/append(x\, c), t))
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, Bytes/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bytes/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bytes/concat(t, b), seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(Bytes/concat(a, b)) =
            seq(va, vb);

        rec take_to_lead(@s : Scan, @b : Bytes, d : Utf8(s, b))
            -> { mid : Bytes, tail : Bytes, midd : Utf8(s, mid), tv : Valid(tail) } =
            let go =
                match s : (s) => (p : Utf8(s, b))
                    -> { mid : Bytes, tail : Bytes, midd : Utf8(s, mid), tv : Valid(tail) }
                | lead() => (d) => (x\, b, Utf8/stop(), d)
                | cont(rem, lo, hi) => (d) =>
                    match d : { mid : Bytes, tail : Bytes, midd : Utf8(Scan/cont(rem, lo, hi), mid), tv : Valid(tail) }
                    | more(c, st, t, rest) =>
                        let w = take_to_lead(rest);
                        (Bytes/concat(Bytes/append(x\, c), w.mid), w.tail,
                         Utf8/more(c, st, w.mid, w.midd), w.tv)
                    end
                | bad() => (d) =>
                    match d : { mid : Bytes, tail : Bytes, midd : Utf8(Scan/bad(), mid), tv : Valid(tail) }
                    | more(c, st, t, rest) =>
                        let w = take_to_lead(rest);
                        (Bytes/concat(Bytes/append(x\, c), w.mid), w.tail,
                         Utf8/more(c, st, w.mid, w.midd), w.tv)
                    end
                end;
            go(d);

        let take1(@b : Bytes, d : Valid(b)) -> { cp : Bytes, v : Valid(cp) } =
            match d : { cp : Bytes, v : Valid(cp) }
            | stop() => (x\, Utf8/stop())
            | more(c, st, t, rest) =>
                let w = take_to_lead(rest);
                (Bytes/concat(Bytes/append(x\, c), w.mid), Utf8/more(c, st, w.mid, w.midd))
            end;

        let drop1(@b : Bytes, d : Valid(b)) -> { rest : Bytes, v : Valid(rest) } =
            match d : { rest : Bytes, v : Valid(rest) }
            | stop() => (x\, Utf8/stop())
            | more(c, st, t, rest) =>
                let w = take_to_lead(rest);
                (w.tail, w.tv)
            end;

        rec drop_n(n : Nat, @b : Bytes, d : Valid(b)) -> { r : Bytes, v : Valid(r) } =
            match Nat/eql(n, 0)
            | true => (b, d)
            | false =>
                let w = drop1(d);
                drop_n(Nat/sub(n, 1), @w.rest, w.v)
            end;

        rec take_n(n : Nat, @b : Bytes, d : Valid(b)) -> { r : Bytes, v : Valid(r) } =
            match Nat/eql(n, 0)
            | true => (x\, Utf8/stop())
            | false =>
                let hd = take1(d);
                let tl = drop1(d);
                let tn = take_n(Nat/sub(n, 1), @tl.rest, tl.v);
                (Bytes/concat(hd.cp, tn.r), concat_closed(@hd.cp, @tn.r, hd.v, tn.v))
            end;

        let slice(@b : Bytes, d : Valid(b), x : Nat, y : Nat) -> { r : Bytes, v : Valid(r) } =
            let dropped = drop_n(x, d);
            take_n(Nat/sub(y, x), @dropped.r, dropped.v);

        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}
