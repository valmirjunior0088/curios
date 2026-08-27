//! UTF-8 validity, decoding, and the automaton that certifies a slice.

use super::super::run;

#[test]
fn slice_proof_aligns_with_byte_walk() {
    // The corrected `slice_closed` shape: a RELEVANT byte walk (`to_lead_bytes`) and a MIRRORING proof walk (`to_lead_proof : Valid(to_lead_bytes(s, b))`). The proof peels the derivation while the byte function reduces in lockstep — which only works now that the `Bytes` eliminator decodes a *symbolic* cons (the new reduce rule). The `cont`/`bad` arms reduce `to_lead_bytes(cont, cons(c,t))` to `to_lead_bytes(step(c,cont), t)`, matching the recursive proof's index.
    let source = r#"
        use /std/{Handle, Byte, Bytes, Nat, Bool};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bool =
            match Nat/ge(c, lo)
            | true => Nat/le(c, hi)
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
        | stop() : (Scan/lead(), x[])
        | more(c : Byte, st : Scan, t : Bytes, rest : Utf8(step(Byte/to_nat(c), st), t))
            : (st, x[c, ..t])
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec to_lead_bytes(s : Scan, b : Bytes) -> Bytes =
            match s
            | lead() => b
            | cont(rem, lo, hi) =>
                match b
                | x[] => x[]
                | x[h, ..t]; ih => to_lead_bytes(step(/std/Byte/to_nat(h), Scan/cont(rem, lo, hi)), t)
                end
            | bad() =>
                match b
                | x[] => x[]
                | x[h, ..t]; ih => to_lead_bytes(step(/std/Byte/to_nat(h), Scan/bad()), t)
                end
            end;

        rec to_lead_proof(s : Scan, b : Bytes, d : Utf8(s, b)) -> Valid(to_lead_bytes(s, b)) =
            let go =
                match s : (s) => (p : Utf8(s, b)) -> Valid(to_lead_bytes(s, b))
                | lead() => (p) => p
                | cont(rem, lo, hi) => (p) =>
                    match p : (q, x, w) => Valid(to_lead_bytes(q, x))
                    | more(c, st, t, rest) => to_lead_proof(step(Byte/to_nat(c), st), t, rest)
                    end
                | bad() => (p) =>
                    match p : (q, x, w) => Valid(to_lead_bytes(q, x))
                    | more(c, st, t, rest) => to_lead_proof(step(Byte/to_nat(c), st), t, rest)
                    end
                end;
            go(d);
        let _ = Handle/write(Handle/stdout, x[0x6F, 0x6B])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn str_of_bytes_accepts_multibyte_utf8() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0xc3, 0xa9]) : (_) => /std/Io({})
        | some(s) => /std/print(s)
        | none() => /std/print("bad")
        end
        "#;

    assert_eq!(run(source), [0xc3, 0xa9]);
}

// An invalid lead byte fails `is_utf8`, so `Str/of_bytes` returns `none`.
#[test]
fn str_of_bytes_rejects_invalid_utf8() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0xff]) : (_) => /std/Io({})
        | some(s) => /std/print(s)
        | none() => /std/print("rejected")
        end
        "#;

    assert_eq!(run(source), b"rejected");
}

// A truncated multi-byte sequence (a 2-byte lead with no continuation) fails the continuation-byte check, so `of_bin` returns `none`.
#[test]
fn str_of_bytes_rejects_truncated_multibyte() {
    let source = r#"
        use /std/{Str, Handle};
        match Str/of_bytes(x[0xc3]) : (_) => /std/Io({})
        | some(s) => /std/print(s)
        | none() => /std/print("rejected")
        end
        "#;

    assert_eq!(run(source), b"rejected");
}

// The UTF-8 decode certification lemmas: naming them forces their bodies to elaborate (demand-driven checking). `cont_len` is the one that exercises the comparison intrinsic — `step` only reduces in `cont` state because `eql(succ(succ k''), 1)` now folds to `false`. `peel_byte`/`count_scalars`/ `decode_head` are the cursor-free decode core: `peel_byte` advances the (prop) validity witness one byte without ever large-eliminating it, `count_scalars` is the codepoint count `len` is built on, and `decode_head` reads the head codepoint from the relevant bytes under that witness.
#[test]
fn decode_lemmas_type_check() {
    let source = r#"
        use /std/{Str, Nat, Handle};
        let lemmas = (Str/utf8/bad_uninhabited, Str/utf8/cont_len, Str/utf8/peel_byte,
            Nat/Le/Ind/trans, Nat/Lt/of_ind_succ, Nat/Le/Ind/add_mono_l, Str/utf8/count_scalars, Str/utf8/cont0_uninhabited, Str/utf8/take_continuations, Str/utf8/decode_head);
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

#[test]
fn char_of_nat_accepts_exact_unicode_scalar_boundaries() {
    let source = r#"
        use /std/{Char, Nat, Str, Option, List, Handle};
        let render(n : Nat) -> Str =
            match Char/of_nat(n)
            | some(c) => Nat/to_str(Char/to_nat(c))
            | none() => "x"
            end;
        /std/print(Str/join(",", List/map(
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
        use /std/{Char, Nat, Bytes, Option, List, Handle};
        let encode(n : Nat) -> Bytes =
            Char/to_utf8(Option/unwrap_or(Char/of_nat(n), '?'));
        let _ = Handle/write(Handle/stdout, Bytes/flatten(List/map(
            [0, 0x7F, 0x80, 0x3BB, 0x7FF, 0x800, 0xD7FF, 0xE000, 0xFFFF,
             0x10000, 0x1F600, 0x10FFFF], encode)))!;
        /std/Io/pure(())
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
fn str_rejects_every_invalid_utf8_shape() {
    let source = r#"
        use /std/{Str, Bool, List, Bytes, Handle};
        let rejected(bytes : Bytes) -> Bool =
            match Str/of_bytes(bytes)
            | some(_) => false
            | none() => true
            end;
        /std/print(Bool/to_str(List/fold([
            x[0xc0, 0xaf], x[0xe0, 0x80, 0x80], x[0xed, 0xa0, 0x80], x[0xf4, 0x90, 0x80, 0x80],
            x[0x80], x[0xc2], x[0xe2, 0x82], x[0xf0, 0x9f, 0x98]
        ], true, (bytes, ok) => ok && rejected(bytes))))
        "#;

    assert_eq!(run(source), b"true");
}

// A *non-productive* inner `rec` forced in a type position must degrade to the reduce budget (an error), never hang or panic — the regression guard for inner-`rec` reduction at the type level (a `Subterm::Rec` demanded by an eliminator is now forced, not left stuck).
#[test]
fn utf8_inductive_spike() {
    // DE-RISKING PROBE (Str migration): a state-indexed inductive relation over a native `Bytes` index, with `cons(c, t)` encoded as `concat(append(x[], c), t)`. The point is `seq` (the concatenation lemma underlying `concat_closed`): a 2-case induction on the derivation whose arms close ONLY if the native-Bytes free-monoid laws hold *definitionally* — `concat(x[], b) ≡ b` (stop arm) and `concat(concat(single c, t), b) ≡ concat(single c, concat(t, b))` (more arm). If this typechecks, the inductive-`IsUtf8` approach is viable and the cons-index inversion limit does not bite the proof path.
    let source = r#"
        use /std/{Handle, Str, Nat, Bytes};

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
        | stop() : (Scan/lead(), x[])
        | more(c : Nat, st : Scan, t : Bytes, rest : Utf8(step(c, st), t))
            : (st, x[Nat/to_byte(c), ..t])
        end

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Utf8(Scan/lead(), b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_construction_spike() {
    // DE-RISKING PROBE 2 (Str migration), the CONSTRUCTING side: the `of_bin` checker must BUILD a derivation whose index matches the input `Bytes`, by native-`Bytes` recursion. That needs the native eliminator's motive to be DEPENDENT — refining `b` to `cons(h, t)` in the cons arm so the arm can return `P(cons h t)` from `ih : P(t)`. Probe with the trivial all-accepting relation `All` built by induction on `b`. If this typechecks, the checker is expressible (real decision-procedure work, but no missing intrinsic).
    let source = r#"
        use /std/{Handle, Str, Nat, Bytes};

        induct All : (b : Bytes) -> Type
        | empty() : (x[])
        | snoc(c : Nat, t : Bytes, rest : All(t)) : (x[Nat/to_byte(c), ..t])
        end

        rec build(b : Bytes) -> All(b) =
            match b : (b) => All(b)
            | x[] => All/empty()
            | x[h, ..t]; ih => All/snoc(/std/Byte/to_nat(h), t, ih)
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn concat_closed_holds_for_the_real_automaton() {
    // INCREMENT A of the Str migration: `concat_closed` against the ACTUAL UTF-8 `Scan`/`classify`/`step` automaton from std/Str.crs (not the spike's stub). The spike showed `seq` is step-agnostic — it threads `step(c, s)` without inspecting it — so swapping in the real, range-checking automaton changes nothing in the proof: both arms still close by the definitional free-monoid laws (`concat(x[], b) ≡ b`; associativity). This is the lemma that earns the proof-carrying newtype: `Valid(a) -> Valid(b) -> Valid(concat a b)`.
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes, Bool};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bool =
            match Nat/ge(c, lo)
            | true => Nat/le(c, hi)
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
        | stop() : (Scan/lead(), x[])
        | more(c : Byte, st : Scan, t : Bytes, rest : Utf8(step(Byte/to_nat(c), st), t))
            : (st, x[c, ..t])
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(x[..a, ..b]) =
            seq(va, vb);
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_of_bin_checker_decides_and_builds_derivations() {
    // INCREMENT B of the Str migration: the `of_bin` decision procedure. It must both DECIDE validity at runtime and BUILD a real `Utf8` derivation in the `some` case. The native `Bytes` eliminator is a fold (its `ih` is the fold-of-tail with fixed parameters), but the checker threads a changing `Scan` state — so we fold `b` into a FUNCTION `(s) -> Option(Utf8(s, b))` (foldl-as-foldr convoy), letting each step receive its state from the caller. `of_bin_valid(b) = check(b)(lead)`. The runtime `decide` proves the automaton actually runs: "hi" (ASCII) is accepted, a lone `x[0x80]` continuation byte is rejected — output "yesno".
    let source = r#"
        use /std/{Handle, Str, Nat, Bytes, Bool, Option};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bool =
            match Nat/ge(c, lo)
            | true => Nat/le(c, hi)
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
        | stop() : (Scan/lead(), x[])
        | more(c : Nat, st : Scan, t : Bytes, rest : Utf8(step(c, st), t))
            : (st, x[Nat/to_byte(c), ..t])
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        let check(b : Bytes) -> ((s : Scan) -> Option(Utf8(s, b))) =
            match b : (b) => (s : Scan) -> Option(Utf8(s, b))
            | x[] => (s) =>
                match s : (s) => Option(Utf8(s, x[]))
                | lead() => Option/some(Utf8/stop())
                | cont(rem, lo, hi) => Option/none()
                | bad() => Option/none()
                end
            | x[h, ..t]; ih => (s) =>
                match ih(step(/std/Byte/to_nat(h), s)) : (_) => Option(Utf8(s, x[h, ..t]))
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
        let _ = Handle/write(Handle/stdout, x[..decide(x[0x68, 0x69]), ..decide(x[0x80])])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"yesno");
}

#[test]
fn decimal_is_ascii_carries_its_proof() {
    // INCREMENT C of the Str migration: producers (`Nat/to_str`) must yield a `Valid` Bytes without a bridge. The trick that avoids ALL Nat-comparison arithmetic: `digit` emits each decimal digit as a CONCRETE byte literal per branch, so `step(byte, lead)` *reduces* to `lead` and the per-digit proof is just `refl`. `single` wraps one ASCII byte into a `Valid` via `subst` over that proof; `decimal` recurses and combines the high digits with the low one through the already-proven `concat_closed`. The result type — `decimal` returns a dependent pair `{ b : Bytes, v : Valid(b) }` — IS `decimal_is_ascii`. Runtime check: `decimal(255).b` renders "255", proving the bytes are real digits.
    let source = r#"
        use /std/{Handle, Str, Nat, Bytes, Bool, Eq};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bool =
            match Nat/ge(c, lo)
            | true => Nat/le(c, hi)
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
        | stop() : (Scan/lead(), x[])
        | more(c : Nat, st : Scan, t : Bytes, rest : Utf8(step(c, st), t))
            : (st, x[Nat/to_byte(c), ..t])
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(x[..a, ..b]) =
            seq(va, vb);

        let single(c : Nat, ok : Eq(step(c, Scan/lead()), Scan/lead()))
            -> Valid(x[Nat/to_byte(c)]) =
            let r : Utf8(step(c, Scan/lead()), x[]) =
                Eq/subst((sc) => Utf8(sc, x[]), Eq/sym(ok), Utf8/stop());
            Utf8/more(c, Scan/lead(), x[], r);

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
            (x[Nat/to_byte(g.c)], single(g.c, g.ok));

        rec decimal(n : Nat) -> { b : Bytes, v : Valid(b) } =
            match Nat/lt(n, 10)
            | true => single_digit(n)
            | false =>
                let hi = decimal(Nat/div(n, 10));
                let lo = single_digit(Nat/rem(n, 10));
                (x[..hi.b, ..lo.b], concat_closed(@hi.b, @lo.b, hi.v, lo.v))
            end;

        let decimal_is_ascii(n : Nat) -> Valid(decimal(n).b) =
            decimal(n).v;
        let _ = Handle/write(Handle/stdout, decimal(255).b)!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"255");
}

#[test]
fn slice_closed_peels_codepoints() {
    // INCREMENT (slice_closed), the hard tail: prove codepoint slicing preserves validity WITHOUT byte-offset reasoning. Walk the derivation, peeling one codepoint at a time (a `more`-run from `lead` back to `lead`). The core lemma `take_to_lead` walks from any state to the next `lead` boundary, returning the consumed codepoint-fragment (with its derivation `midd : Utf8(s, mid)`) and the valid remainder `tv : Valid(tail)`. `take1`/`drop1` then split the first codepoint; iterating them (mechanical) gives `slice`, reassembled via `concat_closed`. The `bad` state never reaches `lead`, but the arm still elaborates for a general index (its `stop` prunes; it's just never hit at runtime on valid input).
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes, Bool};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bool =
            match Nat/ge(c, lo)
            | true => Nat/le(c, hi)
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
        | stop() : (Scan/lead(), x[])
        | more(c : Byte, st : Scan, t : Bytes, rest : Utf8(step(Byte/to_nat(c), st), t))
            : (st, x[c, ..t])
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(x[..a, ..b]) =
            seq(va, vb);

        rec take_to_lead(@s : Scan, @b : Bytes, d : Utf8(s, b))
            -> { mid : Bytes, tail : Bytes, midd : Utf8(s, mid), tv : Valid(tail) } =
            let go =
                match s : (s) => (p : Utf8(s, b))
                    -> { mid : Bytes, tail : Bytes, midd : Utf8(s, mid), tv : Valid(tail) }
                | lead() => (d) => (x[], b, Utf8/stop(), d)
                | cont(rem, lo, hi) => (d) =>
                    match d : (_, _, _) => { mid : Bytes, tail : Bytes, midd : Utf8(Scan/cont(rem, lo, hi), mid), tv : Valid(tail) }
                    | more(c, st, t, rest) =>
                        let w = take_to_lead(rest);
                        (x[c, ..w.mid], w.tail,
                         Utf8/more(c, st, w.mid, w.midd), w.tv)
                    end
                | bad() => (d) =>
                    match d : (_, _, _) => { mid : Bytes, tail : Bytes, midd : Utf8(Scan/bad(), mid), tv : Valid(tail) }
                    | more(c, st, t, rest) =>
                        let w = take_to_lead(rest);
                        (x[c, ..w.mid], w.tail,
                         Utf8/more(c, st, w.mid, w.midd), w.tv)
                    end
                end;
            go(d);

        let take1(@b : Bytes, d : Valid(b)) -> { cp : Bytes, v : Valid(cp) } =
            match d : (_, _, _) => { cp : Bytes, v : Valid(cp) }
            | stop() => (x[], Utf8/stop())
            | more(c, st, t, rest) =>
                let w = take_to_lead(rest);
                (x[c, ..w.mid], Utf8/more(c, st, w.mid, w.midd))
            end;

        let drop1(@b : Bytes, d : Valid(b)) -> { rest : Bytes, v : Valid(rest) } =
            match d : (_, _, _) => { rest : Bytes, v : Valid(rest) }
            | stop() => (x[], Utf8/stop())
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
            | true => (x[], Utf8/stop())
            | false =>
                let hd = take1(d);
                let tl = drop1(d);
                let tn = take_n(Nat/sub(n, 1), @tl.rest, tl.v);
                (x[..hd.cp, ..tn.r], concat_closed(@hd.cp, @tn.r, hd.v, tn.v))
            end;

        let slice(@b : Bytes, d : Valid(b), x : Nat, n : Nat) -> { r : Bytes, v : Valid(r) } =
            let dropped = drop_n(x, d);
            take_n(n, @dropped.r, dropped.v);
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}
