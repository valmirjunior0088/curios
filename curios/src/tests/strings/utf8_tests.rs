//! UTF-8 validity, decoding, and the automaton that certifies a slice.

use crate::tests::run;

#[test]
fn slice_proof_aligns_with_byte_walk() {
    // A relevant byte walk (`to_lead_bytes`) and a proof walk mirroring it (`to_lead_proof : Valid(to_lead_bytes(s, b))`), over a local inductive validity family. The proof peels the derivation while the byte function reduces in lockstep, which rests on the `Bytes` eliminator decoding a *symbolic* cons: the `cont`/`bad` arms reduce `to_lead_bytes(cont, x[c, ..t])` to `to_lead_bytes(step(c, cont), t)`, matching the recursive proof's index.
    let source = r#"
        use /std/{Byte, Bytes, Nat, Bool, Io};

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

        let to_lead_bytes(s : Scan, b : Bytes) -> Bytes =
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

        let to_lead_proof(s : Scan, b : Bytes, d : Utf8(s, b)) -> Valid(to_lead_bytes(s, b)) =
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
        let _ = Io/write(Io/stdout, x[0x6F, 0x6B])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

// The UTF-8 decode lemmas and the order lemmas they rest on. Their bodies are checked when the prelude is built; naming them here pins that each is still exported under its name. `cont_len` is the one that exercises the comparison intrinsic — `step` reduces in `cont` state only because `kpp + 1 + 1 == 1` folds to `false`. `count_scalars`/`decode_head` are the cursor-free decode core: `count_scalars` is the codepoint count `len` is built on, and `decode_head` reads the head codepoint from the relevant bytes under the decided validity, which crosses each byte by conversion alone.
#[test]
fn decode_lemmas_type_check() {
    let source = r#"
        use /std/{Str, Nat, Io};
        let lemmas = (Str/Valid/from_bad, Str/Valid/cont_len,
            Nat/Le/trans, Nat/Lt/lt_of_le_succ, Nat/Le/add_mono_l, Str/count_scalars, Str/take_continuations, Str/Valid/decode_head);
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

#[test]
fn char_to_utf8_matches_rust_across_widths_and_boundaries() {
    let scalars = [
        0x0, 0x7f, 0x80, 0x3bb, 0x7ff, 0x800, 0xd7ff, 0xe000, 0xffff, 0x10000, 0x1f600, 0x10ffff,
    ];
    let source = r#"
        use /std/{Char, Nat, Bytes, Option, List, Io};
        let encode(n : Nat) -> Bytes =
            Char/to_utf8(Option/unwrap_or(Char/of_nat(n), '?'));
        let _ = Io/write(Io/stdout, Bytes/flatten(List/map(
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
fn utf8_inductive_spike() {
    // A state-indexed inductive relation over a native `Bytes` index, whose cons target `x[c, ..t]` lowers to `concat(append(x[], c), t)`. `seq` is induction on the derivation, and its arms close only if the free-monoid laws hold *definitionally* — `concat(x[], b) ≡ b` in the `stop` arm and `concat(concat(single c, t), b) ≡ concat(single c, concat(t, b))` in the `more` arm — so this pins those laws, and that inverting a cons index does not block the proof. `step` is a stub; `concat_closed_holds_for_the_real_automaton` repeats the proof over a full automaton.
    let source = r#"
        use /std/{Str, Nat, Bytes, Io};

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
            : (st, x[Nat/to_byte(c % 256), ..t])
        end

        let seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Utf8(Scan/lead(), b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_construction_spike() {
    // The constructing side: a derivation whose index is the input `Bytes` is built by native `Bytes` recursion. That needs the eliminator's motive to be dependent — refining `b` to `x[h, ..t]` in the cons arm, so the arm can return `P(x[h, ..t])` from `ih : P(t)`. `All` accepts everything, which isolates that requirement from any decision procedure; `utf8_of_bytes_checker_decides_and_builds_derivations` is the one that decides.
    let source = r#"
        use /std/{Str, Nat, Bytes, Io};

        induct All : (b : Bytes) -> Type
        | empty() : (x[])
        | snoc(c : Nat, t : Bytes, rest : All(t)) : (x[Nat/to_byte(c % 256), ..t])
        end

        let build(b : Bytes) -> All(b) =
            match b : (b) => All(b)
            | x[] => All/empty()
            | x[h, ..t]; ih => All/snoc(/std/Byte/to_nat(h), t, ih)
            end;
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn concat_closed_holds_for_the_real_automaton() {
    // `seq` over a full, range-checking UTF-8 `Scan`/`classify`/`step` automaton, where `utf8_inductive_spike` uses a stub. `seq` threads `step(c, s)` without inspecting it, so the real automaton changes nothing in the proof: both arms still close by the definitional free-monoid laws (`concat(x[], b) ≡ b` and associativity). `concat_closed` is the result: `Valid(a) -> Valid(b) -> Valid(x[..a, ..b])`.
    let source = r#"
        use /std/{Str, Nat, Byte, Bytes, Bool, Io};

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

        let seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(x[..a, ..b]) =
            seq(va, vb);
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_of_bytes_checker_decides_and_builds_derivations() {
    // A decision procedure that both decides validity at run time and builds a `Utf8` derivation in the `some` case. The native `Bytes` eliminator is a fold (its `ih` is the fold of the tail at fixed parameters), but the checker threads a changing `Scan` state — so `b` is folded into a function `(s) -> Option(Utf8(s, b))`, a foldl-as-foldr convoy in which each step receives its state from the caller, and `of_bytes_valid(b)` is `check(b)(lead)`. `decide` shows the automaton runs: "hi" (ASCII) is accepted and a lone `x[0x80]` continuation byte is rejected, printing "yesno".
    let source = r#"
        use /std/{Str, Nat, Bytes, Bool, Option, Io};

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
            : (st, x[Nat/to_byte(c % 256), ..t])
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

        let of_bytes_valid(b : Bytes) -> Option(Valid(b)) =
            check(b)(Scan/lead());

        let decide(b : Bytes) -> Bytes =
            match of_bytes_valid(b)
            | some(_) => Str/to_bytes("yes")
            | none() => Str/to_bytes("no")
            end;
        let _ = Io/write(Io/stdout, x[..decide(x[0x68, 0x69]), ..decide(x[0x80])])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"yesno");
}

#[test]
fn decimal_is_ascii_carries_its_proof() {
    // A producer yields a `Valid` value with no bridge lemma and no `Nat` comparison arithmetic: `digit` emits each decimal digit as a concrete byte literal per branch, so `step(byte, lead)` *reduces* to `lead` and the per-digit proof is `refl`. `single` wraps one ASCII byte into a `Valid` by `subst` over that proof, and `decimal` recurses, combining the high digits with the low one through `concat_closed`. `decimal` returns the dependent pair `{ b : Bytes, v : Valid(b) }`, which is what `decimal_is_ascii` states. At run time `decimal(255).b` prints "255", so the bytes are real digits.
    let source = r#"
        use /std/{Str, Nat, Bytes, Bool, Eq, Io};

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
            : (st, x[Nat/to_byte(c % 256), ..t])
        end

        let Valid(b : Bytes) -> Type = Utf8(Scan/lead(), b);

        let seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(x[..a, ..b]) =
            seq(va, vb);

        let single(c : Nat, ok : Eq(step(c, Scan/lead()), Scan/lead()))
            -> Valid(x[Nat/to_byte(c % 256)]) =
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
            (x[Nat/to_byte(g.c % 256)], single(g.c, g.ok));

        let decimal(n : Nat) -> { b : Bytes, v : Valid(b) } =
            match Nat/lt(n, 10)
            | true => single_digit(n)
            | false =>
                let hi = decimal(Nat/div(n, 10));
                let lo = single_digit(Nat/rem(n, 10));
                (x[..hi.b, ..lo.b], concat_closed(@hi.b, @lo.b, hi.v, lo.v))
            end;

        let decimal_is_ascii(n : Nat) -> Valid(decimal(n).b) =
            decimal(n).v;
        let _ = Io/write(Io/stdout, decimal(255).b)!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"255");
}

#[test]
fn slice_closed_peels_codepoints() {
    // Codepoint slicing preserves validity without byte-offset reasoning: the derivation is walked one codepoint at a time, a `more`-run from `lead` back to `lead`. `take_to_lead` walks from any state to the next `lead` boundary, returning the consumed fragment with its derivation `midd : Utf8(s, mid)` and the valid remainder `tv : Valid(tail)`; `take1` and `drop1` split off the first codepoint, and `slice` iterates them, reassembling through `concat_closed`. The `bad` state never reaches `lead`, but its arm still elaborates at a general index: its `stop` is pruned, and valid input never reaches it.
    let source = r#"
        use /std/{Str, Nat, Byte, Bytes, Bool, Io};

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

        let seq(@s : Scan, @a : Bytes, @b : Bytes, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, x[..a, ..b]) =
            match va : (q, x, w) => Utf8(q, x[..x, ..b])
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, x[..t, ..b], seq(rest, vb))
            end;

        let concat_closed(@a : Bytes, @b : Bytes, va : Valid(a), vb : Valid(b))
            -> Valid(x[..a, ..b]) =
            seq(va, vb);

        let take_to_lead(@s : Scan, @b : Bytes, d : Utf8(s, b))
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

        let drop_n(n : Nat, @b : Bytes, d : Valid(b)) -> { r : Bytes, v : Valid(r) } =
            match Nat/eql(n, 0)
            | true => (b, d)
            | false =>
                let w = drop1(d);
                drop_n(Nat/sub(n, 1), @w.rest, w.v)
            end;

        let take_n(n : Nat, @b : Bytes, d : Valid(b)) -> { r : Bytes, v : Valid(r) } =
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
        let _ = Io/write(Io/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}
