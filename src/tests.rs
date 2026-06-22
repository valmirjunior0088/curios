use {
    super::MockHost,
    std::{path::Path, time::Duration},
};

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    io.output().to_vec()
}

#[test]
fn utf8_slice_proof_aligns_with_byte_walk() {
    // The corrected `slice_closed` shape: a RELEVANT byte walk (`to_lead_bytes`) and
    // a MIRRORING proof walk (`to_lead_proof : Valid(to_lead_bytes(s, b))`). The proof
    // peels the derivation while the byte function reduces in lockstep — which only
    // works now that the `Bin` eliminator decodes a *symbolic* cons (the new reduce
    // rule). The `cont`/`bad` arms reduce `to_lead_bytes(cont, cons(c,t))` to
    // `to_lead_bytes(step(c,cont), t)`, matching the recursive proof's index.
    let source = r#"
        use /std/{Io, Bin, Nat, Bln};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        union Scan
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

        union Utf8 : (s : Scan, b : Bin)
        | stop() : (Scan/lead(), \\)
        | more(c : Nat, st : Scan, t : Bin, rest : Utf8(step(c, st), t))
            : (st, Bin/concat(Bin/append(\\, c), t))
        end

        let Valid(b : Bin) -> Type = Utf8(Scan/lead(), b);

        rec to_lead_bytes(s : Scan, b : Bin) -> Bin =
            match s
            | lead() => b
            | cont(rem, lo, hi) =>
                match b
                | \\ => \\
                | (h, t), ih => to_lead_bytes(step(h, Scan/cont(rem, lo, hi)), t)
                end
            | bad() =>
                match b
                | \\ => \\
                | (h, t), ih => to_lead_bytes(step(h, Scan/bad()), t)
                end
            end;

        rec to_lead_proof(s : Scan, b : Bin, d : Utf8(s, b)) -> Valid(to_lead_bytes(s, b)) =
            let go =
                match s : (s) => (p : Utf8(s, b)) -> Valid(to_lead_bytes(s, b))
                | lead() => (p) => p
                | cont(rem, lo, hi) => (p) =>
                    match p : (w : Utf8(q, x)) => Valid(to_lead_bytes(q, x))
                    | more(c, st, t, rest) => to_lead_proof(step(c, st), t, rest)
                    end
                | bad() => (p) =>
                    match p : (w : Utf8(q, x)) => Valid(to_lead_bytes(q, x))
                    | more(c, st, t, rest) => to_lead_proof(step(c, st), t, rest)
                    end
                end;
            go(d);

        Io/write(Io/stdout, \6F\6B)
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn nullary_closure_survives_erasure_and_codegen() {
    // A nullary closure stored in a union field and called indirectly via a
    // `call_ref` — the erasure+codegen path that needed `clsr_arities`. Zero-arity
    // closures survive it, which is what lets the suspension/continuation thunks
    // drop their dummy unit argument (`() -> T` rather than `({}) -> T`). Output
    // proves the suspended effect fired on `force`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Str};
        union Susp(A : Type)
        | now(A)
        | later(() -> Susp(A))
        end
        rec force(@A : Type, s : Susp(A)) -> A =
            match s : A
            | now(a) => a
            | later(k) => force(k())
            end;
        let prog : Susp({}) =
            Susp/later(() =>
                let w = Io/write(Io/stdout, Str/to_bin("ok"));
                Susp/now(()));
        let r = force(prog);
        Io/write(Io/stdout, Str/to_bin("!"))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok!");
}

#[test]
fn end_to_end() {
    let source = r#"
        union Pair
        | left(std/Int)
        | right(std/Flt)
        end
        let pair : Pair = Pair/left(+42);
        let score : (_ : Pair) -> std/Int = (p) =>
            match p : std/Int
            | left(_) => +42
            | right(_) => +7
            end;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(score(pair))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+42"
    );
}

#[test]
fn arr_match_is_a_foldr() {
    // Native `Arr` induction (slice 1): the `| [] | (h, t), ih` eliminator,
    // erased by desugaring to `Nat`-induction on the length and reusing the loop.
    // `f(h, ih) = ih * 10 + h` is non-commutative, so the result distinguishes a
    // structural `foldr` (head is the *first* element, ih is the fold of the tail)
    // from a reversed walk: `[1,2,3,4]` folds to `4321`, not `1234`.
    let source = r#"
        use /std/{Io, Str, Nat, Arr};
        let xs : Arr(Nat) = Arr/cons(1, Arr/cons(2, Arr/cons(3, Arr/single(4))));
        let digits : Nat =
            match xs : Nat
            | [] => 0
            | (h, t), ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn bignat_add_carries_across_limbs() {
    // `add` propagates carry across base-10^4 limbs: 99_999_999 (= [9999, 9999])
    // plus 1 ripples a carry through both limbs and grows a new top limb,
    // yielding [0, 0, 1] = 100_000_000.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/add(BigNat/of_nat(99999999), BigNat/of_nat(1))))
        "#;
    assert_eq!(run(source), b"100000000");
}

#[test]
fn bignat_sub_borrows_and_trims() {
    // `sub` borrows across limbs and then trims the high zero limbs the borrow
    // leaves behind: 100_000_000 (= [0, 0, 1]) minus 1 borrows twice to [9999,
    // 9999, 0], which must trim back to the canonical [9999, 9999] = 99_999_999.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/sub(BigNat/of_nat(100000000), BigNat/of_nat(1))))
        "#;
    assert_eq!(run(source), b"99999999");
}

#[test]
fn bignat_mul_small_propagates_carry() {
    // `mul_small` carries a value that itself exceeds the base: 9999 * 99999 =
    // 999_890_001, whose final carry (99_989) must be split into multiple limbs
    // rather than dropped into one.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/mul_small(BigNat/of_nat(9999), 99999)))
        "#;
    assert_eq!(run(source), b"999890001");
}

#[test]
fn bignat_mul_pow2_builds_large_powers() {
    // `mul_pow2` doubles past every fixed-width integer: 2^40 = 1_099_511_627_776
    // far exceeds the 31-bit `Nat` carrier, so a correct result proves the value
    // is held across limbs, not in a single trapping `Nat`.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(BigNat/to_str(BigNat/mul_pow2(BigNat/of_nat(1), 40)))
        "#;
    assert_eq!(run(source), b"1099511627776");
}

#[test]
fn bignat_compare_orders_by_magnitude() {
    // `compare` decides by the most-significant limb first (recursing to the top
    // of the little-endian list), so two values differing only in the lowest limb
    // still order correctly: 12345678 < 12345679, equal to itself, and the
    // reverse is greater.
    let source = r#"
        use /std/{Io, Str, BigNat, Order};
        let show(o : Order) -> Str =
            match o : Str
            | lt() => "lt"
            | eq() => "eq"
            | gt() => "gt"
            end;
        let a = BigNat/of_nat(12345678);
        let b = BigNat/of_nat(12345679);
        Io/print(Str/concat(Str/concat(show(BigNat/compare(a, b)), show(BigNat/compare(a, a))), show(BigNat/compare(b, a))))
        "#;
    assert_eq!(run(source), b"lteqgt");
}

#[test]
fn bignat_zero_renders_and_roundtrips() {
    // The canonical zero is the empty limb list, which `to_str` renders as "0"
    // (not the empty string), and a value whose lowest limb is zero round-trips
    // through `of_nat`/`to_str` with the high limb un-padded and the rest padded.
    let source = r#"
        use /std/{Io, Str, BigNat};
        Io/print(Str/concat(Str/concat(BigNat/to_str(BigNat/zero), "/"), BigNat/to_str(BigNat/of_nat(70000))))
        "#;
    assert_eq!(run(source), b"0/70000");
}

#[test]
#[allow(clippy::approx_constant)] // "+3.14" is a parse-and-render test vector, not π
fn flt_to_str_matches_rust_shortest_format() {
    // Stage 2: `Flt/to_str` is a real Dragon4 shortest-float renderer (BigNat-backed),
    // matching `format!("{:+}", f32)` byte-for-byte — no longer the `of_bin` shim. The
    // result is assembled from `Str` literals + `Nat/to_str` digits via `Str/concat`, so
    // it carries the UTF-8 proof through `concat_closed` (closing the Stage 3 gap too).
    // Expectations come straight from Rust's own `{:+}` so the test cannot drift from the
    // oracle the host renderer used to call.
    let cases: &[(&str, f32)] = &[
        ("+1.0", 1.0),
        ("Flt/neg(+1.0)", -1.0),
        ("+0.0", 0.0),
        ("Flt/neg(+0.0)", -0.0),
        ("+0.5", 0.5),
        ("+1.5", 1.5),
        ("+0.25", 0.25),
        ("+0.125", 0.125),
        ("+0.1", 0.1),
        ("+3.14", 3.14),
        ("+2.5", 2.5),
        ("+100.0", 100.0),
        ("+1234.5", 1234.5),
        ("+1000000.0", 1000000.0),
        ("+8388608.0", 8388608.0),
        ("+12345678.0", 12345678.0),
        ("+16777216.0", 16777216.0),
        ("+123456790000000.0", 123456790000000.0),
        ("Flt/div(+1.0, +1000000.0)", 1.0 / 1000000.0),
        ("Flt/div(+1.0, +8388608.0)", 1.0 / 8388608.0),
        ("Flt/div(+1.0, +0.0)", f32::INFINITY),
        ("Flt/div(Flt/neg(+1.0), +0.0)", f32::NEG_INFINITY),
        ("Flt/div(+0.0, +0.0)", f32::NAN),
    ];
    let array = cases
        .iter()
        .map(|(expr, _)| format!("Flt/to_str({expr})"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
        use /std/{{Io, Str, Flt, Arr}};
        Io/print(Str/join("|", [{array}]))
        "#
    );
    let expected = cases
        .iter()
        .map(|(_, value)| format!("{value:+}"))
        .collect::<Vec<_>>()
        .join("|");
    assert_eq!(run(&source), expected.into_bytes());
}

#[test]
fn arr_map_fills_every_slot() {
    // `Arr/map` erases to a single O(n) fill loop (`emit_map`): size the result
    // from `src.len`, allocate once, then write `f(src[i])` into slot `i` via an
    // inline closure `call_ref`. A non-identity `f` (`+1`) over `[10, 20, 30]` must
    // fill *every* slot, not just one: `get(_, 0) + get(_, 2)` = 11 + 31 = 42.
    let source = r#"
        use /std/{Io, Str, Nat, Arr, Option};
        let xs : Arr(Nat) = Arr/map((n) => Nat/add(n, 1), Arr/cons(10, Arr/cons(20, Arr/single(30))));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Option/unwrap_or(Arr/get(xs, 0), 0), Option/unwrap_or(Arr/get(xs, 2), 0)))))
        "#;
    assert_eq!(run(source), b"42");
}

#[test]
fn arr_map_distributes_over_cons() {
    // The eliminator rule that lets `Arr/map` stand in for a structural `foldr` in
    // proofs (`to_bins = Arr/map(to_bin)` feeding `/syn/Str/flatten`): for a
    // SYMBOLIC tail `t`, `map f (x :: t) ≡ f x :: map f t` *definitionally*. `refl`
    // checks only because `reduce` distributes the map over the `concat` spine and
    // maps the singleton literal — the same peel the native `Arr` eliminator does,
    // so it reduces under induction without unfolding a symbolic array.
    let source = r#"
        use /std/{Io, Str, Eq, Nat, Arr};
        let step(f : (Nat) -> Nat, x : Nat, t : Arr(Nat))
            -> Eq(Arr/map(f, Arr/concat(Arr/single(x), t)), Arr/concat(Arr/single(f(x)), Arr/map(f, t))) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_match_is_a_foldr() {
    // Native `Bin` induction (slice 2): the `| \\ | (h, t), ih` eliminator, erased
    // exactly like `Arr` — `Nat`-induction on the byte length, reusing the loop.
    // The leading byte `h` is reflected as a `Nat`. Same non-commutative `foldr`
    // probe as `arr_match_is_a_foldr`: the bytes `\01\02\03\04` fold to `4321`, not
    // `1234`, pinning head = first byte and ih = fold of the tail.
    let source = r#"
        use /std/{Io, Str, Nat, Bin};
        let bytes : Bin = \01\02\03\04;
        let digits : Nat =
            match bytes : Nat
            | \\ => 0
            | (h, t), ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn bin_concat_is_a_free_monoid() {
    // `peel_bin` (core::spine) makes `Bin` a free monoid up to *definitional*
    // equality: `concat` associates, the empty bytestring `\\` is its identity,
    // and a literal run re-segments freely — all provable by `refl` for SYMBOLIC
    // operands, which `reduce` cannot fold. Each binding's declared type forces
    // `convert` to peel the two `BinConcat`s to a common normal form; without the
    // peel these are stuck, distinct terms and `refl` would not check.
    let source = r#"
        use /std/{Io, Str, Eq, Bin};
        let assoc(a : Bin, b : Bin, c : Bin)
            -> Eq(Bin/concat(a, Bin/concat(b, c)), Bin/concat(Bin/concat(a, b), c)) =
            Eq/refl();
        let left_id(a : Bin) -> Eq(Bin/concat(\\, a), a) = Eq/refl();
        let right_id(a : Bin) -> Eq(Bin/concat(a, \\), a) = Eq/refl();
        let resegment(x : Bin)
            -> Eq(Bin/concat(\01\02, x), Bin/concat(\01, Bin/concat(\02, x))) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_concat_leading_byte_clash_is_rejected() {
    // The dual: a leading-byte disagreement under a shared symbolic tail is a
    // definite `Clash`, so `\01 ++ x` and `\02 ++ x` are never convertible and the
    // `refl` is rejected. Guards `peel_bin` against deciding unequal values equal.
    let source = r#"
        use /std/{Io, Str, Eq, Bin};
        let bad(x : Bin) -> Eq(Bin/concat(\01, x), Bin/concat(\02, x)) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn bin_slice_is_a_monoid_citizen() {
    // `Bin/slice` rides the free-monoid spine (`core::spine`) as a measured
    // `Window` — a length-`hi - lo` chunk whose contents are symbolic — so the
    // slice algebra holds up to *definitional* equality, provable by `refl` for
    // SYMBOLIC operands that `reduce` cannot fold. `split` fuses two adjacent
    // windows of one base across their shared seam; `empty` drops a zero-width
    // window (the monoid identity); `full` collapses `slice(b, 0, len b)` to its
    // base (the `reduce` partner of the spine's window-collapse). Each declared
    // type forces `convert` to peel the windows to a common normal form; without
    // the peel these are stuck, distinct terms and `refl` would not check.
    let source = r#"
        use /std/{Io, Str, Eq, Bin, Nat};
        let split(b : Bin, s : Nat, m : Nat, e : Nat)
            -> Eq(Bin/concat(Bin/slice(b, s, m), Bin/slice(b, m, e)), Bin/slice(b, s, e)) =
            Eq/refl();
        let empty(b : Bin, i : Nat) -> Eq(Bin/slice(b, i, i), \\) = Eq/refl();
        let full(b : Bin) -> Eq(Bin/slice(b, 0, Bin/len(b)), b) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_slice_window_seam_mismatch_is_rejected() {
    // The dual: two windows whose seam does not meet — `slice(b, s, m)` then
    // `slice(b, n, e)` with `m` and `n` distinct — must NOT fuse, so the concat is
    // not convertible to `slice(b, s, e)` and the `refl` is rejected. Guards the
    // fusion's seam check against gluing non-adjacent slices of one base.
    let source = r#"
        use /std/{Io, Str, Eq, Bin, Nat};
        let bad(b : Bin, s : Nat, m : Nat, n : Nat, e : Nat)
            -> Eq(Bin/concat(Bin/slice(b, s, m), Bin/slice(b, n, e)), Bin/slice(b, s, e)) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn bin_slice_reduces_across_a_cons_spine() {
    // Stage B foundation: `Bin/slice` reduces over a cons spine
    // (`concat(append(\\, h), t)`, the shape the `Utf8` relation builds) one byte
    // per `0`/`succ` boundary, and a nested slice reassociates. All provable by
    // `refl` for SYMBOLIC head/tail, which `reduce` peels via `peel_first_byte`
    // (`core::spine`). `take` keeps the head and recurses into the tail; `drop`
    // discards it and shifts both bounds; `nested` flattens `slice(slice(..))`.
    let source = r#"
        use /std/{Io, Str, Eq, Bin, Nat};
        let take(h : Nat, t : Bin)
            -> Eq(Bin/slice(Bin/concat(Bin/append(\\, h), t), 0, 2),
                  Bin/concat(Bin/append(\\, h), Bin/slice(t, 0, 1))) = Eq/refl();
        let drop(h : Nat, t : Bin)
            -> Eq(Bin/slice(Bin/concat(Bin/append(\\, h), t), 1, 3), Bin/slice(t, 0, 2)) =
            Eq/refl();
        let nested(b : Bin)
            -> Eq(Bin/slice(Bin/slice(b, 2, 10), 1, 3), Bin/slice(b, 3, 5)) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_len_reduces_across_a_cons_spine() {
    // The `Bin/len` partner of the slice/get cons-reduction: length distributes
    // over concatenation and an `append` is one byte longer, so a cons spine's
    // length reduces to a `succ` over the tail's — `len(cons(h, t)) = succ(len t)`.
    // `Nat/lt` then discharges the codepoint walk's bounds guard on that spine:
    // `lt(0, succ _) = true` (the left literal is below the successor floor) and
    // `lt(succ _, 0) = false` (the left is at least the floor). All by `refl` for
    // a SYMBOLIC tail, the pair that lets `advance_codepoint` step a symbolic cons.
    let source = r#"
        use /std/{Io, Str, Eq, Bin, Nat};
        let len(h : Nat, t : Bin)
            -> Eq(Bin/len(Bin/concat(Bin/append(\\, h), t)), Nat/add(1, Bin/len(t))) = Eq/refl();
        let guard(h : Nat, t : Bin)
            -> Eq(Nat/lt(0, Bin/len(Bin/concat(Bin/append(\\, h), t))), true) = Eq/refl();
        let floor(h : Nat, t : Bin)
            -> Eq(Nat/lt(Bin/len(Bin/concat(Bin/append(\\, h), t)), 0), false) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn nat_sub_peels_a_successor_spine() {
    // The subtraction twin of `NatAdd`'s successor peeling: `(s + inner) - k`
    // reduces to `(s - k) + inner` when the literal `k` is within the successor
    // floor `s`, even for a SYMBOLIC `inner` that `reduce` cannot fold. This is
    // what turns the `succ e - 1` bounds the cons slice rule emits back into `e`,
    // so a slice over a symbolic cons keeps reducing. `peel` thins the floor;
    // `to_zero` exhausts it, leaving the bare tail.
    let source = r#"
        use /std/{Io, Str, Eq, Nat};
        let peel(n : Nat) -> Eq(Nat/sub(Nat/add(3, n), 1), Nat/add(2, n)) = Eq/refl();
        let to_zero(n : Nat) -> Eq(Nat/sub(Nat/add(1, n), 1), n) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn arr_concat_is_a_free_monoid() {
    // `peel_arr` (core::spine) makes `Arr` a free monoid on its elements, the twin
    // of `bin_concat_is_a_free_monoid`: `concat` associates, the empty array `[]`
    // is its identity, and a literal run re-segments freely — all by `refl` for
    // SYMBOLIC arrays (and elements), which `reduce` cannot fold. `convert` peels
    // the two `ArrConcat`s to a common normal form.
    let source = r#"
        use /std/{Io, Str, Eq, Arr};
        let assoc(@T : Type, a : Arr(T), b : Arr(T), c : Arr(T))
            -> Eq(Arr/concat(a, Arr/concat(b, c)), Arr/concat(Arr/concat(a, b), c)) =
            Eq/refl();
        let left_id(@T : Type, a : Arr(T)) -> Eq(Arr/concat(Arr/nil(), a), a) = Eq/refl();
        let right_id(@T : Type, a : Arr(T)) -> Eq(Arr/concat(a, Arr/nil()), a) = Eq/refl();
        let resegment(@T : Type, a : T, b : T, c : Arr(T))
            -> Eq(Arr/concat(Arr/cons(a, Arr/single(b)), c), Arr/concat(Arr/single(a), Arr/concat(Arr/single(b), c))) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn arr_concat_length_clash_is_rejected() {
    // Unlike `Bin`, an `Arr` element disagreement is NOT a clash (elements are
    // terms that may be convertible) — but a literal *length* mismatch still is:
    // `[x, y]` and `[x]` peel their shared head and leave one side longer, a
    // definite `Clash`, so the `refl` is rejected. Exercises `peel_arr`'s clash
    // against the empty identity (the element-mismatch case instead defers to the
    // structural arm, kept sound by `Stuck` fall-through).
    let source = r#"
        use /std/{Io, Str, Eq, Arr};
        let bad(@T : Type, x : T, y : T) -> Eq(Arr/cons(x, Arr/single(y)), Arr/single(x)) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn erased_param_used_at_runtime_is_rejected() {
    // Relevance (QTT {0,ω}), phase 2: `@` on the type marks the binder erased
    // (quantity 0). Returning an erased binder is a runtime (ω) use of a `Zero`
    // var — the one and only relevance error.
    let source = r#"
        use /std/{Io, Str, Nat};
        let f : (n : @Nat) -> Nat = (n) => n;
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn erased_param_unused_is_accepted() {
    // An erased binder that the body never references is fine — and at runtime
    // it is still passed (erasure is phase 3), so the call behaves normally.
    let source = r#"
        use /std/{Io, Str, Nat};
        let f : (n : @Nat, m : Nat) -> Nat = (n, m) => m;
        let r : Nat = f(5, 3);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn erased_param_in_type_position_is_accepted() {
    // The category-(a) flip: a term checked against `Type` is an erased context
    // (ρ=0), so an erased binder is usable there. `Eq(n, n)` is a type, so the
    // erased `n` flows freely into it — rejected without the ρ=0 chokepoint.
    let source = r#"
        use /std/{Io, Str, Nat, Eq};
        let P : (n : @Nat) -> Type = (n) => Eq(n, n);
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn erased_param_is_dropped_at_runtime() {
    // Phase 3: erasure actually drops `Zero` binders and their arguments. `g`'s
    // erased `n` does not exist at runtime, yet `g` passes it to `h`'s erased `m`
    // (category c lets the erased var be passed). This runs ONLY if both the
    // parameter and the argument are dropped — otherwise `h(n)` would reference a
    // variable that erase removed from `g`, a dangling runtime reference.
    let source = r#"
        use /std/{Io, Str, Nat};
        let h : (m : @Nat) -> Nat = (m) => 0;
        let g : (n : @Nat) -> Nat = (n) => h(n);
        let r : Nat = g(7);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn erased_struct_field_collapses_to_bare_value() {
    // Phase 3b: a record with an erased field is a newtype — the erased field is
    // dropped and the struct collapses to its single relevant field (bare `Nat`
    // here). `make` fills the erased `ghost` with its own erased `n`, which only
    // works if both are dropped (else a dangling runtime reference). Projecting
    // `.val` off the collapsed record must still yield `val`, not `ghost`.
    let source = r#"
        use /std/{Io, Str, Nat};
        struct Wrap pub { val : Nat, ghost : @Nat }
        let make : (n : @Nat) -> Wrap = (n) => Wrap { val = 5, ghost = n };
        let r : Nat = make(7).val;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_tuple_field_is_a_subset_type() {
    // The anonymous Σ form of the same idea: `(bytes : Bin, @proof)` is a subset
    // type whose erased witness is dropped, collapsing to the bare relevant
    // field. Here `make` puts its erased `n` in the erased second component.
    let source = r#"
        use /std/{Io, Str, Nat};
        let make : (n : @Nat) -> { val : Nat, @Nat } = (n) => (5, n);
        let r : Nat = make(7).0;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_definition_param_is_dropped_at_runtime() {
    // Item 3 (surface): the combined def-form sugar `let f(n : @Nat) -> R = body`
    // carries the quantity on the inline parameter — previously only the explicit
    // `let f : (n : @Nat) -> R = …` signature form did. `g`'s erased `n` is
    // dropped and passed to `h`'s erased `m` (runs only if both are dropped).
    let source = r#"
        use /std/{Io, Str, Nat};
        let h(m : @Nat) -> Nat = 0;
        let g(n : @Nat) -> Nat = h(n);
        let r : Nat = g(7);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn erased_definition_param_used_at_runtime_is_rejected() {
    // The def-form counterpart of the rejection check: an erased inline param
    // returned at runtime is the same leak the signature form catches.
    let source = r#"
        use /std/{Io, Str, Nat};
        let f(n : @Nat) -> Nat = n;
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn erased_field_projected_at_runtime_is_rejected() {
    // Item 2 (soundness): the erased field is dropped from the rep, so projecting
    // it in a runtime (ω) position must be rejected exactly like referencing an
    // erased binder — otherwise the projection reads past the collapsed value.
    // The relevant `.val` (the test above) still projects fine.
    let source = r#"
        use /std/{Io, Str, Nat};
        struct Wrap pub { val : Nat, ghost : @Nat }
        let make : (n : @Nat) -> Wrap = (n) => Wrap { val = 5, ghost = n };
        let r : Nat = make(7).ghost;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn erased_union_payload_is_dropped_at_runtime() {
    // Item 1: a constructor payload field marked `@` on its type is erased —
    // dropped from the runtime variant tuple. `make` fills the erased `ghost`
    // with its own erased `n` (runs only if both are dropped); the match binds
    // only the relevant `val`, so its projection must skip the absent field.
    let source = r#"
        use /std/{Io, Str, Nat};
        union Boxed
        | box(ghost : @Nat, val : Nat)
        end
        let make : (n : @Nat) -> Boxed = (n) => Boxed/box(n, 5);
        let get : (b : Boxed) -> Nat = (b) =>
            match b : Nat
            | box(ghost, val) => val
            end;
        let r : Nat = get(make(7));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_union_payload_used_at_runtime_is_rejected() {
    // The erased payload binder may only appear in erased positions; returning
    // it from the arm is a runtime use of a field that erase has dropped.
    let source = r#"
        use /std/{Io, Str, Nat};
        union Boxed
        | box(ghost : @Nat, val : Nat)
        end
        let get : (b : Boxed) -> Nat = (b) =>
            match b : Nat
            | box(ghost, val) => ghost
            end;
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn flt_to_le_bin_prints_raw_bytes() {
    let source = r#"
        std/Io/write(std/Io/stdout, std/Flt/to_le_bin(+1.5))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        1.5f32.to_le_bytes()
    );
}

#[test]
fn io_write() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, /std/Str/to_bin("hello"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"hello"
    );
}

#[test]
fn io_write_stderr() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stderr, /std/Str/to_bin("oops"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"oops"
    );
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        match std/Io/read(std/Io/stdin, 1024) : {}
        | chunk(b) => let w = std/Io/write(std/Io/stdout, b); ()
        | eof() => ()
        | error(_) => ()
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"hello\n"
    );
}

#[test]
fn empty_bin_literal_is_the_empty_sequence() {
    // The empty `Bin` literal concatenated with a value is the identity.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, std/Bin/concat(\\, /std/Str/to_bin("ok")))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(
        io.output(),
        b"ok"
    );
}

// Local binders shadow like-named *module* bindings, and a local name never
// leaks past its lexical scope. Inside `mod Foo` the module binding is `Foo/go`:
// an inner `let go` must shadow it (so `shadowed` is 3, not the captured 7),
// while a `go` that is a sibling of an inner `let go = 3` — reached only after
// that scope closes — must resolve back to `Foo/go` (so `sibling` is 7, not a
// leaked, unbound bare `go`). Encoded as 3*10 + 7 = 37, so the unlawful-capture
// regression reads 77 and a scope leak fails to compile.
#[test]
fn local_binders_shadow_module_bindings_without_leaking() {
    let source = r#"
        use /std/{Nat, Io, Str};
        mod Foo
            pub let go : /std/Nat = 7;
            pub let shadowed : /std/Nat =
                let go : /std/Nat = 3;
                go;
            pub let sibling : /std/Nat =
                let probe : /std/Nat = (let go : /std/Nat = 3; go);
                go;
        end
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/mul(Foo/shadowed, 10), Foo/sibling))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"37");
}

// Named fields end to end: a dependent record (the vector's length indexes its
// type) constructed with written names, consumed through `.label` and `.index`
// access on the same value — both resolve to the same positional projection.
#[test]
fn named_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Io};
        let p : { n : Nat, v : Vec(Nat, n) } =
            (n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())));
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(Nat/add(total(p.v, 0), Nat/mul(p.0, 0))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

// `Io/read(h, n)` is the typed blocking read: each call yields a `chunk` of
// 1..n available bytes (here one injected line per refill, served in `n`-byte
// slices), and the third read past the data yields `eof`.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io};
        let show(r : Io/Read) -> {} =
            match r : {}
            | chunk(b) => let _ = Io/write(Io/stdout, b); ()
            | eof() => Io/print("1")
            | error(_) => Io/print("e")
            end;
        let _ = show(Io/read(Io/stdin, 2));
        let _ = show(Io/read(Io/stdin, 2));
        show(Io/read(Io/stdin, 2))
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

#[test]
fn file_read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/read_all("data.txt"))
        | success(contents) => Io/write(Io/stdout, contents)
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bin("error"))
        end
        "#;

    let (system, io) =
        MockHost::builder().files([("data.txt", "file contents")]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"file contents"
    );
}

#[test]
fn file_read_all_of_a_missing_path_is_not_found() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/read_all("nope.txt"))
        | success(_) => Io/print("contents")
        | failure(e) =>
            match e : {}
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | refused() => Io/print("refused")
            | tls() => Io/print("tls")
            | would_block() => Io/print("would block")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"not found"
    );
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/with("out.txt", Io/Mode/write(), (f) => File/write(f, /std/Str/to_bin("written"))))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"ok"
    );
    assert_eq!(io.file(b"out.txt"), Some(b"written".to_vec()));
}

// Matching on an effectful scrutinee must evaluate it exactly once — the
// erased union match binds the scrutinee in a `let` and projects from it.
// Append mode makes a second evaluation visible: it would append twice.
#[test]
fn effectful_match_scrutinee_runs_once() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/with("log.txt", Io/Mode/append(), (f) => File/write(f, /std/Str/to_bin("x"))))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"ok"
    );
    assert_eq!(io.file(b"log.txt"), Some(b"x".to_vec()));
}

// The public file ops run on the opaque handle inside the bracket: `File/read`
// pulls bytes from the `File` that `using` hands the body, and `using` closes
// it afterwards.
#[test]
fn file_read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Io, Str, Bin, Task};
        match Task/block_on(File/with("lines.txt", Io/Mode/read(), (f) =>
            Task/bind(File/read(f, 1024), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) =
        MockHost::builder().files([("lines.txt", "first\nsecond\n")]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"first\nsecond\n"
    );
}

#[test]
fn std_io_read_line_sequences_lines() {
    let source = r#"
        use /std/{Io, Reader, Option, Bin, Str};
        let program : Reader({}) =
            let ! = Reader/bind;
            let first = Reader/read_line!;
            let second = Reader/read_line!;
            match first : Reader({})
            | some(a) =>
                match second : Reader({})
                | some(b) =>
                    match Str/of_bin(Bin/concat(a, b)) : Reader({})
                    | some(s) => Reader/pure(Io/print(s))
                    | none() => Reader/pure(Io/print("invalid utf-8"))
                    end
                | none() => Reader/pure(Io/print("missing"))
                end
            | none() => Reader/pure(Io/print("missing"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["alpha", "beta"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"alpha\nbeta\n"
    );
}

#[test]
fn std_io_read_line_signals_eof_with_none() {
    let source = r#"
        use /std/{Io, Reader, Option};
        let program : Reader({}) =
            let ! = Reader/bind;
            let first = Reader/read_line!;
            let second = Reader/read_line!;
            match second : Reader({})
            | some(_) => Reader/pure(Io/print("line"))
            | none() => Reader/pure(Io/print("eof"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["only"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"eof"
    );
}

// A line longer than `read_line`'s 1024-byte refill chunk forces the buffer
// to absorb one full chunk, miss the newline, and refill before slicing.
#[test]
fn std_io_read_line_spans_refills() {
    let source = r#"
        use /std/{Io, Reader, Option, Bin, Nat};
        let program : Reader({}) =
            let ! = Reader/bind;
            let line = Reader/read_line!;
            match line : Reader({})
            | some(bytes) => Reader/pure(Io/print(Nat/to_str(Bin/len(bytes))))
            | none() => Reader/pure(Io/print("eof"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let long_line = "a".repeat(1500);
    let (system, io) = MockHost::builder().stdin_lines([long_line.as_str()]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"1501"
    );
}

#[test]
fn triangular_sum() {
    let source = r#"
        let result : std/Nat =
            match 5 : std/Nat
            | 0 => 0
            | pred + 1, ih => std/Nat/add(ih, pred)
            end;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"10"
    );
}

#[test]
fn match_omitted_motive_infers() {
    // The same induction as `triangular_sum`, but with the motive omitted. It is
    // non-dependent (every arm has type `std/Nat`), so the synthesized metavar
    // motive is solved by the arms — no explicit `: std/Nat` needed.
    let source = r#"
        let result : std/Nat =
            match 5
            | 0 => 0
            | pred + 1, ih => std/Nat/add(ih, pred)
            end;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"10"
    );
}

#[test]
fn multi_arg_function() {
    let source = r#"
        let add : (std/Int, std/Int) -> std/Int = (x, y) => std/Int/add(x, y);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(add(+3, +4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+7"
    );
}

#[test]
fn curried_function() {
    let source = r#"
        let add : (std/Int) -> (std/Int) -> std/Int = (x) => (y) => std/Int/add(x, y);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(add(+3)(+4))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+7"
    );
}

#[test]
fn let_bang_identity_monad_sequences_bangs() {
    // A minimal Identity monad over `std/Nat`: `bind(m, f) = f(m)`. The compiler is
    // monad-agnostic — `let ! = bind;` applies the binary `bind` to `(action, cont)`
    // per `!` site — so the sugar `add(a!, b!)` threads each banged value through a
    // fresh continuation and evaluates to `add(a, b)`.
    let source = r#"
        let bind : (std/Nat, (std/Nat) -> std/Nat) -> std/Nat = (m, f) => f(m);
        let a : std/Nat = 3;
        let b : std/Nat = 4;
        let result : std/Nat = let ! = bind; std/Nat/add(a!, b!);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn let_bang_std_parse_threads_bangs_left_to_right() {
    // The real `std/Parse` monad. `let ! = Parse/bind;` partially applies the curried
    // bind, fixing its leading `Type` arguments with `?` holes — and because the bind is
    // re-elaborated per `!` site, each site mints its own holes (solved by inference).
    // `Parse/bind` stays in head position, so no annotations are needed.
    // Two `any_byte!`s read consecutive bytes; using a *non-commutative* `Nat/sub`
    // pins the evaluation order: on "BA" the first byte is 'B' (66) and the second
    // 'A' (65), so the result is 66 - 65 = 1 (the reversed order would saturate to 0).
    let source = r#"
        use /std/{Parse, Nat, Result, Io};

        let parser : Parse/Parse(Nat) =
            let ! = Parse/bind;
            Parse/pure(Nat/sub(Parse/any_byte!, Parse/any_byte!));

        match Parse/run(parser, /std/Str/to_bin("BA")) : {}
        | success(n) => Io/print(Nat/to_str(n))
        | failure(msg) => Io/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source");
    let loader = crate::text::FileLoader::new(base);

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, &loader, system)
        .expect("expected result");
    assert_eq!(io.output(), b"1");
}

#[test]
fn let_bang_region_mixes_action_types() {
    // A single region sequences two actions of *different* payload types: a
    // `Parse(Bin)` (`take_while`) and a `Parse(Nat)` (`any_byte`). This works only
    // because `let ! = Parse/bind;` is re-elaborated per `!` site, so each site gets
    // its own holes (`?A := Bin` for the first, `?A := Nat` for the second). A single
    // shared bind value would force one `A` and reject this. On "AB": `take_while(is_a)`
    // reads "A" (stops at 'B'), then `any_byte` reads 'B' (66); `Bin/append("A", 66)`
    // is "AB".
    let source = r#"
        use /std/{Parse, Nat, Bin, Bln, Result, Io, Str};

        let is_a : (Nat) -> Bln = (b) => match b : Bln | 'A' => true | _ => false end;

        let parser : Parse/Parse(Bin) =
            let ! = Parse/bind;
            Parse/pure(Bin/append(Parse/take_while(is_a)!, Parse/any_byte!));

        match Parse/run(parser, /std/Str/to_bin("AB")) : {}
        | success(s) =>
            match Str/of_bin(s) : {}
            | some(t) => Io/print(t)
            | none() => Io/print("invalid utf-8")
            end
        | failure(msg) => Io/print(msg)
        end
        "#;

    let base = Path::new(env!("CARGO_MANIFEST_DIR"));
    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source");
    let loader = crate::text::FileLoader::new(base);

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, &loader, system)
        .expect("expected result");
    assert_eq!(
        io.output(),
        b"AB"
    );
}

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : std/Nat) -> Type =
            match n : Type
            | 0 => {}
            | pred + 1, ih => { T, ih }
            end;

        let cons(T : Type, n : std/Nat, x : T, xs : Vec(T, n)) -> Vec(T, std/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : std/Nat, xs : Vec(T, std/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(std/Nat, 1) = cons(std/Nat, 0, 42, ());
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(head(std/Nat, 0, v))))
    "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

#[test]
fn folds_constant_arg_through_let_function() {
    // `let f(x) = Nat/add(x, 1); f(3)` must fold end-to-end to a literal `4` in
    // `main`. Without the interim DCE before `inline_calls`, `specialize_calls`
    // leaves a dead closure body in `module.clsrs` whose direct call to the
    // lifted clone of `f` inflates the inliner's call-site count, blocking the
    // splice that ultimately lets constant folding see `3` next to the successor.
    use crate::{cont, text};

    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = Nat/add(x, 1);
        f(3)
        "#;

    let entrypoint = source.parse::<text::Entrypoint>().unwrap();

    let mut main_func: Option<cont::Func> = None;
    crate::compile_entrypoint(
        Duration::from_secs(5),
        &entrypoint,
        &text::NullLoader,
        |stage| {
            if let crate::Stage::Optm(module) = stage {
                let entry = module.entry().expect("module has entry").clone();
                let (_, func) = module
                    .funcs()
                    .iter()
                    .find(|(name, _)| name == &entry)
                    .expect("entry function present in module");
                main_func = Some(func.clone());
            }
        },
    )
    .expect("compile succeeded");

    let main = main_func.expect("Stage::Optm observed");

    assert!(
        main.region.preallocs.is_empty(),
        "expected main to have no preallocs, got {:?}",
        main.region.preallocs,
    );
    assert!(
        main.region.blocks.is_empty(),
        "expected main to have no nested blocks, got {} block(s)",
        main.region.blocks.len(),
    );

    let folded: Vec<&cont::ValueName> = main
        .region
        .values
        .iter()
        .filter_map(|(name, val)| match val {
            cont::Value::Pure(cont::Data::Nat(4)) => Some(name),
            _ => None,
        })
        .collect();
    assert_eq!(
        folded.len(),
        1,
        "expected exactly one Pure(Data::Nat(4)) in main, got values {:?}",
        main.region.values,
    );
    let folded_name = folded[0].clone();

    match &main.region.tail {
        cont::Tail::Jump(jump) => {
            assert_eq!(
                jump.target, main.resume,
                "expected main to jump to its resume sentinel",
            );
            assert_eq!(
                jump.params,
                vec![folded_name],
                "expected main to return the folded Pure(Data::Nat(4))",
            );
        }
        other => panic!("expected resume jump in main, got {other:?}"),
    }
}

#[test]
fn printf_partial_evaluation_reduces_residual() {
    // End-to-end smoke for §2 (`evaluate_pure_calls`) and §3 (size-bounded
    // multi-site inlining) on `Fmt/printf("%s is %d")(name)(30)`. §2 interprets
    // pure sub-bodies of the parser combinator at compile time; §3 then
    // dissolves the residual primitive wrappers at every call site (including the
    // `Str/of_bin` validation guarding the runtime `%s` argument). Together they
    // collapse the post-§1 residue (≈14 funcs) down to a handful — the assert pins
    // a comfortable upper bound while leaving headroom for legitimate std/Fmt drift.
    // Proof-carrying `Str` routes both runtime paths through recursive, unfoldable
    // validators: the `%d` (`Nat/to_str`) path through its decimal digit producer
    // (digit/single_digit/Str/concat), and the `%s` (`Str/trim`) path through the
    // codepoint-peeling proof-carrying `slice` (drop_n/take_n/drop1/take1/tl_proof).
    // Both carry their UTF-8 proof and can't be folded even for a constant, so a
    // handful of extra residual funcs over the pre-`/syn/Str` baseline are expected.
    // The shared `classify` (the single UTF-8 layout source consumed by both the
    // validator `step` and the runtime decoder) now has two reachable call sites, so
    // size-bounded multi-site inlining keeps it as one residual func rather than
    // folding it into a sole caller — one extra func, the intended cost of the dedup.
    let source = r#"
        use /std/{Str, Io, Bin, Fmt};

        match Io/read(Io/stdin, 1024) : {}
        | chunk(bytes) =>
            match Str/of_bin(bytes) : {}
            | some(s) => Fmt/printf("%s is %d")(Str/trim(s))(30)
            | none() => Io/print("invalid input")
            end
        | eof() => Io/print("invalid input")
        | error(_) => Io/print("invalid input")
        end
        "#;

    let entrypoint = source
        .parse::<crate::text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let mut optm_funcs: Option<usize> = None;

    let wasm_module = crate::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        &crate::text::NullLoader,
        |stage| {
            if let crate::Stage::Optm(module) = stage {
                optm_funcs = Some(module.funcs().len());
            }
        },
    )
    .expect("compile succeeded");

    let funcs = optm_funcs.expect("Stage::Optm observed");
    assert!(
        funcs <= 13,
        "expected at most 13 residual funcs after partial evaluation and \
         size-bounded multi-site inlining, got {funcs}",
    );

    let (system, io) = MockHost::builder().stdin_lines(["Alice"]).build();
    crate::run_wasm(&wasm_module, system).expect("execution succeeded");
    assert_eq!(
        io.output(),
        b"Alice is 30"
    );
}

#[test]
fn indexed_vec_append_executes() {
    // Rung A of the indexed-union ladder, *executed*: `append`'s motive binds
    // the length index (`(v : Vec(T, k)) => Vec(T, Nat/add(k, m))`), the
    // `cons` arm meets it through the definitional successor-peeling of
    // `Nat/add`, and the implicit index arguments of the recursive call are
    // solved to the arm's *first* binder. Running (not just compiling) guards
    // the zonk realignment of multi-binder arm scopes: with the in-group
    // order flipped, the solved indices silently referenced the wrong binder
    // and the program trapped at runtime.
    let source = r#"
        use /std/{Nat, Bin, Io};
        union Vec(T : Type) : (n : Nat)
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        rec append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, Nat/add(n, m)) =
            match v : (v : Vec(T, k)) => Vec(T, Nat/add(k, m))
            | nil() => w
            | cons(j, x, xs) => Vec/cons(x, append(xs, w))
            end;
        rec total(@n : Nat, v : Vec(Nat, n), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        let a : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let b : Vec(Nat, 1) = Vec/cons(4, Vec/nil());
        let c : Vec(Nat, 3) = append(a, b);
        Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(total(c, 0))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn implicit_union_type_param_executes() {
    // A `@`-marked union parameter is implicit at the type constructor too:
    // `Eq2(2, 2)` infers `A` from the indices, `Eq2(@Nat, 3, 3)` pins it, and
    // the eliminator's motive type-pattern still spells every slot. Running
    // (not just checking) also guards metavariable spines through the
    // Π-domain close/reopen round trip: a solved implicit type-arg's solution
    // names a sibling binder, and without the delayed substitution the two
    // spellings of the same domain compare as distinct.
    let source = r#"
        use /std/{Nat, Bin, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(z) => Eq2/refl()
            end;
        let pinned : Eq2(@Nat, 3, 3) = Eq2/refl();
        let proof : Eq2(2, 2) = Eq2/refl();
        let inferred : Eq2(2, 2) = sym2(proof);
        match inferred : {}
        | refl(z) => let _ = Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(z))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn implicit_union_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the old explicit spelling queues `Nat` into the
    // explicit slots — one argument too many, an error rather than a silent
    // reinterpretation. (`Eq2(@Nat, 2, 2)` is the pinned spelling.)
    let source = r#"
        use /std/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat, 2, 2) = Eq2/refl();
        Io/write(Io/stdout, /std/Str/to_bin("no"))
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn parked_constraints_let_nested_constructor_metas_resolve() {
    // `sym2(Eq2/refl())` — the argument's fresh metas meet the domain's fresh
    // metas as flex–flex pairs embedded under the union type. Before the
    // constraint store (§8) the argument's `expect` failed at quiescence,
    // seconds before the result-type unification would have pinned
    // everything. Now the pairs park, the output `expect` solves the domain
    // metas against the annotation, and the wake retries the parked pairs.
    let source = r#"
        use /std/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(z) => Eq2/refl()
            end;
        let direct : Eq2(2, 2) = sym2(Eq2/refl());
        let chained : Eq2(3, 3) = sym2(sym2(Eq2/refl()));
        match chained : {}
        | refl(z) => let _ = Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(z))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"3");
}

#[test]
fn parked_constraints_still_reject_the_unsolvable() {
    // An undecidable-at-first constraint that never resolves must still fail —
    // at the item drain, attributed to its origin. `refl` forces both indices
    // equal; `2` and `3` are not.
    let source = r#"
        use /std/{Nat, Io};
        union Eq2(@A : Type) : (x : A, y : A)
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(2, 3) = Eq2/refl();
        Io/write(Io/stdout, /std/Str/to_bin("no"))
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

#[test]
fn omitted_motive_infers_over_a_compound_scrutinee() {
    // The motive hole's scope is opened with the scrutinee — a non-pattern
    // spine entry when the scrutinee is compound. Occurrence abstraction in
    // `solve` rewrites the scrutinee's occurrences in the expected type to
    // the motive binder, so the dependent motive infers where it previously
    // had to be spelled.
    let source = r#"
        use /std/{Nat, Vec, Io};
        rec build(n : Nat) -> Vec(Nat, n) =
            match n : (m) => Vec(Nat, m)
            | 0 => Vec/nil()
            | pred + 1, ih => Vec/cons(0, ih)
            end;
        let d(k : Nat) -> Vec(Nat, Nat/add(k, k)) =
            match Nat/add(k, k)
            | 0 => Vec/nil()
            | pred + 1, ih => build(Nat/succ(pred))
            end;
        Io/print(Nat/to_str(Vec/len(d(2))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"4");
}

#[test]
fn bare_tuple_continuation_tail_infers() {
    // The recorded dead-end from the result-directed elaboration work: a bare
    // tuple in a monadic continuation's tail, its expected type a metavariable
    // pinned only by the *outer* apply's result unification. The in-apply
    // postponement defers the tuple, the constraint store parks the flex–flex
    // codomain pair across the inner apply, and the outer pin wakes both.
    let source = r#"
        use /std/{Parse, Nat, Bin, Io};
        let pairer : Parse({ Nat, Nat }) =
            Parse/bind(Parse/any_byte, (a) => Parse/pure((a, a)));
        rec with_sugar : Parse({ Nat, Nat }) =
            let ! = Parse/bind;
            let a = Parse/any_byte!;
            Parse/pure((a, 0));
        match Parse/run(pairer, /std/Str/to_bin("hi"))
        | success(pair) => Io/print(Nat/to_str(pair.0))
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"104"
    );
}

#[test]
fn checking_problem_parks_until_an_outer_pin_lands() {
    // The constraint store's own window: the inner apply's output expect
    // parks (provisional success), so the postponed tuple re-check meets a
    // still-unsolved expected type — it now parks as a *checking problem*
    // behind a placeholder metavariable, and the outer annotation's pin wakes
    // it. Before ParkedWork::Checking this was a NotATupleType error.
    let source = r#"
        use /std/{Nat, Lst, Io};
        let mk(@A : Type, a : A) -> Lst(A) = Lst/cons(a, Lst/nil());
        let use_(@B : Type, l : Lst(B)) -> Lst(B) = l;
        let v : Lst({ Nat, Nat }) = use_(mk((1, 2)));
        match v : {}
        | nil() => ()
        | cons(p, rest) => let _ = Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(p.1))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn checking_problem_without_a_pin_still_rejects() {
    // A checking problem whose expected type is never pinned drains as a
    // cannot-infer at the tuple's own span — parked, not silently accepted.
    let source = r#"
        use /std/{Nat, Io};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        Io/write(Io/stdout, /std/Str/to_bin(Nat/to_str(n)))
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// === Structs (SYNTAX.md) ================================================

// A transparent record: build with a pinned head, project by label and by
// index — both resolve to the same positional projection.
#[test]
fn struct_transparent_pair_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair(Nat, Nat) { fst = 2, snd = 5 };
        Io/print(Nat/to_str(Nat/add(p.fst, p.1)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// The bare-name head infers the parameters from the fields (and the expected
// type at the binding).
#[test]
fn struct_parameter_inference_at_construction() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 4, snd = 3 };
        Io/print(Nat/to_str(Nat/mul(p.fst, p.snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"12"
    );
}

// A zero-cost newtype: a single positional field, projected with `.0`. It
// erases to its bare field, so the projection elides at runtime.
#[test]
fn struct_newtype_projects() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Meters pub { Nat }
        let m : Meters = Meters { 5 };
        Io/print(Nat/to_str(m.0))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"5");
}

// A dependent field: a later field's type mentions an earlier field (the
// vector's length indexes its type).
#[test]
fn struct_dependent_fields_run_end_to_end() {
    let source = r#"
        use /std/{Vec, Nat, Io};
        pub struct Sized pub { n : Nat, v : Vec(Nat, n) }
        let s : Sized = Sized { n = 2, v = Vec/cons(30, Vec/cons(12, Vec/nil())) };
        rec total(@k : Nat, v : Vec(Nat, k), acc : Nat) -> Nat =
            match v : Nat
            | nil() => acc
            | cons(m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        Io/print(Nat/to_str(total(s.v, 0)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

// The motivating case: an abstract type — public type, hidden representation —
// usable only through exported smart constructors/accessors in its module.
#[test]
fn struct_abstract_smart_constructor_round_trips() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
            pub let to_nat(c : Celsius) -> Nat = c.0;
        end
        Io/print(Nat/to_str(Celsius/to_nat(Celsius/of_nat(42))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

// Constructing a private-representation struct from outside its declaring
// module is rejected (`PrivateRepresentation`).
#[test]
fn struct_private_construction_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { Nat }
        end
        let c : Celsius/Celsius = Celsius/Celsius { 42 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("representation"),
        "unexpected error: {error}"
    );
}

// Projecting a private-representation struct's field from outside its module is
// rejected (`PrivateField`), even when the value was obtained legitimately.
#[test]
fn struct_private_projection_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        Io/print(Nat/to_str(c.0))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// Diagnostics name binders with the source names the user wrote, not the
// `hint#counter` gensyms elaboration opens them under (axis (a)): the inferred
// function type must read `(n : Nat)`, never `(n#3 : Nat)`.
#[test]
fn diagnostic_uses_source_binder_names() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = n;
        let bad : Nat = f;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("inferred: (n : Nat) -> Nat"),
        "binder lost its source name: {error}"
    );
    assert!(!error.contains('#'), "fresh-name suffix leaked: {error}");
}

// Two binders sharing a source name (shadowing) stay distinct in the message
// via a minimal numeric suffix — `n` and `n2` — instead of both reading `n`
// (axis (a) collision handling).
#[test]
fn diagnostic_disambiguates_shadowed_binders() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> ((n : Nat) -> Nat) = (k : Nat) => n;
        let bad : Nat = f;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("inferred: (n : Nat) -> (n2 : Nat) -> Nat"),
        "shadowed binders not disambiguated: {error}"
    );
    assert!(!error.contains('#'), "fresh-name suffix leaked: {error}");
}

// Globals print under their shortest in-scope spelling, not their fully
// qualified canonical path (axis (b)): `Vec` and `Nat`, never `std/Vec/Vec`
// or `sys/Nat`.
#[test]
fn diagnostic_shortens_global_names() {
    let source = r#"
        use /std/{Nat, Vec};
        let bad(n : Nat, v : Vec(Nat, n)) -> Nat = v;
        bad
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("inferred: Vec(Nat, n)"),
        "globals not shortened: {error}"
    );
    assert!(
        !error.contains("std/Vec"),
        "qualified union path leaked: {error}"
    );
    assert!(
        !error.contains("sys/"),
        "qualified prim path leaked: {error}"
    );
}

// A struct type is nominal: it never converts with a structural tuple type of
// the same fields.
#[test]
fn struct_is_not_a_tuple() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : { fst : Nat, snd : Nat } = Pair { fst = 1, snd = 2 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// A struct literal must supply exactly the declared fields, in order.
#[test]
fn struct_wrong_field_count_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// Written field labels are validated positionally — no reordering.
#[test]
fn struct_field_label_out_of_order_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { snd = 1, fst = 2 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// A struct literal whose head names a non-struct binding is rejected as
// `NotAStructType` (its type is reported), not misreported as unbound.
#[test]
fn struct_literal_non_struct_head_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        let Foo : Nat = 3;
        let bad : Nat = Foo { x = 1 };
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(error.contains("struct type"), "unexpected error: {error}");
}

// === struct destructuring ===============================================

// A struct pattern in a `let` binds each named field by label (a pun binds the
// field's own name); the head's parameters are inferred.
#[test]
fn struct_destructure_pun_binds_fields_by_label() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 2, snd = 5 };
        let Pair { fst, snd } = p;
        Io/print(Nat/to_str(Nat/add(fst, snd)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A rename field `fst = a` binds the field to a fresh name.
#[test]
fn struct_destructure_rename_binds_new_names() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 2, snd = 5 };
        let Pair { fst = a, snd = b } = p;
        Io/print(Nat/to_str(Nat/mul(a, b)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"10"
    );
}

// Naming a subset of fields is allowed — the rest are simply ignored.
#[test]
fn struct_destructure_partial_ignores_unlisted_fields() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 9, snd = 5 };
        let Pair { fst } = p;
        Io/print(Nat/to_str(fst))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"9");
}

// Patterns nest: a renamed field can itself be a struct pattern.
#[test]
fn struct_destructure_nested_struct_pattern() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Inner pub { a : Nat, b : Nat }
        pub struct Outer pub { it : Inner, c : Nat }
        let o : Outer = Outer { it = Inner { a = 1, b = 2 }, c = 3 };
        let Outer { it = Inner { a, b }, c } = o;
        Io/print(Nat/to_str(Nat/add(Nat/add(a, b), c)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"6");
}

// The head is checked nominally: destructuring a `Pair` as a structurally
// identical but distinctly-named `Other` is rejected (not silently projected).
#[test]
fn struct_destructure_wrong_head_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        pub struct Other pub { fst : Nat, snd : Nat }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let Other { fst, snd } = p;
        Io/print("no")
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(5), source, system).is_err());
}

// A struct pattern works as an un-annotated lambda parameter — the domain comes
// from the head, parameters inferred against the expected function type.
#[test]
fn struct_destructure_in_lambda_parameter() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        let sum : (_ : Pair(Nat, Nat)) -> Nat = (Pair { fst, snd }) => Nat/add(fst, snd);
        let p : Pair(Nat, Nat) = Pair { fst = 6, snd = 1 };
        Io/print(Nat/to_str(sum(p)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

// A struct pattern destructures a constructor's payload in a match arm.
#[test]
fn struct_destructure_in_match_arm() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B }
        union Wrap | wrap(Pair(Nat, Nat)) end
        let w : Wrap = Wrap/wrap(Pair { fst = 3, snd = 8 });
        let out : Nat =
            match w : Nat
            | wrap(Pair { fst, snd }) => Nat/add(fst, snd)
            end;
        Io/print(Nat/to_str(out))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"11"
    );
}

// Destructuring projects through the representation-privacy boundary: a
// private-representation struct's field cannot be pulled out from another module.
#[test]
fn struct_destructure_private_field_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        mod Celsius
            use /std/{Nat};
            pub struct Celsius { value : Nat }
            pub let of_nat(n : Nat) -> Celsius = Celsius { value = n };
        end
        let c : Celsius/Celsius = Celsius/of_nat(42);
        let Celsius/Celsius { value } = c;
        Io/print(Nat/to_str(value))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("field") && error.contains("private"),
        "unexpected error: {error}"
    );
}

// === Pattern matrix compilation =========================================

// A nested constructor pattern dispatches on the tail of the list as well as its
// head — `cons(x, cons(y, _))` reads the first two elements in one arm.
#[test]
fn matrix_nested_constructor_pattern() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let xs : Lst(Nat) = cons(4, cons(5, nil()));
        let out : Nat =
            match xs
            | cons(x, cons(y, _)) => Nat/add(x, y)
            | cons(x, nil())      => x
            | nil()               => 0
            end;
        Io/print(Nat/to_str(out))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"9");
}

// A `Nat` literal nested in a constructor payload compiles to a `switch`: the
// `0`-headed list takes the special arm, any other head the binder default.
#[test]
fn matrix_nat_literal_in_nested_column() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let special : Lst(Nat) = cons(0, cons(5, nil()));
        let other : Lst(Nat)   = cons(7, nil());
        let head_code(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(0, _) => 100
            | cons(x, _) => x
            | nil()      => 0
            end;
        Io/print(Nat/to_str(Nat/add(head_code(special), head_code(other))))
        "#;

    // special -> 100 (head is 0), other -> 7 (head binder). 100 + 7 = 107.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"107"
    );
}

// A top-level `Nat` match with a *named* default falls through to the matrix —
// the dedicated nat-match form only accepts an anonymous `| _ =>`, so binding the
// non-literal case is new. It compiles to a `switch` whose default binds `k`.
#[test]
fn matrix_nat_literal_named_default() {
    let source = r#"
        use /std/{Nat, Io};
        let label(n : Nat) -> Nat =
            match n
            | 0 => 100
            | 1 => 200
            | k => Nat/add(k, 1000)
            end;
        Io/print(Nat/to_str(Nat/add(Nat/add(label(0), label(1)), label(7))))
        "#;

    // 100 + 200 + 1007 = 1307.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"1307"
    );
}

// A `Nat` literal nested inside a struct field pattern: the struct column expands
// to its labels, and the `tag` sub-column dispatches via `switch`.
#[test]
fn matrix_nat_literal_in_struct_field() {
    let source = r#"
        use /std/{Nat, Io};
        pub struct Tagged pub { tag : Nat, val : Nat }
        let read(t : Tagged) -> Nat =
            match t
            | Tagged { tag = 0, val = v } => v
            | Tagged { tag = _, val = _ } => 999
            end;
        Io/print(Nat/to_str(read(Tagged { tag = 0, val = 42 })))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"42"
    );
}

// A `_` fallthrough at a union column expands into the *unlisted* constructors:
// here it covers `nil()` (and any non-matching `cons`), which needs the
// constructor's arity from the registry.
#[test]
fn matrix_wildcard_expands_unlisted_constructors() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let head_or_zero(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(x, _) => x
            | _          => 0
            end;
        let full : Lst(Nat)  = cons(9, nil());
        let empty : Lst(Nat) = nil();
        Io/print(Nat/to_str(Nat/add(head_or_zero(full), head_or_zero(empty))))
        "#;

    // full -> 9, empty -> 0 (the `_` materializes the nil arm). 9 + 0 = 9.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"9");
}

// Two rows may share a head constructor, distinguished by a nested literal — the
// whole point of an ordered matrix over a per-constructor map.
#[test]
fn matrix_repeated_constructor_head() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let classify(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(0, _) => 1
            | cons(1, _) => 2
            | cons(_, _) => 3
            | nil()      => 0
            end;
        let a : Lst(Nat) = cons(0, nil());
        let b : Lst(Nat) = cons(1, nil());
        let c : Lst(Nat) = cons(8, nil());
        Io/print(Nat/to_str(Nat/add(Nat/add(classify(a), classify(b)), classify(c))))
        "#;

    // 1 + 2 + 3 = 6.
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"6");
}

// Multiple scrutinees fall out of a tuple scrutinee: `(a, b)` with refutable
// fields is a one-row matrix that expands into two `Bln` columns.
#[test]
fn matrix_multi_scrutinee_via_tuple() {
    let source = r#"
        use /std/{Nat, Io, Bln};
        let combine(a : Bln, b : Bln) -> Nat =
            match (a, b)
            | (true, true)  => 3
            | (true, false) => 2
            | (false, _)    => 1
            end;
        Io/print(Nat/to_str(combine(true, false)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

// Coverage is left to core: a union match that lists neither every constructor
// nor a `_` fallthrough is rejected as non-exhaustive (the `nil` arm is missing).
#[test]
fn matrix_non_exhaustive_missing_constructor_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        use /std/Lst/*;
        let head(xs : Lst(Nat)) -> Nat =
            match xs
            | cons(0, _) => 100
            | cons(x, _) => x
            end;
        Io/print(Nat/to_str(head(nil())))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("missing match case") && error.contains("nil"),
        "unexpected error: {error}"
    );
}

// Expanding a `_` at a union column needs the constructor's union; when the
// constructors are not in scope (no `use`), the tag cannot be resolved and the
// match is rejected with an actionable error.
#[test]
fn matrix_wildcard_unresolved_constructor_rejected() {
    let source = r#"
        use /std/{Nat, Io};
        union Shape | dot() | line(Nat) end
        let area(s : Shape) -> Nat =
            match s
            | line(n) => n
            | _       => 0
            end;
        Io/print(Nat/to_str(area(Shape/dot())))
        "#;

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_text(Duration::from_secs(5), source, system).unwrap_err();
    assert!(
        error.contains("line") && error.contains("resolve"),
        "unexpected error: {error}"
    );
}

// === Str (std/Str) ======================================================

// `"..."` is a `Str` primitive value (UTF-8 by construction); `Io/print` writes
// a `Str` straight to stdout.
#[test]
fn str_literal_prints_its_bytes() {
    let source = r#"
        use /std/{Str, Io};
        let s : Str = "hello";
        Io/print(s)
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"hello"
    );
}

// `Str/of_bin` is the checked constructor: it runs `is_utf8` and yields `some`
// for well-formed UTF-8. `é` is the bytes C3 A9, a valid 2-byte sequence.
#[test]
fn str_of_bin_accepts_multibyte_utf8() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\c3\a9) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), [0xc3, 0xa9]);
}

// An invalid lead byte fails `is_utf8`, so `Str/of_bin` returns `none`.
#[test]
fn str_of_bin_rejects_invalid_utf8() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\ff) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("rejected")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"rejected"
    );
}

// A truncated multi-byte sequence (a 2-byte lead with no continuation) fails the
// continuation-byte check, so `of_bin` returns `none`.
#[test]
fn str_of_bin_rejects_truncated_multibyte() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\c3) : {}
        | some(s) => Io/print(s)
        | none() => Io/print("rejected")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"rejected"
    );
}

// `/std/BinProof` is a library of erased equational lemmas about the random-access
// `Bin` ops (`get`/`slice`/`len`). Type checking is demand-driven, so each lemma is
// referenced here (in a local `let`, checked before it is pruned) to force its proof
// body through the checker; if any lemma fails to check, this test fails.
#[test]
fn arr_fold_sums_elements() {
    let source = r#"
        use /std/{Io, Str, Nat, Arr};
        let xs : Arr(Nat) = Arr/cons(10, Arr/cons(20, Arr/single(30)));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Arr/fold(xs, 0, (e, acc) => Nat/add(acc, e)))))
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"60");
}

#[test]
fn bin_fold_sums_bytes() {
    let source = r#"
        use /std/{Io, Str, Nat, Bin};
        let b = Bin/append(Bin/append(Bin/append(\\, 10), 20), 30);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Bin/fold(b, 0, (byte, acc) => Nat/add(acc, byte)))))
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"60");
}

#[test]
fn bin_proof_lemmas_type_check() {
    let source = r#"
        use /std/{BinProof, Bin, Nat, Io};
        let proofs = (
            Nat/lt_succ(@5),
            Nat/le_refl(4),
            Nat/le_to_lt_succ(Nat/le_refl(3)),
            BinProof/get_cons_zero(@7, @\\),
            BinProof/get_cons_succ(@7, @Bin/append(\\, 9), @0),
            BinProof/len_cons(@7, @\\),
            BinProof/slice_full(@\\),
            BinProof/slice_empty(@\\, @0),
            BinProof/slice_nested(@\\, @0, @0, @0, @0),
            BinProof/slice_cons_zero(@7, @\\, @0),
            BinProof/slice_cons_succ(@7, @\\, @0, @1)
        );
        Io/print("ok")
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}

// The UTF-8 decode certification lemmas: naming them forces their bodies to
// elaborate (demand-driven checking). `cont_len` is the one that exercises the
// comparison intrinsic — `step` only reduces in `cont` state because
// `eql(succ(succ k''), 1)` now folds to `false`.
#[test]
fn utf8_decode_lemmas_type_check() {
    let source = r#"
        use /std/{Str, Io};
        let lemmas = (Str/bad_uninhabited, Str/cont_len);
        Io/print("ok")
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}

// `Str/len` and `Str/get` count and index by codepoint, not byte. The string is
// `a€😀` — a 1-byte, a 3-byte, and a 4-byte scalar — so its length is 3 and the
// codepoints decode to U+0061 (97), U+20AC (8364), and U+1F600 (128512).
#[test]
fn str_get_indexes_codepoints_of_every_width() {
    let source = r#"
        use /std/{Str, Nat, Io, Option};
        match Str/of_bin(\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) =>
            Io/print(Str/flatten([
                Nat/to_str(Str/len(s)), ",",
                Nat/to_str(Option/unwrap_or(Str/get(s, 0), 0)), ",",
                Nat/to_str(Option/unwrap_or(Str/get(s, 1), 0)), ",",
                Nat/to_str(Option/unwrap_or(Str/get(s, 2), 0))
            ]))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"3,97,8364,128512"
    );
}

// `Str/slice` cuts at codepoint boundaries, so slicing `[1, 2)` out of `a€😀`
// yields the whole 3-byte euro sign — never a split sequence.
#[test]
fn str_slice_cuts_on_codepoint_boundaries() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\61\e2\82\ac\f0\9f\98\80) : {}
        | some(s) => Io/print(Str/slice(s, 1, 2))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
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
        match Str/of_bin(\61\c3\a9\e2\82\ac\f0\9f\98\80\62) : {}
        | some(s) => Io/print(Str/slice(s, 1, 4))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), [0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xf0, 0x9f, 0x98, 0x80]);
}

// `Str/trim` is string-typed and strips only the leading/trailing ASCII
// whitespace, leaving the interior multibyte scalar (`café`, with a 2-byte `é`)
// intact.
#[test]
fn str_trim_keeps_interior_multibyte() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\20\20\63\61\66\c3\a9\20\20) : {}
        | some(s) => Io/print(Str/trim(s))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), [0x63, 0x61, 0x66, 0xc3, 0xa9]);
}

// An all-whitespace string trims to empty: `trim_start` overshoots `trim_end`,
// and the `Nat/min` guard collapses the slice to nothing rather than trapping.
#[test]
fn str_trim_all_whitespace_is_empty() {
    let source = r#"
        use /std/{Str, Io};
        match Str/of_bin(\20\09\20) : {}
        | some(s) => Io/print(Str/concat(Str/trim(s), "!"))
        | none() => Io/print("bad")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"!"
    );
}

// A *non-productive* inner `rec` forced in a type position must degrade to the
// reduce deadline (an error), never hang or panic — the regression guard for
// inner-`rec` reduction at the type level (a `Subterm::Rec` demanded by an
// eliminator is now forced, not left stuck).
#[test]
fn nonproductive_inner_rec_in_type_position_is_preempted() {
    let source = r#"
        use /std/{Bln};
        let spin : Bln =
            rec go : Bln = go;
            go;
        let bad : Type =
            match spin : Type
            | true => {}
            | false => {}
            end;
        let x : bad = ();
        0
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(1), source, system).is_err());
}

#[test]
fn random_bin_returns_requested_length() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, /std/Rand/bin(8))"#,
        system,
    )
    .expect("expected result");

    let output = io.output();
    assert_eq!(output.len(), 8);
}

#[test]
fn bln_logic_and_of_str() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Bln, Str, Option, Io};
        let computed = Bln/and(Bln/or(false, true), Bln/not(false));
        let parsed = match Bln/of_str("false") : Bln
            | some(b) => b
            | none() => true
            end;
        Io/write(Io/stdout, Str/to_bin(Str/concat(Bln/to_str(computed), Bln/to_str(parsed))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"truefalse"
    );
}

#[test]
fn bln_xor_executes() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Bln, Str, Io};
        let a = Bln/xor(true, false);
        let b = Bln/xor(true, true);
        Io/write(Io/stdout, Str/to_bin(Str/concat(Bln/to_str(a), Bln/to_str(b))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"truefalse"
    );
}

#[test]
fn bln_eql_executes() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Bln, Str, Io};
        let a = Bln/eql(true, true);
        let b = Bln/eql(true, false);
        Io/write(Io/stdout, Str/to_bin(Str/concat(Bln/to_str(a), Bln/to_str(b))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"truefalse"
    );
}

#[test]
fn nat_bitwise_ops_execute() {
    // The first input byte is `A` (65); reading it from the host keeps the
    // operand opaque to the optimizer, so each op is lowered to its WebAssembly
    // instruction and executed for real rather than folded at compile time. This
    // is what exercises the truncating `shl` (65 << 25 sets bit 31, which the
    // i31 carrier drops to leave 2^25).
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Bin, Nat, Str, Option};
        let bytes = match Io/read(Io/stdin, 16) : Bin
            | chunk(b) => b
            | eof() => \\
            | error(_) => \\
            end;
        let x = Option/unwrap_or(Bin/get(bytes, 0), 0);
        let r = Str/flatten([
            Nat/to_str(Nat/and(x, 15)), ",",
            Nat/to_str(Nat/or(x, 128)), ",",
            Nat/to_str(Nat/xor(x, 255)), ",",
            Nat/to_str(Nat/shl(x, 25)), ",",
            Nat/to_str(Nat/shr(x, 1))
        ]);
        Io/write(Io/stdout, Str/to_bin(r))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"1,193,190,33554432,32"
    );
}

#[test]
fn int_bitwise_ops_execute() {
    // `x` is the host byte `A` (65) read as an `Int`, kept opaque to the
    // optimizer so each op lowers to its WebAssembly instruction. This exercises
    // the Int-distinctive behaviors: a truncating `shl` that lands on bit 30 and
    // so reloads negative (65 << 24), an arithmetic (sign-preserving) `shr` on a
    // negative operand (-65 >> 1 = -33), and the `xor`-based `not` (-x - 1).
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Bin, Nat, Int, Str, Option};
        let bytes = match Io/read(Io/stdin, 16) : Bin
            | chunk(b) => b
            | eof() => \\
            | error(_) => \\
            end;
        let x = Nat/to_int(Option/unwrap_or(Bin/get(bytes, 0), 0));
        let neg = Int/sub(+0, x);
        let r = Str/flatten([
            Int/to_str(Int/and(x, +15)), ",",
            Int/to_str(Int/or(x, +128)), ",",
            Int/to_str(Int/xor(x, +255)), ",",
            Int/to_str(Int/shl(x, +24)), ",",
            Int/to_str(Int/shr(neg, +1)), ",",
            Int/to_str(Int/not(x))
        ]);
        Io/write(Io/stdout, Str/to_bin(r))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"+1,+193,+190,-1056964608,-33,-66"
    );
}

#[test]
fn nat_of_str_returns_option() {
    // `123` parses; `12a` (non-digit) and the empty string are `none`, taking
    // the `unwrap_or` defaults — `123 + 7 + 9`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Nat, Str, Option, Io};
        let ok = Option/unwrap_or(Nat/of_str("123"), 0);
        let bad = Option/unwrap_or(Nat/of_str("12a"), 7);
        let empty = Option/unwrap_or(Nat/of_str(""), 9);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(ok, bad), empty))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"139"
    );
}

#[test]
fn int_of_str_returns_option() {
    // `-5` and `+7` parse (compared by magnitude); `x` is `none` → default `+3`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Nat, Int, Str, Option, Io};
        let neg = Int/abs(Option/unwrap_or(Int/of_str("-5"), +0));
        let pos = Int/abs(Option/unwrap_or(Int/of_str("+7"), +0));
        let bad = Int/abs(Option/unwrap_or(Int/of_str("x"), +3));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(neg, pos), bad))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"15"
    );
}

#[test]
fn flt_of_str_returns_option() {
    // `12.0`, `.5` (empty integer part), and `1e3` parse; `abc` is `none` →
    // default `+4.0`. Values are truncated to `Nat` for an exact assertion:
    // `12 + (0.5*2) + 1000 + 4`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Nat, Flt, Str, Option, Io};
        let whole = Flt/to_nat(Option/unwrap_or(Flt/of_str("12.0"), +0.0));
        let half = Flt/to_nat(Flt/mul(Option/unwrap_or(Flt/of_str(".5"), +0.0), +2.0));
        let exp = Flt/to_nat(Option/unwrap_or(Flt/of_str("1e3"), +0.0));
        let bad = Flt/to_nat(Option/unwrap_or(Flt/of_str("abc"), +4.0));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(whole, half), Nat/add(exp, bad)))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"1017"
    );
}

#[test]
fn option_result_char_helpers() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Option, Result, Char, Nat, Str, Io};
        let opt = Option/unwrap_or(Option/map((x : Nat) => Nat/add(x, 1), Option/some(4)), 0);
        let res0 : Result(Nat, Nat) = Result/success(5);
        let res = Result/unwrap_or(Result/map((x : Nat) => Nat/mul(x, 2), res0), 0);
        let up = Char/to_upper('a');
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Nat/add(opt, res), up))))
        "#,
        system,
    )
    .expect("expected result");

    // opt = 5, res = 10, up = 'A' = 65  ->  80
    assert_eq!(
        io.output(),
        b"80"
    );
}

#[test]
fn clock_diff_of_two_distinct_now_readings() {
    // Two scripted wall readings 30 s + 400 ns apart. `Time/now` referenced
    // twice must perform two *distinct* host calls (the nullary-effect
    // distinctness the struct-head reduction relies on), so the diff is the
    // gap between them, not zero.
    let (system, io) =
        MockHost::builder().wall([(1, 100, 500), (1, 130, 900)]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        let a = /std/Time/now();
        let b = /std/Time/now();
        let d = /std/Time/diff(b, a);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(/std/Time/secs(d))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"30"
    );
}

#[test]
fn clock_mono_reads_scripted_elapsed() {
    let (system, io) = MockHost::builder().mono([(2, 7)]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        let e = /std/Time/elapsed();
        std/Io/write(std/Io/stdout, /std/Str/to_bin(/std/Nat/to_str(/std/Time/secs(e))))
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"2");
}

#[test]
fn proc_args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `Arr(Bin)`; indexing it round-trips one entry.
    let (system, io) =
        MockHost::builder().args(["prog", "hello", "world"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"std/Io/write(std/Io/stdout, /std/Option/unwrap_or(/std/Arr/get(/std/Proc/args, 1), \\))"#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"hello"
    );
}

#[test]
fn proc_env_found_unwraps_to_some() {
    let (system, io) = MockHost::builder().env([("HOME", "/root")]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        match /std/Proc/env("HOME") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bin("missing")); ()
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"/root"
    );
}

#[test]
fn proc_env_absent_is_none() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        match /std/Proc/env("NOPE") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bin("missing")); ()
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(
        io.output(),
        b"missing"
    );
}

#[test]
fn proc_exit_halts_with_code() {
    // exit traps: it surfaces its code *and* the trailing write never runs.
    let entrypoint = r#"
        let _ : std/Void = /std/Proc/exit(7);
        std/Io/write(std/Io/stdout, /std/Str/to_bin("unreachable"))
        "#
    .parse::<crate::text::Entrypoint>()
    .expect("failed to parse source");

    let module = crate::compile_entrypoint(
        Duration::from_secs(5),
        &entrypoint,
        &crate::text::NullLoader,
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = MockHost::builder().build();
    let code = crate::run_wasm(&module, system).expect("execution succeeded");

    assert_eq!(code, 7);
    assert!(io.output().is_empty());
}

#[test]
fn let_tuple_destructures() {
    // `let (a, b) = …` binds each leaf by projection off a fresh temp. The
    // annotation types that temp — a bare tuple literal is uninferable on its
    // own (the same constraint a non-destructuring `let t = (+3, +4)` hits).
    let source = r#"
        let (a, b) : { std/Int, std/Int } = (+3, +4);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(b)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+4"
    );
}

#[test]
fn nested_let_tuple_destructures() {
    // Nested tuple patterns project recursively: `c` is `t.1.1`. Only the outer
    // binding needs an annotation — the inner `(b, c)` projects off `t.1`, whose
    // type the elaborator infers from the projection (unlike a bare literal).
    let source = r#"
        let (a, (b, c)) : { std/Int, { std/Int, std/Int } } = (+1, (+2, +3));
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(c)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+3"
    );
}

#[test]
fn let_tuple_destructures_without_annotation() {
    // PROTOTYPE CHECK: with Infer-mode tuple synthesis, a bare tuple literal no
    // longer needs an annotation — `(+3, +4)` infers `{ std/Int, std/Int }`.
    let source = r#"
        let (a, b) = (+3, +4);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(b)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+4"
    );
}

#[test]
fn let_three_tuple_destructures() {
    // A genuine 3-tuple (not a nested pair): exercises projection at index 2 and
    // a three-pattern binder. `c` is `t.2`.
    let source = r#"
        let (a, b, c) : { std/Int, std/Int, std/Int } = (+10, +20, +30);
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(c)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+30"
    );
}

#[test]
fn func_tuple_param_destructures() {
    // A function-definition-sugar parameter destructures its argument; the
    // Π-binder is anonymous, so the result type cannot mention the whole pair.
    let source = r#"
        let snd((a, b) : { std/Int, std/Int }) -> std/Int = b;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(snd((+7, +8)))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+8"
    );
}

#[test]
fn lambda_tuple_param_destructures() {
    // A bare lambda taking one pair parameter needs its own parens: `((a, b))`.
    let source = r#"
        let fst : (_ : { std/Int, std/Int }) -> std/Int = ((a, b)) => a;
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(fst((+5, +6)))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+5"
    );
}

#[test]
fn match_arm_tuple_destructures() {
    // A constructor whose payload is a tuple destructures inside the arm binder.
    let source = r#"
        union Boxed
        | box({ std/Int, std/Int })
        end
        let value : Boxed = Boxed/box((+9, +1));
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Int/to_str(
            match value : std/Int
            | box((x, y)) => x
            end
        )))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"+9"
    );
}

// Client network IO (Phase A): `connect` rides the `Hdl` byte stream, so
// `Tcp/call` writes a request and drains the scripted response to EOF.
#[test]
fn net_call_round_trips_a_scripted_endpoint() {
    let source = r#"
        use /std/{Tcp, Io, Str, Task};
        match Task/block_on(Tcp/call(Tcp/default, "example.com", 80, Str/to_bin("GET /\r\n\r\n")))
        | success(response) => Io/write(Io/stdout, response)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("example.com:80", "HTTP/1.0 200 OK\r\n\r\nhello")])
        .build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"HTTP/1.0 200 OK\r\n\r\nhello"
    );
}

// Connecting to an endpoint that was never scripted is refused, and the status
// decodes to `Tcp/refused`.
#[test]
fn net_call_to_an_unscripted_endpoint_is_refused() {
    let source = r#"
        use /std/{Tcp, Io, Task};
        match Task/block_on(Tcp/call(Tcp/default, "example.com", 80, /std/Str/to_bin("ping")))
        | success(_) => Io/print("connected")
        | failure(e) =>
            match e : {}
            | refused() => Io/print("refused")
            | tls() => Io/print("tls")
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | would_block() => Io/print("would block")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"refused"
    );
}

// A custom `Config` with an optional `Duration` timeout flows through the
// bracket; `Tcp/read` pulls bytes from the socket the body is handed.
#[test]
fn net_with_custom_timeout_config_reads_response() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Option, Time, Task};
        let settings = Tcp/Settings {
            connect_timeout = Option/some(Time/of_millis(500)),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = false
        };
        match Task/block_on(Tcp/with(settings, "db.internal", 5432, (s) =>
            Task/bind(Tcp/read(s, 64), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder().net([("db.internal:5432", "PONG")]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"PONG"
    );
}

// Server network IO (Stage A): `serve` binds a listener, pulls the scripted
// inbound connection, and runs the handler per connection — which reads the
// request off the socket and writes a response the host captures. The exhausted
// inbound queue then fails the next `accept`, ending the loop and closing the
// bracketed listener.
#[test]
fn net_serve_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Task};
        match Task/block_on(Tcp/serve("0.0.0.0", 8080, (c) =>
            Task/bind(Tcp/read(c, 64), (r) =>
                match r : Task({})
                | chunk(bytes) =>
                    Task/bind(Tcp/write(c, Bin/concat(Str/to_bin("echo: "), bytes)), (wrote) => Task/pure(()))
                | eof() => Task/pure(())
                | error(_) => Task/pure(())
                end))) : {}
        | success(u) => ()
        | failure(_) => Io/print("listen failed")
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"echo: ping".to_vec()]);
}

// TLS client (Stage A): `Tcp/with` with `tls = true` upgrades the connected
// socket via `start_tls` before the body runs. The mock host serves the
// scripted endpoint cleartext (no real handshake under test), so the upgrade is
// a no-op identity and the round-trip still succeeds — exercising the wiring,
// types, and prim threading end to end through codegen.
#[test]
fn net_with_tls_upgrades_and_reads() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Option, Task};
        let settings = Tcp/Settings {
            connect_timeout = Option/none(),
            read_timeout = Option/none(),
            write_timeout = Option/none(),
            tls = true
        };
        match Task/block_on(Tcp/with(settings, "secure.example", 443, (s) =>
            Task/bind(Tcp/read(s, 64), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .net([("secure.example:443", "SECURE-PONG")])
        .build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.output(), b"SECURE-PONG");
}

// Server TLS termination (Stage A): `serve_tls` builds a config token, then
// upgrades each accepted connection via `start_tls_server` before the handler
// runs. The mock host runs cleartext, so the upgrade is a no-op identity and
// the handler echoes the scripted request the host captures.
#[test]
fn net_serve_tls_handles_a_scripted_inbound_connection() {
    let source = r#"
        use /std/{Tcp, Io, Str, Bin, Task};
        match Task/block_on(Tcp/serve_tls("0.0.0.0", 8443, Str/to_bin("CERT"), Str/to_bin("KEY"), (c) =>
            Task/bind(Tcp/read(c, 64), (r) =>
                match r : Task({})
                | chunk(bytes) =>
                    Task/bind(Tcp/write(c, Bin/concat(Str/to_bin("tls: "), bytes)), (wrote) => Task/pure(()))
                | eof() => Task/pure(())
                | error(_) => Task/pure(())
                end))) : {}
        | success(u) => ()
        | failure(_) => Io/print("serve failed")
        end
        "#;

    let (system, io) = MockHost::builder().inbound(["ping"]).build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(io.captures(), vec![b"tls: ping".to_vec()]);
}

// HTTP client (Phase B): `Http/perform` renders a `Request`, sends it through
// `/std/Tcp`, and runs the `/std/Parse`-based response parser over the reply —
// exercising the byte-scanning parser end to end through codegen.
#[test]
fn http_perform_parses_a_scripted_response() {
    let source = r#"
        use /std/{Http, Io, Str, Nat, Task};
        match Task/block_on(Http/perform(Http/get("example.com", 80, "/"))) : {}
        | success(response) =>
            let ct = match Http/header(response, "Content-Type") : Str
                | some(value) => value
                | none() => "none"
                end;
            match Str/of_bin(response.body) : {}
            | some(body) =>
                let _ = Io/write(Io/stdout, Str/to_bin(Str/flatten([
                    Nat/to_str(response.status.code), " ", ct, " ", body
                ]))); ()
            | none() => let _ = Io/write(Io/stdout, Str/to_bin("bad body")); ()
            end
        | failure(_) => let _ = Io/write(Io/stdout, Str/to_bin("error")); ()
        end
        "#;

    // The trailing bytes past `Content-Length: 5` must be dropped by the body
    // framing, leaving the body exactly "hello".
    let (system, io) = MockHost::builder()
        .net([(
            "example.com:80",
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello AND MORE",
        )])
        .build();
    crate::run_text(Duration::from_secs(5), source, system).expect("expected result");
    assert_eq!(
        io.output(),
        b"200 text/plain hello"
    );
}

#[test]
fn task_scheduler_parks_polls_and_resumes() {
    // The `/std/Task` event loop end to end: the root fiber yields a `wait` on
    // stdin-READ and parks, `run` marshals the parked handle/interest into
    // `Io/poll` (the mock reports it ready), and resumes the continuation — which
    // performs the write. Exercises the novel path of a union variant carrying a
    // closure through erasure and codegen.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let prog : Task({}) =
            Task/bind(Task/wait(Io/stdin, 1), (_) =>
                let wrote = Io/write(Io/stdout, Str/to_bin("ok"));
                Task/pure(()));
        Task/run(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
}

#[test]
fn task_bind_reads_and_echoes() {
    // The monad surface: a `with`-bind do-block over `Task/bind`, sequencing the
    // `read` leaf (which completes without parking under the mock) into `write`,
    // driven to its value by `block_on`. Exercises `bind`, the leaf actions, and
    // do-notation against the new module.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io};
        let prog : Task({}) =
            let ! = Task/bind;
            let r = Task/read(Io/stdin, 1024)!;
            match r : Task({})
            | chunk(bytes) =>
                let wrote = Io/write(Io/stdout, bytes);
                Task/pure(())
            | eof() => Task/pure(())
            | error(_) => Task/pure(())
            end;
        Task/block_on(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn block_on_returns_a_typed_value_and_awaits_a_spawned_child() {
    // `block_on` returns a typed value AND a spawned child runs because the root
    // explicitly `await`s it: the root spawns a child (which parks on stdin),
    // writes "root;", then awaits the child's future. Awaiting parks the root on
    // the future, so the child is polled awake, writes "child;", and fulfils the
    // future with 5; the root resumes and `block_on` hands back 5 + 2 = 7.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat};
        let root : Task(Nat) =
            let ! = Task/bind;
            let f = Task/spawn(() =>
                Task/bind(Task/wait(Io/stdin, 1), (_) =>
                    let w = Io/write(Io/stdout, Str/to_bin("child;"));
                    Task/pure(5)))!;
            let w = Io/write(Io/stdout, Str/to_bin("root;"));
            let c = Task/await(f.result)!;
            Task/pure(Nat/add(c, 2));
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Task/block_on(root))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;child;7");
}

#[test]
fn join_all_runs_children_concurrently_and_collects_in_order() {
    // `join_all` spawns every task as its own fiber (they run concurrently) and
    // collects their results positionally regardless of completion order. Here both
    // children complete synchronously when scheduled, writing "a;" then "b;", and
    // the gathered results [1, 2] sum to 3.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat, Arr};
        let main : Task({}) =
            let ! = Task/bind;
            let rs = Task/join_all([
                () =>
                    let w = Io/write(Io/stdout, Str/to_bin("a;"));
                    Task/pure(1),
                () =>
                    let w = Io/write(Io/stdout, Str/to_bin("b;"));
                    Task/pure(2)
            ])!;
            let s = Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(/std/Option/unwrap_or(Arr/get(rs, 0), 0), /std/Option/unwrap_or(Arr/get(rs, 1), 0)))));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"a;b;3");
}

#[test]
fn map_transforms_a_tasks_result() {
    // `Task/map` applies a pure function to a task's result — here turning the Nat
    // 42 into its decimal string, with no explicit `bind`/`pure` at the call site.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat};
        let main : Task({}) =
            let ! = Task/bind;
            let s = Task/map(Nat/to_str, Task/pure(42))!;
            let w = Io/write(Io/stdout, Str/to_bin(s));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"42");
}

#[test]
fn race_returns_the_first_and_runs_a_cancelled_losers_finalizer() {
    // Multi-way `race`: the fast branch completes synchronously and wins, returning
    // 10. The slow branch acquires a finalizer with `using`, then parks on stdin —
    // so it never writes "slow;". `race` cancels the loser, and because the loser
    // holds a resource its finalizer still runs (here writing "released;") when the
    // scheduler reclaims it on exit. Output proves the winner's value AND that the
    // loser's cleanup fired without the loser's body completing.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str, Nat};
        let main : Task({}) =
            let ! = Task/bind;
            let v = Task/race([
                () =>
                    let x = Io/write(Io/stdout, Str/to_bin("fast;"));
                    Task/pure(10),
                () =>
                    Task/using(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bin("released;")); (),
                        Task/bind(Task/wait(Io/stdin, 1), (_) =>
                            let y = Io/write(Io/stdout, Str/to_bin("slow;"));
                            Task/pure(20)))
            ])!;
            let z = Io/write(Io/stdout, Str/to_bin(Nat/to_str(v)));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"fast;10released;");
}

#[test]
fn block_on_drops_a_parked_child_when_root_done() {
    // Prompt drop and no deadlock: a fire-and-forget `go` child parks on stdin, but
    // the root writes and finishes first. `block_on` returns the instant the root
    // is done, dropping the still-parked child instead of blocking forever in
    // `Io/poll` on work nothing will ever join. Only "root;" is written, and `run`
    // returns rather than hanging.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let child : Task({}) =
            Task/bind(Task/wait(Io/stdin, 1), (_) =>
                let w = Io/write(Io/stdout, Str/to_bin("child;"));
                Task/pure(()));
        let main : Task({}) =
            Task/bind(Task/go(() => child), (started) =>
                let w = Io/write(Io/stdout, Str/to_bin("root;"));
                Task/pure(()));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;");
}

#[test]
fn constructing_a_leaf_task_performs_no_effect() {
    // Tasks are inert until served. Building a `Task/read` and discarding it must not
    // touch stdin — the syscall is wrapped in `defer`, so it fires only when the
    // scheduler forces it. We construct (and drop) a read of stdin, then read stdin
    // directly: the direct read still sees "hello" because the discarded Task never
    // ran. Before leaves were deferred, constructing the Task ate stdin eagerly and
    // the direct read saw EOF.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let discarded : Task(Io/Read) = Task/read(Io/stdin, 100);
        let r = Io/read(Io/stdin, 100);
        match r : {}
        | chunk(bytes) => let _ = Io/write(Io/stdout, bytes); ()
        | eof() => let _ = Io/write(Io/stdout, Str/to_bin("<eof>")); ()
        | error(_) => let _ = Io/write(Io/stdout, Str/to_bin("<err>")); ()
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn finalizer_runs_for_a_child_parked_on_an_unfulfilled_future() {
    // The previously-leaking path, now closed. A `go` child acquires a resource via
    // `using` (its finalizer writes "released;"), then `await`s a future that nothing
    // ever fulfils — so it parks in the scheduler's `parked` registry and is never
    // woken. The root writes "root;" and finishes. Because the scheduler now retains
    // ownership of every parked fiber (rather than handing it off to the future's
    // waker list, where it was invisible), `block_on`'s shutdown drains the registry
    // and runs the child's finalizer exactly once. Before the fix the "released;"
    // marker leaked and the output was just "root;".
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let main : Task({}) =
            let ! = Task/bind;
            let f : Task/Future({}) = Task/new_future(@{});
            let started = Task/go(() =>
                Task/using(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bin("released;")); (),
                    Task/await(f)))!;
            let w = Io/write(Io/stdout, Str/to_bin("root;"));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;");
}

#[test]
fn an_acquired_finalizer_runs_when_the_fiber_completes() {
    // "Open and trust it", normal path: a fiber `acquire`s a finalizer (writes
    // "closed;"), runs its body ("body;"), and finishes without ever calling
    // `release`. The scheduler runs the finalizer on completion, so the output is
    // "body;closed;" — cleanup happens for free on the success path.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let main : Task({}) =
            let ! = Task/bind;
            let _ = Task/acquire(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bin("closed;")); ())!;
            let _ = Io/write(Io/stdout, Str/to_bin("body;"));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"body;closed;");
}

#[test]
fn manual_release_runs_a_finalizer_once_and_completion_does_not_repeat_it() {
    // "Close it yourself, no double close": a fiber `acquire`s a finalizer (writes
    // "closed;"), runs its body ("body;"), then manually `release`s and continues
    // ("after;"). `release` runs the finalizer AND dequeues the guard, so the
    // completion drain does not run it again. The single "closed;" between "body;"
    // and "after;" proves it fired exactly once — at the release, not again at the end.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Task, Io, Str};
        let main : Task({}) =
            let ! = Task/bind;
            let _ = Task/acquire(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bin("closed;")); ())!;
            let _ = Io/write(Io/stdout, Str/to_bin("body;"));
            let _ = Task/release(Io/stdin)!;
            let _ = Io/write(Io/stdout, Str/to_bin("after;"));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"body;closed;after;");
}

#[test]
fn label_projection_resolves_on_a_type_valued_field() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Str, Nat};
        union Susp(A : Type)
        | now(A)
        | later(() -> Susp(A))
        end
        let Box : Type = { A : Type, t : Susp(A) };
        let step(b : Box) -> Box =
            match b.t : Box
            | now(a) => (b.A, Susp/now(a))
            | later(k) => (b.A, k())
            end;
        let boxed : Box = (Nat, Susp/now(7));
        let stepped = step(boxed);
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
}

#[test]
fn heterogeneous_existential_task_list_through_a_generic_map() {
    // A `Lst` of existential-boxed tasks of DIFFERENT result types, mapped by a
    // generic HOF whose body does an indirect closure call on a continuation
    // pulled out of the box. The arity-1 closure definition is inlined away by
    // the specializer, leaving the `call_ref` with no surviving definition — the
    // codegen path that needs the call-site arity registered for `envr`/`clsr`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(5),
        r#"
        use /std/{Io, Str, Nat, Lst};
        union Susp(A : Type)
        | now(A)
        | later(() -> Susp(A))
        end
        let Box : Type = { A : Type, t : Susp(A) };
        let boxes : Lst(Box) =
            Lst/cons((Nat, Susp/now(7)),
            Lst/cons(({}, Susp/now(())),
            Lst/nil()));
        let stepped = Lst/map((b : Box) =>
            match b.t : Box
            | now(a) => (b.A, Susp/now(a))
            | later(k) => (b.A, k())
            end, boxes);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Lst/len(stepped))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn cell_get_returns_init_value() {
    // Round-trip: mint a cell then read it back.
    assert_eq!(
        run(r#"
            use /std/{Cell, Io, Nat, Str};
            let n : Nat = 42;
            let cell = Cell/new(n);
            Io/print(Nat/to_str(Cell/get(cell)))
        "#),
        b"42",
    );
}

#[test]
fn cell_set_overwrites_value() {
    // Write then read: the getter sees the new value, not the init.
    assert_eq!(
        run(r#"
            use /std/{Cell, Io, Nat, Str};
            let z : Nat = 0;
            let cell = Cell/new(z);
            let _ = Cell/set(cell, 99);
            Io/print(Nat/to_str(Cell/get(cell)))
        "#),
        b"99",
    );
}

#[test]
fn cell_two_cells_are_distinct() {
    // Two cells minted with the same value are independent heap objects.
    // Setting one must not affect the other.
    assert_eq!(
        run(r#"
            use /std/{Cell, Io, Nat, Str};
            let n : Nat = 7;
            let a = Cell/new(n);
            let b = Cell/new(n);
            let _ = Cell/set(a, 1);
            Io/print(Nat/to_str(Cell/get(b)))
        "#),
        b"7",
    );
}

#[test]
fn utf8_inductive_spike() {
    // DE-RISKING PROBE (Str migration): a state-indexed inductive relation over a
    // native `Bin` index, with `cons(c, t)` encoded as `concat(append(\\, c), t)`.
    // The point is `seq` (the concatenation lemma underlying `concat_closed`): a
    // 2-case induction on the derivation whose arms close ONLY if the native-Bin
    // free-monoid laws hold *definitionally* — `concat(\\, b) ≡ b` (stop arm) and
    // `concat(concat(single c, t), b) ≡ concat(single c, concat(t, b))` (more arm).
    // If this typechecks, the inductive-`IsUtf8` approach is viable and the
    // cons-index inversion limit does not bite the proof path.
    let source = r#"
        use /std/{Io, Str, Nat, Bin};

        union Scan
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

        union Utf8 : (s : Scan, b : Bin)
        | stop() : (Scan/lead(), \\)
        | more(c : Nat, st : Scan, t : Bin, rest : Utf8(step(c, st), t))
            : (st, Bin/concat(Bin/append(\\, c), t))
        end

        rec seq(@s : Scan, @a : Bin, @b : Bin, va : Utf8(s, a), vb : Utf8(Scan/lead(), b))
            -> Utf8(s, Bin/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bin/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bin/concat(t, b), seq(rest, vb))
            end;

        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_construction_spike() {
    // DE-RISKING PROBE 2 (Str migration), the CONSTRUCTING side: the `of_bin`
    // checker must BUILD a derivation whose index matches the input `Bin`, by
    // native-`Bin` recursion. That needs the native eliminator's motive to be
    // DEPENDENT — refining `b` to `cons(h, t)` in the cons arm so the arm can
    // return `P(cons h t)` from `ih : P(t)`. Probe with the trivial all-accepting
    // relation `All` built by induction on `b`. If this typechecks, the checker is
    // expressible (real decision-procedure work, but no missing primitive).
    let source = r#"
        use /std/{Io, Str, Nat, Bin};

        union All : (b : Bin)
        | empty() : (\\)
        | snoc(c : Nat, t : Bin, rest : All(t)) : (Bin/concat(Bin/append(\\, c), t))
        end

        rec build(b : Bin) -> All(b) =
            match b : (b) => All(b)
            | \\ => All/empty()
            | (h, t), ih => All/snoc(h, t, ih)
            end;

        Io/write(Io/stdout, Str/to_bin("ok"))
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
    // laws (`concat(\\, b) ≡ b`; associativity). This is the lemma that earns the
    // proof-carrying newtype: `Valid(a) -> Valid(b) -> Valid(concat a b)`.
    let source = r#"
        use /std/{Io, Str, Nat, Bin, Bln};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        union Scan
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

        union Utf8 : (s : Scan, b : Bin)
        | stop() : (Scan/lead(), \\)
        | more(c : Nat, st : Scan, t : Bin, rest : Utf8(step(c, st), t))
            : (st, Bin/concat(Bin/append(\\, c), t))
        end

        let Valid(b : Bin) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bin, @b : Bin, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, Bin/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bin/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bin/concat(t, b), seq(rest, vb))
            end;

        let concat_closed(@a : Bin, @b : Bin, va : Valid(a), vb : Valid(b))
            -> Valid(Bin/concat(a, b)) =
            seq(va, vb);

        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn utf8_of_bin_checker_decides_and_builds_derivations() {
    // INCREMENT B of the Str migration: the `of_bin` decision procedure. It must
    // both DECIDE validity at runtime and BUILD a real `Utf8` derivation in the
    // `some` case. The native `Bin` eliminator is a fold (its `ih` is the
    // fold-of-tail with fixed parameters), but the checker threads a changing
    // `Scan` state — so we fold `b` into a FUNCTION `(s) -> Option(Utf8(s, b))`
    // (foldl-as-foldr convoy), letting each step receive its state from the caller.
    // `of_bin_valid(b) = check(b)(lead)`. The runtime `decide` proves the automaton
    // actually runs: "hi" (ASCII) is accepted, a lone `\80` continuation byte is
    // rejected — output "yesno".
    let source = r#"
        use /std/{Io, Str, Nat, Bin, Bln, Option};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        union Scan
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

        union Utf8 : (s : Scan, b : Bin)
        | stop() : (Scan/lead(), \\)
        | more(c : Nat, st : Scan, t : Bin, rest : Utf8(step(c, st), t))
            : (st, Bin/concat(Bin/append(\\, c), t))
        end

        let Valid(b : Bin) -> Type = Utf8(Scan/lead(), b);

        let check(b : Bin) -> ((s : Scan) -> Option(Utf8(s, b))) =
            match b : (b) => (s : Scan) -> Option(Utf8(s, b))
            | \\ => (s) =>
                match s : (s) => Option(Utf8(s, \\))
                | lead() => Option/some(Utf8/stop())
                | cont(rem, lo, hi) => Option/none()
                | bad() => Option/none()
                end
            | (h, t), ih => (s) =>
                match ih(step(h, s)) : Option(Utf8(s, Bin/concat(Bin/append(\\, h), t)))
                | some(rest) => Option/some(Utf8/more(h, s, t, rest))
                | none() => Option/none()
                end
            end;

        let of_bin_valid(b : Bin) -> Option(Valid(b)) =
            check(b)(Scan/lead());

        let decide(b : Bin) -> Bin =
            match of_bin_valid(b)
            | some(_) => Str/to_bin("yes")
            | none() => Str/to_bin("no")
            end;

        Io/write(Io/stdout, Bin/concat(decide(\68\69), decide(\80)))
        "#;
    assert_eq!(run(source), b"yesno");
}

#[test]
fn utf8_decimal_is_ascii_carries_its_proof() {
    // INCREMENT C of the Str migration: producers (`Nat/to_str`) must yield a
    // `Valid` Bin without a bridge. The trick that avoids ALL Nat-comparison
    // arithmetic: `digit` emits each decimal digit as a CONCRETE byte literal per
    // branch, so `step(byte, lead)` *reduces* to `lead` and the per-digit proof is
    // just `refl`. `single` wraps one ASCII byte into a `Valid` via `subst` over
    // that proof; `decimal` recurses and combines the high digits with the low one
    // through the already-proven `concat_closed`. The result type — `decimal` returns
    // a dependent pair `{ b : Bin, v : Valid(b) }` — IS `decimal_is_ascii`. Runtime
    // check: `decimal(255).b` renders "255", proving the bytes are real digits.
    let source = r#"
        use /std/{Io, Str, Nat, Bin, Bln, Eq};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        union Scan
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

        union Utf8 : (s : Scan, b : Bin)
        | stop() : (Scan/lead(), \\)
        | more(c : Nat, st : Scan, t : Bin, rest : Utf8(step(c, st), t))
            : (st, Bin/concat(Bin/append(\\, c), t))
        end

        let Valid(b : Bin) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bin, @b : Bin, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, Bin/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bin/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bin/concat(t, b), seq(rest, vb))
            end;

        let concat_closed(@a : Bin, @b : Bin, va : Valid(a), vb : Valid(b))
            -> Valid(Bin/concat(a, b)) =
            seq(va, vb);

        let single(c : Nat, ok : Eq(step(c, Scan/lead()), Scan/lead()))
            -> Valid(Bin/append(\\, c)) =
            let r : Utf8(step(c, Scan/lead()), \\) =
                Eq/subst((sc) => Utf8(sc, \\), Eq/sym(ok), Utf8/stop());
            Utf8/more(c, Scan/lead(), \\, r);

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

        let single_digit(d : Nat) -> { b : Bin, v : Valid(b) } =
            let g = digit(d);
            (Bin/append(\\, g.c), single(g.c, g.ok));

        rec decimal(n : Nat) -> { b : Bin, v : Valid(b) } =
            match Nat/lt(n, 10)
            | true => single_digit(n)
            | false =>
                let hi = decimal(Nat/div(n, 10));
                let lo = single_digit(Nat/rem(n, 10));
                (Bin/concat(hi.b, lo.b), concat_closed(@hi.b, @lo.b, hi.v, lo.v))
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
        use /std/{Io, Str, Nat, Bin, Bln};

        let in_range(c : Nat, lo : Nat, hi : Nat) -> Bln =
            match Nat/gte(c, lo)
            | true => Nat/lte(c, hi)
            | false => false
            end;

        union Scan
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

        union Utf8 : (s : Scan, b : Bin)
        | stop() : (Scan/lead(), \\)
        | more(c : Nat, st : Scan, t : Bin, rest : Utf8(step(c, st), t))
            : (st, Bin/concat(Bin/append(\\, c), t))
        end

        let Valid(b : Bin) -> Type = Utf8(Scan/lead(), b);

        rec seq(@s : Scan, @a : Bin, @b : Bin, va : Utf8(s, a), vb : Valid(b))
            -> Utf8(s, Bin/concat(a, b)) =
            match va : (w : Utf8(q, x)) => Utf8(q, Bin/concat(x, b))
            | stop() => vb
            | more(c, st, t, rest) => Utf8/more(c, st, Bin/concat(t, b), seq(rest, vb))
            end;

        let concat_closed(@a : Bin, @b : Bin, va : Valid(a), vb : Valid(b))
            -> Valid(Bin/concat(a, b)) =
            seq(va, vb);

        rec take_to_lead(@s : Scan, @b : Bin, d : Utf8(s, b))
            -> { mid : Bin, tail : Bin, midd : Utf8(s, mid), tv : Valid(tail) } =
            let go =
                match s : (s) => (p : Utf8(s, b))
                    -> { mid : Bin, tail : Bin, midd : Utf8(s, mid), tv : Valid(tail) }
                | lead() => (d) => (\\, b, Utf8/stop(), d)
                | cont(rem, lo, hi) => (d) =>
                    match d : { mid : Bin, tail : Bin, midd : Utf8(Scan/cont(rem, lo, hi), mid), tv : Valid(tail) }
                    | more(c, st, t, rest) =>
                        let w = take_to_lead(rest);
                        (Bin/concat(Bin/append(\\, c), w.mid), w.tail,
                         Utf8/more(c, st, w.mid, w.midd), w.tv)
                    end
                | bad() => (d) =>
                    match d : { mid : Bin, tail : Bin, midd : Utf8(Scan/bad(), mid), tv : Valid(tail) }
                    | more(c, st, t, rest) =>
                        let w = take_to_lead(rest);
                        (Bin/concat(Bin/append(\\, c), w.mid), w.tail,
                         Utf8/more(c, st, w.mid, w.midd), w.tv)
                    end
                end;
            go(d);

        let take1(@b : Bin, d : Valid(b)) -> { cp : Bin, v : Valid(cp) } =
            match d : { cp : Bin, v : Valid(cp) }
            | stop() => (\\, Utf8/stop())
            | more(c, st, t, rest) =>
                let w = take_to_lead(rest);
                (Bin/concat(Bin/append(\\, c), w.mid), Utf8/more(c, st, w.mid, w.midd))
            end;

        let drop1(@b : Bin, d : Valid(b)) -> { rest : Bin, v : Valid(rest) } =
            match d : { rest : Bin, v : Valid(rest) }
            | stop() => (\\, Utf8/stop())
            | more(c, st, t, rest) =>
                let w = take_to_lead(rest);
                (w.tail, w.tv)
            end;

        rec drop_n(n : Nat, @b : Bin, d : Valid(b)) -> { r : Bin, v : Valid(r) } =
            match Nat/eql(n, 0)
            | true => (b, d)
            | false =>
                let w = drop1(d);
                drop_n(Nat/sub(n, 1), @w.rest, w.v)
            end;

        rec take_n(n : Nat, @b : Bin, d : Valid(b)) -> { r : Bin, v : Valid(r) } =
            match Nat/eql(n, 0)
            | true => (\\, Utf8/stop())
            | false =>
                let hd = take1(d);
                let tl = drop1(d);
                let tn = take_n(Nat/sub(n, 1), @tl.rest, tl.v);
                (Bin/concat(hd.cp, tn.r), concat_closed(@hd.cp, @tn.r, hd.v, tn.v))
            end;

        let slice(@b : Bin, d : Valid(b), x : Nat, y : Nat) -> { r : Bin, v : Valid(r) } =
            let dropped = drop_n(x, d);
            take_n(Nat/sub(y, x), @dropped.r, dropped.v);

        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}
