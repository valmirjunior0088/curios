use super::{error, run};

#[test]
fn list_match_is_a_foldr() {
    // Native `List` induction (slice 1): the `| [] | (h, t), ih` eliminator, erased by desugaring to `Nat`-induction on the length and reusing the loop. `f(h, ih) = ih * 10 + h` is non-commutative, so the result distinguishes a structural `foldr` (head is the *first* element, ih is the fold of the tail) from a reversed walk: `[1,2,3,4]` folds to `4321`, not `1234`.
    let source = r#"
        use /std/{Handle, Str, Nat, List};
        let xs : List(Nat) = [1, 2, 3, 4];
        let digits : Nat =
            match xs : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn list_map_fills_every_slot() {
    // `List/map` erases to a single O(n) fill loop (`emit_map`): size the result from `src.len`, allocate once, then write `f(src[i])` into slot `i` via an inline closure `call_ref`. A non-identity `f` (`+1`) over `[10, 20, 30]` must fill *every* slot, not just one: `get(_, 0) + get(_, 2)` = 11 + 31 = 42.
    let source = r#"
        use /std/{Handle, Str, Nat, List, Option};
        let xs : List(Nat) = List/map([10, 20, 30], (n) => Nat/add(n, 1));
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Option/unwrap_or(List/get(xs, 0), 0), Option/unwrap_or(List/get(xs, 2), 0)))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"42");
}

#[test]
fn list_map_distributes_over_cons() {
    // The eliminator rule that lets `List/map` stand in for a structural `foldr` in proofs: for a SYMBOLIC tail `t`, `map f (x :: t) ≡ f x :: map f t` *definitionally*. `refl` checks only because `reduce` distributes the map over the `concat` spine and maps the singleton literal — the same peel the native `List` eliminator does, so it reduces under induction without unfolding a symbolic array.
    let source = r#"
        use /std/{Handle, Str, Eq, Nat, List};
        let step(f : (Nat) -> Nat, x : Nat, t : List(Nat))
            -> Eq(List/map([x, ..t], f), [f(x), ..List/map(t, f)]) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_match_is_a_foldr() {
    // Native `Bytes` induction (slice 2): the `| x[] | (h, t), ih` eliminator, erased exactly like `List` — `Nat`-induction on the byte length, reusing the loop. The leading byte `h` is reflected as a `Nat`. Same non-commutative `foldr` probe as `list_match_is_a_foldr`: the bytes `x[0x01, 0x02, 0x03, 0x04]` fold to `4321`, not `1234`, pinning head = first byte and ih = fold of the tail.
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes};
        let bytes : Bytes = x[0x01, 0x02, 0x03, 0x04];
        let digits : Nat =
            match bytes : (_) => Nat
            | x[] => 0
            | x[h, ..t]; ih => Nat/add(Nat/mul(ih, 10), Byte/to_nat(h))
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn bin_concat_is_a_free_monoid() {
    // `peel_bin` (core::spine) makes `Bytes` a free monoid up to *definitional* equality: concatenation associates, the empty bytestring `x[]` is its identity, and a literal run re-segments freely — all provable by `refl` for SYMBOLIC operands, which `reduce` cannot fold. Each binding's declared type forces `convert` to peel the two `BinConcat`s to a common normal form; without the peel these are stuck, distinct terms and `refl` would not check. A parenthesized spread operand is what keeps the two nestings apart: writing the operands flat would let the literal build one `BinConcat` for both sides, and the `refl` would hold by syntax rather than by the peel.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes};
        let assoc(a : Bytes, b : Bytes, c : Bytes)
            -> Eq(x[..a, ..(x[..b, ..c])], x[..(x[..a, ..b]), ..c]) =
            Eq/refl();
        let left_id(a : Bytes) -> Eq(x[..(x[]), ..a], a) = Eq/refl();
        let right_id(a : Bytes) -> Eq(x[..a, ..(x[])], a) = Eq/refl();
        let resegment(x : Bytes)
            -> Eq(x[0x01, 0x02, ..x], x[0x01, ..(x[0x02, ..x])]) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_concat_leading_byte_clash_is_rejected() {
    // The dual: a leading-byte disagreement under a shared symbolic tail is a definite `Clash`, so `x[0x01] ++ x` and `x[0x02] ++ x` are never convertible and the `refl` is rejected. Guards `peel_bin` against deciding unequal values equal.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes};
        let bad(x : Bytes) -> Eq(x[0x01, ..x], x[0x02, ..x]) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn a_literal_run_is_the_same_value_however_it_is_grouped() {
    // **The premise the fusion cap rests on, stated as a program.** Reduction fuses an all-literal concatenation into one value today (`normalize_concat`); capping that by operand size leaves the `Concat` node standing instead, so a capped spelling and the literal it would have fused to have to remain definitionally equal. They do because `bin_atoms`/`list_atoms` flatten a concatenation into segments and `push` merges every pair of adjacent literal runs (`core::spine`), so both groupings decompose to the same segment list before anything is compared.
    //
    // **Each law carries a symbolic tail, and that is what makes this a test.** Without it both sides are all-literal, reduction fuses each into one value on the way in, and `refl` checks without the peel having decided anything. With it neither side fuses, so the peel is the only thing that can equate them. `curios-core`'s `spine` tests state the same premise directly against the peel; this states it where *both* checkers see it, since a program here is elaborated and then certified.
    //
    // All three carriers, because each flattens in its own representation: two `Bin` grains over packed runs, and `List` over element vectors whose entries are compared syntactically.
    let source = r#"
        use /std/{Handle, Str, Eq, Bits, Bytes, Bool, List};
        let bytes_law(rest : Bytes)
            -> Eq(x[..x[0x30, 0x31], ..x[0x32, 0x33], ..rest], x[..x[0x30, 0x31, 0x32, 0x33], ..rest]) =
            Eq/refl();
        let bits_law(rest : Bits)
            -> Eq(b[..b[1, 0], ..b[1, 1], ..b[0], ..rest], b[..b[1, 0, 1, 1, 0], ..rest]) =
            Eq/refl();
        let list_law(@T : Type, xs : List(T), p : T, q : T, r : T)
            -> Eq([..[p, q], ..[r], ..xs], [..[p, q, r], ..xs]) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn a_regrouped_run_still_respects_its_order() {
    // The control the law above would be worthless without: regrouping preserves element order, so the peel must merge adjacent runs without becoming blind to a reordering. Two bytes swapped across the seam are a `Clash`, and the `refl` is rejected.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes};
        let bad(rest : Bytes)
            -> Eq(x[..x[0x30], ..x[0x31], ..rest], x[..x[0x31, 0x30], ..rest]) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn a_length_and_a_window_do_not_depend_on_grouping() {
    // **The law the measure adds, stated where both checkers see it.** `Bin/len` now answers a wholly-literal spine by folding the operands' own lengths, and `Bin/get`/`Bin/slice` locate their position the same way, rather than rebuilding a `len` per operand or peeling one generator at a time. A length is a definitional equation, so a measure that disagreed with the run would be a false one and congruence carries a false equation to `False` — which is why this is stated as a checked proof rather than an observed result.
    //
    // Each law puts the *same* run on both sides under different groupings, so what is being proven is precisely that grouping is invisible. `curios-core`'s `reduce::intrinsic` tests state the same thing against the folds directly and over more shapes; this is the both-checkers half.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Byte, Nat, List, Option};
        let split_length : Eq(Bytes/len(x[..x[0x30, 0x31], ..x[0x32, 0x33, 0x34]]), 5) = Eq/refl();
        let nested_length : Eq(Bytes/len(x[..x[..x[0x30], ..x[0x31]], ..x[0x32, 0x33, 0x34]]), 5) =
            Eq/refl();
        let split_window
            : Eq(Bytes/slice(x[..x[0x30, 0x31], ..x[0x32, 0x33, 0x34]], 1, 4), x[0x31, 0x32, 0x33]) =
            Eq/refl();
        let third : Byte = 0x33;
        let split_index
            : Eq(Bytes/get(x[..x[0x30, 0x31], ..x[0x32, 0x33, 0x34]], 3), Option/some(third)) =
            Eq/refl();
        let list_length(p : Nat, q : Nat, r : Nat)
            -> Eq(List/len([..[p, q], ..[r]]), 3) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_slice_is_a_monoid_citizen() {
    // `Bytes/slice` rides the free-monoid spine (`core::spine`) as a measured `Window` — a length-`hi - lo` chunk whose contents are symbolic — so the slice algebra holds up to *definitional* equality, provable by `refl` for SYMBOLIC operands that `reduce` cannot fold. `split` fuses two adjacent windows of one base across their shared seam; `empty` drops a zero-width window (the monoid identity); `full` collapses `slice(b, 0, len b)` to its base (the `reduce` partner of the spine's window-collapse). Each declared type forces `convert` to peel the windows to a common normal form; without the peel these are stuck, distinct terms and `refl` would not check.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Nat, True};
        let split(b : Bytes, s : Nat, m : Nat, e : Nat,
                  sm : Nat/Le(s, m), me : Nat/Le(m, e), el : Nat/Le(e, Bytes/len(b)))
            -> Eq(
                x[
                    ..Bytes/slice(b, s, m, @sm, @Nat/Le/trans(me, el)),
                    ..Bytes/slice(b, m, e, @me, @el)],
                Bytes/slice(b, s, e, @Nat/Le/trans(sm, me), @el)) =
            Eq/refl();
        let empty(b : Bytes, i : Nat, il : Nat/Le(i, Bytes/len(b)))
            -> Eq(Bytes/slice(b, i, i, @Nat/Le/refl(i), @il), x[]) = Eq/refl();
        let full(b : Bytes)
            -> Eq(
                Bytes/slice(b, 0, Bytes/len(b), @True/qed(), @Nat/Le/refl(Bytes/len(b))),
                b) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_slice_window_seam_mismatch_is_rejected() {
    // The dual: two windows whose seam does not meet — `slice(b, s, m)` then `slice(b, n, e)` with `m` and `n` distinct — must NOT fuse, so the concat is not convertible to `slice(b, s, e)` and the `refl` is rejected. Guards the fusion's seam check against gluing non-adjacent slices of one base.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Nat};
        let bad(b : Bytes, s : Nat, m : Nat, n : Nat, e : Nat,
                sm : Nat/Le(s, m), ml : Nat/Le(m, Bytes/len(b)),
                ne : Nat/Le(n, e), el : Nat/Le(e, Bytes/len(b)), se : Nat/Le(s, e))
            -> Eq(
                x[
                    ..Bytes/slice(b, s, m, @sm, @ml),
                    ..Bytes/slice(b, n, e, @ne, @el)],
                Bytes/slice(b, s, e, @se, @el)) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn list_slice_is_a_monoid_citizen() {
    // The `List` mirror of `bin_slice_is_a_monoid_citizen`: `List/slice` now rides the free-monoid spine as a measured `Window` (`core::spine`), so `split` fuses two adjacent windows of one base across their seam — the convert-level capability — while `empty` and `full` exercise the reduce-level slice identities.
    let source = r#"
        use /std/{Handle, Str, Eq, List, Nat, True};
        let split(@T : Type, a : List(T), s : Nat, m : Nat, e : Nat,
                  sm : Nat/Le(s, m), me : Nat/Le(m, e), el : Nat/Le(e, List/len(a)))
            -> Eq(
                [
                    ..List/slice(@T, a, s, m, @sm, @Nat/Le/trans(me, el)),
                    ..List/slice(@T, a, m, e, @me, @el)],
                List/slice(@T, a, s, e, @Nat/Le/trans(sm, me), @el)) =
            Eq/refl();
        let empty(@T : Type, a : List(T), i : Nat, il : Nat/Le(i, List/len(a)))
            -> Eq(List/slice(@T, a, i, i, @Nat/Le/refl(i), @il), []) = Eq/refl();
        let full(@T : Type, a : List(T))
            -> Eq(
                List/slice(@T, a, 0, List/len(a), @True/qed(), @Nat/Le/refl(List/len(a))),
                a) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn list_append_is_concat_with_a_single() {
    // A trailing element entry lowers to an append and a spread of a singleton to a concatenation — two different terms, so this is the peel doing the work rather than the two sides being spelled alike. An append rides the spine as `base ++ [e]` (`core::spine`), which is what converts them, for a symbolic base and element that `reduce` cannot fold.
    let source = r#"
        use /std/{Handle, Str, Eq, List};
        let law(@T : Type, xs : List(T), y : T)
            -> Eq([..xs, y], [..xs, ..[y]]) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_append_is_concat_with_a_single_byte() {
    // The `Bytes` twin of `list_append_is_concat_with_a_single`: an atom splice rides the spine as `base ++ b`, so it converts to the concatenation-with-a-one-byte form by `refl` even for a symbolic base and a symbolic byte.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes};
        let law(xs : Bytes, y : Byte)
            -> Eq(x[..xs, ..(x[y])], x[..xs, y]) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn list_slice_window_seam_mismatch_is_rejected() {
    // The dual of `bin_slice_window_seam_mismatch_is_rejected`: two `List` windows whose seam does not meet must NOT fuse, so the concat is not convertible to the single slice and the `refl` is rejected.
    let source = r#"
        use /std/{Handle, Str, Eq, List, Nat};
        let bad(@T : Type, a : List(T), s : Nat, m : Nat, n : Nat, e : Nat,
                sm : Nat/Le(s, m), ml : Nat/Le(m, List/len(a)),
                ne : Nat/Le(n, e), el : Nat/Le(e, List/len(a)), se : Nat/Le(s, e))
            -> Eq(
                [
                    ..List/slice(@T, a, s, m, @sm, @ml),
                    ..List/slice(@T, a, n, e, @ne, @el)],
                List/slice(@T, a, s, e, @se, @el)) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn bin_len_reduces_across_a_cons_spine() {
    // The `Bytes/len` partner of the slice/get cons-reduction: length distributes over concatenation and an `append` is one byte longer, so a cons spine's length reduces to a `succ` over the tail's — `len(cons(h, t)) = succ(len t)`. `Nat/lt` then discharges the codepoint walk's bounds guard on that spine: `lt(0, succ _) = true` (the left literal is below the successor floor) and `lt(succ _, 0) = false` (the left is at least the floor). All by `refl` for a SYMBOLIC tail, the pair that lets `advance_codepoint` step a symbolic cons.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes, Nat};
        let len(h : Byte, t : Bytes)
            -> Eq(Bytes/len(x[h, ..t]), Nat/add(1, Bytes/len(t))) = Eq/refl();
        let guard(h : Byte, t : Bytes)
            -> Eq(Nat/lt(0, Bytes/len(x[h, ..t])), true) = Eq/refl();
        let floor(h : Byte, t : Bytes)
            -> Eq(Nat/lt(Bytes/len(x[h, ..t]), 0), false) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn packed_atom_splice_builds_the_written_sequence() {
    // `\.` splices one generator into a packed literal, between literal runs and adjacent to another atom. `i` and `bang` are symbolic, so the run is genuinely spliced rather than folded at parse time.
    let source = r#"
        use /std/{Handle, Str, Byte, Bytes};
        let i : Byte = 0x69;
        let bang : Byte = 0x21;
        let _ = Handle/write(Handle/stdout, x[0x48, i, bang])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"Hi!");
}

#[test]
fn packed_atom_splices_are_the_cons_and_append_spellings() {
    // An atom leading a spread lowers to the cons spelling `curios_elab`'s packed-match refinement builds — the singleton `append(x[], h)` concatenated with the tail — so a literal written that way is the cons spine, not merely equal to one. Stated for SYMBOLIC operands through `len` and `get`, the two observations that reduce across that spine, so nothing here is reached by folding literals.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes, Bool, Bits, Nat, Option};
        let cons_len(h : Byte, t : Bytes)
            -> Eq(Bytes/len(x[h, ..t]), Nat/add(1, Bytes/len(t))) = Eq/refl();
        let cons_head(h : Byte, t : Bytes)
            -> Eq(Bytes/get(x[h, ..t], 0), Option/some(h)) = Eq/refl();
        let bits_len(h : Bool, t : Bits)
            -> Eq(Bits/len(b[h, ..t]), Nat/add(1, Bits/len(t))) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn an_append_over_a_nonempty_base_still_decodes_its_first_atom() {
    // `peel_front` (`core::free_monoid`) recognised an append only over the EMPTY base, so `append(x[0x48], b)` — what `x[0x48, b]` lowers to — went opaque and no eliminator over it could reduce, while `core::spine`'s two-value peel had always decoded the same term. The `BinConcat` arm beside it already peeled its first operand and rejoined the residual; the append arm now does the same. `get` at index 0 is the sharp probe: it reduces only where the leading generator is exposed, and every appended atom here is SYMBOLIC, so nothing is literal folding. `chained` is the recursive case — adjacent atoms lower to `append(append(...))`, whose first generator sits two bases down.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes, Bool, Bits, Option};
        let lead : Byte = 0x48;
        let byte_head(b : Byte) -> Eq(Bytes/get(x[0x48, b], 0), Option/some(lead)) = Eq/refl();
        let bit_head(b : Bool) -> Eq(Bits/get(b[1, b], 0), Option/some(true)) = Eq/refl();
        let chained(a : Byte, b : Byte) -> Eq(Bytes/get(x[0x48, a, b], 0), Option/some(lead)) = Eq/refl();
        let chained_len(a : Byte, b : Byte) -> Eq(Bytes/len(x[a, b]), 2) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn nat_sub_peels_a_successor_spine() {
    // The subtraction twin of `NatAdd`'s successor peeling: `(s + inner) - k` reduces to `(s - k) + inner` when the literal `k` is within the successor floor `s`, even for a SYMBOLIC `inner` that `reduce` cannot fold. This is what turns the `succ e - 1` bounds the cons slice rule emits back into `e`, so a slice over a symbolic cons keeps reducing. `peel` thins the floor; `to_zero` exhausts it, leaving the bare tail.
    let source = r#"
        use /std/{Handle, Str, Eq, Nat};
        let peel(n : Nat) -> Eq(Nat/sub(Nat/add(3, n), 1), Nat/add(2, n)) = Eq/refl();
        let to_zero(n : Nat) -> Eq(Nat/sub(Nat/add(1, n), 1), n) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn list_concat_is_a_free_monoid() {
    // `peel_arr` (core::spine) makes `List` a free monoid on its elements, the twin of `bin_concat_is_a_free_monoid`: concatenation associates, the empty array `[]` is its identity, and a literal run re-segments freely — all by `refl` for SYMBOLIC arrays (and elements), which `reduce` cannot fold. `convert` peels the two `ListConcat`s to a common normal form. A spread whose operand is itself a list literal is what keeps the two nestings apart, exactly as the parenthesized packed operand does for `Bytes`.
    let source = r#"
        use /std/{Handle, Str, Eq, List};
        let assoc(@T : Type, a : List(T), b : List(T), c : List(T))
            -> Eq([..a, ..[..b, ..c]], [..[..a, ..b], ..c]) =
            Eq/refl();
        let left_id(@T : Type, a : List(T)) -> Eq([..[], ..a], a) = Eq/refl();
        let right_id(@T : Type, a : List(T)) -> Eq([..a, ..[]], a) = Eq/refl();
        let resegment(@T : Type, a : T, b : T, c : List(T))
            -> Eq([a, b, ..c], [a, ..[b, ..c]]) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn list_concat_length_clash_is_rejected() {
    // Unlike `Bytes`, a `List` element disagreement is NOT a clash (elements are terms that may be convertible) — but a literal *length* mismatch still is: `[x, y]` and `[x]` peel their shared head and leave one side longer, a definite `Clash`, so the `refl` is rejected. Exercises `peel_arr`'s clash against the empty identity (the element-mismatch case instead defers to the structural arm, kept sound by `Stuck` fall-through).
    let source = r#"
        use /std/{Handle, Str, Eq, List};
        let bad(@T : Type, x : T, y : T) -> Eq([x, y], [x]) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn empty_bin_literal_is_the_empty_sequence() {
    // The empty `Bytes` literal concatenated with a value is the identity.
    assert_eq!(
        run(r#"
let _ = std/Handle/write(std/Handle/stdout, x[../std/Str/to_bytes("ok")])!;
/std/Io/pure(())
"#),
        b"ok"
    );
}

#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : std/Nat) -> Type =
            match n : (_) => Type
            | 0 => {}
            | pred + 1; ih => { T, ih }
            end;

        let cons(T : Type, n : std/Nat, x : T, xs : Vec(T, n)) -> Vec(T, std/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : std/Nat, xs : Vec(T, std/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(std/Nat, 1) = cons(std/Nat, 0, 42, ());
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(head(std/Nat, 0, v))))!;
        /std/Io/pure(())
    "#;

    assert_eq!(run(source), b"42");
}

#[test]
fn indexed_vec_append_executes() {
    // Rung A of the indexed-inductive ladder, *executed*: `append`'s motive binds the length index (`(v : Vec(T, k)) => Vec(T, Nat/add(k, m))`), the `cons` arm meets it through the definitional successor-peeling of `Nat/add`, and the implicit index arguments of the recursive call are solved to the arm's *first* binder. Running (not just compiling) guards the zonk realignment of multi-binder arm scopes: with the in-group order flipped, the solved indices silently referenced the wrong binder and the program trapped at runtime.
    let source = r#"
        use /std/{Nat, Bytes, Handle};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        rec append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, Nat/add(n, m)) =
            match v : (k, v) => Vec(T, Nat/add(k, m))
            | nil() => w
            | cons(@j, x, xs) => Vec/cons(x, append(xs, w))
            end;
        rec total(@n : Nat, v : Vec(Nat, n), acc : Nat) -> Nat =
            match v : (_, _) => Nat
            | nil() => acc
            | cons(@m, x, xs) => total(xs, Nat/add(acc, x))
            end;
        let a : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let b : Vec(Nat, 1) = Vec/cons(4, Vec/nil());
        let c : Vec(Nat, 3) = append(a, b);
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(total(c, 0))))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"7");
}

#[test]
fn list_fold_sums_elements() {
    let source = r#"
        use /std/{Handle, Str, Nat, List};
        let xs : List(Nat) = [10, 20, 30];
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(List/fold(xs, 0, (e, acc) => Nat/add(acc, e)))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"60");
}

#[test]
fn list_spread_concats_segments() {
    // `[1, ..xs, 4]` splices `xs` between the literal runs. The non-commutative foldr probe (see `list_match_is_a_foldr`) distinguishes the spliced order `[1, 2, 3, 4]` from any permutation or grouping artifact.
    let source = r#"
        use /std/{Handle, Str, Nat, List};
        let xs : List(Nat) = [2, 3];
        let ys : List(Nat) = [1, ..xs, 4];
        let digits : Nat =
            match ys : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn list_spread_identity_and_multi() {
    // `[..xs]` is an identity copy (reduction collapses the lone operand), and spreads repeat: `[..ys, ..ys]` doubles the sequence in written order.
    let source = r#"
        use /std/{Handle, Str, Nat, List};
        let xs : List(Nat) = [2, 3];
        let ys : List(Nat) = [..xs];
        let zs : List(Nat) = [..ys, ..ys];
        let digits : Nat =
            match zs : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"3232");
}

#[test]
fn list_spread_borrows_expected_element_type() {
    // The `ListConcat` bidirectionality case in `elaborate_intrinsic`: checking `[1, ..xs]` against `List(Int)` must solve the lowering-minted element slot from the expected type BEFORE the literal chunk elaborates, so the unsigned `1` lands at `Int`. Without the borrow, `1` would default-solve the slot to `Nat` and this program would be rejected.
    let source = r#"
        use /std/{Handle, Str, Nat, Int, List};
        let xs : List(Int) = [-1, +2];
        let ys : List(Int) = [1, ..xs];
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(List/len(ys))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn list_spread_of_non_list_is_rejected() {
    // A spread operand must itself be a list of the element type — `..2` in a `List(Nat)` literal is an ordinary type mismatch (Nat vs List(Nat)).
    let source = r#"
        use /std/{Handle, Str, Nat, List};
        let bad : List(Nat) = [1, ..2];
        let _ = Handle/write(Handle/stdout, Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn list_spread_element_type_clash_is_rejected() {
    let source = r#"
        use /std/{Handle, Str, Nat, List};
        let ss : List(Str) = ["a"];
        let bad : List(Nat) = [..ss];
        let _ = Handle/write(Handle/stdout, Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn list_spread_operand_hoists_bangs() {
    // A bang inside a spread operand hoists into the enclosing region exactly like one inside a plain element — the literal is collected, not sealed.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Nat, List};
        let prog : Async({}) =
            let ys : List(Nat) = [1, ..Async/pure([2, 3])!, 4];
            let digits : Nat =
                match ys : (_) => Nat
                | [] => 0
                | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
                end;
            let wrote = Async/lift(Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits))))!;
            Async/pure(());
        Async/run(prog)
        "#),
        b"4321"
    );
}

#[test]
fn bin_spread_concats_segments() {
    // `x[0x01, ..b, 0x04]` splices the bytes of `b` between the literal runs, and the glued suffix chain admits a call operand (`\..Bytes/slice(...)`).
    let source = r#"
        use /std/{Handle, Nat, Bytes};
        let b : Bytes = x[0x02, 0x03];
        let _ = Handle/write(Handle/stdout, x[0x01, ..b, 0x04, ..Bytes/slice(b, 1, 2)])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"\x01\x02\x03\x04\x03");
}

#[test]
fn bin_spread_identity_and_multi() {
    let source = r#"
        use /std/{Handle, Bytes};
        let b : Bytes = x[0x48, 0x65];
        let c : Bytes = x[..b];
        let _ = Handle/write(Handle/stdout, x[..c, ..c])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"HeHe");
}

#[test]
fn bin_spread_of_non_bin_is_rejected() {
    // A spread operand must itself be a `Bytes` — a list is an ordinary type mismatch.
    let source = r#"
        use /std/{Handle, Str, Nat, List, Bytes};
        let xs : List(Nat) = [1, 2];
        let bad : Bytes = x[0x00, ..xs];
        let _ = Handle/write(Handle/stdout, Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn bin_spread_operand_hoists_bangs() {
    // The `Bytes` sibling of `list_spread_operand_hoists_bangs`, through the dedicated `Intrinsic::Bytes` collect arm — the glued `!` binds to the operand.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Bytes};
        let prog : Async({}) =
            let out : Bytes = x[0x3e, ..Async/pure(x[0x68, 0x69])!, 0x3c];
            let wrote = Async/lift(Handle/write(Handle/stdout, out))!;
            Async/pure(());
        Async/run(prog)
        "#),
        b">hi<"
    );
}

#[test]
fn bin_fold_sums_bytes() {
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes};
        let b = x[0x0a, 0x14, 0x1e];
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Bytes/fold(b, 0, (byte, acc) => Nat/add(acc, Byte/to_nat(byte))))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"60");
}

// An empty match is a vacuous elimination: it never inspects its scrutinee. A `False` is a `Prop`, so it erases (sort-driven) — a contradiction may therefore discharge into a *relevant* result, both directly (`match c : (_) => A end`) and through a discharging definition (`let absurd(@A : Type, c : False) -> A = match c end`). This is what lets an impossible runtime branch be closed off by an erased witness, the crux of the UTF-8 decode certification.
