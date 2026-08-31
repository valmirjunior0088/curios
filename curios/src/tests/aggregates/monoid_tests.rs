//! `Bin` and `List` as free monoids: a value does not depend on how its run is grouped, and a window must meet at its seam.

use crate::tests::{error, run};

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
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Option/unwrap_or(List/try_get(xs, 0), 0), Option/unwrap_or(List/try_get(xs, 2), 0)))))!;
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
            : Eq(Bytes/slice(x[..x[0x30, 0x31], ..x[0x32, 0x33, 0x34]], 1, 3), x[0x31, 0x32, 0x33]) =
            Eq/refl();
        let third : Byte = 0x33;
        let split_index
            : Eq(Bytes/try_get(x[..x[0x30, 0x31], ..x[0x32, 0x33, 0x34]], 3), Option/some(third)) =
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
    // `Bytes/slice` rides the free-monoid spine (`core::spine`) as a measured `Window` — a chunk carrying its own length, whose contents are symbolic — so the slice algebra holds up to *definitional* equality, provable by `refl` for SYMBOLIC operands that `reduce` cannot fold. `split` fuses two adjacent windows of one base across their shared seam; `empty` drops a zero-length window (the monoid identity); `full` collapses `slice(b, 0, len b)` to its base (the `reduce` partner of the spine's window-collapse). Each declared type forces `convert` to peel the windows to a common normal form; without the peel these are stuck, distinct terms and `refl` would not check.
    //
    // `split` is where the window's whole bound discipline is visible at once. It takes **one** hypothesis where the `(start, end)` window took three, because a count cannot spell a reversed range; and the fused window is passed `@total` — *the second window's own proof, unchanged*. That only type-checks because `(s + l1) + l2` and `s + (l1 + l2)` are convertible, which is the equation `peel_nat_terms` decides. The first window's bound is the one thing actually derived, and only to weaken the total.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Nat};
        let split(b : Bytes, s : Nat, l1 : Nat, l2 : Nat,
                  total : Nat/Le((s + l1) + l2, Bytes/len(b)))
            -> Eq(
                x[
                    ..Bytes/slice(
                        b, s, l1,
                        @Nat/Le/trans(Nat/Le/of_ind(Nat/Le/Ind/add_r(s + l1, l2)), total)),
                    ..Bytes/slice(b, s + l1, l2, @total)],
                Bytes/slice(b, s, l1 + l2, @total)) =
            Eq/refl();
        let empty(b : Bytes, i : Nat, il : Nat/Le(i, Bytes/len(b)))
            -> Eq(Bytes/slice(b, i, 0, @il), x[]) = Eq/refl();
        let full(b : Bytes)
            -> Eq(Bytes/slice(b, 0, Bytes/len(b), @Nat/Le/refl(Bytes/len(b))), b) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_slice_window_seam_mismatch_is_rejected() {
    // The dual: two windows whose seam does not meet — `slice(b, s, l1)` then `slice(b, o, l2)` with `o` unrelated to `s + l1` — must NOT fuse, so the concat is not convertible to `slice(b, s, l1 + l2)` and the `refl` is rejected. Guards the fusion's seam check, which under `(start, length)` is an *arithmetic* test (`o = s + l1`) decided by the `Nat` peel rather than a shared term compared syntactically, against gluing non-adjacent slices of one base.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Nat};
        let bad(b : Bytes, s : Nat, l1 : Nat, o : Nat, l2 : Nat,
                w1 : Nat/Le(s + l1, Bytes/len(b)), w2 : Nat/Le(o + l2, Bytes/len(b)),
                w3 : Nat/Le(s + (l1 + l2), Bytes/len(b)))
            -> Eq(
                x[..Bytes/slice(b, s, l1, @w1), ..Bytes/slice(b, o, l2, @w2)],
                Bytes/slice(b, s, l1 + l2, @w3)) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn list_slice_is_a_monoid_citizen() {
    // The `List` mirror of `bin_slice_is_a_monoid_citizen`, count-based like it: `split` takes one hypothesis and hands the fused window the second window's own proof, `empty` drops a zero-length window, and `full` collapses the whole one.
    let source = r#"
        use /std/{Handle, Str, Eq, List, Nat};
        let split(@T : Type, a : List(T), s : Nat, l1 : Nat, l2 : Nat,
                  total : Nat/Le((s + l1) + l2, List/len(a)))
            -> Eq(
                [
                    ..List/slice(
                        @T, a, s, l1,
                        @Nat/Le/trans(Nat/Le/of_ind(Nat/Le/Ind/add_r(s + l1, l2)), total)),
                    ..List/slice(@T, a, s + l1, l2, @total)],
                List/slice(@T, a, s, l1 + l2, @total)) =
            Eq/refl();
        let empty(@T : Type, a : List(T), i : Nat, il : Nat/Le(i, List/len(a)))
            -> Eq(List/slice(@T, a, i, 0, @il), []) = Eq/refl();
        let full(@T : Type, a : List(T))
            -> Eq(List/slice(@T, a, 0, List/len(a), @Nat/Le/refl(List/len(a))), a) =
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
fn an_append_over_a_nonempty_base_still_decodes_its_first_atom() {
    // `peel_front` (`core::free_monoid`) recognised an append only over the EMPTY base, so `append(x[0x48], b)` — what `x[0x48, b]` lowers to — went opaque and no eliminator over it could reduce, while `core::spine`'s two-value peel had always decoded the same term. The `BinConcat` arm beside it already peeled its first operand and rejoined the residual; the append arm now does the same. `get` at index 0 is the sharp probe: it reduces only where the leading generator is exposed, and every appended atom here is SYMBOLIC, so nothing is literal folding. `chained` is the recursive case — adjacent atoms lower to `append(append(...))`, whose first generator sits two bases down.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes, Bool, Bits, Option};
        let lead : Byte = 0x48;
        let byte_head(b : Byte) -> Eq(Bytes/try_get(x[0x48, b], 0), Option/some(lead)) = Eq/refl();
        let bit_head(b : Bool) -> Eq(Bits/try_get(b[1, b], 0), Option/some(true)) = Eq/refl();
        let chained(a : Byte, b : Byte) -> Eq(Bytes/try_get(x[0x48, a, b], 0), Option/some(lead)) = Eq/refl();
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
