use {super::run, curios_runtime::MockHost};

#[test]
fn lst_match_is_a_foldr() {
    // Native `Lst` induction (slice 1): the `| [] | (h, t), ih` eliminator, erased by desugaring to `Nat`-induction on the length and reusing the loop. `f(h, ih) = ih * 10 + h` is non-commutative, so the result distinguishes a structural `foldr` (head is the *first* element, ih is the fold of the tail) from a reversed walk: `[1,2,3,4]` folds to `4321`, not `1234`.
    let source = r#"
        use /std/{Handle, Str, Nat, Lst};
        let xs : Lst(Nat) = [1, 2, 3, 4];
        let digits : Nat =
            match xs : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn lst_map_fills_every_slot() {
    // `Lst/map` erases to a single O(n) fill loop (`emit_map`): size the result from `src.len`, allocate once, then write `f(src[i])` into slot `i` via an inline closure `call_ref`. A non-identity `f` (`+1`) over `[10, 20, 30]` must fill *every* slot, not just one: `get(_, 0) + get(_, 2)` = 11 + 31 = 42.
    let source = r#"
        use /std/{Handle, Str, Nat, Lst, Option};
        let xs : Lst(Nat) = Lst/map([10, 20, 30], (n) => Nat/add(n, 1));
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Nat/add(Option/unwrap_or(Lst/get(xs, 0), 0), Option/unwrap_or(Lst/get(xs, 2), 0)))))
        "#;
    assert_eq!(run(source), b"42");
}

#[test]
fn lst_map_distributes_over_cons() {
    // The eliminator rule that lets `Lst/map` stand in for a structural `foldr` in proofs: for a SYMBOLIC tail `t`, `map f (x :: t) ≡ f x :: map f t` *definitionally*. `refl` checks only because `reduce` distributes the map over the `concat` spine and maps the singleton literal — the same peel the native `Lst` eliminator does, so it reduces under induction without unfolding a symbolic array.
    let source = r#"
        use /std/{Handle, Str, Eq, Nat, Lst};
        let step(f : (Nat) -> Nat, x : Nat, t : Lst(Nat))
            -> Eq(Lst/map([x, ..t], f), [f(x), ..Lst/map(t, f)]) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_match_is_a_foldr() {
    // Native `Bytes` induction (slice 2): the `| x[] | (h, t), ih` eliminator, erased exactly like `Lst` — `Nat`-induction on the byte length, reusing the loop. The leading byte `h` is reflected as a `Nat`. Same non-commutative `foldr` probe as `lst_match_is_a_foldr`: the bytes `x[\01, \02, \03, \04]` fold to `4321`, not `1234`, pinning head = first byte and ih = fold of the tail.
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes};
        let bytes : Bytes = x[\01, \02, \03, \04];
        let digits : Nat =
            match bytes : (_) => Nat
            | x[] => 0
            | x[h, ..t]; ih => Nat/add(Nat/mul(ih, 10), Byte/to_nat(h))
            end;
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))
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
            -> Eq(x[\01, \02, ..x], x[\01, ..(x[\02, ..x])]) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_concat_leading_byte_clash_is_rejected() {
    // The dual: a leading-byte disagreement under a shared symbolic tail is a definite `Clash`, so `x[\01] ++ x` and `x[\02] ++ x` are never convertible and the `refl` is rejected. Guards `peel_bin` against deciding unequal values equal.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes};
        let bad(x : Bytes) -> Eq(x[\01, ..x], x[\02, ..x]) = Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
}

#[test]
fn bin_slice_is_a_monoid_citizen() {
    // `Bytes/slice` rides the free-monoid spine (`core::spine`) as a measured `Window` — a length-`hi - lo` chunk whose contents are symbolic — so the slice algebra holds up to *definitional* equality, provable by `refl` for SYMBOLIC operands that `reduce` cannot fold. `split` fuses two adjacent windows of one base across their shared seam; `empty` drops a zero-width window (the monoid identity); `full` collapses `slice(b, 0, len b)` to its base (the `reduce` partner of the spine's window-collapse). Each declared type forces `convert` to peel the windows to a common normal form; without the peel these are stuck, distinct terms and `refl` would not check.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Nat};
        let split(b : Bytes, s : Nat, m : Nat, e : Nat)
            -> Eq(x[..Bytes/slice(b, s, m), ..Bytes/slice(b, m, e)], Bytes/slice(b, s, e)) =
            Eq/refl();
        let empty(b : Bytes, i : Nat) -> Eq(Bytes/slice(b, i, i), x[]) = Eq/refl();
        let full(b : Bytes) -> Eq(Bytes/slice(b, 0, Bytes/len(b)), b) = Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_slice_window_seam_mismatch_is_rejected() {
    // The dual: two windows whose seam does not meet — `slice(b, s, m)` then `slice(b, n, e)` with `m` and `n` distinct — must NOT fuse, so the concat is not convertible to `slice(b, s, e)` and the `refl` is rejected. Guards the fusion's seam check against gluing non-adjacent slices of one base.
    let source = r#"
        use /std/{Handle, Str, Eq, Bytes, Nat};
        let bad(b : Bytes, s : Nat, m : Nat, n : Nat, e : Nat)
            -> Eq(x[..Bytes/slice(b, s, m), ..Bytes/slice(b, n, e)], Bytes/slice(b, s, e)) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
}

#[test]
fn lst_slice_is_a_monoid_citizen() {
    // The `Lst` mirror of `bin_slice_is_a_monoid_citizen`: `Lst/slice` now rides the free-monoid spine as a measured `Window` (`core::spine`), so `split` fuses two adjacent windows of one base across their seam — the convert-level capability — while `empty` and `full` exercise the reduce-level slice identities.
    let source = r#"
        use /std/{Handle, Str, Eq, Lst, Nat};
        let split(@T : Type, a : Lst(T), s : Nat, m : Nat, e : Nat)
            -> Eq([..Lst/slice(a, s, m), ..Lst/slice(a, m, e)], Lst/slice(a, s, e)) =
            Eq/refl();
        let empty(@T : Type, a : Lst(T), i : Nat) -> Eq(Lst/slice(a, i, i), []) = Eq/refl();
        let full(@T : Type, a : Lst(T)) -> Eq(Lst/slice(a, 0, Lst/len(a)), a) = Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_append_is_concat_with_a_single() {
    // A trailing element entry lowers to an append and a spread of a singleton to a concatenation — two different terms, so this is the peel doing the work rather than the two sides being spelled alike. An append rides the spine as `base ++ [e]` (`core::spine`), which is what converts them, for a symbolic base and element that `reduce` cannot fold.
    let source = r#"
        use /std/{Handle, Str, Eq, Lst};
        let law(@T : Type, xs : Lst(T), y : T)
            -> Eq([..xs, y], [..xs, ..[y]]) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_append_is_concat_with_a_single_byte() {
    // The `Bytes` twin of `lst_append_is_concat_with_a_single`: an atom splice rides the spine as `base ++ b`, so it converts to the concatenation-with-a-one-byte form by `refl` even for a symbolic base and a symbolic byte.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes};
        let law(xs : Bytes, y : Byte)
            -> Eq(x[..xs, ..(x[y])], x[..xs, y]) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_slice_window_seam_mismatch_is_rejected() {
    // The dual of `bin_slice_window_seam_mismatch_is_rejected`: two `Lst` windows whose seam does not meet must NOT fuse, so the concat is not convertible to the single slice and the `refl` is rejected.
    let source = r#"
        use /std/{Handle, Str, Eq, Lst, Nat};
        let bad(@T : Type, a : Lst(T), s : Nat, m : Nat, n : Nat, e : Nat)
            -> Eq([..Lst/slice(a, s, m), ..Lst/slice(a, n, e)], Lst/slice(a, s, e)) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
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
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
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
        Handle/write(Handle/stdout, x[\48, i, bang])
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
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn an_append_over_a_nonempty_base_still_decodes_its_first_atom() {
    // `peel_front` (`core::free_monoid`) recognised an append only over the EMPTY base, so `append(x[\48], b)` — what `x[\48, b]` lowers to — went opaque and no eliminator over it could reduce, while `core::spine`'s two-value peel had always decoded the same term. The `BinConcat` arm beside it already peeled its first operand and rejoined the residual; the append arm now does the same. `get` at index 0 is the sharp probe: it reduces only where the leading generator is exposed, and every appended atom here is SYMBOLIC, so nothing is literal folding. `chained` is the recursive case — adjacent atoms lower to `append(append(...))`, whose first generator sits two bases down.
    let source = r#"
        use /std/{Handle, Str, Eq, Byte, Bytes, Bool, Bits, Option};
        let lead : Byte = 0x48;
        let byte_head(b : Byte) -> Eq(Bytes/get(x[\48, b], 0), Option/some(lead)) = Eq/refl();
        let bit_head(b : Bool) -> Eq(Bits/get(b[\1, b], 0), Option/some(true)) = Eq/refl();
        let chained(a : Byte, b : Byte) -> Eq(Bytes/get(x[\48, a, b], 0), Option/some(lead)) = Eq/refl();
        let chained_len(a : Byte, b : Byte) -> Eq(Bytes/len(x[a, b]), 2) = Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
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
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_concat_is_a_free_monoid() {
    // `peel_arr` (core::spine) makes `Lst` a free monoid on its elements, the twin of `bin_concat_is_a_free_monoid`: concatenation associates, the empty array `[]` is its identity, and a literal run re-segments freely — all by `refl` for SYMBOLIC arrays (and elements), which `reduce` cannot fold. `convert` peels the two `LstConcat`s to a common normal form. A spread whose operand is itself a list literal is what keeps the two nestings apart, exactly as the parenthesized packed operand does for `Bytes`.
    let source = r#"
        use /std/{Handle, Str, Eq, Lst};
        let assoc(@T : Type, a : Lst(T), b : Lst(T), c : Lst(T))
            -> Eq([..a, ..[..b, ..c]], [..[..a, ..b], ..c]) =
            Eq/refl();
        let left_id(@T : Type, a : Lst(T)) -> Eq([..[], ..a], a) = Eq/refl();
        let right_id(@T : Type, a : Lst(T)) -> Eq([..a, ..[]], a) = Eq/refl();
        let resegment(@T : Type, a : T, b : T, c : Lst(T))
            -> Eq([a, b, ..c], [a, ..[b, ..c]]) =
            Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_concat_length_clash_is_rejected() {
    // Unlike `Bytes`, an `Lst` element disagreement is NOT a clash (elements are terms that may be convertible) — but a literal *length* mismatch still is: `[x, y]` and `[x]` peel their shared head and leave one side longer, a definite `Clash`, so the `refl` is rejected. Exercises `peel_arr`'s clash against the empty identity (the element-mismatch case instead defers to the structural arm, kept sound by `Stuck` fall-through).
    let source = r#"
        use /std/{Handle, Str, Eq, Lst};
        let bad(@T : Type, x : T, y : T) -> Eq([x, y], [x]) = Eq/refl();
        Handle/write(Handle/stdout, Str/to_bytes("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
}

#[test]
fn empty_bin_literal_is_the_empty_sequence() {
    // The empty `Bytes` literal concatenated with a value is the identity.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        r#"std/Handle/write(std/Handle/stdout, x[../std/Str/to_bytes("ok")])"#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
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
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(head(std/Nat, 0, v))))
    "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
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
        Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(total(c, 0))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn lst_fold_sums_elements() {
    let source = r#"
        use /std/{Handle, Str, Nat, Lst};
        let xs : Lst(Nat) = [10, 20, 30];
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Lst/fold(xs, 0, (e, acc) => Nat/add(acc, e)))))
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"60");
}

#[test]
fn lst_spread_concats_segments() {
    // `[1, ..xs, 4]` splices `xs` between the literal runs. The non-commutative foldr probe (see `lst_match_is_a_foldr`) distinguishes the spliced order `[1, 2, 3, 4]` from any permutation or grouping artifact.
    let source = r#"
        use /std/{Handle, Str, Nat, Lst};
        let xs : Lst(Nat) = [2, 3];
        let ys : Lst(Nat) = [1, ..xs, 4];
        let digits : Nat =
            match ys : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn lst_spread_identity_and_multi() {
    // `[..xs]` is an identity copy (reduction collapses the lone operand), and spreads repeat: `[..ys, ..ys]` doubles the sequence in written order.
    let source = r#"
        use /std/{Handle, Str, Nat, Lst};
        let xs : Lst(Nat) = [2, 3];
        let ys : Lst(Nat) = [..xs];
        let zs : Lst(Nat) = [..ys, ..ys];
        let digits : Nat =
            match zs : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"3232");
}

#[test]
fn lst_spread_borrows_expected_element_type() {
    // The `LstConcat` bidirectionality case in `elaborate_prim`: checking `[1, ..xs]` against `Lst(Int)` must solve the lowering-minted element slot from the expected type BEFORE the literal chunk elaborates, so the unsigned `1` lands at `Int`. Without the borrow, `1` would default-solve the slot to `Nat` and this program would be rejected.
    let source = r#"
        use /std/{Handle, Str, Nat, Int, Lst};
        let xs : Lst(Int) = [-1, +2];
        let ys : Lst(Int) = [1, ..xs];
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Lst/len(ys))))
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn lst_spread_of_non_list_is_rejected() {
    // A spread operand must itself be a list of the element type — `..2` in a `Lst(Nat)` literal is an ordinary type mismatch (Nat vs Lst(Nat)).
    let source = r#"
        use /std/{Handle, Str, Nat, Lst};
        let bad : Lst(Nat) = [1, ..2];
        Handle/write(Handle/stdout, Str/to_bytes("unreachable"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
}

#[test]
fn lst_spread_element_type_clash_is_rejected() {
    let source = r#"
        use /std/{Handle, Str, Nat, Lst};
        let ss : Lst(Str) = ["a"];
        let bad : Lst(Nat) = [..ss];
        Handle/write(Handle/stdout, Str/to_bytes("unreachable"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
}

#[test]
fn lst_spread_operand_hoists_bangs() {
    // A bang inside a spread operand hoists into the enclosing region exactly like one inside a plain element — the literal is collected, not sealed.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        r#"
        use /std/{Async, Handle, Str, Nat, Lst};
        let prog : Async({}) =
            let ys : Lst(Nat) = [1, ..Async/pure([2, 3])!, 4];
            let digits : Nat =
                match ys : (_) => Nat
                | [] => 0
                | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
                end;
            let wrote = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(digits)));
            Async/pure(());
        Async/run(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"4321");
}

#[test]
fn bin_spread_concats_segments() {
    // `x[\01, ..b, \04]` splices the bytes of `b` between the literal runs, and the glued suffix chain admits a call operand (`\..Bytes/slice(...)`).
    let source = r#"
        use /std/{Handle, Nat, Bytes};
        let b : Bytes = x[\02, \03];
        Handle/write(Handle/stdout, x[\01, ..b, \04, ..Bytes/slice(b, 1, 2)])
        "#;
    assert_eq!(run(source), b"\x01\x02\x03\x04\x03");
}

#[test]
fn bin_spread_identity_and_multi() {
    let source = r#"
        use /std/{Handle, Bytes};
        let b : Bytes = x[\48, \65];
        let c : Bytes = x[..b];
        Handle/write(Handle/stdout, x[..c, ..c])
        "#;
    assert_eq!(run(source), b"HeHe");
}

#[test]
fn bin_spread_of_non_bin_is_rejected() {
    // A spread operand must itself be a `Bytes` — a list is an ordinary type mismatch.
    let source = r#"
        use /std/{Handle, Str, Nat, Lst, Bytes};
        let xs : Lst(Nat) = [1, 2];
        let bad : Bytes = x[\00, ..xs];
        Handle/write(Handle/stdout, Str/to_bytes("unreachable"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(source, system).is_err());
}

#[test]
fn bin_spread_operand_hoists_bangs() {
    // The `Bytes` sibling of `lst_spread_operand_hoists_bangs`, through the dedicated `Prim::Bytes` collect arm — the glued `!` binds to the operand.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        r#"
        use /std/{Async, Handle, Bytes};
        let prog : Async({}) =
            let out : Bytes = x[\3e, ..Async/pure(x[\68, \69])!, \3c];
            let wrote = Handle/write(Handle/stdout, out);
            Async/pure(());
        Async/run(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b">hi<");
}

#[test]
fn bin_fold_sums_bytes() {
    let source = r#"
        use /std/{Handle, Str, Nat, Byte, Bytes};
        let b = x[\0a, \14, \1e];
        Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(Bytes/fold(b, 0, (byte, acc) => Nat/add(acc, Byte/to_nat(byte))))))
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"60");
}

// An empty match is a vacuous elimination: it never inspects its scrutinee. A `False` is a `Prop`, so it erases (sort-driven) — a contradiction may therefore discharge into a *relevant* result, both directly (`match c : (_) => A end`) and through `False/absurd`. This is what lets an impossible runtime branch be closed off by an erased witness, the crux of the UTF-8 decode certification.
