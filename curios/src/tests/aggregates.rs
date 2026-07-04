use {super::run, curios_rt::MockHost, std::time::Duration};

#[test]
fn lst_match_is_a_foldr() {
    // Native `Lst` induction (slice 1): the `| [] | (h, t), ih` eliminator,
    // erased by desugaring to `Nat`-induction on the length and reusing the loop.
    // `f(h, ih) = ih * 10 + h` is non-commutative, so the result distinguishes a
    // structural `foldr` (head is the *first* element, ih is the fold of the tail)
    // from a reversed walk: `[1,2,3,4]` folds to `4321`, not `1234`.
    let source = r#"
        use /std/{Io, Str, Nat, Lst};
        let xs : Lst(Nat) = [1, 2, 3, 4];
        let digits : Nat =
            match xs : Nat
            | [] => 0
            | h, ..t; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn lst_map_fills_every_slot() {
    // `Lst/map` erases to a single O(n) fill loop (`emit_map`): size the result
    // from `src.len`, allocate once, then write `f(src[i])` into slot `i` via an
    // inline closure `call_ref`. A non-identity `f` (`+1`) over `[10, 20, 30]` must
    // fill *every* slot, not just one: `get(_, 0) + get(_, 2)` = 11 + 31 = 42.
    let source = r#"
        use /std/{Io, Str, Nat, Lst, Option};
        let xs : Lst(Nat) = Lst/map((n) => Nat/add(n, 1), [10, 20, 30]);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Nat/add(Option/unwrap_or(Lst/get(xs, 0), 0), Option/unwrap_or(Lst/get(xs, 2), 0)))))
        "#;
    assert_eq!(run(source), b"42");
}

#[test]
fn lst_map_distributes_over_cons() {
    // The eliminator rule that lets `Lst/map` stand in for a structural `foldr` in
    // proofs: for a SYMBOLIC tail `t`, `map f (x :: t) ≡ f x :: map f t`
    // *definitionally*. `refl`
    // checks only because `reduce` distributes the map over the `concat` spine and
    // maps the singleton literal — the same peel the native `Lst` eliminator does,
    // so it reduces under induction without unfolding a symbolic array.
    let source = r#"
        use /std/{Io, Str, Eq, Nat, Lst};
        let step(f : (Nat) -> Nat, x : Nat, t : Lst(Nat))
            -> Eq(Lst/map(f, Lst/concat([x], t)), Lst/concat([f(x)], Lst/map(f, t))) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_match_is_a_foldr() {
    // Native `Bin` induction (slice 2): the `| \\ | (h, t), ih` eliminator, erased
    // exactly like `Lst` — `Nat`-induction on the byte length, reusing the loop.
    // The leading byte `h` is reflected as a `Nat`. Same non-commutative `foldr`
    // probe as `lst_match_is_a_foldr`: the bytes `\01\02\03\04` fold to `4321`, not
    // `1234`, pinning head = first byte and ih = fold of the tail.
    let source = r#"
        use /std/{Io, Str, Nat, Bin};
        let bytes : Bin = \01\02\03\04;
        let digits : Nat =
            match bytes : Nat
            | \\ => 0
            | h, ..t; ih => Nat/add(Nat/mul(ih, 10), h)
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
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
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
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn lst_slice_is_a_monoid_citizen() {
    // The `Lst` mirror of `bin_slice_is_a_monoid_citizen`: `Lst/slice` now rides the
    // free-monoid spine as a measured `Window` (`core::spine`), so `split` fuses two
    // adjacent windows of one base across their seam — the convert-level capability —
    // while `empty` and `full` exercise the reduce-level slice identities.
    let source = r#"
        use /std/{Io, Str, Eq, Lst, Nat};
        let split(@T : Type, a : Lst(T), s : Nat, m : Nat, e : Nat)
            -> Eq(Lst/concat(Lst/slice(a, s, m), Lst/slice(a, m, e)), Lst/slice(a, s, e)) =
            Eq/refl();
        let empty(@T : Type, a : Lst(T), i : Nat) -> Eq(Lst/slice(a, i, i), []) = Eq/refl();
        let full(@T : Type, a : Lst(T)) -> Eq(Lst/slice(a, 0, Lst/len(a)), a) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_append_is_concat_with_a_single() {
    // `Lst/append` now rides the spine as `base ++ [e]` (`core::spine`), so it
    // converts to the `concat`-with-`single` form by `refl` even for a symbolic base
    // and element — `append(xs, y) ≡ concat(xs, single(y))`.
    let source = r#"
        use /std/{Io, Str, Eq, Lst};
        let law(@T : Type, xs : Lst(T), y : T)
            -> Eq(Lst/concat(xs, [y]), Lst/append(xs, y)) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn bin_append_is_concat_with_a_single_byte() {
    // The `Bin` twin of `lst_append_is_concat_with_a_single`: `Bin/append` rides the
    // spine as `base ++ b`, so it converts to the `concat`-with-a-one-byte form by
    // `refl` even for a symbolic base and a symbolic byte.
    let source = r#"
        use /std/{Io, Str, Eq, Bin, Nat};
        let law(xs : Bin, y : Nat)
            -> Eq(Bin/concat(xs, Bin/append(\\, y)), Bin/append(xs, y)) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_slice_window_seam_mismatch_is_rejected() {
    // The dual of `bin_slice_window_seam_mismatch_is_rejected`: two `Lst` windows
    // whose seam does not meet must NOT fuse, so the concat is not convertible to the
    // single slice and the `refl` is rejected.
    let source = r#"
        use /std/{Io, Str, Eq, Lst, Nat};
        let bad(@T : Type, a : Lst(T), s : Nat, m : Nat, n : Nat, e : Nat)
            -> Eq(Lst/concat(Lst/slice(a, s, m), Lst/slice(a, n, e)), Lst/slice(a, s, e)) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
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
fn lst_concat_is_a_free_monoid() {
    // `peel_arr` (core::spine) makes `Lst` a free monoid on its elements, the twin
    // of `bin_concat_is_a_free_monoid`: `concat` associates, the empty array `[]`
    // is its identity, and a literal run re-segments freely — all by `refl` for
    // SYMBOLIC arrays (and elements), which `reduce` cannot fold. `convert` peels
    // the two `LstConcat`s to a common normal form.
    let source = r#"
        use /std/{Io, Str, Eq, Lst};
        let assoc(@T : Type, a : Lst(T), b : Lst(T), c : Lst(T))
            -> Eq(Lst/concat(a, Lst/concat(b, c)), Lst/concat(Lst/concat(a, b), c)) =
            Eq/refl();
        let left_id(@T : Type, a : Lst(T)) -> Eq(Lst/concat([], a), a) = Eq/refl();
        let right_id(@T : Type, a : Lst(T)) -> Eq(Lst/concat(a, []), a) = Eq/refl();
        let resegment(@T : Type, a : T, b : T, c : Lst(T))
            -> Eq(Lst/concat([a, b], c), Lst/concat([a], Lst/concat([b], c))) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn lst_concat_length_clash_is_rejected() {
    // Unlike `Bin`, an `Lst` element disagreement is NOT a clash (elements are
    // terms that may be convertible) — but a literal *length* mismatch still is:
    // `[x, y]` and `[x]` peel their shared head and leave one side longer, a
    // definite `Clash`, so the `refl` is rejected. Exercises `peel_arr`'s clash
    // against the empty identity (the element-mismatch case instead defers to the
    // structural arm, kept sound by `Stuck` fall-through).
    let source = r#"
        use /std/{Io, Str, Eq, Lst};
        let bad(@T : Type, x : T, y : T) -> Eq([x, y], [x]) = Eq/refl();
        Io/write(Io/stdout, Str/to_bin("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn empty_bin_literal_is_the_empty_sequence() {
    // The empty `Bin` literal concatenated with a value is the identity.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Io/write(std/Io/stdout, std/Bin/concat(\\, /std/Str/to_bin("ok")))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
}

// Local binders shadow like-named *module* bindings, and a local name never
// leaks past its lexical scope. Inside `mod Foo` the module binding is `Foo/go`:
// an inner `let go` must shadow it (so `shadowed` is 3, not the captured 7),
// while a `go` that is a sibling of an inner `let go = 3` — reached only after
// that scope closes — must resolve back to `Foo/go` (so `sibling` is 7, not a
// leaked, unbound bare `go`). Encoded as 3*10 + 7 = 37, so the unlawful-capture
// regression reads 77 and a scope leak fails to compile.
#[test]
fn vec_cons_with_nat_succ() {
    let source = r#"
        rec Vec(T : Type, n : std/Nat) -> Type =
            match n : Type
            | 0 => {}
            | pred + 1; ih => { T, ih }
            end;

        let cons(T : Type, n : std/Nat, x : T, xs : Vec(T, n)) -> Vec(T, std/Nat/succ(n)) =
            (x, xs);

        let head(T : Type, n : std/Nat, xs : Vec(T, std/Nat/succ(n))) -> T =
            xs.0;

        let v : Vec(std/Nat, 1) = cons(std/Nat, 0, 42, ());
        std/Io/write(std/Io/stdout, /std/Str/to_bin(std/Nat/to_str(head(std/Nat, 0, v))))
    "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"42");
}

#[test]
fn indexed_vec_append_executes() {
    // Rung A of the indexed-inductive ladder, *executed*: `append`'s motive binds
    // the length index (`(v : Vec(T, k)) => Vec(T, Nat/add(k, m))`), the
    // `cons` arm meets it through the definitional successor-peeling of
    // `Nat/add`, and the implicit index arguments of the recursive call are
    // solved to the arm's *first* binder. Running (not just compiling) guards
    // the zonk realignment of multi-binder arm scopes: with the in-group
    // order flipped, the solved indices silently referenced the wrong binder
    // and the program trapped at runtime.
    let source = r#"
        use /std/{Nat, Bin, Io};
        induct Vec(T : Type) : (n : Nat) -> Type
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
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
}

#[test]
fn lst_fold_sums_elements() {
    let source = r#"
        use /std/{Io, Str, Nat, Lst};
        let xs : Lst(Nat) = [10, 20, 30];
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Lst/fold(xs, 0, (e, acc) => Nat/add(acc, e)))))
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"60");
}

#[test]
fn lst_spread_concats_segments() {
    // `[1, ..xs, 4]` splices `xs` between the literal runs. The non-commutative
    // foldr probe (see `lst_match_is_a_foldr`) distinguishes the spliced order
    // `[1, 2, 3, 4]` from any permutation or grouping artifact.
    let source = r#"
        use /std/{Io, Str, Nat, Lst};
        let xs : Lst(Nat) = [2, 3];
        let ys : Lst(Nat) = [1, ..xs, 4];
        let digits : Nat =
            match ys : Nat
            | [] => 0
            | h, ..t; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn lst_spread_identity_and_multi() {
    // `[..xs]` is an identity copy (reduction collapses the lone operand), and
    // spreads repeat: `[..ys, ..ys]` doubles the sequence in written order.
    let source = r#"
        use /std/{Io, Str, Nat, Lst};
        let xs : Lst(Nat) = [2, 3];
        let ys : Lst(Nat) = [..xs];
        let zs : Lst(Nat) = [..ys, ..ys];
        let digits : Nat =
            match zs : Nat
            | [] => 0
            | h, ..t; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(digits)))
        "#;
    assert_eq!(run(source), b"3232");
}

#[test]
fn lst_spread_borrows_expected_element_type() {
    // The `LstConcat` bidirectionality case in `elaborate_prim`: checking
    // `[1, ..xs]` against `Lst(Int)` must solve the lowering-minted element
    // slot from the expected type BEFORE the literal chunk elaborates, so the
    // unsigned `1` lands at `Int`. Without the borrow, `1` would default-solve
    // the slot to `Nat` and this program would be rejected.
    let source = r#"
        use /std/{Io, Str, Nat, Int, Lst};
        let xs : Lst(Int) = [-1, +2];
        let ys : Lst(Int) = [1, ..xs];
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Lst/len(ys))))
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn lst_spread_of_non_list_is_rejected() {
    // A spread operand must itself be a list of the element type — `..2` in a
    // `Lst(Nat)` literal is an ordinary type mismatch (Nat vs Lst(Nat)).
    let source = r#"
        use /std/{Io, Str, Nat, Lst};
        let bad : Lst(Nat) = [1, ..2];
        Io/write(Io/stdout, Str/to_bin("unreachable"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn lst_spread_element_type_clash_is_rejected() {
    let source = r#"
        use /std/{Io, Str, Nat, Lst};
        let ss : Lst(Str) = ["a"];
        let bad : Lst(Nat) = [..ss];
        Io/write(Io/stdout, Str/to_bin("unreachable"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn lst_spread_operand_hoists_bangs() {
    // A bang inside a spread operand hoists into the enclosing region exactly
    // like one inside a plain element — the literal is collected, not sealed.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat, Lst};
        let prog : Task({}) =
            let ys : Lst(Nat) = [1, ..Task/pure([2, 3])!, 4];
            let digits : Nat =
                match ys : Nat
                | [] => 0
                | h, ..t; ih => Nat/add(Nat/mul(ih, 10), h)
                end;
            let wrote = Io/write(Io/stdout, Str/to_bin(Nat/to_str(digits)));
            Task/pure(());
        Task/block_on(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"4321");
}

#[test]
fn bin_spread_concats_segments() {
    // `\01\..b\04` splices the bytes of `b` between the literal runs, and the
    // glued suffix chain admits a call operand (`\..Bin/slice(...)`).
    let source = r#"
        use /std/{Io, Nat, Bin};
        let b : Bin = \02\03;
        Io/write(Io/stdout, \01\..b\04\..Bin/slice(b, 1, 2))
        "#;
    assert_eq!(run(source), b"\x01\x02\x03\x04\x03");
}

#[test]
fn bin_spread_identity_and_multi() {
    let source = r#"
        use /std/{Io, Bin};
        let b : Bin = \48\65;
        let c : Bin = \..b;
        Io/write(Io/stdout, \..c\..c)
        "#;
    assert_eq!(run(source), b"HeHe");
}

#[test]
fn bin_spread_of_non_bin_is_rejected() {
    // A spread operand must itself be a `Bin` — a list is an ordinary type
    // mismatch.
    let source = r#"
        use /std/{Io, Str, Nat, Lst, Bin};
        let xs : Lst(Nat) = [1, 2];
        let bad : Bin = \00\..xs;
        Io/write(Io/stdout, Str/to_bin("unreachable"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn bin_spread_operand_hoists_bangs() {
    // The `Bin` sibling of `lst_spread_operand_hoists_bangs`, through the
    // dedicated `Prim::Bin` collect arm — the glued `!` binds to the operand.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Bin};
        let prog : Task({}) =
            let out : Bin = \3e\..Task/pure(\68\69)!\3c;
            let wrote = Io/write(Io/stdout, out);
            Task/pure(());
        Task/block_on(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b">hi<");
}

#[test]
fn bin_fold_sums_bytes() {
    let source = r#"
        use /std/{Io, Str, Nat, Bin};
        let b = Bin/append(Bin/append(Bin/append(\\, 10), 20), 30);
        Io/write(Io/stdout, Str/to_bin(Nat/to_str(Bin/fold(b, 0, (byte, acc) => Nat/add(acc, byte)))))
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"60");
}

// An empty match is a vacuous elimination: it never inspects its scrutinee. A
// `False` is a `Prop`, so it erases (sort-driven) — a contradiction may therefore
// discharge into a *relevant* result, both directly (`match c : A end`) and
// through `False/absurd`. This is what lets an impossible runtime branch be closed
// off by an erased witness, the crux of the UTF-8 decode certification.
