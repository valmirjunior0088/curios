//! Nested constructor patterns over every carrier, and the shapes that lower without synthetic indirection.

use {
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

// The `;` fold-hypothesis position accepts an irrefutable pattern: the destructuring binds the fold result's fields directly, lowering to the same projections a `let (t, live) = ih;` would.
#[test]
fn a_fold_hypothesis_destructures_directly() {
    let source = r#"
        use /std/{Nat, Bool, Byte, Bytes, Handle};
        let count(n : Nat) -> Nat =
            let (total, _) =
                match n : (_) => {Nat, Bool}
                | 0 => (0, true)
                | pred + 1; (t, live) => (t + 1, live)
                end;
            total;
        let sum(b : Bytes) -> Nat =
            let (s, _) =
                match b : (_) => {Nat, Bool}
                | x[] => (0, true)
                | x[h, ..t]; (acc, live) => (acc + Byte/to_nat(h), live)
                end;
            s;
        /std/print(Nat/to_str(count(5) + sum(x[0x01, 0x02, 0x03])))
        "#;

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"11");
}

// Regression test for a bug found while building the matrix pattern compiler: minting a synthetic binder for a single, unnested constructor arm (rather than reusing the written name directly) produced a core binder whose only label was that gensym — which the erasure pass's hint-based fresh naming then chained into another gensym, compounding until a reference outran its own binding. A plain flat match must still lower with no such indirection.
#[test]
fn flat_option_match_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(y) => y
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(5))))
        "#;

    assert_eq!(run(source), b"5");
}

#[test]
fn bits_structural_fold_preserves_heads_and_bit_unit_tails() {
    let source = r#"
        use /std/{Bits, Nat, Handle};
        let value(bits : Bits) -> Nat =
            match bits
            | b[] => 0
            | b[head, ..tail]; ih =>
                let digit : Nat = match head | false => 0 | true => 1 end;
                digit + 2 * ih
            end;
        /std/print(Nat/to_str(value(b[1, 0, 1, 1, 0, 0, 1, 0, 1, 1])))
        "#;

    assert_eq!(run(source), b"845");
}

// The spec's own motivating example: a single tupled head, fully enumerated over two independent `Option`-shaped columns.
#[test]
fn nested_ctor_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    assert_eq!(run(source), b"7");
}

// A `Nat` literal leaf (`0`/`n + 1; ih`) nested inside a constructor payload.
#[test]
fn nested_nat_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(n + 1; ih) => n
            | none() => 1
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    assert_eq!(run(source), b"2");
}

// An `List` literal leaf (`[]`/`[h, ..t]`) nested inside a tuple field.
#[test]
fn nested_list_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Nat, List, Handle};
        let f(p : { Nat, List(Nat) }) -> Nat =
            match p
            | (x, []) => x
            | (x, [h, ..t]) => h
            end;
        /std/print(Nat/to_str(f((0, [7, 8]))))
        "#;

    assert_eq!(run(source), b"7");
}

// A `Bytes` literal leaf (`x[]`/`x[h, ..t]`) nested inside a tuple field.
#[test]
fn nested_bin_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Handle};
        let f(p : { Nat, Bytes }) -> Nat =
            match p
            | (x, x[]) => x
            | (x, x[h, ..t]) => Byte/to_nat(h)
            end;
        /std/print(Nat/to_str(f((0, Str/to_bytes("A")))))
        "#;

    assert_eq!(run(source), b"65");
}

// A `Bool` literal leaf (`true`/`false`) nested inside a constructor payload — two full rows.
#[test]
fn nested_bool_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Bool, Nat, Handle};
         pub induct Pair(A : Type, B : Type) : pub Type
        | pair(A, B)
        end
        let f(p : Pair(Bool, Nat)) -> Nat =
            match p
            | pair(true, y) => y
            | pair(false, y) => y + 1
            end;
        /std/print(Nat/to_str(f(Pair/pair(false, 4))))
        "#;

    assert_eq!(run(source), b"5");
}

// Regression test mirroring `flat_option_match_lowers_without_synthetic_indirection`: a single, non-nested `some(0)`/`some(n + 1; ih)`/`none()` match must lower and run correctly end-to-end, exercising `compile_ctor`'s and `compile_nat`'s single-row fast paths together — guarding against reintroducing the erasure hint-compounding bug for the new carrier leaves.
#[test]
fn nested_nat_zero_pattern_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat, Handle};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(n + 1; ih) => n
            | none() => 1
            end;
        /std/print(Nat/to_str(f(Option/some(1))))
        "#;

    assert_eq!(run(source), b"0");
}

// Nested literal dispatch as emitted wasm: a runtime-tainted `n == 5` inside `some(n)` selects the `some(5)` arm; the `_` fallthrough covers every other value (and `none()`). Exercises `compile_nat`'s switch mode — a `Cases::Switch` reached through a constructor payload — at runtime rather than folded.
#[test]
fn nested_nat_literal_dispatch_selects_matching_case() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bytes(0)!);
        let n = Nat/add(z, 5);
        let hit =
            match Option/some(n)
            | some(5) => Nat/add(z, 700)
            | _ => Nat/add(z, 999)
            end;
        /std/print(Nat/to_str(hit))
        "#;

    assert_eq!(run(source), b"700");
}

#[test]
fn nested_nat_literal_dispatch_falls_through_to_default() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bytes(0)!);
        let n = Nat/add(z, 6);
        let miss =
            match Option/some(n)
            | some(5) => Nat/add(z, 700)
            | _ => Nat/add(z, 999)
            end;
        /std/print(Nat/to_str(miss))
        "#;

    assert_eq!(run(source), b"999");
}
