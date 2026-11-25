//! Nested constructor patterns over every carrier, and the shapes that lower without synthetic indirection.

use crate::test_support::*;

// A single, unnested constructor arm reuses the written name rather than minting a synthetic binder: a core binder whose only label is a gensym is chained by the erasure pass's hint-based fresh naming into another gensym, compounding until a reference outruns its own binding. A plain flat match lowers with no such indirection.
#[test]
fn flat_option_match_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(y) => y
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(5))))
        "#;

    assert_eq!(run(source), b"5");
}

// A single tupled head, fully enumerated over two independent `Option`-shaped columns.
#[test]
fn nested_ctor_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat};
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

// A `Nat` literal leaf (`0`/`n + 1`) nested inside a constructor payload.
#[test]
fn nested_nat_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Option, Nat};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(n + 1) => n
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
        use /std/{Nat, List};
        let f(p : { Nat, List(Nat) }) -> Nat =
            match p
            | (x, []) => x
            | (x, [h, ..t]) => h
            end;
        /std/print(Nat/to_str(f((0, [7, 8]))))
        "#;

    assert_eq!(run(source), b"7");
}

// A `Bytes` literal leaf (`0x[]`/`0x[h, ..t]`) nested inside a tuple field.
#[test]
fn nested_bytes_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str};
        let f(p : { Nat, Bytes }) -> Nat =
            match p
            | (x, 0x[]) => x
            | (x, 0x[h, ..t]) => Byte/to_nat(h)
            end;
        /std/print(Nat/to_str(f((0, Str/to_bytes("A")))))
        "#;

    assert_eq!(run(source), b"65");
}

// A `Bool` literal leaf (`true`/`false`) nested inside a constructor payload — two full rows.
#[test]
fn nested_bool_pattern_dispatches_by_shape() {
    let source = r#"
        use /std/{Bool, Nat};
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

// The carrier-leaf twin of `flat_option_match_lowers_without_synthetic_indirection`: a single, non-nested `some(0)`/`some(n + 1)`/`none()` match lowers and runs end to end, exercising the single-row fast paths of the matrix compiler's `variants` and `naturals` together, which is where the erasure hint compounding would reappear for a carrier leaf.
#[test]
fn nested_nat_zero_pattern_lowers_without_synthetic_indirection() {
    let source = r#"
        use /std/{Option, Nat};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(n + 1) => n
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
        use /std/{Option, Nat, Bytes, rand};
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            let n = Nat/add(z, 5);
            let hit =
                match Option/some(n)
                | some(5) => Nat/add(z, 700)
                | _ => Nat/add(z, 999)
                end;
            /std/print(Nat/to_str(hit));
        entry()
        "#;

    assert_eq!(run(source), b"700");
}

#[test]
fn nested_nat_literal_dispatch_falls_through_to_default() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand};
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            let n = Nat/add(z, 6);
            let miss =
                match Option/some(n)
                | some(5) => Nat/add(z, 700)
                | _ => Nat/add(z, 999)
                end;
            /std/print(Nat/to_str(miss));
        entry()
        "#;

    assert_eq!(run(source), b"999");
}
