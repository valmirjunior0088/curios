//! What each carrier's operators compute: `Bool`'s logic and equality, `Nat` and `Int` arithmetic and bitwise operations, `Flt`'s remainder, and equality on text and bytes.

use {
    crate::tests::{run, run_text},
    curios_native::MockHost,
};

#[test]
fn bool_logic_and_of_str() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Str, Option, Io};
        let entry() -> /std/Io({}) =
            let computed = Bool/and(Bool/or(false, true), Bool/not(false));
            let parsed = match Bool/of_str("false") : (_) => Bool
                | some(b) => b
                | none() => true
                end;
            let _ = /std/print(Str/concat(Bool/to_str(computed), Bool/to_str(parsed)))!;
            /std/Io/pure(());
        entry()
        "#),
        b"truefalse"
    );
}

#[test]
fn bool_xor_executes() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Str, Io};
        let entry() -> /std/Io({}) =
            let a = Bool/xor(true, false);
            let b = Bool/xor(true, true);
            let _ = /std/print(Str/concat(Bool/to_str(a), Bool/to_str(b)))!;
            /std/Io/pure(());
        entry()
        "#),
        b"truefalse"
    );
}

#[test]
fn bool_eql_executes() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Str, Io};
        let entry() -> /std/Io({}) =
            let a = Bool/eql(true, true);
            let b = Bool/eql(true, false);
            let _ = /std/print(Str/concat(Bool/to_str(a), Bool/to_str(b)))!;
            /std/Io/pure(());
        entry()
        "#),
        b"truefalse"
    );
}

#[test]
fn nat_bitwise_ops_execute() {
    // The first input byte is `A` (65); reading it from the host keeps the operand opaque to the optimizer, so each op is lowered to its WebAssembly instruction and executed for real rather than folded at compile time. The shift stays inside the i31, so every op takes its fast path.
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Byte, Bytes, Nat, Str, Option, Io, Try, Process};
        let entry() -> /std/Io({}) =
            let bytes = match Try/run(Io/read(Process/stdin, 16))! : (_) => Bytes
                | success(bytes(b, @_)) => b
                | success(ended()) => x[]
                | failure(_) => x[]
                end;
            let x = Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0));
            let r = Str/flatten([
                Nat/to_str(Nat/and(x, 15)), ",",
                Nat/to_str(Nat/or(x, 128)), ",",
                Nat/to_str(Nat/xor(x, 255)), ",",
                Nat/to_str(Nat/shl(x, 20)), ",",
                Nat/to_str(Nat/shr(x, 1))
            ]);
            let _ = /std/print(r)!;
            /std/Io/pure(());
        entry()
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"1,193,190,68157440,32");
}

#[test]
fn int_bitwise_ops_execute() {
    // `x` is the host byte `A` (65) read as an `Int`, kept opaque to the optimizer so each op lowers to its WebAssembly instruction. This exercises the Int-distinctive behaviors: an arithmetic (sign-preserving) `shr` on a negative operand (-65 >> 1 = -33) and the `xor`-based `not` (-x - 1). The shift stays inside the signed i31, so every op takes its fast path.
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Byte, Bytes, Nat, Int, Str, Option, Io, Try, Process};
        let entry() -> /std/Io({}) =
            let bytes = match Try/run(Io/read(Process/stdin, 16))! : (_) => Bytes
                | success(bytes(b, @_)) => b
                | success(ended()) => x[]
                | failure(_) => x[]
                end;
            let x = Nat/to_int(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)));
            let neg = Int/sub(+0, x);
            let r = Str/flatten([
                Int/to_str(Int/and(x, +15)), ",",
                Int/to_str(Int/or(x, +128)), ",",
                Int/to_str(Int/xor(x, +255)), ",",
                Int/to_str(Int/shl(x, 20)), ",",
                Int/to_str(Int/shr(neg, 1)), ",",
                Int/to_str(Int/not(x))
            ]);
            let _ = /std/print(r)!;
            /std/Io/pure(());
        entry()
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"+1,+193,+190,+68157440,-33,-66");
}

#[test]
fn infix_nat_arithmetic_respects_precedence_and_associativity() {
    // `*` binds tighter than `+`; parentheses override; `-` is left-associative. 2 + 3*4 = 14, (2+3)*4 = 20, 10-3-2 = 5, 17 % 5 = 2.
    assert_eq!(
        run(r#"
            use /std/{Str, Nat, Io};
            let entry() -> /std/Io({}) =
                let _ = /std/print(Nat/to_str(2 + 3 * 4))!;
                /std/Io/pure(());
            entry()
        "#),
        b"14"
    );
    assert_eq!(
        run(r#"
            use /std/{Str, Nat, Io};
            let entry() -> /std/Io({}) =
                let _ = /std/print(Nat/to_str((2 + 3) * 4))!;
                /std/Io/pure(());
            entry()
        "#),
        b"20"
    );
    assert_eq!(
        run(r#"
            use /std/{Str, Nat, Io};
            let entry() -> /std/Io({}) =
                let _ = /std/print(Nat/to_str(10 - 3 - 2))!;
                /std/Io/pure(());
            entry()
        "#),
        b"5"
    );
    assert_eq!(
        run(r#"
            use /std/{Str, Nat, Io};
            let entry() -> /std/Io({}) =
                let _ = /std/print(Nat/to_str(17 % 5))!;
                /std/Io/pure(());
            entry()
        "#),
        b"2"
    );
}

#[test]
fn infix_comparison_yields_a_bool_scrutinee() {
    // A comparison resolves on its operand type and yields `Bool`, usable directly as a `match` scrutinee. `2 + 2 == 4` is true; `3 < 1` is false.
    assert_eq!(
        run(r#"
            use /std/{Str, Io};
            let entry() -> /std/Io({}) =
                let _ = (match 2 + 2 == 4
                | true => /std/print("yes")
                | false => /std/print("no")
                end)!;
                /std/Io/pure(());
            entry()
        "#),
        b"yes"
    );
    assert_eq!(
        run(r#"
            use /std/{Str, Io};
            let entry() -> /std/Io({}) =
                let _ = (match 3 < 1
                | true => /std/print("yes")
                | false => /std/print("no")
                end)!;
                /std/Io/pure(());
            entry()
        "#),
        b"no"
    );
}

#[test]
fn infix_equality_is_overloaded_for_bool() {
    // `==` resolves at `Bool` too (`BoolEql`), so `(1 < 2) == (3 < 4)` is `true`.
    assert_eq!(
        run(r#"
            use /std/{Str, Io};
            let entry() -> /std/Io({}) =
                let _ = (match (1 < 2) == (3 < 4)
                | true => /std/print("yes")
                | false => /std/print("no")
                end)!;
                /std/Io/pure(());
            entry()
        "#),
        b"yes"
    );
}

#[test]
fn infix_rem_on_flt_computes_fmod() {
    // `%` on `Flt` is exact `fmod` (`FltRem`), which the emitter computes in its shared `$flt/rem` helper. `5.5 % 2.0 == 1.5`, so the branch is `true`.
    assert_eq!(
        run(r#"
            use /std/{Str, Io};
            let entry() -> /std/Io({}) =
                let _ = (match 5.5 % 2.0 == 1.5
                | true => /std/print("yes")
                | false => /std/print("no")
                end)!;
                /std/Io/pure(());
            entry()
        "#),
        b"yes"
    );
}

#[test]
fn infix_not_equal_on_bool_resolves_to_bool_neq() {
    // `bool_ != bool_` projects `Eql(Bool)`'s `neq`, which is `xor` — disequality on one bit needs no negation, so this collapses to a single `BoolXor`. `true != false` is `true`.
    assert_eq!(
        run(r#"
            use /std/{Str, Io};
            let entry() -> /std/Io({}) =
                let _ = (match true != false
                | true => /std/print("yes")
                | false => /std/print("no")
                end)!;
                /std/Io/pure(());
            entry()
        "#),
        b"yes"
    );
}

// `==` on `Str` resolves `/std/Str`'s `Eql(Str)` witness.
#[test]
fn infix_equality_works_on_str() {
    assert_eq!(
        run(r#"
            use /std/{Bool, Str, Io};
            let entry() -> /std/Io({}) =
                let _ = /std/print(Bool/to_str("abc" == "abc"))!;
                /std/Io/pure(());
            entry()
        "#),
        b"true"
    );
}

// Type-level operators: `a + 1` in `Le`'s constructor indices elaborates to a witness projection that must reduce during conversion checking, and the two spellings (the index's and this call site's) stay convertible across items.
#[test]
fn type_level_operator_indices_stay_convertible() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Str, Io};
            pub induct Le: (Nat, Nat) -> pub Prop
            | z(@n : Nat): (0, n)
            | s(@a : Nat, @b : Nat, prev : Le(a, b)): (a + 1, b + 1)
            end
            pub let step(@a : Nat, @b : Nat, p : Le(a, b)) -> Le(a + 1, b + 1) =
                Le/s(p);
            let base : Le(0, 1) = Le/z();
            let bumped : Le(1, 2) = step(base);
            let entry() -> /std/Io({}) =
                let _ = /std/print("ok")!;
                /std/Io/pure(());
            entry()
        "#),
        b"ok"
    );
}

// `!=` is `Eql`'s own `neq` method; on `Bytes` both `==` and `!=` resolve through `/std/Bytes`'s `Eql(Bytes)` witness, `neq` negating `eql` because the packed carrier has no disequality instruction.
#[test]
fn infix_equality_works_on_bytes() {
    assert_eq!(
        run(r#"
            use /std/{Bool, Bytes, Str, Io};
            let a : Bytes = Str/to_bytes("xy");
            let b : Bytes = Str/to_bytes("xz");
            let entry() -> /std/Io({}) =
                let _ = /std/print(Bool/to_str(a != b))!;
                /std/Io/pure(());
            entry()
        "#),
        b"true"
    );
}
