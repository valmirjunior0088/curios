use {
    super::{error, run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn bool_logic_and_of_str() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Str, Option, Handle};
        let computed = Bool/and(Bool/or(false, true), Bool/not(false));
        let parsed = match Bool/of_str("false") : (_) => Bool
            | some(b) => b
            | none() => true
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Str/concat(Bool/to_str(computed), Bool/to_str(parsed))))!;
        /std/Io/pure(())
        "#),
        b"truefalse"
    );
}

#[test]
fn bool_xor_executes() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Str, Handle};
        let a = Bool/xor(true, false);
        let b = Bool/xor(true, true);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Str/concat(Bool/to_str(a), Bool/to_str(b))))!;
        /std/Io/pure(())
        "#),
        b"truefalse"
    );
}

#[test]
fn bool_eql_executes() {
    assert_eq!(
        run(r#"
        use /std/{Bool, Str, Handle};
        let a = Bool/eql(true, true);
        let b = Bool/eql(true, false);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Str/concat(Bool/to_str(a), Bool/to_str(b))))!;
        /std/Io/pure(())
        "#),
        b"truefalse"
    );
}

#[test]
fn nat_bitwise_ops_execute() {
    // The first input byte is `A` (65); reading it from the host keeps the operand opaque to the optimizer, so each op is lowered to its WebAssembly instruction and executed for real rather than folded at compile time. The shift stays inside the i31 envelope — an overflowing `shl` traps at the backend boundary (see the numeric envelope gates).
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Handle, Byte, Bytes, Nat, Str, Option};
        let bytes = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let x = Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0));
        let r = Str/flatten([
            Nat/to_str(Nat/and(x, 15)), ",",
            Nat/to_str(Nat/or(x, 128)), ",",
            Nat/to_str(Nat/xor(x, 255)), ",",
            Nat/to_str(Nat/shl(x, 20)), ",",
            Nat/to_str(Nat/shr(x, 1))
        ]);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(r))!;
        /std/Io/pure(())
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"1,193,190,68157440,32");
}

#[test]
fn int_bitwise_ops_execute() {
    // `x` is the host byte `A` (65) read as an `Int`, kept opaque to the optimizer so each op lowers to its WebAssembly instruction. This exercises the Int-distinctive behaviors: an arithmetic (sign-preserving) `shr` on a negative operand (-65 >> 1 = -33) and the `xor`-based `not` (-x - 1). The shift stays inside the signed i31 envelope — an overflowing `shl` traps at the backend boundary (see the numeric envelope gates).
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Handle, Byte, Bytes, Nat, Int, Str, Option};
        let bytes = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let x = Nat/to_int(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)));
        let neg = Int/sub(+0, x);
        let r = Str/flatten([
            Int/to_str(Int/and(x, +15)), ",",
            Int/to_str(Int/or(x, +128)), ",",
            Int/to_str(Int/xor(x, +255)), ",",
            Int/to_str(Int/shl(x, +20)), ",",
            Int/to_str(Int/shr(neg, +1)), ",",
            Int/to_str(Int/not(x))
        ]);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(r))!;
        /std/Io/pure(())
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
            use /std/{Handle, Str, Nat};
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(2 + 3 * 4)))!;
            /std/Io/pure(())
        "#),
        b"14"
    );
    assert_eq!(
        run(r#"
            use /std/{Handle, Str, Nat};
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str((2 + 3) * 4)))!;
            /std/Io/pure(())
        "#),
        b"20"
    );
    assert_eq!(
        run(r#"
            use /std/{Handle, Str, Nat};
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(10 - 3 - 2)))!;
            /std/Io/pure(())
        "#),
        b"5"
    );
    assert_eq!(
        run(r#"
            use /std/{Handle, Str, Nat};
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(17 % 5)))!;
            /std/Io/pure(())
        "#),
        b"2"
    );
}

#[test]
fn infix_resolves_to_int_for_signed_literals() {
    // Both operands carry a written sign, so the operand type defaults to `Int`.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str, Int};
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Int/to_str(-2 - +3)))!;
            /std/Io/pure(())
        "#),
        b"-5"
    );
}

#[test]
fn infix_resolves_against_a_bound_variable_type() {
    // A bare literal takes the type of the variable it is combined with: here `x : Int`, so `x + 1` is `Int` addition, not a `Nat`/`Int` mismatch.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str, Int};
            let x : Int = -10;
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Int/to_str(x + 1)))!;
            /std/Io/pure(())
        "#),
        b"-9"
    );
}

#[test]
fn infix_comparison_yields_a_bool_scrutinee() {
    // A comparison resolves on its operand type and yields `Bool`, usable directly as a `match` scrutinee. `2 + 2 == 4` is true; `3 < 1` is false.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str};
            let _ = (match 2 + 2 == 4
            | true => Handle/write(Handle/stdout, Str/to_bytes("yes"))
            | false => Handle/write(Handle/stdout, Str/to_bytes("no"))
            end)!;
            /std/Io/pure(())
        "#),
        b"yes"
    );
    assert_eq!(
        run(r#"
            use /std/{Handle, Str};
            let _ = (match 3 < 1
            | true => Handle/write(Handle/stdout, Str/to_bytes("yes"))
            | false => Handle/write(Handle/stdout, Str/to_bytes("no"))
            end)!;
            /std/Io/pure(())
        "#),
        b"no"
    );
}

#[test]
fn infix_equality_is_overloaded_for_bool() {
    // `==` resolves at `Bool` too (`BoolEql`), so `(1 < 2) == (3 < 4)` is `true`.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str};
            let _ = (match (1 < 2) == (3 < 4)
            | true => Handle/write(Handle/stdout, Str/to_bytes("yes"))
            | false => Handle/write(Handle/stdout, Str/to_bytes("no"))
            end)!;
            /std/Io/pure(())
        "#),
        b"yes"
    );
}

#[test]
fn infix_mixes_a_float_variable_with_an_integer_literal() {
    // `x : Flt` pins the operand type, so the bare literal `1` in `x + 1` becomes a `Flt`; the comparison against the `Flt` literal `4.5` then type-checks.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str, Flt};
            let x : Flt = 3.0;
            let _ = (match x + 1 < 4.5
            | true => Handle/write(Handle/stdout, Str/to_bytes("less"))
            | false => Handle/write(Handle/stdout, Str/to_bytes("more"))
            end)!;
            /std/Io/pure(())
        "#),
        b"less"
    );
}

#[test]
fn infix_undefined_operator_for_type_is_rejected() {
    // `&&` only has a witness on `Bool`, so `nat && nat` has no `And(Nat)` — a compile-time error, same as any other operator at an un-witnessed type.
    let source = r#"
        use /std/{Handle, Str};
        let _ = (match 1 && 2
        | true => Handle/write(Handle/stdout, Str/to_bytes("yes"))
        | false => Handle/write(Handle/stdout, Str/to_bytes("no"))
        end)!;
        /std/Io/pure(())
    "#;
    error(source);
}

#[test]
fn infix_rem_on_flt_computes_fmod() {
    // `%` on `Flt` is `fmod` (`FltRem`), expanded to `x - trunc(x/y)*y` at the cont -> wasm boundary. `5.5 % 2.0 == 1.5`, so the branch is `true`.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str};
            let _ = (match 5.5 % 2.0 == 1.5
            | true => Handle/write(Handle/stdout, Str/to_bytes("yes"))
            | false => Handle/write(Handle/stdout, Str/to_bytes("no"))
            end)!;
            /std/Io/pure(())
        "#),
        b"yes"
    );
}

#[test]
fn infix_not_equal_on_bool_resolves_to_bool_neq() {
    // `bool_ != bool_` projects `Eql(Bool)`'s `neq`, which is `xor` — disequality on one bit needs no negation, so this collapses to a single `BoolXor`. `true != false` is `true`.
    assert_eq!(
        run(r#"
            use /std/{Handle, Str};
            let _ = (match true != false
            | true => Handle/write(Handle/stdout, Str/to_bytes("yes"))
            | false => Handle/write(Handle/stdout, Str/to_bytes("no"))
            end)!;
            /std/Io/pure(())
        "#),
        b"yes"
    );
}

#[test]
fn infix_mismatched_operand_types_are_rejected() {
    // No implicit coercion: a `Nat` and an `Int` cannot be added.
    let source = r#"
        use /std/{Handle, Str, Int};
        let n : Nat = 1;
        let i : Int = -1;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Int/to_str(n + i)))!;
        /std/Io/pure(())
    "#;
    error(source);
}

// === Concept-dispatched operators (one path, no overload table) =============

// `+` on a user struct resolves the user's `Add` witness — the operator surface is witness-governed.
#[test]
fn infix_add_on_a_user_record_resolves_its_witness() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Handle, Str, Add};
            struct Point : pub Type { x : Nat, y : Nat }
            satisfy Add(Point) {
                add(a, b) = Point { x = a.x + b.x, y = a.y + b.y }
            }
            let p : Point = Point { x = 3, y = 4 };
            let q : Point = p + p;
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(q.y)))!;
            /std/Io/pure(())
        "#),
        b"8"
    );
}

// In generic code, `x + x` resolves against the local `use Add(A)` premise (resolution step 1) — no extra machinery.
#[test]
fn infix_resolves_against_a_local_use_premise() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Handle, Str, Add};
            pub let double(@A : Type, use Add(A), x : A) -> A = x + x;
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(double(21))))!;
            /std/Io/pure(())
        "#),
        b"42"
    );
}

// `==` on `Str` resolves the std-side `eql_str` witness — impossible under the old per-type overload table.
#[test]
fn infix_equality_works_on_str() {
    assert_eq!(
        run(r#"
            use /std/{Bool, Handle, Str};
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Bool/to_str("abc" == "abc")))!;
            /std/Io/pure(())
        "#),
        b"true"
    );
}

// An operator at a type with no witness is a missing-witness diagnostic — the single operator error vocabulary (`operator ... not defined` survives only for the exotic no-prelude embedding, where the concept itself is absent from the context).
#[test]
fn infix_without_witness_reports_no_witness() {
    let source = r#"
        use /std/{Bool, Handle, Str};
        let b : Bool = true + false;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Bool/to_str(b)))!;
        /std/Io/pure(())
    "#;
    let error = error(source);
    assert!(error.contains("no witness"), "unexpected error: {error}");
}

// A literal mixed with a witnessed user type still errors on the literal: `p` pins the operand type to `Point`, and `1` has no `Point` realization.
#[test]
fn infix_literal_against_a_user_type_is_rejected() {
    let source = r#"
        use /std/{Nat, Handle, Str, Add};
        struct Point : pub Type { x : Nat, y : Nat }
        satisfy Add(Point) {
            add(a, b) = Point { x = a.x + b.x, y = a.y + b.y }
        }
        let p : Point = Point { x = 1, y = 2 };
        let q : Point = p + 1;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(q.x)))!;
        /std/Io/pure(())
    "#;
    error(source);
}

// Type-level operators: `a + 1` in `Lte`'s constructor indices elaborates to a witness projection that must reduce during conversion checking, and the two spellings (the index's and this call site's) stay convertible across items.
#[test]
fn type_level_operator_indices_stay_convertible() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Handle, Str};
            pub let step(@a : Nat, @b : Nat, p : Nat/Lte(a, b)) -> Nat/Lte(a + 1, b + 1) =
                Nat/Lte/s(p);
            let base : Nat/Lte(0, 1) = Nat/Lte/z();
            let bumped : Nat/Lte(1, 2) = step(base);
            let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
            /std/Io/pure(())
        "#),
        b"ok"
    );
}

// `!=` is `Eql`'s own `neq` method; on `Bytes` both `==` and `!=` resolve through the migrated sys witness, `neq` negating `eql` because the packed carrier has no disequality instruction.
#[test]
fn infix_equality_works_on_bin() {
    assert_eq!(
        run(r#"
            use /std/{Bool, Bytes, Handle, Str};
            let a : Bytes = Str/to_bytes("xy");
            let b : Bytes = Str/to_bytes("xz");
            let _ = Handle/write(Handle/stdout, Str/to_bytes(Bool/to_str(a != b)))!;
            /std/Io/pure(())
        "#),
        b"true"
    );
}
