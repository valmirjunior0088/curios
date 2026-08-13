//! The numeric envelope gates: every constant folder computes in exact `u32`/`i32` (the numeric law), and the i31 backend boundary appears only as a trap in emitted Wasm — an overflowing computation traps, and a folded literal the carrier cannot box traps at its materialization point. The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics.

use {
    super::{run, run_text},
    curios_runtime::MockHost,
};

/// Wrap `body` (an expression over the runtime-zero binder `n`, and its `Int`-carrier twin `i`) in a program that reads `n` from the host so the optimizer cannot fold it.
fn tainted(body: &str) -> String {
    format!(
        r#"
        use /std/{{Handle, Nat, Int, Flt, Byte, Bytes, Str, Option}};
        let bytes = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let n = Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/get(bytes, 0), 0)), 65);
        let i = Nat/to_int(n);
        let to_nat_or(x : Int, d : Nat) -> Nat =
            match x >= +0 | true => Int/to_nat(x) | false => d end;
        /std/print({body})
        "#
    )
}

/// The same program with `n`/`i` as literal zeros, so the whole expression folds at compile time.
fn closed(body: &str) -> String {
    format!(
        r#"
        use /std/{{Handle, Nat, Int, Flt, Str}};
        let n = 0;
        let i = +0;
        let to_nat_or(x : Int, d : Nat) -> Nat =
            match x >= +0 | true => Int/to_nat(x) | false => d end;
        /std/print({body})
        "#
    )
}

fn run_tainted(body: &str) -> Result<Vec<u8>, String> {
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(&tainted(body), system)?;
    Ok(io.output().to_vec())
}

/// Assert the folded and executed results of `body` agree byte-for-byte.
fn folded_matches_runtime(body: &str) {
    let folded = run(&closed(body));
    let executed = run_tainted(body).expect("in-envelope expression executes");
    assert_eq!(folded, executed, "fold/runtime disagreement on: {body}");
}

/// Assert the runtime computation of `body` traps at the backend boundary.
fn runtime_traps(body: &str) {
    let error = run_tainted(body).expect_err("expression should trap");
    assert!(
        error.contains("execution failed"),
        "expected a runtime trap for {body}, got: {error}"
    );
}

#[test]
fn folded_and_executed_scalar_ops_agree_inside_the_envelope() {
    for body in [
        // Nat arithmetic at the top of the envelope.
        "Nat/to_str(1000000000 + 1000000000 + n)",
        "Nat/to_str(Nat/sub(3 + n, 5))",
        "Nat/to_str(Nat/mul(46340 + n, 46341))",
        "Nat/to_str(Nat/div(2000000000 + n, 3))",
        "Nat/to_str(Nat/rem(2000000000 + n, 7))",
        "Nat/to_str(Nat/shl(3 + n, 29))",
        "Nat/to_str(Nat/shr(2000000000 + n, 5))",
        // Int arithmetic across zero and at the negative edge.
        "Int/to_str(Int/add(-536870912, Int/add(-536870911, i)))",
        "Int/to_str(Int/mul(-3, Int/add(+7, i)))",
        "Int/to_str(Int/div(Int/add(-7, i), +2))",
        "Int/to_str(Int/rem(Int/add(-7, i), +2))",
        "Int/to_str(Int/shl(Int/add(-3, i), +20))",
        "Int/to_str(Int/shr(Int/add(-65, i), +1))",
        // Carrier reinterpretations inside both envelopes.
        "Int/to_str(Nat/to_int(1000000000 + n))",
        // Guarded on `>= +0`, the comparison `/sys/Int/to_nat`'s precondition is decided on: `i` is runtime-tainted, so nothing settles the sign statically and the narrowing demands evidence. Both arms fold identically at the literal `i`, so the differential still compares the conversion rather than the guard.
        "Nat/to_str(to_nat_or(Int/add(+12345, i), 0))",
        // Rotations, bit counts, and sign transfer.
        "Nat/to_str(Nat/rotl(3 + n, 4))",
        "Nat/to_str(Nat/rotr(64 + n, 3))",
        "Nat/to_str(Nat/clz(1 + n))",
        "Nat/to_str(Nat/ctz(48 + n))",
        "Nat/to_str(Nat/popcnt(255 + n))",
        "Int/to_str(Int/rotl(Int/add(+3, i), +4))",
        "Int/to_str(Int/rotr(Int/add(+64, i), +3))",
        "Int/to_str(Int/clz(Int/add(+1, i)))",
        "Int/to_str(Int/ctz(Int/add(+48, i)))",
        "Int/to_str(Int/popcnt(Int/add(+255, i)))",
        "Flt/to_str(Flt/copysign(Flt/add(2.5, Int/to_flt(i)), -1.0))",
    ] {
        folded_matches_runtime(body);
    }
}

#[test]
fn overflowing_computations_trap_at_the_backend_boundary() {
    // Each expression is a valid u32/i32 computation whose value leaves the i31 envelope; the backend refuses to box it and traps instead of silently truncating (shl formerly truncated, the conversions formerly wrapped).
    for body in [
        "Nat/to_str(1073741824 + 1073741824 + n)",
        "Nat/to_str(Nat/mul(46341 + n, 46341))",
        "Nat/to_str(Nat/shl(1 + n, 31))",
        "Nat/to_str(Nat/rotl(1 + n, 31))",
        "Int/to_str(Int/rotl(Int/add(+1, i), +31))",
        "Int/to_str(Nat/to_int(1073741824 + n))",
    ] {
        runtime_traps(body);
    }
}

/// The domain half, which is no longer a runtime concern: a negative narrowed to `Nat` and a zero divisor are refused where they are written, because `/sys` states both as preconditions.
///
/// These two probes lived in the overflow list above and do not belong there — that list is about a *valid* computation whose value leaves the i31 envelope, a range fact the backend enforces at materialization. Out of domain is a different failure entirely, and it now has a different, earlier answer. `IntDiv` is the one operation in both categories: its precondition rules out the zero divisor, and signed overflow (`i32::MIN / -1`) remains the backend's.
#[test]
fn out_of_domain_computations_are_refused_where_they_are_written() {
    for (body, operation) in [
        ("Nat/to_str(Int/to_nat(Int/sub(i, +1)))", "/sys/Int/to_nat"),
        ("Nat/to_str(Nat/div(5 + n, n))", "/"),
    ] {
        let error = run_tainted(body).expect_err("expression should be refused");
        assert!(
            error.contains("was not inferred") && error.contains(operation),
            "expected {operation} to demand its precondition for {body}, got: {error}"
        );
    }
}

#[test]
fn folded_literal_outside_the_envelope_traps_at_materialization() {
    // `2^30 + 2^30` folds to the u32 constant `2^31` at compile time; adding the runtime zero keeps the literal alive to emission, where the i31 carrier cannot box it. Materialization is the backend boundary, so the program traps at runtime — it must not crash the compiler.
    runtime_traps("Nat/to_str(1073741824 + 1073741824 + n)");
}

#[test]
fn closed_computation_through_the_envelope_folds_in_u32() {
    // Fully constant programs are complete under the numeric law: partial evaluation carries the u32 value straight through `to_str`, so no out-of-envelope literal ever reaches the backend.
    assert_eq!(
        run("use /std/{Handle, Nat}; /std/print(Nat/to_str(1073741824 + 1073741824))"),
        b"2147483648"
    );
}

#[test]
fn carrier_bit_operations_compute_at_the_type_level() {
    // The 32-bit-carrier operations reduce on literals inside the u32/i32 view during conversion, so proofs can compute with them; a value outside the view stays neutral rather than folding wrongly.
    assert_eq!(
        run(r#"
        use /std/{Handle, Nat, Int, Eq};
        let p1 : Eq(Nat/rotl(2, 31), 1) = Eq/refl();
        let p2 : Eq(Nat/clz(1), 31) = Eq/refl();
        let p3 : Eq(Nat/popcnt(255), 8) = Eq/refl();
        let p4 : Eq(Int/rotr(+16, +4), +1) = Eq/refl();
        /std/print("ok")
        "#),
        b"ok"
    );
}

#[test]
fn a_literal_divisor_sees_through_a_symbolic_dividend() {
    // `/` and `%` join the floor seam `+`, `-`, and `*` already share, so a literal divisor reduces against an open term. Two unconditional laws do it: the floor law peels the whole divisors a literal floor certainly carries, and the split divides out a scaled symbol when every other summand is bounded below the divisor — which is exactly the shape a base-256 encoding produces, and what makes one provably injective.
    assert_eq!(
        run(r#"
        use /std/{Handle, Nat, Byte, Eq};
        -- The split: `b` cannot carry, because its carrier bounds it at 255.
        let hi : (x : Nat, b : Byte) -> Eq((256 * x + Byte/to_nat(b)) / 256, x) =
            (x, b) => Eq/refl();
        let lo : (x : Nat, b : Byte) -> Eq((256 * x + Byte/to_nat(b)) % 256, Byte/to_nat(b)) =
            (x, b) => Eq/refl();
        -- The floor law, with nothing bounded to split on.
        let floor : (x : Nat) -> Eq((x + 700) / 256, (x + 188) / 256 + 2) = (x) => Eq/refl();
        -- A remainder is below its divisor, for every dividend.
        let below : (x : Nat) -> Eq(x % 256 < 256, true) = (x) => Eq/refl();
        /std/print("ok")
        "#),
        b"ok"
    );
}
