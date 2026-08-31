//! Every folder computes in exact `u32`/`i32`, and the backend boundary appears only as a trap.

//! The numeric envelope gates: every constant folder computes in exact `u32`/`i32` (the numeric law), and the i31 backend boundary appears only as a trap in emitted Wasm — an overflowing computation traps, and a folded literal the carrier cannot box traps at its materialization point. The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics.

use {
    crate::tests::{compile, run, run_text, typecheck, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_runtime::MockHost,
};

use super::test_support::*;

#[test]
fn folded_and_executed_scalar_ops_agree_inside_the_envelope() {
    folded_matches_runtime(&[
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
        // Sign transfer.
        "Flt/to_str(Flt/copysign(Flt/add(2.5, Int/to_flt(i)), -1.0))",
        // `Flt/rem` is exact `fmod` in every folder, and the emitted Wasm must compute the same: it once expanded `x - trunc(x / y) * y` inline, which rounds at each step and disagreed with the fold on about half of all finite pairs — `1e8 % 3` was `1` folded and `0` executed, and `1 % inf` was `1` folded and NaN executed. Each row below is a pair the expansion got wrong.
        "Flt/to_str(Flt/rem(Flt/add(100000000.0, Nat/to_flt(n)), 3.0))",
        "Flt/to_str(Flt/rem(Flt/add(5.0, Nat/to_flt(n)), 0.1))",
        "Flt/to_str(Flt/rem(Flt/add(1.0, Nat/to_flt(n)), Flt/pos_inf))",
        "Flt/to_str(Flt/rem(Flt/sub(-7.0, Nat/to_flt(n)), 2.0))",
        // An equal pair under `min`/`max` answers by sign, as 754-2019's `minimum`/`maximum` do — and a NaN operand propagates rather than being dropped, which is where Rust's `f32::min` and Wasm's `f32.min` part company. The model defines all three answers, so these rows compare a fold to an execution rather than either to a host.
        "Flt/to_str(Flt/min(Nat/to_flt(n), Flt/neg(Nat/to_flt(n))))",
        "Flt/to_str(Flt/max(Flt/neg(Nat/to_flt(n)), Nat/to_flt(n)))",
        "Flt/to_str(Flt/min(Flt/add(Flt/nan, Nat/to_flt(n)), 1.0))",
        "Flt/to_str(Flt/max(1.0, Flt/add(Flt/nan, Nat/to_flt(n))))",
        // **The two canonicalizing sites**, and the only two operations whose non-NaN result can read a NaN's bits. The fold answers the one canonical NaN; without the emitter's canonicalization the engine answers whatever pattern it is holding, and these rows are what makes the two agree.
        //
        // The NaN is *assembled from bytes* rather than computed, and that is the whole design of these rows. A computed NaN — `0.0 / 0.0` — carries the hardware's default pattern, which on aarch64 is already `0x7fc00000`, so a row built on one passes whether or not the canonicalization is emitted. Reinterpreting a byte pattern the program chose is bit-preserving on every engine, so a payload bit set here reaches the instruction on any architecture. The tainted byte is what keeps the executed side from folding; the folded side canonicalizes in `Floating::from_bits`, so the two disagree unless the emitter closes it.
        //
        // **Both halves of that were measured rather than argued**, 2026-08-24 on aarch64-apple-darwin, by neutering the two `select`s in `code_emitter` and re-running this test. On a computed NaN it still passed — the row proved nothing. On the assembled NaN below it failed, folded `:0:0:192:127` against executed `:1:0:192:127`, which is the payload surviving into a result the model says has none. Reproduce by deleting the two selects; that failure is what these rows are for.
        "Bytes/fold(Flt/to_le_bytes(Flt/of_le_bytes(x[Nat/to_byte(n + 1), 0x00, 0xc0, 0x7f])), \"\", \
            (b, acc) => Str/concat(Str/concat(acc, \":\"), Nat/to_str(Byte/to_nat(b))))",
        // The sign operand is a *negative* non-canonical NaN, so an engine reading its sign bit answers `-1.0` where the model says `abs(1.0)`.
        "Flt/to_str(Flt/copysign(1.0, Flt/of_le_bytes(x[Nat/to_byte(n + 1), 0x00, 0xc0, 0xff])))",
        "Flt/to_str(Flt/copysign(-1.0, Flt/of_le_bytes(x[Nat/to_byte(n + 1), 0x00, 0xc0, 0xff])))",
    ]);
}

/// A reassociating pass may not change *which* programs trap, and multiplication is where it would.
///
/// `k(1)` is zero, so the product is zero and no partial of the written order — innermost-out, `((1 * k(1)) * k(2)) * k(3)` — ever exceeds it. The accumulator rebase in `curios-ersd` threads the addends the other way, and the reversed order reaches `2¹⁶ * 2¹⁶` before it meets the zero, which leaves the `u32` carrier and traps.
///
/// **Both spellings are the same program and the pair is the claim.** Binding the factor before the recursive call is the shape the rebase envelope accepts; using it inline puts the addend after the call, which the envelope declines. A rebase licensed by associativity alone makes the two disagree — this printed `0` and a trap when `NatMul` was a registered monoid — and the licence it actually needs is monotone definedness, which multiplication has not: see `curios-ersd`'s `optimize::rebase` and `documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md`.
///
/// **Falsifiable, and checked to be.** Re-admitting the `NatMul` row to that table fails this fixture on the first assertion and leaves the second passing, which is the disagreement itself rather than a program that merely stopped working.
#[test]
fn a_reassociated_product_agrees_with_the_written_one() {
    let program = |combine: &str| {
        format!(
            "use /std/{{Nat, Bool, Fmt, List, proc}};\n\
             let k(n: Nat) -> Nat = match n == 1 | true => 0 | false => 65536 end;\n\
             let prod(n: Nat) -> Nat =\n\
             match n | 0 => 1 | p + 1; _ => {combine} end;\n\
             Fmt/print(\"%\")(Nat/to_str(prod(List/len(proc/args!))))\n"
        )
    };
    let answer = |combine: &str| {
        let (system, io) = MockHost::builder().args(["a", "b", "c"]).build();
        run_text(&program(combine), system).expect("the product runs");
        io.output()
    };

    // The addend bound before the call: the rebase envelope accepts this one.
    assert_eq!(answer("let factor: Nat = k(p + 1); prod(p) * factor"), b"0");
    // The addend used inline, so it is computed after the call: the envelope declines this one.
    assert_eq!(answer("prod(p) * k(p + 1)"), b"0");
}

#[test]
fn overflowing_computations_trap_at_the_backend_boundary() {
    // Each expression is a valid computation whose value leaves the carrier; the backend refuses it and traps instead of silently answering something else. Three ways a shift used to answer something else are covered below, and each was a different defect: a truncated product, a masked count, and the signed envelope being one place narrower than the unsigned one.
    runtime_traps(&[
        "Nat/to_str(1073741824 + 1073741824 + n)",
        "Nat/to_str(Nat/mul(46341 + n, 46341))",
        "Nat/to_str(Nat/shl(1 + n, 31))",
        "Int/to_str(Nat/to_int(1073741824 + n))",
        // A count under 32 whose *product* leaves the 32-bit carrier: `2^30 << 15` is `2^45`, which
        // an `i32` shift truncates to zero — a result the old bit-31 test read as perfectly good,
        // because the bits it would have seen were already gone.
        "Nat/to_str(Nat/shl(1073741824 + n, 15))",
        // A count Wasm would reduce modulo the operand width, turning `<< 40` into `<< 8`.
        "Nat/to_str(Nat/shl(1 + n, 40))",
        "Int/to_str(Int/shl(Int/add(+1, i), +40))",
        // The signed envelope is `[-2^30, 2^30)`, so one place short of the unsigned one.
        "Int/to_str(Int/shl(Int/add(+1, i), +30))",
    ]);
}

/// A shift whose product leaves the *widened* intermediate as well, compiled both ways.
///
/// **The two axes above are each covered alone, and this is their product.** `2^30 << 15` is a large value with a small count and `1 << 40` a small value with a large count; widening the fold to `u64` answers both, and with a value of `1` no count can defeat it. Together they defeat it: `2^30 << 40` is `2^70`, whose low sixty-four bits are zero, so the truncated intermediate read back as a representable `0`. The folded half printed `0` while the executed half trapped — the same expression, two answers.
///
/// **It sits apart from the trap list because that list cannot see it.** [`runtime_traps`] compiles the tainted table only, so a row there exercises the backend and never the folder; the disagreement here is between the two. Both tables are therefore compiled, and both must trap. The fold's count is now clamped at the carrier's width, which is `curios-cont`'s `emit_clamped_shift` argument made one layer up: a nonzero value shifted that far has already left, so one count decides every larger one.
#[test]
fn a_shift_past_the_widened_intermediate_traps_folded_and_executed() {
    let rows = [
        "Nat/to_str(Nat/shl(1073741824 + n, 40))",
        "Int/to_str(Int/shl(Int/add(+536870912, i), +35))",
    ];

    for tainted in [false, true] {
        let compiled = compile(&table(&rows, tainted)).expect("the table compiles");
        for (index, row) in rows.iter().enumerate() {
            let error = run_row(&compiled, index).expect_err("the expression should trap");
            assert!(
                error.contains("execution failed"),
                "expected a trap for {row} (tainted: {tainted}), got: {error}"
            );
        }
    }
}

/// A shift count past the carrier's width answers the arithmetic, not Wasm's modulo.
///
/// **These agree by computing rather than by refusing, which is why they sit apart from the trap list.** `⌊v / 2^k⌋` is zero for every `k` at or above the width and every `v` the carrier holds, and zero is representable — so refusing here would refuse a value the theory has and the carrier can hold. `Natural`'s bignum shift in `curios-core` is the oracle: it answers zero, and both erased stages must too. Before the count was clamped rather than masked, `1024 >> 40` answered `4`.
///
/// The left shifts are here for the case the trap list cannot cover: shifting *zero* by a count past the width is still zero, so the count alone must not decide a refusal.
///
/// Mutation-checked against both halves of the emitter's shift lowering, and they separate. Masking the count instead of clamping it — Wasm's own reduction — moves this fixture and the trap list together. Restoring the old `i32` shift with its bit-31 test moves the trap list alone, since a truncated product is invisible to that test while a clamped count is unaffected by it. Neither moves [`folded_and_executed_scalar_ops_agree_inside_the_envelope`], whose rows all sit inside the carrier.
#[test]
fn a_shift_past_the_carrier_width_answers_the_arithmetic() {
    let rows = [
        ("Nat/to_str(Nat/shr(1024 + n, 40))", "0"),
        ("Nat/to_str(Nat/shr(1024 + n, 11))", "0"),
        ("Nat/to_str(Nat/shr(1024 + n, 3))", "128"),
        ("Nat/to_str(Nat/shl(0 + n, 40))", "0"),
        ("Int/to_str(Int/shr(Int/add(-65, i), +40))", "-1"),
        ("Int/to_str(Int/shr(Int/add(+1024, i), +40))", "+0"),
        ("Int/to_str(Int/shl(Int/add(+0, i), +40))", "+0"),
    ];
    let bodies = rows.iter().map(|(body, _)| *body).collect::<Vec<_>>();
    for ((body, expected), executed) in rows.iter().zip(folded_matches_runtime(&bodies)) {
        assert_eq!(executed, expected.as_bytes(), "wrong value for: {body}");
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
        let error = match compile(&table(&[body], true)) {
            Err(error) => error,
            Ok(_) => panic!("expression should be refused: {body}"),
        };
        assert!(
            error.contains("was not inferred") && error.contains(operation),
            "expected {operation} to demand its precondition for {body}, got: {error}"
        );
    }
}

#[test]
fn folded_literal_outside_the_envelope_traps_at_materialization() {
    // `2^30 + 2^30` folds to the u32 constant `2^31` at compile time; adding the runtime zero keeps the literal alive to emission, where the i31 carrier cannot box it. Materialization is the backend boundary, so the program traps at runtime — it must not crash the compiler.
    runtime_traps(&["Nat/to_str(1073741824 + 1073741824 + n)"]);
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

/// `Byte/of_nat` is the computed inverse of `to_nat`: a closed argument discharges its bound by reduction, an open one by refining `n < 256` at the call site, and past the bound the refusal is a typecheck fact rather than a runtime one.
#[test]
fn byte_of_nat_inverts_to_nat_and_refuses_the_bound() {
    // Closed: the comparison reduces, so the proof is written nowhere.
    let output = run(r#"
        use /std/{Byte, Nat, Str};
        /std/print(Nat/to_str(Byte/to_nat(Byte/of_nat(72))))
        "#);
    assert_eq!(output, b"72");

    // Open: the read keeps the argument out of the fold, and the refinement discharges the bound.
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Handle, Byte, Bytes, Nat, Str, Option};
        let bytes = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let n = Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0));
        match n < 256
        | true => /std/print(Nat/to_str(Byte/to_nat(Byte/of_nat(n))))
        | false => /std/print("out")
        end
        "#,
        system,
    )
    .expect("the refined conversion elaborates and runs");
    assert_eq!(io.output(), b"65");

    // Past the bound the proof has no inhabitant, so the literal is refused where it is written.
    assert!(
        typecheck_within(DEFAULT_STEP_BUDGET, "use /std/{Byte}; Byte/of_nat(256)").is_err(),
        "an out-of-range conversion typechecks nowhere"
    );
}

/// `Bytes/of_nat` emits minimal big-endian bytes — empty at zero, no leading zero, distinct per value — pinned across the byte-width boundaries.
#[test]
fn bytes_of_nat_is_minimal_big_endian() {
    let output = run(r#"
        use /std/{Bytes, Byte, Nat, Str, List};
        let probe(n: Nat) -> Str =
            let b = Bytes/of_nat(n);
            Str/concat(
                Nat/to_str(Bytes/len(b)),
                Bytes/fold(b, "", (byte, acc) =>
                    Str/concat(acc, Str/concat(":", Nat/to_str(Byte/to_nat(byte))))));
        /std/print(List/fold(
            [probe(0), probe(1), probe(255), probe(256), probe(65536), probe(65537)],
            "",
            (s, acc) => Str/concat(acc, Str/concat(s, " "))))
        "#);
    assert_eq!(output, b"0 1:1 1:255 2:1:0 3:1:0:0 3:1:0:1 ");
}

/// `0.0` and `-0.0` stay distinct terms — one NaN made bitwise identity *value* identity, and it did not merge the zeros. What the fold does make available is the IEEE comparison, which calls them numerically equal; conversion still refuses to identify the terms, which is what keeps `to_le_bytes` from telling apart two things the type level called the same.
#[test]
fn the_two_zeros_stay_distinct_terms_while_comparing_equal() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Str, Eq, Bool};
        let compares_equal: Eq(Flt/eql(+0.0, -0.0), true) = Eq/refl();
        /std/print(Str/concat(Flt/to_str(+0.0), Flt/to_str(-0.0)))
        "#),
        b"+0-0"
    );

    assert!(
        typecheck(
            r#"
        use /std/{Flt, Eq};
        let same: Eq(+0.0, -0.0) = Eq/refl();
        /std/print("")
        "#
        )
        .is_err(),
        "the two zeros are not the same term"
    );
}
