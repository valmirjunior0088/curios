//! Every folder and the running program compute what Core computes, at every magnitude, and a narrowing refuses rather than changing a value.

//! The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics on both sides of the i31, where the running program's form changes from an i31 to a boxed magnitude and its value must not.

use {
    crate::tests::{compile, run, run_text, typecheck, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_runtime::MockHost,
};

use super::test_support::*;

#[test]
fn folded_and_executed_scalar_ops_agree() {
    folded_matches_runtime(&[
        // Nat arithmetic at the top of the i31.
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
        "Int/to_str(Int/shl(Int/add(-3, i), 20))",
        "Int/to_str(Int/shr(Int/add(-65, i), 1))",
        // Carrier reinterpretations.
        "Int/to_str(Nat/to_int(1000000000 + n))",
        // Past the i31, where the running program's form changes and its value must not. Each of these used to refuse: a sum and a product past `2³¹`, a left shift whose product an `i32` would have truncated (`2³⁰ << 15`), a count Wasm would have reduced modulo the width (`<< 40`), the signed range one place short of the unsigned one (`+1 << 30`), and a float truncated past `2³¹` on each side of what `i32.trunc_f64_*` holds.
        "Nat/to_str(1073741824 + 1073741824 + n)",
        "Nat/to_str(Nat/mul(46341 + n, 46341))",
        "Nat/to_str(Nat/shl(1 + n, 31))",
        "Int/to_str(Nat/to_int(1073741824 + n))",
        "Nat/to_str(Nat/shl(1073741824 + n, 15))",
        "Nat/to_str(Nat/shl(1 + n, 40))",
        "Int/to_str(Int/shl(Int/add(+1, i), 40))",
        "Int/to_str(Int/shl(Int/add(+1, i), 30))",
        "Nat/to_str(Option/unwrap_or(Flt/try_to_nat(Flt/mul(Nat/to_flt(n + 3), 1.0e9)), 0))",
        "Nat/to_str(Option/unwrap_or(Flt/try_to_nat(Flt/mul(Nat/to_flt(n + 5), 1.0e9)), 0))",
        "Int/to_str(Option/unwrap_or(Flt/try_to_int(Flt/mul(Nat/to_flt(n + 2), -1.0e9)), +0))",
        "Int/to_str(Option/unwrap_or(Flt/try_to_int(Flt/mul(Nat/to_flt(n + 3), -1.0e9)), +0))",
        // Past a limb, past 64 bits, and back down: a boxed operand in every operation's slow path.
        "Nat/to_str(Nat/div(Nat/mul(4294967296 + n, 4294967297), 65537))",
        "Nat/to_str(Nat/rem(Nat/mul(18446744073709551615 + n, 18446744073709551615), 4294967291))",
        "Int/to_str(Int/mul(Int/sub(i, 9223372036854775808), Int/add(+3, i)))",
        "Int/to_str(Int/div(Int/sub(i, 340282366920938463463374607431768211455), -18446744073709551616))",
        "Int/to_str(Int/rem(Int/sub(i, 340282366920938463463374607431768211455), -18446744073709551616))",
        "Int/to_str(Int/and(Int/sub(i, 18446744073709551616), +4294967295))",
        "Int/to_str(Int/xor(Int/sub(i, 1180591620717411303424), +1180591620717411303423))",
        "Int/to_str(Int/shr(Int/sub(i, 1180591620717411303425), 70))",
        "Flt/to_str(Nat/to_flt(Nat/shl(9007199254740993 + n, 40)))",
        "Flt/to_str(Int/to_flt(Int/sub(i, 1180591620717411303424)))",
        // Guarded on `>= +0`, the comparison `Int/to_nat`'s precondition is decided on: `i` is runtime-tainted, so nothing settles the sign statically and the narrowing demands evidence. Both arms fold identically at the literal `i`, so the differential still compares the conversion rather than the guard.
        "Nat/to_str(to_nat_or(Int/add(+12345, i), 0))",
        // Sign transfer.
        "Flt/to_str(Flt/copysign(Flt/add(2.5, Int/to_flt(i)), -1.0))",
        // `Flt/rem` is exact `fmod` in every folder, and the emitted Wasm must compute the same: it once expanded `x - trunc(x / y) * y` inline, which rounds at each step and disagreed with the fold on about half of all finite pairs — `1e8 % 3` was `1` folded and `0` executed, and `1 % inf` was `1` folded and NaN executed. Each row below is a pair the expansion got wrong.
        "Flt/to_str(Flt/rem(Flt/add(100000000.0, Nat/to_flt(n)), 3.0))",
        "Flt/to_str(Flt/rem(Flt/add(5.0, Nat/to_flt(n)), 0.1))",
        "Flt/to_str(Flt/rem(Flt/add(1.0, Nat/to_flt(n)), Flt/pos_inf))",
        "Flt/to_str(Flt/rem(Flt/sub(-7.0, Nat/to_flt(n)), 2.0))",
        // An equal pair under `min`/`max` answers by sign, as 754-2019's `minimum`/`maximum` do — and a NaN operand propagates rather than being dropped, which is where Rust's `f64::min` and Wasm's `f64.min` part company. The model defines all three answers, so these rows compare a fold to an execution rather than either to a host.
        "Flt/to_str(Flt/min(Nat/to_flt(n), Flt/neg(Nat/to_flt(n))))",
        "Flt/to_str(Flt/max(Flt/neg(Nat/to_flt(n)), Nat/to_flt(n)))",
        "Flt/to_str(Flt/min(Flt/add(Flt/nan, Nat/to_flt(n)), 1.0))",
        "Flt/to_str(Flt/max(1.0, Flt/add(Flt/nan, Nat/to_flt(n))))",
    ]);
}

/// The NaN rule, folded and executed: an operation over NaN operands answers the greatest of their quieted patterns whatever their order, an invalid one over none answers the default NaN, and the sign operations and the byte conversions carry every pattern as it is. Each row prints the result's bytes, which is the one observation that reads a NaN's sign and payload.
///
/// The NaN operands are *assembled from bytes* rather than computed, with a runtime-tainted low byte, and that is the whole design of the table. A computed NaN carries whatever pattern the engine chose, so a row built on one could pass by coincidence of hardware; reinterpreting a pattern the program chose is bit-preserving on every engine, so a payload set here reaches the instruction on any architecture, and the tainted byte keeps the executed side from folding. The computed rows are the invalid operations, whose answer the model fixes as the default NaN where x86 and aarch64 disagree on the sign — so on an x86 host each is a row the check after the instruction has to win.
///
/// **Measured, not argued**, 2026-09-22 on x86_64-unknown-linux-gnu: with `emit_flt_checked`'s NaN arm replaced by the hardware result, the first row to fail was `add(signaling, quiet)` — the engine answered its first NaN operand quieted, `:1:0:0:0:0:0:248:127`, where the model answers the greater pattern, `:2:18:0:0:0:0:248:255`. Reproduce by making that arm `vec![get(&result)]`.
#[test]
fn folded_and_executed_nans_agree() {
    let bytes = |term: &str| {
        format!(
            "Bytes/fold(Flt/to_le_bytes({term}), \"\", (b, acc) => \
             Str/concat(Str/concat(acc, \":\"), Nat/to_str(Byte/to_nat(b))))"
        )
    };
    // A signaling NaN with a payload of one, positive, and a quiet one with a wider payload, negative.
    let signaling =
        "Flt/of_le_bytes(x[Nat/to_byte((n + 1) % 256), 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x7f])";
    let quiet =
        "Flt/of_le_bytes(x[Nat/to_byte((n + 2) % 256), 0x12, 0x00, 0x00, 0x00, 0x00, 0xf8, 0xff])";
    let zero = "Nat/to_flt(n)";

    let rows = [
        bytes(signaling),
        bytes(&format!("Flt/of_le_bytes(Flt/to_le_bytes({quiet}))")),
        bytes(&format!("Flt/add({signaling}, 1.0)")),
        bytes(&format!("Flt/add(1.0, {quiet})")),
        bytes(&format!("Flt/add({signaling}, {quiet})")),
        bytes(&format!("Flt/add({quiet}, {signaling})")),
        bytes(&format!("Flt/mul({signaling}, {quiet})")),
        bytes(&format!("Flt/sub(1.0, {signaling})")),
        bytes(&format!("Flt/sub({quiet}, 1.0)")),
        bytes(&format!("Flt/div({signaling}, 2.0)")),
        bytes(&format!("Flt/rem({signaling}, 3.0)")),
        bytes(&format!("Flt/min({signaling}, {quiet})")),
        bytes(&format!("Flt/max(1.0, {signaling})")),
        bytes(&format!("Flt/sqrt({signaling})")),
        bytes(&format!("Flt/floor({quiet})")),
        bytes(&format!("Flt/nearest({signaling})")),
        bytes(&format!("Flt/neg({signaling})")),
        bytes(&format!("Flt/abs({quiet})")),
        bytes(&format!("Flt/copysign({signaling}, -1.0)")),
        bytes(&format!("Flt/div({zero}, {zero})")),
        bytes(&format!("Flt/sub(Flt/pos_inf, Flt/add(Flt/pos_inf, {zero}))")),
        bytes(&format!("Flt/mul(Flt/add(Flt/pos_inf, {zero}), 0.0)")),
        bytes(&format!("Flt/sqrt(Flt/sub(-1.0, {zero}))")),
        bytes(&format!("Flt/rem(1.0, {zero})")),
        "Flt/to_str(Flt/copysign(1.0, Flt/of_le_bytes(x[Nat/to_byte((n + 1) % 256), 0x00, 0x00, 0x00, \
            0x00, 0x00, 0xf8, 0xff])))"
            .to_string(),
    ];
    let rows = rows.iter().map(String::as_str).collect::<Vec<_>>();

    let executed = folded_matches_runtime(&rows);
    let quieted = b":1:0:0:0:0:0:248:127".as_slice();
    let default = b":0:0:0:0:0:0:248:127".as_slice();

    assert_eq!(
        executed[0], b":1:0:0:0:0:0:240:127",
        "a signaling NaN crosses its bytes unquieted"
    );
    assert_eq!(executed[2], quieted, "an operand NaN answers quieted");
    assert_eq!(
        executed[4], executed[5],
        "the choice does not read the operands' order"
    );
    assert_eq!(
        executed[4], b":2:18:0:0:0:0:248:255",
        "the greater quieted pattern, read unsigned"
    );
    assert_eq!(
        executed[7], b":1:0:0:0:0:0:248:255",
        "a difference negates its subtrahend first"
    );
    assert_eq!(
        executed[19], default,
        "an invalid operation over no NaN answers the default NaN"
    );
    assert_eq!(executed[24], b"-1", "copysign reads a NaN's sign");
}

/// A reassociated product answers what the written one does, however large its partial products grow.
///
/// `k(1)` is zero, so the product is zero, while the accumulator rebase in `curios-ersd` threads the factors the other way and multiplies `65536 · 65536` before it meets the zero. That once turned a program that computed into one that refused, which is why multiplication was kept out of the rebase; nothing refuses a size now, so the row is registered and the two orders differ only in what their partials build.
///
/// **Both spellings are the same program and the pair is the claim.** Binding the factor before the recursive call is the shape the rebase envelope accepts; using it inline puts the addend after the call, which the envelope declines. Both answer `0`.
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

/// A shift far past the i31 answers the exact arithmetic, folded and executed alike.
///
/// **This used to disagree, and then to refuse.** With `u32` carriers a fold had a width of its own, and `2^30 << 40` — `2^70`, whose low sixty-four bits are zero — read back through a widened `u64` intermediate as a representable `0` while the executed half trapped; once the carriers were unbounded the fold computed `2^70` and the executed half refused to box it. Now both compute it, so the rows pin the value as well as the agreement.
#[test]
fn a_shift_past_the_i31_answers_the_arithmetic_folded_and_executed() {
    let rows = [
        (
            "Nat/to_str(Nat/shl(1073741824 + n, 40))",
            "1180591620717411303424",
        ),
        (
            "Int/to_str(Int/shl(Int/add(+536870912, i), 35))",
            "+18446744073709551616",
        ),
        (
            "Int/to_str(Int/shl(Int/add(-536870912, i), 35))",
            "-18446744073709551616",
        ),
    ];
    let bodies = rows.iter().map(|(body, _)| *body).collect::<Vec<_>>();
    for ((body, expected), executed) in rows.iter().zip(folded_matches_runtime(&bodies)) {
        assert_eq!(executed, expected.as_bytes(), "wrong value for: {body}");
    }
}

/// A shift count past the carrier's width answers the arithmetic, not Wasm's modulo.
///
/// **These agree by computing rather than by refusing, which is why they sit apart from the trap list.** `⌊v / 2^k⌋` is zero for every `k` at or above the width and every `v` the carrier holds, and zero is representable — so refusing here would refuse a value the theory has and the carrier can hold. `Natural`'s bignum shift in `curios-core` is the oracle: it answers zero, and both erased stages must too. Before the count was clamped rather than masked, `1024 >> 40` answered `4`.
///
/// The left shifts are here for the case the trap list cannot cover: shifting *zero* by a count past the width is still zero, so the count alone must not decide a refusal.
///
/// The fast path clamps a right shift's count to 31, where any i31 has become its sign, and takes a left shift's fast path only below 32; a count past either goes to the big-number helpers, which answer the same.
#[test]
fn a_shift_past_the_carrier_width_answers_the_arithmetic() {
    let rows = [
        ("Nat/to_str(Nat/shr(1024 + n, 40))", "0"),
        ("Nat/to_str(Nat/shr(1024 + n, 11))", "0"),
        ("Nat/to_str(Nat/shr(1024 + n, 3))", "128"),
        ("Nat/to_str(Nat/shl(0 + n, 40))", "0"),
        ("Int/to_str(Int/shr(Int/add(-65, i), 40))", "-1"),
        ("Int/to_str(Int/shr(Int/add(+1024, i), 40))", "+0"),
        ("Int/to_str(Int/shl(Int/add(+0, i), 40))", "+0"),
    ];
    let bodies = rows.iter().map(|(body, _)| *body).collect::<Vec<_>>();
    for ((body, expected), executed) in rows.iter().zip(folded_matches_runtime(&bodies)) {
        assert_eq!(executed, expected.as_bytes(), "wrong value for: {body}");
    }
}

/// The domain half, which is no longer a runtime concern: a negative narrowed to `Nat` and a zero divisor are refused where they are written, because `/sys` states both as preconditions.
///
/// Out of domain is the one failure an operation on a `Nat` or `Int` has, and its answer is at the type level: the zero divisor and the negative narrowing are ruled out by the preconditions the operations carry, so neither reaches the running program.
#[test]
fn out_of_domain_computations_are_refused_where_they_are_written() {
    for (body, operation) in [
        ("Nat/to_str(Int/to_nat(Int/sub(i, +1)))", "Int/to_nat"),
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

/// `2^30 + 2^30` folds to `2^31` at compile time, exactly as Core computes it, and adding the runtime zero keeps the literal alive to emission, where it materializes as a boxed magnitude built from its limbs — once a refusal, now a constant.
#[test]
fn a_folded_literal_past_the_i31_materializes_as_its_value() {
    assert_eq!(
        folded_matches_runtime(&["Nat/to_str(1073741824 + 1073741824 + n)"]),
        [b"2147483648".to_vec()]
    );
}

#[test]
fn a_closed_computation_folds_at_the_theory_s_width() {
    // The erased carriers are unbounded, so a fold answers what Core answers. This once demonstrated a `u32` band between the folders and the runtime's width; there is no band now, and what it demonstrates is that the fold and the theory agree.
    assert_eq!(
        run("use /std/{Nat, Io}; /std/print(Nat/to_str(1073741824 + 1073741824))"),
        b"2147483648"
    );
}

/// A growing fold declines rather than building a numeral past its allowance, and the program it leaves standing computes the value at run time instead.
///
/// **The bounded carrier was closing this for free and nothing upstream closes it.** `curios-core` charges every reduction step against a budget, but nothing demands the value of a `Nat/shl` in a term position, so the shift reaches erasure unreduced — `wonder stage ersd` shows the call arriving with both operands literal. Unbounded, a fold would be asked for a forty-million-bit numeral; the allowance declines instead, and a decline is invisible: the operation stays, and the running program builds the five megabytes the value is and reads its top bits back.
#[test]
fn a_growing_fold_declines_and_the_running_program_computes_the_value() {
    assert_eq!(
        folded_matches_runtime(&["Nat/to_str(Nat/shr(Nat/shl(1, 40000000) + n, 39999999))"]),
        [b"2".to_vec()]
    );
}

#[test]
fn a_literal_divisor_sees_through_a_symbolic_dividend() {
    // `/` and `%` join the floor seam `+`, `-`, and `*` already share, so a literal divisor reduces against an open term. Two unconditional laws do it: the floor law peels the whole divisors a literal floor certainly carries, and the split divides out a scaled symbol when every other summand is bounded below the divisor — which is exactly the shape a base-256 encoding produces, and what makes one provably injective.
    assert_eq!(
        run(r#"
        use /std/{Nat, Byte, Eq, Io};
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

/// `Nat/to_byte` is the computed inverse of `to_nat`: a closed argument discharges its bound by reduction, an open one by refining `n < 256` at the call site, and past the bound the refusal is a typecheck fact rather than a runtime one.
///
/// **The third case is what the domain bought.** The operation used to mask, so `Nat/to_byte(256)` compiled and answered `0` — a narrowing that changed a value, which is the one thing a narrowing may not do. It is now refused where it is written, and the two above say the refusal costs nothing a correct program was doing.
#[test]
fn nat_to_byte_inverts_to_nat_and_refuses_the_bound() {
    // Closed: the comparison reduces, so the proof is written nowhere.
    let output = run(r#"
        use /std/{Byte, Nat, Str};
        /std/print(Nat/to_str(Byte/to_nat(Nat/to_byte(72))))
        "#);
    assert_eq!(output, b"72");

    // Open: the read keeps the argument out of the fold, and the refinement discharges the bound.
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Byte, Bytes, Nat, Str, Option, Io};
        let bytes = match Io/read(Io/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let n = Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0));
        match n < 256
        | true => /std/print(Nat/to_str(Byte/to_nat(Nat/to_byte(n))))
        | false => /std/print("out")
        end
        "#,
        system,
    )
    .expect("the refined conversion elaborates and runs");
    assert_eq!(io.output(), b"65");

    // Past the bound the proof has no inhabitant, so the literal is refused where it is written.
    assert!(
        typecheck_within(DEFAULT_STEP_BUDGET, "use /std/{Nat}; Nat/to_byte(256)").is_err(),
        "an out-of-range conversion typechecks nowhere"
    );
}

/// `Bytes/of_nat` emits minimal bytes least-significant first, as the carrier reads — empty at zero, no trailing zero, distinct per value — pinned across the byte-width boundaries.
///
/// `65537` is the row that would pass either way: its encoding is a palindrome, so the boundary values on both sides of it are what actually witness the direction.
#[test]
fn bytes_of_nat_is_minimal_least_significant_first() {
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
    assert_eq!(output, b"0 1:1 1:255 2:0:1 3:0:0:1 3:1:0:1 ");
}

/// `0.0` and `-0.0` stay distinct terms — identity is the bit pattern, and the zeros are two. What the fold does make available is the IEEE comparison, which calls them numerically equal; conversion still refuses to identify the terms, which is what keeps `to_le_bytes` from telling apart two things the type level called the same.
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

/// A dispatch *key* narrows at erasure; a dispatch *value* no longer does.
///
/// **These were one boundary and are now two different things.** A written numeral used to narrow into the erased carriers at erasure and refuse what a `u32` could not hold. The carriers are unbounded now, so a literal crosses whole — but a **case key** is not a value: it selects an arm of a branch table, which is a slot. That narrowing stays, and it is the last one at this boundary.
///
/// The dispatch half also guards a defect worth keeping named: `curios-text` once narrowed the key in the parser, four stages above this, and the failure backtracked — a digit run is an identifier, so an oversized case fell past every `Nat` leaf to a plain `Binder`. `match n | 4294967296 => 7 end` compiled to `let 4294967296 = n; 7`, a match that dispatches on nothing and takes its one arm for every input, and printed `7` for `f(0)`. Core keys the switch by its unbounded value and the width is chosen here alone — see [Nat and Int are an i31 until they outgrow it](../../../../documentation/design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md).
#[test]
fn a_dispatch_key_past_a_branch_table_refuses_where_a_value_does_not() {
    let refusal = |source: &str| {
        typecheck(source)
            .expect_err("a numeral past the erased carrier is refused")
            .to_string()
    };

    // A literal in an ordinary term is a *value*, and narrows nowhere.
    assert!(
        typecheck(
            r#"
        use /std/{Nat, Bool};
        let f(n : Nat) -> Bool = n == 4294967296;
        /std/print("unreachable")
        "#
        )
        .is_ok(),
        "a numeral past every machine word is a value, not an error"
    );

    // The same numeral as a dispatch case, which used to become a binder instead.
    assert!(
        refusal(
            r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = match n | 0 => 1 | 4294967296 => 2 | _ => 0 end;
        /std/print(Nat/to_str(f(0)))
        "#
        )
        .contains("does not fit a branch table")
    );

    // A dispatch at the very top of the carrier still compiles and answers.
    assert_eq!(
        run(r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = match n | 4294967295 => 7 | _ => 0 end;
        /std/print(Nat/to_str(f(4294967295)))
        "#),
        b"7"
    );
}

/// A key the i31 does not hold is decided by equality rather than by the branch table, so a value past the i31 meets it exactly.
///
/// A `Switch` reads its scrutinee as a machine word, and a `Nat` narrows to one exactly below `2³² - 1` and saturates there. So `2³²` read as a word is `4294967295`, which is a key a program may write; the lowering keeps every key at or past `2³⁰` out of the table and tests it for equality, and the third row is the one that would answer `7` if it did not.
#[test]
fn a_dispatch_key_past_the_i31_matches_exactly_at_run_time() {
    let rows = [
        (
            "Nat/to_str(match 1073741824 + n | 1073741824 => 7 | 5 => 1 | _ => 0 end)",
            "7",
        ),
        (
            "Nat/to_str(match 4294967295 + n | 4294967295 => 7 | _ => 0 end)",
            "7",
        ),
        (
            "Nat/to_str(match 4294967296 + n | 4294967295 => 7 | _ => 0 end)",
            "0",
        ),
        (
            "Nat/to_str(match 5 + n | 1073741824 => 7 | 5 => 1 | _ => 0 end)",
            "1",
        ),
    ];
    let bodies = rows.iter().map(|(body, _)| *body).collect::<Vec<_>>();
    for ((body, expected), executed) in rows.iter().zip(folded_matches_runtime(&bodies)) {
        assert_eq!(executed, expected.as_bytes(), "wrong value for: {body}");
    }
}
