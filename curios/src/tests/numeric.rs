//! The numeric envelope gates: every constant folder computes in exact `u32`/`i32` (the numeric law), and the i31 backend boundary appears only as a trap in emitted Wasm — an overflowing computation traps, and a folded literal the carrier cannot box traps at its materialization point. The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics.

use {
    super::{run, run_text, typecheck, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
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
        let n = Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)), 65);
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

// The control half of a minimal pair over the unfolding rule. `f`'s base arm returns a literal, so `f(0, n)` reduces to an `Intrinsic`-headed term, `force_rec` keeps that reduct, and the decided `Nat/Le` discharges by reduction. Identical in every other respect to the refused half below, which differs only in what the base arm returns.
#[test]
fn a_bound_over_a_recursion_returning_a_literal_discharges() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Nat};
        rec f(k : Nat, n : Nat) -> Nat =
            match k | 0 => 5 | j + 1; ih => f(j, n) end;
        let bound(n : Nat) -> Nat/Le(5, f(0, n)) = Nat/Le/refl(5);
        /std/print("ok")
        "#),
        b"ok"
    );
}

// The other half: the same shape with a base arm returning a *parameter*. `f(0, n)` reduces correctly to `n`, and `force_rec` discards that reduct for being `Var`-headed — its head-shape test cannot tell a stuck form from an answer that happens to be a variable — so the bound is left standing as `Nat/Le(n, f(0, n))` and refused. Returning one's own parameter is the ordinary shape of an accumulator, which is why this is easy to hit.
//
// Ignored until that specification's M1 lands. It is the acceptance check: this compiling, with the control above still compiling, is what the rule change has to achieve.
#[test]
fn a_bound_over_a_recursion_returning_a_parameter_discharges() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Nat};
        rec f(k : Nat, n : Nat) -> Nat =
            match k | 0 => n | j + 1; ih => f(j, n) end;
        let bound(n : Nat) -> Nat/Le(n, f(0, n)) = Nat/Le/refl(n);
        /std/print("ok")
        "#),
        b"ok"
    );
}

// **A window's bound is stated over a sum, and a guard still discharges it.** `Bytes/slice(b, s, l)` demands `s + l <= len(b)`, so the proposition a guard has to meet contains an addition that folds away — `0 + 10` to `10`, `1 + k` to `k + 1` — and the fold happens inside the intrinsic reduction, one step *after* the refinement store is probed. Keying a probe on the operands as written therefore missed every window bound the moment the window became `(start, length)`; `canonical_scrutinee` reduces an intrinsic's operands for exactly this reason, and these are the shapes that say so.
//
// The control is the third: a guard establishing a *different* window must not discharge this one, or the escalation would be collapsing comparisons rather than spellings of one.
#[test]
fn a_guard_discharges_a_window_bound_stated_over_a_sum() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Bytes, Nat};
        let head(b : Bytes) -> Bytes =
            match 10 <= Bytes/len(b) | true => Bytes/slice(b, 0, 10) | false => x[] end;
        let interior(b : Bytes, k : Nat) -> Bytes =
            match 1 + k <= Bytes/len(b) | true => Bytes/slice(b, 1, k) | false => x[] end;
        /std/print("ok")
        "#),
        b"ok"
    );

    let error = typecheck(
        r#"
        use /std/{Handle, Str, Bytes, Nat};
        let mismatched(b : Bytes, k : Nat) -> Bytes =
            match 1 + k <= Bytes/len(b) | true => Bytes/slice(b, 2, k) | false => x[] end;
        /std/print("unreachable")
        "#,
    )
    .expect_err("a guard over one window does not discharge another");

    assert!(
        error.contains("was not inferred"),
        "expected an uninferred window bound, got: {error}"
    );
}

// A bound whose subject is a *computed* value is discharged by evaluating that value, at elaboration time. `Bytes/slice` states `10 <= Bytes/len(b)`, so `Bytes/slice(built, 0, 10)` puts `go(100000, x[])` in a type and the compiler runs the loop. A hundred thousand iterations costs about seventeen million reduction steps — sixteen times the default budget — so the refusal below is the budget doing exactly its job, and the small figure stated here only makes the fixture cheap.
//
// **What this used to pin was something worse, and the difference is the point.** The budget counted transitions, and the memory a reduction allocated was bounded by nothing: fusing an all-literal concatenation recopied the whole accumulator every step, so the same program spent a quadratic volume of construction against a linear step count and exhausted the machine rather than refusing. `curios-core`'s `FUSION_CAP` and its measure removed that, and `curios`'s `tests::reduction` holds the figures. What is left is an ordinary bounded computation that happens to be bigger than the default allowance.
//
// The trio below is what isolates the cause. All three build the same value; they differ in whether the bound's subject is that value or a parameter standing for it, and in how much of it there is.
#[test]
fn a_bound_on_a_computed_subject_evaluates_it() {
    let error = typecheck_within(
        500_000,
        r#"
        use /std/{Handle, Bytes, Nat, Str};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")]) end;
        let built = go(100000, x[]);
        let head = Bytes/slice(built, 0, 10);
        /std/print("unreachable")
        "#,
    )
    .expect_err("the bound's subject is evaluated, and cannot finish inside this budget");

    assert!(
        error.contains("ran out of steps"),
        "expected a spent-budget refusal, got: {error}"
    );
}

// **The one the campaign bought: the obvious spelling, at a size the ordinary budget admits.** No helper stands between the bound and its computed subject — the compiler runs the loop, measures what it built, and discharges `10 <= Bytes/len(built)` from the result. This is what a user reaching for `Bytes/slice` on a computed value writes, and it now works; what decides whether it works is the ordinary reduction budget, on a cost linear in the iteration count, rather than how much memory the host happens to have.
#[test]
fn a_bound_on_a_small_computed_subject_discharges() {
    typecheck_within(
        DEFAULT_STEP_BUDGET,
        r#"
        use /std/{Handle, Bytes, Nat, Str};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")]) end;
        let built = go(2000, x[]);
        let head = Bytes/slice(built, 0, 10);
        /std/print("ok")
        "#,
    )
    .expect("a computed subject the budget can afford discharges its own bound");
}

// The shared figure above and below is *priced reduction work*, not transitions, and it moved with the pricing rather than being preserved: 50 000 transitions became 500 000 units. What the pair asserts is the contrast, and the contrast is untouched — the computed spelling is refused and the opaque one is ample, at one budget.
//
// The control: the same program with the bound read off a parameter. `b` is opaque behind `head_of`, the guard refines it once and generically, and nothing computes — so the identical budget that the hundred-thousand-iteration spelling cannot finish inside is ample here. It says what it always meant: that opacity costs *nothing*, not that opacity is how a computed subject survives — and this is now the helper's last home, `tests::runtime`'s accumulation measurement having returned to the direct spelling once the closed machine made evaluating its subject an ordinary cost.
#[test]
fn a_bound_behind_a_parameter_evaluates_nothing() {
    typecheck_within(
        500_000,
        r#"
        use /std/{Handle, Bytes, Nat, Str};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")]) end;
        let head_of(b : Bytes) -> Bytes =
            match 10 <= Bytes/len(b) | true => Bytes/slice(b, 0, 10) | false => x[] end;
        let built = go(100000, x[]);
        let head = head_of(built);
        /std/print("ok")
        "#,
    )
    .expect("a bound over an opaque parameter reduces nothing");
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
