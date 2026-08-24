//! The numeric envelope gates: every constant folder computes in exact `u32`/`i32` (the numeric law), and the i31 backend boundary appears only as a trap in emitted Wasm — an overflowing computation traps, and a folded literal the carrier cannot box traps at its materialization point. The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics.

use {
    super::{Compiled, compile, run, run_text, typecheck, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_runtime::MockHost,
};

/// One program holding every row of a table, the row chosen by the host: its stdin line is two bytes, the first read as the runtime zero `n` (`'A' − 65`) with its `Int` twin `i`, which is what keeps every row out of the folder's reach, and the second naming the row. One compile therefore serves the whole table, and a run costs milliseconds — the reason this is a selector rather than one program per row. With `taint` false, `n` and `i` are literal zeros instead, so every row folds to a literal while the selection alone stays runtime, which is what "folded" asserts.
fn table(rows: &[&str], taint: bool) -> String {
    let zero = if taint {
        "Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)), 65)"
    } else {
        "0"
    };
    let arms = rows
        .iter()
        .enumerate()
        .map(|(index, row)| format!("| {index} => /std/print({row})"))
        .collect::<Vec<_>>()
        .join("\n        ");
    format!(
        r#"
        use /std/{{Handle, Nat, Int, Flt, Byte, Bytes, Str, Option}};
        let bytes = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let n = {zero};
        let i = Nat/to_int(n);
        let to_nat_or(x : Int, d : Nat) -> Nat =
            match x >= +0 | true => Int/to_nat(x) | false => d end;
        let row = Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 1), 0)), 32);
        match row
        {arms}
        | _ => /std/print("no such row")
        end
        "#
    )
}

/// Run row `index` of a compiled table: the taint byte, then the row byte, printable so the line stays one.
fn run_row(compiled: &Compiled, index: usize) -> Result<Vec<u8>, String> {
    let row = u8::try_from(index).expect("a table of under 95 rows") + b' ';
    let (system, io) = MockHost::builder().stdin_lines([[b'A', row]]).build();
    compiled.run(system)?;
    Ok(io.output())
}

/// Compile `rows` closed and tainted — two compiles for the table — and assert each row's folded and executed answers agree byte-for-byte. The executed answers are returned, for a fixture that also pins what they are.
fn folded_matches_runtime(rows: &[&str]) -> Vec<Vec<u8>> {
    let folded = compile(&table(rows, false)).expect("the closed table compiles");
    let executed = compile(&table(rows, true)).expect("the tainted table compiles");
    rows.iter()
        .enumerate()
        .map(|(index, row)| {
            let folded = run_row(&folded, index).expect("a folded row runs");
            let executed = run_row(&executed, index).expect("in-envelope expression executes");
            assert_eq!(folded, executed, "fold/runtime disagreement on: {row}");
            executed
        })
        .collect()
}

/// Compile `rows` tainted and assert each traps at the backend boundary when it is the row selected.
fn runtime_traps(rows: &[&str]) {
    let executed = compile(&table(rows, true)).expect("the tainted table compiles");
    for (index, row) in rows.iter().enumerate() {
        let error = run_row(&executed, index).expect_err("expression should trap");
        assert!(
            error.contains("execution failed"),
            "expected a runtime trap for {row}, got: {error}"
        );
    }
}

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
        // An equal pair under `min`/`max` answers by sign, as `f32.min`/`f32.max` do: the folder spells the rule rather than trusting the host's `min`. Both orders are the folder's own unit test; one each here is the runtime half.
        "Flt/to_str(Flt/min(Nat/to_flt(n), Flt/neg(Nat/to_flt(n))))",
        "Flt/to_str(Flt/max(Flt/neg(Nat/to_flt(n)), Nat/to_flt(n)))",
    ]);
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
// `over_a_definition` is the shape that needs the *key* reduced rather than merely rewritten: the base is a local definition, so the probe unfolds it to the literal and cancels the shared floor while the registered key holds neither. `canonical_key` settles that under a ceiling, once per key. `indexed` is the same story for `Bytes/get`'s strict bound.
//
// The control is the last: a guard establishing a *different* window must not discharge this one, or the escalation would be collapsing comparisons rather than spellings of one.
#[test]
fn a_guard_discharges_a_window_bound_stated_over_a_sum() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Bytes, Nat};
        let head(b : Bytes) -> Bytes =
            match 10 <= Bytes/len(b) | true => Bytes/slice(b, 0, 10) | false => x[] end;
        let interior(b : Bytes, k : Nat) -> Bytes =
            match 1 + k <= Bytes/len(b) | true => Bytes/slice(b, 1, k) | false => x[] end;
        let named = x[0x61, 0x62, 0x63];
        let over_a_definition(k : Nat) -> Bytes =
            match 1 + k <= Bytes/len(named)
            | true => Bytes/slice(named, 1, k)
            | false => x[]
            end;
        let indexed(b : Bytes, i : Nat) -> Bytes =
            match i < Bytes/len(b) | true => x[Bytes/get(b, i)] | false => x[] end;
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

/// **The two narrowings out of `Flt` state their domains, and a guard discharges them.** `Flt/to_nat` demands `/syn/Flt/NonNeg` and `Flt/to_int` demands `/syn/Flt/Finite`, both decided over the raw comparisons — so refining the scrutinee is what makes the obligation reduce to `True`, exactly as `Int/to_nat`'s bound does.
///
/// The `try_` forms are the same discharge routed through `/std/Flt`'s deciders, which is the shape a caller who cannot guard in place reaches for. A closed literal is deliberately *not* probed here: it needs the fold, which is the next commit's, and this fixture is what says the bounds stand without it.
#[test]
fn a_flt_narrowing_bound_discharges_behind_a_guard() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Nat, Int, Str, Option};
        let to_nat_or(f: Flt, fallback: Nat) -> Nat =
            match f >= +0.0 && f <= 3.4028235e38
            | true => Flt/to_nat(f)
            | false => fallback
            end;
        let to_int_or(f: Flt, fallback: Int) -> Int =
            match -3.4028235e38 <= f && f <= 3.4028235e38
            | true => Flt/to_int(f)
            | false => fallback
            end;
        /std/print(Str/concat(
            Str/concat(Nat/to_str(to_nat_or(2.5, 9)), " "),
            Int/to_str(to_int_or(-2.5, +9))))
        "#),
        b"2 -2"
    );
}

/// The bounds refuse what they exclude, at runtime through the deciding pair: a NaN and either infinity are not numbers, and a negative is not a non-negative one. `-0.0` *is* non-negative, because IEEE says `-0.0 >= +0.0`, and that is the case a reader is most likely to think the bound rejects.
#[test]
fn a_flt_narrowing_bound_refuses_what_is_not_a_number() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Nat, Str, Option, List};
        let probe(f: Flt) -> Str =
            match Flt/try_to_nat(f)
            | some(n) => Nat/to_str(n)
            | none() => "-"
            end;
        /std/print(List/fold(
            [probe(2.5), probe(-0.0), probe(-2.5), probe(Flt/pos_inf), probe(Flt/neg_inf),
                probe(Flt/nan)],
            "",
            (s, acc) => Str/concat(acc, Str/concat(s, " "))))
        "#),
        b"2 0 - - - - "
    );
}
