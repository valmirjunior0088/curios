//! Discharging a bound: by a guard, by evaluation, or not at all.

//! The numeric envelope gates: every constant folder computes in exact `u32`/`i32` (the numeric law), and the i31 backend boundary appears only as a trap in emitted Wasm — an overflowing computation traps, and a folded literal the carrier cannot box traps at its materialization point. The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics.

use {
    crate::tests::{run, typecheck, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
};

// The control half of a minimal pair over the unfolding rule. `f`'s base arm returns a literal, so `f(0, n)` reduces to an `Intrinsic`-headed term, `force_rec` keeps that reduct, and the decided `Nat/Le` discharges by reduction. Identical in every other respect to the refused half below, which differs only in what the base arm returns.
#[test]
fn a_bound_over_a_recursion_returning_a_literal_discharges() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Nat};
        let f(k : Nat, n : Nat) -> Nat =
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
        let f(k : Nat, n : Nat) -> Nat =
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

// The guard records `n + n <= Bytes/len(b)` as written; the slice asks for `2 * n <= Bytes/len(b)`. The fold merges both to `2 · n` the moment they are built, so the refinement key matches; this pins that a sum keeps merging eagerly while a product of two sums does not.
#[test]
fn a_guard_over_like_terms_discharges_a_bound_spelled_the_other_way() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Bytes, Nat};
        let doubled(b : Bytes, n : Nat) -> Bytes =
            match n + n <= Bytes/len(b) | true => Bytes/slice(b, 0, 2 * n) | false => x[] end;
        /std/print("ok")
        "#),
        b"ok"
    );
}

// A shape from `/std/Str/utf8`: `len(h :: t) - 1` is a difference over a folded recursive application, and `len(t)` is that recursion one step further. Both checkers judge the `refl`.
#[test]
fn a_difference_over_a_folded_recursion_converts_with_its_unfolding() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Bytes, Byte, Nat, Eq};
        let len(b : Bytes) -> Nat = match b | x[] => 0 | x[_, ..t] => 1 + len(t) end;
        let step(h : Byte, t : Bytes) -> Eq(Nat/sub(len(x[h, ..t]), 1), len(t)) = Eq/refl();
        /std/print("ok")
        "#),
        b"ok"
    );
}

// A bound whose subject genuinely diverges used to spend the whole budget and report exhaustion, where the same subject in a declared type was refused by name before anything ran. The check still runs — a subject that terminates discharges, whatever the analysis classified it — and only an exhausted one is re-read for the partial definition it names. `spin` recurses on `p + 1`, which no size-change order accepts.
//
// The budget is stated rather than defaulted because `spin` exhausts whatever it is given: the default's thirty million steps bought nothing but the wait, and made this the slowest test in the suite. A hundred thousand is far more than the rest of the program elaborates in and still exhausts in well under a second.
#[test]
fn a_bound_over_a_diverging_subject_is_refused_by_name() {
    let error = typecheck_within(
        100_000,
        r#"
        use /std/{Nat, Int};
        let spin(n : Nat) -> Int = match n | 0 => +0 | p + 1; _ => spin(p + 1) end;
        let k : Nat = Int/to_nat(spin(3));
        /std/print("unreachable")
        "#,
    )
    .expect_err("a diverging subject cannot discharge a bound");

    assert!(
        error.contains("is a proof position but reaches '/spin'"),
        "expected the totality refusal by name, got: {error}"
    );
    assert!(
        !error.contains("ran out of steps"),
        "exhaustion should have been re-reported by name, got: {error}"
    );
}

// `Int/NonNeg` says `Int/ge(a, 0)`; the guard says `0 <= a`. Before the mirror they were two neutrals neither conversion nor refinement related, and the guard did not discharge the bound; the `/sys` rows now build both as `Int/le(0, a)`, and the reducer mirrors one built by hand.
#[test]
fn a_guard_spelled_the_other_way_discharges_a_bound() {
    assert_eq!(
        run(r#"
        use /std/{Handle, Str, Nat, Int};
        let narrow(a : Int) -> Nat = match 0 <= a | true => Int/to_nat(a) | false => 0 end;
        /std/print(Nat/to_str(narrow(+7)))
        "#),
        b"7"
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
        let go(i : Nat, acc : Bytes) -> Bytes =
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
        let go(i : Nat, acc : Bytes) -> Bytes =
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
        let go(i : Nat, acc : Bytes) -> Bytes =
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
///
/// Both deciders appear, because they state different domains: `non_neg` excludes a negative where `finite` admits it, and each excludes both infinities and the NaN. Reading them side by side is what shows `-2.5` is the one column they disagree on.
#[test]
fn a_flt_narrowing_bound_refuses_what_is_not_a_number() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Nat, Int, Str, Option, List};
        let to_nat(f: Flt) -> Str =
            match Flt/try_to_nat(f) | some(n) => Nat/to_str(n) | none() => "-" end;
        let to_int(f: Flt) -> Str =
            match Flt/try_to_int(f) | some(n) => Int/to_str(n) | none() => "-" end;
        let probe(f: Flt) -> Str = Str/concat(Str/concat(to_nat(f), "/"), to_int(f));
        /std/print(List/fold(
            [probe(2.5), probe(-0.0), probe(-2.5), probe(Flt/pos_inf), probe(Flt/neg_inf),
                probe(Flt/nan)],
            "",
            (s, acc) => Str/concat(acc, Str/concat(s, " "))))
        "#),
        b"2/+2 0/+0 -/-2 -/- -/- -/- "
    );
}

/// **The call the fold buys.** With `Flt` operations folding through the model, a closed narrowing discharges its own bound: `NonNeg(2.5)` reduces to `True` because the comparisons in it reduce, so nothing is written at the call site. That is exactly the one shape a guard could not stand in for, and it is why the two narrowings could not state their domains while the family was opaque.
///
/// The `refl` laws beside it are the same fold read as an equation, and each holds *here* rather than being a property of whatever machine compiled the program.
///
/// `0.1 + 0.2 == 0.3` is the row worth reading twice. It is **true** in binary32 and false in binary64, so the famous example is the other format's — and this fixture is where the difference is pinned rather than assumed. Its first spelling here asserted the binary64 folklore and the fold refused it, which is the mechanism working: a claim about floats is now something the compiler checks instead of something a comment asserts.
#[test]
fn a_closed_flt_bound_discharges_and_the_model_decides_the_laws() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Nat, Int, Str, Eq, Bool};
        let two: Nat = Flt/to_nat(2.5);
        let minus_two: Int = Flt/to_int(-2.5);
        let sum: Eq(Flt/add(1.0, 1.0), 2.0) = Eq/refl();
        let binary32_is_not_binary64: Eq(Flt/eql(Flt/add(0.1, 0.2), 0.3), true) = Eq/refl();
        let tie: Eq(Flt/nearest(2.5), 2.0) = Eq/refl();
        let subnormal_tie: Eq(Flt/div(1.0e-45, 2.0), +0.0) = Eq/refl();
        let signed_zero: Eq(Flt/add(-0.0, +0.0), +0.0) = Eq/refl();
        let round_trip: Eq(Flt/of_le_bytes(Flt/to_le_bytes(2.5)), 2.5) = Eq/refl();
        let widen: Eq(Flt/to_nat(Nat/to_flt(16777215)), 16777215) = Eq/refl();
        /std/print(Str/concat(Nat/to_str(two), Int/to_str(minus_two)))
        "#),
        b"2-2"
    );
}

// `Nat/Lt/strong` is course-of-values induction at a `Type`-valued motive, and cumulativity lets a `Prop`-valued claim ride it: `below` proves `Lt(n, n + 3)` through it and the proof discharges `Str/get`'s bound, an erased position. The second use computes: a step that reads the hypothesis two steps down is a recursion the successor's principle cannot express, and `strong` carries it as ordinary induction on the bound.
//
// The proof is consumed where erasure deletes it, deliberately: binding it by a value-level `let` runs `strong` with a step erased to `unit`, which is the erasure asymmetry `tests::erasure` pins as ignored.
#[test]
fn strong_induction_serves_a_proposition_and_a_computation() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, True, Char};
        let below(n: Nat) -> Nat/Lt(n, n + 3) =
            Nat/Lt/strong((k) => Nat/Lt(k, k + 3), (k, ih) => True/qed(), n);
        let fib(n: Nat) -> Nat =
            Nat/Lt/strong(
                (k) => Nat,
                (k, ih) =>
                    match k
                    | 0 => 0
                    | kp + 1 =>
                        match kp
                        | 0 => 1
                        | kpp + 1 => ih(kp, True/qed()) + ih(kpp, True/qed())
                        end
                    end,
                n);
        /std/print(Str/flatten([Str/of_char(Str/get("hello", 2, below(2))), Nat/to_str(fib(1)), ",", Nat/to_str(fib(10))]))
        "#),
        b"l1,55"
    );
}
