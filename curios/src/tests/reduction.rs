//! What a sequence costs to build at the *type* level, measured against what the same loop costs at runtime.
//!
//! `Bytes/slice` states `10 <= Bytes/len(b)`, a decided proposition, so its subject stands in a type and the obligation is discharged by reducing that subject. Writing `Bytes/slice(built, 0, 10)` over a computed accumulator therefore runs the whole accumulation at elaboration time. `normalize_concat` fuses an all-literal concatenation into one packed value, so each step copies everything accumulated so far — the representation the runtime replaced with a rope, still in use here. The same loop is linear when the program runs it (`tests::runtime`'s `accumulation_loops_are_linear_by_construction`) and quadratic when a bound makes the compiler evaluate it.
//!
//! Three arms divide that cost, and the division is the point: the middle arm performs the same number of transitions as the last one and constructs nothing, so whatever separates them is construction rather than machinery.
//!
//! Both carriers are measured, and `Bytes` covers the byte grain only. `Bits` shares `normalize_concat` and `PackedBin::concat` with it at a different generator width, so it would report the same shape eight times smaller per step; the two carriers here are the ones whose *representations* differ.

use {
    super::typecheck_within,
    curios_pipeline::DEFAULT_STEP_BUDGET,
    std::time::{Duration, Instant},
};

/// The bound read off an opaque parameter: reduction stops at `b`, the guard refines it once and generically, and nothing is evaluated. The control the other two arms are read against, and the workaround `tests::runtime`'s accumulation measurement relies on.
fn bytes_opaque(n: usize) -> String {
    format!(
        r#"
        use /std/{{Handle, Bytes, Nat, Str}};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")]) end;
        let head_of(b : Bytes) -> Bytes =
            match 10 <= Bytes/len(b) | true => Bytes/slice(b, 0, 10) | false => x[] end;
        let built = go({n}, x[]);
        let head = head_of(built);
        /std/print("ok")
        "#
    )
}

/// Transitions without payload growth: the accumulator is *replaced* each step rather than extended, so the loop runs its full length against a bound that evaluates it, and the value it builds never exceeds ten bytes. What this arm still costs per transition is the floor any budget default has to respect.
fn bytes_fixed(n: usize) -> String {
    format!(
        r#"
        use /std/{{Handle, Bytes, Nat, Str}};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1; ih => go(k, Str/to_bytes("0123456789")) end;
        let built = go({n}, x[]);
        let head = Bytes/slice(built, 0, 10);
        /std/print("ok")
        "#
    )
}

/// Both: the same transitions as the arm above, over an accumulator that grows by ten bytes a step. Its excess over that arm is constructed payload and nothing else.
fn bytes_growing(n: usize) -> String {
    format!(
        r#"
        use /std/{{Handle, Bytes, Nat, Str}};
        rec go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1; ih => go(k, x[..acc, ..Str/to_bytes("0123456789")]) end;
        let built = go({n}, x[]);
        let head = Bytes/slice(built, 0, 10);
        /std/print("ok")
        "#
    )
}

/// [`bytes_opaque`] over the `List` carrier, whose fusion flattens element vectors where `Bin`'s copies packed bytes.
fn list_opaque(n: usize) -> String {
    format!(
        r#"
        use /std/{{Handle, List, Nat, Str}};
        rec go(i : Nat, acc : List(Nat)) -> List(Nat) =
            match i | 0 => acc | k + 1; ih => go(k, [..acc, ..[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]) end;
        let head_of(a : List(Nat)) -> List(Nat) =
            match 10 <= List/len(a) | true => List/slice(a, 0, 10) | false => [] end;
        let built = go({n}, []);
        let head = head_of(built);
        /std/print("ok")
        "#
    )
}

/// [`bytes_fixed`] over the `List` carrier.
fn list_fixed(n: usize) -> String {
    format!(
        r#"
        use /std/{{Handle, List, Nat, Str}};
        rec go(i : Nat, acc : List(Nat)) -> List(Nat) =
            match i | 0 => acc | k + 1; ih => go(k, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) end;
        let built = go({n}, []);
        let head = List/slice(built, 0, 10);
        /std/print("ok")
        "#
    )
}

/// [`bytes_growing`] over the `List` carrier.
fn list_growing(n: usize) -> String {
    format!(
        r#"
        use /std/{{Handle, List, Nat, Str}};
        rec go(i : Nat, acc : List(Nat)) -> List(Nat) =
            match i | 0 => acc | k + 1; ih => go(k, [..acc, ..[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]) end;
        let built = go({n}, []);
        let head = List/slice(built, 0, 10);
        /std/print("ok")
        "#
    )
}

/// The smallest power-of-two step budget `source` elaborates within, as `Ok`; `Err` carries the largest budget tried when none of them sufficed.
///
/// The budget is per declaration and restored at every item boundary, so this reports the *heaviest declaration's* spend rather than a total — which is the quantity a budget default has to clear. A power of two rather than a bisection because the question is whether the count grows linearly in the iteration count, and a factor of two answers that; the failing probes abort as soon as the budget is spent, so only the succeeding one costs full price.
///
/// The `Err` payload is the largest budget *tried*, not [`DEFAULT_STEP_BUDGET`]: the last power of two below the default is 524 288, so a program needing 600 000 steps elaborates fine at the default while every probe here fails. Reporting the default in that case would claim the program does not elaborate, which is the opposite of true.
fn budget_floor(source: &str) -> Result<u64, u64> {
    let mut largest = 0;

    for budget in std::iter::successors(Some(1024u64), |budget| budget.checked_mul(2))
        .take_while(|budget| *budget <= DEFAULT_STEP_BUDGET)
    {
        largest = budget;
        if typecheck_within(budget, source).is_ok() {
            return Ok(budget);
        }
    }

    Err(largest)
}

/// Render a [`budget_floor`] outcome for the table.
fn floor_cell(floor: Result<u64, u64>) -> String {
    match floor {
        Ok(steps) => format!("{steps}"),
        Err(largest) => format!("> {largest}"),
    }
}

/// Elaborate `source` at the default budget, returning how long it took.
fn elaboration_time(source: &str) -> Duration {
    let start = Instant::now();
    let outcome = typecheck_within(DEFAULT_STEP_BUDGET, source);
    let elapsed = start.elapsed();

    outcome.expect("the arm elaborates within the default budget");
    elapsed
}

/// What a type-level accumulation costs, divided three ways per carrier.
///
/// ```sh
/// cargo test --release --package curios -- --ignored --nocapture type_level_sequence_cost_measurements
/// ```
///
/// It asserts nothing beyond each arm elaborating at all — a measurement that fails is a measurement with an opinion. What it does *not* cover is peak process memory, which no figure taken inside this process would be honest about: the allocator has already returned intermediates to the pool by the time a test could read a high-water mark. That half is taken from outside, with its command recorded below.
///
/// # What it last printed
///
/// Taken **2026-08-14**, **release**, on `aarch64-apple-darwin`, before any part of the fusion cap existed. Kept here rather than in a document that cites this test, so a number cannot drift from the thing that would check it.
///
/// ```text
/// Bytes
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        800    127.6ms    131.1ms    137.5ms          3.5ms          6.4ms         65536        131072
///       1600    100.3ms    123.8ms    136.5ms         23.5ms         12.6ms        131072        262144
///       3200    103.7ms    145.4ms    183.4ms         41.7ms         38.0ms        262144        524288
///       6400     96.6ms    190.2ms    319.1ms         93.6ms        128.9ms        524288      > 524288
///
/// List
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        250    107.0ms    102.1ms    147.0ms          0.0ns         44.9ms         16384         65536
///        500    115.1ms    105.8ms    244.2ms          0.0ns        138.4ms         32768        131072
///       1000    111.5ms    111.3ms    598.8ms          0.0ns        487.6ms         65536        262144
///       2000    115.4ms    123.8ms       2.0s          8.4ms           1.8s        131072        524288
/// ```
///
/// **Every budget floor doubles when the iteration count doubles.** That holds in all four columns, on both carriers, for the arm that constructs nothing and the arm that constructs quadratically alike. The step counter prices this loop identically whichever it is running, which is the whole of what the counter cannot see.
///
/// **The fixed-payload arm is linear and the growing arm is not.** `fixed-opaque` roughly doubles per rung (3.5 → 23.5 → 41.7 → 93.6 ms); `growing-fixed` grows by 1.97×, 3.02× and 3.39× on `Bytes` and by 3.08×, 3.52× and 3.69× on `List`, converging on the 4× a quadratic gives for a doubled input. Nothing about performing a transition is superlinear; what grows is what a transition builds.
///
/// **`Bytes` needs eight times the iteration count to show what `List` shows**, which is why the two ladders differ. A packed byte copy is a `memcpy`; an element copy is a reference-count increment per element. Same shape, two orders of magnitude apart in the constant.
///
/// **`> 524288` is a gap in this probe, not a refusal.** The last power of two below the default budget is 524 288, so a program needing between that and 1 000 000 steps elaborates fine while every probe here fails. `Bytes`/growing at n = 6400 is that case — the timing column beside it is a successful elaboration.
///
/// # Peak memory
///
/// Taken the same day, same profile, from outside the process because a high-water mark read from inside it would already have been returned to the allocator:
///
/// ```sh
/// /usr/bin/time -l target/release/curios compile bytes_growing_6400.crs -o /dev/null
/// ```
///
/// | Arm | n = 800 | n = 6400 |
/// | --- | --- | --- |
/// | opaque | 75.3 MiB | 75.5 MiB |
/// | fixed | 75.7 MiB | 87.8 MiB |
/// | growing | 87.8 MiB | 396.3 MiB |
///
/// **The opaque arm is flat in the iteration count** — 75.3 against 75.5 MiB across an eightfold range — so it is the compiler's own baseline and the other two rows are read as excesses over it.
///
/// **The fixed-payload arm retains about 2 KiB per transition while constructing nothing.** 12.5 MiB over 6400 iterations, growing linearly. That is the floor a budget default has to respect, and it is the sharper half of this measurement: at that rate a budget of a million transitions admits roughly two gigabytes before a single byte of payload is built.
///
/// **The growing arm retains 321 MiB above baseline to produce a 64 KiB value.** Its excess (12.5 → 36.8 → 127.0 → 320.8 MiB) grows superlinearly, matching the timings.
///
/// **This corrects a figure stated in prose, which is why it is here.** The specification says compile-time evaluation of a small fraction of the runtime measurement's size "already costs gigabytes". Inside the default budget it does not: the largest iteration count the default admits for this arm is around 6400, and that costs 321 MiB. Gigabytes are reached by this shape, but only past a budget the compiler does not ship — which is where the n = 100 000 fixture in `tests::numeric` sits, and it refuses rather than arriving there.
///
/// # The fixed prelude, which the cap must not move
///
/// The other half of the gate: capping fusion must not cost the prelude anything. Taken the same day, from the build script's own capture rather than from `cargo`'s wall clock, because `cargo build` is mostly `rustc` and its RSS says nothing about elaboration:
///
/// ```sh
/// touch curios-prelude-archive/std.crs
/// cargo build --package curios-prelude-archive --features profile
/// # target/debug/build/curios-prelude-archive-*/out/profile.tsv
/// ```
///
/// | Span | Time | Retained | Allocated | Allocations |
/// | --- | --- | --- | --- | --- |
/// | `elaborate_and_zonk_module` | 18.6 s | 248.7 MiB | 10 208.7 MiB | 67 368 289 |
/// | `erase_unit` | 1.75 s | 47.9 MiB | 664.8 MiB | 6 318 194 |
///
/// Reported peak, printed by the build as a warning: **606.6 MiB**, identical across two runs.
///
/// **Compare the allocation columns, not the time one.** Time here is a debug build under a capture and moved by three seconds between runs; the allocation volume, the allocation count and the reported peak came back bit-identical, because they are counted rather than sampled. A regression that matters to this work is one that allocates more.
#[test]
#[ignore = "measurement: reports what a type-level accumulation costs rather than asserting"]
fn type_level_sequence_cost_measurements() {
    type Build = fn(usize) -> String;
    // Each carrier carries its own ladder, because one ladder cannot show both shapes: a packed byte copy is a `memcpy` and an element copy is a reference-count increment per element, so `List` reaches the same construction volume two orders of magnitude sooner. Both ladders stop where the default budget does — the floors below are linear in the iteration count, so the last rung of each is about the largest that still elaborates.
    let carriers: [(&str, [usize; 4], [Build; 3]); 2] = [
        (
            "Bytes",
            [800, 1600, 3200, 6400],
            [bytes_opaque, bytes_fixed, bytes_growing],
        ),
        (
            "List",
            [250, 500, 1000, 2000],
            [list_opaque, list_fixed, list_growing],
        ),
    ];

    for (carrier, ladder, [opaque, fixed, growing]) in carriers {
        println!("\n{carrier}");
        println!(
            "    {:>6}  {:>9}  {:>9}  {:>9}  {:>13}  {:>13}  {:>12}  {:>12}",
            "n",
            "opaque",
            "fixed",
            "growing",
            "fixed-opaque",
            "growing-fixed",
            "floor fixed",
            "floor growing",
        );

        for n in ladder {
            // The three arms elaborate the same loop the same number of times, so the excesses below subtract everything they share: the ~0.2 s of prelude restore, backend and Wasm emission that the opaque arm is made of, and then the transition machinery the fixed arm adds. What survives both subtractions is construction.
            let (fixed_source, growing_source) = (fixed(n), growing(n));
            let opaque_time = elaboration_time(&opaque(n));
            let fixed_time = elaboration_time(&fixed_source);
            let growing_time = elaboration_time(&growing_source);

            println!(
                "    {n:>6}  {:>9.1?}  {:>9.1?}  {:>9.1?}  {:>13.1?}  {:>13.1?}  {:>12}  {:>12}",
                opaque_time,
                fixed_time,
                growing_time,
                fixed_time.saturating_sub(opaque_time),
                growing_time.saturating_sub(fixed_time),
                floor_cell(budget_floor(&fixed_source)),
                floor_cell(budget_floor(&growing_source)),
            );
        }
    }
}
