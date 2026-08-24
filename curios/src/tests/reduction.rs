//! What a sequence costs to build at the *type* level, measured against what the same loop costs at runtime.
//!
//! `Bytes/slice` states `10 <= Bytes/len(b)`, a decided proposition, so its subject stands in a type and the obligation is discharged by reducing that subject. Writing `Bytes/slice(built, 0, 10)` over a computed accumulator therefore runs the whole accumulation at elaboration time — and it used to run it quadratically, because `normalize_concat` fused an all-literal concatenation into one packed value and so recopied everything accumulated so far on every step. `curios-core`'s `FUSION_CAP` stopped that, and its measure is what keeps a length over the resulting spine a single fold; the figures below are what those two decided, taken on both sides of them.
//!
//! Three arms divide that cost, and the division is the point: the middle arm performs the same number of transitions as the last one and constructs nothing, so whatever separates them is construction rather than machinery.
//!
//! Both carriers are measured, and `Bytes` covers the byte grain only. `Bits` shares `normalize_concat` and `PackedBin::concat` with it at a different generator width, so it would report the same shape eight times smaller per step; the two carriers here are the ones whose *representations* differ.
//!
//! A second probe reads the same programs the other way round. [`type_level_sequence_cost_measurements`] divides one checker's cost into machinery and construction; [`kernel_memo_charge_measurements`] holds the program fixed and divides the *checkers*, because the compile path puts one budget to both and a user meets whichever demands more.

use {
    super::{
        typecheck_within,
        unfolding::{Consumed, predicates},
    },
    curios_core::{Consumption, Cost},
    curios_pipeline::{
        DEFAULT_STEP_BUDGET, recheck_with_prelude, recheck_with_prelude_measured,
        typecheck_with_prelude, typecheck_with_prelude_measured,
    },
    curios_text::{Entrypoint, RootSource},
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

/// The smallest power-of-two budget `accepts` answers `true` at, as `Ok`; `Err` carries the largest budget tried when none of them sufficed.
///
/// The budget is per declaration and restored at every item boundary, so this reports the *heaviest declaration's* spend rather than a total — which is the quantity a budget default has to clear. A power of two rather than a bisection because the question is whether the count grows linearly in the iteration count, and a factor of two answers that; the failing probes abort as soon as the budget is spent, so only the succeeding one costs full price.
///
/// The `Err` payload is the largest budget *tried*, not [`DEFAULT_STEP_BUDGET`]: the sweep stops at the last power of two below the default, so a program needing more than that but less than the default elaborates fine while every probe here fails. Reporting the default in that case would claim the program does not elaborate, which is the opposite of true.
fn floor(mut accepts: impl FnMut(u64) -> bool) -> Result<u64, u64> {
    let mut largest = 0;

    for budget in std::iter::successors(Some(1024u64), |budget| budget.checked_mul(2))
        .take_while(|budget| *budget <= DEFAULT_STEP_BUDGET)
    {
        largest = budget;
        if accepts(budget) {
            return Ok(budget);
        }
    }

    Err(largest)
}

/// [`floor`] for the whole compile path, which puts the same budget to the elaborator and then to the kernel — so this reports whichever of the two demands more.
fn budget_floor(source: &str) -> Result<u64, u64> {
    floor(|budget| typecheck_within(budget, source).is_ok())
}

/// [`floor`] for elaboration alone, with the kernel not asked.
fn elaborator_floor(entrypoint: &Entrypoint) -> Result<u64, u64> {
    floor(|budget| typecheck_with_prelude(budget, entrypoint, &RootSource::none()).is_ok())
}

/// [`floor`] for the kernel alone, over a module elaboration already produced.
///
/// Elaborating once at the default budget and re-certifying the result is what separates the two counters: the module does not change with the budget the kernel is then given, so the sweep measures the kernel's own spend rather than a compile that fails earlier.
fn kernel_floor(entrypoint: &Entrypoint) -> Result<u64, u64> {
    let (module, _obligations) =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, entrypoint, &RootSource::none())
            .expect("the arm elaborates within the default budget");

    floor(|budget| recheck_with_prelude(&module, budget).is_empty())
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

/// **The regression guard the measurement above cannot be.** A probe is ignored, so nothing runs it; and a cache hit charges nothing in either checker, so both absorb this entire class of defect and stay silent until a budget runs out. That is how a quadratic length went unnoticed, so the guard has to be an ordinary assertion at the ordinary budget.
///
/// It went unnoticed for longer than it had to because the two checkers disagreed about the *price* as well: the kernel charged a memo hit what the computation it replaced had cost, so it refused at 8–16× the elaborator's budget for the same program, and a construction defect reached a user as a kernel refusal rather than as either checker's honest cost. That asymmetry is gone — see [`kernel_memo_charge_measurements`] — and this guard is what remains needed once the two agree.
///
/// Both carriers, at an iteration count that costs a small multiple of the default budget when a length is quadratic in the spine's depth and a small fraction of it when a length is a fold.
#[test]
fn an_accumulated_sequence_is_bounded_when_a_window_is_taken_of_it() {
    for source in [bytes_growing(2000), list_growing(2000)] {
        typecheck_within(DEFAULT_STEP_BUDGET, &source)
            .expect("an accumulation and a window over it fit the ordinary budget");
    }
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
/// Taken **2026-08-16**, **release**, on `x86_64-unknown-linux-gnu`, with the closed machine evaluating the accumulation.
///
/// ```text
/// Bytes
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        800    227.2ms    231.8ms    240.5ms          4.6ms          8.6ms         65536         65536
///       1600    215.7ms    247.3ms    262.6ms         31.6ms         15.3ms        131072        262144
///       3200    209.0ms    296.9ms    325.7ms         87.8ms         28.8ms        262144        262144
///       6400    206.7ms    391.1ms    452.2ms        184.4ms         61.1ms        524288        524288
///
/// List
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        250    229.4ms    217.1ms    239.7ms          0.0ns         22.6ms         65536         65536
///        500    227.5ms    233.2ms    253.1ms          5.6ms         19.9ms         32768         65536
///       1000    227.1ms    246.0ms    279.5ms         18.9ms         33.5ms        131072        131072
///       2000    233.5ms    287.1ms    333.7ms         53.6ms         46.6ms        131072        262144
/// ```
///
/// **This table corrects an attribution the one below made.** The growing arm's floor now sits at the fixed arm's, within one power of two at every rung — where it was sixteen times it and doubling with the input — and the fixed arm's own floors did not move. What left was the frame row: the growing arm's accumulator, substituted unreduced, was forced into a chain whose walk priced one native frame per iteration, and the closed machine's eager substitution removes it. So the sixteenfold gap the table below calls "constructed payload" was overwhelmingly the *unforced accumulator's depth*: ten bytes of payload price six units a step, a native frame priced a thousand, and only the wall-time excess in the `growing-fixed` column — tens of milliseconds, growing linearly — was construction all along.
///
/// # What it printed with construction priced, before the closed machine
///
/// Taken **2026-08-15**, **release**, on `aarch64-apple-darwin`. The floor columns are units of reduction *work*, not transitions, and are not comparable to the pre-pricing table below except in shape.
///
/// ```text
/// Bytes
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        800    120.1ms    117.8ms    123.8ms          0.0ns          6.0ms         65536       1048576
///       1600    105.8ms    128.1ms    140.0ms         22.3ms         11.9ms        262144       2097152
///       3200    106.8ms    154.7ms    173.7ms         47.9ms         19.0ms        262144       4194304
///       6400    108.2ms    203.6ms    248.3ms         95.4ms         44.7ms        524288       8388608
///
/// List
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        250    116.6ms    106.7ms    118.3ms          0.0ns         11.7ms        131072        524288
///        500    118.6ms    110.3ms    126.5ms          0.0ns         16.2ms        131072       1048576
///       1000    119.1ms    117.9ms    135.5ms          0.0ns         17.6ms        262144       2097152
///       2000    119.0ms    131.5ms    151.7ms         12.5ms         20.2ms        262144       4194304
/// ```
///
/// **This table is the work's own verdict, and the two floor columns are the whole of it.** The fixed-payload arm builds nothing and its floor barely moves across the ladder — 65 536 to 524 288 over an eightfold input, and within a factor of two of what it was before pricing. The growing arm performs *the same transitions* and its floor is now sixteen times the fixed arm's and doubles exactly with the input. That gap is constructed payload, and before this work the counter could not see one unit of it: the same two columns used to sit within 2× of each other whichever arm was running.
///
/// **The `Bytes` ladder's last rung is inside the sweep again**, where it read `> 524288` before — not because the program got cheaper but because the default it is swept against was recalibrated with the pricing.
///
/// # What it printed before construction was priced
///
/// Taken **2026-08-14**, **release**, same machine, with the fusion cap and the measure in place and the counter still charging one unit per transition.
///
/// ```text
/// Bytes
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        800    186.9ms    166.1ms    168.6ms          0.0ns          2.4ms         65536        131072
///       1600    154.0ms    176.3ms    186.5ms         22.3ms         10.1ms        131072        262144
///       3200    156.0ms    197.9ms    219.5ms         41.9ms         21.6ms        262144        524288
///       6400    156.2ms    249.4ms    288.6ms         93.2ms         39.2ms        524288      > 524288
///
/// List
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        250    170.7ms    155.2ms    171.9ms          0.0ns         16.7ms         16384         65536
///        500    168.5ms    159.7ms    172.4ms          0.0ns         12.7ms         32768        131072
///       1000    167.7ms    165.7ms    182.4ms          0.0ns         16.7ms         65536        262144
///       2000    169.6ms    177.4ms    198.1ms          7.8ms         20.8ms        131072        524288
/// ```
///
/// # What it printed before any of this
///
/// The same command on the same machine, before the cap and the measure existed. This is the baseline the work is read against, and the arms that did *not* move are as much of the evidence as the ones that did.
///
/// ```text
/// Bytes    n=800: fixed-opaque 3.5ms  growing-fixed   6.4ms   floors  65536 / 131072
///          n=1600:              23.5ms                12.6ms         131072 / 262144
///          n=3200:              41.7ms                38.0ms         262144 / 524288
///          n=6400:              93.6ms               128.9ms         524288 / > 524288
/// List     n=250:                0.0ns                44.9ms          16384 /  65536
///          n=500:                0.0ns               138.4ms          32768 / 131072
///          n=1000:               0.0ns               487.6ms          65536 / 262144
///          n=2000:               8.4ms                  1.8s         131072 / 524288
/// ```
///
/// **Every budget floor doubles when the iteration count doubles — in every one of these tables.** That held even when the growing arm was quadratic in *time*, on both carriers, for the arm that constructs nothing and the arm that constructed quadratically. The step counter priced this loop identically whichever it was running, which is the whole of what it could not see, and the reason the memory column below was the one that moved. Pricing construction is what finally separated the two arms in the column that decides acceptance.
///
/// **The fixed-payload arm did not move**, which is what makes the rest a removal rather than a reallocation: 3.5 / 23.5 / 41.7 / 93.6 ms became 0.0 / 22.3 / 41.9 / 93.2 ms.
///
/// **The growing arm's excess over it went from quadratic to linear.** On `Bytes` its per-rung growth was 1.97×, 3.02×, 3.39× — converging on the 4× a quadratic gives for a doubled input — and is now 4.2×, 2.1×, 1.8×. On `List` it was 3.08×, 3.52×, 3.69× and the arm cost 1.8 s at n = 2000; it now costs 20.8 ms, and does not grow.
///
/// **`Bytes` needed eight times the iteration count to show what `List` showed**, which is why the two ladders differ. A packed byte copy is a `memcpy`; an element copy is a reference-count increment per element. Same shape, two orders of magnitude apart in the constant.
///
/// # Peak memory
///
/// Taken the same day, same profile, from outside the process because a high-water mark read from inside it would already have been returned to the allocator:
///
/// ```sh
/// /usr/bin/time -l target/release/curios compile bytes_growing_6400.crs -o out
/// ```
///
/// | Arm | n = 800 | n = 6400 | before, n = 800 | before, n = 6400 |
/// | --- | --- | --- | --- | --- |
/// | opaque | — | 74.8 MiB | 75.3 MiB | 75.5 MiB |
/// | fixed | — | 87.9 MiB | 75.7 MiB | 87.8 MiB |
/// | growing | 80.4 MiB | 126.6 MiB | 87.8 MiB | 396.3 MiB |
///
/// **The growing arm's excess over baseline fell from 321 MiB to 52 MiB** — a sixfold reduction to produce the same 64 KiB value — and its growth across the ladder (5.6 / 9.5 / 15.7 / 51.8 MiB) is no longer quadratic. That is the whole point of the work: the accumulator stops being recopied every step.
///
/// **The opaque and fixed arms are unmoved**, at 74.8 against 75.5 and 87.9 against 87.8 MiB. The fixed arm still retains about 2 KiB per transition while constructing nothing, growing linearly, and that figure is untouched by any of this — it is a property of the machinery rather than of what a transition builds, and it is the floor a budget default has to respect: at that rate a million transitions admit roughly two gigabytes before a single byte of payload is built.
///
/// **One figure this corrects.** The specification says compile-time evaluation of a small fraction of the runtime measurement's size "already costs gigabytes". It did not even before this work: the largest iteration count the default budget admits for this arm is around 6400, and that cost 321 MiB. Gigabytes are reached by the shape, but only past a budget the compiler does not ship.
///
/// # The fixed prelude, which the cap must not move
///
/// The other half of the gate, from the build script's own capture rather than `cargo`'s wall clock, because `cargo build` is mostly `rustc` and its RSS says nothing about elaboration:
///
/// ```sh
/// touch curios-prelude-archive/std.crs
/// cargo build --package curios-prelude-archive --features profile
/// # target/debug/build/curios-prelude-archive-*/out/profile.tsv
/// ```
///
/// | Span | Retained | Allocated | Allocations | Before |
/// | --- | --- | --- | --- | --- |
/// | `elaborate_and_zonk_module` | 248.7 MiB | 10 210.3 MiB | 67 383 115 | 248.7 MiB / 10 208.7 MiB / 67 368 289 |
/// | `erase_unit` | 47.9 MiB | 665.0 MiB | 6 319 619 | 47.9 MiB / 664.8 MiB / 6 318 194 |
///
/// Reported peak, printed by the build as a warning: **606.6 MiB**, before and after, identical across runs.
///
/// **Compare the allocation columns, not the time one.** Time here is a debug build under a capture and moves by seconds between runs; allocation volume, allocation count and the reported peak come back bit-identical, because they are counted rather than sampled. The prelude moved by **+0.02%** in allocations and not at all in retained memory or reported peak — the cost of the measure's segment list running over values that are one to three generators long.
///
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

/// What the kernel charges for a memo hit, read off the budget it forces.
///
/// ```sh
/// cargo test --release --package curios -- --ignored --nocapture kernel_memo_charge_measurements
/// ```
///
/// The two floors are the same program put to the two checkers separately, at a budget swept independently for each: [`elaborator_floor`] does not ask the kernel, and [`kernel_floor`] re-certifies a module elaboration already produced. Their ratio is the quantity of interest. Both checkers reduce the same terms and neither is the reference implementation of the other, so a *small* divergence is expected and says only that two evaluators differ; a large one says the two are pricing differently, and the compile path takes the larger of the two, so it is the kernel's number a user meets.
///
/// It asserts nothing beyond each arm elaborating at all — a measurement that fails is a measurement with an opinion.
///
/// # What it last printed
///
/// Taken **2026-08-16**, **release**, on `x86_64-unknown-linux-gnu`, with the closed machine evaluating the accumulation on both sides.
///
/// ```text
/// Bytes
///          n  floor elaborator  floor kernel  divergence
///        800             65536         65536          1x
///       1600            131072        262144          2x
///       3200            262144        262144          1x
///       6400            524288        524288          1x
///
/// List
///          n  floor elaborator  floor kernel  divergence
///        250             32768         65536          2x
///        500             65536         65536          1x
///       1000            131072        131072          1x
///       2000            262144        262144          1x
/// ```
///
/// **Parity held through the machine, and every floor fell about sixteenfold.** The two checkers run one shared evaluator for the closed accumulation, so agreement here is structural now rather than measured luck; the scattered 2× rungs are adjacent powers of two, which the sweep cannot distinguish. The fall is the frame row leaving: the accumulator that was substituted unreduced and forced into a chain at the end priced one native frame per iteration, and prices a machine frame now. Where the two checkers *do* part is a `Str` literal — [`str_literal_cost_measurements`] carries that table — and it is a difference in how many times the elaborator demands one scan, not in what a demand costs.
///
/// # What it printed with construction priced, before the closed machine
///
/// Taken **2026-08-15**, **release**, on `aarch64-apple-darwin`.
///
/// ```text
/// Bytes
///          n  floor elaborator  floor kernel  divergence
///        800           1048576       1048576          1x
///       1600           2097152       2097152          1x
///       3200           4194304       4194304          1x
///       6400           8388608       8388608          1x
///
/// List
///          n  floor elaborator  floor kernel  divergence
///        250            524288        524288          1x
///        500           1048576       1048576          1x
///       1000           2097152       2097152          1x
///       2000           4194304       4194304          1x
/// ```
///
/// **Every rung is 1×.** The two checkers agreed on this program's cost exactly, on both carriers and at every size — which is more than the memo change alone bought, and says the two evaluators differ in what they do far less than they differed in what they charged.
///
/// # What it printed with memo hits free but construction unpriced
///
/// ```text
/// Bytes
///          n  floor elaborator  floor kernel  divergence
///        800             16384         16384          1x
///       1600             16384         32768          2x
///       3200             32768         65536          2x
///       6400             65536        131072          2x
///
/// List
///          n  floor elaborator  floor kernel  divergence
///        250              4096          8192          2x
///        500              8192         16384          2x
///       1000             16384         16384          1x
///       2000             32768         32768          1x
/// ```
///
/// # What it printed before either
///
/// The same command on the same machine, with a hit charged the whole recorded cost of the computation it replaced.
///
/// ```text
/// Bytes
///          n  floor elaborator  floor kernel  divergence
///        800             16384        131072          8x
///       1600             16384        262144         16x
///       3200             32768        524288         16x
///       6400             65536      > 524288           —
///
/// List
///          n  floor elaborator  floor kernel  divergence
///        250              4096         65536         16x
///        500              8192        131072         16x
///       1000             16384        262144         16x
///       2000             32768        524288         16x
/// ```
///
/// **The elaborator column did not move, and that is what identifies the change as the kernel's pricing.** Its hits were already free; every figure in it is identical on both sides. The kernel's fell by 8× or 16× at every rung that had a figure on both sides, and the `Bytes` ladder's last rung came back inside the sweep at all.
///
/// **The divergence is what a user met.** The compile path puts the same budget to both checkers, so `budget_floor` above — which reports the larger — was reporting the kernel's number throughout, and a program the elaborator accepted within its budget was refused for exhaustion by the kernel with no disagreement about any rule. That happened twice while the free monoid's measure was being developed.
///
/// **What no longer reproduces here.** The 8–16× was measured across every rung of both ladders and is now 1–2×, which is two evaluators differing rather than two price lists differing. A residual factor of two is expected and is not evidence of anything: the sweep doubles, so adjacent powers of two are one step apart.
///
/// # Whole-unit certification, which the clearing must not move
///
/// Free hits come with the `whnf`/`forced` tables cleared at every declaration boundary, and 1107 clearings over a module walk is the cost that had to be checked. It is `curios-prelude-archive`'s `stored_prelude_measurements` that takes this figure, and the retake is recorded there: **6.2 s before, 6.1 s after**, 0 refusals both times, and `kernel_memo_parity` passing unchanged on both sides.
#[test]
#[ignore = "measurement: reports what a memo hit costs the kernel rather than asserting"]
fn kernel_memo_charge_measurements() {
    type Build = fn(usize) -> String;
    let carriers: [(&str, [usize; 4], Build); 2] = [
        ("Bytes", [800, 1600, 3200, 6400], bytes_growing),
        ("List", [250, 500, 1000, 2000], list_growing),
    ];

    for (carrier, ladder, growing) in carriers {
        println!("\n{carrier}");
        println!(
            "    {:>6}  {:>16}  {:>12}  {:>10}",
            "n", "floor elaborator", "floor kernel", "divergence",
        );

        for n in ladder {
            let entrypoint = growing(n).parse::<Entrypoint>().expect("the arm parses");
            let elaborator = elaborator_floor(&entrypoint);
            let kernel = kernel_floor(&entrypoint);
            let divergence = match (elaborator, kernel) {
                (Ok(elaborator), Ok(kernel)) => format!("{}x", kernel / elaborator),
                _ => "—".to_string(),
            };

            println!(
                "    {n:>6}  {:>16}  {:>12}  {divergence:>10}",
                floor_cell(elaborator),
                floor_cell(kernel),
            );
        }
    }
}

/// A `Str` literal of `n` identical ASCII characters, bound and used `uses` times.
///
/// The literal lowers to `Str { bytes = <Bytes>, valid = of_scan_eq(b, refl_scan(b)) }`, and checking that proof makes conversion decide `scan_from(lead, b) ≡ lead` — a `rec` unfold, a `Bytes` peel, a `Byte/to_nat`, `classify`'s ladder and an inductive match, per byte. Nothing else in the program costs anything, so what a floor over this reports is the check.
fn str_literal(n: usize, uses: usize) -> String {
    let literal = "0123456789".repeat(n.div_ceil(10))[..n].to_string();
    let used = (0..uses)
        .map(|index| format!("let use{index} = Str/to_bytes(s);\n"))
        .collect::<String>();

    format!(
        r#"
        use /std/{{Str, Bytes, Handle}};
        let s : Str = "{literal}";
        {used}
        /std/print("ok")
        "#
    )
}

/// The same `n` bytes written as a raw `Bytes` literal — the derivation-free control, and the whole of what a `Str` literal would cost if its validity were not checked by running a fold.
fn bytes_literal(n: usize) -> String {
    let entries = (0..n)
        .map(|index| format!("0x{:02x}", b'0' + (index % 10) as u8))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        r#"
        use /std/{{Bytes, Nat, Handle}};
        let b : Bytes = x[{entries}];
        /std/print(Nat/to_str(Bytes/len(b)))
        "#
    )
}

/// What both checkers spend on `source`, each reporting its own heaviest declaration, beside what the kernel's walk retained.
///
/// Reported rather than bisected. A budget floor found from outside costs one whole compile per probe, reports only the larger of the two checkers, and cannot separate depth from the rest at all — which is the separation that matters, because depth is the one row whose size is set by the reduction *strategy* rather than by the term.
///
/// Retention rides along because the two are coupled from one side: a memo that cannot be stored is re-derived against the *work* budget, so a program large enough to exhaust the compilation's retention allowance stops being linear in what it spends. That coupling is invisible in the work figures alone, and reading them without it is how a cliff gets mistaken for a cost model.
fn declaration_cost(source: &str) -> (Consumption, u64, Consumption, u64) {
    let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
    let (module, _obligations, elaborator, elaborator_retained) =
        typecheck_with_prelude_measured(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the program elaborates within the default budget");
    let (verdicts, kernel) = recheck_with_prelude_measured(&module, DEFAULT_STEP_BUDGET);

    assert!(verdicts.is_empty(), "the kernel accepts it: {verdicts:?}");

    (
        elaborator,
        elaborator_retained,
        kernel.heaviest_declaration(),
        kernel.retained(),
    )
}

/// One row of the table below: what a program cost each checker, split into depth and everything else.
fn cost_row(label: &str, source: &str) {
    let (elaborator, elaborator_retained, kernel, retained) = declaration_cost(source);
    let divergence = match elaborator.units() {
        0 => 0.0,
        units => kernel.units() as f64 / units as f64,
    };

    println!(
        "  {label:<22}  {:>10}  {:>6}  {:>9}  {:>12}  {:>10}  {:>6}  {:>9}  {:>12}  {:>6.1}x",
        elaborator.units(),
        elaborator.peak_depth(),
        elaborator.other_units(),
        elaborator_retained,
        kernel.units(),
        kernel.peak_depth(),
        kernel.other_units(),
        retained,
        divergence,
    );
}

/// What a `Str` literal costs to check, and which row of the price list it spends on.
///
/// ```sh
/// cargo test --release --package curios -- --ignored --nocapture str_literal_cost_measurements
/// ```
///
/// This is the probe [`a_str_literal_costs_transitions_rather_than_frames`] guards. It asserts only that each arm checks at all — a measurement that fails is a measurement with an opinion — and the assertion that a regression has to trip lives in that ordinary test instead.
///
/// # What it last printed
///
/// Taken **2026-08-22**, **release**, on `x86_64-unknown-linux-gnu`, with the elaborator's conversion forcing a folded recursive call before comparing it, a window comparing equal to itself by identity, and the declaration-scoped memo tables no longer charged against the allowance.
///
/// ```text
///   program                      units   depth      other      retained       units   depth      other      retained  kernel/elab
///   Str literal, n=250           16655       1      15631         33461       22363       6      16219             0     1.3x
///   Str literal, n=500           27905       1      26881         34546       28776       2      26728             0     1.0x
///   Str literal, n=1000          50405       1      49381         36716       51276       2      49228             0     1.0x
///   Str literal, n=2000          95405       1      94381         41091       96276       2      94228             0     1.0x
///   Str literal, n=4000         185405       1     184381         49841      186276       2     184228             0     1.0x
///   Str literal, n=8000         365405       1     364381         67341      366276       2     364228             0     1.0x
///   Str n=500, 1 uses            27905       1      26881         35037       28776       2      26728             0     1.0x
///   Str n=500, 3 uses            27905       1      26881         35037       28776       2      26728             0     1.0x
///   Bytes literal, n=500          8541       2       6493         19540       20502       6      14358             0     2.4x
///   Str n=500, sliced            27905       1      26881         29786       28776       2      26728             0     1.0x
/// ```
///
/// The kernel's `retained` column reads its unfold table alone now, and a literal unfolds nothing monomorphic; the day before, with its term-keyed tables still charged, it read 184 567 to 202 959 across the ladder.
///
/// **The elaborator's retention is linear now** — 33K to 67K units across the ladder, about four a character, where it grew as 37·n² and saturated the quota near 5 200 characters — and **its units are the kernel's**: the `kernel/elab` column reads 1.0× at every size, where it read 0.3×. Both were one defect. The literal's proof is `of_scan_eq(b, refl_scan(b))`, and checking it asks conversion one question, `scan_from(lead, b) ≡ Scan/lead()`; the elaborator's conversion reduced the left at the *plain* demand — where a folded recursive call is its own normal form, as the machine's contract says — met the fold against a constructor, and unfolded it **one step per round**, each round storing a cache entry keyed on the next folded spelling with the scan's state unreduced in its argument, one `step` deeper per character. The kernel forces both sides of every comparison, which is one machine run. `Convert::force_folded_call` now does the same, falling back to the one-step unfold only when forcing reaches no value.
///
/// **Wall clock was superlinear where every counter was linear, and that was the representation.** Release, the bisection's rungs: 16K in 0.95 s, 32K in 2.15 s, 64K in 5.86 s, 128K in 18.3 s — where they were 5.7 s, 21 s, 82 s and about 250 s on this host. The whole of the difference was inside one `reduce_closed` run, and `PackedBin`'s equality was what it did per element: the run-scoped memo probes a key holding the current tail window and finds the key it stored, and confirming those equal walked the window bit by bit. A window of one buffer at one offset is the same bits, and `PartialEq` now says so without a read; aligned windows compare as byte slices beside it. What remains grows as about n^1.5 at the top of that ladder and is not a per-element walk — its shape is the run-scoped memo's size — and is left measured rather than chased.
///
/// # What it printed before conversion forced a folded call
///
/// Taken **2026-08-16**, **release**, on `x86_64-unknown-linux-gnu`, with the closed machine evaluating the scan. `depth` is the peak guarded reduction level; `other` is what the declaration spent on everything but the frame row. The first four columns are the elaborator's, the next four the kernel's. Units are machine-independent by construction, and a debug run of the same ladder reproduces every unit column exactly.
///
/// ```text
///   program                      units   depth      other      retained       units   depth      other      retained  kernel/elab
///   Str literal, n=250           44613       1      43589       2560644       22400       6      16256        184663     0.5x
///   Str literal, n=500           84363       1      83339       9596043       28776       2      26728        185252     0.3x
///   Str literal, n=1000         163863       1     162839      37213713       51276       2      49228        186430     0.3x
///   Str literal, n=2000         322863       1     321839     146636588       96276       2      94228        188805     0.3x
///   Str literal, n=4000         640863       1     639839     582232338      186276       2     184228        193555     0.3x
///   Str literal, n=8000        1276934       1    1275910     999999990      366276       2     364228        203055     0.3x
///   Str n=500, 1 uses            84363       1      83339       9596534       28776       2      26728        185360     0.3x
///   Str n=500, 3 uses            84363       1      83339       9596534       28776       2      26728        185360     0.3x
///   Bytes literal, n=500          9302       2       7254         22209       20539       6      14395        171164     2.2x
///   Str n=500, sliced            84363       1      83339       9587945       28776       2      26728        179159     0.3x
/// ```
///
/// # What the figures decide
///
/// **Guarded depth is flat in the literal's length, on both checkers.** The scan used to nest one native reduction level per byte, and a character cost 1 088 units with 1 024 of them the frame row; on the machine the whole ladder runs at a peak of one or two levels, and **a character costs 45 units on the kernel and 159 on the elaborator** — transitions, openings, and machine bookkeeping, no frame row at all.
///
/// **The ceiling moved from between 16 625 and 16 750 characters to between 185 000 and 200 000**, found by the same length bisection at the default budget: a 185 000-character literal compiles and a 200 000-character one is refused. That is the order of magnitude the closed machine's acceptance asked for, with the elaborator's per-character price the binding side.
///
/// **The kernel/elab column is a demand count, not a price list.** Both checkers run the same machine on the same closed scan, so one demand costs both the same; the elaborator's 3.5× is it demanding the scan at several sites and spellings — checking, conversion, and the passes after — where the kernel demands it once and replays its memo. The construction-dominated programs in [`kernel_memo_charge_measurements`] still floor at 1× between the checkers, which is where the price-list parity claim lives and holds.
///
/// **Use count is flat**, which is what spec 01's first milestone bought and this keeps honest. **Slicing is not what costs**, exactly as before: the sliced arm is the bare arm, since `Str/slice` supplies its bounds with `@drop_width_within`.
///
/// **Wall clock is superlinear where units are exactly linear.** The bisection's rungs, release: 16K in 7.1 s, 32K in 22.6 s, 64K in 82.5 s, 128K in 249.5 s, 185K in 492.5 s — growth near n^1.8 against unit columns that are linear to the third digit. The unit model prices what a reduction builds and transitions, not the O(size) hashing of large keys the elaborator's caches perform; the excess wall shares a source with the retention residue below.
///
/// # The retention residue, as it stood before the forcing
///
/// **The kernel's retention is flat across the ladder** — 184K to 203K units from n=250 to n=8000, where it was 3.2M rising quadratically to 774M and a quota cliff. The quadratic's recorded cause, the scan's unreduced accumulator chain, is gone with the machine's eager substitution, and the kernel's side went with it entirely.
///
/// **The elaborator's did not, and the chain was therefore never most of its story.** Its retention still grew as roughly 37·n² units — about three-quarters of its pre-machine figure — and saturated [`DEFAULT_RETENTION_QUOTA`](curios_core::DEFAULT_RETENTION_QUOTA) near n ≈ 5 200. What the same table shows is that saturating cost this program nothing: the n=8000 row's units were linear on trend, so the refused entries were not ones this walk re-needed. The source was the conversion stepping above, and the section at the top is where it went.
///
/// # What it printed before the closed machine
///
/// Taken **2026-08-15**, **release**, on `aarch64-apple-darwin` — the table the machine is measured against, kept whole because every claim above is a delta from it.
///
/// ```text
///   program                      units   depth      other      retained       units   depth      other      retained  kernel/elab
///   Str literal, n=250          278374     255      17254       3186474      279697     256      17553       3360434     1.0x
///   Str literal, n=500          550374     505      33254      12366375      550947     506      32803      12541116     1.0x
///   Str literal, n=1000        1094374    1005      65254      48819921     1093447    1006      63303      48996224     1.0x
///   Str literal, n=2000        2182374    2005     129254     194102046     2178447    2006     124303     194281474     1.0x
///   Str literal, n=4000        4358374    4005     257254     774166296     4348447    4006     246303     774351974     1.0x
///   Str literal, n=8000       11261744    8005    3064624    1000000000    14422506    8006    6224362     999999999     1.3x
///   Str n=500, 1 uses           550374     505      33254      12366576      550947     506      32803      12541224     1.0x
///   Str n=500, 3 uses           550374     505      33254      12366576      550947     506      32803      12541224     1.0x
///   Bytes literal, n=500          5064       2       3016         17331       17992       7      10824        188255     3.6x
///   Str n=500, sliced           550374     505      33254      12357908      550947     506      32803      12532062     1.0x
/// ```
#[test]
#[ignore = "measurement: reports what a Str literal costs rather than asserting"]
fn str_literal_cost_measurements() {
    println!(
        "\n  {:<22}  {:>10}  {:>6}  {:>9}  {:>12}  {:>10}  {:>6}  {:>9}  {:>12}  {:>7}",
        "program",
        "units",
        "depth",
        "other",
        "retained",
        "units",
        "depth",
        "other",
        "retained",
        "kernel/elab",
    );

    for n in [250, 500, 1000, 2000, 4000, 8000] {
        cost_row(&format!("Str literal, n={n}"), &str_literal(n, 0));
    }

    // Flat in use count is what spec 01's first milestone bought; a regression here is that milestone coming undone.
    for uses in [1, 3] {
        cost_row(&format!("Str n=500, {uses} uses"), &str_literal(500, uses));
    }

    // The control: the same bytes with no derivation over them.
    cost_row("Bytes literal, n=500", &bytes_literal(500));

    // Slicing supplies its bounds with `@drop_width_within` rather than leaving a decided proposition to reduce, so this should sit within a few percent of the bare literal — the check is the cost, not the slice.
    cost_row(
        "Str n=500, sliced",
        &str_literal(500, 0).replace(r#"/std/print("ok")"#, r#"/std/print(Str/slice(s, 0, 10))"#),
    );
}

/// What a web of combinator definitions costs each checker, and what consuming its value does to that.
///
/// ```sh
/// cargo test --release --package curios -- --ignored --nocapture combinator_web_cost_measurements
/// ```
///
/// The third of the parity probes, and the one aimed at a gap that was an *exponent* rather than a multiple. [`str_literal_cost_measurements`] holds a derivation fixed and divides one checker's cost; [`kernel_memo_charge_measurements`] divides the two checkers by budget floor; this one divides them on a program shape where they used to disagree without disagreeing about any rule — a scrutinee whose subject mentions a binder, which the kernel reduced once per arm to key its case refinement and the elaborator did not reduce at all.
///
/// Three rows per size, differing only in what demands the web's value: nothing, a `match` at a binder, a `match` at a literal. The middle row is the one that used to grow; the last is the control that says the trigger was the binder rather than the `match`.
///
/// It asserts nothing beyond each arm checking at all. `curios`' `scrutinee_refinement_measurements` carries the wall clocks and the refusals beside it.
///
/// # What it last printed
///
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`.
///
/// ```text
///   program                      units   depth      other      retained       units   depth      other      retained  kernel/elab
///   web n=8, applied              9302       2       7254         48649       22400       6      16256        186809     2.4x
///   web n=8, scrutinized          9302       2       7254         65685       22400       6      16256        208459     2.4x
///   web n=8, closed               9302       2       7254         65685       22400       6      16256        196313     2.4x
///   web n=13, applied             9302       2       7254         52729       22400       6      16256        189089     2.4x
///   web n=13, scrutinized         9302       2       7254         69765       22400       6      16256        210739     2.4x
///   web n=13, closed              9302       2       7254         69765       22400       6      16256        198593     2.4x
///   web n=20, applied             9302       2       7254         58441       22400       6      16256        192281     2.4x
///   web n=20, scrutinized         9302       2       7254         75477       22400       6      16256        213931     2.4x
///   web n=20, closed              9302       2       7254         75477       22400       6      16256        201785     2.4x
/// ```
///
/// **Every unit column is constant**, across five sizes and all three consumptions, and the ratio is 2.4× everywhere. Twenty definitions at fourteen — the size that refused — cost what eight do.
///
/// The heaviest declaration is the same one in every row, and that is what the flatness is *about*: it is `probe`, the declaration holding the `match`, and what it costs no longer has anything to do with the web it scrutinizes. Before the key moved to the written spelling this row grew by a factor of two per definition and refused at fourteen; the wall clocks and the refusals are in `curios`' `scrutinee_refinement_measurements`.
///
/// Retention is the one column that still separates the rows, linearly in the web: an equation is now *recorded* rather than reduced, so what a scrutinee adds is one more term held for the length of an arm.
#[test]
#[ignore = "measurement: reports what a combinator web costs each checker rather than asserting"]
fn combinator_web_cost_measurements() {
    println!(
        "\n  {:<22}  {:>10}  {:>6}  {:>9}  {:>12}  {:>10}  {:>6}  {:>9}  {:>12}  {:>7}",
        "program",
        "units",
        "depth",
        "other",
        "retained",
        "units",
        "depth",
        "other",
        "retained",
        "kernel/elab",
    );

    for rules in [8usize, 12, 13, 14, 20] {
        for (consumed, label) in [
            (Consumed::Applied, "applied"),
            (Consumed::Scrutinized, "scrutinized"),
            (Consumed::ScrutinizedClosed, "closed"),
        ] {
            cost_row(
                &format!("web n={rules}, {label}"),
                &predicates(rules, consumed, true),
            );
        }
    }
}

/// **The guard [`str_literal_cost_measurements`] cannot be**, because a probe is ignored and nothing runs it.
///
/// What it holds is the shape of a literal's cost rather than a number, and the closed machine is what set the shape: guarded reduction depth *flat* in the literal's length on both checkers, a per-character price in transitions and machine frames far below [`Cost::FRAME`], and neither checker paying a multiple of the other for the same reduction. The first two are the machine's whole yield — a literal used to nest one native reduction level per byte and cost 1 088 units a character, 1 024 of them the frame row, which capped a literal near 16 700 characters; the third failed within living memory on its own, when the kernel's memo stopped short of its internal levels and charged 5.3× the elaborator at the same depth.
///
/// The bound is stated against [`Cost::FRAME`] rather than as a literal because the quantity asserted is *that no per-character native frame is being paid at all*: a per-character price within even a quarter of the frame row means closed evaluation has fallen off the machine and back onto the recursive strategy, which is the silent cliff this guard exists to catch.
#[test]
fn a_str_literal_costs_transitions_rather_than_frames() {
    let (elaborator_small, _, kernel_small, _) = declaration_cost(&str_literal(500, 0));
    let (elaborator_large, _, kernel_large, _) = declaration_cost(&str_literal(1000, 0));

    // The literal's scan runs on the machine's explicit stack, so doubling the literal moves guarded depth not at all — where it used to move it by exactly the added byte count.
    assert_eq!(kernel_large.peak_depth(), kernel_small.peak_depth());
    assert_eq!(elaborator_large.peak_depth(), elaborator_small.peak_depth());

    let per_character = (kernel_large.units() - kernel_small.units()) / 500;
    let frame = Cost::FRAME.get();
    assert!(
        per_character < frame / 4,
        "a character costs {per_character} units against a {frame}-unit frame, so the ceiling is about {} characters",
        DEFAULT_STEP_BUDGET / per_character.max(1),
    );

    // The two checkers run the same machine on the same closed terms, so agreement here is structural; the factor-two slack covers what each checker's own strategy spends around the machine.
    assert!(
        kernel_large.units() < elaborator_large.units() * 2,
        "kernel {} against elaborator {}",
        kernel_large.units(),
        elaborator_large.units(),
    );

    // Flat in use count: what spec 01's first milestone bought, and the defect this line of work was opened on.
    let (_, _, kernel_used, _) = declaration_cost(&str_literal(500, 3));
    assert_eq!(kernel_used.units(), kernel_small.units());
}

/// A user's own refinement over a packed carrier, `n` bytes long: an authored `rec` fold decides ASCII-ness, and a literal's proof of it is discharged by conversion running the closed fold — exactly the shape of `Str`'s validity check with nothing of `Str` in it.
///
/// The proof is bound by a `let` rather than packed into a dependent record deliberately: checking a *struct declaration* whose field applies a fold to an earlier field costs native stack this fixture does not measure, and overflows the default test-thread stack in debug builds with or without the closed machine — the pre-existing cliff [`a_struct_refinement_field_overflows_the_test_thread_stack`] reproduces, hit nowhere else only because the prelude's own instance of the shape (`/std/BigNat`) is checked on a main thread with four times the stack.
fn ascii_refinement(n: usize) -> String {
    let entries = (0..n)
        .map(|index| format!("0x{:02x}", b'a' + (index % 26) as u8))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        r#"
        use /std/{{Bytes, Byte, Nat, Bool, Eq, Handle}};
        rec all_ascii(b : Bytes) -> Bool =
            match b
            | x[] => true
            | x[h, ..t] =>
                match Byte/to_nat(h) < 0x80
                | true => all_ascii(t)
                | false => false
                end
            end;
        let ok : Eq(all_ascii(x[{entries}]), true) = Eq/refl();
        /std/print("ok")
        "#
    )
}

/// **A pre-existing defect's repro, not a passing test: running it aborts the harness.** Checking a struct declaration whose proof field applies a `rec` fold to an earlier field — `ok: Certified(bytes)`, the exact shape `/std/BigNat` ships — recurses natively past the default test-thread stack, in debug, at any data size, with the closed machine on or off (established by running this with both checkers' machine gates hardwired shut). The prelude never notices because build scripts check it on a main thread with four times the stack; `documentation/syntax.md`'s conformance promise and the "works on the default test-thread stack" invariant are what it breaks. Kept ignored under this name so the defect has an address; the fix is elsewhere ­— somewhere in struct-declaration checking's open-term reduction, which no closed machine can reach.
#[test]
#[ignore = "repro of a pre-existing stack overflow in struct-declaration checking; running it aborts the process"]
fn a_struct_refinement_field_overflows_the_test_thread_stack() {
    let source = r#"
        use /std/{Bytes, Bool, Eq, Handle};
        use /syn/{True, False};
        rec always(b : Bytes) -> Bool =
            match b | x[] => true | x[h, ..t] => always(t) end;
        let Certified(b: Bytes) -> Prop =
            match always(b) | true => True | false => False end;
        struct Wrapped: Type {
            bytes: Bytes,
            ok: Certified(bytes),
        }
        let w : Wrapped = Wrapped { bytes = x[0x61], ok = True/qed() };
        /std/print("ok")
    "#;

    let _ = typecheck_within(DEFAULT_STEP_BUDGET, source);
}

/// **The fixture the closed machine's acceptance asks for: a refinement that is not `Str`.** The machine fires on closedness, not on anything about strings, so a user's fold over a packed carrier gets the same flat depth and sub-frame per-element price a `Str` literal gets — asserted with the same two bounds as [`a_str_literal_costs_transitions_rather_than_frames`], so this cannot quietly hold for the prelude's type and not for a user's.
#[test]
fn a_user_refinement_over_a_packed_carrier_takes_the_same_machine() {
    let (_, _, kernel_small, _) = declaration_cost(&ascii_refinement(500));
    let (_, _, kernel_large, _) = declaration_cost(&ascii_refinement(1000));

    assert_eq!(kernel_large.peak_depth(), kernel_small.peak_depth());

    let per_element = (kernel_large.units() - kernel_small.units()) / 500;
    assert!(
        per_element < Cost::FRAME.get() / 4,
        "an element costs {per_element} units against a {}-unit frame",
        Cost::FRAME.get(),
    );
}

/// **The construction-dominated fixture the acceptance criteria ask for**, and it is deliberately not the accumulate-then-slice shape above: capping fusion made that program's construction linear, so it now refuses on ordinary step cost like any other long computation and would be testing the wrong thing.
///
/// A shift is the shape no representation change can flatten. It has no loop, so nothing amortizes it; its result is `bits(value) + amount` wide and the amount is a numeral the program writes, so no operand size bounds it. Before construction was priced this compiled in well under a second while building fifty megabytes of magnitude, with the counter charging a handful of transitions.
///
/// The paired control is [`a_bound_behind_a_parameter_evaluates_nothing`](super::numeric) in spirit and the second arm here in fact: the same term at an amount the budget affords still folds, so what the first arm demonstrates is a refusal about *size* rather than about the operation.
///
/// **The subject stands under an obligation rather than in a match scrutinee, and it had to move there.** `Bytes/drop` states `Le(k, len(b))`, a decided proposition whose subject stands in a type, so discharging it *is* reducing the shift — which is what makes this fixture about construction at all. The program it replaced put the same shift under `match Nat/le(1, big)`, and nothing in either checker demands a top-level match's scrutinee: what evaluated it was the kernel reducing every non-variable scrutinee to key a case equation, which for a *local-free* scrutinee like this one it then discarded, since `Scope::refine` records only local-bearing keys. The two-tier key stopped that reduction, and this fixture stopped testing anything — both arms of it, since the affordable arm was not folding either. The pricing itself never moved: what changed is that the program has a demander again, chosen to be one a user would actually write.
#[test]
fn an_oversized_construction_is_refused_before_it_is_allocated() {
    let shift = |amount: u64| {
        format!(
            r#"
            use /std/{{Handle, Nat, Bytes, Str}};
            let big : Nat = Nat/shl(1, {amount});
            let b : Bytes = Str/to_bytes("0123456789");
            let rest : Bytes = Bytes/drop(b, big);
            /std/print("ok")
            "#
        )
    };

    let refusal = typecheck_within(DEFAULT_STEP_BUDGET, &shift(1 << 40))
        .expect_err("a shift whose result no budget affords is refused");
    assert!(
        refusal.contains("ran out"),
        "expected a spent-budget refusal, got: {refusal}"
    );

    typecheck_within(DEFAULT_STEP_BUDGET, &shift(3))
        .expect("the same operation at an affordable size still folds");
}

/// Repeated concatenation is bounded by *cumulative* charges even though every individual result fits: the budget is never refunded, so a loop that builds a growing value pays for each of them and runs out on the total.
///
/// The two arms differ only in how many iterations they run, and the small one establishes that the shape itself is affordable — so the large one's refusal is about the accumulation rather than about the program. The refusing count moved once, deliberately: a hundred thousand iterations refused under the recursive strategy and fits under the closed machine, so the arm that must refuse now runs two million — the property held is that a count exists past which cumulative construction refuses, not where it sits.
#[test]
fn a_growing_accumulation_is_bounded_by_what_it_has_already_built() {
    typecheck_within(DEFAULT_STEP_BUDGET, &bytes_growing(2_000))
        .expect("an accumulation this size fits the ordinary budget");

    let refusal = typecheck_within(DEFAULT_STEP_BUDGET, &bytes_growing(2_000_000))
        .expect_err("a thousand times the iterations does not");
    assert!(
        refusal.contains("ran out"),
        "expected a spent-budget refusal, got: {refusal}"
    );
}

/// A `Bits` fold over `width` bits returning a pair, and its single-value twin. `let (a, b) = go(…)` is projection sugar, so the pair form demands the *same* recursive call once per component; the twin demands it once. Everything else about the two is identical, so the gap between their curves is the cost of that second demand and nothing else.
fn paired_fold(width: usize, paired: bool) -> String {
    let ones = vec!["1"; 32].join(", ");
    match paired {
        true => format!(
            r#"
            use /std/{{Nat, Bool, Bits, BigNat}};
            rec go(x: Bits, c: Bool) -> {{Bits, Bool}} =
                match x
                | b[] => (b[], c)
                | b[h, ..t] =>
                    let (rest, out) = go(t, BigNat/xor3(h, c, c));
                    (b[BigNat/xor3(h, c, c), ..rest], out)
                end;
            let (bits, _) = go(Bits/slice(b[{ones}], 0, {width}), false);
            let n : Nat = Nat/div(100, Bits/len(bits) + 3);
            /std/print("")
            "#
        ),
        false => format!(
            r#"
            use /std/{{Nat, Bool, Bits, BigNat}};
            rec go(x: Bits, c: Bool) -> Bits =
                match x
                | b[] => b[]
                | b[h, ..t] => b[BigNat/xor3(h, c, c), ..go(t, BigNat/xor3(h, c, c))]
                end;
            let n : Nat = Nat/div(100, Bits/len(go(Bits/slice(b[{ones}], 0, {width}), false)) + 3);
            /std/print("")
            "#
        ),
    }
}

/// **A recursive call whose result is read at two positions is evaluated once, not twice.**
///
/// The machine records a forced application's value under the application itself, and [`Frame::Head`](curios_core) used to drop that key on the one path whose head is a recursive member — on the premise, written in its eval arm, that *a member selection's calls never repeat within a run because the fold argument strictly shrinks*. That premise is false for every tuple-returning recursion: `let (a, b) = go(…)` lowers to two projections of one call, so each level demanded the same call twice and the fold cost `2^n`.
///
/// What this asserts is the *shape* rather than a figure: the pair form's increment per four bits must not grow. The single-value twin is the control — it makes one demand per level and was never affected, so a regression that slowed both equally would not read as this defect.
///
/// **Run against the defect and observed to fail**, which is what makes it a detector rather than a description: with the record removed the pair form reads `[12146, 39978, 481810, 7547642]`, increments `[27832, 441832, 7065832]`, and the assertion names them. Reproduce by deleting the `Frame::Memo` push at the head frame's entry in `curios-core`'s machine.
///
/// The figures, `cargo test --package curios -- a_recursive_call_read_twice`, 2026-08-24, aarch64-apple-darwin:
///
/// ```text
///   width   single    paired (before)    paired (after)
///       4     8622             11058              9789
///       8     8622             38954             10469
///      12     8622            480850             11149
///      16     8622           7546746             11829
/// ```
#[test]
fn a_recursive_call_read_twice_is_evaluated_once() {
    let units = |width: usize, paired: bool| {
        let source = paired_fold(width, paired);
        let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
        let (_, _, consumption, _) =
            typecheck_with_prelude_measured(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
                .expect("the fold elaborates within the default budget");

        consumption.units()
    };

    let paired = [4usize, 8, 12, 16].map(|width| units(width, true));
    let single = [4usize, 8, 12, 16].map(|width| units(width, false));

    // The control is flat, so the pair form's growth is about the second demand rather than about the fold.
    assert_eq!(
        single[0], single[3],
        "the single-value twin should not grow with width: {single:?}"
    );

    // Linear growth: each further four bits costs about what the previous four did. Doubling per bit makes the later increments explode, which is exactly what this refuses.
    let increments = [
        paired[1] - paired[0],
        paired[2] - paired[1],
        paired[3] - paired[2],
    ];
    assert!(
        increments[2] <= increments[0] * 2,
        "the pair form's cost is not linear in width — a recursive call read twice is being evaluated twice: {paired:?} (increments {increments:?})"
    );
}

/// **A bound over a computed `BigNat` subject is affordable, and stays affordable as the subject widens.**
///
/// `compare_nat` matches two summands *up to universe instances*, which means projecting both through [`project_erased_universes`](curios_core) at every comparison. That projection rebuilds the term, and its traversal mode was the one memoizing mode's opposite: a reduct is a DAG whose tree expansion doubles per level, so each projection walked `2^n` while the *unit* counter — which prices transitions and constructions, not re-walks of one node — saw a linear program. A bound over `BigNat/sub` therefore had linear units and exponential wall clock, which no budget could refuse because no budget could see it.
///
/// What this asserts is the shape: widening the subject by sixteen bits must not multiply its cost. The units are linear either way, so a unit assertion would have passed throughout the defect — the fixture has to read the clock, and it reads it coarsely, as a factor rather than a figure.
///
/// **Run against the defect and observed to fail**, which is what makes it a detector: with `Mode::ErasingUniverses` taken back out of `Visit::memoizes`, it reports `46.397584ms at 7 bits against 67.041283875s at 23`. Reproduce by removing that arm.
///
/// Measured 2026-08-24, `aarch64-apple-darwin`, debug:
///
/// ```text
///   subject                 before        after
///   sub @ 7 bits             43 ms        47 ms
///   sub @ 23 bits          49 400 ms      71 ms
///   Flt/of_decimal         77 000 ms     105 ms
///   Flt/of_str under a bound  78 000 ms   133 ms
/// ```
#[test]
fn a_bound_over_a_widening_subject_stays_affordable() {
    let elapsed = |shift: usize| {
        let subject = match shift {
            0 => "BigNat/of_nat(120) - BigNat/of_nat(10)".to_string(),
            k => format!("BigNat/mul/pow2(BigNat/of_nat(120), {k}) - BigNat/of_nat(10)"),
        };
        let source = format!(
            r#"
            use /std/{{Nat, Bool, BigNat}};
            let n : Nat = Nat/div(100, BigNat/bit_len({subject}));
            /std/print("")
            "#
        );
        let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
        let started = Instant::now();
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the bound discharges within the default budget");

        started.elapsed()
    };

    // The first call carries the run's warm-up, so the pair that is compared is taken after it.
    let _ = elapsed(0);
    let narrow = elapsed(0);
    let wide = elapsed(16);

    // Sixteen further bits doubled the cost sixteen times under the defect. A generous factor keeps this about the *shape* rather than about this host's speed.
    assert!(
        wide < narrow * 20,
        "a bound over a wider subject costs disproportionately more — the universe-erased projection is walking a shared graph as a tree: {narrow:?} at 7 bits against {wide:?} at 23"
    );
}
