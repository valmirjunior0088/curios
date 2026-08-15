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
    super::typecheck_within,
    curios_pipeline::{DEFAULT_STEP_BUDGET, recheck_with_prelude, typecheck_with_prelude},
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
    floor(|budget| typecheck_with_prelude(budget, entrypoint, RootSource::none()).is_ok())
}

/// [`floor`] for the kernel alone, over a module elaboration already produced.
///
/// Elaborating once at the default budget and re-certifying the result is what separates the two counters: the module does not change with the budget the kernel is then given, so the sweep measures the kernel's own spend rather than a compile that fails earlier.
fn kernel_floor(entrypoint: &Entrypoint) -> Result<u64, u64> {
    let (module, _obligations) =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, entrypoint, RootSource::none())
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
/// Taken **2026-08-15**, **release**, on `aarch64-apple-darwin`, with construction priced. The floor columns are units of reduction *work*, not transitions, and are not comparable to the pre-pricing table below except in shape.
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
/// Taken **2026-08-15**, **release**, on `aarch64-apple-darwin`, with construction priced — so these floors are units of reduction *work* rather than transitions, and are not comparable to the two tables below except in shape.
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
/// **Every rung is 1×.** The two checkers now agree on this program's cost exactly, on both carriers and at every size — which is more than the memo change alone bought, and says the two evaluators differ in what they do far less than they differed in what they charged.
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

/// **The construction-dominated fixture the acceptance criteria ask for**, and it is deliberately not the accumulate-then-slice shape above: capping fusion made that program's construction linear, so it now refuses on ordinary step cost like any other long computation and would be testing the wrong thing.
///
/// A shift is the shape no representation change can flatten. It has no loop, so nothing amortizes it; its result is `bits(value) + amount` wide and the amount is a numeral the program writes, so no operand size bounds it. Before construction was priced this compiled in well under a second while building fifty megabytes of magnitude, with the counter charging a handful of transitions.
///
/// The paired control is [`a_bound_behind_a_parameter_evaluates_nothing`](super::numeric) in spirit and the second arm here in fact: the same term at an amount the budget affords still folds, so what the first arm demonstrates is a refusal about *size* rather than about the operation.
#[test]
fn an_oversized_construction_is_refused_before_it_is_allocated() {
    let shift = |amount: u64| {
        format!(
            r#"
            use /std/{{Handle, Nat, Bool}};
            let big : Nat = Nat/shl(1, {amount});
            let check = match Nat/lte(1, big) | true => () | false => () end;
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

    typecheck_within(DEFAULT_STEP_BUDGET, &shift(64))
        .expect("the same operation at an affordable size still folds");
}

/// Repeated concatenation is bounded by *cumulative* charges even though every individual result fits: the budget is never refunded, so a loop that builds a growing value pays for each of them and runs out on the total.
///
/// The two arms differ only in how many iterations they run, and the small one establishes that the shape itself is affordable — so the large one's refusal is about the accumulation rather than about the program.
#[test]
fn a_growing_accumulation_is_bounded_by_what_it_has_already_built() {
    typecheck_within(DEFAULT_STEP_BUDGET, &bytes_growing(2_000))
        .expect("an accumulation this size fits the ordinary budget");

    let refusal = typecheck_within(DEFAULT_STEP_BUDGET, &bytes_growing(100_000))
        .expect_err("fifty times the iterations does not");
    assert!(
        refusal.contains("ran out"),
        "expected a spent-budget refusal, got: {refusal}"
    );
}
