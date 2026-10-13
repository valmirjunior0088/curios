//! What a sequence costs to build at the *type* level, measured against what the same loop costs at runtime.
//!
//! `Bytes/slice` states `10 <= Bytes/len(b)`, a decided proposition, so its subject stands in a type and the obligation is discharged by reducing that subject. Writing `Bytes/slice(built, 0, 10)` over a computed accumulator therefore runs the whole accumulation at elaboration time. `curios-core`'s `FUSION_CAP` keeps `normalize_concat` from fusing an all-literal concatenation into one packed value — which would recopy everything accumulated so far on every step, a quadratic — and its measure keeps a length over the resulting spine a single fold.
//!
//! Three arms divide that cost, and the division is the point: the middle arm performs the same number of transitions as the last one and constructs nothing, so whatever separates them is construction rather than machinery.
//!
//! Both carriers are measured, and `Bytes` covers the byte grain only. `Bits` shares `normalize_concat` and `Binary::concat` with it at a different generator width, so it would report the same shape eight times smaller per step; the two carriers here are the ones whose *representations* differ.
//!
//! A second probe reads the same programs the other way round. [`type_level_sequence_cost_measurements`] divides one checker's cost into machinery and construction; [`kernel_memo_charge_measurements`] holds the program fixed and divides the *checkers*, because the compile path puts one budget to both and a user meets whichever demands more.

use {
    super::{Consumed, predicates, typecheck_within},
    curios_core::{Consumption, Cost},
    curios_pipeline::{
        DEFAULT_STEP_BUDGET, recheck_with_prelude, recheck_with_prelude_measured,
        typecheck_with_prelude,
    },
    curios_text::{Entrypoint, RootSource},
    std::time::{Duration, Instant},
};

/// The bound read off an opaque parameter: reduction stops at `b`, the guard refines it once and generically, and nothing is evaluated. The control the other two arms are read against, and the workaround `tests::runtime`'s accumulation measurement relies on.
fn bytes_opaque(n: usize) -> String {
    format!(
        r#"
        use /std/{{Bytes, Nat, Str}};
        let go(i : Nat, acc : Bytes) -> Bytes =
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
        use /std/{{Bytes, Nat, Str}};
        let go(i : Nat, acc : Bytes) -> Bytes =
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
        use /std/{{Bytes, Nat, Str}};
        let go(i : Nat, acc : Bytes) -> Bytes =
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
        use /std/{{List, Nat, Str}};
        let go(i : Nat, acc : List(Nat)) -> List(Nat) =
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
        use /std/{{List, Nat, Str}};
        let go(i : Nat, acc : List(Nat)) -> List(Nat) =
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
        use /std/{{List, Nat, Str}};
        let go(i : Nat, acc : List(Nat)) -> List(Nat) =
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
    let module = typecheck_with_prelude(DEFAULT_STEP_BUDGET, entrypoint, &RootSource::none())
        .expect("the arm elaborates within the default budget")
        .program;

    let module = curios_core::Zonked::project(&module).expect("the checked module is zonked");
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

/// **The regression guard the measurement above cannot be.** A probe is ignored, so nothing runs it; and a cache hit charges nothing in either checker, so both absorb this entire class of defect and stay silent until a budget runs out. A quadratic length would go unnoticed that way, so the guard has to be an ordinary assertion at the ordinary budget.
///
/// The two checkers price a memo hit alike — see [`kernel_memo_charge_measurements`] — so a construction defect surfaces as either checker's honest cost, and this guard is what catches it.
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
/// Taken on top of `d5046c5cc`, **release**, on `x86_64-unknown-linux-gnu`, with the closed machine evaluating the accumulation.
///
/// ```text
/// Bytes
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        800    176.8ms    167.9ms    178.7ms          0.0ns         10.9ms         65536        131072
///       1600    144.0ms    196.3ms    217.4ms         52.3ms         21.1ms        131072        262144
///       3200    144.1ms    251.9ms    288.6ms        107.8ms         36.7ms        262144        524288
///       6400    143.9ms    359.0ms    437.6ms        215.2ms         78.6ms        524288       1048576
///
/// List
///          n     opaque      fixed    growing   fixed-opaque  growing-fixed   floor fixed  floor growing
///        250    169.8ms    164.5ms    184.3ms          0.0ns         19.9ms         32768         32768
///        500    203.6ms    162.4ms    235.0ms          0.0ns         72.6ms         65536         65536
///       1000    201.0ms    188.3ms    272.6ms          0.0ns         84.2ms        131072        131072
///       2000    202.6ms    237.8ms    341.3ms         35.2ms        103.4ms        262144        262144
/// ```
///
/// **The two arms' floors sit within one power of two of each other at every rung**, and both double with the input: the transitions are what a budget sees, and ten bytes of payload price six units a step. Construction shows in the wall-time excess of the `growing-fixed` column alone — tens of milliseconds, growing linearly.
///
/// # Peak memory
///
/// Taken from outside the process, because a high-water mark read from inside it would already have been returned to the allocator:
///
/// ```sh
/// /usr/bin/time -l target/release/curios compile bytes_growing_6400.crs -o out
/// ```
#[test]
#[ignore = "measurement, mixed: reports what a type-level accumulation costs rather than asserting"]
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
/// Taken at `ac663f240`, **release**, on `x86_64-unknown-linux-gnu`, with the closed machine evaluating the accumulation on both sides.
///
/// ```text
/// Bytes
///          n  floor elaborator  floor kernel  divergence
///        800            131072        131072          1x
///       1600            262144        262144          1x
///       3200            524288        524288          1x
///       6400           1048576       1048576          1x
///
/// List
///          n  floor elaborator  floor kernel  divergence
///        250             32768         32768          1x
///        500             65536         65536          1x
///       1000            131072        131072          1x
///       2000            262144        262144          1x
/// ```
///
/// **Parity holds, and structurally.** The two checkers run one shared evaluator for the closed accumulation, and the two floors are one power of two at every rung, which is as fine as the sweep tells them apart. Where the two checkers *do* part is a `Str` literal — [`str_literal_cost_measurements`] carries that table — and it is a difference in how many times the elaborator demands one scan, not in what a demand costs.
///
/// **A memo hit is free in both checkers.** A kernel charging a hit the whole recorded cost of the computation it replaced would refuse for exhaustion, at 8–16× the elaborator's floor, programs the elaborator accepts within its budget — and since the compile path puts one budget to both and meets the larger, that is the number a user would meet, with no disagreement about any rule.
#[test]
#[ignore = "measurement, counted: reports what a memo hit costs the kernel rather than asserting"]
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
/// The literal lowers to `Str { bytes = <Bytes>, @valid = True/qed() }`, and checking that proof makes conversion decide `True ≡ Valid(b)` — `Valid`'s unfolding once, then a `rec` unfold, a `Bytes` peel, a `Byte/to_nat`, `classify`'s ladder and an inductive match, per byte. Nothing else in the program costs anything, so what a floor over this reports is the check.
fn str_literal(n: usize, uses: usize) -> String {
    let literal = "0123456789".repeat(n.div_ceil(10))[..n].to_string();
    let used = (0..uses)
        .map(|index| format!("let use{index} = Str/to_bytes(s);\n"))
        .collect::<String>();

    format!(
        r#"
        use /std/{{Str, Bytes}};
        let s : Str = "{literal}";
        {used}
        /std/print("ok")
        "#
    )
}

/// The same `n` bytes written as a raw `Bytes` literal — the proof-free control, and the whole of what a `Str` literal would cost if its validity were not checked by running a fold.
fn bytes_literal(n: usize) -> String {
    let entries = (0..n)
        .map(|index| format!("0x{:02x}", b'0' + (index % 10) as u8))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        r#"
        use /std/{{Bytes, Nat}};
        let b : Bytes = x[{entries}];
        /std/print(Nat/to_str(Bytes/len(b)))
        "#
    )
}

/// What both checkers spend on `source`, each reporting its own heaviest declaration.
///
/// Reported rather than bisected. A budget floor found from outside costs one whole compile per probe, reports only the larger of the two checkers, and cannot separate depth from the rest at all — which is the separation that matters, because depth is the one row whose size is set by the reduction *strategy* rather than by the term.
fn declaration_cost(source: &str) -> (Consumption, Consumption) {
    let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
    let checked = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
        .expect("the program elaborates within the default budget");
    let elaborator = checked.consumption;
    let module =
        curios_core::Zonked::project(&checked.program).expect("the checked module is zonked");
    let (verdicts, heaviest) =
        recheck_with_prelude_measured(&module, DEFAULT_STEP_BUDGET, |kernel| {
            kernel.heaviest_declaration()
        });

    assert!(verdicts.is_empty(), "the kernel accepts it: {verdicts:?}");

    (elaborator, heaviest)
}

/// One row of the table below: what a program cost each checker, split into depth and everything else.
fn cost_row(label: &str, source: &str) {
    let (elaborator, kernel) = declaration_cost(source);
    let divergence = match elaborator.units() {
        0 => 0.0,
        units => kernel.units() as f64 / units as f64,
    };

    println!(
        "  {label:<22}  {:>10}  {:>6}  {:>9}  {:>10}  {:>6}  {:>9}  {:>6.1}x",
        elaborator.units(),
        elaborator.peak_depth(),
        elaborator.other_units(),
        kernel.units(),
        kernel.peak_depth(),
        kernel.other_units(),
        divergence,
    );
}

/// A literal folded at the type level is folded once however many types mention it. Its certificate, `True/qed()`, has no universe to instantiate, so every mention is one term to the reduction cache; a certificate instantiated at a fresh level per mention would make the four mentions here four keys and four full folds.
#[test]
fn a_literal_mentioned_in_several_types_is_folded_once() {
    let literal = "0123456789".repeat(30);
    let program = |uses: usize| {
        let types = vec![format!("Eq()(Str/len(\"{literal}\"), 300)"); uses].join(", ");
        let proofs = vec!["Eq/refl()"; uses].join(", ");
        format!(
            "use /std/{{Str, Eq, Nat}};\n\nlet q: {{{types}}} = ({proofs},);\n\n/std/print(\"ok\")\n"
        )
    };

    let (one, ..) = declaration_cost(&program(1));
    let (four, ..) = declaration_cost(&program(4));

    assert!(
        four.units() < one.units() * 2,
        "one mention costs {} units and four cost {}",
        one.units(),
        four.units()
    );
}

/// An item's verdict is the same compiled alone and after its neighbours. Item `_a` states one type-level claim over a literal; item `_b` states the same claim and a second one, so a reduct `_a` left behind would answer half of `_b`'s work for nothing. `_b` is the heaviest declaration either way; each checker spends the same on it in both programs, and one unit short of that, each refuses it in both.
///
/// **A guard on this tree rather than a regression test.** Every item's finalization rewrites its universe levels and clears the reducts with them, so `_b` meets a cold table after `_a` whether or not the table outlives a declaration. What keeps it from outliving one is held where it is kept: `curios-elab`'s `a_closed_reduct_does_not_outlive_its_declaration`, and in each checker `what_a_declaration_spends_does_not_depend_on_what_was_reduced_before_it`.
#[test]
fn an_items_verdict_is_the_same_compiled_alone_and_after_its_neighbours() {
    let claim = |literal: String| format!("Eq()(Str/len(\"{literal}\"), 300)");
    let (first, second) = (
        claim("0123456789".repeat(30)),
        claim("abcdefghij".repeat(30)),
    );
    let neighbour = format!("let _a: {first} = Eq/refl();\n\n");
    let program = |neighbours: &str| {
        format!(
            "use /std/{{Str, Eq}};\n\n{neighbours}let _b: {{{first}, {second}}} = (Eq/refl(), Eq/refl(),);\n\n/std/print(\"ok\")\n"
        )
    };
    let (alone, after) = (program(""), program(&neighbour));

    let elaborated = |source: &str, budget: u64| {
        let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
        typecheck_with_prelude(budget, &entrypoint, &RootSource::none())
            .map(|checked| (checked.program, checked.consumption.units()))
    };
    let (alone_module, alone_units) =
        elaborated(&alone, DEFAULT_STEP_BUDGET).expect("`_b` elaborates alone");
    let (after_module, after_units) =
        elaborated(&after, DEFAULT_STEP_BUDGET).expect("`_b` elaborates after `_a`");

    assert_eq!(
        after_units, alone_units,
        "the elaborator spends the same on `_b`"
    );
    assert!(
        elaborated(&alone, alone_units - 1).is_err(),
        "one unit short, the elaborator refuses `_b` alone"
    );
    assert!(
        elaborated(&after, alone_units - 1).is_err(),
        "and after `_a`"
    );

    let certified = |module: &curios_core::Program, budget: u64| {
        let module = curios_core::Zonked::project(module).expect("the checked module is zonked");
        let (verdicts, heaviest) =
            recheck_with_prelude_measured(&module, budget, |kernel| kernel.heaviest_declaration());
        (verdicts.is_empty(), heaviest.units())
    };
    let (accepted, alone_units) = certified(&alone_module, DEFAULT_STEP_BUDGET);
    assert!(accepted, "the kernel accepts `_b` alone");
    let (accepted, after_units) = certified(&after_module, DEFAULT_STEP_BUDGET);
    assert!(accepted, "the kernel accepts `_b` after `_a`");

    assert_eq!(
        after_units, alone_units,
        "the kernel spends the same on `_b`"
    );
    assert!(
        !certified(&alone_module, alone_units - 1).0,
        "one unit short, the kernel refuses `_b` alone"
    );
    assert!(
        !certified(&after_module, alone_units - 1).0,
        "and after `_a`"
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
/// Counted, at `85da54b97`. The unit columns do not depend on the profile — a debug run of a ladder reproduces its release units exactly — and the table carries no wall clock.
///
/// ```text
///   program                      units   depth      other       units   depth      other  kernel/elab
///   Str literal, n=250           22195       1      21171       22242       1      21218     1.0x
///   Str literal, n=500           39445       1      38421       39492       1      38468     1.0x
///   Str literal, n=1000          73945       1      72921       73992       1      72968     1.0x
///   Str literal, n=2000         142945       1     141921      142992       1     141968     1.0x
///   Str literal, n=4000         280945       1     279921      280992       1     279968     1.0x
///   Str literal, n=8000         556945       1     555921      556992       1     555968     1.0x
///   Str n=500, 1 uses            39445       1      38421       39492       1      38468     1.0x
///   Str n=500, 3 uses            39445       1      38421       39492       1      38468     1.0x
///   Bytes literal, n=500          1170       1        146        1254       1        230     1.1x
///   Str n=500, cut               39445       1      38421       39492       1      38468     1.0x
/// ```
///
/// **A character costs 69 units on each checker**, at every rung of the ladder. Dividing the default budget by it puts the ceiling near 434 000 characters, by arithmetic rather than by bisection. Guarded depth is flat in the literal's length on both checkers, and so is the cost in use count.
///
/// **Cutting costs nothing a unit column sees.** The cut row, which cuts through `Str/before` at a position whose boundary is decided by the one byte there, spends exactly the bare `n=500` row's units on both checkers.
///
/// **The `kernel/elab` column reads 1.0× on every `Str` row**, the kernel 47 units over the elaborator at each.
///
/// **The `Bytes` row is the control**: the same bytes with no proof over them cost each checker under 1 300 units, so a `Str` literal's units are its validity's.
#[test]
#[ignore = "measurement, counted: reports what a Str literal costs rather than asserting"]
fn str_literal_cost_measurements() {
    println!(
        "\n  {:<22}  {:>10}  {:>6}  {:>9}  {:>10}  {:>6}  {:>9}  {:>7}",
        "program", "units", "depth", "other", "units", "depth", "other", "kernel/elab",
    );

    for n in [250, 500, 1000, 2000, 4000, 8000] {
        cost_row(&format!("Str literal, n={n}"), &str_literal(n, 0));
    }

    // Flat in use count: a regression here is a literal checked once per mention.
    for uses in [1, 3] {
        cost_row(&format!("Str n=500, {uses} uses"), &str_literal(500, uses));
    }

    // The control: the same bytes with no proof over them.
    cost_row("Bytes literal, n=500", &bytes_literal(500));

    // A cut at a position decides its boundary by the one byte there, discharged by reduction, so this should sit within a few percent of the bare literal — the check is the cost, not the cut.
    cost_row(
        "Str n=500, cut",
        &str_literal(500, 0).replace(
            r#"/std/print("ok")"#,
            r#"/std/print(Str/before(s, Str/At/of_offset(s, 10)))"#,
        ),
    );
}

/// What a web of combinator definitions costs each checker, and what consuming its value does to that.
///
/// ```sh
/// cargo test --release --package curios -- --ignored --nocapture combinator_web_cost_measurements
/// ```
///
/// The third of the parity probes. [`str_literal_cost_measurements`] holds a proof fixed and divides one checker's cost; [`kernel_memo_charge_measurements`] divides the two checkers by budget floor; this one divides them on a program shape where they can part by an *exponent* without disagreeing about any rule — a scrutinee whose subject mentions a binder, which a checker reducing it once per arm to key its case refinement would pay for per definition in the web.
///
/// Three rows per size, differing only in what demands the web's value: nothing, a `match` at a binder, a `match` at a literal. The middle row is the one such keying would grow; the last is the control that says the trigger is the binder rather than the `match`.
///
/// It asserts nothing beyond each arm checking at all. `scrutinee_refinement_measurements` carries the wall clocks beside it.
///
/// # What it last printed
///
/// Counted, at `85da54b97`.
///
/// ```text
///   program                      units   depth      other       units   depth      other  kernel/elab
///   web n=8, applied              5161       5         41        3771       3        699     0.7x
///   web n=8, scrutinized          9429       8       1237       10680       9       1464     1.1x
///   web n=8, closed               5161       5         41        3771       3        699     0.7x
///   web n=12, applied             5161       5         41        3771       3        699     0.7x
///   web n=12, scrutinized         9429       8       1237       10680       9       1464     1.1x
///   web n=12, closed              5161       5         41        3771       3        699     0.7x
///   web n=13, applied             5161       5         41        3771       3        699     0.7x
///   web n=13, scrutinized         9429       8       1237       10680       9       1464     1.1x
///   web n=13, closed              5161       5         41        3771       3        699     0.7x
///   web n=14, applied             5161       5         41        3771       3        699     0.7x
///   web n=14, scrutinized         9429       8       1237       10680       9       1464     1.1x
///   web n=14, closed              5161       5         41        3771       3        699     0.7x
///   web n=20, applied             5161       5         41        3771       3        699     0.7x
///   web n=20, scrutinized         9429       8       1237       10680       9       1464     1.1x
///   web n=20, closed              5161       5         41        3771       3        699     0.7x
/// ```
///
/// **Every unit column is constant across the sizes**, under each consumption and on both checkers: twenty definitions cost what eight do. Keyed on the reduced spelling rather than the written one, the middle row would grow by a factor of two per definition.
///
/// **A `match` at a binder is paid in guarded depth**: three levels more than applying on the elaborator and six more on the kernel, a frame each, which is most of what its row adds. A `match` at a literal costs what applying does.
#[test]
#[ignore = "measurement, counted: reports what a combinator web costs each checker rather than asserting"]
fn combinator_web_cost_measurements() {
    println!(
        "\n  {:<22}  {:>10}  {:>6}  {:>9}  {:>10}  {:>6}  {:>9}  {:>7}",
        "program", "units", "depth", "other", "units", "depth", "other", "kernel/elab",
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
/// What it holds is the shape of a literal's cost rather than a number, and the closed machine is what set the shape: guarded reduction depth *flat* in the literal's length on both checkers, a per-character price in transitions and machine frames far below [`Cost::FRAME`], and neither checker paying a multiple of the other for the same reduction. The first two are the machine's yield — a recursive strategy nests one native reduction level per byte and pays the frame row per character; the third is what a checker's memo stopping short of its internal levels breaks.
///
/// The bound is stated against [`Cost::FRAME`] rather than as a literal because the quantity asserted is *that no per-character native frame is being paid at all*: a per-character price within even a quarter of the frame row means closed evaluation has fallen off the machine and back onto the recursive strategy, which is the silent cliff this guard exists to catch.
#[test]
fn a_str_literal_costs_transitions_rather_than_frames() {
    let (elaborator_small, kernel_small) = declaration_cost(&str_literal(500, 0));
    let (elaborator_large, kernel_large) = declaration_cost(&str_literal(1000, 0));

    // The literal's scan runs on the machine's explicit stack, so doubling the literal moves guarded depth not at all.
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

    // Flat in use count: a literal is checked once however many types mention it.
    let (_, kernel_used) = declaration_cost(&str_literal(500, 3));
    assert_eq!(kernel_used.units(), kernel_small.units());
}

/// A user's own refinement over a packed carrier, `n` bytes long: an authored `rec` fold decides ASCII-ness, and a literal's proof of it is discharged by conversion running the closed fold — exactly the shape of `Str`'s validity check with nothing of `Str` in it.
///
/// The proof is bound by a `let` rather than packed into a dependent record deliberately: this fixture measures the machine, and a *struct declaration* whose field applies a fold to an earlier field is measured by nothing here. What it costs is the strict-positivity walk's, spent after every item over the declarations alone, where no reduction budget reaches and no closed machine applies. That shape is `a_refinement_field_over_a_self_calling_fold_is_admitted` in `positivity.rs`.
fn ascii_refinement(n: usize) -> String {
    let entries = (0..n)
        .map(|index| format!("0x{:02x}", b'a' + (index % 26) as u8))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        r#"
        use /std/{{Bytes, Byte, Nat, Bool, Eq}};
        let all_ascii(b : Bytes) -> Bool =
            match b
            | x[] => true
            | x[h, ..t] =>
                match Byte/to_nat(h) < 0x80
                | true => all_ascii(t)
                | false => false
                end
            end;
        let ok : Eq()(all_ascii(x[{entries}]), true) = Eq/refl();
        /std/print("ok")
        "#
    )
}

/// **The fixture the closed machine's acceptance asks for: a refinement that is not `Str`.** The machine fires on closedness, not on anything about strings, so a user's fold over a packed carrier gets the same flat depth and sub-frame per-element price a `Str` literal gets — asserted with the same two bounds as [`a_str_literal_costs_transitions_rather_than_frames`], so this cannot quietly hold for the prelude's type and not for a user's.
#[test]
fn a_user_refinement_over_a_packed_carrier_takes_the_same_machine() {
    let (_, kernel_small) = declaration_cost(&ascii_refinement(500));
    let (_, kernel_large) = declaration_cost(&ascii_refinement(1000));

    assert_eq!(kernel_large.peak_depth(), kernel_small.peak_depth());

    let per_element = (kernel_large.units() - kernel_small.units()) / 500;
    assert!(
        per_element < Cost::FRAME.get() / 4,
        "an element costs {per_element} units against a {}-unit frame",
        Cost::FRAME.get(),
    );
}

/// **The construction-dominated fixture the acceptance criteria ask for**, and it is deliberately not the accumulate-then-slice shape above: with fusion capped that program's construction is linear, so it refuses on ordinary step cost like any other long computation and would test the wrong thing.
///
/// A shift is the shape no representation change can flatten. It has no loop, so nothing amortizes it; its result is `bits(value) + amount` wide and the amount is a numeral the program writes, so no operand size bounds it. Priced by transitions alone it would compile in well under a second while building fifty megabytes of magnitude.
///
/// The paired control is [`a_bound_behind_a_parameter_evaluates_nothing`](super::numeric) in spirit and the second arm here in fact: the same term at an amount the budget affords still folds, so what the first arm demonstrates is a refusal about *size* rather than about the operation.
///
/// **The subject stands under an obligation rather than in a match scrutinee.** `Bytes/drop` states `Le(k, len(b))`, a decided proposition whose subject stands in a type, so discharging it *is* reducing the shift — which is what makes this fixture about construction at all. Nothing in either checker demands a top-level match's scrutinee, so the same shift under `match Nat/le(1, big)` would evaluate nothing and test nothing; the demander here is one a user would actually write.
#[test]
fn an_oversized_construction_is_refused_before_it_is_allocated() {
    let shift = |amount: u64| {
        format!(
            r#"
            use /std/{{Nat, Bytes, Str}};
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
/// The two arms differ only in how many iterations they run, and the small one establishes that the shape itself is affordable — so the large one's refusal is about the accumulation rather than about the program. The property held is that a count exists past which cumulative construction refuses, not where it sits: a hundred thousand iterations fit under the closed machine.
///
/// The budget is stated, at a thirtieth of the default, because the refusing arm's cost is the budget it spends before refusing: two million iterations against the default's thirty million steps would be among the suite's slowest tests, all of it the wait for exhaustion. The measured floors above put two thousand iterations under `2^19`, so `2^20` affords the fitting arm with the same headroom the default gave it, and a hundred times the iterations exhausts it as surely as a thousand times exhausted the default.
#[test]
fn a_growing_accumulation_is_bounded_by_what_it_has_already_built() {
    const BUDGET: u64 = 1 << 20;

    typecheck_within(BUDGET, &bytes_growing(2_000))
        .expect("an accumulation this size fits the stated budget");

    let refusal = typecheck_within(BUDGET, &bytes_growing(200_000))
        .expect_err("a hundred times the iterations does not");
    assert!(
        refusal.contains("ran out"),
        "expected a spent-budget refusal, got: {refusal}"
    );
}

/// A `Bits` fold over `width` bits returning a pair, and its single-value twin. `let (a, b) = go(…)` is projection sugar, so the pair form demands the *same* recursive call once per component; the twin demands it once. Everything else about the two is identical, so the gap between their curves is the cost of that second demand and nothing else.
///
/// Each program claims the fold's length, an equation the claim's check decides by running the fold. A call that only handed the fold's result on would not be run by a type at all: a divisor's bound over it is decided by what the sum is, whatever its operand comes to.
fn paired_fold(width: usize, paired: bool) -> String {
    let ones = vec!["1"; 32].join(", ");
    match paired {
        true => format!(
            r#"
            use /std/{{Nat, Bool, Bits, Eq}};
            let xor3(a: Bool, b: Bool, c: Bool) -> Bool =
                match a
                | true => match b | true => c | false => Bool/not(c) end
                | false => match b | true => Bool/not(c) | false => c end
                end;
            let go(x: Bits, c: Bool) -> {{Bits, Bool}} =
                match x
                | b[] => (b[], c)
                | b[h, ..t] =>
                    let (rest, out) = go(t, xor3(h, c, c));
                    (b[xor3(h, c, c), ..rest], out)
                end;
            let (bits, _) = go(Bits/slice(b[{ones}], 0, {width}), false);
            let _counted: Eq()(Bits/len(bits), {width}) = Eq/refl();
            /std/print("")
            "#
        ),
        false => format!(
            r#"
            use /std/{{Nat, Bool, Bits, Eq}};
            let xor3(a: Bool, b: Bool, c: Bool) -> Bool =
                match a
                | true => match b | true => c | false => Bool/not(c) end
                | false => match b | true => Bool/not(c) | false => c end
                end;
            let go(x: Bits, c: Bool) -> Bits =
                match x
                | b[] => b[]
                | b[h, ..t] => b[xor3(h, c, c), ..go(t, xor3(h, c, c))]
                end;
            let _counted: Eq()(Bits/len(go(Bits/slice(b[{ones}], 0, {width}), false)), {width}) = Eq/refl();
            /std/print("")
            "#
        ),
    }
}

/// **A recursive call whose result is read at two positions is evaluated once, not twice.**
///
/// The machine records a forced application's value under the application itself, the path whose head is a recursive member included. That *a member selection's calls never repeat within a run because the fold argument strictly shrinks* is false for every tuple-returning recursion: `let (a, b) = go(…)` lowers to two projections of one call, so without the record each level would demand the same call twice and the fold would cost `2^n`.
///
/// What this asserts is the *shape* rather than a figure: the pair form's increment per four bits must not grow. The single-value twin is the control — it makes one demand per level, so it grows by the same amount every four bits, and a regression that slowed both equally would not read as this defect. What is read is the heaviest single judgment, which here is the elaboration of the declaration that claims the fold's length.
///
/// **Run against the defect and observed to fail**, which is what makes it a detector rather than a description: with the record removed the pair form's increments grow with every four bits, and the assertion names them. Reproduce by deleting the `Frame::Memo` push at the head frame's entry in `curios-core`'s machine.
#[test]
fn a_recursive_call_read_twice_is_evaluated_once() {
    let units = |width: usize, paired: bool| {
        let source = paired_fold(width, paired);
        let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fold elaborates within the default budget")
            .consumption
            .units()
    };

    let paired = [4usize, 8, 12, 16].map(|width| units(width, true));
    let single = [4usize, 8, 12, 16].map(|width| units(width, false));

    // The control grows by one demand a level and no more, so what the pair form adds to it is the second demand rather than the fold.
    assert_eq!(
        single[1] - single[0],
        single[3] - single[2],
        "the single-value twin should grow by the same amount every four bits: {single:?}"
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

/// A packed fold costs the elaborator linearly in the length folded. Its per-step bound is the decided `i < i + (kp + 1)`, settled by cancellation; an inductive proof by recursion on the remaining length would make every step pay the steps left, and a fold over a 2 KB literal at the type level would run out of budget where an indexed walk finishes.
#[test]
fn a_packed_fold_costs_linearly_in_its_length() {
    let program = |bytes: usize| {
        let literal = "0123456789".repeat(bytes / 10);
        format!(
            "use /std/{{Str, Bytes, Nat, Eq}};\n\nlet counted: Eq()(Bytes/fold(Str/to_bytes(\"{literal}\"), 0, (_, n) => n + 1), {bytes}) = Eq/refl();\n\n/std/print(\"ok\")\n"
        )
    };

    let (short, ..) = declaration_cost(&program(300));
    let (long, ..) = declaration_cost(&program(600));

    assert!(
        long.units() < short.units() * 3,
        "300 bytes cost {} units and 600 cost {}; a linear fold doubles, a quadratic one quadruples",
        short.units(),
        long.units()
    );
}

/// A bytes walk is run once, however many walks follow it. The grammar reads a run and the comma that closes it, once per token, and the claim reduces it in a type, so the units count the walks: `Parse/bytes/take_while` counts the bytes it reads into a number, stops at the first one refused, and hands on a position at that number. An induction on the remaining length, whose count is its hypothesis read back, multiplies its cost with each token a grammar reads.
#[test]
fn a_bytes_walk_is_run_once_however_many_follow_it() {
    let program = |tokens: usize| {
        let literal = "ab,".repeat(tokens);
        let total = 2 * tokens;
        format!(
            "use /std/{{Parse, Result, Eq, Byte, Bytes, Str, Nat}};\n\nlet token: Parse(Bytes, Nat) =\n    let run = Parse/bytes/take_while((b) => Byte/to_nat(b) != ',')!;\n    let _ = Parse/bytes/expect(',')!;\n    Parse/pure(Bytes/len(run));\n\nlet tokens(count: Nat, acc: Nat) -> Parse(Bytes, Nat) =\n    match count\n    | 0 => Parse/pure(acc)\n    | k + 1 =>\n        let n = token!;\n        tokens(k, acc + n)\n    end;\n\nlet walked: Eq()(Parse/run(tokens({tokens}, 0), Str/to_bytes(\"{literal}\")), Result/success({total})) = Eq/refl();\n\n/std/print(\"ok\")\n"
        )
    };

    let (short, ..) = declaration_cost(&program(4));
    let (long, ..) = declaration_cost(&program(8));

    assert!(
        long.units() < short.units() * 8,
        "4 tokens cost {} units and 8 cost {}; a walk run once grows with its offsets, one run again by each walk after it multiplies with every token",
        short.units(),
        long.units()
    );
}

/// A program stating a claim over one parse: a literal of `letters` letters read by `/std/Parse/text/literal` of the same text, which `claim` is handed as written and answers the declarations for.
///
/// `/std/Parse/run` takes its result's type as a hidden member, which unification solves and the term spells as a metavariable until its declaration is zonked: a claim holding the parse holds that metavariable, and a claim over a declaration bound to the parse holds none.
fn parse_claim(letters: usize, claim: impl FnOnce(&str) -> String) -> String {
    let literal = "a".repeat(letters);
    let claim = claim(&format!(
        "Parse/run(Parse/text/literal(\"{literal}\"), \"{literal}\")"
    ));

    format!(
        "use /std/{{Str, Parse, Result, Bool, Eq}};\nuse /std/Bool/{{Holds, True}};\n\nlet ok(r: Result(Parse/Error, {{}})) -> Bool =\n    match r\n    | success(_) => true\n    | failure(_) => false\n    end;\n\n{claim}\n\n/std/print(\"ok\")\n"
    )
}

/// A claim over a declaration bound to the parse: `prove` is the claim's own declaration, stated over `read`.
fn over_read(prove: &str) -> impl FnOnce(&str) -> String {
    move |parse| format!("let read: Result(Parse/Error, {{}}) = {parse};\n\n{prove}")
}

/// A closed claim costs the elaborator what it costs with nothing left to inference. Reduced as its solutions spell it, the claim holding a solved metavariable is one run of the closed machine, as the claim over `read` is; declined at the gate for the metavariable, it is taken apart by the recursive strategy, each closed piece a run of its own, at several times the cost and growing faster than the text.
#[test]
fn a_claim_over_a_solved_metavariable_costs_what_the_claim_written_out_costs() {
    for letters in [16, 32] {
        let (inline, ..) = declaration_cost(&parse_claim(letters, |parse| {
            format!("let _claim: Holds(ok({parse})) = True/qed();")
        }));
        let (named, ..) = declaration_cost(&parse_claim(
            letters,
            over_read("let _claim: Holds(ok(read)) = True/qed();"),
        ));

        assert!(
            inline.units() * 4 <= named.units() * 5,
            "at {letters} letters the claim costs {} units with the parse inline and {} with it named",
            inline.units(),
            named.units()
        );
    }
}

/// An equation costs each checker what the decision of the same fact costs. A claim `Eq()(lhs, rhs)` is typed by a run that evaluates both sides, a family's operands being evaluated as any substituend is, and conversion then asks for each side by itself: what the first run answered is kept by the closed term (`curios_core::ClosedMemo`), so the second asking is a probe. Asked twice, the equation cost twice the decision.
#[test]
fn an_equation_costs_what_its_decision_costs() {
    for letters in [16, 32] {
        let decided = declaration_cost(&parse_claim(
            letters,
            over_read("let _claim: Holds(ok(read)) = True/qed();"),
        ));
        let equated = declaration_cost(&parse_claim(
            letters,
            over_read("let _claim: Eq()(ok(read), true) = Eq/refl();"),
        ));

        for (checker, decided, equated) in [
            ("elaborator", decided.0, equated.0),
            ("kernel", decided.1, equated.1),
        ] {
            assert!(
                equated.units() * 4 <= decided.units() * 5,
                "at {letters} letters the {checker} spends {} units on the equation and {} on the decision",
                equated.units(),
                decided.units()
            );
        }
    }
}

/// A published item's binders state whether they bind a proof, each asked at its declared type under the binders before it: a function's proof does, and its type parameter, its number and a value at the type parameter do not; of two `let`s, the equation does and the sum does not.
#[test]
fn a_published_binder_states_whether_it_binds_a_proof() {
    let source = "use /std/{Nat, Eq};\nuse /std/Bool/{Holds};\n\nlet through(A: Type, n: Nat, h: Holds(0 <= n), x: A) -> Nat =\n    let twice = n + n;\n    let same: Eq()(n, n) = Eq/refl();\n    twice;\n\n/std/print(\"ok\")\n";
    let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
    let program = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
        .expect("the program elaborates within the default budget")
        .program;
    let through = program
        .module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition)
                if definition.name.faithful().to_string() == "/through" =>
            {
                Some(definition)
            }
            _ => None,
        })
        .expect("the program defines `through`");

    let curios_core::Subterm::Func(function) = &*through.body else {
        panic!("a function literal: {:?}", through.body);
    };
    assert_eq!(function.telescope.proofs(), [false, false, true, false]);

    let curios_core::Subterm::Let(block) = &**function.telescope.terminal() else {
        panic!("a `let`: {:?}", function.telescope.terminal());
    };
    assert_eq!(
        [block.tail.binds_proof(0), block.tail.binds_proof(1)],
        [false, true]
    );
}

/// A claim over a call with a proof argument: `through` hands its number on and never reads its proof, and `proof` is the argument written at it.
///
/// `checked` is no recursive member's call: it eliminates the lemma, so taking it to a weak-head value runs the lemma's induction, `steps` deep, where a recursive call alone would stay folded.
fn proof_argument_claim(steps: usize, proof: &str) -> String {
    format!(
        "use /std/{{Nat, Eq}};\n\nlet lemma(n: Nat) -> Eq()(n, n) =\n    match n\n    | 0 => Eq/refl()\n    | k + 1 => match lemma(k) | refl() => Eq/refl() end\n    end;\n\nlet checked(n: Nat) -> Eq()(n, n) =\n    match lemma(n) | refl() => Eq/refl() end;\n\nlet through(n: Nat, h: Eq()(n, n)) -> Nat = n;\n\nlet _claim: Eq()(through({steps}, {proof}), {steps}) = Eq/refl();\n\n/std/print(\"ok\")\n"
    )
}

/// A type does not run a proof it only binds. The claim's call hands `through` a proof that costs an induction a thousand deep, at a binder that states it binds one, so both checkers bind it as written and the claim costs what it costs with `Eq/refl()` there. Evaluated before it is bound, the proof is the whole of the claim's cost, twenty times the rest.
#[test]
fn a_type_does_not_run_a_proof_it_only_binds() {
    let (proved_elaborated, proved_checked) =
        declaration_cost(&proof_argument_claim(1000, "checked(1000)"));
    let (plain_elaborated, plain_checked) =
        declaration_cost(&proof_argument_claim(1000, "Eq/refl()"));

    assert!(
        proved_elaborated.units() * 4 <= plain_elaborated.units() * 5
            && proved_checked.units() * 4 <= plain_checked.units() * 5,
        "the elaborator spends {} units with the proof computed and {} with it trivial, and the kernel {} and {}",
        proved_elaborated.units(),
        plain_elaborated.units(),
        proved_checked.units(),
        plain_checked.units()
    );
}

/// A program whose one claim is that `/std/http/Url/lit` reads a URL of `characters` characters: `lit` discharges `Valid(s)` by running `/std/http/Url/of_str` in a type, so what the declaration costs is what the reader costs a type.
fn url_literal(characters: usize) -> String {
    let literal = match characters {
        32 => "https://example.com:8443/a/b?x=1",
        64 => "https://example.com:8443/api/v1/items/42/parts?sort=name&page=12",
        128 => {
            "https://example.com:8443/api/v1/items/42/parts/7/revisions/2024/attachments/photo.png?sort=name&page=12&size=50&filter=open&x=12"
        }
        other => panic!("no literal of {other} characters"),
    };
    assert_eq!(literal.len(), characters);

    format!(
        "use /std/http/{{Url}};\n\nlet home: Url =\n    Url/lit(\"{literal}\");\n\n/std/print(Url/to_str(home))\n"
    )
}

/// What reading the 32-character literal cost the heavier checker while `/std/http/Url/of_str` read the bytes of its text, counted at `eef14bad1`: the least budget under which the program is accepted.
const URL_OVER_BYTES_AT_32: u64 = 69_816;

/// **A URL literal costs a type what its length does, read as text.** `/std/http/Url/of_str` reads `Parse(Str, Url)`, each piece a cut of the input between two positions, and `lit` runs it in a type. A position's bound is a proof and a walk's step carries one, and neither checker runs a proof it only binds, so the reader over text costs under twice what the reader over bytes did, and a literal twice as long costs no more than twice as much.
///
/// With the proofs run, a doubling costs more than a doubling, since each step's bound is a lemma by induction on the offset it stands at.
#[test]
fn a_url_literal_read_as_text_costs_a_type_what_its_length_does() {
    let heavier = |characters: usize| {
        let (elaborator, kernel) = declaration_cost(&url_literal(characters));

        elaborator.units().max(kernel.units())
    };
    let [short, middle, long] = [32, 64, 128].map(heavier);

    assert!(
        short < 2 * URL_OVER_BYTES_AT_32,
        "the 32-character literal costs {short} units, where the reader over bytes cost {URL_OVER_BYTES_AT_32}"
    );
    assert!(
        middle <= 2 * short && long <= 2 * middle,
        "a doubling costs more than a doubling: {short}, {middle} and {long} units at 32, 64 and 128 characters"
    );
}

/// A program whose claim is that `parser` reads `input`, stated over whether it succeeded and over a declaration bound to the read, so the claim holds nothing of what was read and no member left to inference.
fn primitive_claim(carrier: &str, parser: &str, input: &str) -> String {
    format!(
        "use /std/{{Str, Bytes, Byte, Char, Nat, Parse, Result, Bool}};\nuse /std/Bool/{{Holds, True}};\n\nlet ok(@A: Type, r: Result(Parse/Error, A)) -> Bool =\n    match r\n    | success(_) => true\n    | failure(_) => false\n    end;\n\nlet times(@I: Type, use Parse/Input(I), @A: Type, n: Nat, p: Parse(I, A)) -> Parse(I, {{}}) =\n    match n\n    | 0 => Parse/pure(())\n    | k + 1 =>\n        let _ = p!;\n        times(k, p)\n    end;\n\nlet reader: Parse({carrier}, {{}}) =\n    let _ = {parser}!;\n    Parse/pure(());\n\nlet read: Result(Parse/Error, {{}}) = Parse/run(reader, {input});\n\nlet _claim: Holds(ok(read)) = True/qed();\n\n/std/print(\"ok\")\n"
    )
}

/// What each primitive of `/std/Parse/text` costs each checker beside its twin in `/std/Parse/bytes`, over 32 letters: a text primitive steps by a character and hands on a position that is a boundary of valid text, where its twin steps by a byte.
///
/// ```sh
/// cargo test --package curios --all-features -- --ignored --nocapture parse_primitive_measurements
/// ```
///
/// Counted, so a reading transports between machines. A change to `/std/Parse`, to `/std/Str/At` or to what the closed machine evaluates retakes it.
///
/// # What it last printed
///
/// At `96d967b9b`:
///
/// ```text
///   primitive            text elab      kernel  bytes elab      kernel
///   literal                  74947       74973       18390       18416
///   take_while               48931       48957        9910        9936
///   char, byte               58355       58381       31930       31956
///   char_if, byte_if         64538       64564       38120       38146
///   expect                   60868       60894       35136       35162
///   rest                      8391        6494        8699        6515
///   until                    49214       49240
/// ```
///
/// **A text primitive costs four to five times its twin where the primitive is the walk**, `literal` and `take_while`, and under twice where each step is a parser run through `times`: `char`, `char_if` and `expect`. `rest` cuts once and reads nothing.
#[test]
#[ignore = "measurement, counted: reports what each parser primitive costs over text and over bytes rather than asserting"]
fn parse_primitive_measurements() {
    let letters = "a".repeat(32);
    let text = format!("\"{letters}\"");
    let bytes = format!("Str/to_bytes(\"{letters}\")");
    let rows: [(&str, String, Option<String>); 7] = [
        (
            "literal",
            format!("Parse/text/literal({text})"),
            Some(format!("Parse/bytes/literal({text})")),
        ),
        (
            "take_while",
            "Parse/text/take_while((c) => Char/to_nat(c) == 'a')".to_string(),
            Some("Parse/bytes/take_while((b) => Byte/to_nat(b) == 'a')".to_string()),
        ),
        (
            "char, byte",
            "times(32, Parse/text/char)".to_string(),
            Some("times(32, Parse/bytes/byte)".to_string()),
        ),
        (
            "char_if, byte_if",
            "times(32, Parse/text/char_if((c) => Char/to_nat(c) == 'a'))".to_string(),
            Some("times(32, Parse/bytes/byte_if((b) => Byte/to_nat(b) == 'a'))".to_string()),
        ),
        (
            "expect",
            "times(32, Parse/text/expect('a'))".to_string(),
            Some("times(32, Parse/bytes/expect('a'))".to_string()),
        ),
        (
            "rest",
            "Parse/text/rest".to_string(),
            Some("Parse/bytes/rest".to_string()),
        ),
        ("until", "Parse/text/until(\"b\")".to_string(), None),
    ];

    println!(
        "\n  {:<18}  {:>10}  {:>10}  {:>10}  {:>10}",
        "primitive", "text elab", "kernel", "bytes elab", "kernel"
    );
    for (label, over_text, over_bytes) in rows {
        let input = match label {
            "until" => format!("\"{letters}b\""),
            _ => text.clone(),
        };
        let (elaborator, kernel) = declaration_cost(&primitive_claim("Str", &over_text, &input));
        let twin = over_bytes
            .map(|parser| declaration_cost(&primitive_claim("Bytes", &parser, &bytes)))
            .map_or_else(
                || format!("{:>10}  {:>10}", "", ""),
                |(elaborator, kernel)| {
                    format!("{:>10}  {:>10}", elaborator.units(), kernel.units())
                },
            );

        println!(
            "  {label:<18}  {:>10}  {:>10}  {twin}",
            elaborator.units(),
            kernel.units(),
        );
    }
}

/// What each claim of the type-level benchmark set costs each checker: an equality a type states and `Eq/refl()` proves, so that checking the program is reducing the claim's left side to its right. Six take a `Str` apart, one reads a `Flt`, one searches a `Str` and maps the position found.
///
/// ```sh
/// cargo test --package curios --all-features -- --ignored --nocapture type_level_claim_measurements
/// ```
///
/// Counted, so a reading transports between machines. A change to how a `Str` or a `Flt` is read at the type level retakes it and accounts for each row that moved. Each checker reports its heaviest declaration, and a program that states no claim reads under 1 300 units ([`str_literal_cost_measurements`]'s `Bytes` row), so each row is its claim's.
///
/// # What it last printed
///
/// At `96d967b9b`:
///
/// ```text
///   claim                        units   depth      other       units   depth      other  kernel/elab
///   Str/split                    83152      19      63696       49320       1      48296     0.6x
///   Str/split_once               63563       3      60491       50490       2      48442     0.8x
///   Str/lines                    54456      12      42168       36288       1      35264     0.7x
///   Str/replace                  65983      15      50623       36678       1      35654     0.6x
///   Str/trim                     60698       3      57626       47570       1      46546     0.8x
///   Str/strip_suffix             55609       3      52537       55317       2      53269     1.0x
///   Flt/of_str                   67810       3      64738       66967       2      64919     1.0x
///   Str/index_of                 42154       3      39082       41630       2      39582     1.0x
/// ```
///
/// **A builder nests in the elaborator**: `Str/split`, `Str/lines` and `Str/replace` hand on what they have built, an argument the elaborator's reducer holds a level deeper for each piece, where the kernel's depth stays at one.
///
/// **The first five rows cost the kernel six to eight tenths of what they cost the elaborator**, and the last three cost the two alike.
///
/// **`Flt/of_str` is a grammar run**: reading `"-Infinity"` runs the reader's `Parse(Str, Flt)` grammar over nine characters, so its row is what the grammar costs each checker.
#[test]
#[ignore = "measurement, counted: reports what each type-level claim costs rather than asserting"]
fn type_level_claim_measurements() {
    let claims = [
        (
            "Str/split",
            r#"Str/split("a€€b€c", "€")"#,
            r#"["a", "", "b", "c"]"#,
        ),
        (
            "Str/split_once",
            r#"Str/split_once("key=value=x", "=")"#,
            r#"Option/some(("key", "value=x"))"#,
        ),
        ("Str/lines", r#"Str/lines("a\r\nb\n")"#, r#"["a", "b"]"#),
        (
            "Str/replace",
            r#"Str/replace("a€b€", "€", "-")"#,
            r#""a-b-""#,
        ),
        ("Str/trim", r#"Str/trim(" \té \n")"#, r#""é""#),
        (
            "Str/strip_suffix",
            r#"Str/strip_suffix("héllo", "llo")"#,
            r#"Option/some("hé")"#,
        ),
        (
            "Flt/of_str",
            r#"Flt/of_str("-Infinity")"#,
            "Option/some(-inf.0)",
        ),
        (
            "Str/index_of",
            r#"Option/map(Str/index_of("abcdefgh", 'h'), Str/At/to_offset)"#,
            "Option/some(7)",
        ),
    ];

    println!(
        "\n  {:<22}  {:>10}  {:>6}  {:>9}  {:>10}  {:>6}  {:>9}  {:>7}",
        "claim", "units", "depth", "other", "units", "depth", "other", "kernel/elab",
    );
    for (label, left, right) in claims {
        cost_row(
            label,
            &format!(
                "use /std/{{Str, Option, Eq, Nat, Flt, List}};\n\nlet _claim: Eq()({left}, {right}) = Eq/refl();\n\n/std/print(\"ok\")\n"
            ),
        );
    }
}
