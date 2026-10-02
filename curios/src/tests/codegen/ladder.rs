//! The string-walk ladder: what one character of an idiomatic walk costs, divided across the three programs written to divide it.
//!
//! `programs/parse_digits.crs`, `programs/parse_bindless.crs` and `programs/parse_manual.crs` decode the same digit string the same number of times and differ only in what they pay for it. Each step down the ladder removes exactly one cost:
//!
//! | Rung | UTF-8 scan | Closure per character | Bind per character |
//! | --- | --- | --- | --- |
//! | `parse_digits` | yes | yes | yes |
//! | `parse_bindless` | yes | yes | no |
//! | `parse_manual` | no | no | no |
//!
//! So `digits − bindless` is the bind, `bindless − manual` is the closure plus the scan, and the bottom rung is a *ceiling rather than an equivalent* — it declines work the abstraction performs rather than performing it more cheaply.
//!
//! These are real programs rather than fixtures because the question is what idiomatic code costs, and a fixture written to be measured tends to answer a question nobody asked.
//!
//! A change that moves this ladder owes a reading of the two harness workloads beside it, because "moved the column it aimed at" is only a claim if the other columns were looked at.

use {
    super::structural::{compile_raw, functions, user_allocations, wat},
    curios_runtime::{ForeignBindings, MockHost, precompile, run_bytes},
    curios_wasm::to_bytes,
    std::time::Instant,
};

const PARSE_DIGITS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_digits.crs"
));
const PARSE_BINDLESS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_bindless.crs"
));
const PARSE_MANUAL: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_manual.crs"
));
const PARSE_MULTIBYTE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_multibyte.crs"
));

const WALK_MIRROR_BASELINE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_baseline.crs"
));
const WALK_MIRROR_FLAT_ACC: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_flat_acc.crs"
));
const WALK_MIRROR_HELD_SCAN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_held_scan.crs"
));
const WALK_MIRROR_INLINE_STEP: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_inline_step.crs"
));
const WALK_MIRROR_INDEXED: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_indexed.crs"
));

/// Every static figure this attribution leans on, taken over the three programs above.
///
/// # How to run it
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture string_walk_ladder_measurements
/// ```
///
/// It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. Allocation *sites* are counted, not allocations: a site inside the per-character walk runs once per character and a site in the program's setup runs once, and this instrument cannot tell them apart. That is what the timings below are for, and why neither half is quoted without the other.
///
/// # The dynamic half, and how to retake it
///
/// Timings are taken over native binaries rather than in-process, because the in-process module is the raw pre-Binaryen one and the native path is what a user runs:
///
/// ```sh
/// cargo xtask runtime
/// cargo build --package curios
/// cargo run --package curios -- compile programs/parse_digits.crs -o /tmp/parse_digits
/// echo 1000000 | /usr/bin/time -v /tmp/parse_digits    # and the same for the other two rungs
/// ```
///
/// Check the output, not just the clock: all three programs print the same number for the same input, and a rung that has stopped agreeing is measuring something else.
///
/// # The regime
///
/// N is read from stdin and the string decoded is `Nat/to_str(n)`, so at N = 1 000 000 these programs decode a **seven-character** string a million times — seven million characters through a seven-link chain, not one walk over a million. Per-call overhead amortizes differently in the two regimes, so a per-character figure derived here does not transfer to a long string without saying so.
///
/// # What it last printed
///
/// The three rungs together, debug-profile compiler, native binaries, Linux, N = 1 000 000, `user` seconds over five runs: `parse_digits` 0.89–0.91, `parse_bindless` 0.92–0.93, `parse_manual` 0.16. `parse_digits` beats `parse_bindless` although it does strictly more work, since `!` is what separates them: read the bind as an upper bound that has closed rather than as evidence `!` is free — two rungs this close are separated by code layout as much as by work.
///
/// The idiomatic walks alone, in one interleaved session of their own with the two harness workloads as controls, `user` seconds over five runs: `parse_digits` at N = 1 000 000 0.46–0.48, `parse_multibyte` at N = 300 000 0.42–0.43, `lcg` at 100 000 000 0.31, `trees` at 21 0.24–0.26. A delta read across two sessions is not attributable to the change between them, so a change to the walk is timed against a control built from the same tree in the same session.
///
/// The static half:
///
/// ```text
/// parse_digits:   0 closure sites, 2 env sites, 10 slice calls, 443738 bytes of wat
/// parse_bindless: 0 closure sites, 3 env sites, 10 slice calls, 458940 bytes of wat
/// parse_manual:   0 closure sites, 2 env sites, 10 slice calls, 496933 bytes of wat
/// ```
///
/// The env sites that remain belong to `/std/Nat/of_str/1` and `/std/Str/trim_bounds/1` in all three programs — `parse_manual` included, because every rung parses its stdin the same way. **A site count cannot see a loop.** It is kept because it is the half that survives a machine change, and because a site appearing or vanishing is a real event; it is never the half that answers "what does this cost".
#[test]
#[ignore = "measurement: divides the string-walk gap rather than asserting"]
fn string_walk_ladder_measurements() {
    for (label, source) in [
        ("parse_digits", PARSE_DIGITS),
        ("parse_bindless", PARSE_BINDLESS),
        ("parse_manual", PARSE_MANUAL),
    ] {
        let wat = wat(source);
        // The three allocation kinds `structural.rs` separates. A closure's *environment* is the one that answers this document's question: `$envr/<N>$<hint>` carries the hint of the function whose closure it is, so the walk's own allocations can be named rather than merely counted.
        let closures = user_allocations(&wat, "struct.new $clsr/").len();
        let envs = user_allocations(&wat, "struct.new $envr/");
        // A rope view is allocated *inside* the shared `slice` helper, so no `struct.new` site in this module moves when a caller stops slicing — the call sites are what move. Counted directly rather than through [`user_allocations`], whose `$io/` separation exists for instructions that name their own definition and would report nothing here.
        let slices = wat
            .lines()
            .map(str::trim)
            .filter(|line| {
                line.starts_with("call $bytes/slice") || line.starts_with("call $list/slice")
            })
            .count();
        println!(
            "{label}: {closures} closure sites, {} env sites, {slices} slice calls, {} bytes of wat",
            envs.len(),
            wat.len(),
        );

        // Which environments the string walk itself allocates. A site is static and the walk is a loop, so each of these runs once per character rather than once per program — which is exactly why this half cannot be read without the timings above.
        let mut walk: Vec<&str> = envs
            .iter()
            .filter_map(|line| line.split("struct.new ").nth(1))
            .filter(|name| name.contains("/Str/") || name.contains("/Nat/of_str"))
            .collect();
        walk.sort_unstable();
        for name in walk {
            println!("    {name}");
        }
    }
}

// -- the mirror family -------------------------------------------------------
//
// `programs/walk_mirror_*.crs` are five user-level mirrors of the fold's walk over the same multi-byte text: a faithful baseline, and one program per removed obligation — the accumulator tuple (`flat_acc`), the scan argument reconstruction (`held_scan`), the returned scan state (`inline_step`), and the suffix view (`indexed`). They exist because the fold's own obligations cannot be removed one at a time at the source, and they are bounds rather than equivalents: each removal necessarily reshapes the arms around it, and the mirrors carry no validity witness.

/// The module-wide counts the family's claims are stated over: arity-4 tuple constructions (the scan shape), arity-2 tuple constructions (the accumulator shape), rope-slice calls, and calls to the mirror's own `mstep`.
fn mirror_counts(wat: &str) -> (usize, usize, usize, usize) {
    let lines = |needle: &str| {
        wat.lines()
            .map(str::trim)
            .filter(|line| line.starts_with(needle))
            .count()
    };
    (
        lines("struct.new $tuple/4"),
        lines("struct.new $tuple/2"),
        lines("call $bytes/slice"),
        wat.lines()
            .map(str::trim)
            .filter(|line| line.starts_with("call $func/") && line.contains("mstep"))
            .count(),
    )
}

/// The calls to `needle` lexically inside a `loop`, anywhere in the module: the per-character cost, which a module-wide count cannot tell from the one-time plumbing around the walk. Nesting is read off the printer's indentation, which the emitter keeps exact.
fn calls_inside_loops(wat: &str, needle: &str) -> usize {
    let mut open = Vec::<usize>::new();
    let mut count = 0;
    for line in wat.lines() {
        let indent = line.len() - line.trim_start().len();
        let line = line.trim();
        while open
            .last()
            .is_some_and(|&loop_indent| loop_indent >= indent)
        {
            open.pop();
        }
        if line.starts_with("loop ") {
            open.push(indent);
        } else if line.starts_with(needle) && !open.is_empty() {
            count += 1;
        }
    }
    count
}

/// What the family guards: the allocation rungs were built to isolate obligations by *removing* them at the source, and continuation scalar replacement erases those obligations from the baseline itself, so `held_scan` compiles to counts identical to the baseline's and `flat_acc` saves no accumulator construction — naming the value costs nothing. Window virtualization reaches the walk in the faithful spelling too, so `indexed` sheds no slice the baseline pays, and the per-loop fact below is the stronger statement. The step rung keeps its spelling-structural fact: inlining the step removes its calls.
///
/// **The slice claim is per loop, because the module-wide count cannot see the event it guards.** The walk is inlined into the entry, so a slice leaving the walk's loop reads as one fewer in `mirror_counts` and nothing more, and a walk slicing a fresh rope per character passes that count. What keeps the walk's own region virtualized is order: `split_windows` takes the widest admissible region, and runs only once the continuations the walk flows into exist, so no one-continuation sub-region below the walk can claim its positions first.
#[test]
fn walk_mirror_family_isolates_each_obligation() {
    let baseline_wat = wat(WALK_MIRROR_BASELINE);
    let held_scan_wat = wat(WALK_MIRROR_HELD_SCAN);
    let indexed_wat = wat(WALK_MIRROR_INDEXED);
    let baseline = mirror_counts(&baseline_wat);
    let flat_acc = mirror_counts(&wat(WALK_MIRROR_FLAT_ACC));
    let held_scan = mirror_counts(&held_scan_wat);
    let inline_step = mirror_counts(&wat(WALK_MIRROR_INLINE_STEP));
    let indexed = mirror_counts(&indexed_wat);

    assert_eq!(
        held_scan, baseline,
        "naming the reconstruction no longer costs: the spellings compile identically",
    );
    assert!(
        baseline.1 <= flat_acc.1,
        "the idiomatic accumulator pays no more tuples than the hand-flattened spelling: baseline {baseline:?}, flat_acc {flat_acc:?}",
    );
    assert_eq!(
        inline_step.3, 0,
        "inline_step calls no step at all: {inline_step:?}",
    );
    assert!(
        baseline.3 >= 3,
        "the baseline pays a step call per arm: {baseline:?}",
    );
    assert_eq!(
        indexed.2, baseline.2,
        "indexing no longer sheds a slice the baseline pays: baseline {baseline:?}, indexed {indexed:?}",
    );
    for (label, wat) in [
        ("baseline", &baseline_wat),
        ("held_scan", &held_scan_wat),
        ("indexed", &indexed_wat),
    ] {
        assert_eq!(
            calls_inside_loops(wat, "call $bytes/slice"),
            0,
            "{label}'s per-character path slices nothing: the walk is a virtualized window",
        );
    }
}

/// Probe one of the four attributions: the returned scan state, tracked by where its construction lives — which is nowhere.
///
/// The fold does not call `step`. Since it carries the string's validity, each arm names the state it moves to — `cont(rem, lo, hi)` after a lead byte, `lead` or `cont(rem - 1, 0x80, 0xBF)` after a continuation — and the validity it passes on holds that state to the one `step` computes, so no call and no four-result return remain in the loop: the obligation the `inline_step` rung of [`walk_mirror_attribution_measurements`] bounds at roughly a fifth of the walk. Nothing else on this program's path calls `step` either: `Str/trim` moves by position through `Str/At/next`, which decides a character's width from its lead byte, so the module holds no copy of `step` at all.
///
/// **So the per-character path of an idiomatic UTF-8 walk allocates nothing.** `Str/At/next` constructs nothing, and the fold's whole body carries no `struct.new` of any kind: not the accumulator, not the suffix view, not the scan. That last assertion is deliberately about *every* allocation rather than the tuple shapes named above, because a rewrite that moved the cost into some other object would satisfy the narrow reading and fail this one.
#[test]
fn the_per_character_walk_carries_its_scan_without_allocating() {
    let wat = wat(PARSE_MULTIBYTE);
    let split = functions(&wat);
    let count_in = |body: &str, needle: &str| {
        body.lines()
            .map(str::trim)
            .filter(|line| line.starts_with(needle))
            .count()
    };

    assert!(
        split
            .iter()
            .all(|function| !function.name.contains("/std/Str/step")),
        "nothing on this program's path calls `step`, so the module holds no copy of it",
    );
    let next = split
        .iter()
        .find(|function| function.name.contains("/std/Str/At/next"))
        .expect("`trim` steps its positions through `Str/At/next`");
    assert_eq!(
        count_in(next.body, "struct.new"),
        0,
        "and a step from one position to the next constructs nothing",
    );

    // The fold's per-character shape. The accumulator travels as fields, the suffix view is virtualized — the walk carries `(base, offset, length)` through the loop, its slice an extent guard plus an offset sum — and the scan travels as a discriminant and three payload slots through the loop *and* through the call that consumes it.
    let fold = split
        .iter()
        .find(|function| function.name.contains("$/std/Str/fold"))
        .expect("the fold survives as a function in this module");
    let in_fold = |needle: &str| count_in(fold.body, needle);
    assert_eq!(
        in_fold("struct.new"),
        0,
        "the walk's body allocates nothing at all:\n{}",
        fold.body,
    );
    // Matched on the name rather than through `in_fold`, because the emitted call spells the callee's index before its hint and that index is not stable across passes.
    assert_eq!(
        fold.body.matches("$/std/Str/step").count(),
        0,
        "the walk steps its scan by the arm it is in rather than calling `step`",
    );
    assert_eq!(
        in_fold("call $bytes/slice"),
        0,
        "the suffix view is a virtual window: no helper call, no view allocation",
    );
    // One site, not one per arm: the dead-hypothesis peel in `into_ersd::eliminate` emits the read once and binds a name rather than substituting `get(b, 0)` into every arm that mentions it. Exactly one arm runs, so this is a static site count; the per-character cost is one read either way.
    assert_eq!(
        in_fold("call $bytes/read"),
        1,
        "the byte is read once and shared by the arms, not re-read per arm",
    );
    assert_eq!(
        in_fold("call_indirect $clsr/2 $clsr/2"),
        2,
        "the user's step closure"
    );
}

/// The attribution measurement over the mirror family: static counts, raw-and-Binaryen agreement at a small input, and the recorded native timing protocol. Run explicitly:
///
/// ```sh
/// cargo test --package curios --lib --all-features -- --ignored --nocapture walk_mirror_attribution_measurements
/// ```
///
/// # The dynamic half, and how to retake it
///
/// Timings are taken over native binaries, same protocol as [`string_walk_ladder_measurements`], rather than in-process: the debug-built runtime executes the same modules fourteen to forty times slower than the native binaries — the GC libcalls compile at opt-level zero — which overweights exactly the allocation shares this family exists to divide.
///
/// ```sh
/// cargo xtask build
/// ./target/release/curios compile programs/walk_mirror_baseline.crs -o /tmp/wm-baseline    # and the other four
/// echo 300000 | /usr/bin/time -f "%U" /tmp/wm-baseline                                    # five runs each
/// ```
///
/// # What it last measured
///
/// Native binaries, Linux, N = 300 000, five runs each, `user` seconds:
///
/// | Rung | `user` | Isolates | Reading |
/// | --- | --- | --- | --- |
/// | `baseline` | 0.76 0.76 0.84 0.73 0.71 | — | within noise of `parse_multibyte`'s 0.75–0.76, so the mirror calibrates against the real fold |
/// | `flat_acc` | 0.62 0.60 0.53 0.59 0.65 | the accumulator tuple | roughly a fifth of the walk |
/// | `held_scan` | 0.69 0.68 0.75 0.67 0.66 | the scan argument reconstruction | roughly 7% here; the real fold's own sweep measured ~2%, and the real figure is the sweep's — the mirror's smaller step inlines differently |
/// | `inline_step` | 0.57 0.58 0.74 0.64 0.59 | the returned scan state and its call | roughly a fifth, read as a bound: inlining also deduplicates a range test |
/// | `indexed` | 1.22 1.22 1.19 1.23 1.01 | nothing — a negative result | `Bytes/get`'s checked `Option` path costs more than the suffix view it replaces, so this rung cannot attribute the suffix view; the window split's own transformation is that obligation's only honest instrument |
///
/// The two shares this family does measure — the accumulator and the returned scan — are each around twenty percent of the walk.
#[test]
#[ignore = "measurement: attributes the walk's obligations rather than asserting"]
fn walk_mirror_attribution_measurements() {
    let input = "2000";
    let mut agreed = None;
    for (label, source) in [
        ("baseline", WALK_MIRROR_BASELINE),
        ("flat_acc", WALK_MIRROR_FLAT_ACC),
        ("held_scan", WALK_MIRROR_HELD_SCAN),
        ("inline_step", WALK_MIRROR_INLINE_STEP),
        ("indexed", WALK_MIRROR_INDEXED),
    ] {
        let module = compile_raw(source);
        println!("{label:12} {:?} (tuple4, tuple2, slice, mstep calls)", {
            let printed = module.to_string();
            mirror_counts(&printed)
        });
        let raw = precompile(&to_bytes(&module)).expect("raw module precompiles");
        let optimized = crate::to_cwasm(&module).expect("binaryen path precompiles");
        for (kind, cwasm) in [("raw", &raw), ("binaryen", &optimized)] {
            let (system, io) = MockHost::builder().stdin_lines([input]).build();
            let start = Instant::now();
            // SAFETY: both payloads were precompiled above, in this process.
            unsafe { run_bytes(cwasm, system, ForeignBindings::empty()) }.expect("mirror executes");
            let elapsed = start.elapsed().as_secs_f64();
            let printed = String::from_utf8_lossy(&io.output()).trim().to_string();
            println!("{label:12} {kind:9} {elapsed:.3}s prints {printed}");
            let agreed = agreed.get_or_insert_with(|| printed.clone());
            assert_eq!(*agreed, printed, "{label} disagrees with the family");
        }
    }
}
