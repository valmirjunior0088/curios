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
    curios::{TALLIES, test_support::*},
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_wasm::{Names, to_bytes},
    std::{cell::RefCell, collections::BTreeMap},
    xbench::{Measure, phase, read, turn},
};

const PARSE_DIGITS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_digits.crs"
));

const PARSE_BINDLESS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_bindless.crs"
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

const PARSE_MANUAL: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_manual.crs"
));

/// Every static figure this attribution leans on, taken over the three programs above.
///
/// `cargo xbench measure string_walk_ladder_measurements` takes it: `<rung>/closure sites`, `/env sites`, `/slice calls` and `/wat bytes`, and `<rung>/walk/<environment>` the sites of each environment the walk itself allocates. It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. Allocation *sites* are counted, not allocations: a site inside the per-character walk runs once per character and a site in the program's setup runs once, and this instrument cannot tell them apart. That is what the timings below are for, and why neither half is quoted without the other.
///
/// # The dynamic half, and how to retake it
///
/// Timings are taken over native binaries rather than in-process, because the in-process module is the raw pre-Binaryen one and the native path is what a user runs:
///
/// ```sh
/// cargo xtask compiler launcher
/// cargo build --release --package curios
/// cargo run --release --package curios -- compile programs/parse_digits.crs -o /tmp/parse_digits
/// /usr/bin/time -v /tmp/parse_digits 1000000    # and the same for the other two rungs
/// ```
///
/// Check the output, not just the clock: all three programs print the same number for the same input, and a rung that has stopped agreeing is measuring something else.
///
/// # The regime
///
/// N is read from the command line and the string decoded is `Nat/to_str(n)`, so at N = 1 000 000 these programs decode a **seven-character** string a million times — seven million characters through a seven-link chain, not one walk over a million. Per-call overhead amortizes differently in the two regimes, so a per-character figure derived here does not transfer to a long string without saying so.
///
/// # What it last printed
///
/// The three rungs together, timed at `00627ea06`, release compiler, native binaries, Linux, N = 1 000 000, `user` seconds as the median and span of five interleaved runs: `parse_digits` 0.20 [0.20–0.21], `parse_bindless` 0.23 [0.23–0.24], `parse_manual` 0.19 [0.18–0.19]. `parse_digits` beats `parse_bindless` although it does strictly more work, since `!` is what separates them: read the bind as an upper bound that has closed rather than as evidence `!` is free — two rungs this close are separated by code layout as much as by work.
///
/// The idiomatic walks alone, in one interleaved session of their own with the two harness workloads as controls, timed at `00627ea06` as the rungs are, `user` seconds as the median and span of five runs: `parse_digits` at N = 1 000 000 0.20 [0.20–0.20], `parse_multibyte` at N = 300 000 0.19 [0.19–0.20], `lcg` at 100 000 000 0.37 [0.36–0.37], `trees` at 21 0.17 [0.16–0.18]. A delta read across two sessions is not attributable to the change between them, so a change to the walk is timed against a control built from the same tree in the same session.
///
/// The static half, taken at `ac663f240`:
///
/// ```text
/// parse_digits:   0 closure sites, 4 env sites, 21 slice calls, 1153927 bytes of wat
/// parse_bindless: 0 closure sites, 6 env sites, 21 slice calls, 1202540 bytes of wat
///     $envr/2093$/std/Nat/of_str/1
/// parse_manual:   0 closure sites, 4 env sites, 21 slice calls, 1273107 bytes of wat
/// ```
///
/// Four env sites in every rung, `parse_manual` included, carry the hint of a local `go`: two loops, each of which builds its environment once in `$start` and once in its own body. `parse_bindless` adds `/decode/1` and `/std/Nat/of_str/1`, the one site the walk's own filter names. **A site count cannot see a loop.** It is kept because it is the half that survives a machine change, and because a site appearing or vanishing is a real event; it is never the half that answers "what does this cost".
pub fn string_walk_ladder_measurements() {
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
        for (figure, count) in [
            ("closure sites", closures),
            ("env sites", envs.len()),
            ("slice calls", slices),
            ("wat bytes", wat.len()),
        ] {
            read(
                &format!("{label}/{figure}"),
                Measure::Count,
                None,
                count as u64,
            );
        }

        // Which environments the string walk itself allocates. A site is static and the walk is a loop, so each of these runs once per character rather than once per program — which is exactly why this half cannot be read without the timings above.
        let mut walk = BTreeMap::<&str, usize>::new();
        for name in envs
            .iter()
            .filter_map(|line| line.split("struct.new ").nth(1))
            .filter(|name| name.contains("/Str/") || name.contains("/Nat/of_str"))
        {
            *walk.entry(name).or_default() += 1;
        }
        for (name, sites) in walk {
            read(
                &format!("{label}/walk/{name}"),
                Measure::Count,
                None,
                sites as u64,
            );
        }
    }
}

/// The attribution measurement over the mirror family: static counts, raw-and-Binaryen agreement, and the family timed.
///
/// `cargo xbench measure walk_mirror_attribution_measurements` takes it: `<member>/tuple4`, `/tuple2`, `/slice calls` and `/mstep calls`, the module-wide counts the family's claims are stated over, and `<member>/raw` and `<member>/binaryen`, a run's time on each path at N = 300 000, every member's two runs turned round-robin seven times after an untimed round, so no member is measured on a warmer machine than another.
///
/// # The dynamic half, and how to retake it
///
/// The test times the family in process, under the release build the runner takes it in, so an allocation share reads as the shipped runtime's. The native protocol of [`string_walk_ladder_measurements`] retakes it outside a test:
///
/// ```sh
/// cargo xtask distribution build
/// ./target/release/curios compile programs/walk_mirror_baseline.crs -o /tmp/wm-baseline    # and the other four
/// echo 300000 | /usr/bin/time -f "%U" /tmp/wm-baseline                                    # five runs each
/// ```
///
/// # What it last measured
///
/// Taken at `9c543476a`, x86-64 Linux, at a load average under two, in process in the test profile at N = 300 000: one warmup round, then seven rounds over the whole family, each round timing every member so no member is measured on a warmer machine than another. Median of the seven, with their span.
///
/// | Rung | raw | binaryen | Isolates | Reading |
/// | --- | --- | --- | --- | --- |
/// | `baseline` | 0.2939s [0.2901–0.2953] | 0.2072s [0.2045–0.2092] | — | the reference the rest are read against |
/// | `flat_acc` | 0.2942s [0.2918–0.2976] | 0.2049s [0.2040–0.2121] | the accumulator tuple | no difference proven: its spans overlap the baseline's on both paths |
/// | `held_scan` | 0.2992s [0.2955–0.3013] | 0.2091s [0.2063–0.2098] | the scan argument reconstruction | 2% over the baseline raw, its span clearing the baseline's by two tenths of a millisecond, and no difference proven once Binaryen has run |
/// | `inline_step` | 0.2582s [0.2566–0.2600] | 0.1731s [0.1720–0.1743] | the returned scan state and its call | 0.88× the baseline raw and 0.84× once Binaryen has run, spans far apart on both paths |
/// | `indexed` | 0.3769s [0.3730–0.3789] | 0.2930s [0.2907–0.2977] | nothing — a negative result | 1.28× the baseline raw and 1.41× once Binaryen has run: `Bytes/get`'s checked `Option` path costs more than the suffix view it replaces, so this rung cannot attribute the suffix view; the window split's own transformation is that obligation's only honest instrument |
///
/// **What this family can and cannot attribute.** One obligation is measurable here: the returned scan state and its call, which `inline_step` removes for about a sixth of the walk once optimized. The accumulator tuple is not: `flat_acc` reads inside the baseline's span on both paths. Nor, once optimized, is the scan argument reconstruction: `held_scan`'s raw excess of 2% is gone once Binaryen has run. The suffix view cannot be attributed by this family, since `indexed` replaces it with a path that costs more.
pub fn walk_mirror_attribution_measurements() {
    // The size the family's own recorded protocol uses. At 2000 the walk is a minority of each run — module
    // instantiation is the rest, and it scales with a member's module rather than with what the member removed,
    // which prices the largest module slowest whatever its walk costs.
    let input = "300000";

    // Every member is compiled before anything is timed: a compile between two timings is a position
    // effect, and this family exists to compare its members to each other.
    let prepared = [
        ("baseline", WALK_MIRROR_BASELINE),
        ("flat_acc", WALK_MIRROR_FLAT_ACC),
        ("held_scan", WALK_MIRROR_HELD_SCAN),
        ("inline_step", WALK_MIRROR_INLINE_STEP),
        ("indexed", WALK_MIRROR_INDEXED),
    ]
    .map(|(label, source)| {
        let (module, foreigns) = compile_raw_program(source);
        let counts = mirror_counts(&module.to_string());
        start_test_engine();
        let raw = Compiled::of(
            curios_cranelift::compile(&to_bytes(&module, &Names::default()))
                .expect("raw module compiles"),
            foreigns.clone(),
        );
        let optimized = to_compiled(&module, foreigns).expect("binaryen path compiles");

        (label, counts, raw, optimized)
    });

    for (label, (tuple4, tuple2, slices, steps), _, _) in &prepared {
        for (figure, count) in [
            ("tuple4", tuple4),
            ("tuple2", tuple2),
            ("slice calls", slices),
            ("mstep calls", steps),
        ] {
            read(
                &format!("{label}/{figure}"),
                Measure::Count,
                None,
                *count as u64,
            );
        }
    }

    // What the first run printed, which every member's every run must print too.
    let agreed = RefCell::new(None);
    let mut runs = prepared
        .iter()
        .flat_map(|(label, _, raw, optimized)| {
            [("raw", raw), ("binaryen", optimized)].map(|(path, compiled)| (*label, path, compiled))
        })
        .map(|(label, path, compiled)| {
            let agreed = &agreed;
            move || {
                let (system, io) = MockHost::builder().args(["program", input]).build();
                phase(TALLIES, || {
                    compiled
                        .run(system, ForeignBindings::empty())
                        .expect("mirror executes")
                })
                .read(&format!("{label}/{path}"), None);

                let printed = String::from_utf8_lossy(&io.output()).trim().to_string();
                let mut agreed = agreed.borrow_mut();
                let agreed = agreed.get_or_insert_with(|| printed.clone());
                assert_eq!(*agreed, printed, "{label} disagrees with the family");
            }
        })
        .collect::<Vec<_>>();
    let mut turned = runs
        .iter_mut()
        .map(|run| run as &mut dyn FnMut())
        .collect::<Vec<_>>();
    turn(7, &mut turned);
}

/// One decoder over an input its second line of standard input sizes, so nothing about the document is closed and no evaluation folds the walk away.
const DECODER_DOUBLING: &str = r#"
use /std/{Str, Nat, Byte, Bytes, Parse, Result, List, Option, Json, Html};

let token_walk: Parse(Bytes, List(Bytes)) =
    Parse/sep_by0(Parse/bytes/take_while((b) => Byte/to_nat(b) != ','), Parse/bytes/expect(','));

let work(mode: Str, n: Nat) -> Nat =
    match
    | Str/eql(mode, "json") =>
        match Json/decode(Str/flatten(["[", Str/repeat("1, ", n), "1]"]))
        | success(value) => match value | arr(items) => List/length(items) | _ => 0 end
        | failure(_) => 0
        end
    | Str/eql(mode, "html") =>
        match Html/decode(Str/repeat("<p>x</p>", n)) | success(_) => n | failure(_) => 0 end
    | Str/eql(mode, "tokens") =>
        match Parse/run(token_walk, Str/to_bytes(Str/repeat("abcdefgh,", n)))
        | success(runs) => List/length(runs)
        | failure(_) => 0
        end
    | _ => 0
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let mode = /std/List/try_get(arguments, 1);
    let size = /std/List/try_get(arguments, 2);
    let answer: Str =
        match mode
        | none() => "0"
        | some(m) =>
            match size
            | none() => "0"
            | some(s) =>
                match Str/of_bytes(m)
                | failure(_) => "0"
                | success(decoder) =>
                    match Option/bind(Result/to_option(Str/of_bytes(s)), Nat/of_str)
                    | none() => "0"
                    | some(n) => Nat/to_str(work(decoder, n))
                    end
                end
            end
        end;
    /std/print(answer);
entry()
"#;

/// What a decoder's time does when its document doubles: `Json/decode` over `[1, 1, …]`, `Html/decode` over `<p>x</p>…` and a `take_while` and `expect` token walk over `abcdefgh,…`, each at three sizes that double, timed in process over the Binaryen path. A decoder that costs what it reads doubles; one whose walk reads what is left of its input at every token quadruples.
///
/// `cargo xbench measure decoder_doubling_slope` takes it: `<decoder> at <size>`, a run's time, every decoder at every size turned round-robin five times after an untimed round. It asserts nothing about time, which a busy machine moves: the ratio of a size's median to its smaller neighbour's is the reading, and a size is only read against its neighbours in the same sitting.
pub fn decoder_doubling_slope() {
    const SIZES: [u64; 3] = [20_000, 40_000, 80_000];
    const DECODERS: [&str; 3] = ["json", "html", "tokens"];

    let (module, foreigns) = compile_raw_program(DECODER_DOUBLING);
    let compiled = to_compiled(&module, foreigns).expect("binaryen path compiles");

    let mut runs = DECODERS
        .iter()
        .flat_map(|mode| SIZES.map(|size| (*mode, size)))
        .map(|(mode, size)| {
            let compiled = &compiled;
            move || {
                let (system, io) = MockHost::builder()
                    .args(["program".to_string(), mode.to_string(), size.to_string()])
                    .build();
                phase(TALLIES, || {
                    compiled
                        .run(system, ForeignBindings::empty())
                        .expect("the decoder executes")
                })
                .read(&format!("{mode} at {size}"), Some(size));

                let printed = String::from_utf8_lossy(&io.output()).trim().to_string();
                assert_ne!(printed, "0", "{mode} refused its input at {size}");
            }
        })
        .collect::<Vec<_>>();
    let mut turned = runs
        .iter_mut()
        .map(|run| run as &mut dyn FnMut())
        .collect::<Vec<_>>();
    turn(5, &mut turned);
}
