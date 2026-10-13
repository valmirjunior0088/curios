//! Runtime measurements behind `curios-runtime/README.md`'s decision "The heap is sized ahead of its churn": the collection decomposition of `chain` and `spines` under differently sized heaps, and the pinned absence of allocation in `churn`'s threaded-record loop. Both count the engine's collections through `curios_runtime::test_support::collected`, which hears the engine's own announcements.
//!
//! What the sixteen-mebibyte initial heap buys: under death-birth churn an unsized heap parks within a doubling of the live set and the collector recopies it continually; sized, those collections are gone, the allocation-free `churn` does not move — the empirical pin on its exit from the churn class — and `trees` gains little, because an all-live tree must outgrow any constant.

use {
    crate::to_compiled,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{Compiled, ForeignBindings, MockHost, test_support::collected},
    curios_text::{Entrypoint, RootSource},
    std::time::Instant,
};

/// The cross-language workloads, read from the corpus so the probes measure the programs the results files time.
const CHAIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/chain/chain.crs"
));

const CHURN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/churn/churn.crs"
));

/// `chain` behind a dead ballast: a BALLAST_CELLS-cell chain is built and totalled before the unchanged churn phase, so the heap has already grown to hold it when the churn runs — the guest-side spelling of a pre-grown heap, valid because the pinned engine's copying heap grows and never shrinks. The ballast seed derives from K so nothing is closed enough for the optimizer to fold, and its total is folded into the printed number so the ballast cannot be dead.
const CHAIN_BALLAST: &str = r#"use /std/{Str, Nat, Option, Io};

induct Chain: Type
| nil()
| cons(Nat, Chain)
end

let cells: Nat = 10000;
let ballast_cells: Nat = BALLAST_CELLS;

let build(n: Nat, x: Nat, acc: Chain) -> Chain =
    match n: (_) => Chain
    | 0 => acc
    | m + 1; ih => build(m, 75 * x % 65537, Chain/cons(x, acc))
    end;

let step(rest: Chain, acc: Chain) -> Chain =
    match rest: (_) => Chain
    | nil() => acc
    | cons(v, tail) => step(tail, Chain/cons((75 * v + 13) % 65537, acc))
    end;

let rounds(k: Nat, c: Chain) -> Chain =
    match k: (_) => Chain
    | 0 => c
    | m + 1; ih => rounds(m, step(c, Chain/nil()))
    end;

let total(c: Chain, acc: Nat) -> Nat =
    match c: (_) => Nat
    | nil() => acc
    | cons(v, tail) => total(tail, (acc + v) % 1000003)
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | success(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(k) =>
            let ballast = total(build(ballast_cells, (k + 2) % 65537, Chain/nil()), 0);
            let chain = rounds(k, build(cells, (k + 1) % 65537, Chain/nil()));
            /std/print(Str/concat(Nat/to_str((ballast + total(chain, 0)) % 1000003), "\n"))
        | none() => /std/print("bad input\n")
        end
    | failure(_) => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

/// The churn phase is measured as T(K) − T(0) at this K, which subtracts startup, the initial build, and — in the ballast arms — the whole ballast phase.
const ROUNDS: u64 = 400;

fn compiled_of(source: &str) -> Compiled {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())
        .expect("the workload parses");
    let (module, foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("the workload compiles");

    to_compiled(&module, foreigns).expect("the workload compiles")
}

/// One run: milliseconds around it — instantiate, and the whole call — and what the program printed.
fn run(compiled: &Compiled, k: u64) -> (f64, Vec<u8>) {
    let (host, io) = MockHost::builder().stdin_lines([k.to_string()]).build();
    let start = Instant::now();
    let code = compiled
        .run(host, ForeignBindings::empty())
        .expect("the workload runs");
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    assert_eq!(code, 0, "the workload exits cleanly");

    (elapsed, io.output())
}

/// Best of three, since a timing floor is what the decomposition subtracts.
fn timed(compiled: &Compiled, k: u64) -> f64 {
    (0..3)
        .map(|_| run(compiled, k).0)
        .fold(f64::INFINITY, f64::min)
}

/// Collections and the heap's final size during one run at `k`, from the engine's own records. The counted run's duration is discarded: the raised log level prices every suppressed record, so timing and counting are separate runs.
fn collections(compiled: &Compiled, k: u64) -> (usize, Option<String>) {
    let (_, collected) = collected(|| run(compiled, k));

    (collected.collections, collected.growth)
}

/// # What this measures, and how to retake it
///
/// ```sh
/// cargo test --package curios --release -- codegen::churn::chain_collection_decomposition --ignored --nocapture
/// ```
///
/// `--release` is load-bearing: the collector under measurement is host-side wasmtime code, and a debug build inflates exactly the share this probe exists to isolate.
///
/// Three arrangements run the identical churn phase — `ROUNDS` rounds over the harness's 10 000-cell chain — inside heaps the dead ballast pre-grows to different sizes, and the churn cost is the K = `ROUNDS` time minus the K = 0 time, which cancels startup and the ballast phase itself. Collections are counted from the engine's own `Begin GC` records through `curios_runtime::test_support::collected`, in separate runs so the raised log level never touches a timed figure.
///
/// # What it last printed
///
/// Taken on top of `d5046c5cc` on an x86-64 Linux dev machine, wasmtime 47.0.3 with the sixteen-mebibyte default (see `curios-runtime/README.md`), the command above:
///
/// ```text
/// == chain collection decomposition (K = 400)
///   stock: churn 0.072 ms/round, 0.037 collections/round, no heap growth recorded
///   ballast 250k (~16 MiB heap): churn 0.058 ms/round, 0.040 collections/round, no heap growth recorded
///   ballast 4M (~256 MiB heap): churn 0.183 ms/round, 0.003 collections/round, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// # The reading
///
/// The engine grows the heap only when a single post-collection allocation cannot fit, so a heap left unsized parks barely above a tiny live set, a collection fires more than once a round, and every cell is copied more often than it is born. Under the default, stock *is* the sized arrangement — no growth is ever recorded because the initial size absorbs the whole run — and it matches the 16 MiB ballast arm, which is the ballast-to-initial-size equivalence this probe's method assumes. The 250k arm reads slightly under stock because its ballast phase pre-touches the pages stock first meets cold, and over-sizing (the 256 MiB arm) gives the win back to cold pages and TLB misses, so the lever is a sizing *policy*, not a maximal pre-grow. The residual warm-heap floor is the compiler-side birth path. Collection counts are deterministic and transport across machines; the time shares are this machine's.
#[test]
#[ignore = "measurement, mixed: times the churn workload and counts its collections rather than asserting"]
fn chain_collection_decomposition() {
    let stock = compiled_of(CHAIN);

    // The documented cross-language anchor, so a mistranslation fails before any figure is read.
    let (_, output) = run(&stock, 8);
    assert_eq!(output, b"819185\n", "chain(8) anchor");

    println!("== chain collection decomposition (K = {ROUNDS})");

    let arrangements: [(&str, Option<u64>); 3] = [
        ("stock", None),
        ("ballast 250k (~16 MiB heap)", Some(250_000)),
        ("ballast 4M (~256 MiB heap)", Some(4_000_000)),
    ];

    for (label, ballast) in arrangements {
        let ballasted = ballast
            .map(|cells| compiled_of(&CHAIN_BALLAST.replace("BALLAST_CELLS", &cells.to_string())));
        let compiled = ballasted.as_ref().unwrap_or(&stock);

        let churn_ms = timed(compiled, ROUNDS) - timed(compiled, 0);
        let (gc_churn, heap) = collections(compiled, ROUNDS);
        let (gc_base, _) = collections(compiled, 0);

        println!(
            "  {label}: churn {:.3} ms/round, {:.3} collections/round, {}",
            churn_ms / ROUNDS as f64,
            (gc_churn as f64 - gc_base as f64) / ROUNDS as f64,
            heap.as_deref().unwrap_or("no heap growth recorded"),
        );
    }
}

/// The `churn` workload's threaded record travels as fields: over a million spread-update steps the emitted program allocates so little that the collector never runs, because continuation scalar replacement and the known-function field split erase the reconstruction the source spells. The workload's Curios column therefore prices dispatch and checked arithmetic rather than allocation, and a record update worth measuring is a record at rest — what `spines` and the census reach. Collection counts are deterministic, which is what makes the absence assertable rather than merely measurable; the anchor is cross-checked against the Rust and Node contestants.
#[test]
fn threaded_record_allocates_nothing() {
    let compiled = compiled_of(CHURN);

    let ((_, output), collected) = collected(|| run(&compiled, 1_000_000));

    assert_eq!(output, b"863718\n", "churn(1000000) anchor");
    assert_eq!(
        collected.collections, 0,
        "the spread-update loop reached the collector"
    );
}

/// The count's positive control. `threaded_record_allocates_nothing` asserts an absence, and an absence read off a count that hears nothing holds whatever the program does, as one taken on the calling thread does of a module the engine runs on another. So a workload that reaches the collector is heard to: the chain's churn, whose collections `chain_collection_decomposition` decomposes.
#[test]
fn a_churning_chain_is_heard_to_collect() {
    let compiled = compiled_of(CHAIN);

    let (_, collected) = collected(|| run(&compiled, ROUNDS));

    assert!(
        collected.collections > 0,
        "the chain churned through {ROUNDS} rounds and no collection was heard"
    );
}

const SPINES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/spines/spines.crs"
));

/// `spines` behind the same dead ballast `CHAIN_BALLAST` carries: a BALLAST_CELLS-cell chain is built and drained before the unchanged insert phase, pre-growing the heap the inserts then churn inside. The ballast total is folded into the printed number so nothing is dead, and the seed derives from N so nothing folds.
const SPINES_BALLAST: &str = r#"use /std/{Str, Nat, Bytes, Option, Map, Io};

induct Chain: Type
| nil()
| cons(Nat, Chain)
end

let ballast_cells: Nat = BALLAST_CELLS;

let build(n: Nat, x: Nat, acc: Chain) -> Chain =
    match n: (_) => Chain
    | 0 => acc
    | m + 1; ih => build(m, 75 * x % 65537, Chain/cons(x, acc))
    end;

let drain(c: Chain, acc: Nat) -> Nat =
    match c: (_) => Nat
    | nil() => acc
    | cons(v, tail) => drain(tail, (acc + v) % 1000003)
    end;

let walk(n: Nat, i: Nat, x: Nat, m: Map(Bytes, Nat)) -> Map(Bytes, Nat) =
    match n: (_) => Map(Bytes, Nat)
    | 0 => m
    | k + 1; ih =>
        let y = 75 * x % 65537;
        walk(k, i + 1, y, Map/insert(m, Bytes/of_nat(y), i % 1000003))
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | success(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(n) =>
            let ballast = drain(build(ballast_cells, (n + 2) % 65537, Chain/nil()), 0);
            let m = walk(n, 0, (n + 1) % 65537, Map/empty());
            /std/print(Str/concat(Nat/to_str((ballast + Map/fold(m, 0, (_, v, acc) => (acc + v) % 1000003)) % 1000003), "\n"))
        | none() => /std/print("bad input\n")
        end
    | failure(_) => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

/// The `spines` half of the heap-sizing evidence, by the method `chain_collection_decomposition` documents — `--release`, timed and counted runs separate. The one difference is the subject: `spines`' live set grows toward the map's plateau instead of holding still, so an unsized heap's collections copy a growing structure and the per-insert cost is superlinear until the plateau.
///
/// # What it last printed
///
/// Taken on top of `d5046c5cc` on an x86-64 Linux dev machine, release, under the sixteen-mebibyte default:
///
/// ```text
/// == spines collection decomposition
///   stock, N=12500: churn 0.65 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   stock, N=25000: churn 0.70 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   ballast 250k, N=25000: churn 0.56 us/insert, 0.1 collections per 1000 inserts, no heap growth recorded
///   ballast 4M, N=25000: churn 0.54 us/insert, 0.0 collections per 1000 inserts, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// **The collector's share of the remaining insert is nil** — zero collections per thousand inserts at stock, at both Ns. The remaining insert cost (`map_wall_spines_slope`) is mutator work, so a rebuild-and-collection lever — a generational nursery among them — has nothing to take on this workload; what is left to rank is the representation tax against per-insert key construction, which `shapes.rs`'s instruments measure.
#[test]
#[ignore = "measurement, mixed: times the spines workload and counts its collections rather than asserting"]
fn spines_collection_decomposition() {
    let stock = compiled_of(SPINES);

    // The documented cross-language anchor, so a mistranslation fails before any figure is read.
    let (_, output) = run(&stock, 8);
    assert_eq!(output, b"28\n", "spines(8) anchor");

    println!("== spines collection decomposition");

    let arrangements: [(&str, Option<u64>, u64); 4] = [
        ("stock, N=12500", None, 12_500),
        ("stock, N=25000", None, 25_000),
        ("ballast 250k, N=25000", Some(250_000), 25_000),
        ("ballast 4M, N=25000", Some(4_000_000), 25_000),
    ];

    for (label, ballast, inserts) in arrangements {
        let ballasted = ballast
            .map(|cells| compiled_of(&SPINES_BALLAST.replace("BALLAST_CELLS", &cells.to_string())));
        let compiled = ballasted.as_ref().unwrap_or(&stock);

        let churn_ms = timed(compiled, inserts) - timed(compiled, 0);
        let (gc_churn, heap) = collections(compiled, inserts);
        let (gc_base, _) = collections(compiled, 0);

        println!(
            "  {label}: churn {:.2} us/insert, {:.1} collections per 1000 inserts, {}",
            churn_ms * 1000.0 / inserts as f64,
            (gc_churn as f64 - gc_base as f64) * 1000.0 / inserts as f64,
            heap.as_deref().unwrap_or("no heap growth recorded"),
        );
    }
}
