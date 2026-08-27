//! Runtime measurements for the death-birth churn campaign, retired into `documentation/design/toolchain/the-heap-is-sized-ahead-of-its-churn.md`: the collection decomposition of `chain` behind lever A, and the pinned absence of allocation in `churn`'s threaded-record loop. Both hear the engine's own per-collection announcements through `curios-profile`'s log bridge, which is why this module lives behind the `profile` feature.

use {
    crate::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_profile::capture_host_records,
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
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

rec build(n: Nat, x: Nat, acc: Chain) -> Chain =
    match n: (_) => Chain
    | 0 => acc
    | m + 1; ih => build(m, 75 * x % 65537, Chain/cons(x, acc))
    end;

rec step(rest: Chain, acc: Chain) -> Chain =
    match rest: (_) => Chain
    | nil() => acc
    | cons(v, tail) => step(tail, Chain/cons((75 * v + 13) % 65537, acc))
    end;

rec rounds(k: Nat, c: Chain) -> Chain =
    match k: (_) => Chain
    | 0 => c
    | m + 1; ih => rounds(m, step(c, Chain/nil()))
    end;

rec total(c: Chain, acc: Nat) -> Nat =
    match c: (_) => Nat
    | nil() => acc
    | cons(v, tail) => total(tail, (acc + v) % 1000003)
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(k) =>
            let ballast = total(build(ballast_cells, (k + 2) % 65537, Chain/nil()), 0);
            let chain = rounds(k, build(cells, (k + 1) % 65537, Chain/nil()));
            /std/print(Str/concat(Nat/to_str((ballast + total(chain, 0)) % 1000003), "\n"))
        | none() => /std/print("bad input\n")
        end
    | none() => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

/// Where the engine announces each collection: one `Begin GC` record per collection, and each heap growth reports its new size, both at trace level on this target.
const GC_TARGET: &str = "wasmtime::runtime::store::gc";

/// The churn phase is measured as T(K) − T(0) at this K, which subtracts startup, the initial build, and — in the ballast arms — the whole ballast phase.
const ROUNDS: u64 = 400;

fn cwasm_of(source: &str) -> Vec<u8> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())
        .expect("the workload parses");
    let (module, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("the workload compiles");

    to_cwasm(&module).expect("the workload precompiles")
}

/// One run: milliseconds around `run_bytes` — deserialize, instantiate, and the whole call — and what the program printed.
fn run(cwasm: &[u8], k: u64) -> (f64, Vec<u8>) {
    let (host, io) = MockHost::builder().stdin_lines([k.to_string()]).build();
    let start = Instant::now();
    let code = run_bytes(cwasm, host, ForeignBindings::empty()).expect("the workload runs");
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    assert_eq!(code, 0, "the workload exits cleanly");

    (elapsed, io.output())
}

/// Best of three, since a timing floor is what the decomposition subtracts.
fn timed(cwasm: &[u8], k: u64) -> f64 {
    (0..3)
        .map(|_| run(cwasm, k).0)
        .fold(f64::INFINITY, f64::min)
}

/// Collections and the heap's final size during one run at `k`, from the engine's own records. The counted run's duration is discarded: the raised log level prices every suppressed record, so timing and counting are separate runs.
fn collections(cwasm: &[u8], k: u64) -> (usize, Option<String>) {
    let (_, records) = capture_host_records(GC_TARGET, || run(cwasm, k));
    let begins = records
        .iter()
        .filter(|message| message.contains("Begin GC"))
        .count();
    let heap = records
        .iter()
        .rfind(|message| message.contains("new size is"))
        .map(|message| message.trim().to_string());

    (begins, heap)
}

/// # What this measures, and how to retake it
///
/// ```sh
/// cargo test --package curios --release --all-features -- codegen::churn::chain_collection_decomposition --ignored --nocapture
/// ```
///
/// `--release` is load-bearing: the collector under measurement is host-side wasmtime code, and a debug build inflates exactly the share this probe exists to isolate. `--all-features` supplies the `profile` feature the module is gated on.
///
/// Three arrangements run the identical churn phase — `ROUNDS` rounds over the harness's 10 000-cell chain — inside heaps the dead ballast pre-grows to different sizes, and the churn cost is the K = `ROUNDS` time minus the K = 0 time, which cancels startup and the ballast phase itself. Collections are counted from the engine's own `Begin GC` records through `curios-profile`'s log bridge, in separate runs so the raised log level never touches a timed figure.
///
/// # What it last printed
///
/// Taken **2026-08-17**, x86-64 Linux dev machine, the command above:
///
/// ```text
/// == chain collection decomposition (K = 400)
///   stock: churn 0.472 ms/round, 1.580 collections/round, -> grew GC heap by 0x80000 bytes: new size is 0x100000 bytes
///   ballast 250k (~16 MiB heap): churn 0.171 ms/round, 0.040 collections/round, -> grew GC heap by 0x800000 bytes: new size is 0x1000000 bytes
///   ballast 4M (~256 MiB heap): churn 0.324 ms/round, 0.003 collections/round, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// # The reading
///
/// The engine grows the heap only when a single post-collection allocation cannot fit, so under a workload whose live set is tiny the heap parks barely above that live set — the stock arrangement's ~320 KB chain in a 1 MiB heap leaves the semi-space half ~190 KB of allocation room, a collection fires about 1.6 times per round, and every cell is copied more often than it is born. Right-sizing the heap (the 16 MiB arm) removes almost every collection and with it roughly two thirds of the churn cost, at a few MB of RSS; over-sizing it (the 256 MiB arm) gives half that win back to cold pages and TLB misses, so the lever is a sizing *policy*, not a maximal pre-grow. The residual warm-heap floor is the compiler-side birth path. Collection counts are deterministic and transport across machines; the time shares are this machine's.
///
/// # Retaken under the sizing decision
///
/// Same day, same box, wasmtime 47.0.3 with the sixteen-mebibyte default this measurement chose (see `the-heap-is-sized-ahead-of-its-churn.md`):
///
/// ```text
/// == chain collection decomposition (K = 400)
///   stock: churn 0.176 ms/round, 0.030 collections/round, no heap growth recorded
///   ballast 250k (~16 MiB heap): churn 0.143 ms/round, 0.040 collections/round, no heap growth recorded
///   ballast 4M (~256 MiB heap): churn 0.345 ms/round, 0.003 collections/round, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// Stock now *is* the sized arrangement — no growth is ever recorded because the initial size absorbs the whole run — and it reproduces the old 16 MiB ballast arm's figure, which is the ballast-to-initial-size equivalence this probe's method assumed, verified. The 250k arm reads slightly under stock because its ballast phase pre-touches the pages stock first meets cold, and the 256 MiB arm still carries the cold-sweep tax. The 46-era figures above stay as the record of what admitted the lever.
#[test]
#[ignore = "measurement: times the churn workload rather than asserting"]
fn chain_collection_decomposition() {
    let stock = cwasm_of(CHAIN);

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
        let cwasm = match ballast {
            None => stock.clone(),
            Some(cells) => cwasm_of(&CHAIN_BALLAST.replace("BALLAST_CELLS", &cells.to_string())),
        };

        let churn_ms = timed(&cwasm, ROUNDS) - timed(&cwasm, 0);
        let (gc_churn, heap) = collections(&cwasm, ROUNDS);
        let (gc_base, _) = collections(&cwasm, 0);

        println!(
            "  {label}: churn {:.3} ms/round, {:.3} collections/round, {}",
            churn_ms / ROUNDS as f64,
            (gc_churn as f64 - gc_base as f64) / ROUNDS as f64,
            heap.as_deref().unwrap_or("no heap growth recorded"),
        );
    }
}

/// The `churn` workload's threaded record travels as fields: over a million spread-update steps the emitted program allocates so little that the collector never runs, because continuation scalar replacement and the known-function field split erase the reconstruction the source spells. The workload's Curios column therefore prices dispatch and checked arithmetic rather than allocation, and the campaign's record-update question narrows to records at rest — which is what the specification's `spines` workload and the census exist to reach. Collection counts are deterministic, which is what makes the absence assertable rather than merely measurable; the anchor is cross-checked against the Rust and Node contestants.
#[test]
fn threaded_record_allocates_nothing() {
    let cwasm = cwasm_of(CHURN);

    let ((_, output), records) = capture_host_records(GC_TARGET, || run(&cwasm, 1_000_000));

    assert_eq!(output, b"863718\n", "churn(1000000) anchor");

    let collections = records
        .iter()
        .filter(|message| message.contains("Begin GC"))
        .count();
    assert_eq!(
        collections, 0,
        "the spread-update loop reached the collector"
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

rec build(n: Nat, x: Nat, acc: Chain) -> Chain =
    match n: (_) => Chain
    | 0 => acc
    | m + 1; ih => build(m, 75 * x % 65537, Chain/cons(x, acc))
    end;

rec drain(c: Chain, acc: Nat) -> Nat =
    match c: (_) => Nat
    | nil() => acc
    | cons(v, tail) => drain(tail, (acc + v) % 1000003)
    end;

rec walk(n: Nat, i: Nat, x: Nat, m: Map(Nat)) -> Map(Nat) =
    match n: (_) => Map(Nat)
    | 0 => m
    | k + 1; ih =>
        let y = 75 * x % 65537;
        walk(k, i + 1, y, Map/insert(m, Bytes/of_nat(y), i % 1000003))
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Nat/of_str(Str/trim(s)): (_) => Io({})
        | some(n) =>
            let ballast = drain(build(ballast_cells, (n + 2) % 65537, Chain/nil()), 0);
            let m = walk(n, 0, (n + 1) % 65537, Map/empty());
            /std/print(Str/concat(Nat/to_str((ballast + Map/fold(m, 0, (_, v, acc) => (acc + v) % 1000003)) % 1000003), "\n"))
        | none() => /std/print("bad input\n")
        end
    | none() => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

/// The `spines` half of lever A's class evidence, by the method `chain_collection_decomposition` documents — `--release`, `--all-features`, timed and counted runs separate. The one difference is the subject: `spines`' live set grows toward the map's plateau instead of holding still, so the stock arrangement's collections copy a growing structure and the per-insert cost is superlinear until the plateau.
///
/// # What it last printed
///
/// Taken **2026-08-17**, x86-64 Linux dev machine:
///
/// ```text
/// == spines collection decomposition
///   stock, N=12500: churn 6.04 us/insert, 15.9 collections per 1000 inserts, -> grew GC heap by 0x200000 bytes: new size is 0x400000 bytes
///   stock, N=25000: churn 6.82 us/insert, 10.0 collections per 1000 inserts, -> grew GC heap by 0x400000 bytes: new size is 0x800000 bytes
///   ballast 250k, N=25000: churn 2.26 us/insert, 0.2 collections per 1000 inserts, -> grew GC heap by 0x800000 bytes: new size is 0x1000000 bytes
///   ballast 4M, N=25000: churn 2.29 us/insert, 0.0 collections per 1000 inserts, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// # The reading
///
/// Same policy, same verdict as `chain`: the heap parks within a doubling of the live set, and about two thirds of the churn cost is collection work — 6.82 µs per insert falling to 2.26 pre-grown. Two facts are new. The stock per-insert cost is superlinear (6.04 at half the inserts, 6.82 at all of them) because every collection copies the *growing* map, which is what a plateauing live set under churn buys the collector. And the two ballast arms tie, where `chain`'s split by two: a trie walk is cache-scattered whichever heap it runs in, so the cold-page tax that made chain's sizing non-monotonic barely registers here — the non-monotonicity is a hot-loop artifact, not a law of the lever.
///
/// # Retaken under the sizing decision
///
/// Same day, same box, wasmtime 47.0.3 with the sixteen-mebibyte default:
///
/// ```text
/// == spines collection decomposition
///   stock, N=12500: churn 2.65 us/insert, 0.1 collections per 1000 inserts, no heap growth recorded
///   stock, N=25000: churn 2.70 us/insert, 0.1 collections per 1000 inserts, no heap growth recorded
///   ballast 250k, N=25000: churn 2.36 us/insert, 0.1 collections per 1000 inserts, no heap growth recorded
///   ballast 4M, N=25000: churn 2.17 us/insert, 0.0 collections per 1000 inserts, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// The sized stock reproduces the pre-grown arms — collections effectively gone, and the per-insert cost flat across N where it was superlinear — with the small residual over the 46-era ballast figure being the 47 pin's own cost plus first-touch. The figures above stay as the admission record.
///
/// # Retaken 2026-08-20, as the map-distance decomposition's first cut
///
/// Same box, release, after the single-walk map, the two-way branch, exact tuple reads, and the per-arity typed closure tables:
///
/// ```text
/// == spines collection decomposition
///   stock, N=12500: churn 0.62 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   stock, N=25000: churn 0.90 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   ballast 250k, N=25000: churn 0.75 us/insert, 0.1 collections per 1000 inserts, no heap growth recorded
///   ballast 4M, N=25000: churn 1.08 us/insert, 0.0 collections per 1000 inserts, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// What the figure decided: **the collector's share of the remaining insert is nil** — zero collections per thousand inserts at stock, at both Ns, on the post-campaign code. The remaining ~744 ns/insert (`map_wall_spines_slope`) is mutator work, so the decomposition's rebuild-and-collection candidate is measured out for this workload, the generational nursery is demoted with it, and what is left to rank is the representation tax against per-insert key construction — both owned by the typed-fields campaign's instruments.
#[test]
#[ignore = "measurement: times the spines workload rather than asserting"]
fn spines_collection_decomposition() {
    let stock = cwasm_of(SPINES);

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
        let cwasm = match ballast {
            None => stock.clone(),
            Some(cells) => cwasm_of(&SPINES_BALLAST.replace("BALLAST_CELLS", &cells.to_string())),
        };

        let churn_ms = timed(&cwasm, inserts) - timed(&cwasm, 0);
        let (gc_churn, heap) = collections(&cwasm, inserts);
        let (gc_base, _) = collections(&cwasm, 0);

        println!(
            "  {label}: churn {:.2} us/insert, {:.1} collections per 1000 inserts, {}",
            churn_ms * 1000.0 / inserts as f64,
            (gc_churn as f64 - gc_base as f64) * 1000.0 / inserts as f64,
            heap.as_deref().unwrap_or("no heap growth recorded"),
        );
    }
}
