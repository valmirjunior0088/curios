//! Runtime measurements for the death-birth churn campaign (`documentation/roadmap/death_birth_churn_spec.md`): the collection decomposition of `chain` behind lever A, and the pinned absence of allocation in `churn`'s threaded-record loop. Both hear the engine's own per-collection announcements through `curios-profile`'s log bridge, which is why this module lives behind the `profile` feature.

use {
    crate::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_profile::capture_host_records,
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
    curios_text::{Entrypoint, RootSource},
    std::time::Instant,
};

/// The harness's own workloads, included from their source of truth so the probes measure the programs the results files time.
const CHAIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../curios-benchmarks/programs/chain/chain.crs"
));

const CHURN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../curios-benchmarks/programs/churn/churn.crs"
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
fn churn_threaded_record_allocates_nothing() {
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
