//! Runtime measurements behind `curios-native/README.md`'s decision "The heap is sized ahead of its churn": the collection decomposition of `chain` and `spines` under differently sized heaps, and the pinned absence of allocation in `churn`'s threaded-record loop. Both count the engine's collections through `curios_native::test_support::collected`, which hears the engine's own announcements.
//!
//! What the thirty-two-mebibyte initial heap buys: under death-birth churn an unsized heap parks within a doubling of the live set and the collector recopies it continually; sized, those collections are gone, the allocation-free `churn` does not move — the empirical pin on its exit from the churn class — and `trees` gains little, because an all-live tree must outgrow any constant.

use {
    super::test_support::*,
    crate::test_support::*,
    curios_native::{Compiled, test_support::*},
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
    match n
    | 0 => acc
    | m + 1 => build(m, 75 * x % 65537, Chain/cons(x, acc))
    end;

let step(rest: Chain, acc: Chain) -> Chain =
    match rest
    | nil() => acc
    | cons(v, tail) => step(tail, Chain/cons((75 * v + 13) % 65537, acc))
    end;

let rounds(k: Nat, c: Chain) -> Chain =
    match k
    | 0 => c
    | m + 1 => rounds(m, step(c, Chain/nil()))
    end;

let total(c: Chain, acc: Nat) -> Nat =
    match c
    | nil() => acc
    | cons(v, tail) => total(tail, (acc + v) % 1000003)
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(s) =>
            match Nat/of_str(Str/trim(s))
            | some(k) =>
                let ballast = total(build(ballast_cells, (k + 2) % 65537, Chain/nil()), 0);
                let chain = rounds(k, build(cells, (k + 1) % 65537, Chain/nil()));
                /std/print(Str/concat(Nat/to_str((ballast + total(chain, 0)) % 1000003), "\n"))
            | none() => /std/print("bad input\n")
            end
        | failure(_) => /std/print("invalid utf-8\n")
        end
    | none() => /std/print("no input\n")
    end;
entry()
"#;

/// Read each of `runs`, a label, its program and the size it runs at, as a decomposition reads one: what the collector did over a run of it, once ([`read_collected`]), and its time, the runs turned round-robin `rounds` times ([`turn_runs`]).
fn decomposed(rounds: usize, runs: &[(&str, &Compiled, u64)]) {
    for &(label, compiled, n) in runs {
        read_collected(label, compiled, n);
    }
    turn_runs(rounds, runs);
}

/// # What this measures
///
/// `cargo xbench measure chain_collection_decomposition` takes it. Three arrangements run the identical churn phase — `ROUNDS` rounds over the harness's 10 000-cell chain — inside heaps the dead ballast pre-grows to different sizes, `stock`, `ballast 500k` (about 32 MiB) and `ballast 4M` (about 256 MiB), each read at K = `ROUNDS` and at K = 0 ([`decomposed`]): `<arrangement> at <K>` the time of a run, the runs turned round-robin, the churn cost the first's median less the second's, which cancels startup and the ballast phase itself; and `/collections` and `/grown` what the collector did, counted from the engine's own records through `curios_native::test_support::collected` in a run of its own, so the raised log level never touches a timed figure.
///
/// # What it last printed
///
/// Taken on top of `9c543476a` on an x86-64 Linux dev machine at a load average under two, wasmtime 47.0.3 with the thirty-two-mebibyte default (see `curios-native/README.md`), the command above:
///
/// ```text
/// == chain collection decomposition (K = 400)
///   stock: churn 0.083 ms/round, 0.018 collections/round, no heap growth recorded
///   ballast 500k (~32 MiB heap): churn 0.068 ms/round, 0.020 collections/round, no heap growth recorded
///   ballast 4M (~256 MiB heap): churn 0.174 ms/round, 0.003 collections/round, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// # The reading
///
/// The engine grows the heap only when a single post-collection allocation cannot fit, so a heap left unsized parks barely above a tiny live set, a collection fires more than once a round, and every cell is copied more often than it is born. Under the default, stock *is* the sized arrangement — no growth is ever recorded because the initial size absorbs the whole run — and it matches the 32 MiB ballast arm in collections, which is the ballast-to-initial-size equivalence this probe's method assumes. The 500k arm reads under stock in time because its ballast phase pre-touches the pages stock first meets cold, and over-sizing (the 256 MiB arm) gives the win back to cold pages and TLB misses, so the lever is a sizing *policy*, not a maximal pre-grow. The residual warm-heap floor is the compiler-side birth path. Collection counts are deterministic and transport across machines; the time shares are this machine's.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
fn chain_collection_decomposition() {
    let stock = compiled_of(CHAIN);

    // The documented cross-language anchor, so a mistranslation fails before any figure is read.
    let (_, output) = run_at(&stock, 8);
    assert_eq!(output, b"819185\n", "chain(8) anchor");

    let ballasted =
        |cells: u64| compiled_of(&CHAIN_BALLAST.replace("BALLAST_CELLS", &cells.to_string()));
    let (small, large) = (ballasted(500_000), ballasted(4_000_000));
    decomposed(
        3,
        &[
            ("stock", &stock, 0),
            ("stock", &stock, ROUNDS),
            ("ballast 500k", &small, 0),
            ("ballast 500k", &small, ROUNDS),
            ("ballast 4M", &large, 0),
            ("ballast 4M", &large, ROUNDS),
        ],
    );
}

/// The `churn` workload's threaded record travels as fields: over a million spread-update steps the emitted program allocates so little that the collector never runs, because continuation scalar replacement and the known-function field split erase the reconstruction the source spells. The workload's Curios column therefore prices dispatch and checked arithmetic rather than allocation, and a record update worth measuring is a record at rest — what `spines` and the census reach. Collection counts are deterministic, which is what makes the absence assertable rather than merely measurable; the anchor is cross-checked against the Rust and Node contestants.
#[test]
fn threaded_record_allocates_nothing() {
    let compiled = compiled_of(CHURN);

    let ((_, output), collected) = collected(|| run_at(&compiled, 1_000_000));

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

    let (_, collected) = collected(|| run_at(&compiled, ROUNDS));

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
    match n
    | 0 => acc
    | m + 1 => build(m, 75 * x % 65537, Chain/cons(x, acc))
    end;

let drain(c: Chain, acc: Nat) -> Nat =
    match c
    | nil() => acc
    | cons(v, tail) => drain(tail, (acc + v) % 1000003)
    end;

let walk(n: Nat, i: Nat, x: Nat, m: Map(Bytes, Nat)) -> Map(Bytes, Nat) =
    match n
    | 0 => m
    | k + 1 =>
        let y = 75 * x % 65537;
        walk(k, i + 1, y, Map/insert(m, Bytes/of_nat(y), i % 1000003))
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(s) =>
            match Nat/of_str(Str/trim(s))
            | some(n) =>
                let ballast = drain(build(ballast_cells, (n + 2) % 65537, Chain/nil()), 0);
                let m = walk(n, 0, (n + 1) % 65537, Map/empty());
                /std/print(Str/concat(Nat/to_str((ballast + Map/fold(m, 0, (_, v, acc) => (acc + v) % 1000003)) % 1000003), "\n"))
            | none() => /std/print("bad input\n")
            end
        | failure(_) => /std/print("invalid utf-8\n")
        end
    | none() => /std/print("no input\n")
    end;
entry()
"#;

/// The `spines` half of the heap-sizing evidence, by the method `chain_collection_decomposition` documents, `cargo xbench measure spines_collection_decomposition` taking it: `stock` read at none, 12 500 and 25 000 inserts, and the two ballast arms at none and 25 000, an insert's cost the difference of two sizes' medians over the inserts between them. The one difference is the subject: `spines`' live set grows toward the map's plateau instead of holding still, so an unsized heap's collections copy a growing structure and the per-insert cost is superlinear until the plateau.
///
/// # What it last printed
///
/// Taken on top of `9c543476a` on an x86-64 Linux dev machine at a load average under two, release, under the thirty-two-mebibyte default:
///
/// ```text
/// == spines collection decomposition
///   stock, N=12500: churn 0.59 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   stock, N=25000: churn 0.63 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   ballast 500k, N=25000: churn 0.55 us/insert, 0.0 collections per 1000 inserts, no heap growth recorded
///   ballast 4M, N=25000: churn 0.51 us/insert, 0.0 collections per 1000 inserts, -> grew GC heap by 0x8000000 bytes: new size is 0x10000000 bytes
/// ```
///
/// **The collector's share of the remaining insert is nil** — zero collections per thousand inserts at stock, at both Ns. The remaining insert cost (`spines_insert_slope`) is mutator work, so a rebuild-and-collection lever — a generational nursery among them — has nothing to take on this workload; what is left to rank is the representation tax against per-insert key construction, which `shapes.rs`'s instruments measure.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
fn spines_collection_decomposition() {
    let stock = compiled_of(SPINES);

    // The documented cross-language anchor, so a mistranslation fails before any figure is read.
    let (_, output) = run_at(&stock, 8);
    assert_eq!(output, b"28\n", "spines(8) anchor");

    let ballasted =
        |cells: u64| compiled_of(&SPINES_BALLAST.replace("BALLAST_CELLS", &cells.to_string()));
    let (small, large) = (ballasted(500_000), ballasted(4_000_000));
    decomposed(
        3,
        &[
            ("stock", &stock, 0),
            ("stock", &stock, 12_500),
            ("stock", &stock, 25_000),
            ("ballast 500k", &small, 0),
            ("ballast 500k", &small, 25_000),
            ("ballast 4M", &large, 0),
            ("ballast 4M", &large, 25_000),
        ],
    );
}

/// `spines` over a map of byte keys, `KEY` its key of `y`: the stock program's walk with the key widened, read the same way.
const SPINES_KEYED: &str = r#"use /std/{Str, Nat, Map, Io, Bytes};

let walk(n: Nat, i: Nat, x: Nat, m: Map(Bytes, Nat)) -> Map(Bytes, Nat) =
    match n
    | 0 => m
    | k + 1 =>
        let y = 75 * x % 65537;
        walk(k, i + 1, y, Map/insert(m, KEY, i % 1000003))
    end;

let entry() -> /std/Io({}) =
    let arguments = /std/Proc/args()!;
    let input = /std/List/try_get(arguments, 1);
    match input
    | some(bytes) =>
        match Str/of_bytes(bytes)
        | success(s) =>
            match Nat/of_str(Str/trim(s))
            | some(n) =>
                let m = walk(n, 0, (n + 1) % 65537, Map/empty());
                /std/print(
                    Str/concat(
                        Nat/to_str(Map/fold(m, 0, (_, v, acc) => (acc + v) % 1000003)),
                        "\n"))
            | none() => /std/print("bad input\n")
            end
        | failure(_) => /std/print("invalid utf-8\n")
        end
    | none() => /std/print("no input\n")
    end;

entry()
"#;

/// The `spines` half read over its keys' width, the floor's other side: a key past an immediate makes every node of the map a heap object, so the live set the collector recopies grows with the map, and a heap that holds a map of immediates at its plateau holds a map of text past its knee.
///
/// # What this measures
///
/// `cargo xbench measure spines_key_width_measurements` takes it. Four programs, `spines` as it is (`nat`) and over `Bytes` keys of three, five and ten bytes (`bytes3`, `Bytes/of_nat(y * 251)`; `bytes5`, `Bytes/of_nat(y * 70001)`; `text10`, `Str/to_bytes(Nat/to_str(y * 70001))`), each read at 4 000, 12 000, 25 000 and 75 000 inserts as [`decomposed`] reads a run: the slope between two sizes, at 4 000 and 12 000 inserts and at 25 000 and 75 000, is the difference of their medians over the inserts between them, and the collections each size makes are counted in a run of their own.
///
/// # What it last printed
///
/// Taken on top of `9c543476a` on an x86-64 Linux dev machine at a load average under two, release, under the thirty-two-mebibyte default, nanoseconds an insert:
///
/// ```text
/// == spines key width
///   nat, 4000-12000: 623 ns/insert [583-630]
///   nat, 25000-75000: 669 ns/insert [652-674]
///   nat, collections at 4k, 12k, 25k, 75k: 0, 0, 0, 2
///   bytes3, 4000-12000: 609 ns/insert [573-621]
///   bytes3, 25000-75000: 625 ns/insert [620-633]
///   bytes3, collections at 4k, 12k, 25k, 75k: 0, 0, 0, 2
///   bytes5, 4000-12000: 1549 ns/insert [1544-1607]
///   bytes5, 25000-75000: 1512 ns/insert [1509-1523]
///   bytes5, collections at 4k, 12k, 25k, 75k: 0, 0, 2, 9
///   text10, 4000-12000: 3055 ns/insert [2947-3118]
///   text10, 25000-75000: 2801 ns/insert [2776-2814]
///   text10, collections at 4k, 12k, 25k, 75k: 0, 2, 4, 19
/// ```
///
/// **No width sits at the knee by seventy-five thousand inserts.** An insert between 25 000 and 75 000 inserts costs within a tenth of what it cost between 4 000 and 12 000 at every width — up to 7% more over immediates and three-byte keys, less over five and ten — and a map of ten-byte keys, the widest, is collected 19 times in its 75 000 inserts. Collection counts are deterministic and transport across machines; the slopes are this machine's.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
fn spines_key_width_measurements() {
    let programs = [
        ("nat", SPINES.to_string()),
        (
            "bytes3",
            SPINES_KEYED.replace("KEY", "Bytes/of_nat(y * 251)"),
        ),
        (
            "bytes5",
            SPINES_KEYED.replace("KEY", "Bytes/of_nat(y * 70001)"),
        ),
        (
            "text10",
            SPINES_KEYED.replace("KEY", "Str/to_bytes(Nat/to_str(y * 70001))"),
        ),
    ];

    let compiled = programs.map(|(label, source)| {
        let compiled = compiled_of(&source);
        let (_, anchor) = run_at(&compiled, 8);
        assert_eq!(anchor, b"28\n", "{label}(8) anchor");

        (label, compiled)
    });
    let runs = compiled
        .iter()
        .flat_map(|(label, compiled)| {
            [4_000, 12_000, 25_000, 75_000].map(|inserts| (*label, compiled, inserts))
        })
        .collect::<Vec<_>>();
    decomposed(5, &runs);
}
