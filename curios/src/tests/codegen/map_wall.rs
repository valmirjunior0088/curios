//! The `spines` insert slope, the end-to-end instrument behind `documentation/design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md`, kept in the `stored_prelude_measurements` pattern — the command and what it last printed, beside the code so a number cannot drift from the thing that would check it.

use {
    crate::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
    curios_text::{Entrypoint, RootSource},
    std::time::Instant,
};

/// The cross-language workload, read from the corpus so the probe measures the program the results files time.
const SPINES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/spines/spines.crs"
));

pub(super) fn cwasm_of(source: &str) -> Vec<u8> {
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
pub(super) fn run(cwasm: &[u8], n: u64) -> (f64, Vec<u8>) {
    let (host, io) = MockHost::builder().stdin_lines([n.to_string()]).build();
    let start = Instant::now();
    // SAFETY: the caller precompiled `cwasm` in this process.
    let code =
        unsafe { run_bytes(cwasm, host, ForeignBindings::empty()) }.expect("the workload runs");
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    assert_eq!(code, 0, "the workload exits cleanly");

    (elapsed, io.output())
}

/// Best of five: a timing floor is what the slope subtracts.
pub(super) fn timed(cwasm: &[u8], n: u64) -> f64 {
    (0..5)
        .map(|_| run(cwasm, n).0)
        .fold(f64::INFINITY, f64::min)
}

/// Nanoseconds per `/std/Map` insert, as the slope of `spines` between two Ns so deserialization, instantiation, and the workload's fixed phases cancel.
///
/// # Why it is in-tree
///
/// A figure whose probe was thrown away decays into a claim about history — the `stored_prelude_measurements` lesson — so the probe that retakes this one is kept here.
///
/// # How to read it
///
/// Numbers are only comparable to the recorded ones in `--release` — a debug build measures a different program:
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture map_wall_spines_slope
/// ```
///
/// The slope isolates the marginal insert between N=25 000 and N=75 000 — deeper-than-average descents plus the replace churn of returning keys — so it reads *steeper* than the harness's total-over-N average and is not comparable to that table. It is one fixed method compared against itself across steps, not a decomposition. The anchor `spines(8) = 28` fails the probe before any figure is read if the workload mistranslates.
///
/// # What it last printed
///
/// x86-64 dev box, release, seven readings in one sitting: 924, 941, 942, 951, 1047, 1058, 1133 ns/insert, median 951. The machine read bimodally in that sitting, two clusters about 10% apart, so a figure is compared only within one sitting.
///
/// # What the design rests on
///
/// - **The key rides the i31.** Small-canonical `Bytes` — length in the top 2 payload bits, bytes LSB-first below — deletes the dependent loads of the key's bytes, which are the wall.
/// - **The leaf split is the emitter's.** At Binaryen optimize 3 / shrink 0 the optimized `spines` module still carries all 15 `call $bytes/read` sites, so no level produces the call-site leaf split (`emit_seq_get`) and the invariant is written in the emitter. The read protocol's *call* component is not the wall, so a wider split answering cached nodes inline is not worth its ~15 instructions per site.
/// - **The crit-bit trie stands.** An immediate key's bit test costs a few register ops, so seventeen cheap levels beat a qp-trie's four or five, each paying an O(width) child copy per rebuild.
/// - **The map walks once.** `insert1`/`remove1` descend once and decide on the way back up.
#[test]
#[ignore = "measurement: reports timings rather than asserting"]
fn map_wall_spines_slope() {
    let cwasm = cwasm_of(SPINES);

    // The documented cross-language anchor, so a mistranslation fails before any figure is read.
    let (_, output) = run(&cwasm, 8);
    assert_eq!(output, b"28\n", "spines(8) anchor");

    println!("== map wall: spines insert slope");

    let (n1, n2) = (25_000u64, 75_000u64);
    let (t1, t2) = (timed(&cwasm, n1), timed(&cwasm, n2));
    println!(
        "  N={n1}: {t1:.1} ms, N={n2}: {t2:.1} ms, slope {:.0} ns/insert",
        (t2 - t1) * 1_000_000.0 / (n2 - n1) as f64,
    );
}

/// The per-iteration loop of `spines` with the map removed: the same LCG, the same key derivation, folding a checksum instead of inserting. The pair differs only in whether `Bytes/of_nat` runs, so the slope delta is the key construction's own price per insert.
const KEY_OFNAT: &str = r#"
use /std/{Str, Nat, Bytes, Option, Io};

let walk(n: Nat, x: Nat, s: Nat) -> Nat =
    match n: (_) => Nat
    | 0 => s
    | k + 1; ih =>
        let y = 75 * x % 65537;
        walk(k, y, (s + y + Bytes/len(Bytes/of_nat(y))) % 1000003)
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(str) =>
        match Nat/of_str(Str/trim(str)): (_) => Io({})
        | some(n) => /std/print(Str/concat(Nat/to_str(walk(n, (n + 1) % 65537, 0)), "\n"))
        | none() => /std/print("bad input\n")
        end
    | none() => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

const KEY_CONTROL: &str = r#"
use /std/{Str, Nat, Bytes, Option, Io};

let walk(n: Nat, x: Nat, s: Nat) -> Nat =
    match n: (_) => Nat
    | 0 => s
    | k + 1; ih =>
        let y = 75 * x % 65537;
        walk(k, y, (s + y + 2) % 1000003)
    end;

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(str) =>
        match Nat/of_str(Str/trim(str)): (_) => Io({})
        | some(n) => /std/print(Str/concat(Nat/to_str(walk(n, (n + 1) % 65537, 0)), "\n"))
        | none() => /std/print("bad input\n")
        end
    | none() => /std/print("invalid utf-8\n")
    end
| none() => /std/print("no input\n")
end
"#;

/// The key's share of the insert: what `Bytes/of_nat` costs per call at `spines`' key magnitudes, isolated from the map. Sound as a pair of scalar loops because the collector's share of this workload measured nil (`spines_collection_decomposition`) — the live-set confound that would invalidate a whole-process ablation has no collections left to misattribute.
///
/// # How to run it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture map_wall_key_share
/// ```
///
/// The control folds a constant where the probe folds `Bytes/len(Bytes/of_nat(y))`, so the delta is `of_nat` plus one immediate `len` — read it as a slight upper bound on the key class. One call per insert makes the share arithmetic direct against `map_wall_spines_slope`'s figure.
///
/// # What it last printed
///
/// Release, x86-64 Linux, twice for stability:
///
/// ```text
/// outputs at N=1000: ofnat "923684", control "923689"
///   ofnat 18 ns/iter, control 4 ns/iter, key construction 14 ns/insert   (retake: 18 / 3 / 14)
/// ```
///
/// **The key class is negligible — 14 ns against the insert `map_wall_spines_slope` reads, under 2%** — so the workload confound `programs/README.md` flags is real but immaterial at these key magnitudes. Beside it, the collector's share measured nil (`spines_collection_decomposition`), and the optimized `spines` module's nine surviving `br_table`s all sit in string/UTF-8 decoding and `main`, none in `insert1`, `bit`, `crit`, `lookup`, `wedge`, or the fold, so the descent has no table left to replace. What remains of the insert is the uniform-representation tax, which `documentation/design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md` takes up.
#[test]
#[ignore = "measurement: reports timings rather than asserting"]
fn map_wall_key_share() {
    let ofnat = cwasm_of(KEY_OFNAT);
    let control = cwasm_of(KEY_CONTROL);

    // Self-pinned outputs: the two arms legitimately differ (len versus the constant), so each pins its own.
    let (_, ofnat_out) = run(&ofnat, 1000);
    let (_, control_out) = run(&control, 1000);
    println!(
        "outputs at N=1000: ofnat {:?}, control {:?}",
        String::from_utf8_lossy(&ofnat_out).trim(),
        String::from_utf8_lossy(&control_out).trim(),
    );

    let (n1, n2) = (25_000u64, 75_000u64);
    let slope =
        |cwasm: &[u8]| (timed(cwasm, n2) - timed(cwasm, n1)) * 1_000_000.0 / (n2 - n1) as f64;
    let ofnat_ns = slope(&ofnat);
    let control_ns = slope(&control);
    println!(
        "  ofnat {ofnat_ns:.0} ns/iter, control {control_ns:.0} ns/iter, key construction {:.0} ns/insert",
        ofnat_ns - control_ns,
    );
}
