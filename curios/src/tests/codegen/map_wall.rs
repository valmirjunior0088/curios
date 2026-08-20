//! The map-wall campaign's end-to-end instrument: the `spines` insert slope, retaken as each step of the campaign landed — `documentation/design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md` is the decision record, and the retired specification in git history holds the schedule and the decomposition's throwaway-probe figures. This module is the figure that persists, in the `stored_prelude_measurements` pattern — the command, the date, and what it last printed, kept beside the code so a number cannot drift from the thing that would check it.

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
fn run(cwasm: &[u8], n: u64) -> (f64, Vec<u8>) {
    let (host, io) = MockHost::builder().stdin_lines([n.to_string()]).build();
    let start = Instant::now();
    let code = run_bytes(cwasm, host, ForeignBindings::empty()).expect("the workload runs");
    let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    assert_eq!(code, 0, "the workload exits cleanly");

    (elapsed, io.output())
}

/// Best of five, per the decomposition's method: a timing floor is what the slope subtracts.
fn timed(cwasm: &[u8], n: u64) -> f64 {
    (0..5)
        .map(|_| run(cwasm, n).0)
        .fold(f64::INFINITY, f64::min)
}

/// The campaign figure: nanoseconds per `/std/Map` insert, as the slope of `spines` between two Ns so deserialization, instantiation, and the workload's fixed phases cancel.
///
/// # Why it is in-tree
///
/// The spec's schedule gives every step a predicted band against the 2026-08-18 decomposition, and each landing is judged by what this slope does. A figure whose probe was thrown away decays into a claim about history — the `stored_prelude_measurements` lesson — so the probe that retakes the campaign's headline number is kept here.
///
/// # How to read it
///
/// Numbers are only comparable to the recorded ones in `--release` — a debug build measures a different program:
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture map_wall_spines_slope
/// ```
///
/// The slope isolates the marginal insert between N=25 000 and N=75 000 — deeper-than-average descents plus the replace churn of returning keys — so it reads *steeper* than the harness's total-over-N average (run 07's 2.8 µs/insert) and is not comparable to that table. It is one fixed method compared against itself across steps, not a decomposition. The anchor `spines(8) = 28` fails the probe before any figure is read if the workload mistranslates.
///
/// # Baseline, 2026-08-19
///
/// Taken at the campaign branch's start (stock emission, Binaryen optimize 2 / shrink 1), x86-64 dev box, release: N=25000: 47.4 ms, N=75000: 463.4 ms, **slope 8320 ns/insert**.
///
/// The spec's free experiment, run the same day: at Binaryen optimize 3 / shrink 0 the optimized `spines` module still carries all 15 `call $bytes/read` sites (counted over `--print=wasm-optm`), so no level setting produces the leaf split and the invariant is written in the emitter, not delegated to a third-party heuristic. No partial-inlining knobs are bound in `curios-binaryen/src/sys.rs` to try beyond the levels.
///
/// # Step 1, 2026-08-19
///
/// Landed: the in-helper cache probe in `$bytes/read`/`$list/read` and the call-site leaf split (`emit_seq_get`). Figures, same box: the landed variant read 8849/8458/8275 ns/insert over one quiet sequence; an experimental wider split that also answered cached nodes inline at the call site read 7986/7897/8111 in another. Within a sequence the spread is ±1.3%; across sequences it is ±6%, and runs taken right after a compile read up to 5% hot — so none of these deltas, the baseline included, is resolvable.
///
/// # Step 2, packed half, 2026-08-19
///
/// `fuse_append_chains` + `BinChunk` landed; the slope read 8166 ns/insert — inside the band, as the spec predicts, since the key builder is a recursive append the fusion correctly declines (charged to step 4). The shape evidence is `tainted_packed_literals_fuse_to_flat_chunks` in `structural.rs`; the beneficiary class (encoders) is not timed by this probe.
///
/// # Step 4, 2026-08-19
///
/// Small-canonical `Bytes` on the i31 landed — length in the top 2 payload bits, bytes LSB-first below; every byte-grain producer answers through `$bytes/norm`, every rope-shaped consumer enters through `$bytes/box`, and `BinGet`/`BinLen`/`BinEql`/`BinAppend`/`BinChunk` carry inline immediate arms. `Bits` stays all-rope, its layout reserved. Quiet run of three after the landing: **1379/1348/1353 ns/insert** — the slope fell from the 8320 baseline by 6.1×, and the N=75000 total from 463 ms to 96 ms. The one regression the landing hit is pinned: hoisted small constants stayed leaves while runtime values rode the i31, and the canonical equality answered false on the mixed pair — `tainted_packed_literals_fuse_to_flat_chunks` caught it and stands guard, with `packed_unary_payload_declines_the_immediate_encoding` pinning the `FieldShape` admission invariant beside it.
///
/// # Step 5, 2026-08-19 — implemented, measured, declined
///
/// The qp reshape was built in full and its list-half integration proven live (four `ListFlat`, five `ListSettle` in a map program's lowering), then measured: loop-built windows 5918 ns/insert, view windows with settled flat children **1802** — against **1353** for the crit-bit trie it would replace. Step 4 had rewritten the calculus the reshape was priced under: an immediate key's bit test costs a few register ops, so seventeen cheap levels beat four or five levels each paying an O(width) child-copy per rebuild on an all-insert workload. The crit-bit shape stands, the `Key` field is `to_bytes` now, and the census's store-safety fixpoint — found by the experiment, kept on its fixtures — stays. The campaign's closing figure is step 4's: **~1353 ns/insert, 6.1× off the 8320 baseline**.
///
/// What the figure decided: the read protocol's *call* component is not the spines wall. The decomposition's ~60% read share must be carried by what the calls sat beside — the serial dependent fork loads and the walk structure — which halving the call count does not touch. Step 1 stays landed for the invariant's location and for the leaf-rooted walks it does serve (literals, `List/map` results, step 2's flat builds); the cached-node call-site arm was dropped as unresolvable benefit against ~15 instructions per site, to be revisited only if a cached-node-dominated read path is ever hot — none is expected, since step 2 makes fork children leaves. The campaign's weight shifts where the spec already put its centerpiece: the register key (step 4) deletes the loads themselves, and the width reshape (step 5) deletes levels of them.
/// # The single walk, 2026-08-20
///
/// A later campaign's first step, and the first change to `/std/Map` itself since the shape was settled: `insert1`/`remove1` descend once and decide on the way back up, replacing the `lookup` descent plus the `insert_node`/`remove_node` descent. See `documentation/roadmap/map-distance-spec.md`.
///
/// **Measured like-for-like on one box in one sitting, rather than against the 1353 above.** That figure was taken at a different sitting, and this machine was reading bimodally on the day — two clusters about 10% apart, in *both* arms — so comparing across sittings would have priced the machine as much as the change. Seven readings each, same binary shape, alternating nothing else:
///
/// | Variant | Readings (ns/insert) | Median | Min |
/// | --- | --- | --- | --- |
/// | two walks (`HEAD` before this step) | 1267, 1281, 1287, 1309, 1318, 1360, 1421 | 1309 | 1267 |
/// | one walk | 924, 941, 942, 951, 1047, 1058, 1133 | 951 | 924 |
///
/// **−27% on the medians and −27% on the minima** — the same answer from both ends of a noisy distribution, which is what makes it reportable. The specification predicted −30%; a standalone reimplementation of the same shape, measured separately on the same box, read −31%. Take −27% as the conservative figure and the agreement across three methods as the evidence.
///
/// The baseline median of 1309 sits within 3.3% of the 1353 recorded above, so this box is comparable to the one that took the campaign's closing figure; the bimodality is the day's, not a drift in the workload.
///
/// What the figure decided: the walk was the win the specification claimed, and the remaining ~950 ns/insert is what the next decomposition has to explain. Neither of that specification's compiler-side steps — a two-way branch, exact tuple reads — is priced against a decomposition yet, and together they were predicted to reach only about 16% of the insert.
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
