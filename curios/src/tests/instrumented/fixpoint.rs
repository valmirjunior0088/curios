//! What the Cont fixpoint costs, pass by pass.
//!
//! Instrumented because the per-pass spans and fired-samples `curios_cont::optimize` carries exist only under the `profile` feature. It reports and does not assert, in the shape of `unfolding.rs`'s measurements, and its subject is the census's `TOML_DRIVER`, so the program a figure is taken over is named.

use {
    crate::tests::TOML_DRIVER,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_profile::{Destination, fold, trace},
    curios_text::{Entrypoint, RootSource},
};

/// The stream, kept in memory so the fold reads it back without a file.
#[derive(Clone, Default)]
struct Rows(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

impl Rows {
    fn rows(&self) -> Vec<u8> {
        self.0.lock().expect("rows lock").clone()
    }
}

impl std::io::Write for Rows {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.0.lock().expect("rows lock").extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// Where the fixpoint's time goes, and which passes keep it running.
///
/// # How to take it
///
/// ```sh
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture fixpoint_pass_measurements --test-threads=1
/// ```
///
/// This prints one profiled compilation of `TOML_DRIVER`: each span's total, call count and extremes, followed by its magnitude samples. A pass's boolean sample total counts rounds in which it changed the graph; its sample count counts invocations. A sweep can rewrite many candidates in one invocation. Its fired count neither counts rewrites nor establishes one-candidate draining or independence: the direct pass tests hold the snapshot and deferral contracts. Samples ending in `::nodes`, `::conts`, `::cap` and `::values` are graph sizes, not fired counts.
///
/// The `profile` feature installs the process-wide counting allocator as well as tracing. The recorded stream therefore contains allocation readings, although this test prints timings and samples alone; `curios_profile::fold` supplies the allocation columns. Profile timings include tracing and allocator overhead and do not price the default-feature compiler.
///
/// # Comparison protocol
///
/// Use isolated source revisions and Cargo target directories, the same lockfile and Rust toolchain, release `opt-level = 3` and build-script `opt-level = 3`. Finish all builds and checks before timing. A temporary native probe depends on the revision's pipeline, text, Wasm, Binaryen, Cranelift, runtime, ABI and archive crates. Its compilation operation reads a source string, parses `Entrypoint`, calls `compile_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none(), |_| {})`, and passes the result through `std::hint::black_box`. It uses no package cache, stage rendering, Binaryen or Cranelift during the compilation measurement.
///
/// The common source cohort is copied from the measurement checkout before preparing the compiler revisions. Sources are `TOML_DRIVER` in `codegen/census.rs`; `JUMPS`, `WORKERS`, `UNCURRY`, `CALLS` and `SCCS` in `codegen/sweeps/test_support.rs`; `LISTING_PROGRAM` in `tui.rs`; and `programs/{parse_digits,monad_io,monad_suspend,walk_mirror_held_scan}.crs`. Copy their literal contents to temporary source files. The source tests keep their small runtime answers, including the escaping uses that keep the call-pattern consumer outside inlining and worker splitting.
///
/// For unprofiled compilation, build the probe with default features. Take one unmeasured warm batch and seven measured batches per source and revision, running the two revisions consecutively for each source and alternating which goes first each batch. A batch is ten fresh probe processes, except TOML and TUI, whose batch is one. Time the batch with `/usr/bin/time -f '%e'`, divide its process wall time by the batch size, and report the median and observed span of the seven readings. Startup, source reading, parsing, prelude restoration and teardown are included; Cargo, prelude construction, serialization and native precompilation are excluded. The clock prints in 10 ms increments: expand each observed span by one tick divided by its batch size before deciding whether spans are disjoint.
///
/// For pass attribution, build both probes with a `profile` feature forwarding `curios-pipeline/profile`, `curios-runtime/profile` and `curios-profile/enabled`. Trace the same TOML compilation with `curios_profile::trace` to `Destination::File`, fold its stream and require every span to close. Take one warmup and seven interleaved readings, alternating the first revision. The table below gives medians and observed spans of each pass's aggregate time; rounds and allocations are read from the same captures. The observer still renders nothing. The command above uses an in-memory stream for inspection; paired figures use the file-stream protocol on both revisions.
///
/// For counted sizes, serialize the emitted module with `curios_wasm::to_bytes` (names retained), validate it and call `curios_binaryen::optimize(raw, false, |_| {})` (names removed). Keep the existing Binaryen configuration. Precompile those optimized bytes with `curios_cranelift::precompile` and archive the harvested `ForeignStore` beside the probe's own machine code. Use one unchanged default-feature runtime probe for both revisions' artifacts, holding the native host and engine constant. The runtime probe deserializes that pair once per batch and repeatedly runs it with a fresh `MockHost`, checking successful exit, empty stderr and stable stdout on every execution.
///
/// Runtime arguments are eight copies of `"argument"` for each small source fixture, `["program", "answer = 42"]` for TOML, and `["program", n]` for the corpus: `n = 10000` for parsing and suspension, `100000` for monadic IO and `1000` for the rope walk. TUI uses a fixed 12×5 terminal, 100 down/up pairs and `q`, the first down escape split into two chunks. Its monotonic-clock script contains 100,000 readings advancing by 1 ms. Require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, one screen clear and no stderr. This fixed-size workload does not assert the resizes the older coordination measurement scripts.
///
/// Batch counts, selected by separate calibration to run for about half a second, are 3,000 executions for each small fixture, 2,500 for TOML, 150 for parsing, 200 for monadic IO, 30 for suspension, 500 for the rope walk and 10 for TUI. Calibration is excluded from the reported samples.
///
/// Runtime uses five groups of five measured batches, one unmeasured warm batch opening each group. Within each source and batch, interleave the revisions and alternate which goes first. Divide GNU time's process wall time by the batch's execution count; this includes deserialization, host construction, instantiation, execution and teardown, amortizing the fixed process cost. Report the median of the five group medians and their span. Expand spans by the clock tick per execution before comparison. Overlapping spans establish no difference; disjoint spans establish the direction, without a percentage threshold.
///
/// # Latest reading
///
/// Mixed: counted Wasm bytes, rounds and allocation figures; timed default-release process wall times and matched profile attribution. Taken at `ce963cf3f` against `eb40b82e9`, AMD Ryzen 7 5800X3D, x86-64 Linux `7.2.9-ogc3.1.fc44.x86_64`, Rust `1.95.0`, Wasmtime `47.0.3`, default engine configuration, frequency scaling and boost enabled. The profile builds are isolated from the default-feature builds.
///
/// Default-feature compilation, process wall time per compilation:
///
/// | Consumer | Baseline ms [span] | Candidate ms [span] | Span comparison |
/// | --- | ---: | ---: | --- |
/// | `calls` | 116 [115–117] | 105 [104–106] | faster |
/// | `jumps` | 123 [121–123] | 123 [122–123] | overlap |
/// | `monad_io` | 280 [279–281] | 277 [276–279] | overlap |
/// | `monad_suspend` | 513 [509–515] | 504 [500–506] | faster |
/// | `parse_digits` | 272 [270–273] | 270 [267–271] | overlap |
/// | `sccs` | 86 [85–86] | 85 [84–86] | overlap |
/// | `toml` | 9530 [9490–9610] | 4010 [3990–4030] | faster |
/// | `tui` | 2760 [2750–2780] | 2670 [2640–2680] | faster |
/// | `uncurry` | 89 [88–89] | 88 [86–88] | overlap |
/// | `walk_mirror_held_scan` | 317 [314–319] | 314 [311–316] | overlap |
/// | `workers` | 86 [84–86] | 86 [84–86] | overlap |
///
/// Runtime, amortized process wall time per execution:
///
/// | Consumer | Baseline ms [span] | Candidate ms [span] | Span comparison |
/// | --- | ---: | ---: | --- |
/// | `calls` | 0.163 [0.160–0.163] | 0.163 [0.160–0.163] | overlap |
/// | `jumps` | 0.160 [0.160–0.163] | 0.160 [0.160–0.163] | overlap |
/// | `monad_io` | 2.40 [2.40–2.45] | 2.40 [2.40–2.45] | overlap |
/// | `monad_suspend` | 17.0 [17.0–17.3] | 17.0 [17.0–17.3] | overlap |
/// | `parse_digits` | 2.60 [2.60–2.60] | 2.60 [2.60–2.60] | overlap |
/// | `sccs` | 0.163 [0.160–0.163] | 0.163 [0.160–0.163] | overlap |
/// | `toml` | 0.208 [0.208–0.208] | 0.208 [0.208–0.208] | overlap |
/// | `tui` | 45 [45–46] | 45 [45–46] | overlap |
/// | `uncurry` | 0.163 [0.160–0.163] | 0.163 [0.160–0.163] | overlap |
/// | `walk_mirror_held_scan` | 0.94 [0.94–0.94] | 0.92 [0.92–0.92] | overlap |
/// | `workers` | 0.160 [0.160–0.160] | 0.160 [0.160–0.160] | overlap |
///
/// | Consumer | Raw bytes: baseline → candidate | Optimized bytes: baseline → candidate |
/// | --- | ---: | ---: |
/// | `calls` | 51,334 → 51,334 | 19,674 → 19,674 |
/// | `jumps` | 46,071 → 45,869 | 16,388 → 16,470 |
/// | `monad_io` | 122,086 → 121,763 | 33,307 → 33,289 |
/// | `monad_suspend` | 186,317 → 185,994 | 50,990 → 50,972 |
/// | `parse_digits` | 118,805 → 118,495 | 33,025 → 33,007 |
/// | `sccs` | 44,514 → 44,514 | 16,978 → 16,978 |
/// | `toml` | 755,931 → 711,929 | 200,748 → 193,464 |
/// | `tui` | 708,215 → 701,711 | 182,524 → 181,264 |
/// | `uncurry` | 43,963 → 43,963 | 16,948 → 16,948 |
/// | `walk_mirror_held_scan` | 139,690 → 139,367 | 35,144 → 35,126 |
/// | `workers` | 45,832 → 45,832 | 17,178 → 17,174 |
///
/// The join consumer's sole size increase is 82 optimized code bytes. Its raw code shrinks by 100 bytes and its name section by 102. Raw and optimized function counts and type sections, and raw `struct.new*` and `array.new*` instruction counts, are unchanged; the changed join layout alters Binaryen's flattening of the entry and `io/surface`, leaving one optimized function body at 617 rather than 535 bytes. All other sections are identical. The existing growth ceilings remain unchanged.
///
/// | Profile span | Baseline ms [span] | Candidate ms [span] |
/// | --- | ---: | ---: |
/// | `compile_entrypoint` | 10200 [10140–10280] | 4230 [4220–4270] |
/// | `cont_optimize` | 9670 [9610–9750] | 3720 [3700–3750] |
///
/// | Candidate pass | ms [span] | Fired / calls | Allocated MiB | Allocation calls |
/// | --- | ---: | ---: | ---: | ---: |
/// | `inline_known_calls` | 491 [489–495] | 5 / 11 | 508.0 | 6,292,859 |
/// | `answer_row_tests` | 428 [427–432] | 0 / 11 | 352.0 | 4,798,544 |
/// | `split_parameters` | 408 [405–413] | 6 / 11 | 203.5 | 2,452,167 |
/// | `split_workers` | 398 [396–400] | 2 / 11 | 351.8 | 4,255,474 |
/// | `eliminate_dead_parameters` | 397 [396–404] | 6 / 11 | 31.4 | 357,213 |
/// | `known_values` | 242 [239–245] | — / 11 | 264.4 | 3,614,207 |
/// | `split_returns` | 203 [202–204] | 6 / 11 | 190.3 | 2,399,815 |
/// | `uncurry_returns` | 156 [155–157] | 3 / 11 | 162.7 | 2,104,178 |
/// | `flatten_indexed_lists` | 155 [154–156] | 1 / 11 | 68.4 | 1,181,005 |
/// | `contify_calls` | 137 [136–139] | 5 / 11 | 148.9 | 1,896,062 |
/// | `specialize_scc_calls` | 135 [134–137] | 0 / 11 | 149.6 | 1,902,926 |
/// | `separate_loop_entries` | 129.5 [129.1–130.2] | 5 / 11 | 148.5 | 1,904,264 |
/// | `inline_single_use_continuations` | 51.2 [50.6–51.7] | 7 / 11 | 15.1 | 294,304 |
/// | `eliminate_dead_calls` | 45.6 [45.2–46.0] | 1 / 11 | 27.2 | 253,845 |
/// | `eliminate_dead_bindings` | 38.0 [37.6–38.3] | 8 / 11 | 12.5 | 285,515 |
/// | `prune_unreachable` | 35.3 [34.8–35.6] | 6 / 11 | 5.0 | 122,716 |
/// | `split_windows` | 23.4 [23.2–23.8] | 1 / 2 | 8.8 | 159,530 |
/// | `dedupe_intrinsics` | 19.7 [19.6–19.9] | 5 / 11 | 7.3 | 104,920 |
/// | `specialize_jump_patterns` | 18.5 [18.5–18.8] | 3 / 11 | 9.4 | 168,294 |
/// | `fuse_append_chains` | 12.8 [12.7–13.0] | 1 / 11 | 4.6 | 104,904 |
/// | `forward_aggregate_projections` | 12.2 [12.0–12.3] | 8 / 11 | 1.8 | 9,479 |
/// | `fold_intrinsic_identities` | 11.1 [11.0–11.2] | 5 / 11 | 0.0 | 78 |
/// | `specialize_call_patterns` | 5.88 [5.84–5.99] | 2 / 11 | 2.1 | 16,263 |
/// | `forward_continuations` | 5.2 [5.1–5.3] | 7 / 11 | 5.7 | 104,093 |
/// | `rewrite_atoms` | 3.44 [3.40–3.52] | 5 / 11 | 0.0 | 54 |
/// | `forward_calls` | 2.96 [2.92–2.99] | 2 / 11 | 0.5 | 3,361 |
/// | `simplify_nodes` | 1.22 [1.20–1.30] | 5 / 11 | 0.0 | 973 |
///
/// The TOML fixpoint takes 39 rounds at the baseline and 11 at the candidate. Its cumulative allocations fall from 7,872.4 MiB over 101,127,354 allocator calls to 2,797.3 MiB over 36,502,263 calls; these are allocation work, not peak resident memory. Jump specialization changes 34 baseline rounds and 3 candidate rounds, uncurrying 25 and 3, worker splitting 8 and 2, and call specialization 4 and 2. SCC specialization changes no TOML round; its independent contexts are covered by the direct and source consumers. The boolean totals do not count candidates. `split_windows` runs only on otherwise idle rounds, so its denominator is two invocations, not eleven rounds. Loop-entry separation changes five rounds in both revisions: its separately bounded, atomic copy-and-contify policy stays unchanged, and these counts do not establish that its remaining cases are independent. The default-feature comparison establishes the compilation improvement; every runtime span overlaps its control after accounting for clock resolution.

#[test]
#[ignore = "measurement, mixed: reports what each pass of the fixpoint costs rather than asserting"]
fn fixpoint_pass_measurements() {
    let entrypoint = TOML_DRIVER.parse::<Entrypoint>().expect("driver parses");
    // The stream goes to a buffer and is folded back here: this test wants the aggregate, which is one consumer of the rows.
    let rows = Rows::default();
    let outcome = trace(Destination::Stream(Box::new(rows.clone())), || {
        compile_with_prelude(
            DEFAULT_STEP_BUDGET,
            &entrypoint,
            &RootSource::none(),
            |_| {},
        )
    })
    .expect("a stream destination opens");
    outcome.expect("driver compiles");
    let report = fold(rows.rows().as_slice()).expect("the rows fold");

    println!(
        "{:>10} {:>6} {:>9} {:>9}  name",
        "total_ms", "calls", "min_ms", "max_ms"
    );
    for summary in report.summaries.iter().filter(|summary| {
        summary.name == "compile_entrypoint" || summary.target.starts_with("curios_cont")
    }) {
        println!(
            "{:>10.3} {:>6} {:>9.3} {:>9.3}  {}",
            summary.total.as_secs_f64() * 1_000.0,
            summary.calls,
            summary.min.as_secs_f64() * 1_000.0,
            summary.max.as_secs_f64() * 1_000.0,
            summary.name,
        );
    }

    println!();
    println!("{:>10} {:>7}  name", "total", "samples");
    for sample in report
        .samples
        .iter()
        .filter(|sample| sample.name.starts_with("cont"))
    {
        println!("{:>10} {:>7}  {}", sample.total, sample.count, sample.name);
    }
}
