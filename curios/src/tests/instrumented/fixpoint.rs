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
/// Use isolated source revisions and separate Cargo target directories for default-feature and profiling builds, the same lockfile and Rust toolchain, release `opt-level = 3` and build-script `opt-level = 3`. Copy each revision's executable before building the next, and finish every build and check before timing. A temporary native probe depends on the revision's pipeline, text, Wasm, Binaryen, ABI, archive and profile crates; its source is identical for both revisions. Its compilation operation reads a source string, parses `Entrypoint`, calls `compile_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none(), |_| {})`, and passes the result through `std::hint::black_box`. It uses no package cache, stage rendering, Binaryen or Cranelift during the compilation measurement.
///
/// The common source cohort is copied from the measurement checkout before preparing the compiler revisions. Sources are `TOML_DRIVER` in `codegen/census.rs`; `JUMPS`, `WORKERS`, `UNCURRY`, `CALLS` and `SCCS` in `codegen/sweeps/test_support.rs`; `LISTING_PROGRAM` in `tui.rs`; and `programs/{parse_digits,monad_io,monad_suspend,walk_mirror_held_scan}.crs`. Copy their literal contents to temporary source files. The source tests keep their small runtime answers, including the escaping uses that keep the call-pattern consumer outside inlining and worker splitting.
///
/// For unprofiled compilation, build the probe with default features. Take one unmeasured warm batch and seven measured batches per source and revision, running the two revisions consecutively for each source and alternating which goes first each batch. A batch is ten fresh probe processes, except TOML and TUI, whose batch is one. Time the batch with `/usr/bin/time -f '%e'`, divide its process wall time by the batch size, and report the median and observed span of the seven readings. Startup, source reading, parsing, prelude restoration and teardown are included; Cargo, prelude construction, serialization and native precompilation are excluded. The clock prints in 10 ms increments: expand each observed span by one tick divided by its batch size before deciding whether spans are disjoint.
///
/// For pass attribution, build both probes with a `profile` feature forwarding `curios-pipeline/profile` and `curios-profile/enabled`. Trace the same TOML compilation with `curios_profile::trace` to `Destination::File`, fold its stream and require every span to close. Take one warmup and seven interleaved readings, alternating the first revision. The table below gives medians and observed spans of each pass's aggregate time; rounds and allocations are read from the same captures. The observer renders nothing. The command above uses an in-memory stream for inspection; paired figures use the file-stream protocol on both revisions.
///
/// To attribute bounded copy admission, add temporary spans in the isolated profiling copies around the inliner and loop separator's call analysis, planning and execution, and the call/jump specializers' planning and copy execution. Wrap the shared planner in a span and sample its encountered distinct nodes and enumerated binding-group members at every return; sample whether each inline is declined for its size. These instruments exist only in the comparison copies. Default-feature executables are frozen before instrumentation. Profile timings include tracing and the counting allocator; default-feature process times establish the product comparison.
///
/// For counted artifacts, compile each common source with an observer that writes `Stage::ContOptm`'s printed graph, serialize the module with `curios_wasm::to_bytes` (names retained), call `curios_binaryen::optimize(raw, false, |_| {})` (names removed), and archive the harvested `ForeignStore` with `curios_archive::to_bytes`. Compare all four outputs byte for byte. The codegen consumers validate and execute their raw and optimized modules. Identical runtime artifacts are not retimed.
///
/// For counted planning work, add temporary Cont tests to the two isolated revisions and run them with `profile` enabled. Build one non-escaping callee with a spine of `N = 512, 1024, 2048` nodes (one return, preceded by literal bindings), reached by `K = 1, 16, 64` known sites in continuation arms of an entry switch. Trace only the production inliner, excluding fixture construction and verification. Repeat with accepted one-, eight- and sixteen-node spines and with `N` one-node nested functions introduced by one wide `LetFun` in a two-node root body. Fold node and group-member samples separately and compare resulting Cont text. Read each shared-planner sample under its enclosing planning span to attribute consumer counts by caller; no per-node event or production test hook is added.
///
/// # Latest reading
///
/// Mixed: counted artifact identity, optimizer rounds, planning visits and allocation work; timed default-release process wall times and matched profile attribution. Taken at `98c8c3647` against `3afb81c44`, AMD Ryzen 7 5800X3D, x86-64 Linux `7.2.9-ogc3.1.fc44.x86_64`, Rust `1.95.0`, frequency scaling and boost enabled. Default and profile executables are built separately; the profiling copies alone carry temporary attribution spans and planning samples.
///
/// Default-feature compilation, process wall time per compilation:
///
/// | Consumer | Baseline ms [span] | Candidate ms [span] | Span comparison |
/// | --- | ---: | ---: | --- |
/// | `calls` | 109 [109–111] | 109 [109–111] | overlap |
/// | `jumps` | 130 [128–131] | 129 [129–131] | overlap |
/// | `monad_io` | 285 [282–289] | 284 [281–287] | overlap |
/// | `monad_suspend` | 516 [513–522] | 519 [512–521] | overlap |
/// | `parse_digits` | 277 [276–279] | 277 [276–278] | overlap |
/// | `sccs` | 89 [87–89] | 88 [88–89] | overlap |
/// | `toml` | 3740 [3730–3770] | 3780 [3760–3800] | overlap |
/// | `tui` | 2680 [2670–2730] | 2700 [2670–2720] | overlap |
/// | `uncurry` | 91 [90–93] | 92 [90–92] | overlap |
/// | `walk_mirror_held_scan` | 323 [321–323] | 323 [320–324] | overlap |
/// | `workers` | 89 [87–94] | 89 [88–90] | overlap |
///
/// Every default-feature compilation span overlaps its control after accounting for clock resolution, so this campaign establishes no compilation speedup or regression.
///
/// All eleven consumers have byte-identical optimized Cont, raw Wasm, Binaryen-optimized Wasm and archived foreign declarations. Runtime artifacts are identical and are not retimed. The copying, sweep and dead-parameter codegen consumers validate and execute both Wasm paths, including suspension and nested captures.
///
/// Matched TOML profile attribution:
///
/// | Span | Baseline ms [span] | Candidate ms [span] |
/// | --- | ---: | ---: |
/// | `compile_entrypoint` | 4244.7 [4212.7–4269.6] | 4301.1 [4283.6–4385.7] |
/// | `cont_optimize` | 3704.7 [3676.6–3737.1] | 3760.2 [3748.8–3842.1] |
/// | `cont::inline_known_calls` | 718.5 [712.2–725.4] | 715.6 [709.8–733.5] |
/// | `cont::inline_analysis` | 380.0 [375.5–383.8] | 395.5 [391.3–414.0] |
/// | `cont::inline_planning` | 150.3 [149.8–151.4] | 135.4 [134.7–137.4] |
/// | `cont::inline_execution` | 29.1 [28.4–30.0] | 28.7 [28.2–30.2] |
/// | `cont::separate_loop_entries` | 134.0 [132.4–138.1] | 138.7 [135.2–141.7] |
/// | `cont::loop_analysis` | 129.2 [127.6–133.4] | 134.0 [130.5–137.0] |
/// | `cont::loop_planning` | 0.1 [0.1–0.1] | 0.1 [0.1–0.1] |
/// | `cont::loop_execution` | 0.5 [0.5–0.6] | 0.5 [0.5–0.5] |
///
/// Both revisions settle in eleven optimizer rounds. Every Cont fired count and graph-size sample agrees. Planning is a portion of each pass: the analysis and successful rewrites remain, and the complete pass time is not the saving. The profiling build has higher candidate compile and Cont totals despite the smaller planner workload; the default-feature readings above establish the product comparison. Per-attempt tracing contributes to the planning spans and is absent from those default executables. Allocation figures include temporary profile instrumentation and measure cumulative allocation work, not peak resident memory.
///
/// | Planning caller | Attempts, both | Baseline nodes | Candidate nodes | Baseline group members | Candidate group members |
/// | --- | ---: | ---: | ---: | ---: | ---: |
/// | `cont::inline_planning` | 14,485 | 756,142 | 257,961 | 231,599 | 98,516 |
/// | `cont::loop_planning` | 11 | 291 | 291 | 116 | 116 |
/// | `cont::call_planning` | 39 | 852 | 852 | 393 | 393 |
/// | `cont::jump_planning` | 1,052 | 17,868 | 17,868 | 6,137 | 6,137 |
///
/// Both revisions decline 10,950 of the inliner's 14,485 planning attempts for their size. Inliner call analysis retains the same 5,693,518 allocation calls in both revisions. Total Cont allocation work is 2,813.0 MiB over 37,504,052 calls at the baseline and 2,806.9 MiB over 37,410,058 calls at the candidate.
///
/// | Candidate pass | ms [span] | Fired / calls | Allocated MiB | Allocation calls |
/// | --- | ---: | ---: | ---: | ---: |
/// | `inline_known_calls` | 715.6 [709.8–733.5] | 5 / 11 | 516.5 | 7,131,197 |
/// | `answer_row_tests` | 456.3 [449.4–462.5] | 0 / 11 | 352.0 | 4,798,544 |
/// | `split_parameters` | 425.1 [422.2–436.1] | 6 / 11 | 203.5 | 2,452,167 |
/// | `split_workers` | 422.3 [416.7–431.6] | 2 / 11 | 351.8 | 4,255,474 |
/// | `known_values` | 260.2 [256.5–267.9] | — / 11 | 264.4 | 3,614,207 |
/// | `split_returns` | 213.8 [211.1–215.1] | 6 / 11 | 190.3 | 2,399,815 |
/// | `uncurry_returns` | 164.8 [162.9–169.2] | 3 / 11 | 162.7 | 2,104,178 |
/// | `flatten_indexed_lists` | 158.7 [157.5–161.1] | 1 / 11 | 68.4 | 1,181,005 |
/// | `specialize_scc_calls` | 145.3 [144.2–151.0] | 0 / 11 | 149.7 | 1,911,317 |
/// | `contify_calls` | 145.3 [141.3–148.4] | 5 / 11 | 148.9 | 1,896,062 |
/// | `separate_loop_entries` | 138.7 [135.2–141.7] | 5 / 11 | 148.5 | 1,905,120 |
/// | `eliminate_dead_parameters` | 57.0 [56.6–58.4] | 6 / 11 | 31.5 | 357,630 |
/// | `inline_single_use_continuations` | 52.0 [51.4–53.4] | 7 / 11 | 15.1 | 294,303 |
/// | `eliminate_dead_calls` | 46.7 [46.2–47.3] | 1 / 11 | 27.2 | 253,844 |
/// | `eliminate_dead_bindings` | 38.8 [38.4–39.7] | 8 / 11 | 12.5 | 285,515 |
/// | `prune_unreachable` | 36.2 [35.3–37.4] | 6 / 11 | 5.0 | 122,716 |
/// | `specialize_jump_patterns` | 32.5 [32.0–33.0] | 3 / 11 | 10.3 | 225,772 |
/// | `split_windows` | 23.8 [23.6–26.5] | 1 / 2 | 8.8 | 159,530 |
/// | `dedupe_intrinsics` | 20.8 [20.1–22.4] | 5 / 11 | 7.3 | 104,920 |
/// | `fuse_append_chains` | 13.2 [12.8–14.0] | 1 / 11 | 4.6 | 104,904 |
/// | `forward_aggregate_projections` | 12.5 [12.3–12.9] | 8 / 11 | 1.8 | 9,479 |
/// | `fold_intrinsic_identities` | 11.1 [11.0–11.6] | 5 / 11 | 0.0 | 78 |
/// | `specialize_call_patterns` | 6.7 [6.6–7.2] | 2 / 11 | 2.1 | 18,583 |
/// | `forward_continuations` | 5.3 [5.1–5.7] | 7 / 11 | 5.7 | 104,093 |
/// | `rewrite_atoms` | 3.5 [3.4–3.6] | 5 / 11 | 0.0 | 54 |
/// | `forward_calls` | 3.1 [3.0–3.2] | 2 / 11 | 0.5 | 3,361 |
/// | `simplify_nodes` | 1.2 [1.2–1.3] | 5 / 11 | 0.0 | 973 |
///
/// `split_windows` runs only on otherwise idle rounds. Fired samples count invocations changing the graph, not candidates.
///
/// Counted distinct nodes encountered while rejecting oversized spines:
///
/// | Body nodes | Known sites | Baseline visits | Candidate visits |
/// | ---: | ---: | ---: | ---: |
/// | 512 | 1 | 512 | 257 |
/// | 512 | 16 | 8,192 | 272 |
/// | 512 | 64 | 32,768 | 1,088 |
/// | 1,024 | 1 | 1,024 | 257 |
/// | 1,024 | 16 | 16,384 | 272 |
/// | 1,024 | 64 | 65,536 | 1,088 |
/// | 2,048 | 1 | 2,048 | 257 |
/// | 2,048 | 16 | 32,768 | 272 |
/// | 2,048 | 64 | 131,072 | 1,088 |
///
/// A single-site attempt encounters 257 nodes and a duplicated-site attempt seventeen, independent of the oversized spine length. The accepted one-, eight- and sixteen-node controls encounter the same nodes at all three site counts, and all twenty-seven synthetic resulting Cont graphs are byte-identical. Wide groups still enumerate every member at their binding node: the `N`-member, `K`-site controls enumerate `N × K` members in both revisions while bounding body-node visits. The allowance bounds body traversal, not arbitrary binding-group width.

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
