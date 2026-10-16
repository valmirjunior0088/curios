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
/// To separate dead-parameter demand solving from the remainder of the pass, add temporary `cont::dead_parameter_demands` and `cont::dead_parameter_rewrite` spans to the isolated source copies: wrap the initial `demands(module)` call with the former, and scope the latter over the rest of `eliminate_dead_parameters`. These spans and the repair-visit sample described below exist only in the comparison copies. Profile timings include their instrumentation and the counting allocator; default-feature process times establish the product comparison.
///
/// For counted artifacts, compile each common source with an observer that writes `Stage::ContOptm`'s printed graph, serialize the module with `curios_wasm::to_bytes` (names retained), call `curios_binaryen::optimize(raw, false, |_| {})` (names removed), and archive the harvested `ForeignStore` with `curios_archive::to_bytes`. Compare all four outputs byte for byte. The codegen consumers validate and execute their raw and optimized modules. Identical runtime artifacts are not retimed.
///
/// For counted repair work, build temporary Cont tests in the two isolated revisions with `profile` enabled. Construct `N = 64, 128, 256` pairs: a non-escaping known function taking one unread parameter and returning zero, and a continuation taking one unread parameter and passing it to that function, with the call returning to the entry's sentinel. The nullary entry binds all functions and continuations and switches between jumps passing zero into each continuation. Each function body is one node, each continuation body one node, and the entry adds a switch and the two binding nodes: `2N + 3` live nodes with no tombstones. Verify before and after one invocation of `eliminate_dead_parameters`, tracing only that invocation. In temporary pass instrumentation, count each actual repair traversal by adding the invocation's live-node count; include every baseline `Module::replace_atom` walk. The pass never mints or removes nodes, so that count stays exact throughout the invocation. Record the sum once as `cont::dead_parameter_repair_visits`, and read it with `curios_profile::fold`. Exclude demand iterations and read-only scans for result receivers and escaping functions. Remove the tests and instrumentation after the investigation; no production counter or metric API is retained.
///
/// # Latest reading
///
/// Mixed: counted artifact identity, optimizer rounds, repair visits and allocation work; timed default-release process wall times and matched profile attribution. Taken at `812080299` against `62cf6687d`, AMD Ryzen 7 5800X3D, x86-64 Linux `7.2.9-ogc3.1.fc44.x86_64`, Rust `1.95.0`, frequency scaling and boost enabled. Default and profile executables are built separately; the profiling copies alone carry the temporary attribution spans and repair-visit sample.
///
/// Default-feature compilation, process wall time per compilation:
///
/// | Consumer | Baseline ms [span] | Candidate ms [span] | Span comparison |
/// | --- | ---: | ---: | --- |
/// | `calls` | 108 [107–110] | 108 [107–110] | overlap |
/// | `jumps` | 126 [125–132] | 128 [126–132] | overlap |
/// | `monad_io` | 285 [283–287] | 284 [282–286] | overlap |
/// | `monad_suspend` | 516 [514–520] | 516 [510–521] | overlap |
/// | `parse_digits` | 276 [274–278] | 277 [272–279] | overlap |
/// | `sccs` | 89 [86–90] | 89 [87–91] | overlap |
/// | `toml` | 4060 [4040–4150] | 3740 [3700–3770] | faster |
/// | `tui` | 2720 [2690–2740] | 2670 [2670–2750] | overlap |
/// | `uncurry` | 90 [90–93] | 91 [90–92] | overlap |
/// | `walk_mirror_held_scan` | 322 [320–327] | 321 [319–325] | overlap |
/// | `workers` | 88 [87–89] | 88 [87–90] | overlap |
///
/// TOML compilation is faster; every other span overlaps its control after accounting for clock resolution. All eleven consumers have byte-identical optimized Cont, raw Wasm, Binaryen-optimized Wasm and archived foreign declarations. Runtime artifacts are identical and are not retimed. The two consumers in `codegen/dead_parameters.rs` validate and execute both Wasm paths at four host-tainted inputs, preserving an effect computed solely for an unused argument and a nested capture across suspension.
///
/// Matched TOML profile attribution:
///
/// | Span | Baseline ms [span] | Candidate ms [span] |
/// | --- | ---: | ---: |
/// | `compile_entrypoint` | 4306.2 [4284.7–4346.5] | 3962.4 [3941.0–3991.9] |
/// | `cont_optimize` | 3779.9 [3758.7–3809.1] | 3434.0 [3414.2–3463.6] |
/// | `cont::eliminate_dead_parameters` | 363.4 [355.6–369.0] | 57.2 [56.0–58.0] |
/// | `cont::dead_parameter_demands` | 45.0 [44.4–45.3] | 44.4 [43.7–45.2] |
/// | `cont::dead_parameter_rewrite` | 317.4 [310.4–323.0] | 11.9 [11.5–12.1] |
///
/// Both revisions settle in eleven rounds, with dead-parameter cleanup changing six invocations. Every Cont fired count and graph-size sample agrees. Demand solving overlaps its control; the reduction is in the remainder of dead-parameter cleanup. The extra removal maps and binding sets keep allocations bounded by the admitted edits: cumulative Cont allocation work is 2,797.3 MiB over 36,502,710 allocator calls at the baseline and 2,797.4 MiB over 36,503,120 calls at the candidate. These figures include temporary profile instrumentation and measure allocation work, not peak resident memory.
///
/// | Candidate pass | ms [span] | Fired / calls | Allocated MiB | Allocation calls |
/// | --- | ---: | ---: | ---: | ---: |
/// | `inline_known_calls` | 501.1 [496.3–502.4] | 5 / 11 | 508.0 | 6,292,858 |
/// | `answer_row_tests` | 436.9 [434.6–439.9] | 0 / 11 | 352.0 | 4,798,544 |
/// | `split_parameters` | 407.8 [403.4–411.4] | 6 / 11 | 203.5 | 2,452,167 |
/// | `split_workers` | 403.6 [401.9–412.1] | 2 / 11 | 351.8 | 4,255,474 |
/// | `known_values` | 248.2 [244.6–251.0] | — / 11 | 264.4 | 3,614,207 |
/// | `split_returns` | 207.3 [205.4–211.5] | 6 / 11 | 190.3 | 2,399,815 |
/// | `uncurry_returns` | 159.6 [157.0–162.1] | 3 / 11 | 162.7 | 2,104,178 |
/// | `flatten_indexed_lists` | 158.3 [156.6–159.2] | 1 / 11 | 68.4 | 1,181,005 |
/// | `contify_calls` | 139.6 [138.0–142.4] | 5 / 11 | 148.9 | 1,896,062 |
/// | `specialize_scc_calls` | 137.6 [136.1–140.7] | 0 / 11 | 149.6 | 1,902,926 |
/// | `separate_loop_entries` | 132.3 [131.0–132.7] | 5 / 11 | 148.5 | 1,904,264 |
/// | `eliminate_dead_parameters` | 57.2 [56.0–58.0] | 6 / 11 | 31.5 | 358,076 |
/// | `inline_single_use_continuations` | 52.4 [51.7–53.9] | 7 / 11 | 15.1 | 294,303 |
/// | `eliminate_dead_calls` | 46.4 [45.6–46.6] | 1 / 11 | 27.2 | 253,844 |
/// | `eliminate_dead_bindings` | 38.9 [38.1–39.6] | 8 / 11 | 12.5 | 285,515 |
/// | `prune_unreachable` | 35.7 [35.3–37.9] | 6 / 11 | 5.0 | 122,716 |
/// | `split_windows` | 24.1 [23.6–24.2] | 1 / 2 | 8.8 | 159,530 |
/// | `dedupe_intrinsics` | 20.3 [20.0–21.0] | 5 / 11 | 7.3 | 104,920 |
/// | `specialize_jump_patterns` | 18.9 [18.7–19.5] | 3 / 11 | 9.4 | 168,294 |
/// | `fuse_append_chains` | 12.9 [12.7–13.3] | 1 / 11 | 4.6 | 104,904 |
/// | `forward_aggregate_projections` | 12.8 [12.3–13.7] | 8 / 11 | 1.8 | 9,479 |
/// | `fold_intrinsic_identities` | 11.4 [10.9–11.8] | 5 / 11 | 0.0 | 78 |
/// | `specialize_call_patterns` | 6.1 [6.0–6.5] | 2 / 11 | 2.1 | 16,263 |
/// | `forward_continuations` | 5.2 [5.2–5.9] | 7 / 11 | 5.7 | 104,093 |
/// | `rewrite_atoms` | 3.4 [3.4–3.9] | 5 / 11 | 0.0 | 54 |
/// | `forward_calls` | 3.1 [3.0–3.1] | 2 / 11 | 0.5 | 3,361 |
/// | `simplify_nodes` | 1.2 [1.2–1.3] | 5 / 11 | 0.0 | 973 |
///
/// `split_windows` runs only on otherwise idle rounds: two invocations rather than eleven. Fired samples count invocations changing the graph, not candidates.
///
/// Counted repair visits in one invocation, with `N` function/continuation pairs:
///
/// | Pairs | Arena nodes | Baseline visits | Candidate visits |
/// | ---: | ---: | ---: | ---: |
/// | 64 | 131 | 33,536 | 262 |
/// | 128 | 259 | 132,608 | 518 |
/// | 256 | 515 | 527,360 | 1,030 |
///
/// The baseline repairs `4N` times and the candidate twice. Across TOML's eleven optimizer rounds, repair visits fall from 67,639,772 to 139,185; the totals include live nodes awaiting pruning. The counts establish the repair bound independently of the timed improvement and exclude the solver and read-only boundary scans.

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
