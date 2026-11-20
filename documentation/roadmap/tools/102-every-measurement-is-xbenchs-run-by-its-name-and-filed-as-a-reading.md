# Every measurement is xbench's, run by its name and filed as a reading

Adopted by the user on 2026-11-19: `xbench` becomes the workspace's one measurement harness, taking readings and benchmarking alike, and the measurement logic scattered through the crates, the scratchpads and the documents' prose is gathered into it. A measurement is then run by one step its documents name, in a process of its own, by the method its kind fixes, and what it reads is a file the documents cite, so a figure can be retaken by anyone, held to its reading, and told from one the code has moved past.

## What is wrong today

- **Measurements are written as tests that are not tests.** 57 tests marked `#[ignore]` — 47 `measurement, counted|mixed|timed`, 5 `inventory` and 1 `a sweep`, across 24 files of `curios` (its codegen, instrumented, reduction, unfolding, inference and laws suites), `curios-pipeline` (11, `standard/tests.rs`), `curios-prelude` (7, `restore/`) and `curios-core` (1, `machine/tests.rs`) — assert nothing, or assert what no step checks, and are run by no step: each is compiled by every test build and linted by `clippy --all-targets`, and run by hand through the bare `cargo test … -- --ignored --nocapture <name>` its doc names, some with `--all-features`, so one that panics or breaks its bound is found by whoever runs it next. `growth_slopes` holds the growths that are linear to their slopes and runs only by hand, which is how [the workspace's `growth_slopes` finding](../000-findings.md) went unseen. The seven of `curios/src/tests/instrumented/` read the process-wide allocator and are each run "alone in its invocation". [The findings rules](../../../.claude/rules/findings.md) call an item cast as a kind it is not a finding. The ignored tests that assert and are held back for their cost or their environment — the network, a local `git`, 1 GiB of guest heap, `curios-num`'s two sweeps — are tests, and stay.
- **A protocol is prose each session rebuilds.** [Pending work is owned by its specification](../../design/pending-work-is-owned-by-its-specification.md) has a measurement state "a protocol precise enough to rebuild its scripts; the scripts are not checked in", and the figures across the READMEs, decisions and roadmap repeat a handful of protocols, each rebuilt by whoever retakes it: a corpus's bytes (`wonder --stage wasm`, `wasm-tools parse`, over `programs/`), what a compiled executable retires in user-space instructions, timed rounds interleaved at N less N = 1 with their median and span, the work counted over a geometric size grid, and a census of the emitted text.
- **`xbench` measures one thing.** It times the corpus's six workloads in Curios and in Rust, with the method a timing needs — groups interleaved round-robin, the median of group medians and the span between them, a difference proven only where two spans are disjoint, a comparison refused across machines — and files each sitting as a TOML file its build embeds ([xbench's README](../../../xbench/README.md)); it names nothing of the workspace, and no other measurement reaches its method.
- **`curios-profile`'s instruments are opened by hand.** Spans, the counting allocator, `sample!`, `note!` and the retired-instruction count (`Instructions`, through `perf_event_open`) are what every counted measurement reads, and each opens and reads them itself; a run under the recorder is `cargo xtask profile debug|release|prelude`, the last running `curios-prelude`'s own `profile-prelude` binary.
- **Figures are quoted where nothing holds them.** A figure names its commit and its kind by [the documentation rules](../../../.claude/rules/documentation.md), and nothing ties it to a reading: a commit rewritten after the figure names it, as an amended tip is, leaves the figure naming a commit no history holds.

## Settled

- **The harness is `xbench`**, its README restated from the corpus's timings to the workspace's measurements, the corpus's timings one measurement among them.
- **A measurement is a plain function of its crate's `test_support::measure`**, riding the crate's `test-support` feature and `profile` where it is instrumented, so it keeps the private access a crate's tests have; no `#[test]` and no `#[ignore]`.
- **A measurement declares its kind** — counted, timed, mixed or inventory, the words the documentation rules already use — and the kind fixes the method: a counted reading is one run, and a timed one is `xbench`'s groups, medians, spans and disjoint-span rule, which every timed measurement then has.
- **A measurement answers a reading**: rows of named metrics with their units, one format for every measurement, as Go's benchmark data format is one for every Go benchmark ([proposal 14313](https://go.googlesource.com/proposal/+/master/design/14313-benchmark-format.md)).
- **Each measurement runs in a process of its own**, the harness starting itself again for it, so a process-wide counter reads one measurement alone.
- **A measurement is run by one step**, `cargo xtask bench <name>`, read off `xbench`'s registry, and the protocol of every figure names it.
- **A reading is filed and cited by its file.** It is a TOML file the harness prints and the build embeds, as a timing is, recording the commit, the machine and the kind; a document cites the reading its figure is read from, and a lint holds the quoted figure to it.
- **On demand only.** No measurement runs in the handoff, a bound included; a regression a bound would show is the measurement's to show when it is run.
- **A recurring protocol is a measurement.** A protocol more than one figure uses — corpus bytes, an executable's retired instructions, interleaved timed rounds, a geometric grid, a census of the emitted text — is registered in the harness and its step is what a document names; a one-off probe's scripts stay out of the tree, and the filing decision's sentence is restated so.
- **`curios-profile` gives a reading.** A scoped reading — the instructions retired, the bytes and the count allocated, the samples taken, around a closure — is what a measurement calls instead of opening its counters by hand; `profile-prelude` and `cargo xtask profile`'s steps become measurements, the recorder and the fold as they are.
- **The harness builds apart.** Built with `test-support` and `profile`, `xbench` would build a second prelude archive where the workspace builds its own, and the two evict each other, so it builds in a target directory of its own.
- **The peers.** rustc-perf compares two artifacts by their commits and reads retired instructions by default, as their graphs are "basically straight lines"; GHC's testsuite records each counted metric per commit and compares against a baseline commit rather than a hand-written number; Gungraun counts instructions as "precise and portable" and pairs them with wall-clock tools for end-to-end claims; pyperf runs workers in processes of their own; Lean's speedcenter reads instructions for stability and meets the noise a concurrent compiler adds to them.

## Open

- **The reading's file.** Where readings stand (`xbench/readings/<measurement>/`), how one is named, and what of the machine a counted reading records, which transports across machines where a timed one does not.
- **The lint.** How a document cites a reading — a link to the file and the metric's name beside the figure — and what the lint compares: the figure as printed, at the reading's precision.
- **The registry.** How a crate's `test_support::measure` declares a measurement to `xbench`: a function per measurement listed in `xbench`, or a table each crate exports.
- **The timings.** Whether the eight sittings filed under `xbench/timings/` become readings of the corpus measurement in the new format, or stay as they are beside it.

## Stages

1. **The harness.** `xbench`'s registry, kinds, reading format, process per measurement, target directory and `cargo xtask bench` steps, the corpus's timings its first measurement. Check: the corpus's timing taken and reported as before; a counted measurement of a test fixture run, filed and reported.
2. **`curios-profile`'s reading**, and `profile-prelude` and the profile steps as measurements. Check: a scoped reading of a known workload equal to what the hand-opened counters read; `cargo xtask profile`'s three runs as measurements.
3. **`curios-core`'s and `curios-prelude`'s measurements moved**, 8. 4. **`curios-pipeline`'s**, 11. 5. **`curios`'s**, 38, by suite. Each stage: the functions moved into `test_support::measure`, their `#[ignore]` tests gone, their "What it last printed" filed as their first readings, and every document citing them restated to the reading and the step.
6. **The recurring protocols as measurements**, and every document whose figure states one restated to its step and reading.
7. **The lint**, and the rules restated: `.claude/rules/rust.md`'s "a measurement names its instrument", `.claude/rules/documentation.md`'s "a named measurement test or a stated protocol" and its measured-figure rules, and the filing decision's "the scripts are not checked in".

## Next step

The decisions under *Open*, then stage 1.

## Retirement

When no `#[ignore]` marks a measurement, an inventory or a sweep, every figure a document quotes cites a reading the lint holds it to, and `xbench`'s README states the harness, this file is deleted.
