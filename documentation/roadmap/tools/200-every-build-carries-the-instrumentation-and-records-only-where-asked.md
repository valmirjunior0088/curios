# Every build carries the instrumentation, and records only where asked

This specification removes the cost of the instrumentation being a build of its own: what only a feature compiles takes a second compilation of the workspace at every check, and the compiler a user installs could profile nothing. Every build now carries the macros, the recorder, `--profile` and `curios profile`, the prelude's profiling program is `curios-prelude-build`'s, and the guest's map follows `--profile`; what is left is the step that compiled the features. The next step is the delivery of stage 1.

## What is wrong today

- **A step compiles what no feature turns on any longer.** `cargo xtask instrumented lint` lints the workspace with every feature on, a second compilation of all `workspace lint` compiles, among the checks CLAUDE.md names for every change and a job of `.github/workflows/check.yml`; every feature of the workspace is now a crate's `test-support`, which a dev-dependency of another crate turns on, so `workspace lint`'s `--all-targets` compiles it already, and the step finds nothing `workspace lint` does not ([Every handoff step catches what no other step does](../../design/every-handoff-step-catches-what-no-other-step-does.md)). The measurements' recorded commands spell `--all-features`, which turns nothing on.

## Peers

- rustc's self-profiler is in every rustc, switched by `-Z self-profile`: a session holds an optional profiler, `None` when off, and the event filter is inlined with the recording kept out of line, the code "optimized for non-profiling compilation sessions" ([`profiling.rs`](https://rust.googlesource.com/rust/+/HEAD/compiler/rustc_data_structures/src/profiling.rs), [the self-profiler](https://blog.rust-lang.org/inside-rust/2020/02/25/intro-rustc-self-profile/)).
- Clang's `-ftime-trace`: LLVM's `TimeTraceScope` costs a branch where no profiler was initialized ([`TimeProfiler.h`](https://llvm.org/doxygen/TimeProfiler_8h_source.html)).
- Lean's `trace.profiler.output` is an option of every `lean` ([`Lean.Util.Profiler`](https://lean-lang.org/doc/api/Lean/Util/Profiler.html)), and its heartbeats, the small allocations a thread performs, are counted in every build because a deterministic timeout reads them ([Timing](https://lean-lang.org/doc/reference/latest/IO/Timing/)).
- Rocq's `NewProfile` is compiled in and wraps what it profiles, the components to record read from the environment ([`NewProfile`](https://ocaml.org/p/rocq-runtime/9.1.1/rocq-runtime.lib/NewProfile/index.html)).
- GHC 9.4 removed its separate eventlog runtime ways, enabling the eventlog in all of them for simpler packaging and shorter builds, and deprecated `-eventlog` ([MR 7983](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7983)); cost-centre profiling stays a build apart, `-prof`, since cost centres change how what they measure is optimised ([Profiling](https://haskell.org/ghc/docs/latest/html/users_guide/profiling.html)).
- LLVM's `STATISTIC` counters compile only where `NDEBUG` is off or `LLVM_FORCE_ENABLE_STATS` is set, and its header recommends `ALWAYS_ENABLED_STATISTIC` unless there is a reason not to ([`Statistic.h`](https://llvm.org/doxygen/Statistic_8h_source.html)).
- Chromium's `TRACE_EVENT` caches its category's enabled flag at the site, a load and a branch where tracing is off, and converts no argument ([`trace_event_common.h`](https://chromium.googlesource.com/chromium/src/base/trace_event/common/+/master/trace_event_common.h)); Linux's static keys patch a no-op in place of even that load ([Static Keys](https://docs.kernel.org/staging/static-keys.html)), which Rust has no stable form of.

Adopted: the instrumentation in every build, switched at run time by an argument, as rustc, Clang, Lean and GHC's eventlog are; a counter in every build where a reading shows it cheap. Avoided: counters compiled into one configuration, which LLVM's own header steers away from, and the environment as the switch, which [`curios-profile`'s README](../../../curios-profile/README.md#profiling-is-configured-in-code-never-from-the-environment) refuses. Curios's own: a count a verdict reads is the budget, in every build already, so the looks need no verdict's argument and stand or fall by the study alone.

## What the study found

Counted at `decbf4ff5`, in the user-space instructions retired by a subject's process and every thread and process it starts, read by a wrapper that opens `Instructions` and then starts the subject, under `BINARYEN_CORES=1 RAYON_NUM_THREADS=1`. Six release builds of one scratch copy of that commit, each of `curios` and of `profile-prelude` with its `main` reduced to `compile` into a kept directory and no recorder, told apart by digest: **A**, every `profile` feature off; **B**, the macros and every `cfg(feature = "profile")` site compiled, the look counter and the allocator not, the guest's map off; **C**, B and the look counter counting; **C′**, B and the counter behind a relaxed load nothing sets; **D**, B and the allocator counting; **E**, B and the allocator installed, behind a relaxed load nothing sets. The copy splits `curios-profile`'s `enabled` from the allocator and `curios-core`'s `profile` from the counter by features of its own, and names `curios-core` as a dependency of `curios`, whose dev-dependency it is, so the counter's features reach the shipped binary. Subjects: `profile-prelude`, the prelude's elaboration, certification and erasure; and `curios compile -o` over the 31 programs of `programs/`, each with a fresh `CURIOS_CABINET`. One reading of each build and subject, the six builds in a fresh shuffled order for each subject:

| | the prelude's build | the corpus compiled |
| --- | --- | --- |
| A | 1,323,874,529,180 | 480,562,435,429 |
| B over A, the macros and their sites | −0.22% | +0.05% |
| C over B, the look counter counting | +0.31% | +0.01% |
| C′ over B, the counter behind a load | +0.15% | −0.01% |
| D over B, the allocator counting | +3.42% | +0.53% |
| E over B, the allocator behind a load | −0.78% | −0.06% |

- **A build repeats to 0.02%**: B and E read again on the prelude retire 1,320,689,926,581 and 1,310,430,046,188 against 1,320,956,043,088 and 1,310,623,317,179. A gap of up to 0.8% between two builds is the price of compiling a different binary, which E, doing more than B, reads below it by; only D stands past it, by four locked read-modify-writes an allocation.
- **Bytes**: the release `curios` grows by 1,028,376 bytes under B, 0.49%; the bundle's `curios_js.wasm`, built for `wasm32-unknown-unknown`, by 330,591, 0.26%.
- **Outputs**: every program's executable and the prelude's five images are one digest across the six builds.

## Stages

1. **The jig builds nothing of its own.** `instrumented` goes, with its job in `check.yml`, its subject in the handoff's `requires`, its line in CLAUDE.md's checks and its paragraphs in `xtask`'s README and [Every handoff step catches what no other step does](../../design/every-handoff-step-catches-what-no-other-step-does.md); the measurements' recorded commands drop `--all-features`. Check: `cargo xtask tools test`, `cargo xtask docs build`.

## Retirement

Once stage 1 lands, `curios-profile`'s README holds *What the study found* as the argument of its decision that every build carries the instrumentation, and this file is deleted.
