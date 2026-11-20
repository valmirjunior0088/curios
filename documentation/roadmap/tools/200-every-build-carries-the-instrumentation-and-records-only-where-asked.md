# Every build carries the instrumentation, and records only where asked

**Not refined yet.** This specification removes the cost of asking the compiler for a profile: today only a build with the `profile` feature can record one, and that build is a second build of the workspace with a prelude archive of its own. Its next step is a bounded investigation, the counted reading under *Stages*, which decides whether the rest proceeds.

## What is wrong today

- **The instrumentation is a build axis.** Every macro of `curios-profile` expands under the *invoking* crate's `profile` feature, which twenty crates declare as `profile = ["curios-profile/enabled", …]` (`curios-profile/src/lib.rs`), and `--profile` and `curios profile` exist only in such a build (`curios/src/cli.rs`). `cargo xtask profile debug` and `release` build the compiler with the feature (`xtask/tasks/profile.toml`), and a build with a feature set of its own builds a prelude archive of its own, which evicts the other ([CLAUDE.md](../../../CLAUDE.md)'s feature rule). So a profile of a change costs a second build of the workspace, and an installed `curios` can profile nothing.
- **The guest's code map rides the same feature.** Under `profile` the engine writes the map a sampling profiler reads emitted code by (`ProfilingStrategy::PerfMap`, `curios-native/src/engine.rs`), so a sampler reads the guest's functions by name only in a profile build. Which names a payload carries is the writer's to say, by the names entry of [the compilation findings](../compilation/000-findings.md).

## Context

The recorder's idle cost is one relaxed atomic load per span, by its README; what a build without the feature saves is that load, the `sample!` expressions it never evaluates, and the counting allocator's relaxed atomics per allocation. The peers compile their profiler into every build and switch it at run time: rustc's `-Z self-profile` ([measureme](https://github.com/rust-lang/measureme)), Clang's `-ftime-trace`, Lean's `trace.profiler.output`, Rocq's `NewProfile`, whose annotations are computed only while profiling is active, and GHC's `-ddump-timings` with its eventlog; GHC's cost-centre profiling, which changes the code it measures, is the one built apart.

## Settled

- **The ask stays one argument**, `--profile <PATH>`, read where it is written ([`curios-profile`'s README](../../../curios-profile/README.md#profiling-is-configured-in-code-never-from-the-environment)): nothing records unasked, in any build.
- **`--profile` turns on the guest's map too**, settled by the user on 2026-11-20: one ask makes a compile and its run legible, the stream where the argument says and the map where a sampler looks for it, `/tmp/perf-<pid>.map`, a path the engine's agent offers no other choice of.
- **`Instructions` stays a measurement's**, opened by the measurements that read it and compiled where they are.
- **`profile-prelude` stays**, a build script taking no argument.

## Open

- **The counting allocator**: installed in every build and counting behind one relaxed load, so every stream carries its byte columns, or kept to measurement builds, so a stream from an ordinary build carries none. Decided by the reading.

## Stages

1. **The reading**, counted: the user-space instructions `profile-prelude`'s elaboration, certification and erasure of `/std` retire, and those of `curios compile` over every program of `programs/`, at one commit, built with the macros expanding unconditionally and no subscriber installed, against the same commit as it stands; and the same with the counting allocator installed and counting behind a relaxed load. Check: the figures written here with their commit; past a stated share, this specification is dropped.
2. **The macros expand in every build**, `trace`, `install` and `fold` compiled with them, `--profile` and `curios profile` present in every build, and the `profile` features the macros alone needed removed from every crate; the allocator as the reading decided. Check: `cargo xtask workspace lint`, `cargo xtask instrumented lint`, and a stream recorded by an ordinary build's `curios --profile` folded.
3. **The guest's code map written where `--profile` asks**, the engine configured before its first use. Check: a run under `--profile` leaves a map naming the guest's functions; one without leaves none.
4. **`cargo xtask profile` builds nothing of its own**, its steps running the ordinary build, and the README's decisions restated. Check: `cargo xtask docs build`.

## Retirement

Once stage 4 lands, the README's decision says what every build carries, and this file is deleted.
