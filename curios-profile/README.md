# curios-profile

Programmatic profiling for the workspace: the `profile!`, `sample!` and `note!` macros every instrumenting crate uses, and — under `enabled` — `trace` and `install`, the two scopings of the subscriber that writes one tab-separated row per span and event as it happens; `fold`, which recomputes timings, allocation figures and magnitude distributions from those rows; `capture_host_records`, the same scoping for what the engine says through the `log` facade; `trace_build_script`, the scoping for a build script; and the counting allocator this crate installs. It is the workspace's only `tracing`, `tracing-subscriber` and `tracing-log` dependency, and through the last its only naming of `log` ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)): every crate that instruments depends on it unconditionally and declares `profile = ["curios-profile/enabled", …]`, and its macros expand to `$crate::tracing::…`, asking nothing of the caller. What each macro expands to, what a row holds and what a fold returns belong to the crate rustdoc.

## Taking a profile

The `profile` feature compiles the instrumentation in, and `--profile <PATH>` is what makes something listen. `cargo xtask profile <PATH>` does both — it builds the compiler with the feature, runs `<PATH>` under it with a destination, and folds what the run filed — in debug by default, and `--profile release` measures the shipped compiler:

```sh
cargo xtask profile programs/hello_world.crs
```

The flag is not a mode, so it measures whatever the invocation does anyway — `run`, `test`, `document`, a package build — and it takes its destination rather than defaulting to one, so no path lives in the compiler. The stream rotates to `<PATH>.prev` at half a gibibyte, keeping an endless run's tail. A build with the feature on and no flag records nothing, which keeps `--all-features` across the gate from filing anything; instrumentation nobody listens to costs one relaxed atomic load per span. A build script has no caller to take a destination from, so `trace_build_script` files its stream at `.artifacts/profile.tsv` beside the crate being built: the prelude's elaboration at `curios-prelude-archive/.artifacts/profile.tsv`, its certification at `curios-prelude/.artifacts/profile.tsv`.

Nothing waits for the end: each row is on disk before the step after it runs, so a compilation that hangs, overflows its stack or aborts loses nothing a buffer held. Fold a killed run's file and the summary names the spans still open — the stack the compiler was inside when it stopped. The first column is the row's kind, so a question the summaries do not answer is a one-liner:

```sh
awk -F'\t' '$1 == "V"' curios/.artifacts/profile.tsv
```

## Design

### Profiling is configured in code, never from the environment

**Decision.** There is no environment-variable switch and no metrics API. What is instrumented is decided by the `profile` feature and the call sites it compiles in; whether anything records, and where, is one argument at the call site — `curios`'s `--profile <PATH>`, or a destination a caller hands `trace`. The one path this crate names is a build script's, `.artifacts/profile.tsv` beside the crate being built, read from `CARGO_MANIFEST_DIR`, which says where and never whether. `trace` runs one operation under a thread-local subscriber, and `install` makes the recorder the process's global default, which a binary needs because its work is whatever subcommand the arguments selected and it has no closure to wrap; a thread-local `trace` still overrides the default for its closure. Stage entry points and optimizer passes carry permanent spans; a span added for one investigation is removed once it is answered.

**Rationale.** A measurement is already specified at its call sites, and an out-of-band specification could only disagree; an argument is that specification, in band and read where it is written. `capture_host_records` keeps the rule within the one constraint the `log` facade imposes, one process-global logger: its bridge is installed lazily and permanently, but `log`'s max level stays `Off` except inside a capture.

**Rejected.** Recording on every invocation of a build with the feature: `--all-features` turns it on across the gate, so every spawned `curios` would file a stream onto one path concurrently. An exclusive lock on the stream, which makes an arbitrary process the winner, or a stream per process, which files one per spawned compiler, treat that symptom. A profiling subcommand, which could profile one synthetic compilation and nothing else; `curios profile` reads a stream back instead.

### Three instruments, because time and bytes cannot tell waste from bad inputs

**Decision.** Beside duration (`profile!`) and allocation (the counting allocator), `sample!` records a *magnitude* — how many, how wide, how deep — whose count, total, min, max and mean a fold reports. `note!` is the third call-site macro and measures nothing: it states why a decision went the way it did, on a path taken rarely — a solver giving up, a level defaulting, a candidate rejected.

**Rationale.** A duration and a byte count are equally consistent with an operation that is wasteful and one handed inputs it should never have seen, and optimizing the wrong one buys a constant factor against something structural; the distribution of input sizes chooses the fix. A refusal nobody can explain is what sends a reader back to `println!`, and a note that fires on everything explains nothing.

**Rejected.** A `sample!` of a constant, a call counter in this instrument's clothes that the enclosing span already counts: nine such sites were 97% of every event the prelude build emitted, 14.5 million from one site inside `Term`'s equality.

### The library emits records, and aggregation is one of its consumers

**Decision.** `trace` writes one row per span and event as it happens and nothing is summed in the process measured; `fold` recomputes the aggregate from the rows for every consumer that wants one. The fold also credits each exit's extent to the span it was entered inside and reports what a span kept for itself, in time and bytes, and keeps each span's costliest calls with the fields beyond its `group`, a bounded number per row. The rows are tab-separated, and each rotated file restates the callsite table at its head.

**Rationale.** A capture that returns its report can return it only once the operation ends, so the runs most worth profiling, the ones that hang, would produce nothing. Every aggregate column is a difference of two rows, and the rows keep what no aggregate holds: the order of events, the spans still open, and any question thought of after the run. An inclusive row counts every span entered inside it, which overstates work that re-enters itself, and the order of entries and exits is the nesting self time needs; the stream has one stack because the compiler is single-threaded by construction, and an exit that is not the innermost entry keeps its inclusive extent and credits no parent. A span's fields are already on its creation row, so keeping the costliest calls records nothing new. Tab-separated rows need no reader but `awk`.

### The allocator counts process-wide, and this crate installs it

**Decision.** The counting allocator maintains process-wide live, cumulative, high-water and count figures, and this crate installs it as the `#[global_allocator]` of every binary it is linked into with `enabled` on; no binary names it.

**Rationale.** A `GlobalAlloc` cannot allocate the thread-local state per-thread attribution needs, and the profiled pipelines are single-threaded, so process-wide is precise where it is used. A binary that had to opt in could forget, and its columns would read zero — absent evidence rather than a failure — and the opt-in was four copies of one static. This crate's own test binary counts too, so a test can hold a boundary row's readings to the allocation made inside it. Every binary built with `enabled` pays a handful of relaxed atomics per allocation.

**Rejected.** Each binary opting in, a place to forget.
