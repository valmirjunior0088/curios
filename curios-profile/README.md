# curios-profile

Programmatic profiling for the workspace: the `profile!`, `sample!` and `note!` macros every crate instruments with, and — under `enabled` — `trace` and `install`, the two scopings of the subscriber that writes one tab-separated row per span and event as it happens; `fold`, which recomputes timings, allocation figures and magnitude distributions from those rows; `capture_host_records`, the same scoping for what the engine says through the `log` facade; `trace_build_script`, the scoping for a build script, which has no caller to name a destination; and the counting allocator this crate installs, the memory half of a report. It is the workspace's only `tracing`, `tracing-subscriber` and `tracing-log` dependency, and through the last the only naming of `log`. What each macro expands to, what a row holds and what a fold returns belong to the crate rustdoc.

## Taking a profile

The instrumentation is compiled in by the `profile` feature; `--profile <PATH>` is what makes something listen to it. `cargo x profile <PATH>` does both — it builds the compiler with the feature, runs `<PATH>` under it with a destination, and folds what that run filed. It builds in debug, the build iterating already has, so a question about a change costs the change; `--profile release` builds the shipped compiler, whose timings are the ones a specification quotes:

```sh
cargo x profile programs/hello_world.crs
```

The flag is not a mode, so it measures whatever the invocation was going to do anyway: `run`, `test`, `document`, a package build. It takes the destination rather than defaulting to one, which is what keeps a path out of the compiler entirely — the reader chooses where the stream lands and reads it back from there, so there is one spelling instead of two that must agree. The stream rotates to `<PATH>.prev` at half a gibibyte, so an endless run keeps its tail and cannot fill a disk.

**A build with the feature on and no flag records nothing.** That is what keeps `--all-features` — which the gate uses, and which turns the feature on for every `curios` the integration suite spawns — from filing anything at all. Instrumentation nobody listens to costs one relaxed atomic load per span.

A build script is the one exception, and it has to be: it has no caller to take a destination from, so `trace_build_script` files its stream at `.artifacts/profile.tsv` beside the crate being built. The prelude's two halves are its callers — the elaboration at `curios-prelude-archive/.artifacts/profile.tsv` and the certification at `curios-prelude/.artifacts/profile.tsv`, whose `recheck_module` span times the kernel's whole walk.

**Nothing waits for the end.** That is the point: a compilation that hangs or dies is exactly the one worth profiling, and each of its rows is on disk before the step after it runs — a stack overflow or an abort, which end the process without unwinding, lose nothing a buffer was holding, because nothing is held. A run that returned is summarized by the recipe; for one that had to be killed, fold the file — the summary names the spans that were still open, which is the stack the compiler was inside when it stopped. The first column is the kind, so a question the summaries do not answer is a one-liner:

```sh
awk -F'\t' '$1 == "V"' curios/.artifacts/profile.tsv
```

## Design

### One crate is the authority for one external concern

**Decision.** `tracing` and its two companions are named in this manifest and nowhere else. Every crate depends on this one unconditionally — it is close to empty until `enabled` is on — and declares its own `profile` feature as `profile = ["curios-profile/enabled", …]`. The arrangement is [One crate is the authority for one external concern](../documentation/design/toolchain/one-crate-is-the-authority-for-one-external-concern.md).

**Rationale.** The design entry's, and one concrete consequence: it is what retired `#[cfg_attr(feature = "profile", tracing::instrument(…))]`, which could not survive re-export because its expansion requires a crate literally named `tracing` in the invoking crate's extern prelude. A macro of this crate's own expands to `$crate::tracing::…` and asks nothing of the caller.

### Profiling is configured in code, never from the environment

**Decision.** There is no environment-variable switch and no metrics API. What is instrumented is decided by the `profile` feature and by the call sites it compiles in; whether anything records, and where, is decided by one argument at the call site — `curios`'s `--profile <PATH>`, or a destination a caller hands `trace` directly. This crate names one path, for the one caller that cannot name its own: a build script's stream goes to `.artifacts/profile.tsv` beside the crate being built, the repository's rule for a build product that outlives its build, read from `CARGO_MANIFEST_DIR` — which says where, never whether, since the calling crate's `profile` feature is what decides that a build script records at all. A capture is scoped two ways, and which one a caller wants follows from whether it has a closure to wrap: `trace` runs one operation under a thread-local subscriber, and `install` makes the recorder this process's global default. Stage entrypoints and optimizer passes carry permanent spans; a span added to isolate one investigation is removed once the question is answered.

**Rationale.** A measurement is already specified at its call sites, and a second, out-of-band specification could only disagree with the first. An argument is that first specification rather than a second one: it is in-band, explicit, and read where it is written.

**Why a global default, given that.** A *binary* has no closure to wrap: the work is whatever subcommand the arguments selected, so a scoped capture could only ever cover a synthetic operation the binary performed on profiling's behalf — which is what `curios profile` was, and why it could profile one compilation and nothing else. Making the invocation the scope is what lets `run`, `test` and a package build be profiled at all. The two do not compete: `set_global_default` is consulted only where no thread-local subscriber is set, so `trace` still overrides it for the duration of its closure, and the build script and the probes that need a scoped capture keep one.

**Rejected — recording on every invocation of a build that has the feature.** It needs no argument, which is what recommended it, and it was how this landed at first. But `--all-features` turns the feature on across the gate, so every `curios` the integration suite spawns would have filed a stream, all of them onto one path, concurrently. The feature stopped being inert, and a build-level switch with an unconditional runtime effect is a thing that breaks quietly later. Two ways of containing that were weighed and both treat the symptom: an exclusive lock on the stream, which makes an arbitrary process the winner, and a stream named per process, which would have the gate write one real stream per spawned compiler.

`capture_host_records` keeps the rule within the one constraint the `log` facade imposes — one process-global logger — so its bridge is installed lazily and permanently, but `log`'s max level stays `Off` except inside a capture, and a build that never captures pays one relaxed atomic load per suppressed record.

### Three instruments, because time and bytes cannot tell waste from bad inputs

**Decision.** Beside duration (`profile!`) and allocation (the counting allocator), `sample!` records a *magnitude* — how many, how wide, how deep — and a fold reports its count, total, min, max and mean.

**Rationale.** A duration and a byte count are equally consistent with an operation that is wasteful and one that is being handed inputs it should never have seen, and optimizing the wrong one buys a constant factor against something structural. Reach for the input sizes — elements walked, entries rewritten, candidates considered — before optimizing a hot span, and let the distribution choose the fix.

**A refusal is neither.** `note!` is the fourth call site macro and the one that measures nothing: it states *why* a decision went the way it did, for a path — a solver giving up, a level defaulting, a candidate rejected — that has no duration and no size worth reporting. It earns its place because the alternative is a silent refusal, and a refusal nobody can explain is what sends a reader back to `println!`. It belongs only on a path taken rarely: a note that fires on everything answers *why this one* about nothing.

**A magnitude, never a tally.** A `sample!` whose value is always the same number is a call counter in this instrument's clothes: it says nothing about the inputs, and the enclosing span already counts its calls and times them. Nine such sites once accounted for **97% of every event the prelude build emitted** — 14.5 million of them from one site inside `Term`'s equality — and one of the nine reported a count an existing span reported exactly. They were removed rather than filtered: a rule about what the instrument is for is worth more than a list of exceptions.

### The library emits records, and aggregation is one of its consumers

**Decision.** `trace` writes one row per span and event as it happens; nothing is summed in the process being measured. `fold` recomputes the aggregate from the rows, and every consumer that wants a summary — the CLI, `trace_build_script`, a measurement test — calls it.

**Rationale.** A capture that returns its report can only return it once the operation ends, so the runs most worth profiling — the ones that hang — produced nothing at all. Streaming inverts that: what a compile did is on disk as soon as it did it, and a run that has to be killed, or that crashes, leaves a file whose last rows name the span it was inside. The aggregate lost nothing in the move — every column it had is a difference of two rows — and it gained what no aggregate could hold: the order events happened in, the spans still open at the end, and any question thought of after the run rather than before it.

**Self time is one of those differences.** An inclusive row counts every span entered inside it, which is right for a stage and wrong for work that re-enters itself: a judgment that calls another judgment that calls the first counts the inner extent once per enclosing entry, so a row per judgment overstates each one and the rows cannot be added. The fold therefore also credits each exit's extent to the span it was entered inside, and reports what a span kept for itself, in time and in bytes — which needs no new row, because the order of the entries and exits is the nesting. It reads one stack per stream, which holds because the compiler is single-threaded by construction; an exit that is not the innermost entry keeps its whole extent and credits no parent, so a stream that did interleave threads keeps its inclusive figures and misattributes only its self ones.

**So are the costliest calls.** An aggregate says how long a span took in all and never which call took it, and a hunt for a pathological input asks exactly that. A span created with fields beyond its `group` has its costliest calls kept with those fields — a bounded number per row, so the fold stays a constant per span name — and the report lists them under its rows. Nothing new is recorded for it: a span's fields are already on its creation row, and keeping the costliest is one more thing a reader of the rows can compute.

The rows are tab-separated because the analysis should not need this crate: `awk '$1 == "V"'` is a whole query, and one file is readable by anything. A rotating pair bounds what an endless run can write, keeping the tail rather than the head, and each file restates the callsite table at its head so the survivor stands alone.

### The allocator counts process-wide, and this crate installs it

**Decision.** The counting allocator maintains process-wide live, cumulative, high-water and count figures, and this crate installs it as the `#[global_allocator]` of every binary it is linked into with `enabled` on. No binary names it.

**Rationale.** A `GlobalAlloc` cannot allocate the thread-local state per-thread attribution would need, and the stage pipelines the workspace profiles are single-threaded, so process-wide is precise where it is used and an overcount anywhere else. Installing it here is what makes a profile build count wherever it measures: a binary that had to opt in could forget, and the columns would then read zero — absent evidence rather than a failure, which nothing reports. The opt-in had become four copies of one static — the CLI, the prelude archive's build script, this crate's own test binary, and a fourth on its way into the certifying build script — and one site cannot drift. This crate's test binary is one of the binaries it is linked into, which keeps the accounting tests falsifiable: inverting the sign of `retained` was observed to pass them under the system allocator.

**What it costs.** Every binary built with `enabled` — which `--all-features` makes every test binary of the gate — pays a handful of relaxed atomic operations per allocation. A binary that wanted a different global allocator could not link `enabled`; none does.

**Rejected — each binary opts in.** It was how this landed, on the argument that the ordinary CLI keeps the system allocator; that holds here too, since `enabled` is off outside a profile build. What it bought beyond that was a place to forget.
