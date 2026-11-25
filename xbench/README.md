# xbench — where Curios's cost is, and whether it moved

The benchmark bench, and the owner of every measurement's record: `cargo xbench measure <MEASUREMENT>` takes a sitting and files it as the measurement's next record, and `cargo xbench report <MEASUREMENT>` computes what its records say. One measurement is the bench's own, the contest: Curios and Rust on every workload of the corpus, held to the answers those workloads are known to have and timed, a run-once-every-never comparison for orientation — "Curios is ~Nx off Rust on integer loops" — and **not** a rigorous benchmark suite. Every other is a function of the bench's, which the runner takes by name. What the bench answers rigorously is whether a figure moved, by the method [A measured figure is counted where it can be, and gated only where it repeats](../documentation/design/a-measured-figure-is-counted-where-it-can-be-and-gated-only-where-it-repeats.md) states: a record holds everything its sitting was taken under, nothing derived from a reading is stored, and a comparison the records cannot justify is refused rather than drawn.

A record is a historical fact and cannot be re-run. That is what separates this crate from [`xboard`](../xboard/README.md), whose witnesses go to the checkers on every run and fail where they disagree with their ticket. Nothing here regresses; the bench fails only where a filed record is malformed.

## Run it

```
xbench/
  src/lib.rs                      what a record is, the lines a measurement prints for one, and the harness that prints them
  src/main.rs                     the command line, and the records the build embeds
  src/measurements/               every measurement but the contest, a module by subject, each named once
  src/                            what a record is taken under, the contest, the runner, the report
  src/bin/twin.rs                 the runner `rust-wasm` runs under, on the engine the compiler embeds
  records/<measurement>/NN.toml   one record each, the file its sitting filed

../programs/
  lcg/ trees/ chain/ churn/ walk/ spines/   the timed programs, one directory per workload
```

```sh
cargo xbench measure contest                   # a sitting, filed as the contest's next record
cargo xbench report contest                    # every record of the contest, and what changed between them
cargo xbench report contest 00                 # one record in full
cargo xbench measure launcher_guard_positive_control   # another measurement, by its name
cargo xbench report launcher_guard_positive_control    # its records, figure by figure
```

`measure` files the record itself, once the sitting has ended, under the next number of its measurement and never over a file that stands, so a sitting that fails files nothing; the build log, the agreement check and the live tables go to standard error.

A measurement is a function of the bench's, named once in [`src/measurements.rs`](src/measurements.rs), that names each figure through this crate's library — [`read`](src/harness.rs), [`turn`](src/harness.rs) and [`phase`](src/harness.rs) — which prints each reading as a line under `xbench:`. A phase is a span the harness opens around a measurement's work, under the recorder: its time, its allocations and its looks are read off that span, as the spans inside it are, and its instructions at its two ends, so each quantity is read one way. A set a measurement reports is read a member a figure of one, and a map a key a figure of its count, so a record holds the whole of a survey and two records compare it member by member. The runner builds the bench in release, sits the measurement in it as many times as its table entry's spread asks — five where a figure moves from run to run, a time or an instruction count, and two where every figure is a count that repeats — a group a process of its own (`xbench sit <MEASUREMENT>`, a subcommand no one runs by hand), and files the figures those printed, each figure's readings by group.

Taking a sitting needs a checkout and `cargo`, and a tree that holds no change a commit does not: a record names the commit that holds what it measured, so `measure` refuses a tree with a change in it, staged, unstaged or untracked, before it builds anything. `rust-wasm` runs under `twin`, the bench's own runner, which links the `wasmtime` `Cargo.lock` resolves for the compiler's own, so the WebAssembly comparison is between two compilers on one engine by construction, and a record names that engine for each. `measure` builds the runner beside the compiler, and the runner precompiles each twin once, so an execution spends a load and a run as `curios`'s self-contained executable does. Everything else is already here: `rustc` and the `wasm32-wasip2` target come from the workspace's own `rust-toolchain.toml`. There is no image and nothing to install.

## Contestants

| Contestant | What it is | What it tells you |
| --- | --- | --- |
| **Curios** | a self-contained executable, reading its size from its arguments | the subject |
| **Rust (Native)** | `rustc -O`, native | the ceiling |
| **Rust (WebAssembly)** | the same source, `--target wasm32-wasip2`, precompiled and run by `twin` on the engine the compiler embeds | what WebAssembly itself costs |

Rust (Native) against Rust (WebAssembly) is one source compiled two ways, so what separates them is WebAssembly's own cost, and what remains between Curios and Rust (WebAssembly) is Curios's.

On a workload that allocates, Rust (WebAssembly) is a **bound** and the report names it one. Rust manages linear memory while Curios delegates collection to the engine, so that gap holds Curios's codegen and the whole cost of having a collector at all, and no contestant separates them. A WebAssembly-GC peer would, and none is reachable: WASI passes its arguments through linear memory while WebAssembly-GC values live on the engine's heap, so every WebAssembly-GC toolchain targets a JavaScript host. Curios runs standalone only because [`curios-abi`](../curios-abi/README.md) is its own host boundary. That question is answered inside Curios instead, by counts and by arrangements of the collector — `chain_collection_decomposition` in `curios/src/tests/codegen/churn.rs` is where it lives.

## Workloads

Six, one directory each, carrying the same program in Curios and in Rust. What each computes, its anchors and why its constants are small are [the corpus's to state](../programs/README.md).

What belongs here is the guarantee: before anything is timed, all three contestants are held to the answer each workload is known to have — at a small check size, and at the size it is timed at — and the run stops where one differs, so a mistranslation surfaces as a failed run rather than a wrong number.

## How a figure is read

A contestant is measured in **five groups of five executions**, one untimed round opening each group, with the contestants **interleaved round-robin inside a group** rather than run one at a time. Each execution is timed and its **instructions counted**: the count is opened before the process starts, so the process and everything it starts inherit it, and read once it has been waited for. Where the kernel opens no count, a sitting records its times alone.

- Within a group is iteration noise. Between the group medians is the **span**.
- A contestant's figure is the median of its group medians; its span is the least and greatest of them.
- Across two records, what is compared of times is **Curios over Rust (Native), group by group**: each group's Curios median over its native Rust median, read as a figure is. What is compared of instructions is Curios's own count, which no disturbance of the machine moves and so needs no control.
- A difference is **proven only where two spans are disjoint**. Overlapping spans could have produced each other, so the report says `no difference proven`.
- Every figure is stated to the place of the first significant digit of its half-span, so no number claims a precision its span does not support.

Interleaving is what makes the ratio survive a disturbance: a compile elsewhere on the machine, a thermal ramp or a cache warming reaches all three contestants alike when they run adjacent in time, and unevenly when each runs to exhaustion in turn.

## When a comparison is refused

| Differs between two records | Times | Instructions |
| --- | --- | --- |
| `machine` | refused, naming it | refused, naming it |
| `state` — governor, boost, SMT | refused, naming it | compared |
| `pinned` — rustc's, Wasmtime's and Binaryen's versions | refused, naming it | refused, naming it |
| a workload's size | that workload compared no further, naming both sizes on its row | the same |
| `software` — kernel, microcode | compared | compared |

A measurement beside its code has figures in other measures, each refused the same way, as its measure is gated (`Measure::gated`): bytes, allocations and an engine's collections across a pin alone, the standard library, the optimizer and the engine a pin builds each deciding theirs, and a count the compiler keeps of its own work — calls, looks, budget units, the bytes it alone emits, a sampled magnitude — across nothing. A refusal reaches only the unit it crosses, as [A measured figure is counted where it can be, and gated only where it repeats](../documentation/design/a-measured-figure-is-counted-where-it-can-be-and-gated-only-where-it-repeats.md) carries each: a machine's arrangement moves a time and no count, and another machine runs other code for one build, Cranelift compiling for the host's processor. Software is not gated because a patch that moves a figure moves Rust with Curios, and the ratio a comparison reads cancels it; gating on it would strand the record at every update for nothing.

## Design

### A measurement is the bench's, and its record leaves it on standard output

**Decision.** A measurement is a function of this crate, named once in its table of measurements, that drives the compiler through its public entries and the test support a crate keeps for another's tests, as [`xboard`](../xboard/README.md) drives the checkers, and names its figures through the harness of this crate's library, which prints each reading as a line under a fixed prefix; the code it measures keeps only its spans (decided by the user on 2026-11-25). The runner takes it by name, builds the bench in release, sits it in a process of its own a group, five groups where a figure moves from run to run and two where every figure repeats, as the measurement's entry states and the sitting's figures hold, and files what those processes printed as the measurement's next record, nothing where one fails.

**Rationale.** A measurement reaches its crate's test fixtures, not its private code, so the bench reaches what it did beside the code once those fixtures are the crate's test support, and one home holds every measurement, costs and surveys alike. Its record passes out of it as the lines it prints, as Go's benchmarks print [the Go benchmark format](https://go.googlesource.com/proposal/+/master/design/14313-benchmark-format.md) a reader files, so a run that fails files nothing and no path is spelled twice. A process is where what a run draws differs — its hash keys, its addresses, its restore of the prelude — so the span between groups is the span between processes, as [pyperf](https://pyperf.readthedocs.io/en/latest/run_benchmark.html) runs each of its readings' groups in a worker of its own; inside one, the harness turns the variants a measurement compares round-robin, so no fixed order puts one always first. Five groups are the fewest whose ranks separate two records entirely, which a comparison of a figure that moves needs; a count that repeats needs only to be seen repeating, which two groups show, so a survey of counts sits two groups rather than five (decided by the user on 2026-11-25). A debug build keeps checks the shipped compiler runs without, which move counts as well as times: a debug assertion peels a pair twice (`runaway_measurements`).

**Rejected.** A measurement as an ignored test beside its code: a test used as a tag, read through libtest's harness, with the bench's dependency in every crate holding one. A measurement as an example or a bench target beside its code: the same exposure of fixtures as the bench needs, and a binary each linking the whole compiler. Every measurement driving the shipped binary over `--profile`, rustc-perf's shape: the costs a scripted host or a skipped Binaryen reads, and every survey of structure, cannot reach it, which would leave measurements two homes; it stays open as an addition for the product's own costs. Costs in the bench and surveys beside their code, one concept in two homes. A record written to a file by the measurement, its path spelled in the harness and the runner and a failed run's predecessor filed as its own. Every group taken in one process, whose span never sees a second process's draws. A measurement built in the dev profile `cargo xbench` runs in, whose figures hold its checks.

### The harness is this crate's library

**Decision.** The record, the lines a measurement prints for one and the harness that prints them are this crate's library, which a measurement's crate takes as a dev-dependency beside `curios-profile`; the library reads the instruments — the recorder, its fold, `Instructions` — through `curios-profile`. This crate's own dependencies come with it into those crates' test builds, the twin runner's `wasmtime` features among them (decided by the user on 2026-11-25).

**Rationale.** The method — groups, an untimed round opening each, variants turned round-robin — is the contest's already, and a figure is what a record holds, so the writer and the reader of the lines are one crate; reading the instruments stays `curios-profile`'s, a crate the compiler ships, which holds nothing only a measurement calls.

**Rejected.** The harness in `curios-profile`, which states the contest's method a second time or takes it from the bench that applies it, and compiles what tests alone call into a crate the compiler ships. The harness in a crate of its own, which writes the lines in one crate and reads them in another. The twin runner moved to a crate of its own to keep its `wasmtime` features from the measurement crates' test builds.

### A record holds what it was taken under, and nothing derived from it

**Decision.** A record holds the compiler it is about, the machine, that machine's arrangement, the tool pins, and each figure of its sitting: its name within its measurement, the measure it is counted in, the size it was taken at, and every reading in the order it was taken. It holds no median, span, ratio or percentage: the report computes each when it is asked.

**Rationale.** A figure whose conditions are not recorded cannot be repeated, and a ratio across two such figures cannot be wrong, because nothing says what it is a ratio of. Keeping the samples rather than a summary of them is what lets the span — and so the whole question of whether anything moved — be computed at all.

**Rejected.** A record of a measurement beside its code in a format of its own beside the contest's, two shapes of record and two readers. The machine as a sentence written after the fact: nothing holds it to the sitting and nothing can compare two of them. A mean and a standard deviation over one batch: five samples summarised that way cannot tell a one percent move from nothing, and stating a ratio from them claims a precision the measurement does not have. Sizes read from the contest at print time rather than from the record: changing a workload's size would silently relabel what every earlier record measured.

### A record is a file the build embeds

**Decision.** A record is `records/<measurement>/<NN>.toml`, the file its sitting filed, its directory naming the measurement it is a sitting of: the day as a TOML local date, every reading an integer of its measure, whole microseconds for a time, and the pins and the figures as arrays in the order they were taken. The build embeds every file there, a measurement a directory, in the order of its number, and refuses a name that is not the next number of its measurement's, and a test holds each file to what its sitting files of the record it holds.

**Rationale.** A record is data, and a record kept as code is read by Rust alone: a field added rewrote every record, and each was listed by hand. A microsecond is an integer, so the record is exact where a decimal millisecond is a float a reader parses again, and TOML refuses a day its month does not have, which a date type of the bench's own held before. Embedding keeps a binary built at one commit reporting that commit's records. The measurement is its directory's name alone, so no path is spelled twice.

**Rejected.** A record as a Rust module, for the reasons above. Reading the directory where the bench runs, under which a binary built at one commit reports another's records. A sitting printed for a redirect to file, which leaves a file cut short where the sitting fails. The TOML printer's pretty form, which lays a nested array out unevenly where the plain form puts a group's samples on one line.

### Each execution is timed and counted

**Decision.** Each execution of a contestant is timed and its user-space instructions counted, the count opened before the process starts and inherited into it, and a record files both, a figure each.

**Rationale.** A count is read off the work, so it compares across a machine's arrangement a time is refused across; another machine runs other code for one build, so instructions are refused across a machine as a time is. A count opened before the process starts reaches every thread and process it starts, the engine's compiling threads among them.

**Rejected.** The contest kept timed alone, which leaves every sitting in another state without a comparison.

### A difference is proven only where two spans are disjoint

**Decision.** The span between group medians is the bar, and a movement under it is `no difference proven` rather than a number.

**Rationale.** It is the one standard that needs no judgement of ours, because the sitting measured it rather than asserting it. It assumes nothing about how wall-clock samples are distributed — they are skewed and serially correlated, so a test assuming otherwise over-claims — and it errs toward missing a real movement rather than announcing one, since a single bad batch widens a span and hides a change.

**Rejected.** A threshold written by hand, "ignore moves under two percent": a number someone believed, where the machine can measure the real one. Student's t over the samples, as `ministat` does: the samples are not independent and not normal, and the assumption buys a confidence level to argue about rather than an answer. Two groups rather than five: the span would be a single difference between two numbers instead of an estimate of one.

### A movement is read against Rust (Native)

**Decision.** Across two records the report compares Curios's time over native Rust's, taken group by group — each group's Curios median over that group's native Rust median — and a movement is proven where the two records' spans of that ratio are disjoint.

**Rationale.** The contestants run interleaved inside a group, so what moves the machine during a group or between two sittings — a kernel update, a thermal state, something else running — reaches both and cancels in the ratio, where Curios's time alone would carry it as a movement of the compiler. The bench this one replaces asked of every sitting, by hand in prose, whether the controls held still; the ratio answers that in the figure itself, on every workload, with nothing left for a person to remember to apply.

**Rejected.** Curios's time alone, which reports a movement of the machine as one of the compiler. Drawing no figure for a workload whose Rust moved past its span: it leaves the workload without a verdict, and a Rust span wide enough to hold still hides a shift that moved Curios with it. Reporting the movement with a caveat beside it: a caveat is read after the number, and the number is what gets quoted.

### The contestants are Curios and Rust, compiled two ways

**Decision.** Three outputs, from two sources.

**Rationale.** Those two carry every cross-language argument a record of the contest makes: a Curios figure is read against what WebAssembly costs Rust, and needs no third language. Every other contestant is a toolchain to install and keep steady, for a row no argument reads.

**Rejected.** Seven contestants: five never entered an argument, two were the noisiest rows in the harness, and one of them alone required an emulated build stage. A peer kept for the occasional finding that the paradigm is not the excuse — that is an investigation, run once and filed, not a toolchain installed forever.

### The bench runs where it is, and there is no image

**Decision.** `cargo xbench measure` runs on the host. The platform is recorded rather than standardised.

**Rationale.** With three contestants, the engine the bench's own runner, the image would install nothing, and it never made the platform uniform in any case — a container shares the host's kernel, CPU and governor. What makes two records comparable is the protocol each carries, which a container is not.

**Rejected.** The kitchen-sink image: it existed for the five contestants that are gone. Pinning the timed processes to one core: core 0 is the one interrupts favour, and holding one core while the rest of the machine runs free invites interference rather than excluding it.
