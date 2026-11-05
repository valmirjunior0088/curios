# xbench — where Curios's cost is, and whether it moved

The benchmark bench: `cargo xbench collect` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them, and prints the sitting as the TOML file that records it; `cargo xbench report` computes what the readings say. It is a run-once-every-never bench for orientation — "Curios is ~Nx off Rust on integer loops" — and **not** a rigorous benchmark suite. What makes it worth keeping is not its precision but its bookkeeping: a reading records everything it was taken under, nothing derived from a reading is stored, and a comparison the record cannot justify is refused rather than drawn.

A reading is a historical fact and cannot be re-run. That is what separates this crate from [`xboard`](../xboard/README.md), whose witnesses go to the checkers on every run and fail where they disagree with their ticket. Nothing here regresses; the bench fails only where a filed record is malformed.

## Run it

```
xbench/
  src/main.rs        the command line, and the readings the build embeds
  src/               what a reading is, what it is taken under, the contest, the report
  readings/NN.toml   one reading each, the file `collect` printed

../programs/
  lcg/ trees/ chain/ churn/ walk/ spines/   the timed programs, one directory per workload
```

```sh
cargo xbench collect > xbench/readings/00.toml   # filed under the next number, which collect names
cargo xbench report                              # every reading, and what changed between them
cargo xbench report 00                           # one reading in full
```

Standard output carries the reading alone, so a redirect files the capture and nothing else; the build log, the agreement check and the live tables go to standard error.

Taking a reading needs a checkout, `cargo`, and the `wasmtime` CLI at the version `Cargo.lock` resolves — `rust-wasm` runs on the CLI while `curios` runs on the engine the compiler embeds, so unless the two agree the WebAssembly comparison is between two engines rather than two compilers. A reading records both, and `collect` refuses before it builds anything where they differ. Everything else is already here: `rustc` and the `wasm32-wasip2` target come from the workspace's own `rust-toolchain.toml`. There is no image and nothing to install beyond the engine.

## Contestants

| Contestant | What it is | What it tells you |
| --- | --- | --- |
| **Curios** | a self-contained executable, reading its size from its arguments | the subject |
| **Rust (Native)** | `rustc -O`, native | the ceiling |
| **Rust (WebAssembly)** | the same source, `--target wasm32-wasip2`, under the `wasmtime` CLI | what WebAssembly itself costs |

Rust (Native) against Rust (WebAssembly) is one source compiled two ways, so what separates them is WebAssembly's own cost, and what remains between Curios and Rust (WebAssembly) is Curios's.

On a workload that allocates, Rust (WebAssembly) is a **bound** and the report names it one. Rust manages linear memory while Curios delegates collection to the engine, so that gap holds Curios's codegen and the whole cost of having a collector at all, and no contestant separates them. A WebAssembly-GC peer would, and none is reachable: WASI passes its arguments through linear memory while WebAssembly-GC values live on the engine's heap, so every WebAssembly-GC toolchain targets a JavaScript host. Curios runs standalone only because [`curios-abi`](../curios-abi/README.md) is its own host boundary. That question is answered inside Curios instead, by counts and by arrangements of the collector — `chain_collection_decomposition` in `curios/src/tests/codegen/churn.rs` is where it lives.

## Workloads

Six, one directory each, carrying the same program in Curios and in Rust. What each computes, its anchors and why its constants are small are [the corpus's to state](../programs/README.md).

What belongs here is the guarantee: before anything is timed, all three contestants are held to the answer each workload is known to have — at a small check size, and at the size it is timed at — and the run stops where one differs, so a mistranslation surfaces as a failed run rather than a wrong number.

## How a figure is read

A contestant is measured in **five groups of five executions**, one untimed round opening each group, with the contestants **interleaved round-robin inside a group** rather than run one at a time.

- Within a group is iteration noise. Between the group medians is the **span**.
- A contestant's figure is the median of its group medians; its span is the least and greatest of them.
- Across two readings, what is compared is **Curios over Rust (Native), group by group**: each group's Curios median over its native Rust median, read as a figure is.
- A difference is **proven only where two spans are disjoint**. Overlapping spans could have produced each other, so the report says `no difference proven`.
- Every figure is stated to the place of the first significant digit of its half-span, so no number claims a precision its span does not support.

Interleaving is what makes the ratio survive a disturbance: a compile elsewhere on the machine, a thermal ramp or a cache warming reaches all three contestants alike when they run adjacent in time, and unevenly when each runs to exhaustion in turn.

## When a comparison is refused

| Differs between two readings | What the report does |
| --- | --- |
| `machine` | refuses, naming it |
| `state` — governor, boost, SMT | refuses, naming it |
| `pinned` — the tool versions | refuses, naming it |
| a workload's size | compares that workload no further, naming both sizes on its row |
| `software` — kernel, microcode | compares |

Software is not gated because a patch that moves a figure moves Rust with Curios, and the ratio a comparison reads cancels it; gating on it would strand the record at every update for nothing.

## Design

### A reading records what it was taken under, and nothing derived from it

**Decision.** A reading carries the compiler it is about, the machine, that machine's arrangement, the tool pins, the size of every workload, and every sample in the order it ran. It carries no median, span, ratio or percentage: the report computes each when it is asked.

**Rationale.** A figure whose conditions are not recorded cannot be repeated, and a ratio across two such figures cannot be wrong, because nothing says what it is a ratio of. Keeping the samples rather than a summary of them is what lets the span — and so the whole question of whether anything moved — be computed at all.

**Rejected.** The machine as a sentence written after the fact: nothing holds it to the sitting and nothing can compare two of them. A mean and a standard deviation over one batch: five samples summarised that way cannot tell a one percent move from nothing, and stating a ratio from them claims a precision the measurement does not have. Sizes read from the contest at print time rather than from the reading: changing a workload's size would silently relabel what every earlier reading measured.

### A reading is a file the build embeds

**Decision.** A reading is `readings/<NN>.toml`, the file `collect` printed: the day as a TOML local date, every sample in whole microseconds, and the pins, the sizes and the timings as arrays in the order they were taken. The build embeds every file there in the order of its number and refuses a name that is not the next number, and a test holds each file to what `collect` prints of the reading it holds.

**Rationale.** A reading is data, and a record kept as code is read by Rust alone: a field added rewrote every reading, and each was listed by hand. A microsecond is an integer, so the record is exact where a decimal millisecond is a float a reader parses again, and TOML refuses a day its month does not have, which a date type of the bench's own held before. Embedding keeps a binary built at one commit reporting that commit's readings.

**Rejected.** A reading as a Rust module, for the reasons above. Reading the directory where the bench runs, under which a binary built at one commit reports another's readings. The TOML printer's pretty form, which lays a nested array out unevenly where the plain form puts a group's samples on one line.

### A difference is proven only where two spans are disjoint

**Decision.** The span between group medians is the bar, and a movement under it is `no difference proven` rather than a number.

**Rationale.** It is the one standard that needs no judgement of ours, because the sitting measured it rather than asserting it. It assumes nothing about how wall-clock samples are distributed — they are skewed and serially correlated, so a test assuming otherwise over-claims — and it errs toward missing a real movement rather than announcing one, since a single bad batch widens a span and hides a change.

**Rejected.** A threshold written by hand, "ignore moves under two percent": a number someone believed, where the machine can measure the real one. Student's t over the samples, as `ministat` does: the samples are not independent and not normal, and the assumption buys a confidence level to argue about rather than an answer. Two groups rather than five: the span would be a single difference between two numbers instead of an estimate of one.

### A movement is read against Rust (Native)

**Decision.** Across two readings the report compares Curios's time over native Rust's, taken group by group — each group's Curios median over that group's native Rust median — and a movement is proven where the two readings' spans of that ratio are disjoint.

**Rationale.** The contestants run interleaved inside a group, so what moves the machine during a group or between two sittings — a kernel update, a thermal state, something else running — reaches both and cancels in the ratio, where Curios's time alone would carry it as a movement of the compiler. The bench this one replaces asked of every sitting, by hand in prose, whether the controls held still; the ratio answers that in the figure itself, on every workload, with nothing left for a person to remember to apply.

**Rejected.** Curios's time alone, which reports a movement of the machine as one of the compiler. Drawing no figure for a workload whose Rust moved past its span: it leaves the workload without a verdict, and a Rust span wide enough to hold still hides a shift that moved Curios with it. Reporting the movement with a caveat beside it: a caveat is read after the number, and the number is what gets quoted.

### The contestants are Curios and Rust, compiled two ways

**Decision.** Three outputs, from two sources.

**Rationale.** Those two carry every cross-language argument a reading makes: a Curios figure is read against what WebAssembly costs Rust, and needs no third language. Every other contestant is a toolchain to install and keep steady, for a row no argument reads.

**Rejected.** Seven contestants: five never entered an argument, two were the noisiest rows in the harness, and one of them alone required an emulated build stage. A peer kept for the occasional finding that the paradigm is not the excuse — that is an investigation, run once and filed, not a toolchain installed forever.

### The bench runs where it is, and there is no image

**Decision.** `cargo xbench collect` runs on the host. The platform is recorded rather than standardised.

**Rationale.** With three contestants the image would install the engine and nothing else, and it never made the platform uniform in any case — a container shares the host's kernel, CPU and governor. What makes two readings comparable is the protocol each carries, which a container is not.

**Rejected.** The kitchen-sink image: it existed for the five contestants that are gone. Pinning the timed processes to one core: core 0 is the one interrupts favour, and holding one core while the rest of the machine runs free invites interference rather than excluding it.
