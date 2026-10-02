# xbench — where Curios's cost is, and whether it moved

The benchmark bench: `cargo xbench collect` builds Curios and Rust on every workload of the corpus, holds all three contestants to the answers those workloads are known to have, times them, and prints the sitting as the module that records it; `cargo xbench report` computes what the readings say. It is a run-once-every-never bench for orientation — "Curios is ~Nx off Rust on integer loops" — and **not** a rigorous benchmark suite. What makes it worth keeping is not its precision but its bookkeeping: a reading records everything it was taken under, nothing derived from a reading is stored, and a comparison the record cannot justify is refused rather than drawn.

A reading is a historical fact and cannot be re-run. That is what separates this crate from [`xboard`](../xboard/README.md), whose witnesses go to the checkers on every run and fail where they disagree with their ticket. Nothing here regresses; the bench fails only where a filed record is malformed.

## Run it

```
xbench/
  src/lib.rs and beside it    the library: what a reading is, what it is taken under, the contest, the report
  src/main.rs                 the binary: the command line
  src/readings.rs             every reading taken — none yet
  src/readings/reading_NN.rs  one reading each, as data

../programs/
  lcg/ trees/ chain/ churn/ walk/ spines/   the timed programs, one directory per workload
```

```sh
cargo xbench collect > xbench/src/readings/reading_00.rs   # then list the module in readings.rs
cargo xbench report                                        # every reading, and what changed between them
cargo xbench report 00                                     # one reading in full
```

Standard output carries the reading module alone, so a redirect files the capture and nothing else; the build log, the agreement check and the live tables go to standard error.

Taking a reading needs a checkout, `cargo`, and the `wasmtime` CLI at the version `Cargo.lock` resolves — `rust-wasm` runs on the CLI while `curios` runs on the engine the compiler embeds, so unless the two agree the WebAssembly comparison is between two engines rather than two compilers. A reading records both, and `collect` refuses before it builds anything where they differ. Everything else is already here: `rustc` and the `wasm32-wasip2` target come from the workspace's own `rust-toolchain.toml`. There is no image and nothing to install beyond the engine.

## Contestants

| Contestant | What it is | What it tells you |
| --- | --- | --- |
| **Curios** | a self-contained executable, reading its size on standard input | the subject |
| **Rust** | `rustc -O`, native | the ceiling |
| **Rust → wasm** | the same source, `--target wasm32-wasip2`, under the `wasmtime` CLI | what WebAssembly itself costs |

Rust native against Rust → wasm is one source compiled two ways, so what separates them is WebAssembly's own cost, and what remains between Curios and Rust → wasm is Curios's.

On a workload that allocates, Rust → wasm is a **bound** and the report names it one. Rust manages linear memory while Curios delegates collection to the engine, so that gap holds Curios's codegen and the whole cost of having a collector at all, and no contestant separates them. A WebAssembly-GC peer would, and none is reachable: WASI passes its arguments through linear memory while WebAssembly-GC values live on the engine's heap, so every WebAssembly-GC toolchain targets a JavaScript host. Curios runs standalone only because [`curios-abi`](../curios-abi/README.md) is its own host boundary. That question is answered inside Curios instead, by counts and by arrangements of the collector — `chain_collection_decomposition` in `curios/src/tests/codegen/churn.rs` is where it lives.

## Workloads

Six, one directory each, carrying the same program in Curios and in Rust. What each computes, its anchors and why its constants are small are [the corpus's to state](../programs/README.md).

What belongs here is the guarantee: before anything is timed, all three contestants are held to the answer each workload is known to have — at a small check size, and at the size it is timed at — and the run stops where one differs, so a mistranslation surfaces as a failed run rather than a wrong number.

## How a figure is read

A contestant is measured in **five groups of five executions**, one untimed round opening each group, with the contestants **interleaved round-robin inside a group** rather than run one at a time.

- Within a group is iteration noise. Between the group medians is the **span**.
- A contestant's figure is the median of its group medians; its span is the least and greatest of them.
- A difference is **proven only where two spans are disjoint**. Overlapping spans could have produced each other, so the report says `no difference proven`.
- Every figure is stated to the place of the first significant digit of its half-span, so no number claims a precision its span does not support.

Interleaving is what makes the ratio survive a disturbance: a compile elsewhere on the machine, a thermal ramp or a cache warming reaches all three contestants alike when they run adjacent in time, and unevenly when each runs to exhaustion in turn.

## What a build weighs

A reading also records what each contestant's build weighs, taken by stating the file rather than by running it. That figure is **deterministic** under the pins — same compilers, same sources, same weight — so it needs no span, no warmup and no control: a difference between two readings is real, and the report states it as itself.

It is read **down a column, never across one**. The Curios contestant is a self-contained executable with the engine compiled into it, so its weight is the engine's with the program's on top, while `rust` is an ordinary native binary and `rust-wasm` a module. Putting two of them side by side would compare three different kinds of file. What a column answers is whether one contestant's build grew between two readings.

This is the whole of the counted evidence for now. What it cannot see is work: a loop body executed a hundred million times weighs what it weighs. The figures that answer *where the work goes* — fuel consumed, allocations, collections — need the program to run and report on itself, which reaches `curios-runtime`'s engine configuration and the command line, and is deliberately left to its own change.

## When a comparison is refused

| Differs between two readings | What the report does |
| --- | --- |
| `machine` | refuses, naming it |
| `state` — governor, boost, SMT | refuses, naming it |
| `pinned` — the tool versions | refuses, naming it |
| `sizes` | refuses, naming it |
| `software` — kernel, microcode | notes it and compares |
| the **control** moved past its span | draws no Curios figure for that workload |

Software is not gated because a patch rarely moves a figure past its span, and where it does the control moves with it and the comparison is disqualified anyway; gating on it would strand the record at every update while catching nothing the control does not.

## Design

### A reading records what it was taken under, and nothing derived from it

**Decision.** A reading carries the compiler it is about, the machine, that machine's arrangement, the tool pins, the size of every workload, and every sample in the order it ran. It carries no median, span, ratio or percentage: the report computes each when it is asked.

**Rationale.** A figure whose conditions are not recorded cannot be repeated, and a ratio across two such figures cannot be wrong, because nothing says what it is a ratio of. Keeping the samples rather than a summary of them is what lets the span — and so the whole question of whether anything moved — be computed at all.

**Rejected.** The machine as a sentence written after the fact: nothing holds it to the sitting and nothing can compare two of them. A mean and a standard deviation over one batch: five samples summarised that way cannot tell a one percent move from nothing, and stating a ratio from them claims a precision the measurement does not have. Sizes read from the contest at print time rather than from the reading: changing a workload's size would silently relabel what every earlier reading measured.

### A difference is proven only where two spans are disjoint

**Decision.** The span between group medians is the bar, and a movement under it is `no difference proven` rather than a number.

**Rationale.** It is the one standard that needs no judgement of ours, because the sitting measured it rather than asserting it. It assumes nothing about how wall-clock samples are distributed — they are skewed and serially correlated, so a test assuming otherwise over-claims — and it errs toward missing a real movement rather than announcing one, since a single bad batch widens a span and hides a change.

**Rejected.** A threshold written by hand, "ignore moves under two percent": a number someone believed, where the machine can measure the real one. Student's t over the samples, as `ministat` does: the samples are not independent and not normal, and the assumption buys a confidence level to argue about rather than an answer. Two groups rather than five: the span would be a single difference between two numbers instead of an estimate of one.

### The control can disqualify a comparison

**Decision.** Rust is the control. Where it moved past its own span between two readings, the report draws no Curios figure for that workload and says so.

**Rationale.** It is the only reasoning the bench this one replaces ever produced that was worth the harness, and it was done by hand in prose every sitting — "the controls did **not** hold still… no attribution should be drawn." A rule a person must remember to apply is the perimeter before it was a board. It also covers what nothing else can: a kernel change, a thermal state, a machine running something else all move the control, so the check detects them without the record having to anticipate them.

**Rejected.** Reporting the movement with a caveat beside it: a caveat is read after the number, and the number is what gets quoted.

### The contestants are Curios and Rust, compiled two ways

**Decision.** Three outputs, from two sources.

**Rationale.** Those two carry every cross-language argument the old bench's ten runs actually made; the reasoning that read a Curios figure against what WebAssembly costs Rust needs no third language. Dropping the rest removes five toolchains, an entire cross-architecture build stage, and the only contestant whose steady state could not be assumed.

**Rejected.** Seven contestants: five never entered an argument, two were the noisiest rows in the harness, and one of them alone required an emulated build stage. A peer kept for the occasional finding that the paradigm is not the excuse — that is an investigation, run once and filed, not a toolchain installed forever.

### The bench runs where it is, and there is no image

**Decision.** `cargo xbench collect` runs on the host. The platform is recorded rather than standardised.

**Rationale.** With three contestants the image would install the engine and nothing else, and it never made the platform uniform in any case — a container shares the host's kernel, CPU and governor. What makes two readings comparable is the protocol each carries, which a container is not.

**Rejected.** The kitchen-sink image: it existed for the five contestants that are gone. Pinning the timed processes to one core: core 0 is the one interrupts favour, and holding one core while the rest of the machine runs free invites interference rather than excluding it.

### The bench begins with no reading

**Decision.** `READINGS` starts empty. The runs a vanished harness took are not carried in.

**Rationale.** It is the board's rule — it began with no ticket, because a flaw never seen admitted by a run of the board says nothing. No earlier run was taken under a protocol this bench records: none states its sizes, its sample count or its warmup, none kept its samples, and each was taken on a machine described in prose. Not one could be compared with a reading taken now, so carrying them in would furnish the record with rows the report must always refuse.

**Rejected.** Keeping them as an archive, marked as taken under an unrecorded protocol: it carries ten readings of unusable data forever and every reader has to be told why they are there.
