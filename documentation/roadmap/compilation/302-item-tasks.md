# A compilation is a graph of item tasks

**Not refined yet.** The next step is the serial baseline and graph model under *Readiness*, before changing the term representation or adding an executor.

Working specification for multithreaded compilation, the payoff of [a declaration being a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md). Once that holds, no artifact carries a minted identity ([a unit carries none another compilation could mint](../../../curios-unit/README.md#a-unit-carries-no-identity-another-compilation-could-mint)) and no verdict depends on history ([no memo outlives its declaration](../../design/soundness/a-reduction-step-costs-what-it-builds.md)), the items of a compilation can run on as many workers as the product supplies, in any order, and the determinism that property gives is what the gate holds them to. The compiler is single-threaded today by omission rather than by decision, and every design statement to the contrary is listed under what this overturns.

It needs that decision to hold, [A compile costs what it reads](../../design/compilation/a-compile-costs-what-it-reads.md) for the facets its workers write, the records that say which of them may run together and an erased form that is each declaration's own, and what they build on; [Cranelift's parallel compilation](../../../curios-cranelift/README.md#compilation-runs-across-threads-and-only-where-compilation-exists) is already its first parallelism.

## What this builds on

- **What already runs beside the compiler**: `wonder server`'s protocol thread, Binaryen behind its process lock, and the prelude images validated once per process.
- **A facet for each stage of a declaration, read through one environment** ([A compile costs what it reads](../../design/compilation/a-compile-costs-what-it-reads.md)): the recorded reads say which items may run together, a declaration's erased form is its own, so no arena is two workers', and a stored unit is addressed by who it is, so no order of units is part of what one is.
- **The graph admits a scheduling model, not a wall-time prediction.** Allocations can weight declarations for a counted list schedule, but do not measure elapsed time, executor overhead, atomic sharing, memory pressure or the serial optimizers and emitter. A declaration's signature is published with its body. [A signature is published ahead of its body](303-a-signature-is-published-ahead-of-its-body.md) owns an optional refinement for signatures that fix their own universe context; whole-declaration scheduling is the baseline and does not wait for it. The split model has no throughput claim until its weights and dependencies are defined as *Verification* requires.
- **Peers.** Lean 4.19 elaborates theorem bodies in parallel (lean4#7084). rustc parallelizes type checking per item under one build that selects serial or parallel synchronization at run time (`rustc_data_structures::sync`, `DynSend`/`DynSync`). Kontroli, a Rust checker for the λΠ-calculus modulo rewriting ([Färber, CPP 2022](https://arxiv.org/pdf/2102.08766)), measures a 28.2% single-thread runtime overhead for its atomic term representation on the HOL Light dataset, and up to 6.6× for type checking on eight threads on Isabelle/HOL after subtracting non-checking work. These are different workloads and measurements, and supply no expected overhead or end-to-end speedup for Curios. Rocq checks opaque proofs in worker processes (Barras, Tankink and Tassi, ITP 2015); GHC compiles modules in parallel under `-j`.

## The gap

**Nothing the compiler holds can cross a thread.** `Term` is an `Rc<Node>` whose caches are `Cell`s and `OnceCell`s; the surface trees are `Rc`-shared. `thread_local!` tables sit under that: the restored prelude (`curios-prelude/src/restore.rs`, restored again by every thread that compiles), the packrat memo, the comment table, the formatter's owed comments, and the shared empty free-variable set.

## Readiness

The lowering-record census of [A lowering is held to its recorded lookups](300-a-lowering-is-held-to-its-recorded-lookups.md) precedes scheduling from those records. Source identity is settled by [A library source is named by its package](301-a-library-source-is-named-by-its-package.md). The focused binder and term-type delivery of [The editor is answered at a position](../tools/300-the-editor-is-answered-at-a-position.md) precedes this architectural expansion.

Take demonstrated compiler-scaling violations first under the filing order, then the attributed escape-only query of [An optimizer query costs what its answer uses](200-an-optimizer-query-costs-what-its-answer-uses.md). That query waits on no bounded-copy consumer. Once those costs have moved, the bounded investigation here records current default-release elapsed time, allocation work, peak memory, critical-path weight and serial stage shares over `/std` and `programs/`. It ends with a rebuildable whole-declaration scheduling model and a protocol for measuring the representation change and executor against that serial baseline. No measured speedup is claimed before the implementation is compared with it.

## Permanent decisions

**The determinism contract.** A compilation's units, verdicts, diagnostics and emitted bytes are the same for every number of workers: [a declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md), held under a schedule. The schedule is never an input: not to a verdict, not to a minted identity, not to the order anything is reported in. That is also what keeps the worker count out of a stored unit's address.

**The execution strategy is the product's.** `curios-pipeline` declares an executor seam, as it declares `Cache`: `curios` supplies a work-stealing pool, and `curios-js` and the fixtures supply one worker. One worker is the same code, not a second path.

**One representation, shareable across threads.** Terms are `Arc`-shared, their scalar derivations computed when a node is built and their lazy derivations in `OnceLock`s. Its single-threaded cost is measured and reported, never assumed.

## The components

| Component | Treatment |
| --- | --- |
| Executor | Once-written facets on the product's pool. An item becomes ready when the declarations its lowering names are published; a read discovered while running — a chosen witness, a key, an unfolded body — of a facet not yet written voids the attempt, as it does on one worker, and the item is queued again behind the declaration it needs, which is started where no worker holds it. No worker waits on another, so nothing deadlocks, and a cycle is read off what each voided item needed and reported by its members in source order, which is the same report under any schedule |
| Parsing and lowering | Per file, then per module in two phases over the possibly cyclic module graph: collect every module's exports, then lower |
| Terms and trees | `Arc`-shared; `Cell` caches computed at construction, `OnceCell`s become `OnceLock`s |
| Per-thread tables | The prelude restored once per process; the packrat, comment and formatter tables become state of the parse that owns them; the parsed-file memo becomes the session's |
| Progress | Each report names its subject, since several are in flight |

## Stages

Each lands alone, on its own check.

1. **A term representation that crosses threads.** Needs the interned names a unit carries, so that the measurement includes what interning removes. Measured: the prelude build's wall time, the compile time of the corpus in `programs/` and peak memory on a release build without `profile`, before and after, with one worker. Use warm-ups and repeated interleaved baseline and candidate runs; report medians and spans with their conditions. Set the acceptable single-worker trade-off from these readings before proceeding to the executor; no percentage from another checker is assumed.
2. **The executor, and the gate holding one worker and many to the same bytes.** The seam and the two executors land first, with the differential; then, each switched on alone and measured: the kernel behind elaboration, elaboration by item, parsing and lowering by file and module, erasure by item, and the independent jobs — `curios test`'s library and executables, `format` and `lint` over their files, and `wonder`'s analysts.

## Verification

- At every stage, every verdict over `/std` and the corpus in `programs/` is compared with the compiler before the stage; a difference is a finding, never a fixture update.
- **The graph model.** The nodes are `/std`'s declarations, an item with the items generated from it, and an edge is a name `Module::reaches` gives for an item of one, declared by another, over the restored prelude's module. A declaration's weight is the allocations its `declaration` span made in `curios-prelude`'s build under `profile`. The protocol records the commit, grouping of generated items, edge extraction, weights, source-order tie breaking and critical path. Modeled throughput on n workers is summed weight divided by the finishing time of a list schedule that hands the lowest-positioned ready item to a free worker. Counted; no modeled factor is an elapsed-time speedup. An optional signature-split model additionally states which signatures can publish early, separate non-overlapping signature and body weights whose sum accounts for the declaration's work, and each task's dependencies. Compare both models at equal total work, explain any worse split schedule and distinguish list-schedule effects from a change in critical path before reporting a factor.
- The differential compiles `/std` and the corpus with one worker and with many, with cold and warm stores and under schedules varied by injected delays, and requires identical stored units, diagnostics and emitted bytes. Incremental edits and missing dependencies exercise retries and publication as well as successful full compiles.
- A genuine cycle between two items is reported with the same text under one worker and many.
- Each stage reports the time and peak memory it moved, named by the stage; the executor is compared with both the unchanged serial compiler and the one-worker shareable representation, so representation overhead and scheduling benefit remain separate.

## Design decisions this overturns or corrects

- [`curios-prelude`'s README](../../../curios-prelude/README.md#a-root-is-imaged-as-a-certified-unit): the images are restored once per process.
- `curios-wonder/src/server.rs`: its module documentation's *single-threaded by construction*.
- `curios-pipeline`: `Progress`'s *sequential by construction*.
- `curios-core/src/term/frees.rs`: the comment justifying a per-thread table by `Rc`.

## Rejected

- **Scheduling units rather than items.** The time is inside units — `/std` is one, and so is the program asked for — so a unit scheduler buys almost nothing now, and the item graph honours the unit graph without one.
- **Threads sharing nothing, as the end state.** Units crossing as archived bytes serve jobs as coarse as a whole compilation, restore the prelude per worker, and cannot share an environment between items. It is not a stage either: the one environment makes it unnecessary.
- **Two representations generic over their pointer**, Kontroli's answer: one representation spelled twice, kept in agreement by the type system rather than removed.

## Non-goals

Parallelizing the ANF and CPS optimizers or the emitter beyond Cranelift; caching per item in the store; compiling across processes or machines; sharing cores with an outer build through a job server.

## Completion and retirement

- A compilation is the same bytes for every number of workers, and the gate holds it.
- The prelude build and a program's compilation schedule ready independent work on the workers the product supplies, with measured end-to-end speedup, single-worker overhead, memory and remaining serial shares recorded. A dependency chain is not promised to occupy every worker.

Before this specification is deleted, its contracts are recorded in the owning crates' READMEs and rustdoc, its decision and its rejected alternatives are a design decision in `documentation/design/compilation/`, `.claude/rules/`' area routing names the executor, no reference to this filename remains.
