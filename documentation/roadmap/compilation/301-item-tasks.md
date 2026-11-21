# A compilation is a graph of item tasks

**Not refined yet.** The next step is the serial baseline and graph model under *Readiness*, a bounded investigation nothing holds back, before changing the term representation or adding an executor: no specification numbered from `100` or `200` is open in any area, so no demonstrated compiler-scaling violation waits ahead of it.

Working specification for multithreaded compilation, the payoff of [a declaration being a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md). Once that holds, no artifact carries a minted identity ([a unit carries none another compilation could mint](../../../curios-unit/README.md#a-unit-carries-no-identity-another-compilation-could-mint)) and no verdict depends on history ([no memo outlives its declaration](../../design/soundness/a-reduction-step-costs-what-it-builds.md)), the items of a compilation can run on as many workers as the product supplies, in any order, and the determinism that property gives is what the gate holds them to. The compiler is single-threaded today by omission rather than by decision, and every design statement to the contrary is listed under what this overturns.

## What this builds on

- **What already runs beside the compiler**: [Cranelift's parallel compilation](../../../curios-cranelift/README.md#compilation-runs-across-threads-and-only-where-compilation-exists), its first parallelism, `wonder --server`'s protocol thread, Binaryen behind its process lock, and the prelude images validated once per process.
- **Names already cross threads**: qualifiers and symbols are interned once per process ([`curios-common`'s decision](../../../curios-common/README.md#qualifiers-and-symbols-are-interned-once-per-process)).
- **A facet for each stage of a declaration, read through one environment** ([A compile costs what it reads](../../design/compilation/a-compile-costs-what-it-reads.md)): the recorded reads say which items may run together, a declaration's erased form is its own, so no arena is two workers', and a stored unit is addressed by who it is, so no order of units is part of what one is.
- **Peers.** Lean 4.19 elaborates theorem bodies in parallel with one another and with the rest of elaboration (lean4#7084, [its release notes](https://lean-lang.org/doc/reference/4.26.0/releases/v4.19.0/)). rustc parallelizes type checking per item under one build that selects serial or parallel synchronization at run time (`rustc_data_structures::sync`, `DynSend`/`DynSync`); run on one thread it is "typically 0% to 2% slower" than the serial front end ([the 2023 announcement](https://blog.rust-lang.org/2023/11/09/parallel-rustc/)), and its 2025 project goal reports 20 to 30% and more off whole compiles on many threads, with contention past sixteen left to remove ([the goal](https://goals.rust-lang.org/2025h2/parallel-front-end.html)). Zig's compiler keeps a part of its intern pool per thread beside power-of-two shards (`src/InternPool.zig`), and a compilation's state for each thread apart from what they share (`src/Zcu/PerThread.zig`): the split this draws for the elaborator's context. Kontroli, a Rust checker for the λΠ-calculus modulo rewriting ([Färber, CPP 2022](https://arxiv.org/pdf/2102.08766)), measures a 28.2% single-thread runtime overhead for its atomic term representation on the HOL Light dataset, and up to 6.6× for type checking on eight threads on Isabelle/HOL after subtracting non-checking work. These are different workloads and measurements, and supply no expected overhead or end-to-end speedup for Curios. Rocq checks opaque proofs in worker processes (Barras, Tankink and Tassi, ITP 2015); GHC compiles modules in parallel under `-j`.

## The gap

**Nothing the compiler holds can cross a thread.** `Term` is an `Rc<Node>` whose caches are `Cell`s and `OnceCell`s; the surface trees are `Rc`-shared. `thread_local!` tables sit under that: the restored prelude (`curios-prelude/src/restore.rs`, restored again by every thread that compiles), the packrat memo, the comment table, the parsed-file memo (`curios-module/src/root_source.rs`), the archived pointer an extent read last (`curios-core/src/extent.rs`), and the shared empty set of mentions (`curios-core/src/term/mentions.rs`). The look count (`take_looks`, `curios-core/src/term.rs`), counted in every build, is per thread as well, and is no obstacle: its readers, `curios-core`'s sharing tests, walk on the thread they read it on.

**One context elaborates every declaration of a unit.** `curios-elab`'s `Context` holds in one value what an attempt at a declaration replaces as it begins (`Context::attempt`: its identities, metavariables, universe levels and caches, `curios-elab/src/context/typing.rs`) and what every declaration reads once another has published it (`Program`, `curios-elab/src/context/program.rs`: the registry's inductive, struct and concept entries, the witness table, totality, variances and value sorts), written through `&mut self` as each item publishes (`Context::publish`). A worker needs a context of its own over one environment that every worker reads and each declaration writes once.

**The instruments read one thread.** `curios-profile`'s fold credits a span's self time off one stack for the stream, "because the spans a stream records nest on one thread" (`curios-profile/src/fold.rs`), and its allocator counts for the whole process, "precise for the single-threaded stage pipelines the workspace profiles" (`curios-profile/src/count.rs`): a stream recorded from several workers keeps its inclusive figures and misattributes the rest, and a span's allocations are every worker's.

## Readiness

Each declaration's lowering is held to its record and to its answers by a census ([A lowering is kept where its text and the names it resolved stand](../../design/compilation/a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md)), which scheduling from those records rests on. Source identity is settled by [A source is named by its package and read from its file](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md).

Take demonstrated compiler-scaling violations first under the filing order. Once those costs have moved, the bounded investigation here records current default-release elapsed time, allocation work, peak memory, critical-path weight and serial stage shares over `/std` and `programs/`. It ends with a rebuildable whole-declaration scheduling model and a protocol for measuring the representation change and executor against that serial baseline. No measured speedup is claimed before the implementation is compared with it. Publishing a signature ahead of its body is [a specification of its own](302-a-signature-is-published-ahead-of-its-body.md), which this model releases; whole-declaration scheduling does not wait for it.

**The transcript holds several open subjects already** ([A transcript is drawn plain or live](300-a-transcript-is-drawn-plain-or-live.md)): a worker sends the events the sequential compiler sends, each naming its subject, and both drawings show what is open as they do on one worker.

## Permanent decisions

**The determinism contract.** A compilation's units, verdicts, diagnostics and emitted bytes are the same for every number of workers: [a declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md), held under a schedule. The schedule is never an input: not to a verdict, not to a minted identity, not to the order any report is written in. That is also what keeps the worker count out of a stored unit's address. The transcript's plain drawing is no report: it is a log of events as they happen, every line naming its subject, so its order is the schedule's and needs to be no other.

**The execution strategy is the product's.** `curios-pipeline` declares an executor seam, as it declares `Cache`: `curios` supplies a work-stealing pool, rayon, which Wasmtime's parallel compilation already links into the native product ([`curios-cranelift`'s decision](../../../curios-cranelift/README.md#compilation-runs-across-threads-and-only-where-compilation-exists)), so no crate is added; `curios-js` and the fixtures supply one worker. One worker is the same code, not a second path.

**One representation, shareable across threads.** Terms are `Arc`-shared, and their derivations are filled once whichever worker asks first. Its single-threaded cost is measured and reported, never assumed.

## The components

| Component | Treatment |
| --- | --- |
| Executor | Once-written facets on the product's pool. An item becomes ready when the declarations its lowering names are published; a read discovered while running — a chosen witness, a key, an unfolded body — of a facet not yet written voids the attempt, as it does on one worker, and the item is queued again behind the declaration it needs, which is started where no worker holds it. No worker waits on another, so nothing deadlocks, and a cycle is read off what each voided item needed and reported by its members in source order, which is the same report under any schedule |
| Parsing and lowering | Per file, then per module in two phases over the possibly cyclic module graph: collect every module's exports, then lower |
| Terms and trees | `Arc`-shared; `OnceCell`s become `OnceLock`s. The scalar caches (`ScalarCache`, `curios-core/src/term/scalars.rs`, two `Cell<u64>`s filled by a post-order walk on first demand) are open: computed at construction, a node restored from an image, which skips its caches, pays for them where it is restored; kept lazy in atomics, the hash stored before the word whose bit says it is filled, with release and acquire, two workers racing to fill one node write the same bits, a derivation being a function of its node |
| Elaborator's context | A context per worker over one published environment: what an attempt replaces is the worker's own, and the entries a declaration publishes are written once, as it publishes, and read by every worker; the witness table, registered from every witness's key before any item elaborates, is read and never written while items run |
| Per-thread tables | The prelude restored once per process; the packrat and comment tables become state of the parse that owns them; the parsed-file memo and an extent's last read become the session's; the look count stays per thread |
| Profiling | Each row names its thread and the fold keeps a stack per thread; allocations are counted per thread, which needs a thread's count the allocator can bump without allocating: a `const` thread-local of a `Cell<u64>` registers no destructor and takes no allocation, against `curios-profile`'s premise that per-thread attribution needs one, which is shown before it is relied on |
| Progress and transcript | Each event names its subject, and both drawings hold any number of subjects open ([A transcript is drawn plain or live](300-a-transcript-is-drawn-plain-or-live.md)); the live drawing streams the oldest open subject, so whether a worker's unit opens in the fold's order, or its events carry the fold's position, is settled with the executor, keeping the scrollback's order the fold's |

## Stages

Each lands alone, on its own check.

1. **A term representation that crosses threads.** Measured: the prelude build's wall time, the compile time of the corpus in `programs/` and peak memory on a release build, before and after, with one worker. Use warm-ups and repeated interleaved baseline and candidate runs; report medians and spans with their conditions. Set the acceptable single-worker trade-off from these readings before proceeding to the executor; no percentage from another checker is assumed.
2. **The executor, and the gate holding one worker and many to the same bytes.** The seam and the two executors land first, with the differential; then, each switched on alone and measured: the certifier behind elaboration, elaboration by item on a context per worker, parsing and lowering by file and module, erasure by item, and the independent jobs — `curios test`'s library and executables, `format` and `lint` over their files, and `wonder`'s analysts.

## Verification

- At every stage, every verdict over `/std` and the corpus in `programs/` is compared with the compiler before the stage; a difference is a finding, never a fixture update.
- **The graph model.** The nodes are `/std`'s declarations, an item with the items generated from it, and an edge is a name `Module::reaches` gives for an item of one, declared by another, over the restored prelude's module. A declaration's weight is the allocations its `declaration` span made in the stream of `curios-prelude-build`'s `profile` (`cargo xtask profile prelude`), a build script recording nothing. The protocol records the commit, grouping of generated items, edge extraction, weights, source-order tie breaking and critical path. Modeled throughput on n workers is summed weight divided by the finishing time of a list schedule that hands the lowest-positioned ready item to a free worker. Counted; no modeled factor is an elapsed-time speedup, since allocations measure no elapsed time, executor overhead, atomic sharing, memory pressure, or the serial optimizers and emitter.
- The differential compiles `/std` and the corpus with one worker and with many, with cold and warm cabinets and under schedules varied by injected delays, and requires identical stored units, diagnostics and emitted bytes. Incremental edits and missing dependencies exercise retries and publication as well as successful full compiles.
- A genuine cycle between two items is reported with the same text under one worker and many.
- Each stage reports the time and peak memory it moved, named by the stage; the executor is compared with both the unchanged serial compiler and the one-worker shareable representation, so representation overhead and scheduling benefit remain separate.

## Design decisions this overturns or corrects

- [`curios-prelude`'s README](../../../curios-prelude/README.md#a-root-is-imaged-as-a-certified-unit): the images are restored once per process.
- `curios-wonder/src/server.rs`: its module documentation's *single-threaded by construction*.
- `curios-core/src/term/mentions.rs`: `Mentions::empty`'s comment justifying a per-thread table by `Rc`.
- [`curios-profile`'s README](../../../curios-profile/README.md), with `count.rs` and `fold.rs`: allocations counted for the whole process as "precise for the single-threaded stage pipelines", per-thread attribution needing an allocation, and one stack for a stream because "the compiler is single-threaded by construction".

## Rejected

- **Scheduling units rather than items.** The time is inside units — `/std` is one, and so is the program asked for — so a unit scheduler buys almost nothing now, and the item graph honours the unit graph without one.
- **Threads sharing nothing, as the end state.** Units crossing as archived bytes serve jobs as coarse as a whole compilation, restore the prelude per worker, and cannot share an environment between items. It is not a stage either: the one environment makes it unnecessary.
- **Two representations generic over their pointer**, Kontroli's answer: one representation spelled twice, kept in agreement by the type system rather than removed.
- **A worker that blocks on another's item**, as Salsa's runtime does, parking a thread that asks a query another runs and keeping a graph of blocked threads to find a cycle across them ([the cycles chapter](https://docs.rs/crate/salsa/0.26.1/source/book/src/plumbing/cycles.md)): a void attempt and a requeue need neither, and the elaborator voids already.

## Non-goals

Parallelizing the ANF and CPS optimizers or the emitter beyond Cranelift; caching per item in the cabinet; compiling across processes or machines; sharing cores with an outer build through a job server.

## Completion and retirement

- A compilation is the same bytes for every number of workers, and the handoff holds it.
- The prelude build and a program's compilation schedule ready independent work on the workers the product supplies, with measured end-to-end speedup, single-worker overhead, memory and remaining serial shares recorded. A dependency chain is not promised to occupy every worker.

Before this specification is deleted, its contracts are recorded in the owning crates' READMEs and rustdoc, its decision and its rejected alternatives are a design decision in `documentation/design/compilation/`, `.claude/rules/`' area routing names the executor, no reference to this filename remains.
