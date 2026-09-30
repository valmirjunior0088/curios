# A compilation is a graph of item tasks

Working specification for multithreaded compilation, the payoff of [one environment](02-one-environment.md). Once every read goes through that environment, no artifact carries a minted identity ([a unit carries none another compilation could mint](../../../curios-unit/README.md#a-unit-carries-no-identity-another-compilation-could-mint)) and no verdict depends on history ([no memo outlives its declaration](../../design/soundness/a-reduction-step-costs-what-it-builds.md)), the items of a compilation can run on as many workers as the product supplies, and the determinism those establish is what the gate holds them to. The compiler is single-threaded today by omission rather than by decision, and every design statement to the contrary is listed under what this overturns.

It needs [one environment](02-one-environment.md) and what that builds on, and not [checked evidence](../01-soundness/03-checked-evidence.md); [Cranelift's parallel compilation](../../../curios-runtime/README.md#compilation-runs-across-threads-and-only-where-compilation-exists) is already its first parallelism.

## What this builds on

- **What already runs beside the compiler**: `wonder server`'s protocol thread, Binaryen behind its process lock, and the prelude images validated once per process.
- **The one environment**, whose recorded graph says which items may run together, and the critical path it measured.
- **Peers.** Lean 4.19 elaborates theorem bodies in parallel (lean4#7084). rustc parallelizes type checking per item under one build that selects serial or parallel synchronization at run time (`rustc_data_structures::sync`, `DynSend`/`DynSync`). Kontroli, a Rust checker for the λΠ-calculus modulo rewriting (Färber, CPP 2022), measured `Arc` terms 28.2% slower than `Rc` on one thread, won already at two threads, and reached 6.6× at eight. Rocq checks opaque proofs in worker processes (Barras, Tankink and Tassi, ITP 2015); GHC compiles modules in parallel under `-j`.

## The gap

**Nothing the compiler holds can cross a thread.** `Term` is an `Rc<Node>` whose caches are `Cell`s and `OnceCell`s; the surface trees are `Rc`-shared. `thread_local!` tables sit under that: the restored prelude (`curios-prelude-archive/src/restore.rs`, restored again by every thread that compiles), the packrat memo, the comment table, the formatter's owed comments, and the shared empty free-variable set.

**The erased arena is cumulative from the first unit**, and a stored unit is addressed by the ordered list of its predecessors.

## Permanent decisions

**The determinism contract.** A compilation's units, verdicts, diagnostics and emitted bytes are the same for every number of workers. The schedule is never an input: not to a verdict, not to a minted identity, not to the order anything is reported in. That is also what keeps the worker count out of a stored unit's address.

**The execution strategy is the product's.** `curios-pipeline` declares an executor seam, as it declares `Cache`: `curios` supplies a work-stealing pool, and `curios-js` and the fixtures supply one worker. One worker is the same code, not a second path.

**One representation, shareable across threads.** Terms are `Arc`-shared, their scalar derivations computed when a node is built and their lazy derivations in `OnceLock`s. Its single-threaded cost is measured and reported, never assumed.

## The components

| Component | Treatment |
| --- | --- |
| Erasure | Per item, into an erased form addressed by global name; the back end links the items reachable from the entry into an arena of its own. The cumulative arena is deleted. This is linking by name, not the relocation of an index `curios-unit`'s README rejected |
| Stored units | Addressed over their dependency closure's content rather than an ordered predecessor list, since the arena that forced the order is gone and the seed table is each unit's own |
| Executor | Once-written cells on the product's pool. An item becomes ready when the declarations its lowering names are published; a request discovered while running — a chosen witness, an unfolded body — waits on the task that holds it or runs it inline if nothing has started it. A cycle is detected over who waits for whom and reported by its members in source order, which is the same report under any schedule |
| Parsing and lowering | Per file, then per module in two phases over the possibly cyclic module graph: collect every module's exports, then lower |
| Terms and trees | `Arc`-shared; `Cell` caches computed at construction, `OnceCell`s become `OnceLock`s |
| Per-thread tables | The prelude restored once per process; the packrat, comment and formatter tables become state of the parse that owns them; the parsed-file memo becomes the session's |
| Progress | Each report names its subject, since several are in flight |

## Stages

Each lands alone and passes the gate.

1. **Erasure per item, linked by name.** The cumulative arena is deleted, and stored units are addressed over their dependency closure. Each item is erased under a budget of its own: erasure walks a whole unit under one today, so whether an item erases depends on what was erased before it.
2. **A term representation that crosses threads.** Needs the interned names a unit carries, so that the measurement includes what interning removes. Measured: the prelude build's wall time and the compile time of the corpus in `programs/`, on a release build without `profile`, before and after. Kontroli's 28.2% is the figure to expect and not the figure to report.
3. **The executor, and the gate holding one worker and many to the same bytes.** The seam and the two executors land first, with the differential; then, each switched on alone and measured: the kernel behind elaboration, elaboration by item, parsing and lowering by file and module, erasure by item, and the independent jobs — `curios test`'s library and executables, `format` and `lint` over their files, and `wonder`'s analysts.

## Verification

- The gate passes at every stage, and every verdict over `/std` and the corpus in `programs/` is compared with the compiler before the stage; a difference is a finding, never a fixture update.
- The differential compiles `/std` and the corpus with one worker and with many and requires identical stored units, diagnostics and emitted bytes.
- A genuine cycle between two items is reported with the same text under one worker and many.
- Each stage reports the time it moved, named by the stage.

## Design decisions this overturns or corrects

- [`curios-unit`'s README](../../../curios-unit/README.md): *The erased arena is the prefix's, not the unit's*.
- [`curios-prelude-archive`'s README](../../../curios-prelude-archive/README.md): `/std`'s arena stops resuming above `/sys`'s, and the images are restored once per process.
- [Cached verdicts](../../design/soundness/admission/cached-verdicts.md): the address loses its ordered predecessors.
- `curios-wonder/src/server.rs`: its module documentation's *single-threaded by construction*.
- `curios-pipeline`: `Progress`'s *sequential by construction*.
- `curios-core/src/term/frees.rs`: the comment justifying a per-thread table by `Rc`.

## Rejected

- **Scheduling units rather than items.** The time is inside units — `/std` is one, and so is the program asked for — so a unit scheduler buys almost nothing now, and the item graph honours the unit graph without one.
- **Threads sharing nothing, as the end state.** Units crossing as archived bytes serve jobs as coarse as a whole compilation, restore the prelude per worker, and cannot share an environment between items. It is not a stage either: the one environment makes it unnecessary.
- **Two representations generic over their pointer**, Kontroli's answer: one representation spelled twice, kept in agreement by the type system rather than removed.

## Non-goals

Parallelizing the Ersd and Cont optimizers or the emitter beyond Cranelift; caching per item in the store; compiling across processes or machines; sharing cores with an outer build through a job server.

## Completion and retirement

- A compilation is the same bytes for every number of workers, and the gate holds it.
- No erased arena is cumulative, and no stored unit is addressed by an ordered predecessor list.
- The prelude build and a program's compilation use every worker the product supplies, with the measured speedup recorded.

Before this specification is deleted, its contracts are recorded in the owning crates' READMEs and rustdoc, its decision and its rejected alternatives are a design decision in `documentation/design/architecture/`, `.claude/rules/`' area routing names the executor, the roadmap entry is a checked summary, and no reference to this filename remains.
