# A compilation is a graph of item tasks

Working specification for multithreaded compilation, reached by making the compiler principled about what one declaration may read of another. A compilation becomes a graph of tasks, one per item, over an environment that is only ever added to; each task's output is a function of the inputs it declares, and every read of another item goes through one interface that records it. Parallelism is the payoff, and it is not the only one: the same property makes a verdict independent of the order items were elaborated in, and makes invalidation precise. The compiler is single-threaded today by omission rather than by decision, and every design statement to the contrary is listed under what this overturns.

## What this builds on

- **Per-declaration budgets.** The elaborator (`elaborate_module_item`) and the kernel (`check_definition`) restore the work budget at every item, so whether an item is accepted does not depend on what earlier items spent.
- **The kernel's memo discipline** (`curios-cert/src/kernel/memos.rs`): term-keyed tables live one declaration and hit free; the one table outliving a declaration is keyed by name and charged. That is the rule under which a cache cannot change a verdict with the schedule.
- **`validate_stored_identities`** (`curios-core/src/module.rs`): a stored unit carries no metavariable and no free local. And a stored binder identity is read only by the printer — both checkers mint fresh variables when they open a scope — so the identities a unit carries are not what either checker's freshness rests on.
- **Three computations of the item graph**: the lowering's sort (`curios-text/src/into_core/order.rs`), the kernel's `dependency_order` (`curios-cert/src/recheck.rs`), and the recompile closure `invalidated` (`curios-pipeline/src/recompile.rs`).
- **The `Cache` seam and the stored-unit format** (`curios-pipeline/src/compile.rs`, `curios-unit`): a product-supplied policy the fold consults, and a serialized unit that already crosses processes.
- **What already runs beside the compiler**: `wonder server`'s protocol thread, Binaryen behind its process lock, and the prelude images validated once per process.
- **Peers.** Lean 4.19 elaborates theorem bodies in parallel (lean4#7084): a constant's signature is available at once, and reading its body blocks on the task that checks it. rustc parallelizes type checking per item under one build that selects serial or parallel synchronization at run time (`rustc_data_structures::sync`, `DynSend`/`DynSync`). Kontroli, a Rust checker for the λΠ-calculus modulo rewriting (Färber, CPP 2022), measured `Arc` terms 28.2% slower than `Rc` on one thread, won already at two threads, and reached 6.6× at eight. Rocq checks opaque proofs in worker processes (Barras, Tankink and Tassi, ITP 2015); GHC compiles modules in parallel under `-j`.

## The gap

**Nothing the compiler holds can cross a thread.** `Term` is an `Rc<Node>` whose caches are `Cell`s and `OnceCell`s; `Qualifier` is an `Rc<Vec<String>>` interned per thread; `Span` holds an `Rc<Source>`; the surface trees are `Rc`-shared. Six `thread_local!` tables sit under that: the restored prelude (`curios-prelude-archive/src/restore.rs`, restored again by every thread that compiles), the packrat memo, the comment table, the parsed-file memo (`curios-text/src/root_source.rs`), the formatter's owed comments, and the shared empty free-variable set.

**The fold is a line, and positions are carried along it.** A `Prefix` is every unit before this one rather than what this one depends on; the erased arena is cumulative from the first unit; the binder, metavariable and universe floors resume above the previous unit's; the universe-seed table is cumulative from index zero; and a stored unit is addressed by the ordered list of its predecessors because of the last two. `curios-package` has the dependency graph and flattens it into that line.

**The elaborator threads state from item to item.** A witness goal that finds no table entry is deferred, swept after every later item, and may then refuse an item already finished (`curios-elab/src/resolve.rs`); a refused item poisons the items after it that reach it; the witness table grows as items elaborate; and one metavariable and binder counter serves the whole unit.

**Acceptance already depends on history.** The elaborator's closed-reduct cache outlives the declaration that filled it and a hit on it is free, so a declaration that would have hit a warm cache can exhaust its budget when compiled after different neighbours — `curios-core/src/retention.rs` calls this the elaborator's warmth-dependence. It is a defect of the serial compiler, and under a parallel schedule it would be a verdict that changes between runs.

**A signature is not final before its body.** `finalize_definition` settles a written signature's universe context with the constraints its body raised (`finalize_universe_metas(interface, internal)`), and the surface has no syntax for a level, so Lean's rule — the signature alone fixes the universe parameters — is not available.

**A proof body is not opaque.** Eliminating `Accessible` into data reduces the proof, and a relevant `match` on an `Eq` proof reduces it to `refl` ([A proof is never reduced to decide what irrelevance decides](../design/language/a-proof-is-never-reduced-to-decide-what-irrelevance-decides.md)). A later item may need any body.

**Where the time is.** The prelude build's `profile.tsv` of 2026-09-23 — an instrumented build, so the shares are the figures and the durations are not — elaborates `/std`'s 2311 items in 49.7 s, parses its 137 files in 10.5 s, runs the whole-module finalization in 9.1 s and erases in 6.7 s. Item costs are spread thin: the heaviest, `/std/Tui/run`, is 2.3% of elaboration, the fifty heaviest 20.8%, and the median item 11.6 ms. A one-declaration program in `curios/.artifacts/profile.tsv` spends 1.3 s of 1.5 s elaborating and 0.1 s restoring the prelude; the back half costs tens of milliseconds. Cranelift precompiles serially, since `curios-runtime` enables no `parallel-compilation`. To retake a figure: build with the `profile` feature, and fold the file by pairing each `E` row with its `X` row per span id and summing per callsite name — a `declaration` span names its item in its `group` field, which is what the per-item distribution is read from.

## Permanent decisions

**The determinism contract.** A compilation's units, verdicts, diagnostics and emitted bytes are the same for every number of workers. The schedule is never an input: not to a verdict, not to a minted identity, not to the order anything is reported in. That is also what keeps the worker count out of a stored unit's address.

**A compilation is a graph of item tasks.** The item — a definition, a recursive group, a declaration with its registry entry, a witness — is the unit of work and of scheduling. A unit remains what names, privatizes and caches: its mounts, its manifest, its record and its slot in the store are unchanged in role, and the store stays per unit. Units are not scheduled; an item in one waits on the items of another it reads, so the package graph is honoured without a scheduler of its own.

**One environment, and every read goes through it.** A declaration is a record of cells, each written once: its key (a witness's head), its signature, its body, its kernel verdict and its erased form. Every read of another item is a request for one of those cells and is recorded, distinguishing a signature read from a body unfolded. That record is the one item graph: scheduling, the kernel's order and incremental invalidation are all read off it.

**A signature is published with its body.** An item that reads another's type waits for that declaration's elaboration to finish. Publishing a signature earlier is a refinement for items whose signature provably fixes its own universe context, taken only if the measured critical path shows it pays.

**A body another item needs is awaited, never withheld.** No body is treated as opaque; a request for one blocks on, or runs, the task that produces it.

**One caching rule for both checkers.** A table that lives one declaration hits free. A table outliving a declaration is keyed by name and a hit on it costs what recomputing would. Under that rule the compilation-wide retention allowance can change how fast a compilation runs and never what it decides, so it stays compilation-wide.

**Artifacts carry no minted identity.** A stored binder label is a display hint; binder, metavariable and universe identities are minted in a space private to the item task that mints them. Witness ordinals stay per module, as they are.

**The execution strategy is the product's.** `curios-pipeline` declares an executor seam, as it declares `Cache`: `curios` supplies a work-stealing pool, and `curios-js` and the fixtures supply one worker. One worker is the same code, not a second path.

**One representation, shareable across threads.** Terms are `Arc`-shared, their scalar derivations computed when a node is built and their lazy derivations in `OnceLock`s. Its single-threaded cost is measured and reported, never assumed.

## The components

| Component | Treatment |
| --- | --- |
| Names and qualifiers | Interned once per process into copyable identities; the per-thread interner is deleted |
| Sources and spans | A source map owns every text; a span is a source identity and a range, copied rather than shared. Two separately loaded sources stay distinct, as `Rc` pointer identity keeps them today |
| Binder labels | Hints; the printer derives its labels from hints and depth |
| Floors and the seed table | Deleted with the positions they protected: `Module::binder_floor`, `Unit::binder_floor`, `derived_binder_floor`, and the metavariable and universe floors with the cumulative seed table |
| Elaboration context | Per item: its metavariables, universe solver, caches, budget and identities. Zonked when the item finishes |
| Witness resolution | Against a complete key index built from every `satisfy` head in scope before any body elaborates; choosing a witness requests its cells. The deferred-goal sweeps and the retraction of finished items are deleted |
| Refusal recovery | An item whose request reaches a refused declaration is withheld, as today, by the graph rather than the order |
| Registries | A structure, inductive or concept is published by the item that declares it, elaborated |
| Kernel | `certify` per published declaration, run as soon as elaboration publishes it; `dependency_order` is deleted |
| Whole-module passes | Concept-registry checks, positivity, the erasure obligations, the witness-cycle report and the final zonk run as a barrier after the items. Per-item passes over summaries of published declarations are later work, taken if the barrier is on the measured critical path |
| Erasure | Per item, into an erased form addressed by global name; the back end links the items reachable from the entry into an arena of its own. The cumulative arena is deleted. This is linking by name, not the relocation of an index `curios-unit`'s README rejected |
| Stored units | Addressed over their dependency closure's content rather than an ordered predecessor list, since the seed table and the arena that forced the order are gone |
| Executor | Once-written cells on the product's pool. An item becomes ready when the declarations its lowering names are published; a request discovered while running — a chosen witness, an unfolded body — waits on the task that holds it or runs it inline if nothing has started it. A cycle is detected over who waits for whom and reported by its members in source order, which is the same report under any schedule |
| Parsing and lowering | Per file, then per module in two phases over the possibly cyclic module graph: collect every module's exports, then lower |
| Terms and trees | `Arc`-shared; `Cell` caches computed at construction, `OnceCell`s become `OnceLock`s |
| Per-thread tables | The prelude restored once per process; the packrat, comment and formatter tables become state of the parse that owns them; the parsed-file memo becomes the session's |
| Progress | Each report names its subject, since several are in flight |
| Cranelift | `curios-runtime`'s `cranelift` feature enables Wasmtime's `parallel-compilation`; the launcher, which never enables `cranelift`, is unaffected |

## Stages

Each lands alone and passes the gate. Each of the first seven is a change the serial compiler is better for; the last is where the threads arrive.

- **Cranelift compiles in parallel.** Needs nothing. Accepted when the bundle guards in `curios/src/bundle.rs` pass and the launcher's graph is unchanged, with precompilation time reported before and after.
- **One caching rule for both checkers.** Needs nothing, and fixes the warmth-dependence. The elaborator's closed-reduct and elaboration caches take the kernel's split: term-keyed tables cleared at the declaration boundary, one name-keyed unfold table charged. Charging recorded costs everywhere is not the rule — `curios-cert`'s README records how recorded costs compound. Measured over `/std` and the corpus in `programs/`: the heaviest declaration's consumption, as `Context::heaviest_declaration` reports it, against `DEFAULT_STEP_BUDGET`, and wall time, before and after. Headroom that shrinks is answered by raising the budget by the measured figure. A fixture holds that an item's verdict is the same compiled alone and after its neighbours.
- **Interned names and a source map.** Needs nothing.
- **Artifacts carry no minted identity.** Needs nothing. Binder labels become hints, identities are minted per item, and the floors and the seed table are deleted. `validate_stored_identities` keeps refusing what it refuses today.
- **Elaboration local to an item, against a complete witness index.** Needs the previous stage. Items still elaborate one at a time in source order; what changes is that nothing one item leaves behind is read by the next except through what it published. Every verdict over `/std` and the corpus is compared with the compiler before it, and any program whose witness resolution changes is recorded as a finding, since coherence already claims resolution is independent of order.
- **One environment, reads recorded.** Needs the previous stage. `Established`, `Globals`, `Resumed` and `Prefix` become views of it; the lowering's sort, `dependency_order` and `invalidated` are replaced by the recorded graph. Recompiling over a baseline then invalidates by recorded reads rather than the transitive closure of every name, and [cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md)' per-item argument is restated over them in the same change. The critical path is measured here: the recorded graph weighted by each item's `declaration` span, reported as the speedup the graph admits.
- **Erasure per item, linked by name.** Needs the environment. The cumulative arena is deleted, and stored units are addressed over their dependency closure.
- **A term representation that crosses threads.** Needs the interned names, so that the measurement includes what interning removes. Measured: the prelude build's wall time and the compile time of the corpus in `programs/`, on a release build without `profile`, before and after. Kontroli's 28.2% is the figure to expect and not the figure to report.
- **The executor, and the gate holding one worker and many to the same bytes.** Needs every stage above. The seam and the two executors land first, with the differential; then, each switched on alone and measured: the kernel behind elaboration, elaboration by item, parsing and lowering by file and module, erasure by item, and the independent jobs — `curios test`'s library and executables, `format` and `lint` over their files, and `wonder`'s analysts.

## Design decisions this overturns or corrects

Each is revised in the change that makes it true.

- [`curios-unit`'s README](../../curios-unit/README.md): *The erased arena is the prefix's, not the unit's* and *A scope is borrowed, per stage*, with `Prefix`'s own documentation.
- [A module is a compilation unit, and the prelude is an environment](../design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md): a compilation stops being units folded over one dependency order.
- [A stored unit is a baseline for an item-level recompile](../design/toolchain/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md): invalidation by recorded reads.
- [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md): the address loses its ordered predecessors, and the per-item argument is restated over recorded reads.
- [`curios-prelude-archive`'s README](../../curios-prelude-archive/README.md): `/std`'s seeds, floors and arena no longer resume above `/sys`'s, and the images are restored once per process.
- `curios-core/src/retention.rs` and `curios-cert/src/kernel/memos.rs`: the caching rule is stated once, for both checkers, and the warmth-dependence paragraph is deleted with the warmth.
- `curios-wonder/src/server.rs`: its module documentation's *single-threaded by construction*.
- `curios-pipeline`: `Progress`'s *sequential by construction*.
- `curios-text/src/root_source.rs`, `curios-utilities/src/qualifier.rs` and `curios-core/src/term/frees.rs`: the comments justifying a per-thread table by `Rc`.

## Rejected

- **Scheduling units rather than items.** The time is inside units — `/std` is one, and so is the program asked for — so a unit scheduler buys almost nothing now, and the item graph honours the unit graph without one.
- **Threads sharing nothing, as the end state.** Units crossing as archived bytes serve jobs as coarse as a whole compilation, restore the prelude per worker, and cannot share an environment between items. It is not a stage either: the environment stage makes it unnecessary.
- **Two representations generic over their pointer**, Kontroli's answer: one representation spelled twice, kept in agreement by the type system rather than removed.
- **Hash-consing as a prerequisite.** It changes what a term's identity means and needs a global interner with a reclamation story for the language server; it is a decision of its own, taken on its own merits.
- **salsa.** Pre-1.0 with breaking releases, it removed its experimental parallel feature in 0.24; its database model would own the term representation, and its invalidation would sit beside the store's trust argument rather than under it. The graph here has thousands of nodes and obligations — budgets, deterministic identities, charged caches — that a small component states directly.
- **Lean's signature rule**, that a signature alone fixes a declaration's universe parameters: it needs level syntax the surface does not have.
- **Opaque proof bodies**, Lean's reason its theorem bodies parallelize freely: Curios reduces proofs where it eliminates `Accessible` and matches on `refl`.
- **Publishing a signature speculatively and rolling back dependents** when the body disagrees: the answer would be deterministic and the work would not, and a dependent's diagnostics would be computed against a signature that never existed.
- **Keeping the warm caches and ordering the schedule** to reproduce the serial order: it serializes exactly what this specification parallelizes, and keeps a defect.

## Non-goals

Parallelizing the Ersd and Cont optimizers or the emitter beyond Cranelift; caching per item in the store; compiling across processes or machines; sharing cores with an outer build through a job server.

## Verification

- The gate passes at every stage, and every verdict over `/std` and the corpus in `programs/` is compared with the compiler before the stage; a difference is a finding, never a fixture update.
- The differential compiles `/std` and the corpus with one worker and with many and requires identical stored units, diagnostics and emitted bytes.
- A genuine cycle between two items is reported with the same text under one worker and many.
- An item's verdict is the same compiled alone and after its neighbours.
- Each stage reports the time it moved, named by the stage.

## Completion criteria

- A compilation is the same bytes for every number of workers, and the gate holds it.
- Every read of one item by another goes through the environment, and the item graph is computed once.
- No artifact carries a minted identity, and no floor survives.
- No erased arena is cumulative, and no stored unit is addressed by an ordered predecessor list.
- The prelude build and a program's compilation use every worker the product supplies, with the measured speedup recorded.
- Before this specification is deleted, its contracts are recorded in the owning crates' READMEs and rustdoc, the decision and its rejected alternatives are a design decision in `documentation/design/toolchain/`, `CLAUDE.md`'s change routing names the environment and the executor, the roadmap entry is a checked summary, and no reference to this filename remains.
