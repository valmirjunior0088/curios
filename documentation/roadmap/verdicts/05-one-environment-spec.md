# Verdicts, part 5: one environment, and every read recorded

Working specification for making the compiler principled about what one declaration may read of another. A compilation becomes a graph of items over an environment that is only ever added to; each item's output is a function of the inputs it declares, and every read of another item goes through one interface that records it. The same record schedules the work [part 6](06-item-tasks-spec.md) parallelizes, orders the kernel, and makes invalidation precise, and it is what makes a verdict independent of the order items were elaborated in. Items still run one at a time here, in source order.

It needs [part 2](02-no-history-spec.md)'s caching rule, [part 3](03-no-minted-identity-spec.md)'s identities and [the certifier's record](../../../curios-cert/README.md#a-later-walk-reads-the-certifiers-own-totality-record-never-elaborations-stamp). It changes how the elaborator threads state from item to item, so it lands after [the invariants campaign](../invariants/01-checkers-agree-spec.md)'s change to the solver, never beside it.

## What this builds on

- **Three computations of the item graph**: the lowering's sort (`curios-text/src/into_core/order.rs`), the kernel's `dependency_order` (`curios-cert/src/recheck.rs`), and the recompile closure `invalidated` (`curios-pipeline/src/recompile.rs`).
- **The `Cache` seam and the stored-unit format** (`curios-pipeline/src/compile.rs`, `curios-unit`): a product-supplied policy the fold consults, and a serialized unit that already crosses processes.
- **The certifier's record and reads.** Each unit carries `curios_core::Certification`: per definition, its totality and what judging it read (`curios_core::Reads`) — the types, universe schemes and registry entries it consulted and the bodies it asked for — a group's members sharing the group's reads and a declaration's entry filed with its type former. Positivity, one judgment over the whole declaration set, is attributed to no item. A verdict read is no third kind, every one being a signature read of a mentioned name, and `curios-pipeline`'s `the_prelude_reads_along_the_graph_its_definitions_reach` holds the reads inside the elaborated graph.
- **Peers.** Lean 4.19 elaborates theorem bodies in parallel (lean4#7084): a constant's signature is available at once, and reading its body blocks on the task that checks it.

## The gap

**The fold is a line.** A `Prefix` is every unit before this one rather than what this one depends on, and `curios-package` has the dependency graph and flattens it into that line.

**The elaborator threads state from item to item.** A witness goal that finds no table entry is deferred, swept after every later item, and may then refuse an item already finished (`curios-elab/src/resolve.rs`); a refused item poisons the items after it that reach it; the witness table grows as items elaborate; and one metavariable and binder counter serves the whole unit.

**A signature is not final before its body.** `finalize_definition` settles a written signature's universe context with the constraints its body raised (`finalize_universe_metas(interface, internal)`), and the surface has no syntax for a level, so Lean's rule — the signature alone fixes the universe parameters — is not available.

**A proof body is not opaque.** Eliminating `Accessible` into data reduces the proof, and a relevant `match` on an `Eq` proof reduces it to `refl` ([A proof is never reduced to decide what irrelevance decides](../../design/language/a-proof-is-never-reduced-to-decide-what-irrelevance-decides.md)). A later item may need any body.

## Permanent decisions

**A compilation is a graph of item tasks.** The item — a definition, a recursive group, a declaration with its registry entry, a witness — is the unit of work and of scheduling. A unit remains what names, privatizes and caches: its mounts, its manifest, its record and its slot in the store are unchanged in role, and the store stays per unit. Units are not scheduled; an item in one waits on the items of another it reads, so the package graph is honoured without a scheduler of its own.

**One environment, and every read goes through it.** A declaration is a record of cells, each written once: its key (a witness's head), its signature, its body, its kernel verdict — the certifier's record — and its erased form. Every read of another item is a request for one of those cells and is recorded, distinguishing a signature read from a body unfolded. That record is the one item graph: scheduling, the kernel's order and incremental invalidation are all read off it.

**A signature is published with its body.** An item that reads another's type waits for that declaration's elaboration to finish. Publishing a signature earlier is a refinement for items whose signature provably fixes its own universe context, taken only if the measured critical path shows it pays.

**A body another item needs is awaited, never withheld.** No body is treated as opaque; a request for one blocks on, or runs, the task that produces it.

## The components

| Component | Treatment |
| --- | --- |
| Elaboration context | Per item: its metavariables, universe solver, caches, budget and identities. Zonked when the item finishes |
| Witness resolution | Against a complete key index built from every `satisfy` head in scope before any body elaborates; choosing a witness requests its cells. The deferred-goal sweeps and the retraction of finished items are deleted |
| Refusal recovery | An item whose request reaches a refused declaration is withheld, as today, by the graph rather than the order |
| Registries | A structure, inductive or concept is published by the item that declares it, elaborated |
| Kernel | `certify` per published declaration, run as soon as elaboration publishes it, its verdict filed in the declaration's cell; `dependency_order` is deleted |
| Whole-module passes | Concept-registry checks, positivity, the erasure obligations, the witness-cycle report and the final zonk run as a barrier after the items. Per-item passes over summaries of published declarations are later work, taken if the barrier is on the measured critical path |

## Stages

Each lands alone and passes the gate.

1. **Elaboration local to an item, against a complete witness index.** Items still elaborate one at a time in source order; what changes is that nothing one item leaves behind is read by the next except through what it published. Every verdict over `/std` and the corpus is compared with the compiler before it, and any program whose witness resolution changes is recorded as a finding, since coherence already claims resolution is independent of order.
2. **One environment, reads recorded.** `Established`, `Globals`, `Resumed` and `Prefix` become views of it; the lowering's sort, `dependency_order` and `invalidated` are replaced by the recorded graph. Recompiling over a baseline then invalidates by recorded reads rather than the transitive closure of every name, and [cached verdicts](../../soundness/admission-without-judgment/cached-verdicts.md)' per-item argument is restated over them in the same change, extending its account of the certifier's record. The critical path is measured here: the recorded graph weighted by each item's `declaration` span, reported as the speedup the graph admits.

## Verification

- Every verdict over `/std` and the corpus in `programs/` is compared with the compiler before each stage; a difference is a finding, never a fixture update.
- A recompile over a baseline invalidates exactly the items whose recorded reads changed, held by fixtures over a signature change, a body change, and a witness added.
- The critical path the recorded graph admits is reported.

## Design decisions this overturns or corrects

- [`curios-unit`'s README](../../../curios-unit/README.md): *A scope is borrowed, per stage*, with `Prefix`'s own documentation.
- [A module is a compilation unit, and the prelude is an environment](../../design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md): a compilation stops being units folded over one dependency order.
- [A stored unit is a baseline for an item-level recompile](../../design/toolchain/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md): invalidation by recorded reads.
- [Cached verdicts](../../soundness/admission-without-judgment/cached-verdicts.md): the per-item argument restated over recorded reads.

## Rejected

- **salsa.** Pre-1.0 with breaking releases, it removed its experimental parallel feature in 0.24; its database model would own the term representation, and its invalidation would sit beside the store's trust argument rather than under it. The graph here has thousands of nodes and obligations — budgets, deterministic identities, charged caches — that a small component states directly.
- **Lean's signature rule**, that a signature alone fixes a declaration's universe parameters: it needs level syntax the surface does not have.
- **Opaque proof bodies**, Lean's reason its theorem bodies parallelize freely: Curios reduces proofs where it eliminates `Accessible` and matches on `refl`.
- **Publishing a signature speculatively and rolling back dependents** when the body disagrees: the answer would be deterministic and the work would not, and a dependent's diagnostics would be computed against a signature that never existed.

## Completion and retirement

Every read of one item by another goes through the environment, the item graph is computed once, and the kernel certifies each declaration as it is published. Record the environment's contract in the owning crates' READMEs and rustdoc, and `CLAUDE.md`'s change routing names the environment; replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
