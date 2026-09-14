# curios-pipeline

The Curios compile driver: `compile_entrypoint`, `compile_units`, `Stage`, and the fold that strings the pipeline together from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module` — plus, in `standard.rs`, the same fold with the fixed prelude supplied. It is the compiler boundary: everything a compilation needs and nothing a *product* decides. How the stages themselves work belongs to each stage's own crate; where judgment sits in the sequence, what `Stage` observes, and why the standard prefix is a function rather than a policy belong to the crate rustdoc, which states them beside the code.

## Design

### The driver is the compiler boundary, and scope is not its decision

**Decision.** This crate depends on no runtime, no Binaryen, no CLI, and — the constraint this section exists for — no `curios-package`. It folds its stages over whatever scope it is handed. `curios-package` sits *beside* this boundary rather than under it, and `curios-js` does not touch it at all.

**Rationale.** Manifests, dependency resolution, and the store answer one question — what is in this compilation, and where did each part come from — and that question is a product's to answer, not a compiler's. `curios` answers it over `curios-package`; the browser has no filesystem to resolve against and answers it differently. A driver that knew about manifests would have to be given a plausible answer by every caller that has none, so the browser product would end up simulating a package layer to compile one string.

The boundary is enforced by the manifest rather than by discipline: there is no `curios-package` row here, so nothing in the fold can reach a resolver, and the two products can disagree about scope without the driver noticing. `curios-package/src/lib.rs` states the same rule from the other side, which is the shape a real boundary has — both crates know where it is.

**Rejected.** Taking `curios-package` as a dependency and letting the driver resolve its own inputs. It would put a filesystem assumption below the browser product, and it would make "what is in this compilation" a compiler question in a workspace where the whole reason two products share one pipeline is that it is not one.

### The standard prefix is a function here, not a policy

**Decision.** `compile_entrypoint` takes a `Prefix` and cannot tell which unit is `/std`; that does not change. `standard.rs` sits above it and supplies the fixed prelude, and nothing in the scope-agnostic half calls anything in it.

**Rationale.** Naming the standard library is a product's decision, and it still is — but it had been written three times by hand, by the native product, the browser product, and this crate's own fixtures. Three callers agreeing on one spelling is a missing function, not a policy being violated, and the third was not a product at all. Lifting the shared spelling into a layer that the pure half cannot call keeps the original reading intact while removing the duplication it was paying for.

### A baseline crosses the cache seam as a unit, and the cache decides

**Decision.** `Cache::baseline` hands the fold a unit compiled from an earlier text of the same sources, or nothing; the fold compiles over it with `compile_unit_over` and hands the result to `put` as it hands any unit. Whether a store offers a baseline and whether it files what was compiled over one are the cache implementation's decisions: the store's own cache offers none and files everything, and the `wonder` engine's read-only cache offers one and files nothing.

**Rationale.** The fold learns nothing about projects, so a baseline crosses the seam as the one thing the fold already understands, a `Unit`, and never as a path or a record. Keeping the policy in the cache implementations rather than in the fold is what makes "only questions take a baseline" a decision written in one place: a build that wanted one later would change a cache, not the fold, and the differential gate that earns that change runs the same code either way.

**Rejected.** A `baselines` parameter on `compile_units` — a second seam for the same fact, and one the fold would have to reconcile with the cache. Deciding by entry point — `wonder stage` and `curios test` reach the fold through the same entry points, so an entry point cannot tell a question from a build.

### A package named `std` is the standard library, compiled over the archived one

**Decision.** `standard.rs` puts every archived root in scope whatever the units beside it declare, with one exception: the first unit of a fold claiming the prefix the last archived root mounts — a package named `std` — takes that root's place, compiled over the archived unit as its baseline and granted what the root could see, with every later unit compiled against it. The name is reserved for this. Only the first unit and only the last root can qualify, and both are asserted rather than reasoned about.

**Rationale.** The standard library is the largest Curios program and the one edited most, and a question about one of its modules had no answer: the package claims `/std`, the archive mounts it, and two claims of one prefix are a collision. What the name buys over any finer identity — the archive's record names the tree it was built from, and a first cut compared directories against it — is that the rule does not depend on which binary answers: an installed compiler answers a question about any checkout's `std/`, a worktree's included. Whatever tree the package is, the baseline is correct, because an item is reused only where its lowered form matches the archived one and nothing it reaches changed; a tree far from the archive's is a larger closure, not a wrong answer. And nothing is lost that a scope could have had: `/std` cannot be swapped under a dependency without changing what that dependency means, so "which `/std`" was never a parameter of a scope, and reserving the name is the cost every language with a standard library already pays.

**Rejected.** Superseding by prefix for any unit in the fold — a later unit's scope is not the one the archived unit was compiled in, so its items could not be reused, and a scope that changed shape on a later member's name would not be a scope. Identifying the tree by the record's directory — correct, but it made the answer depend on the binary being the one built from that very checkout. A manifest key or a root kind naming the standard library — the privilege tier the mount design removed, and no more than the name says. Recognizing the prelude in `curios-wonder` or in the package layer — neither can reach the archive, and every other product would keep colliding.
