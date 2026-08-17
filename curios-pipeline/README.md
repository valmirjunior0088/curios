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
