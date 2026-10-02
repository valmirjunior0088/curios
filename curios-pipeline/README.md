# curios-pipeline

The Curios compile driver: `compile_entrypoint`, `compile_units`, `Stage`, and the fold that strings the pipeline together from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module` — plus, in `standard.rs`, the same fold with the fixed prelude supplied. It is the compiler boundary: everything a compilation needs and nothing a product decides. How the stages work belongs to each stage's crate; where judgment sits in the sequence and what `Stage` observes belong to the crate rustdoc.

## Design

### The driver is the compiler boundary, and scope is not its decision

**Decision.** This crate depends on no runtime, no Binaryen, no CLI and no `curios-package`, and folds its stages over whatever scope it is handed. `curios-package` sits beside the boundary, and `curios-js` does not touch it.

**Rationale.** Manifests, dependency resolution and the store answer what is in a compilation and where each part came from, which is a product's question: `curios` answers it over `curios-package`, and the browser, with no filesystem, answers it differently. A driver that knew manifests would need a plausible answer from every caller that has none. The boundary is the manifest's — no `curios-package` row here — and `curios-package/src/lib.rs` states it from the other side.

**Rejected.** The driver resolving its own inputs, which puts a filesystem assumption below the browser product.

### The standard prefix is a function here, not a policy

**Decision.** `compile_entrypoint` takes a `Prefix` and cannot tell which unit is `/std`. `standard.rs` sits above it and supplies the fixed prelude, `/sys` and `/std` first in scope, and nothing in the scope-agnostic half calls it. A package named `std` that is a fold's first unit takes the archived `/std`'s place, compiled over the archived unit as its baseline and granted what that root could see, and every later unit is compiled against it; only the first unit and the last root can qualify, and both are asserted ([A stored unit is a baseline for an item-level recompile](../documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)).

**Rationale.** The native product, the browser product and this crate's fixtures would each spell the prelude by hand, and three callers agreeing on one spelling is a missing function. The standard library is the largest Curios program and the one edited most, and a question about one of its modules would otherwise be a collision of two claims on `/std`. Reserving the name makes the answer independent of which binary answers, and a baseline is correct for any tree, since an item is reused only where its lowered form matches and nothing it reaches changed.

**Rejected.** Superseding by prefix for any unit of the fold, when a later unit's scope is not the one the archived unit was compiled in; identifying the tree by the record's directory, which depends on the binary's checkout; a manifest key or root kind for the standard library; recognizing the prelude in `curios-wonder` or the package layer, neither of which can reach the archive.

### A baseline crosses the cache seam as a unit, and the cache decides

**Decision.** `Cache::baseline` hands the fold a unit compiled from an earlier text of the same sources, or nothing; the fold compiles over it with `compile_unit_over` and hands the result to `put` as any unit. Whether a store offers a baseline and files what was compiled over one is the cache implementation's decision: the store's own cache offers none and files everything, and the `wonder` engine's offers one and files only what it compiled whole from the text on disk ([`curios-wonder`'s README](../curios-wonder/README.md#a-question-files-the-units-it-compiled-from-disk)).

**Rationale.** A baseline crosses the seam as the one thing the fold understands, a `Unit`, never a path or a record, and "only questions take a baseline" is written in one place: a build that wanted one would change a cache, not the fold.

**Rejected.** A `baselines` parameter on `compile_units`, a second seam for the same fact; deciding by entry point, when `wonder stage` and `curios test` reach the fold through the same ones.
