# curios-verdicts

The store as a compilation sees it: the `Cache` the fold consults for units already judged, and the payload family an invocation consults before compiling a program it has already compiled — both believed on a verified record rather than on an address. The store's families and keys belong to `curios-package`, what a stored unit is to `curios-unit`, and why believing one is sound to [Cached verdicts](../documentation/design/soundness/cached-verdicts.md) and [Reused payloads](../documentation/design/soundness/reused-payloads.md); the mechanism belongs to the crate rustdoc.

## Design

### Beside the pipeline, over the package, and under neither

**Decision.** This crate depends on `curios-pipeline`, whose `Cache` trait it implements, and on `curios-package`, whose store layout and keys it reads and writes through; neither holds the implementation. It links no back end: `cargo tree -p curios-verdicts --edges normal` contains neither `curios-binaryen` nor `curios-runtime`, and the one machine-dependent fact a payload address carries, the engine that will run it, is handed in by the crate that owns the runtime.

**Rationale.** `Cache` in `curios-package` would make the crate answering what is in a compilation depend on the driver that folds stages over the answer; in `curios-pipeline` it would make the pure fold name a manifest. The `wonder` engine reads verdicts for every question it answers and links neither Binaryen nor Wasmtime, and a store read has no use for either. `curios-package` depends on `curios-text` and so links the surface language, its lowering and `curios-core`; the dependency this boundary keeps out is the driver's.

**Rejected.** The implementation in `curios`, handing the `wonder` engine a `dyn Cache`, whose trait cannot keep a unit without filing it; computing the engine fingerprint here behind a feature, which unifies across a workspace build and puts Cranelift under every consumer.

### A disagreeing slot is a baseline, for a question and for a build

**Decision.** `Verdicts::earlier` hands back a slot's unit when the slot is intact and read from files the asking source could have read, whatever those files and the units before it hold now, and says which units of the fold contain what the slot's record names them by (`curios_pipeline::After`). The store's own `Cache` offers the fold the nearest earlier compilation of a unit as its baseline — what an attached session last had in hand for it, then that slot, then what the scope offered — and the `wonder` engine's cache asks it for the same answer, so `run`, `compile` and `test` compile a moved unit over its baseline as a question does, and file it. What a question compiled over a baseline it files as it files what it compiled whole: it asks `Verdicts::taken_on_disk`, and files where the disk holds every text its fold has read.

**Rationale.** A unit compiled over a baseline is stored as the bytes a whole compile of the same text stores ([A stored unit is a baseline for an item-level recompile](../documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)), so the slot is the one a whole compile would have written whichever command wrote it, and the next baseline is no further from a whole compile than the last. No verdict a build files rests on what the elaborator kept: which of a baseline's verdicts stand is the certifier's decision, from the reads its own walk filed ([Cached verdicts](../documentation/design/soundness/cached-verdicts.md)). A baseline is offered whatever moved before it, since what a declaration read of another unit is asked again by the stage that read it, by the digest it recorded, wherever that unit is not the one the record names.

**Rejected.** A build compiling a moved unit whole, which pays for every declaration of the unit where a question pays for what moved, to store the same bytes; withholding what a question compiled over a baseline, which leaves every question after a saved edit recompiling what moved until a build files the unit.
