# curios-verdicts

The store as a compilation sees it: the `Cache` the fold consults for units already judged, and the payload family an invocation consults before compiling a program it has already compiled — both believed on a verified record rather than on an address. The store's families and keys belong to `curios-package`, what a stored unit is to `curios-unit`, and why believing one is sound to [Cached verdicts](../documentation/design/soundness/admission/cached-verdicts.md) and [Reused payloads](../documentation/design/soundness/admission/reused-payloads.md); the mechanism belongs to the crate rustdoc.

## Design

### Beside the pipeline, over the package, and under neither

**Decision.** This crate depends on `curios-pipeline`, whose `Cache` trait it implements, and on `curios-package`, whose store layout and keys it reads and writes through; neither holds the implementation. It links no back end: `cargo tree -p curios-verdicts --edges normal` contains neither `curios-binaryen` nor `curios-runtime`, and the one machine-dependent fact a payload address carries, the engine that will run it, is handed in by the crate that owns the runtime.

**Rationale.** `Cache` in `curios-package` would make the crate answering what is in a compilation depend on the driver that folds stages over the answer; in `curios-pipeline` it would make the pure fold name a manifest. The `wonder` engine reads verdicts for every question it answers and links neither Binaryen nor Wasmtime, and a store read has no use for either. `curios-package` depends on `curios-text` and so links the surface language, its lowering and `curios-core`; the dependency this boundary keeps out is the driver's.

**Rejected.** The implementation in `curios`, handing the `wonder` engine a `dyn Cache`, whose trait cannot place a unit in the chain without filing it; computing the engine fingerprint here behind a feature, which unifies across a workspace build and puts Cranelift under every consumer.

### A disagreeing slot is a baseline for a question and a whole compile for a build

**Decision.** `Verdicts::earlier` hands back a slot's unit when the slot is intact, filed after the current chain, and read from files the asking source could have read, whatever those files hold now. Only the `wonder` engine's read-only cache offers it to the fold as a baseline; the store's own `Cache` offers none, so `run`, `compile` and `test` compile a moved unit whole and file it, and nothing compiled over a baseline is filed.

**Rationale.** A reused item rests on the verdict recorded when the baseline was judged, and the kernel skips it by name, so the guarantee rests on the recompile's closure being closed — an argument the differential gate checks on fixtures and has not earned for a filed unit. A question's answer is corrected by the next build, while a filed unit would become the next baseline and a mistake would compound. The chain clause keeps a baseline to the scope it was compiled in, since an item mentioning a predecessor's name is outside the diff.

**Rejected.** A baseline for every fold, trading trust in the closure for a build's speed; filing what was compiled over a baseline, where a closure mistake persists until something recompiles whole.
