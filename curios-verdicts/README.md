# curios-verdicts

The store as a compilation sees it: the `Cache` the fold consults for units already judged, and the payload family an invocation consults before compiling a program it has already compiled — both believed on a verified record rather than on an address. What the store's families and keys are belongs to `curios-package`; why believing a stored unit is sound belongs to [Cached verdicts](../documentation/soundness/admission-without-judgment/cached-verdicts.md) and [Reused payloads](../documentation/soundness/admission-without-judgment/reused-payloads.md); the mechanism belongs to the crate rustdoc.

## Design

### Beside the pipeline, over the package, and under neither

**Decision.** This crate depends on `curios-pipeline`, whose `Cache` trait it implements, and on `curios-package`, whose store layout and keys it reads and writes through. Neither of those may hold the implementation, and this crate links no back end: `cargo tree -p curios-verdicts --edges normal` contains neither `curios-binaryen` nor `curios-runtime`.

**Rationale.** Implementing `Cache` in `curios-package` would make the crate that answers "what is in this compilation" depend on the driver that folds stages over the answer, which is the one direction the layering forbids; implementing it in `curios-pipeline` would make the pure fold name a manifest. It was a module of the native compiler until the `wonder` engine, which reads verdicts for every question it answers, needed a home that links neither Binaryen nor Wasmtime — and a store read has no use for either. The one machine-dependent fact a payload address carries, the engine that will run it, is handed in by the crate that owns the runtime rather than computed here, which is what keeps Cranelift out of this crate's graph.

**Rejected.** Keeping the implementation in `curios` and handing the `wonder` engine a `dyn Cache`: a query must place a unit in the chain without filing it, and the trait cannot say the first without the second. Computing the engine fingerprint here behind a feature: a feature is additive and unifies across a workspace build, so any consumer enabling it would put Cranelift under every other.

It is not about keeping `curios-package` free of the compiler. That crate depends on `curios-text` and so already links the elaborator — `curios new` included. The dependency the boundary buys is the driver's.
