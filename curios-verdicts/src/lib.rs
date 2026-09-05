//! What a compilation consults before doing its work again — the fold's units, and the invocation's own precompiled payload — as the store beside a project holds them.
//!
//! **Beside the pipeline, over the package, and under neither.** The store's layout and its keys belong to `curios-package`; reading and writing a `Unit` through them is this crate, because [`Cache`](curios_pipeline::Cache) is `curios-pipeline`'s trait and `curios-package` sits *beside* that boundary rather than under it. Implementing it there would make the crate that answers "what is in this compilation" depend on the driver that folds stages over the answer, which is the one direction the layering forbids; implementing it in the driver would make the pure fold name a manifest. So the implementation is a crate of its own, and every product that consults a store — the native compiler for what it runs, the `wonder` engine for what it is asked — takes it from here. It links no back end: nothing below this crate reaches Binaryen or Wasmtime, and the one machine-dependent fact a payload address carries, the engine that will run it, is handed in by the crate that owns the runtime rather than computed here.
//!
//! **Taking a unit from here is believing a verdict this compiler reached earlier.** That is a change to what the compiler believes rather than a faster way to do what it already did, and the argument for it is in [Cached verdicts](../../documentation/soundness/admission-without-judgment/cached-verdicts.md). Everything in `verdicts` is the mechanism the argument is about. The payload family in `payload` is that same argument one level up, with [Reused payloads](../../documentation/soundness/admission-without-judgment/reused-payloads.md) stating what it adds. Both file into the one-file slots `slot` frames.

mod slot;
pub use slot::*;

mod verdicts;
pub use verdicts::*;

mod payload;
pub use payload::*;
