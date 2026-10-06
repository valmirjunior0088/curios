//! What a compilation consults before doing its work again — the fold's units, and the invocation's own precompiled payload — as the store beside a project holds them.
//!
//! **Beside the pipeline, over the package, and under neither** — `README.md`'s decision. The store's layout and its keys belong to `curios-package`, [`Cache`](curios_pipeline::Cache) is `curios-pipeline`'s trait, and reading and writing a `Unit` through the one to implement the other is this crate; every product that consults a store — the native compiler for what it runs, the `wonder` engine for what it is asked — takes it from here. It links no back end: the one machine-dependent fact a payload address carries, the engine that will run it, is handed in by the crate that owns the runtime rather than computed here.
//!
//! **Taking a unit from here is believing a verdict this compiler reached earlier.** That is a change to what the compiler believes rather than a faster way to do what it already did, and the argument for it is in [Cached verdicts](../../documentation/design/soundness/cached-verdicts.md). Everything in `verdicts` is the mechanism the argument is about. The payload family in `payload` is that same argument one level up, with [Reused payloads](../../documentation/design/soundness/reused-payloads.md) stating what it adds. Both file into one-file slots — framed as `curios-unit` states a stored unit is, and replaced whole by `slot`.

mod slot;
pub(crate) use slot::*;

mod verdicts;
pub use verdicts::*;

mod payload;
pub use payload::*;
