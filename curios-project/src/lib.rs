//! What a Curios project is, and everything that reads one: the `Curios.toml` manifest, the walk that decides which manifest governs an invocation, the resolver that turns a declared dependency into a module tree, and the store the results are filed in.
//!
//! **One subsystem, one owner.** Those four are faces of a single question — what is in this compilation, and where did each part come from — so they are one crate rather than four seams through the driver. `curios-pipeline` stays pure and never learns this crate exists: it folds its stages over whatever scope it is handed, and *deciding* that scope is a product's job, exactly as supplying the `/syn` registry already is. `curios` is the product that decides it, over this crate. `curios-web` never touches it, which is the constraint that keeps a resolver from quietly assuming a filesystem.
//!
//! **This is the workspace's only TOML dependency**, on the pattern `curios-archive` sets for rkyv. `/std`'s TOML codec is a guest library — it runs in the compiled program, not in the compiler — and cannot serve the driver.
//!
//! Every refusal here fires before elaboration and names both parties. A manifest declaring two modes, a name no path could spell, a dependency row missing the pin its source requires: each is diagnosed against the file somebody wrote, never surfaced downstream as an unbound name.

mod hash;
pub use hash::*;

mod manifest;
pub use manifest::*;
