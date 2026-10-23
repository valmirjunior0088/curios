//! What a Curios package is, and everything that reads one: the `curios.toml` manifest, the walk that decides which manifest governs an invocation, the resolver that turns a declared dependency into a module tree, and the store the results are filed in.
//!
//! Those four are faces of a single question — what is in this compilation, and where did each part come from — and why they are one crate, which `curios-pipeline` never learns exists, is `README.md`'s decision.
//!
//! This crate is the workspace's authority for TOML, as `documentation/design/one-crate-is-the-authority-for-one-external-concern.md` decides; `/std`'s TOML codec is a guest library — it runs in the compiled program, not in the compiler — and cannot serve the driver.
//!
//! Every refusal here fires before elaboration and names both parties. A manifest declaring two modes, a name no path could spell, a dependency row missing the pin its source requires: each is diagnosed against the file somebody wrote, never surfaced downstream as an unbound name.

mod compatibility;
pub use compatibility::*;

mod compiler;
pub use compiler::*;

mod curate;
pub use curate::*;

mod govern;
pub use govern::*;

mod graph;
pub use graph::*;

mod hash;
pub use hash::*;

mod layout;
pub use layout::*;

mod manifest;
pub use manifest::*;

mod new;
pub use new::*;

mod pin;
pub use pin::*;

mod selection;
pub use selection::*;

mod served;
pub use served::*;

mod store;
pub use store::*;
