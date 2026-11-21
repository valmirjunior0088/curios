//! What a Curios package is, and everything that reads one: the `curios.toml` manifest, the walk that decides which manifest governs an invocation, the resolver that turns a declared dependency into a module tree, and where what it compiles and fetches is kept: the products a command emits and the cabinet kept for reuse.
//!
//! Those four are faces of a single question — what is in this compilation, and where did each part come from — and why they are one crate, which `curios-pipeline` never learns exists, is `README.md`'s decision.
//!
//! This crate is the workspace's authority for TOML, as `documentation/design/one-crate-is-the-authority-for-one-external-concern.md` decides; `/std`'s TOML codec is a guest library — it runs in the compiled program, not in the compiler — and cannot serve the driver.
//!
//! Every refusal here fires before elaboration and names both parties. A manifest declaring two modes, a name no path could spell, a dependency row missing the pin its source requires: each is diagnosed against the file somebody wrote, never surfaced downstream as an unbound name.

mod cabinet;
pub use cabinet::{Cabinet, Families, Runs};

mod compatibility;
pub use compatibility::CompilerVersion;
use compatibility::*;

mod compiler;
pub use compiler::compiler;

mod curate;
use curate::*;
pub use curate::{
    Acquisition, Curated, Fetch, ModuleAcquisition, Provider, curate, declared_modules,
};

mod decode;
use decode::*;

mod format;
pub use format::FormatSettings;
use format::*;

mod govern;
use govern::*;
pub use govern::{Governing, nearest_manifest};

mod graph;
pub use graph::order;

mod hash;
pub use hash::{FileHash, TreeHash};

mod layout;
pub use layout::package_at;
use layout::*;

mod manifest;
use manifest::*;
pub use manifest::{
    Delivery, Dependency, EXTENSION, Executable, Foreign, Gateway, LIBRARY, MANIFEST, Module,
    PAGE_FILES, Package, Snapshot, Target, Umbrella,
};

mod new;
pub use new::scaffold;

mod pin;
pub use pin::{Pinned, Pinning, Probe, Repoint, Subject, Transport, pin};

mod products;
pub use products::Products;
use products::*;

mod reach;
pub use reach::{Reach, Reached, tests_filed_as};

mod refused;
pub use refused::{At, Refused, Row, Step};

mod selection;
pub use selection::{Entire, Entry, Home, Library, Named, Program, Selection, Spelling, Supplied};

mod served;
pub use served::Served;
