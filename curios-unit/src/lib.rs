//! The compilation unit: what one unit provides to its successors, one opaque artifact per stage, and the prefix of borrowed predecessors each stage is compiled against.
//!
//! A compilation is a set of units folded over a dependency order. Each stage takes a *scope* — what earlier units established — and one unit, and a [`Unit`] is defined by what it hands the next one rather than by what it is. The standard library is a unit; a package is a unit; the program you asked for is the unit with no successors, which is what lets it own the empty prefix and carry the entrypoint.
//!
//! # Why this is a crate, and not part of `curios-pipeline`
//!
//! The driver depends on `curios-cert`, and `curios-prelude-archive`'s build script has to construct a `Unit`. A build script reaching the kernel re-runs on every certifier edit, and re-running that one re-elaborates the whole standard library — the 469-second regression `curios-analysis` was split out to fix, arriving through a different door. So the unit and the stages that do not judge live below the kernel, and judgment is interleaved by the driver above it.
//!
//! The rule is checkable and is the same one `curios-prelude-archive` states for itself:
//!
//! ```sh
//! cargo tree -p curios-unit --edges normal   # must not contain curios-cert
//! ```
//!
//! # What a scope hands to each stage
//!
//! Not one merged value. Each stage receives every predecessor *borrowed*, because merging would copy the standard library into every compilation — the cost retiring the splice removed. And each receives them as its own opaque type rather than as anything this crate unpacks: `curios-text`'s resolution state and `curios-elab`'s erased arena keep their fields private, so [`Prefix`] hands each stage a slice of the type that stage owns and lets it build its own view.

mod prefix;
pub use prefix::*;

mod unit;
pub use unit::*;
