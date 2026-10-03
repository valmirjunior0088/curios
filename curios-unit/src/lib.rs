//! The compilation unit: what one unit provides to its successors, one opaque artifact per stage, and the borrowed predecessors each stage is compiled against.
//!
//! A compilation is a set of units folded over a dependency order. Each stage takes its *predecessors* — what earlier units established — and one unit, and a [`Unit`] is defined by what it hands the next one rather than by what it is. The standard library is a unit; a package is a unit; the program you asked for is the unit with no successors, which is what lets it carry the entrypoint.
//!
//! A unit's serialized form is a *stored unit*: a [`Record`] of what it was compiled from, framed ahead of the archived unit in one file. The store files a [`Unit`] under a slot, and the fixed prelude is imaged the same way holding an [`Uncertified`] one — written below the certifier, and certified where `curios-prelude` restores it — and the framing lives here because those two producers may not depend on each other — see `stored`.
//!
//! This crate sits below the kernel — the unit and the stages that do not judge live here, and judgment is interleaved by the driver above it — for the reason `README.md` states. The rule is checkable and is the same one `curios-prelude-archive` states for itself:
//!
//! ```sh
//! cargo tree -p curios-unit --edges normal   # must not contain curios-cert
//! ```
//!
//! # What the predecessors hand to each stage
//!
//! Not one merged value. Each stage receives every predecessor *borrowed*, as its own opaque type rather than as anything this crate unpacks: `curios-text`'s resolution state and `curios-elab`'s erased arena keep their fields private, so [`Predecessors`] hands each stage a slice of the type that stage owns and lets it build its own view. Why borrowed and why opaque are `README.md`'s decisions.

mod predecessors;
pub use predecessors::*;

mod unit;
pub use unit::*;

mod stored;
pub use stored::*;
