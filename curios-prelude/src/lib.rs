//! Build-scoped archived ownership of Curios's fixed `/sys`, `/syn`, and `/std` prelude.
//!
//! `/sys` mirrors the host store one declaration per wire row, and every row returns an `Io` — a description of the call, not its result. `/sys/Io` holds the sequencing (`pure`, `bind`) and nothing else; `/std` owns the taxonomy that wraps them. See this crate's README for the placement law, and "Effects are descriptions, and the carrier has no eliminator" in `documentation/DESIGN.md` for the invariant those wrappers rest on.

mod archive;
pub(crate) use archive::*;

mod syntax;
pub use syntax::*;

mod restore;
pub use restore::*;
