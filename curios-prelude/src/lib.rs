//! Build-scoped archived ownership of Curios's fixed `/sys`, `/syn`, and `/std` prelude.

mod archive;
pub(crate) use archive::*;

mod syntax;
pub use syntax::*;

mod restore;
pub use restore::*;
