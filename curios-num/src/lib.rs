//! The Curios numeric tower: the one crate that names `num-bigint` and `num-traits`.
//!
//! Two layers, each with its own reading key. [`Natural`] and [`Integer`] are *type-level* values — unbounded, pretending ℕ and ℤ, sealed newtypes whose magnitudes are private — and [`Floating`] is the binary32 model beside them, computed exactly over [`Natural`] and rounded once. The `scalar` functions are the *erased* carriers' exact semantics — `Nat` as `u32`, `Int` as `i32` — shared by every stage's constant folder so their arithmetic cannot drift from the backend's; each signature there says what a folder must do with a failure, and [`ScalarTrap`] names why one traps. The `archive` feature adds the rkyv proxies both magnitudes archive through.
//!
//! Why the dependency is named here and nowhere else, why the magnitudes are sealed rather than re-exported, and why the two layers are kept apart rather than expressed in one another are `README.md`'s decisions.

mod natural;
pub use natural::*;

mod integer;
pub use integer::*;

mod floating;
pub use floating::*;

mod scalar;
pub use scalar::*;

#[cfg(feature = "archive")]
mod archive;
#[cfg(feature = "archive")]
pub use archive::*;
