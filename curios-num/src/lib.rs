//! The Curios numeric tower: the one crate that names `num-bigint` and `num-traits`.
//!
//! Two layers, each with its own reading key. [`Natural`] and [`Integer`] are *type-level* values — unbounded, pretending ℕ and ℤ, sealed newtypes whose magnitudes are private — and [`Floating`] is the binary64 model beside them, computed exactly over [`Natural`] and rounded once. The same types are the *erased* carriers' exact semantics — `Nat` as [`Natural`] and `Int` as [`Integer`], unbounded too, as the running program is — whose methods every stage's constant folder calls so their arithmetic cannot drift from Core's; the `scalar` module states the contract a signature keeps, and [`ScalarTrap`] names why an operation traps. [`Binary`] is the packed value a `Bits` or `Bytes` is. The `archive` feature adds the rkyv proxies both magnitudes archive through.
//!
//! Why the magnitudes are sealed rather than re-exported, and why one carrier serves both layers rather than a layer of free functions beside it, are `README.md`'s decisions; why the dependency is named here and nowhere else is `documentation/design/one-crate-is-the-authority-for-one-external-concern.md`'s.

mod natural;
pub use natural::*;

mod integer;
pub use integer::*;

mod floating;
pub use floating::*;

mod binary;
pub use binary::*;

mod scalar;
pub use scalar::*;

#[cfg(feature = "archive")]
mod archive;
#[cfg(feature = "archive")]
pub use archive::*;
