//! The Curios numeric tower: the one crate that names `num-bigint` and `num-traits`.
//!
//! The pattern is `curios-archive`'s and `curios-profile`'s. Those crates are the workspace's only rkyv and `tracing` dependencies and name their pins in their own manifests; this is the same arrangement for arithmetic, and for the same reason — a dependency that exists in exactly one manifest cannot be added elsewhere without someone writing the version down again, which is a question a reviewer will ask. A `[workspace.dependencies]` row shares *configuration* and concentrates no authority at all.
//!
//! Unlike those two, this crate does not re-export what it owns. [`Natural`] and [`Integer`] are sealed newtypes whose magnitudes are private, so no crate above this one can name a `BigUint` or import a `num-traits` trait to call a method on one. That is what lets `num-traits` disappear from the workspace's code entirely: every use of it was a trait import — `Zero`, `One`, `ToPrimitive`, `FromPrimitive` — existing only to make a method callable on a bignum, and those methods are now inherent.
//!
//! # Two layers, deliberately separate
//!
//! [`Natural`] and [`Integer`] are *type-level* values: unbounded, pretending ℕ and ℤ, because a type-level natural bounded by a machine word would make a term's meaning depend on the host. The runtime's 31-bit range is enforced only where a literal must materialize, in erasure's narrowing and in the runtime's own overflow traps.
//!
//! The `scalar` functions are the other layer: the exact semantics of the *erased* carriers, where `Nat` is a `u32` that wraps and `Int` an `i32` that traps. Every stage's constant folder shares them so its arithmetic cannot drift from the backend's. Neither layer is expressible in the other, which is why `Natural`'s `-` panics on underflow while [`nat_sub`] saturates: they are different operations about different things.

mod natural;
pub use natural::*;

mod integer;
pub use integer::*;

mod flt;
pub use flt::*;

mod scalar;
pub use scalar::*;

#[cfg(feature = "archive")]
mod archive;
#[cfg(feature = "archive")]
pub use archive::*;
