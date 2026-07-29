//! The Curios core language: the term representation the elaborator produces and the kernel checks.
//!
//! This crate is **representation and primitive computation only**. It defines
//! [`Term`]/[`Subterm`] and its locally-nameless binder discipline ([`Scope`],
//! [`Telescope`], [`Bound`]), the primitive roster ([`Prim`]) and its
//! free-monoid spine algebra, algebraic universe [`Level`]s and the
//! [`UniverseContext`] declarations generalize into, the nominal registry
//! entries ([`InductDecl`], [`StructDecl`]) and the [`Polarity`] lattice they
//! carry, compiler names, and the printer.
//!
//! What it deliberately does *not* contain is every judgment: elaboration,
//! unification, zonking, conversion, reduction strategy, witness resolution,
//! and erasure all live in `curios-elab`, which depends on this crate. The
//! universe *solver* stays there too — a [`UniverseContext`] is data, and
//! deciding whether its constraints are satisfiable is a judgment over that
//! data.
//!
//! That split is the point: this crate is intended to become the trusted base,
//! so a rule that can admit a program belongs on the far side of it.
//!
//! One module is not trusted and is here only because the representation needs
//! it: `print` renders terms for diagnostics. Printing cannot admit a bad
//! program, and it lives here solely because [`Term`]'s `Display` is used
//! throughout the elaborator's error paths.
//!
//! The crate is a flat module space: every module re-exports at the root, so
//! consumers use `curios_core::Term`, not paths into the modules.

mod scope;
pub use scope::*;

mod nat;
pub use nat::*;

mod universe;
pub use universe::*;

mod prim;
pub use prim::*;

mod spine;
pub use spine::*;

mod free_monoid;
pub use free_monoid::*;

mod names;
pub use names::*;

mod term;
pub use term::*;

mod inductive;
pub use inductive::*;

mod structure;
pub use structure::*;

mod polarity;
pub use polarity::*;

mod print;
pub use print::*;
