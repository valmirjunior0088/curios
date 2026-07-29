//! The Curios core language: the term representation the elaborator produces, and the kernel that re-checks it.
//!
//! This crate holds **the representation and the rules over it**. It defines
//! [`Term`]/[`Subterm`] and its locally-nameless binder discipline ([`Scope`],
//! [`Telescope`], [`Bound`]), the primitive roster ([`Prim`]) with its
//! free-monoid spine algebra and its folds ([`reduce_prim`]), algebraic
//! universe [`Level`]s and the [`UniverseContext`] declarations generalize
//! into, the nominal registry entries ([`InductDecl`], [`StructDecl`]) and the
//! [`Polarity`] lattice they carry, compiler names, and the printer — and, on
//! top of all that, the [`kernel`](Kernel) that decides whether a finished term
//! is well-typed.
//!
//! What it deliberately does *not* contain is anything that makes the *surface
//! language* work: elaboration, unification, zonking, witness resolution, and
//! erasure all live in `curios-elab`, which depends on this crate. The universe
//! *solver* stays there too — a [`UniverseContext`] is data, and deciding
//! whether a constraint set is satisfiable is inference over that data, not a
//! rule of the theory. The primitive folds are the boundary case, and
//! [`Reducer`] is where the line is drawn: what `2 + 2` folds to is arithmetic
//! on the representation and belongs here, while how far an operand reduces
//! before the fold sees it is a strategy each side supplies for itself.
//!
//! That split is the point. This crate is the trusted base, so a rule that can
//! admit a program belongs on this side of it and everything that merely makes
//! programs pleasant to write belongs on the far side.
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

mod reduce;
pub use reduce::*;

mod kernel;
pub use kernel::*;

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
