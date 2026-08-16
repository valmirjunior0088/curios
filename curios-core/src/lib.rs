//! The Curios core language: the term representation both checkers build on.
//!
//! This crate holds **what a term is** and nothing that judges one. It defines [`Term`]/[`Subterm`] and its locally-nameless binder discipline ([`Scope`], [`Telescope`], [`Bound`]), the intrinsic roster ([`Intrinsic`]) with its free-monoid spine algebra and its folds ([`reduce_intrinsic`]), algebraic universe [`Level`]s and the [`UniverseContext`] declarations generalize into, the nominal registry entries ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]) and the [`Polarity`] lattice they carry, the finished-program shape both checkers walk ([`Module`], [`Item`], [`Definition`]), compiler names, and the printer.
//!
//! The judgments live on either side of it. `curios-analysis` holds the rules both checkers run and `curios-cert` the kernel that only one of them does — together, every rule that can admit a program — and `curios-elab` holds everything that makes the *surface language* work: elaboration, unification, zonking, witness resolution, erasure, and the universe *solver*. Solving and satisfiability part company here: a [`UniverseContext`] is data, and *inferring* the instance a program meant is inference over that data, so it sits with the elaborator; deciding whether the resulting constraint set is satisfiable admits a program, so it is shared. Both depend on this crate and never the reverse. The intrinsic folds are the boundary case, and [`Reducer`] is where the line is drawn: what `2 + 2` folds to is arithmetic on the representation and belongs here, while how far an operand reduces before a fold sees it is a strategy each checker supplies for itself.
//!
//! One module is not trusted and is here only because the representation needs it: `print` renders terms for diagnostics. Printing cannot admit a bad program, and it lives here solely because [`Term`]'s `Display` is used throughout the elaborator's error paths.
//!
//! The crate is a flat module space: every module re-exports at the root, so consumers use `curios_core::Term`, not paths into the modules.

mod scope;
pub use scope::*;

mod concept;
pub use concept::*;

mod module;
pub use module::*;

mod nat;
pub use nat::*;

mod universe;
pub use universe::*;

mod intrinsic;
pub use intrinsic::*;

mod spine;
pub use spine::*;

mod free_monoid;
pub use free_monoid::*;

mod cost;
pub use cost::*;

mod consumption;
pub use consumption::*;

mod retention;
pub use retention::*;

mod reduce;
pub use reduce::*;

mod machine;
pub use machine::*;

mod names;
pub use names::*;

mod term;
pub use term::*;

mod walk;
pub use walk::*;

mod inductive;
pub use inductive::*;

mod structure;
pub use structure::*;

mod polarity;
pub use polarity::*;

mod print;
pub use print::*;
