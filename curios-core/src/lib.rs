//! The Curios core language: the term representation both checkers build on.
//!
//! This crate holds **what a term is** and nothing that judges one. It defines [`Term`]/[`Subterm`] and its locally-nameless binder discipline ([`Scope`], [`Telescope`], [`Bound`]), the intrinsic roster ([`Intrinsic`]) with its free-monoid spine algebra and its folds ([`reduce_intrinsic`]), algebraic universe [`Level`]s and the [`UniverseContext`] declarations generalize into, the nominal registry entries ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]) and the [`Polarity`] lattice they carry, the finished-program shape both checkers walk ([`Module`], [`Program`], [`Item`], [`Definition`]), compiler names, and the printer.
//!
//! The judgments live on either side of it, as `README.md` states. `curios-analysis` holds the rules both checkers run and `curios-cert` the kernel that only one of them does — together, every rule that can admit a program — and `curios-elab` holds everything that makes the *surface language* work: elaboration, unification, zonking, witness resolution, erasure, and the universe *solver*. Both depend on this crate and never the reverse. A [`UniverseContext`] is data the elaborator's solver infers over and each checker decides the satisfiability of for itself, and [`Reducer`] is the seam between a fold's arithmetic and a checker's reduction strategy; both lines are `README.md`'s decisions.
//!
//! Two modules are not trusted, `print` and `builders` — the constructors the surface lowering and the elaborator both build terms with, such as [`str_literal`] and [`Term::num_lit`] — and why they live here is `README.md`'s decision. Neither can admit a bad program: no judgment calls a builder, and printing decides nothing.
//!
//! The crate is a flat module space: every module re-exports at the root, so consumers use `curios_core::Term`, not paths into the modules.

mod scope;
pub use scope::*;

mod concept;
pub use concept::*;

mod items;
pub use items::*;

mod item_run;
pub use item_run::*;

mod read_table;
pub use read_table::*;

mod declarations;
pub use declarations::*;

mod module;
pub use module::*;

mod places;
pub use places::*;

mod order;
pub use order::*;

mod atoms;
pub use atoms::*;

mod linear;
pub use linear::*;

mod nat;
pub use nat::*;

mod int;
pub use int::*;

mod lattice;
pub use lattice::*;

mod universe;
pub use universe::*;

mod intrinsic;
pub use intrinsic::*;

mod spine;
pub use spine::*;

mod words;
use words::*;

mod free_monoid;
pub use free_monoid::*;

mod cost;
pub use cost::*;

mod consumption;
pub use consumption::*;

mod reduce;
pub use reduce::*;

mod machine;
pub use machine::*;

mod names;
pub use names::*;

mod unresolved;
pub use unresolved::*;

mod extent;
pub use extent::*;

mod witness;
pub use witness::*;

mod trace;
pub use trace::*;

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

mod variance;
pub use variance::*;

mod print;
pub use print::*;

mod builders;
pub use builders::*;

#[cfg(feature = "test-support")]
pub mod test_support;
