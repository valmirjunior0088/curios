//! The seams a *shared* analysis runs on, so one implementation can be driven by either checker.
//!
//! # Why some analyses are shared and others are written twice
//!
//! [`Reducer`](curios_core::Reducer) already draws this line for primitive folding: the arithmetic is algebra over the representation and belongs in this crate, while how far an operand reduces before a fold sees it is a strategy each side supplies for itself. The traits here extend that line to whole analyses, and the test for which side an analysis falls on is:
//!
//! > Do the two checkers see different inputs?
//!
//! Reduction, conversion, and the typing judgment stay **duplicated**. The elaborator's versions see metavariables, refinements, expected types, parked goals, and memoized derivations; the kernel's see finished terms and no caches at all. Different inputs and different strategies are where a systematic mistake is both likely and expensive, and where two verdicts are genuinely two samples.
//!
//! Index inversion, strict positivity, and size-change totality are the other case. All three run *post-zonk*, on final meta-free terms — which is exactly what the kernel is handed. Two runs of a total function on identical input is one sample, not two: duplicating them would buy a diff test, which property-testing the single implementation buys more cheaply, and would cost thousands of trusted lines written twice.
//!
//! # The concession this records
//!
//! [`Env`] is environment queries only — nothing it answers is a judgment, so an analysis needing no more than `Env` borrows no opinion from its driver. [`Judge`] adds conversion, which *is* a judgment, and an analysis requiring it takes its driver's word on convertibility. That is a real concession and the split exists to keep it visible in the type system rather than buried in a call graph: today `invert` is the only consumer of [`Judge`], and if its products are ever emitted as certificates the kernel type-checks instead, `Judge` losing its last implementor is the signal the concession is gone.
//!
//! # Errors belong to the driver
//!
//! Both traits report through an associated [`Env::Error`] rather than through [`ReduceError`](curios_core::ReduceError). The kernel's failures are `KernelError`s and the elaborator's are spanned diagnostics that name the offending term, and a shared analysis should not have to know which. This is the rule `ReduceError` already states from the other direction — a reducer reports what the *term* did, and the driver that owns the user-facing diagnostic decides how to phrase it.

use curios_core::{Free, Global, InductDecl, StructDecl, Term};

/// What a shared analysis may ask of the checker running it, beyond the terms it was handed.
///
/// Deliberately small, and for the same reason `Kernel` is: every method here is a way for an answer to come from something other than the term in hand. A new one should have to argue for itself.
pub trait Env {
    /// How this checker reports a failure. The kernel's is `KernelError`; the elaborator's is its spanned diagnostic.
    type Error;

    /// Reduce to weak-head normal form, then force a `rec` head — a position that demands a value rather than a normal form.
    ///
    /// Each side supplies its own strategy: which definitions unfold, what a step costs, whether a refinement is in scope. Only the *result* is shared.
    fn force(&mut self, term: &Term) -> Result<Term, Self::Error>;

    /// The type `name` was assumed at, or `None` for a name not in scope.
    fn assumption(&self, name: &Free) -> Option<&Term>;

    /// A fresh binder identity, rendering as `hint`.
    ///
    /// `&mut self` for the *implementors'* sake — both mint from an interior counter and could take `&self`, but the elaborator's method is spelled `&mut` and a trait should not force a signature change to satisfy it.
    fn fresh(&mut self, hint: Option<&str>) -> Free;

    /// What `name` unfolds to through its *definition* — never through a refinement. The two implementations are semantically identical, which is deliberate: a definitions-only reading needs no invariant about when the elaborator's refinement store happens to be empty.
    fn unfold(&self, name: &Free) -> Option<&Term>;

    /// The registry entry for an `induct` declaration, or `None` when the name is not one.
    fn induct_decl(&self, name: &Global) -> Option<&InductDecl>;

    /// The registry entry for a `struct` declaration, or `None` when the name is not one.
    fn struct_decl(&self, name: &Global) -> Option<&StructDecl>;
}

/// [`Env`], plus the one judgment a shared analysis is allowed to borrow.
///
/// Conversion is duplicated between the two checkers on purpose, so an analysis that asks for it here is trusting whichever implementation is driving it. See the module documentation on what that concession is and why it is a separate trait.
pub trait Judge: Env {
    /// Whether `this` and `that` are convertible *at* `type_`, so that proof irrelevance and eta fire at the terms' real sort.
    fn convert_at(&mut self, type_: &Term, this: &Term, that: &Term) -> Result<bool, Self::Error>;
}
