//! The seams a *shared* analysis runs on, so one implementation can be driven by either checker.
//!
//! # Why some analyses are shared and others are written twice
//!
//! [`Reducer`](curios_core::Reducer) already draws this line for intrinsic folding: the arithmetic is algebra over the representation and belongs to `curios-core`, while how far an operand reduces before a fold sees it is a strategy each side supplies for itself. The traits here extend that line to whole analyses, and the test for which side an analysis falls on is:
//!
//! > Do the two checkers see different inputs?
//!
//! Reduction, conversion, and the typing judgment stay **duplicated**. The elaborator's versions see metavariables, refinements, expected types, parked goals, and memoized derivations; the kernel's see finished terms and no caches at all. Different inputs and different strategies are where a systematic mistake is both likely and expensive, and where two verdicts are genuinely two samples.
//!
//! One part of conversion is shared all the same, because it is algebra rather than strategy: what two intrinsics are to each other — the carriers' peels, laws and views, and the congruence read off an operation's signature — which [`convert_intrinsics`](crate::convert_intrinsics) states once and each checker discharges by its own judgment. Two copies would buy a second transcription of one function, which a slip could make disagree without either copy holding a second opinion about arithmetic. What holds it is what holds any shared rule: the algebra's own tests over bare atoms and concrete values, and the law grid, held through both checkers, as integration evidence.
//!
//! Index inversion, strict positivity, and size-change totality are the other case: each is a total function of the terms it is handed, and what the checkers hand them differs only in what [`Env`] answers. Two runs of a total function on one input is one sample, not two: duplicating them would buy a diff test, which property-testing the single implementation buys more cheaply, and would cost thousands of trusted lines written twice.
//!
//! # The concession this records
//!
//! [`Env`] is environment queries only — nothing it answers is a judgment, so an analysis needing no more than `Env` borrows no opinion from its driver. [`Judge`] adds conversion, which *is* a judgment, and an analysis requiring it takes its driver's word on convertibility. That is a real concession and the split exists to keep it visible in the type system rather than buried in a call graph: today `invert` is the only consumer of [`Judge`], and if its products are ever emitted as certificates the kernel type-checks instead, `Judge` losing its last implementor is the signal the concession is gone.
//!
//! # Errors belong to the driver
//!
//! Both traits report through an associated [`Env::Error`] rather than through [`ReduceError`](curios_core::ReduceError). The kernel's failures are `curios_cert::Error`s and the elaborator's are spanned diagnostics that name the offending term, and a shared analysis should not have to know which. This is the rule `ReduceError` already states from the other direction — a reducer reports what the *term* did, and the driver that owns the user-facing diagnostic decides how to phrase it.

use curios_core::{Bound, Exhaustion, Free, Global, InductDecl, StructDecl, Subterm, Term};

/// Whether it is both meaningful and *safe* to hand `term` to [`Env::force`] — the guard a shared analysis takes before spending a reduction on a term it only wants to read.
///
/// Two halves, paired because every site that needs one needs both. The shape half asks whether the head could move at all; anything else is already weak-head normal, so forcing it spends budget to learn nothing. The scope half is a *safety* precondition rather than an optimization: a term whose [`reach`](Term::reach) is non-zero still sits under enclosing binders, and reduction assumes free occurrences — it would panic on a dangling index rather than refuse.
///
/// `Metavar` is excluded, which is the reading `whnf` itself takes — a metavariable is a stuck neutral, weak-head normal already. The kernel hands this seam meta-free terms; where the elaborator hands it one holding a metavariable, declining to force it leaves the term opaque, which is the refusing direction for every caller.
pub(crate) fn forceable(term: &Term) -> bool {
    matches!(
        &**term,
        Subterm::Var(_)
            | Subterm::Apply(_)
            | Subterm::Instance(_)
            | Subterm::Proj(_)
            | Subterm::Match(_)
            | Subterm::Let(_)
            | Subterm::Rec(_)
    ) && term.reach() == 0
}

/// What a shared analysis may ask of the checker running it, beyond the terms it was handed.
///
/// Deliberately small, and for the same reason `Kernel` is: every method here is a way for an answer to come from something other than the term in hand. A new one should have to argue for itself.
pub trait Env {
    /// How this checker reports a failure. The kernel's is `curios_cert::Error`; the elaborator's is its spanned diagnostic. Either says whether it is the budget's refusal, which an analysis reading a term as a [`Probe`](curios_core::Probe) propagates while it reads any other failure as nothing to read.
    type Error: Exhaustion;

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

    /// Whether `name` is a local the walk in progress opened, as opposed to a top-level name, whose meaning no case can refine. A local may carry a definition — the elaborator keeps a `let` as one, where the kernel hands an analysis its terms by value and answers none — which [`Env::unfold`] answers.
    fn is_local(&self, name: &Free) -> bool;

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
