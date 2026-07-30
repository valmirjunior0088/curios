//! The independent kernel: the judgments that decide whether a term is
//! well-typed, written against the representation and nothing else.
//!
//! The elaborator in `curios-elab` is a large, stateful program. It inserts
//! implicit arguments, invents and solves metavariables, parks and wakes
//! conversion goals, resolves witnesses, refines scrutinees inside match arms,
//! and memoizes almost all of it. Every one of those mechanisms exists to make
//! the *surface language* ergonomic, and every one of them is a way for a bad
//! program to be admitted. What this module provides is the other half of the
//! bargain: a second opinion that shares none of that machinery.
//!
//! The independence is structural, not a matter of discipline. This crate does
//! not depend on `curios-elab`, so nothing here can consult a metavariable
//! store, a refinement, or a cached elaboration — not because the code declines
//! to, but because those types are not in scope. A judgment the elaborator gets
//! wrong is re-decided here from the term alone.
//!
//! What the kernel *does* share is the representation: [`Term`], its binder
//! discipline, the primitive roster, and the primitive folds. Sharing a
//! representation is not sharing a judgment. Two checkers that disagree about a
//! term's type while agreeing on what a term *is* still catch each other's
//! mistakes; two that share the rule that admits a bad program catch nothing.
//! That line is why [`Reducer`](super::Reducer) exists, and it is why the match
//! dispatch in `whnf` is written out again here rather than lifted from the
//! elaborator's reducer, which it closely resembles.
//!
//! # Refusing beats guessing
//!
//! Where the elaborator cannot classify something it falls back conservatively
//! and carries on, because a diagnostic is worth more to a programmer than a
//! refusal. The kernel does the opposite: a shape it cannot classify is a
//! [`KernelError`], not a default. A guessed universe level is the unsound
//! direction — it claims a type is smaller than it is — and a checker that
//! guesses is not a second opinion. The cost is that the kernel may reject a
//! term the elaborator accepted; that is a disagreement to investigate, which
//! is exactly what a second opinion is for.

mod convert;
pub use convert::*;

mod infer;
pub use infer::*;

mod module;
pub use module::*;

mod sort;
pub use sort::*;

mod whnf;
pub use whnf::*;

use {
    super::{
        Atom, Env, Free, Global, InductDecl, Judge, ReduceError, Reducer, StructDecl, Term,
        UniverseContext, UniverseError,
    },
    curios_base::Entropy,
    std::{collections::HashMap, fmt},
};

/// Why the kernel refused a term.
///
/// Every variant is a refusal, never a warning: reaching one means the kernel
/// declined to certify the term, and a caller must treat that as rejection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KernelError {
    /// Reduction failed — the budget ran out, or a partial primitive was folded
    /// outside its domain.
    Reduce(ReduceError),
    /// A variable with no binder and no definition. In a well-formed module
    /// this cannot happen, which is why it is an error rather than a stuck
    /// neutral: the kernel is checking a *finished* term.
    Unbound(Free),
    /// A nominal type with no registry entry, so its fields, constructors, and
    /// result sort are all unknown.
    Undeclared(Global),
    /// A type whose sort the kernel could not determine. Guessing here is the
    /// unsound direction, so it refuses. See the module documentation.
    Unclassified(Term),
    /// A term used as a universe that is neither `Type` nor `Prop`.
    NotASort(Term),
    /// A term arrived with a type other than the one required of it.
    Mismatch {
        inferred: Box<Term>,
        expected: Box<Term>,
    },
    /// A head applied to arguments that is not a function.
    NotAFunction(Term),
    /// A term projected from that has no components.
    NotATuple(Term),
    /// A count that did not match: arguments against a telescope, a payload
    /// against a constructor's signature, a motive against a family's indices.
    Arity { expected: usize, actual: usize },
    /// A proposition eliminated into a relevant result while carrying
    /// something a program could read back. Permitted only for an empty
    /// proposition or a singleton whose payload is entirely determined.
    LargeElimination(Global),
    /// Elaboration-only syntax — a metavariable, an unresolved infix operator,
    /// or a polymorphic numeric literal — reached the kernel. The term was
    /// handed over before elaboration finished with it.
    NotCore(Term),
    /// An elimination with no arm for this constructor, no catch-all, and no
    /// clash making the case impossible at the scrutinee's indices. An arm may
    /// be legitimately absent only when its index targets cannot equal the
    /// actuals; anything else is a stuck term inhabiting the motive.
    MissingArm { family: Global, tag: Atom },
}

impl From<ReduceError> for KernelError {
    fn from(error: ReduceError) -> Self {
        KernelError::Reduce(error)
    }
}

impl From<UniverseError> for KernelError {
    fn from(error: UniverseError) -> Self {
        KernelError::Reduce(ReduceError::Universe(error))
    }
}

impl fmt::Display for KernelError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KernelError::Reduce(ReduceError::Exhausted) => {
                formatter.write_str("the kernel's reduction budget ran out")
            }
            KernelError::Reduce(_) => formatter.write_str("reduction failed in the kernel"),
            KernelError::Unbound(name) => write!(formatter, "unbound name `{name}`"),
            KernelError::Undeclared(name) => {
                write!(formatter, "no declaration registered for `{name}`")
            }
            KernelError::Unclassified(type_) => {
                write!(formatter, "cannot determine the sort of `{type_}`")
            }
            KernelError::NotASort(term) => write!(formatter, "`{term}` is not a universe"),
            KernelError::Mismatch { inferred, expected } => {
                write!(formatter, "expected `{expected}`, found `{inferred}`")
            }
            KernelError::NotAFunction(type_) => {
                write!(formatter, "`{type_}` is not a function type")
            }
            KernelError::NotATuple(type_) => write!(formatter, "`{type_}` has no components"),
            KernelError::Arity { expected, actual } => {
                write!(formatter, "expected {expected} of them, found {actual}")
            }
            KernelError::LargeElimination(name) => write!(
                formatter,
                "cannot eliminate the proposition `{name}` into a relevant result",
            ),
            KernelError::NotCore(term) => {
                write!(formatter, "`{term}` is elaboration-only syntax")
            }
            KernelError::MissingArm { family, tag } => write!(
                formatter,
                "no arm for `{tag}` of `{family}`, and its case is not impossible",
            ),
        }
    }
}

/// The kernel's side of the shared-analysis seam.
///
/// `assumption` reads the *locals* rather than [`Kernel::type_of`], because a
/// shared analysis asking what a binder was assumed at means the binder in
/// scope, not a top-level name that happens to share its spelling. That matches
/// what the elaborator's `Context::assumption` answers, which is the point of
/// the seam.
impl Env for Kernel {
    type Error = KernelError;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(self.reduce_forced(term.clone())?)
    }

    fn assumption(&self, name: &Free) -> Option<&Term> {
        self.local_type(name)
    }
}

impl Judge for Kernel {
    fn convert_at(&mut self, type_: &Term, this: &Term, that: &Term) -> Result<bool, Self::Error> {
        convert::convert(self, type_, this, that)
    }
}

/// A top-level name's entry: what it is, and what it unfolds to if anything.
///
/// The universe context is not decoration. A definition with universe
/// parameters is *not* unfoldable through a bare occurrence, because such an
/// occurrence denotes no particular instance; it reduces only through a
/// [`UniverseInst`](super::UniverseInst) that says which one.
struct Definition {
    type_: Term,
    /// `None` for something with a type and no body — a `foreign` declaration,
    /// or a name deliberately kept opaque.
    value: Option<Term>,
    universes: UniverseContext,
}

/// The kernel's context: what is in scope, what may unfold, and how much work
/// a judgment may spend.
///
/// Deliberately small. The elaborator's `Context` carries fifteen-odd stores —
/// caches, parked goals, refinement layers, a metavariable heap — and each is a
/// place where an answer can come from something other than the term in hand.
/// The kernel holds a local telescope, top-level definitions, the nominal
/// registry, and a budget. Growing this struct is how independence gets lost,
/// so a new field should have to argue for itself.
pub struct Kernel {
    /// Reduction steps a single judgment may spend. Restored at each
    /// declaration boundary by [`Kernel::restore_budget`].
    budget: u64,
    remaining: u64,
    /// Identities for binders the kernel opens itself, when comparing under a
    /// telescope and when eta-contracting. Seeded above every index the earlier
    /// stages minted, so a kernel-minted binder can never alias one in a term.
    fresh_names: Entropy,
    /// Binders opened by the walk in progress, with their types, outermost
    /// first. A local has a type and never a value: `let` substitutes rather
    /// than binding, so nothing in scope here can be unfolded.
    locals: Vec<(Free, Term)>,
    definitions: HashMap<Free, Definition>,
    inducts: HashMap<Global, InductDecl>,
    structs: HashMap<Global, StructDecl>,
}

impl Kernel {
    /// A kernel that may spend `budget` reduction steps per judgment.
    pub fn new(budget: u64) -> Self {
        Self {
            budget,
            remaining: budget,
            fresh_names: Entropy::new(),
            locals: Vec::new(),
            definitions: HashMap::new(),
            inducts: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    /// Raise the binder counter above every index minted by an earlier stage.
    ///
    /// The lowerer, the elaborator, and the archived prelude all mint into one
    /// identity space; a kernel that started at zero would mint binders that
    /// alias theirs, and an alias between two distinct binders is a capture.
    pub fn set_local_floor(&mut self, floor: usize) {
        self.fresh_names.seed(floor);
    }

    /// Record a top-level definition: `name : type_ = value`, generalized over
    /// `universes`.
    pub fn define(&mut self, name: &Free, type_: &Term, value: &Term, universes: &UniverseContext) {
        self.definitions.insert(
            name.clone(),
            Definition {
                type_: type_.clone(),
                value: Some(value.clone()),
                universes: universes.clone(),
            },
        );
    }

    /// Record a top-level name with a type and no body — a `foreign`
    /// declaration, or one kept opaque. It never unfolds, so it is a permanent
    /// neutral.
    pub fn declare(&mut self, name: &Free, type_: &Term, universes: &UniverseContext) {
        self.definitions.insert(
            name.clone(),
            Definition {
                type_: type_.clone(),
                value: None,
                universes: universes.clone(),
            },
        );
    }

    /// Register an `induct` declaration's registry entry.
    pub fn declare_induct(&mut self, name: &Global, declaration: &InductDecl) {
        self.inducts.insert(name.clone(), declaration.clone());
    }

    /// Register a `struct` declaration's registry entry.
    pub fn declare_struct(&mut self, name: &Global, declaration: &StructDecl) {
        self.structs.insert(name.clone(), declaration.clone());
    }

    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.inducts.get(name)
    }

    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.structs.get(name)
    }

    /// Open a binder: bring `name : type_` into scope for the walk in progress.
    ///
    /// Locals are a stack, and every judgment that opens one is responsible for
    /// closing it — take a [`Kernel::mark`] first and [`Kernel::retract`] to it
    /// afterwards, on every path including the failing one.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.locals.push((name.clone(), type_.clone()));
    }

    /// The current local depth, to be handed back to [`Kernel::retract`].
    pub(crate) fn mark(&self) -> usize {
        self.locals.len()
    }

    /// Close every binder opened since `mark`.
    pub(crate) fn retract(&mut self, mark: usize) {
        self.locals.truncate(mark);
    }

    /// The types of the binders currently in scope, outermost first. The
    /// conversion history keys on this: the same goal under a different context
    /// is a different goal.
    pub(crate) fn local_types(&self) -> Vec<Term> {
        self.locals.iter().map(|(_, type_)| type_.clone()).collect()
    }

    /// The identities of the binders currently in scope, outermost first —
    /// parallel to [`Kernel::local_types`]. What the conversion history renames
    /// away, so that a goal reached again on a later round of an unfolding
    /// cycle is recognized as the goal it already is.
    pub(crate) fn local_names(&self) -> Vec<Free> {
        self.locals.iter().map(|(name, _)| name.clone()).collect()
    }

    /// The type `name` was opened at, if it is a binder currently in scope.
    ///
    /// Innermost first — which cannot actually matter, since binder identities
    /// are minted unique, but scanning in that order means the rule does not
    /// depend on that being true.
    pub(crate) fn local_type(&self, name: &Free) -> Option<&Term> {
        self.locals
            .iter()
            .rev()
            .find(|(bound, _)| bound == name)
            .map(|(_, type_)| type_)
    }

    /// The type `name` was bound or declared at. Locals shadow definitions.
    pub(crate) fn type_of(&self, name: &Free) -> Option<&Term> {
        self.local_type(name)
            .or_else(|| self.definitions.get(name).map(|entry| &entry.type_))
    }

    /// The universe scheme `name` was generalized under, for a use that states
    /// its own instance.
    pub(crate) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.definitions
            .get(name)
            .map(|entry| (&entry.type_, &entry.universes))
    }

    /// Charge one reduction step, failing when the budget is spent.
    ///
    /// The kernel is not strongly normalizing and does not pretend to be: a
    /// non-productive `rec` reduces forever. The budget is what makes every
    /// judgment terminate, and it is deterministic — the same program spends
    /// the same steps on every machine — so exhausting it is a fact about the
    /// program, not about the host that checked it.
    pub(crate) fn spend(&mut self) -> Result<(), ReduceError> {
        match self.remaining {
            0 => Err(ReduceError::Exhausted),
            remaining => {
                self.remaining = remaining - 1;
                Ok(())
            }
        }
    }

    /// A fresh binder identity, rendering as `hint`.
    pub(crate) fn fresh(&self, hint: Option<&str>) -> Free {
        let index = u32::try_from(self.fresh_names.fresh()).expect("binder space exhausted");

        Free::local(index, hint)
    }

    /// What `name` unfolds to through a bare occurrence.
    ///
    /// A definition with universe parameters is withheld: see [`Definition`].
    pub(crate) fn value(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .filter(|definition| definition.universes.parameter_count == 0)
            .and_then(|definition| definition.value.as_ref())
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one
    /// position a polymorphic definition may be unfolded from.
    pub(crate) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .and_then(|definition| definition.value.as_ref())
    }

    /// Restore the full budget for a new judgment.
    pub fn restore_budget(&mut self) {
        self.remaining = self.budget;
    }
}
