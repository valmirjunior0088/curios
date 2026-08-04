//! The independent kernel: the judgments that decide whether a term is well-typed, written against the representation and nothing else.
//!
//! The elaborator in `curios-elab` is a large, stateful program. It inserts implicit arguments, invents and solves metavariables, parks and wakes conversion goals, resolves witnesses, refines scrutinees inside match arms, and memoizes almost all of it. Every one of those mechanisms exists to make the *surface language* ergonomic, and every one of them is a way for a bad program to be admitted. What this module provides is the other half of the bargain: a second opinion that shares none of that machinery.
//!
//! The independence is structural, not a matter of discipline. This crate does not depend on `curios-elab`, so nothing here can consult a metavariable store, a refinement, or a cached elaboration — not because the code declines to, but because those types are not in scope. A judgment the elaborator gets wrong is re-decided here from the term alone.
//!
//! What the kernel *does* share is the representation: [`Term`], its binder discipline, the primitive roster, and the primitive folds. Sharing a representation is not sharing a judgment. Two checkers that disagree about a term's type while agreeing on what a term *is* still catch each other's mistakes; two that share the rule that admits a bad program catch nothing. That line is why [`Reducer`](curios_core::Reducer) exists, and it is why the match dispatch in `whnf` is written out again here rather than lifted from the elaborator's reducer, which it closely resembles.
//!
//! # Refusing beats guessing
//!
//! Where the elaborator cannot classify something it falls back conservatively and carries on, because a diagnostic is worth more to a programmer than a refusal. The kernel does the opposite: a shape it cannot classify is a [`KernelError`], not a default. A guessed universe level is the unsound direction — it claims a type is smaller than it is — and a checker that guesses is not a second opinion. The cost is that the kernel may reject a term the elaborator accepted; that is a disagreement to investigate, which is exactly what a second opinion is for.

mod at;
pub(crate) use at::*;

mod convert;
pub use convert::*;

mod globals;
use globals::Globals;

mod infer;
pub use infer::*;

mod memos;
use memos::Memos;

mod module;
pub(crate) use module::*;

mod positions;
use positions::Positions;

mod scope;
use scope::Scope;

mod sort;
pub use sort::*;

mod spend;
pub(crate) use spend::Replay;
use spend::Spend;

mod whnf;
pub(crate) use whnf::*;

use {
    crate::{Env, Judge, entails},
    crate::{Erased, erased_half},
    curios_core::{
        Atom, Free, Global, InductDecl, Level, LevelHead, Polarity, ReduceError, Reducer,
        StructDecl, Term, UniverseConstraint, UniverseContext, UniverseError,
    },
    std::{collections::HashMap, fmt},
};

/// Why the kernel refused a term.
///
/// Every variant is a refusal, never a warning: reaching one means the kernel declined to certify the term, and a caller must treat that as rejection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KernelError {
    /// Reduction failed — the budget ran out, or a partial primitive was folded outside its domain.
    Reduce(ReduceError),
    /// A variable with no binder and no definition. In a well-formed module this cannot happen, which is why it is an error rather than a stuck neutral: the kernel is checking a *finished* term.
    Unbound(Free),
    /// A nominal type with no registry entry, so its fields, constructors, and result sort are all unknown.
    Undeclared(Global),
    /// A type whose sort the kernel could not determine. Guessing here is the unsound direction, so it refuses. See the module documentation.
    Unclassified(Term),
    /// A term used as a universe that is neither `Type` nor `Prop`.
    NotASort(Term),
    /// An elimination's motive is not a well-typed function landing in a sort. The motive is a claim the term makes about its own result — `infer` reads the elimination's type off it and `Sort::of` classifies a type-valued `match` by it — so a motive stating one sort while its arms inhabit another would be believed by both.
    NotAMotive(Term),
    /// A term arrived with a type other than the one required of it.
    Mismatch {
        inferred: Box<Term>,
        expected: Box<Term>,
    },
    /// A head applied to arguments that is not a function.
    NotAFunction(Term),
    /// A term projected from that has no components.
    NotATuple(Term),
    /// A count that did not match: arguments against a telescope, a payload against a constructor's signature, a motive against a family's indices.
    Arity { expected: usize, actual: usize },
    /// A proposition eliminated into a relevant result while carrying something a program could read back. Permitted only for an empty proposition or a singleton whose payload is entirely determined.
    LargeElimination(Global),
    /// Elaboration-only syntax — a metavariable, an unresolved infix operator, or a polymorphic numeric literal — reached the kernel. The term was handed over before elaboration finished with it.
    NotCore(Term),
    /// A family that declares one constructor tag more than once. Every lookup resolves a tag by first match, so a repeat hides a constructor rather than adding one — and the coverage rule then answers about the first entry once per entry, reporting a family empty at an index its own later entry constructs at.
    RepeatedTag(Atom),
    /// An elimination with no arm for this constructor, no catch-all, and no clash making the case impossible at the scrutinee's indices. An arm may be legitimately absent only when its index targets cannot equal the actuals; anything else is a stuck term inhabiting the motive.
    MissingArm { family: Global, tag: Atom },
    /// A recursive member that is a proof or a type, in a group whose recursion does not descend. Assuming such a member at its declared type is what certifies `rec f : False = f` — erasure deletes proofs and types wholesale, so a non-descending one proves anything. A non-descending *value* recursion is not an error: `rec` is general recursion by design, and a program that loops is only a program that loops.
    NotDescending { type_: Box<Term> },
    /// A declaration that is not strictly positive: `part` of `name` reaches back to `name` at a non-accepting polarity. Without this gate, `induct Bad | c(f : (Bad) -> False) end` inhabits `False` in four lines with no recursion at all.
    NotPositive {
        name: Global,
        part: String,
        polarity: Polarity,
    },
    /// A constructor payload, uniform parameter, or field whose level exceeds the declaring family's result sort — the size condition that keeps an inductive from containing the universe it lives in.
    Oversized { domain: Level, bound: Level },
    /// A proof or a type that reaches something not known to terminate, or that is such a thing itself — an inline `rec` group that does not descend, or a `Prim::Exit`. Erasure deletes both halves, so a proof that may not terminate proves anything and a type that may not terminate reties the negative knot positivity forbids. `reached` names the offending definition, or is absent when the position is partial in itself and there is no name to blame.
    NotTotal {
        erased: crate::Erased,
        reached: Option<Global>,
    },
    /// A field of a `Prop`-sorted structure that is not a proof. Irrelevance identifies every inhabitant of a proposition, while projection reads a field back out without meeting any elimination guard, so an informative field hands two convertible values to the same projection — a type-valued field included.
    Informative { field: Box<Term> },
    /// A declaration whose universe constraints name something the declaration does not have: a parameter past its own count, or a metavariable elaboration should have solved. Either way the context cannot be instantiated, so assuming it means assuming something with no meaning.
    UnclosedUniverses,
    /// A declaration whose own universe constraints have no solution. The kernel *assumes* an item's constraints while checking it, so an unsatisfiable set is a hypothesis set from which everything follows: level questions stop being answered by the hierarchy and start being answered by the contradiction.
    UnsatisfiableUniverses,
    /// A universe instance whose stated levels do not satisfy the scheme's constraint set. The scheme declared `lower ≤ upper` over its parameters; at this instance's levels, under the hypotheses of the item being checked, the inequality does not hold — which is the route back to the paradox the hierarchy exists to exclude.
    UniverseInstance { lower: Level, upper: Level },
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
            KernelError::NotAMotive(term) => write!(
                formatter,
                "`{term}` is not a valid motive: it must be well-typed and land in a sort",
            ),
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
            KernelError::RepeatedTag(tag) => {
                write!(
                    formatter,
                    "constructor tag `{tag}` is declared more than once"
                )
            }
            KernelError::MissingArm { family, tag } => write!(
                formatter,
                "no arm for `{tag}` of `{family}`, and its case is not impossible",
            ),
            KernelError::NotDescending { type_ } => write!(
                formatter,
                "a recursive proof or type at `{type_}` does not descend",
            ),
            KernelError::UnclosedUniverses => write!(
                formatter,
                "this declaration's universe constraints name a parameter it does not declare",
            ),
            KernelError::UnsatisfiableUniverses => write!(
                formatter,
                "this declaration's universe constraints have no solution",
            ),
            KernelError::Informative { field } => write!(
                formatter,
                "a `Prop` structure carries an informative field at `{field}`",
            ),
            KernelError::NotTotal {
                erased,
                reached: Some(reached),
            } => write!(
                formatter,
                "a {erased} position reaches `{reached}`, which is not known to terminate",
            ),
            KernelError::NotTotal {
                erased,
                reached: None,
            } => write!(
                formatter,
                "a {erased} position does not terminate: it is a non-descending recursion or an exit",
            ),
            KernelError::NotPositive {
                name,
                part,
                polarity,
            } => write!(
                formatter,
                "`{part}` of `{name}` reaches back to it at {polarity:?}, which is not strictly positive",
            ),
            KernelError::Oversized { domain, bound } => write!(
                formatter,
                "a declaration domain at level `{domain}` exceeds its family's `{bound}`",
            ),
            KernelError::UniverseInstance { lower, upper } => write!(
                formatter,
                "this instance does not satisfy its scheme's `{lower} <= {upper}`",
            ),
        }
    }
}

/// The kernel's side of the shared-analysis seam.
///
/// `assumption` reads the *locals* rather than [`Kernel::type_of`], because a shared analysis asking what a binder was assumed at means the binder in scope, not a top-level name that happens to share its spelling. That matches what the elaborator's `Context::assumption` answers, which is the point of the seam.
impl Env for Kernel {
    type Error = KernelError;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(self.reduce_forced(term.clone())?)
    }

    fn assumption(&self, name: &Free) -> Option<&Term> {
        self.local_type(name)
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        Kernel::fresh(self, hint)
    }

    fn unfold(&self, name: &Free) -> Option<&Term> {
        self.value_at(name)
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        Kernel::induct_decl(self, name)
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        Kernel::struct_decl(self, name)
    }

    fn effect_memo(&mut self) -> &mut HashMap<Free, bool> {
        self.globals.effects_mut()
    }
}

impl Judge for Kernel {
    fn convert_at(&mut self, type_: &Term, this: &Term, that: &Term) -> Result<bool, Self::Error> {
        convert::convert(self, type_, this, that)
    }
}

/// The kernel's context: what is in scope, what may unfold, and how much work a judgment may spend.
///
/// Deliberately small, and now deliberately *composed*. The elaborator's `Context` carries fifteen-odd stores — caches, parked goals, refinement layers, a metavariable heap — and each is a place where an answer can come from something other than the term in hand. This held seventeen loose fields, which is the same shape read from the other end: five independent jobs whose invariants were spread across the methods that happened to touch them. Each is now a component that states its own, and what remains here is the composition and the couplings that genuinely cross it.
///
/// The one such coupling is worth naming, because it is why [`Globals::insert`] reports rather than acts: overwriting a definition invalidates every remembered reduct, which is a fact about [`Globals`] *and* [`Memos`] and therefore belongs to neither.
///
/// Growing this struct is still how independence gets lost. A new *component* should have to argue for itself the way a new field used to.
pub struct Kernel {
    /// What the walk in progress has opened.
    scope: Scope,
    /// What a judgment may consume, and what it has.
    spend: Spend,
    /// Remembered weak-head reducts, replayed rather than re-derived.
    memos: Memos,
    /// The erased positions this walk recorded — an output, not an input.
    positions: Positions,
    /// Top-level definitions and the nominal registry.
    globals: Globals,
    /// The constraint set of the item being checked — its own declared hypotheses, assumed while its parameters are held abstract. A generic definition is valid exactly when it checks *under* its constraints, so the level judgments below consult these; discarding them was the route by which a correct polymorphic definition was refused.
    ///
    /// The one field with no component of its own: it is a single vector replaced wholesale at each declaration boundary, and wrapping it would state nothing the type does not.
    assumed: Vec<UniverseConstraint>,
}

impl Kernel {
    /// A kernel that may spend `budget` reduction steps per judgment.
    pub fn new(budget: u64) -> Self {
        Self {
            scope: Scope::default(),
            spend: Spend::new(budget),
            memos: Memos::new(true),
            positions: Positions::default(),
            globals: Globals::default(),
            assumed: Vec::new(),
        }
    }

    /// A kernel whose evaluation memos are off — every reduction re-derived from scratch. Exists for one purpose: asserting that memoization changes no verdict.
    pub fn uncached(budget: u64) -> Self {
        Self {
            memos: Memos::new(false),
            ..Self::new(budget)
        }
    }

    /// The remembered reduct of `name`'s body, with the replayed computation's whole consumption charged.
    ///
    /// Looking one up and charging it are two components' jobs, joined here: [`Memos`] can hand back a [`Replay`] and cannot apply one.
    pub(crate) fn unfold_hit(&mut self, name: &Free) -> Option<Result<Term, ReduceError>> {
        let replay = self.memos.unfold(name)?;

        Some(self.spend.charge(replay))
    }

    /// Remember what `name`'s body reduces to, and what computing it consumed.
    pub(crate) fn unfold_store(&mut self, name: Free, replay: Replay) {
        self.memos.store_unfold(name, replay);
    }

    /// The remembered weak-head reduct of a local-free `term`, per entry point, with the replayed computation's whole consumption charged.
    pub(crate) fn whnf_hit(
        &mut self,
        term: &Term,
        forced: bool,
    ) -> Option<Result<Term, ReduceError>> {
        let replay = self.memos.whnf(term, forced)?;

        Some(self.spend.charge(replay))
    }

    /// Remember a local-free `term`'s weak-head reduct and its consumption.
    pub(crate) fn whnf_store(&mut self, term: Term, forced: bool, replay: Replay) {
        self.memos.store_whnf(term, forced, replay);
    }

    /// A consumption snapshot, for measuring what a computation charges.
    pub(crate) fn consumption(&self) -> (u64, usize) {
        self.spend.snapshot()
    }

    /// The [`Replay`] for `reduct`, measured against the snapshot taken before the computation ran.
    pub(crate) fn replay_since(&self, reduct: Term, before: (u64, usize)) -> Replay {
        self.spend.replay_since(reduct, before)
    }

    /// Raise the binder counter above every index minted by an earlier stage.
    pub fn set_local_floor(&mut self, floor: usize) {
        self.spend.set_local_floor(floor);
    }

    /// Assume `universes`' constraints for the item about to be checked, replacing the previous item's. Like [`Kernel::restore_budget`], this is a declaration-boundary reset.
    pub fn assume_universes(&mut self, universes: &UniverseContext) {
        self.assumed = universes.constraints.clone();
    }

    /// Whether `lower ≤ upper` — structurally, or through the assumed constraints of the item being checked.
    pub(crate) fn level_leq(&self, lower: &Level, upper: &Level) -> bool {
        lower.structurally_leq(upper) || entails(&self.assumed, lower, upper)
    }

    /// Whether two levels are equal under the assumed constraints — mutual [`Kernel::level_leq`], with syntactic equality as the fast path.
    pub(crate) fn level_eq(&self, left: &Level, right: &Level) -> bool {
        left == right || (self.level_leq(left, right) && self.level_leq(right, left))
    }

    /// [`Kernel::level_eq`] pointwise over two instance vectors.
    pub(crate) fn levels_eq(&self, left: &[Level], right: &[Level]) -> bool {
        left.len() == right.len()
            && left
                .iter()
                .zip(right)
                .all(|(this, that)| self.level_eq(this, that))
    }

    /// Verify a stated instance satisfies its scheme's constraint set: each declared `lower ≤ upper`, instantiated at this occurrence's levels, must hold under the assumed constraints of the item being checked.
    ///
    /// A constraint level naming a parameter the instance does not supply is refused rather than kept: an unsubstituted scheme parameter would be misread as one of the ambient item's, which is the accepting direction.
    ///
    /// The instance's *width* is checked first, and separately, because the constraint loop cannot see it. A scheme with an empty constraint set never enters the guard above at all, and the guard only ever covered levels appearing in constraints — while every one of the declaration's own terms is instantiated at these same levels by the callers below. There an unsupplied parameter is not refused but renumbered: `instantiate_universe_levels_scoped` shifts it down by the instance's width, which is the correct de Bruijn step for a full instance and a capture for a short one, landing the declaration's parameter on the ambient item's. This is the same hazard the paragraph above names, at the position it does not reach.
    pub(crate) fn check_instance(
        &self,
        context: &UniverseContext,
        levels: &[Level],
    ) -> Result<(), KernelError> {
        if levels.len() != context.parameter_count {
            return Err(KernelError::Arity {
                expected: context.parameter_count,
                actual: levels.len(),
            });
        }

        let instantiate = |level: &Level| -> Result<Level, KernelError> {
            if level.params().any(|param| param.0 >= levels.len()) {
                return Err(KernelError::UniverseInstance {
                    lower: level.clone(),
                    upper: level.clone(),
                });
            }

            Ok(level.substitute(|head| match head {
                LevelHead::Param(param) => levels.get(param.0).cloned(),
                LevelHead::Meta(_) => None,
            })?)
        };

        for constraint in &context.constraints {
            let lower = instantiate(&constraint.lower)?;
            let upper = instantiate(&constraint.upper)?;

            if !self.level_leq(&lower, &upper) {
                return Err(KernelError::UniverseInstance { lower, upper });
            }
        }

        Ok(())
    }

    /// Record a top-level name at `type_`, generalized over `universes`, with `value` as its body where it has one.
    ///
    /// The invalidation clause lives here rather than at either entry point below, and here rather than inside [`Globals`], because it is the one coupling that crosses two components: a redefinition makes every remembered reduct stale. `Globals::insert` reports the overwrite and this applies it, so neither component can forget the other exists.
    fn insert(
        &mut self,
        name: &Free,
        type_: &Term,
        value: Option<&Term>,
        universes: &UniverseContext,
    ) {
        if self.globals.insert(name, type_, value, universes) {
            self.memos.invalidate();
            self.globals.forget_effects();
        }
    }

    /// Record a top-level definition: `name : type_ = value`, generalized over `universes`.
    pub fn define(&mut self, name: &Free, type_: &Term, value: &Term, universes: &UniverseContext) {
        self.insert(name, type_, Some(value), universes);
    }

    /// Record a top-level name with a type and no body — a `foreign` declaration, or one kept opaque. It never unfolds, so it is a permanent neutral.
    pub fn declare(&mut self, name: &Free, type_: &Term, universes: &UniverseContext) {
        self.insert(name, type_, None, universes);
    }

    /// Register an `induct` declaration's registry entry.
    pub fn declare_induct(&mut self, name: &Global, declaration: &InductDecl) {
        self.globals.declare_induct(name, declaration);
    }

    /// Register a `struct` declaration's registry entry.
    pub fn declare_struct(&mut self, name: &Global, declaration: &StructDecl) {
        self.globals.declare_struct(name, declaration);
    }

    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.globals.induct_decl(name)
    }

    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.globals.struct_decl(name)
    }

    /// Open a binder: bring `name : type_` into scope for the walk in progress.
    ///
    /// Locals are a stack, and closing them is [`Kernel::scoped`]'s job rather than the caller's — it is the only bracket there is, so a binder opened here is closed on every path out of the walk that opened it.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.scope.assume(name, type_);
    }

    /// Run `walk` with every binder it opened — and every case equation it assumed — closed again afterwards, on the failing path as well as the succeeding one.
    ///
    /// **The only way to open a binder scope.** [`Scope`]'s `mark` and `retract` are `pub(super)` and this is their only caller anywhere, which is what makes that true. A judgment that opened a binder and returned early would leak it into the conversion history, where the local context is part of the goal key, and no amount of care spread over a dozen call sites makes that structural. Written as a bracket rather than a guard object because the walks it wraps take `&mut Kernel` throughout, and a guard holding the borrow would leave them nothing to be called with.
    pub(crate) fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        let mark = self.scope.mark();
        let outcome = walk(self);
        self.scope.retract(mark);

        outcome
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` — already in weak-head normal form — is `value`, definitionally. Assumed inside the arm's [`Kernel::scoped`] bracket, which is what scopes it.
    pub(crate) fn refine(&mut self, scrutinee: Term, value: Term) {
        self.scope.refine(scrutinee, value);
    }

    /// The case value the stuck form `term` is refined to, innermost arm first.
    pub(crate) fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.scope.refinement_of(term)
    }

    /// The types of the binders currently in scope, outermost first. The conversion history keys on this: the same goal under a different context is a different goal.
    pub(crate) fn local_types(&self) -> Vec<Term> {
        self.scope.local_types()
    }

    /// The identities of the binders currently in scope, outermost first — parallel to [`Kernel::local_types`]. What the conversion history renames away, so that a goal reached again on a later round of an unfolding cycle is recognized as the goal it already is.
    pub(crate) fn local_names(&self) -> Vec<Free> {
        self.scope.local_names()
    }

    /// The type `name` was opened at, if it is a binder currently in scope.
    pub(crate) fn local_type(&self, name: &Free) -> Option<&Term> {
        self.scope.local_type(name)
    }

    /// The type `name` was bound or declared at. Locals shadow definitions.
    pub(crate) fn type_of(&self, name: &Free) -> Option<&Term> {
        self.scope
            .local_type(name)
            .or_else(|| self.globals.type_of(name))
    }

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(crate) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.globals.scheme_of(name)
    }

    /// Charge one reduction step, failing when the budget is spent.
    pub(crate) fn spend(&mut self) -> Result<(), ReduceError> {
        self.spend.step()
    }

    /// A fresh binder identity, rendering as `hint`.
    pub(crate) fn fresh(&self, hint: Option<&str>) -> Free {
        self.spend.fresh(hint)
    }

    /// What `name` unfolds to through a bare occurrence. A definition with universe parameters is withheld.
    pub(crate) fn value(&self, name: &Free) -> Option<&Term> {
        self.globals.value(name)
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(crate) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.globals.value_at(name)
    }

    /// Record `term` as an erased position if the type it was judged at makes it one: a term at a `Prop`-sorted type is a proof, and one at a sort is a type.
    ///
    /// Called from both `check` and `infer`, because a term's type is its type however the judgment reached it. The orchestration lives here rather than on [`Positions`] because the middle of it — `erased_half` — needs the whole kernel; see `Positions::begin` on why that bracket cannot be a closure.
    pub(crate) fn record_checked(&mut self, term: &Term, type_: &Term) {
        if self.positions.suppressed() {
            return;
        }

        let erased = match self.positions.remembered(type_) {
            Some(erased) => erased,
            None => {
                self.positions.begin();
                let outcome = erased_half(self, type_);

                self.positions.settle(type_, outcome)
            }
        };

        if let Some(erased) = erased {
            self.positions.push(term, erased);
        }
    }

    /// Take this item's recorded positions and any classification that could not be decided, leaving both empty for the next item.
    pub(crate) fn take_checked(&mut self) -> (Vec<(Term, Erased)>, Option<KernelError>) {
        self.positions.drain()
    }

    pub fn restore_budget(&mut self) {
        self.spend.restore_budget();
    }
}
