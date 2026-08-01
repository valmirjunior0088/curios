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
    crate::{Env, Judge, entails},
    crate::{Erased, erased_half},
    curios_base::Entropy,
    curios_core::{
        Atom, Free, Global, InductDecl, Level, LevelHead, ReduceError, Reducer, StructDecl, Term,
        UniverseConstraint, UniverseContext, UniverseError,
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
        polarity: curios_core::Polarity,
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
}

impl Judge for Kernel {
    fn convert_at(&mut self, type_: &Term, this: &Term, that: &Term) -> Result<bool, Self::Error> {
        convert::convert(self, type_, this, that)
    }
}

/// One remembered reduction: the reduct, and everything computing it consumed — so a hit can charge exactly what a recomputation would have.
#[derive(Debug, Clone)]
pub(crate) struct Replay {
    reduct: Term,
    steps: u64,
    mints: usize,
}

/// A top-level name's entry: what it is, and what it unfolds to if anything.
///
/// The universe context is not decoration. A definition with universe parameters is *not* unfoldable through a bare occurrence, because such an occurrence denotes no particular instance; it reduces only through a [`UniverseInst`](curios_core::UniverseInst) that says which one.
struct Definition {
    type_: Term,
    /// `None` for something with a type and no body — a `foreign` declaration, or a name deliberately kept opaque.
    value: Option<Term>,
    universes: UniverseContext,
}

/// The kernel's context: what is in scope, what may unfold, and how much work a judgment may spend.
///
/// Deliberately small. The elaborator's `Context` carries fifteen-odd stores — caches, parked goals, refinement layers, a metavariable heap — and each is a place where an answer can come from something other than the term in hand. The kernel holds a local telescope, top-level definitions, the nominal registry, and a budget. Growing this struct is how independence gets lost, so a new field should have to argue for itself. A scope checkpoint: the depths of the binder stack and the case-equation stack at [`Kernel::mark`] time, restored together by [`Kernel::retract`] so neither can outlive the arm that opened it.
#[derive(Clone, Copy)]
pub(crate) struct Mark {
    locals: usize,
    refinements: usize,
}

pub struct Kernel {
    /// Reduction steps a single judgment may spend. Restored at each declaration boundary by [`Kernel::restore_budget`].
    budget: u64,
    remaining: u64,
    /// Identities for binders the kernel opens itself, when comparing under a telescope and when eta-contracting. Seeded above every index the earlier stages minted, so a kernel-minted binder can never alias one in a term.
    fresh_names: Entropy,
    /// Binders opened by the walk in progress, with their types, outermost first. A local has a type and never a value: `let` substitutes rather than binding, so nothing in scope here can be unfolded.
    locals: Vec<(Free, Term)>,
    /// Whether the evaluation memos below are consulted. On by default; [`Kernel::uncached`] exists so a test can assert that switching them off changes no verdict — the property that makes them an evaluation strategy rather than a store.
    memoized: bool,
    /// The weak-head reduct each *monomorphic* definition's body reaches, keyed by name and computed on first delta — the analog of Lean's `m_unfold`, which its trusted `type_checker` holds beside `m_whnf` and `m_whnf_core`. Definition bodies are closed, so an entry depends on nothing but the definition store, and [`Kernel::define`] clears every memo if it ever *overwrites* a name — so validity is by construction, not by an append-only assumption.
    ///
    /// This is not the kind of store the warning below forbids. A metavariable heap or a refinement layer *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. The measured cost of refusing even that was a 10× whole-prelude re-check, spent re-deriving the same prelude spines for every recursive group the totality gate walks.
    ///
    /// Every entry carries what its computation consumed — budget steps and minted binder identities — and a hit charges exactly both. The budget prices the work a term *denotes*, performed or replayed, and the entropy counter advances as if the eta probes had run, so the whole observable trajectory — refusal payloads, exhaustion points, every later-minted identity — is bit-identical with the memos on or off. `kernel_memo_parity` is the test that holds this to account.
    unfold_memo: HashMap<Free, Replay>,
    /// Weak-head reducts of *local-free* terms, per entry point (plain, and rec-forced). Local-free — every free variable a definition name — is what makes a key scope-independent: whnf reads no local (locals carry no values by design) and no constraint, so the reduct is a function of the definition store alone, and no key can dangle a retracted binder.
    whnf_memo: HashMap<Term, Replay>,
    forced_memo: HashMap<Term, Replay>,
    /// The case equations of the arms currently being checked, innermost last: within an arm, the scrutinee expression *is* the case's value, definitionally — the built-in face of the convoy pattern, which is how the elaborator's refinement store reads inside an arm. Scoped exactly like the binders the arm opens, through the same [`Kernel::mark`] / [`Kernel::retract`] bracket. The reducer consults these at stuck heads.
    refinements: Vec<(Term, Term)>,
    /// The constraint set of the item being checked — its own declared hypotheses, assumed while its parameters are held abstract. A generic definition is valid exactly when it checks *under* its constraints, so the level judgments below consult these; discarding them was the route by which a correct polymorphic definition was refused.
    assumed: Vec<UniverseConstraint>,
    /// The erased positions this walk has recorded — the seed for obligations (T) and (V), taken from the kernel's own typing rather than from another crate's record. Drained per item by [`Kernel::take_checked`].
    ///
    /// Classified *when recorded*, not afterwards. A position's type routinely mentions the binders the item opened, and those are retracted the moment the item's check returns, so a later pass cannot ask for their sorts at all — it can only fail, and failing quietly is how positions across a tenth of the fixed prelude came to be silently unconstrained.
    checked: Vec<(Term, Erased)>,
    /// Sort-hood per distinct type, so classifying at every record site costs one question per type rather than one per position. Keyed on terms whose binders are the item's own, which is what makes an entry meaningful only within the item that made it — cleared with the drain.
    erasure_memo: HashMap<Term, Option<Erased>>,
    /// The first classification that could not be decided, surfaced with the drain. Deciding asks for a sort, which cannot be reported from a recording site that returns nothing.
    erasure_failure: Option<KernelError>,
    /// Re-entrancy guard. Deciding a position's erased half types terms of its own, and those must not be recorded as positions in turn.
    classifying: bool,
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
            memoized: true,
            unfold_memo: HashMap::new(),
            whnf_memo: HashMap::new(),
            forced_memo: HashMap::new(),
            refinements: Vec::new(),
            assumed: Vec::new(),
            checked: Vec::new(),
            erasure_memo: HashMap::new(),
            erasure_failure: None,
            classifying: false,
            definitions: HashMap::new(),
            inducts: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    /// A kernel whose evaluation memos are off — every reduction re-derived from scratch. Exists for one purpose: asserting that memoization changes no verdict.
    pub fn uncached(budget: u64) -> Self {
        Self {
            memoized: false,
            ..Self::new(budget)
        }
    }

    /// The remembered reduct of `name`'s body, with the replayed computation's whole consumption charged.
    pub(crate) fn unfold_hit(&mut self, name: &Free) -> Option<Result<Term, ReduceError>> {
        if !self.memoized {
            return None;
        }

        let replay = self.unfold_memo.get(name).cloned()?;

        Some(self.charge(replay))
    }

    /// Remember what `name`'s body reduces to, and what computing it consumed.
    pub(crate) fn unfold_store(&mut self, name: Free, replay: Replay) {
        if self.memoized {
            self.unfold_memo.insert(name, replay);
        }
    }

    /// The remembered weak-head reduct of a local-free `term`, per entry point, with the replayed computation's whole consumption charged.
    pub(crate) fn whnf_hit(
        &mut self,
        term: &Term,
        forced: bool,
    ) -> Option<Result<Term, ReduceError>> {
        if !self.memoized || term.has_local_free() {
            return None;
        }

        let replay = match forced {
            false => self.whnf_memo.get(term).cloned()?,
            true => self.forced_memo.get(term).cloned()?,
        };

        Some(self.charge(replay))
    }

    /// Remember a local-free `term`'s weak-head reduct and its consumption.
    pub(crate) fn whnf_store(&mut self, term: Term, forced: bool, replay: Replay) {
        if !self.memoized || term.has_local_free() {
            return;
        }

        match forced {
            false => self.whnf_memo.insert(term, replay),
            true => self.forced_memo.insert(term, replay),
        };
    }

    /// A consumption snapshot, for measuring what a computation charges.
    pub(crate) fn consumption(&self) -> (u64, usize) {
        (self.remaining, self.fresh_names.count())
    }

    /// The [`Replay`] for `reduct`, measured against the snapshot taken before the computation ran.
    pub(crate) fn replay_since(&self, reduct: Term, (budget, minted): (u64, usize)) -> Replay {
        Replay {
            reduct,
            steps: budget - self.remaining,
            mints: self.fresh_names.count() - minted,
        }
    }

    /// Charge a replayed computation's whole consumption at once. Exhausting here is exactly where the recomputation would have exhausted mid-chain, and it reports the same error; the entropy advances as if every probe binder had been minted, so later identities land where they would have.
    fn charge(&mut self, replay: Replay) -> Result<Term, ReduceError> {
        self.fresh_names
            .seed(self.fresh_names.count() + replay.mints);

        match self.remaining.checked_sub(replay.steps) {
            Some(remaining) => {
                self.remaining = remaining;
                Ok(replay.reduct)
            }
            None => {
                self.remaining = 0;
                Err(ReduceError::Exhausted)
            }
        }
    }

    /// Raise the binder counter above every index minted by an earlier stage.
    ///
    /// The lowerer, the elaborator, and the archived prelude all mint into one identity space; a kernel that started at zero would mint binders that alias theirs, and an alias between two distinct binders is a capture.
    pub fn set_local_floor(&mut self, floor: usize) {
        self.fresh_names.seed(floor);
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
    pub(crate) fn check_instance(
        &self,
        context: &UniverseContext,
        levels: &[Level],
    ) -> Result<(), KernelError> {
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

    /// Record a top-level definition: `name : type_ = value`, generalized over `universes`.
    pub fn define(&mut self, name: &Free, type_: &Term, value: &Term, universes: &UniverseContext) {
        let previous = self.definitions.insert(
            name.clone(),
            Definition {
                type_: type_.clone(),
                value: Some(value.clone()),
                universes: universes.clone(),
            },
        );

        // A redefinition invalidates every remembered reduct — the memos' validity is by construction, never by an append-only assumption.
        if previous.is_some() {
            self.unfold_memo.clear();
            self.whnf_memo.clear();
            self.forced_memo.clear();
        }
    }

    /// Record a top-level name with a type and no body — a `foreign` declaration, or one kept opaque. It never unfolds, so it is a permanent neutral.
    pub fn declare(&mut self, name: &Free, type_: &Term, universes: &UniverseContext) {
        let previous = self.definitions.insert(
            name.clone(),
            Definition {
                type_: type_.clone(),
                value: None,
                universes: universes.clone(),
            },
        );

        if previous.is_some() {
            self.unfold_memo.clear();
            self.whnf_memo.clear();
            self.forced_memo.clear();
        }
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
    /// Locals are a stack, and every judgment that opens one is responsible for closing it — take a [`Kernel::mark`] first and [`Kernel::retract`] to it afterwards, on every path including the failing one.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.locals.push((name.clone(), type_.clone()));
    }

    /// The current scope depth, to be handed back to [`Kernel::retract`].
    pub(crate) fn mark(&self) -> Mark {
        Mark {
            locals: self.locals.len(),
            refinements: self.refinements.len(),
        }
    }

    /// Close every binder opened — and drop every case equation assumed — since `mark`.
    pub(crate) fn retract(&mut self, mark: Mark) {
        self.locals.truncate(mark.locals);
        self.refinements.truncate(mark.refinements);
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` — already in weak-head normal form — is `value`, definitionally. Push inside the arm's `mark`/`retract` bracket, which is what scopes it.
    ///
    /// A local-free scrutinee is skipped rather than recorded: local-free terms reduce to their case values instead of sticking, and the skip is also what keeps the evaluation memos sound — they store local-free terms only, and reduction of a local-free term never encounters a local-bearing stuck form, so no memoized reduct can depend on an equation that was later retracted.
    pub(crate) fn refine(&mut self, scrutinee: Term, value: Term) {
        if scrutinee.has_local_free() {
            self.refinements.push((scrutinee, value));
        }
    }

    /// The case value the stuck form `term` is refined to, innermost arm first.
    pub(crate) fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.refinements
            .iter()
            .rev()
            .find(|(key, _)| key == term)
            .map(|(_, value)| value.clone())
    }

    /// The types of the binders currently in scope, outermost first. The conversion history keys on this: the same goal under a different context is a different goal.
    pub(crate) fn local_types(&self) -> Vec<Term> {
        self.locals.iter().map(|(_, type_)| type_.clone()).collect()
    }

    /// The identities of the binders currently in scope, outermost first — parallel to [`Kernel::local_types`]. What the conversion history renames away, so that a goal reached again on a later round of an unfolding cycle is recognized as the goal it already is.
    pub(crate) fn local_names(&self) -> Vec<Free> {
        self.locals.iter().map(|(name, _)| name.clone()).collect()
    }

    /// The type `name` was opened at, if it is a binder currently in scope.
    ///
    /// Innermost first — which cannot actually matter, since binder identities are minted unique, but scanning in that order means the rule does not depend on that being true.
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

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(crate) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.definitions
            .get(name)
            .map(|entry| (&entry.type_, &entry.universes))
    }

    /// Charge one reduction step, failing when the budget is spent.
    ///
    /// The kernel is not strongly normalizing and does not pretend to be: a non-productive `rec` reduces forever. The budget is what makes every judgment terminate, and it is deterministic — the same program spends the same steps on every machine — so exhausting it is a fact about the program, not about the host that checked it.
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

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(crate) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .and_then(|definition| definition.value.as_ref())
    }

    /// Restore the full budget for a new judgment.
    /// Record `term` as an erased position if the type it was judged at makes it one: a term at a `Prop`-sorted type is a proof, and one at a sort is a type.
    ///
    /// Called from both `check` and `infer`, because a term's type is its type however the judgment reached it.
    pub(crate) fn record_checked(&mut self, term: &Term, type_: &Term) {
        if self.classifying {
            return;
        }

        let erased = match self.erasure_memo.get(type_) {
            Some(erased) => *erased,
            None => {
                self.classifying = true;
                let outcome = erased_half(self, type_);
                self.classifying = false;

                let erased = match outcome {
                    Ok(erased) => erased,
                    Err(error) => {
                        self.erasure_failure.get_or_insert(error);
                        None
                    }
                };
                self.erasure_memo.insert(type_.clone(), erased);
                erased
            }
        };

        if let Some(erased) = erased {
            self.checked.push((term.clone(), erased));
        }
    }

    /// Take this item's recorded positions and any classification that could not be decided, leaving both empty for the next item.
    ///
    /// The memo goes with them: its keys mention the item's own binders, so an entry means nothing once they are retracted.
    pub fn take_checked(&mut self) -> (Vec<(Term, Erased)>, Option<KernelError>) {
        self.erasure_memo.clear();

        (
            std::mem::take(&mut self.checked),
            self.erasure_failure.take(),
        )
    }

    pub fn restore_budget(&mut self) {
        self.remaining = self.budget;
    }
}
