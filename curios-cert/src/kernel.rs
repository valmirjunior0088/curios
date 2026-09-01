//! The independent kernel: the judgments that decide whether a term is well-typed, written against the representation and nothing else.
//!
//! The elaborator in `curios-elab` is a large, stateful program. It inserts implicit arguments, invents and solves metavariables, parks and wakes conversion goals, resolves witnesses, refines scrutinees inside match arms, and memoizes almost all of it. Every one of those mechanisms exists to make the *surface language* ergonomic, and every one of them is a way for a bad program to be admitted. What this module provides is the other half of the bargain: a second opinion that shares none of that machinery.
//!
//! The independence is structural, not a matter of discipline. This crate does not depend on `curios-elab`, so nothing here can consult a metavariable store, a refinement, or a cached elaboration — not because the code declines to, but because those types are not in scope. A judgment the elaborator gets wrong is re-decided here from the term alone.
//!
//! What the kernel *does* share is the representation: [`Term`], its binder discipline, the intrinsic roster, and the intrinsic folds. Sharing a representation is not sharing a judgment. Two checkers that disagree about a term's type while agreeing on what a term *is* still catch each other's mistakes; two that share the rule that admits a bad program catch nothing. That line is why [`Reducer`] exists, and it is why the match dispatch in `whnf` is written out again here rather than lifted from the elaborator's reducer, which it closely resembles.
//!
//! # Refusing beats guessing
//!
//! Where the elaborator cannot classify something it falls back conservatively and carries on, because a diagnostic is worth more to a programmer than a refusal. The kernel does the opposite: a shape it cannot classify is a [`KernelError`], not a default. A guessed universe level is the unsound direction — it claims a type is smaller than it is — and a checker that guesses is not a second opinion. The cost is that the kernel may reject a term the elaborator accepted; that is a disagreement to investigate, which is exactly what a second opinion is for.

mod at;
pub(crate) use at::*;

mod convert;
pub use convert::*;

mod globals;
pub use globals::*;

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
    crate::{entails, erased_half},
    curios_analysis::{Env, Erased, Judge},
    curios_core::{
        Atom, Consumption, Cost, DEFAULT_RETENTION_QUOTA, Free, Global, InductDecl, Level,
        LevelHead, Module, Polarity, ReduceError, Reducer, Retention, Spelling, StructDecl, Term,
        UniverseConstraint, UniverseContext, UniverseError, build_shorten,
    },
    curios_utilities::SyntaxRegistry,
    std::{fmt, rc::Rc},
};

/// Why the kernel refused a term.
///
/// What a [`KernelError::Arity`] counted. One refusal, many tallies — a message reading `expected 1, found 0` with nothing to say what the 1 was sent a reader to the kernel's source to learn it was a universe level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Counted {
    /// The levels an occurrence supplies, against the parameters its declaration's scheme binds.
    UniverseLevels,
    /// The parameters an occurrence supplies, or a constructor's telescope opens with, against the declaration's.
    Parameters,
    /// The indices an occurrence supplies, or the targets a constructor states, against the declaration's index telescope.
    Indices,
    /// The names a recursive group is exported under, against the members it holds.
    GroupMembers,
    /// The arguments of a call, against the parameters of the function's type or the foreign row.
    Arguments,
    /// The components a projection reaches past — the index it names plus one — against the tuple or structure it projects from.
    Components,
    /// A constructor value's payload, against its signature.
    Payload,
    /// A tuple or structure value's fields, against its telescope.
    Fields,
    /// A motive's binders, against the family's indices plus the scrutinee.
    MotiveBinders,
    /// An arm's binders, against the constructor's signature.
    ArmBinders,
}

impl fmt::Display for Counted {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Counted::UniverseLevels => "universe levels",
            Counted::Parameters => "parameters",
            Counted::Indices => "indices",
            Counted::GroupMembers => "recursive group members",
            Counted::Arguments => "arguments",
            Counted::Components => "components",
            Counted::Payload => "constructor payload",
            Counted::Fields => "fields",
            Counted::MotiveBinders => "motive binders",
            Counted::ArmBinders => "arm binders",
        })
    }
}

/// Every variant is a refusal, never a warning: reaching one means the kernel declined to certify the term, and a caller must treat that as rejection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KernelError {
    /// Reduction failed — the budget ran out, or a partial intrinsic was folded outside its domain.
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
    /// A count that did not match — what was counted is [`Counted`], so the refusal names it.
    Arity {
        counted: Counted,
        expected: usize,
        actual: usize,
    },
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
    /// A proof or a type that reaches something not known to terminate, or that is such a thing itself — an inline `rec` group that does not descend, or an `Intrinsic::ProcExit`. Erasure deletes both halves, so a proof that may not terminate proves anything and a type that may not terminate reties the negative knot positivity forbids. `reached` names the offending definition, or is absent when the position is partial in itself and there is no name to blame.
    NotTotal {
        erased: Erased,
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
    /// An occurrence of a universe-polymorphic definition that states no instance. Such an occurrence denotes no particular instance, which is why `Globals::value` withholds its body; reading its *type* regardless hands back the scheme's own parameters, which are then read as the ambient item's, and skips `check_instance` entirely — so the scheme's constraints are discharged by nothing and a use the stated-instance spelling refuses is admitted by dropping the instance.
    MissingUniverseInstance { name: Free, expected: usize },
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

impl KernelError {
    /// Render this refusal with global names shortened against `module`'s symbol table and a nominal family's implicit parameters marked — the two axes a reader needs to recognize the types they wrote.
    ///
    /// Universe instances are deliberately *not* suppressed here, unlike an elaboration diagnostic. A kernel refusal is often *about* the universes: `convert.rs` records one reading "a ground `Type` against a `Type.{u}`", and erasing the instance would reduce that to `Type` against `Type`. The same call the `--print` stage dumps make, for the same reason — a reader looking at the checker wants the levels the checker is arguing about.
    pub fn format_with(&self, module: &Module, scope: &[&Module]) -> String {
        // See `curios_elab::Error::format_with`: a module carries only its own declarations, so *both* halves of the spelling have to be told what its environment put in scope — the shortening table and the plicity marks alike.
        let mut symbols = module.module_symbols();
        let mut plicities = module.nominal_plicities();
        for unit in scope {
            symbols.extend(unit.module_symbols());
            for (name, marks) in unit.nominal_plicities() {
                plicities.entry(name).or_insert(marks);
            }
        }

        let spelling = Rc::new(
            Spelling::default()
                .with_short_names(Rc::new(build_shorten(&symbols)))
                .with_nominal_plicities(Rc::new(plicities)),
        );
        Displayed(self, spelling).to_string()
    }
}

/// The faithful rendering: core's own names, every universe shown. A refusal reported to a reader goes through [`KernelError::format_with`].
impl fmt::Display for KernelError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        Displayed(self, Rc::new(Spelling::default())).fmt(formatter)
    }
}

/// A refusal paired with the [`Spelling`] its terms render under — the parameter `Display::fmt` cannot take. Local to this crate because the orphan rule forbids implementing a foreign trait for a foreign wrapper, and because the axes a kernel refusal wants are not the ones an elaboration diagnostic wants.
struct Displayed<'a>(&'a KernelError, Rc<Spelling>);

/// Every arm below rebinds its term fields through the spelling before interpolating them. A field left unrebound renders core's own spelling and still compiles, which is why the rebinding is mechanical rather than left to each `write!`.
impl fmt::Display for Displayed<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = &self.1;
        match self.0 {
            KernelError::Reduce(ReduceError::Exhausted {
                category,
                remaining,
                attempted,
            }) => write!(
                formatter,
                "the kernel's reduction budget ran out: {category} needed {attempted} units with {remaining} left"
            ),
            KernelError::Reduce(_) => formatter.write_str("reduction failed in the kernel"),
            KernelError::Unbound(name) => write!(formatter, "unbound name `{name}`"),
            KernelError::Undeclared(name) => {
                write!(formatter, "no declaration registered for `{name}`")
            }
            KernelError::Unclassified(type_) => {
                let type_ = type_.spelled(spelling);
                write!(formatter, "cannot determine the sort of `{type_}`")
            }
            KernelError::NotASort(term) => {
                let term = term.spelled(spelling);
                write!(formatter, "`{term}` is not a universe")
            }
            KernelError::NotAMotive(term) => {
                let term = term.spelled(spelling);
                write!(
                    formatter,
                    "`{term}` is not a valid motive: it must be well-typed and land in a sort",
                )
            }
            KernelError::Mismatch { inferred, expected } => {
                let inferred = inferred.spelled(spelling);
                let expected = expected.spelled(spelling);
                write!(formatter, "expected `{expected}`, found `{inferred}`")
            }
            KernelError::NotAFunction(type_) => {
                let type_ = type_.spelled(spelling);
                write!(formatter, "`{type_}` is not a function type")
            }
            KernelError::NotATuple(type_) => {
                let type_ = type_.spelled(spelling);
                write!(formatter, "`{type_}` has no components")
            }
            KernelError::Arity {
                counted,
                expected,
                actual,
            } => {
                write!(formatter, "{counted}: expected {expected}, found {actual}")
            }
            KernelError::LargeElimination(name) => write!(
                formatter,
                "cannot eliminate the proposition `{name}` into a relevant result",
            ),
            KernelError::NotCore(term) => {
                let term = term.spelled(spelling);
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
            KernelError::NotDescending { type_ } => {
                let type_ = type_.spelled(spelling);
                write!(
                    formatter,
                    "a recursive proof or type at `{type_}` does not descend",
                )
            }
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
            KernelError::MissingUniverseInstance { name, expected } => write!(
                formatter,
                "this occurrence of `{name}` states no universe instance, and its scheme declares {expected}",
            ),
        }
    }
}

/// The kernel's side of the shared-analysis seam.
///
/// `assumption` reads the *locals* rather than `Kernel::type_of`, because a shared analysis asking what a binder was assumed at means the binder in scope, not a top-level name that happens to share its spelling. That matches what the elaborator's `Context::assumption` answers, which is the point of the seam.
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

/// The kernel's context: what is in scope, what may unfold, and how much work a judgment may spend.
///
/// Deliberately small, and now deliberately *composed*. The elaborator's `Context` carries fifteen-odd stores — caches, parked goals, refinement layers, a metavariable heap — and each is a place where an answer can come from something other than the term in hand. This held seventeen loose fields, which is the same shape read from the other end: five independent jobs whose invariants were spread across the methods that happened to touch them. Each is now a component that states its own, and what remains here is the composition and the couplings that genuinely cross it.
///
/// The one such coupling is worth naming, because it is why `Globals::insert` reports rather than acts: overwriting a definition invalidates every remembered reduct, which is a fact about `Globals` *and* `Memos` and therefore belongs to neither.
///
/// Growing this struct is still how independence gets lost. A new *component* should have to argue for itself the way a new field used to.
pub struct Kernel {
    /// What the walk in progress has opened.
    scope: Scope,
    /// What a judgment may consume, and what it has.
    spend: Spend,
    /// Remembered weak-head reducts, replayed rather than re-derived.
    memos: Memos,
    /// What this walk may still retain in those memos. Compilation-scoped, never restored — see [`Retention`].
    retention: Retention,
    /// The erased positions this walk recorded — an output, not an input.
    positions: Positions,
    /// Top-level definitions and the nominal registry.
    globals: Globals,
    /// The `/syn` spellings this walk may need to *state* a type — today the propositions the guarded operations take as bounds, read through `Intrinsic::signature`.
    ///
    /// Handed in rather than defaulted, and deliberately not optional. An absent registry could only mean skipping the bound check, and a check that silently does not run is worse than one that is missing outright: the kernel would report a verdict it had not reached.
    syntax: SyntaxRegistry,
    /// The constraint set of the item being checked — its own declared hypotheses, assumed while its parameters are held abstract. A generic definition is valid exactly when it checks *under* its constraints, so the level judgments below consult these; discarding them was the route by which a correct polymorphic definition was refused.
    ///
    /// The one field with no component of its own: it is a single vector replaced wholesale at each declaration boundary, and wrapping it would state nothing the type does not.
    assumed: Vec<UniverseConstraint>,
    /// Whether the closed machine may run — false only in the differential fixture's strategy arm, which is what makes the machine's reducts checkable against the strategy's at all.
    machine: bool,
}

impl Kernel {
    /// A kernel that may spend `budget` reduction steps per judgment, stating types through `syntax`.
    pub fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            scope: Scope::default(),
            spend: Spend::new(budget),
            memos: Memos::new(true),
            retention: Retention::new(DEFAULT_RETENTION_QUOTA),
            positions: Positions::default(),
            globals: Globals::default(),
            syntax,
            assumed: Vec::new(),
            machine: true,
        }
    }

    /// The `/syn` spellings this walk states types through.
    pub(crate) fn syntax(&self) -> SyntaxRegistry {
        self.syntax
    }

    /// Start this kernel's environment from `globals` — the scope an earlier walk established — rather than from nothing.
    ///
    /// Set wholesale where [`Kernel::define`] has to report an overwrite, because a kernel is seeded before it has judged anything: there are no remembered reducts yet for a replaced name to invalidate.
    pub(crate) fn seed(&mut self, globals: &Globals) {
        self.globals = globals.clone();
    }

    /// A kernel at a stated retention allowance rather than the product default.
    ///
    /// Exists so a test can put the quota under pressure without building a module large enough to reach the shipped figure — which is measured to be unreachable by ordinary compilation, and would therefore make the degradation path untestable.
    pub fn with_retention(budget: u64, quota: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            retention: Retention::new(quota),
            ..Self::new(budget, syntax)
        }
    }

    /// A kernel whose evaluation memos are off — every reduction re-derived from scratch. Exists for one purpose: asserting that memoization changes no *semantic* verdict. It may change a resource one, since a term-keyed hit is free and an uncached walk therefore spends at least as much; see the `spend` module's documentation for why that is the whole of what was given up.
    pub fn uncached(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            memos: Memos::new(false),
            ..Self::new(budget, syntax)
        }
    }

    /// The remembered reduct of `name`'s body, with the replayed computation's whole consumption charged — or `None` when there is no entry, *or* when its charge does not fit.
    ///
    /// Looking one up and charging it are two components' jobs, joined here: [`Memos`] can hand back a [`Replay`] and cannot apply one.
    ///
    /// The two `None`s are deliberately the same answer, because the caller does the same thing with them: evaluate the body directly. An unaffordable replay is a reason to take the direct path and let it fail where it actually fails, not a reason to refuse from an aggregate — see [`Spend::charge`].
    pub(crate) fn unfold_hit(&mut self, name: &Free) -> Option<Term> {
        let replay = self.memos.unfold(name)?;

        self.spend.charge(replay)
    }

    /// Remember what `name`'s body reduces to, and what computing it consumed — unless the compilation's retention allowance cannot cover the entry.
    ///
    /// A declined insertion is not a refusal of anything: the reduct has already been computed and is returned either way, and the only consequence is that the next occurrence of this name recomputes it. See [`Retention`].
    pub(crate) fn unfold_store(&mut self, name: Free, replay: Replay) {
        if self.retention.admits(replay.retention()) {
            self.memos.store_unfold(name, replay);
        }
    }

    /// The remembered weak-head reduct of a local-free `term`, per entry point, replayed for nothing.
    ///
    /// The one hit that cannot fail, because it spends no steps: the kernel did not perform this computation, and charging it what a memo-free evaluator would have spent is what made a budget run out on work nobody did. [`Spend::charge_nothing`] and [`Memos`] state the two halves of why that is safe.
    pub(crate) fn whnf_hit(&mut self, term: &Term, forced: bool) -> Option<Term> {
        let replay = self.memos.whnf(term, forced)?;

        Some(self.spend.charge_nothing(replay))
    }

    /// Remember a local-free `term`'s weak-head reduct and its consumption.
    ///
    /// **Not charged to the retention allowance, and deliberately.** That allowance exists for storage that outlives the budget that built it — [`Retention`] names the composition it bounds, a cache surviving item boundaries times a budget restored at each — and this table does not: [`Memos::begin_declaration`] clears it exactly where [`Spend::restore_budget`] fires, and every node it holds was built under that budget, which charges a construction what it builds. It was charged anyway, key and reduct, at the tree footprint of each — and a thirteen-definition proof whose reducts were graphs with `2^n`-node trees spent a third of the whole compilation's allowance on entries that died with the declaration. The name-keyed table beside this one does outlive a declaration, and [`Kernel::unfold_store`] still pays for it.
    pub(crate) fn whnf_store(&mut self, term: Term, forced: bool, replay: Replay) {
        self.memos.store_whnf(term, forced, replay);
    }

    /// How much of this walk's retention allowance its memos have consumed.
    ///
    /// An observation for a measurement, not a control: nothing in the kernel reads it, and what it is for is setting [`DEFAULT_RETENTION_QUOTA`] against a figure rather than a guess.
    pub fn retained(&self) -> u64 {
        self.retention.spent()
    }

    /// The heaviest declaration this kernel has walked — what it spent, and how deep it went.
    ///
    /// The measurement counterpart of [`Kernel::retained`], and the same kind of thing: nothing in the kernel reads it, and it exists so a figure can be stated with a probe beside it instead of bisected against a budget from outside the compiler. See [`Consumption`] for why depth is the row it separates out.
    pub fn heaviest_declaration(&self) -> Consumption {
        self.spend.heaviest()
    }

    /// See [`Spend::snapshot`].
    pub(crate) fn consumption(&self) -> (u64, usize) {
        self.spend.snapshot()
    }

    /// See [`Spend::replay_since`].
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
                counted: Counted::UniverseLevels,
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
    /// Locals are a stack, and closing them is `Kernel::scoped`'s job rather than the caller's — it is the only bracket there is, so a binder opened here is closed on every path out of the walk that opened it.
    pub fn assume(&mut self, name: &Free, type_: &Term) {
        self.scope.assume(name, type_);
    }

    /// Run `walk` with every binder it opened — and every case equation it assumed — closed again afterwards, on the failing path as well as the succeeding one.
    ///
    /// **The only way to open a binder scope.** [`Scope`]'s `mark` and `retract` are `pub(super)` and this is their only caller anywhere, which is what makes that true. A judgment that opened a binder and returned early would leak it into the conversion history, where the local context is part of the goal key, and no amount of care spread over a dozen call sites makes that structural. Written as a bracket rather than a guard object because the walks it wraps take `&mut Kernel` throughout, and a guard holding the borrow would leave them nothing to be called with.
    pub(crate) fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        let mark = self.scope.mark();
        let outcome = walk(self);
        // Retracting an equation changes what a local-bearing term reduces to, so the reducts remembered under it go with it. A bracket that assumed none leaves the tables alone — most do, and what they remembered is still true.
        if self.scope.retract(mark) {
            self.memos.begin_equations();
        }

        outcome
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` — as written — is `value`, definitionally. Assumed inside the arm's [`Kernel::scoped`] bracket, which is what scopes it.
    pub(crate) fn refine(&mut self, scrutinee: Term, value: Term) {
        self.scope.refine(scrutinee, value);
        self.memos.begin_equations();
    }

    /// The case value `term` is refined to under the written spelling, innermost arm first.
    pub(crate) fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.scope.refinement_of(term)
    }

    /// The case value `term` is refined to under a reduced spelling already settled, innermost arm first.
    pub(crate) fn refinement_of_reduct(&self, term: &Term) -> Option<Term> {
        self.scope.refinement_of_reduct(term)
    }

    /// The innermost equation in force whose reduced spelling has not been asked for and could be `candidate`, as its position and the term to reduce.
    pub(crate) fn unasked_refinement(&self, candidate: &Term) -> Option<(usize, Term)> {
        self.scope.unasked_refinement(candidate)
    }

    /// Settle the reduced spelling of the equation at `index`, reducing `key` with that equation — and every equation inside it — withheld.
    ///
    /// **The whole of what the two-tier key defers.** Recording an equation costs nothing now; this is where the reduction the old key performed eagerly, once per arm, actually happens — at most once per equation, and only because a probe presented a term the written spelling did not answer.
    ///
    /// Withholding is [`Scope::hide_refinements_from`]'s to justify. An error settles the equation as having no reduced spelling, so the attempt is paid once rather than repeated at every later probe; exhaustion is the one error that also propagates, because the budget it spent is real and the judgment has no business continuing at zero.
    pub(crate) fn settle_refinement(&mut self, index: usize, key: Term) -> Result<(), ReduceError> {
        // Withholding equations is a change to the set in force, and so is restoring them: the local-bearing reducts remembered on either side of the settlement must not answer on the other.
        let outer = self.scope.hide_refinements_from(index);
        self.memos.begin_equations();
        // The reduced spelling is held operand-canonical, the form `refined_reduct` brings a probed value to before comparing — see `canonical_operands`.
        let reduct = whnf(self, key).and_then(|reduct| whnf::canonical_operands(self, &reduct));
        self.scope.show_refinements(outer);
        self.memos.begin_equations();

        match reduct {
            Ok(value) => {
                self.scope.settle_refinement(index, Some(value));
                Ok(())
            }
            Err(error) => {
                self.scope.settle_refinement(index, None);

                match error.is_exhausted() {
                    true => Err(error),
                    false => Ok(()),
                }
            }
        }
    }

    /// Whether any arm's case equation is currently assumed — the judgment-side half of the closed machine's gate.
    pub(crate) fn has_refinements(&self) -> bool {
        self.scope.has_refinements()
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
    ///
    /// A definition with universe parameters is refused here rather than answered, which is [`Globals::value`]'s rule applied to the other half of a definition. A bare occurrence denotes no particular instance, so there is no instantiation to report a type at: handing back the stored scheme type reads that scheme's parameters as the ambient item's — the capture `documentation/soundness.md` records at the neighbouring position — and reaches [`Kernel::check_instance`] never, so the scheme's constraints go undischarged. A local is exempt because it is monomorphic: it was opened at one type, and there is no scheme to instantiate.
    pub(crate) fn type_of(&self, name: &Free) -> Result<Option<&Term>, KernelError> {
        if let Some(local) = self.scope.local_type(name) {
            return Ok(Some(local));
        }

        match self.globals.scheme_of(name) {
            None => Ok(None),
            Some((type_, universes)) => match universes.parameter_count {
                0 => Ok(Some(type_)),
                expected => Err(KernelError::MissingUniverseInstance {
                    name: name.clone(),
                    expected,
                }),
            },
        }
    }

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(crate) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.globals.scheme_of(name)
    }

    /// Charge `cost` against this judgment's budget, failing when it cannot be afforded.
    pub(crate) fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.spend.spend(cost)
    }

    /// Enter one guarded reduction level, charging its frame when it is a new peak. See [`Spend::enter_level`].
    pub(crate) fn enter_level(&mut self) -> Result<(), ReduceError> {
        self.spend.enter_level(Cost::FRAME)
    }

    /// See [`Spend::leave_level`].
    pub(crate) fn leave_level(&mut self) {
        self.spend.leave_level();
    }

    /// See [`Spend::fresh`].
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

    /// Begin a new declaration: the full budget back, and the term-keyed memos discarded.
    ///
    /// The two go together and neither is optional. A restored budget is what keeps one declaration's verdict off what the declarations before it spent; discarding the tables a hit is *free* on is what keeps it off what they warmed.
    pub fn restore_budget(&mut self) {
        self.spend.restore_budget();
        self.memos.begin_declaration();
    }
}
