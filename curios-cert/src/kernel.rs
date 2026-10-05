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
//! Where the elaborator cannot classify something it falls back conservatively and carries on, because a diagnostic is worth more to a programmer than a refusal. The kernel does the opposite: a shape it cannot classify is an [`Error`], not a default. A guessed universe level is the unsound direction — it claims a type is smaller than it is — and a checker that guesses is not a second opinion. The cost is that the kernel may reject a term the elaborator accepted; that is a disagreement to investigate, which is exactly what a second opinion is for.

mod at;
pub(crate) use at::*;

mod convert;
pub use convert::*;

mod globals;
pub use globals::*;

mod infer;
pub use infer::*;

mod memos;
use memos::*;

mod module;
pub(crate) use module::*;

mod positions;
pub(crate) use positions::*;

mod calls;
use calls::*;

mod reads;
use reads::*;

mod scope;
use scope::*;

mod sort;
pub use sort::*;

mod spend;
pub(crate) use spend::*;

mod whnf;
pub(crate) use whnf::*;

use {
    crate::{entails, erased_half},
    curios_analysis::{Env, Erased, Judge},
    curios_core::{
        Advance, Atom, Consumption, Cost, Exhaustion, Free, Global, InductDecl, Level, LevelHead,
        Module, Polarity, Probe, Reads, ReduceError, Reducer, Spelling, StructDecl, Term,
        UniverseConstraint, UniverseContext, UniverseError, Variance, build_shorten_layered,
    },
    curios_utilities::{Plicity, SyntaxRegistry},
    std::{fmt, rc::Rc},
};

/// Why the kernel refused a term.
///
/// What an [`Error::Arity`] counted. One refusal, many tallies — a message reading `expected 1, found 0` with nothing to say what the 1 was would send a reader to the kernel's source to learn it was a universe level.
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
pub enum Error {
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
    /// A free-monoid fold whose arm reads its induction hypothesis, at an ambient goal. The hypothesis is the fold itself at the tail, and its type is the goal at the tail — an instance only a family can state once the head has been substituted away. A case split, whose arm reads none, takes an ambient goal as any other elimination does.
    AmbientFold(Term),
    /// A free-monoid fold whose arm reads its induction hypothesis, under a motive that mentions its scrutinee other than through the binder. The hypothesis is typed at the motive opened at the tail, and the arm is checked with the scrutinee specialized to the cons value — so a captured occurrence is specialized too, and the hypothesis is assumed at the goal of the arm instead of at the tail's, which proves the goal from itself.
    FoldMotiveCapturesScrutinee(Term),
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
    /// A written mark that is not its slot's: an argument's against its parameter's, or an arm binder's against its payload's. `counted` says which, and `position` counts from one.
    Mark {
        counted: Counted,
        position: usize,
        declared: Plicity,
        written: Plicity,
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
    /// A declaration carried as indifferent to a universe level its own telescopes mention. The item walk compared two instances of `name` on the carried vector's word, so a vector claiming an irrelevance the recomputation denies is a conversion nothing licensed.
    VarianceDenied { name: Global, level: usize },
    /// A constructor payload, uniform parameter, or field whose level exceeds the declaring family's result sort — the size condition that keeps an inductive from containing the universe it lives in.
    Oversized { domain: Level, bound: Level },
    /// A proof or a type that reaches something not known to terminate, or that is such a thing itself — an inline `rec` group that does not descend, or a call to a host row that diverges. Erasure deletes both halves, so a proof that may not terminate proves anything and a type that may not terminate reties the negative knot positivity forbids. `reached` names the offending definition, or is absent when the position is partial in itself and there is no name to blame.
    NotTotal {
        erased: Erased,
        reached: Option<Global>,
    },
    /// A field of a `Prop`-sorted structure that is not a proof. Irrelevance identifies every inhabitant of a proposition, while projection reads a field back out without meeting any elimination guard, so an informative field hands two convertible values to the same projection — a type-valued field included.
    Informative { field: Box<Term> },
    /// An entrypoint that states no type to judge it at. Elaboration writes the type it judged the body at, so an entry without one did not come through it; inferring one here would accept a program against a contract nobody checked.
    UntypedEntry,
    /// A declaration whose universe constraints name something the declaration does not have: a parameter past its own count, or a metavariable elaboration should have solved. Either way the context cannot be instantiated, so assuming it means assuming something with no meaning.
    UnclosedUniverses,
    /// A declaration whose own universe constraints have no solution. The kernel *assumes* an item's constraints while checking it, so an unsatisfiable set is a hypothesis set from which everything follows: level questions stop being answered by the hierarchy and start being answered by the contradiction.
    UnsatisfiableUniverses,
    /// A universe instance whose stated levels do not satisfy the scheme's constraint set. The scheme declared `lower ≤ upper` over its parameters; at this instance's levels, under the hypotheses of the item being checked, the inequality does not hold — which is the route back to the paradox the hierarchy exists to exclude.
    UniverseInstance { lower: Level, upper: Level },
    /// An occurrence of a universe-polymorphic definition that states no instance. Such an occurrence denotes no particular instance, which is why `Globals::value` withholds its body; reading its *type* regardless hands back the scheme's own parameters, which are then read as the ambient item's, and skips `check_instance` entirely — so the scheme's constraints are discharged by nothing and a use the stated-instance spelling refuses is admitted by dropping the instance.
    MissingUniverseInstance { name: Free, expected: usize },
}

impl Exhaustion for Error {
    fn refusal(&self) -> Option<&ReduceError> {
        match self {
            Self::Reduce(error) => error.refusal(),
            _ => None,
        }
    }
}

impl From<ReduceError> for Error {
    fn from(error: ReduceError) -> Self {
        Error::Reduce(error)
    }
}

impl From<UniverseError> for Error {
    fn from(error: UniverseError) -> Self {
        Error::Reduce(ReduceError::Universe(error))
    }
}

impl Error {
    /// Render this refusal with global names shortened against `module`'s symbol table and a nominal family's implicit parameters marked — the two axes a reader needs to recognize the types they wrote.
    ///
    /// Universe instances are deliberately *not* suppressed here, unlike an elaboration diagnostic. A kernel refusal is often *about* the universes: `convert.rs` records one reading "a ground `Type` against a `Type.{u}`", and erasing the instance would reduce that to `Type` against `Type`. The same call `wonder stage`'s dumps make, for the same reason — a reader looking at the checker wants the levels the checker is arguing about.
    pub fn format_with(
        &self,
        module: &Module,
        predecessors: &[&Module],
        syntax: &SyntaxRegistry,
    ) -> String {
        // See `curios_elab::Error::format_with`: a module carries only its own declarations, so *both* halves of the spelling have to be told what its environment put in scope — the shortening table and the plicity marks alike. The shortening keeps the two apart as that one does, so `module`'s own declarations settle their spelling before the environment competes for it.
        let own = module.module_symbols();
        let mut symbols = Vec::new();
        let mut plicities = module.nominal_plicities();
        for unit in predecessors {
            symbols.extend(unit.module_symbols());
            for (name, marks) in unit.nominal_plicities() {
                plicities.entry(name).or_insert(marks);
            }
        }

        let spelling = Rc::new(
            Spelling::default()
                .with_short_names(Rc::new(build_shorten_layered(&own, &symbols)))
                .with_nominal_plicities(Rc::new(plicities))
                .with_string_literals(Global::Authored(syntax.string.string.qualifier())),
        );
        Displayed(self, spelling).to_string()
    }
}

/// The faithful rendering: core's own names, every universe shown. A refusal reported to a reader goes through [`Error::format_with`].
impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        Displayed(self, Rc::new(Spelling::default())).fmt(formatter)
    }
}

/// A refusal paired with the [`Spelling`] its terms render under — the parameter `Display::fmt` cannot take. Local to this crate because the orphan rule forbids implementing a foreign trait for a foreign wrapper, and because the axes a kernel refusal wants are not the ones an elaboration diagnostic wants.
struct Displayed<'a>(&'a Error, Rc<Spelling>);

/// Every arm below rebinds its term fields through the spelling before interpolating them. A field left unrebound renders core's own spelling and still compiles, which is why the rebinding is mechanical rather than left to each `write!`.
impl fmt::Display for Displayed<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = &self.1;
        match self.0 {
            Error::Reduce(ReduceError::Exhausted {
                category,
                remaining,
                attempted,
            }) => write!(
                formatter,
                "the kernel's reduction budget ran out: {category} needed {attempted} units with {remaining} left"
            ),
            Error::Reduce(_) => formatter.write_str("reduction failed in the kernel"),
            Error::Unbound(name) => write!(formatter, "unbound name `{name}`"),
            Error::Undeclared(name) => {
                write!(formatter, "no declaration registered for `{name}`")
            }
            Error::Unclassified(type_) => {
                let type_ = type_.spelled(spelling);
                write!(formatter, "cannot determine the sort of `{type_}`")
            }
            Error::NotASort(term) => {
                let term = term.spelled(spelling);
                write!(formatter, "`{term}` is not a universe")
            }
            Error::NotAMotive(term) => {
                let term = term.spelled(spelling);
                write!(
                    formatter,
                    "`{term}` is not a valid motive: it must be well-typed and land in a sort",
                )
            }
            Error::AmbientFold(goal) => {
                let goal = goal.spelled(spelling);
                write!(
                    formatter,
                    "a fold that reads its induction hypothesis needs a motive to type it, and `{goal}` is an ambient goal",
                )
            }
            Error::FoldMotiveCapturesScrutinee(scrutinee) => {
                let scrutinee = scrutinee.spelled(spelling);
                write!(
                    formatter,
                    "a fold's motive may only reach its scrutinee `{scrutinee}` through the binder it declares, or its induction hypothesis would be typed at the arm's own goal",
                )
            }
            Error::Mismatch { inferred, expected } => {
                let inferred = inferred.spelled(spelling);
                let expected = expected.spelled(spelling);
                write!(formatter, "expected `{expected}`, found `{inferred}`")
            }
            Error::NotAFunction(type_) => {
                let type_ = type_.spelled(spelling);
                write!(formatter, "`{type_}` is not a function type")
            }
            Error::NotATuple(type_) => {
                let type_ = type_.spelled(spelling);
                write!(formatter, "`{type_}` has no components")
            }
            Error::Arity {
                counted,
                expected,
                actual,
            } => {
                write!(formatter, "{counted}: expected {expected}, found {actual}")
            }
            Error::Mark {
                counted,
                position,
                declared,
                written,
            } => {
                let spelled = |mark: &Plicity| match mark {
                    Plicity::Explicit => "plain",
                    Plicity::Implicit => "`@`",
                    Plicity::Witness => "`use`",
                };
                write!(
                    formatter,
                    "{counted}: the one at position {position} is written {}, and its slot is {}",
                    spelled(written),
                    spelled(declared)
                )
            }
            Error::LargeElimination(name) => write!(
                formatter,
                "cannot eliminate the proposition `{name}` into a relevant result",
            ),
            Error::NotCore(term) => {
                let term = term.spelled(spelling);
                write!(formatter, "`{term}` is elaboration-only syntax")
            }
            Error::RepeatedTag(tag) => {
                write!(
                    formatter,
                    "constructor tag `{tag}` is declared more than once"
                )
            }
            Error::MissingArm { family, tag } => write!(
                formatter,
                "no arm for `{tag}` of `{family}`, and its case is not impossible",
            ),
            Error::NotDescending { type_ } => {
                let type_ = type_.spelled(spelling);
                write!(
                    formatter,
                    "a recursive proof or type at `{type_}` does not descend",
                )
            }
            Error::UntypedEntry => {
                write!(formatter, "the entrypoint states no type to judge it at")
            }
            Error::UnclosedUniverses => write!(
                formatter,
                "this declaration's universe constraints name a parameter it does not declare",
            ),
            Error::UnsatisfiableUniverses => write!(
                formatter,
                "this declaration's universe constraints have no solution",
            ),
            Error::Informative { field } => write!(
                formatter,
                "a `Prop` structure carries an informative field at `{field}`",
            ),
            Error::NotTotal {
                erased,
                reached: Some(reached),
            } => write!(
                formatter,
                "a {erased} position reaches `{reached}`, which is not known to terminate",
            ),
            Error::NotTotal {
                erased,
                reached: None,
            } => write!(
                formatter,
                "a {erased} position does not terminate: it is a non-descending recursion or an exit",
            ),
            Error::NotPositive {
                name,
                part,
                polarity,
            } => write!(
                formatter,
                "`{part}` of `{name}` reaches back to it at {polarity:?}, which is not strictly positive",
            ),
            Error::VarianceDenied { name, level } => write!(
                formatter,
                "`{name}` is carried as indifferent to its universe level {level}, which its declaration mentions",
            ),
            Error::Oversized { domain, bound } => write!(
                formatter,
                "a declaration domain at level `{domain}` exceeds its family's `{bound}`",
            ),
            Error::UniverseInstance { lower, upper } => write!(
                formatter,
                "this instance does not satisfy its scheme's `{lower} <= {upper}`",
            ),
            Error::MissingUniverseInstance { name, expected } => write!(
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
    type Error = Error;

    /// Plain reduction, as the seam asks: a shared analysis borrows no opinion of the kernel's conversion through the term it reads.
    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(self.plainly(|kernel| kernel.reduce_forced(term.clone()))?)
    }

    fn assumption(&self, name: &Free) -> Option<&Term> {
        self.local_type(name)
    }

    fn is_local(&self, name: &Free) -> bool {
        self.local_type(name).is_some()
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
/// Deliberately small, and deliberately *composed*. The elaborator's `Context` carries fifteen-odd stores — caches, parked goals, refinement layers, a metavariable heap — and each is a place where an answer can come from something other than the term in hand. Here each independent job is a component that states its own invariants, and what remains is the composition and the couplings that genuinely cross it.
///
/// The one such coupling is worth naming, because it is why `Globals::insert` reports rather than acts: overwriting a definition invalidates every remembered reduct, which is a fact about `Globals` *and* `Memos` and therefore belongs to neither.
///
/// Growing this struct is how independence gets lost. A new *component* should have to argue for itself.
pub struct Kernel {
    /// What the walk in progress has opened.
    scope: Scope,
    /// What a judgment may consume, and what it has.
    spend: Spend,
    /// Remembered weak-head reducts, replayed rather than re-derived.
    memos: Memos,
    /// The erased positions this walk recorded — an output, not an input.
    positions: Positions,
    /// The recursive calls this walk typed, and each checked group's verdict — an output, not an input.
    calls: Calls,
    /// What the item being judged has read of other items — an output, not an input.
    reads: ReadRecorder,
    /// Top-level definitions and the nominal registry.
    globals: Globals,
    /// The registered spellings this walk may need to *state* a type — today the propositions the guarded operations take as bounds, read through `Intrinsic::signature`.
    ///
    /// Handed in rather than defaulted, and deliberately not optional. An absent registry could only mean skipping the bound check, and a check that silently does not run is worse than one that is missing outright: the kernel would report a verdict it had not reached.
    syntax: SyntaxRegistry,
    /// The constraint set of the item being checked — its own declared hypotheses, assumed while its parameters are held abstract. A generic definition is valid exactly when it checks *under* its constraints, so the level judgments below consult these; discarding them would refuse a correct polymorphic definition.
    ///
    /// The one field with no component of its own: it is a single vector replaced wholesale at each declaration boundary, and wrapping it would state nothing the type does not.
    assumed: Vec<UniverseConstraint>,
    /// Whether the closed machine may run — false only in the differential fixture's strategy arm, which is what makes the machine's reducts checkable against the strategy's at all.
    machine: bool,
    /// Whether the terms in hand are already by value: set where reduction or conversion is entered from typing, so the entries beneath read what they are handed, and put back where typing binds a `let`. See [`Kernel::by_value`].
    reading: bool,
}

impl Kernel {
    /// A kernel that may spend `budget` reduction steps per judgment, stating types through `syntax`.
    pub fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            scope: Scope::default(),
            spend: Spend::new(budget),
            memos: Memos::new(true),
            positions: Positions::default(),
            calls: Calls::default(),
            reads: ReadRecorder::default(),
            globals: Globals::default(),
            syntax,
            assumed: Vec::new(),
            machine: true,
            reading: false,
        }
    }

    /// The registered spellings this walk states types through.
    pub(crate) fn syntax(&self) -> SyntaxRegistry {
        self.syntax
    }

    /// Start this kernel's environment from `globals` — the scope an earlier walk established — rather than from nothing.
    ///
    /// Set wholesale where [`Kernel::define`] has to report an overwrite, because a kernel is seeded before it has judged anything: there are no remembered reducts yet for a replaced name to invalidate.
    pub(crate) fn seed(&mut self, globals: &Globals) {
        self.globals = globals.clone();
    }

    /// A kernel whose evaluation memos are off — every reduction re-derived from scratch. Exists for one purpose: asserting that memoization changes no *semantic* verdict. It may change a resource one, since a term-keyed hit is free and an uncached walk therefore spends at least as much; see the `spend` module's documentation for why that is the whole of what was given up.
    pub fn uncached(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            memos: Memos::new(false),
            ..Self::new(budget, syntax)
        }
    }

    /// The remembered weak-head reduct of a local-free `term`, per entry point, replayed for nothing.
    ///
    /// The one hit that cannot fail, because it spends no steps: the kernel did not perform this computation, and charging it what a memo-free evaluator would have spent would run a budget out on work nobody did. [`Spend::charge_nothing`] and [`Memos`] state the two halves of why that is safe.
    pub(crate) fn whnf_hit(&mut self, term: &Term, forced: bool) -> Option<Term> {
        let replay = self.memos.whnf(term, forced, self.scope.plain())?;

        Some(self.spend.charge_nothing(replay))
    }

    /// The remembered type of `term`, with nothing spent and nothing minted.
    ///
    /// **Nothing minted, unlike a reduct's hit.** A reduct's hit mints the identities its computation did, so that every later identity lands where a recomputation would have put it. For this table that recomputation is what it exists to avoid: it types a graph once per node where recomputing types it once per path, so the identities a recomputation would mint are counted over the tree, and on the graphs the table is for they outgrow the identity space — replayed, a `Str/split_once` claim stated in a type exhausts the 32-bit identity space. What the replay protected holds without it: the counter never falls, so every identity minted after a hit is above those the remembered inference opened, and it closed them before it returned. [`Spend`]'s module documentation states what is given up.
    ///
    /// **Read off the scope where it is.** A term naming a local is typed off the binders in scope, and a term typed while a case equation is in force is filed as one that may rest on it — the machine stands aside under an equation for that reason. Such a type is filed beside the binders it was inferred under and taken only while they stand, in a table cleared wherever an equation moves or a local is re-typed; [`Memos`] states the two lives.
    ///
    /// **Never a term naming a member of a group whose body is being checked.** A recursive call is an application of that local, recorded where it is typed and graded against the member whose body states it. A hit types nothing, so a term that can hold a call is typed at every occurrence: one body stated by two members holds a call from each.
    pub(crate) fn infer_hit(&self, term: &Term) -> Option<Term> {
        if self.calls.names_member(term) {
            return None;
        }

        self.standing(self.memos.infer(term, self.has_refinements()))
    }

    /// Remember `term`'s type, under the life and the refusal [`Kernel::infer_hit`] reads it by.
    pub(crate) fn infer_store(&mut self, term: Term, type_: Term) {
        if !self.calls.names_member(&term) {
            self.memos
                .store_infer(term, self.has_refinements(), self.scope.prefix(), type_);
        }
    }

    /// Whether `term` is remembered to check at `expected`, as [`Kernel::infer_hit`] remembers a type and under its refusal.
    pub(crate) fn check_hit(&self, term: &Term, expected: &Term) -> bool {
        !self.calls.names_member(term)
            && self
                .standing(self.memos.checked(term, expected, self.has_refinements()))
                .is_some()
    }

    /// Remember that `term` checks at `expected`.
    pub(crate) fn check_store(&mut self, term: &Term, expected: &Term) {
        if !self.calls.names_member(term) {
            self.memos
                .store_checked(term, expected, self.has_refinements(), self.scope.prefix());
        }
    }

    /// The remembered verdict of comparing `this` with `that` at `type_`, under the lives a typing has. Only a verdict reached with no goal in progress assumed is ever filed, so it is the verdict under whatever goals are in progress now.
    pub(crate) fn convert_hit(&self, type_: &Term, this: &Term, that: &Term) -> Option<bool> {
        self.standing(self.memos.converted(
            type_,
            this,
            that,
            self.has_refinements(),
            self.scope.plain(),
        ))
    }

    /// Remember the verdict of comparing `this` with `that` at `type_`.
    pub(crate) fn convert_store(&mut self, type_: &Term, this: &Term, that: &Term, verdict: bool) {
        self.memos.store_converted(
            (type_.clone(), this.clone(), that.clone()),
            self.has_refinements(),
            self.scope.plain(),
            self.scope.prefix(),
            verdict,
        );
    }

    /// The remembered sort of `type_`, with nothing spent and nothing minted, as a remembered type is handed back: a sort's hit replays nothing for [`Kernel::infer_hit`]'s reason. A local-free type's, read with no case equation in force, is the declaration's; one read off the scope, for a type naming a local or under an equation, is taken only while the binders it was read under stand.
    pub(crate) fn sort_hit(&self, type_: &Term) -> Option<Sort> {
        self.standing(
            self.memos
                .sort(type_, self.has_refinements(), self.scope.plain()),
        )
    }

    /// A remembered answer, where it is the declaration's or the binders it was read under all still stand.
    fn standing<T>(&self, remembered: Option<(Option<Prefix>, T)>) -> Option<T> {
        let (prefix, answer) = remembered?;

        prefix
            .is_none_or(|prefix| self.scope.stands(prefix))
            .then_some(answer)
    }

    /// Remember `type_`'s sort, beside the binders in scope now.
    pub(crate) fn sort_store(&mut self, type_: Term, sort: Sort) {
        self.memos.store_sort(
            type_,
            self.has_refinements(),
            self.scope.plain(),
            self.scope.prefix(),
            sort,
        );
    }

    /// Remember a `term`'s weak-head reduct and the identities computing it minted.
    ///
    /// **Stored for nothing.** [`Memos::begin_declaration`] clears the table exactly where [`Spend::restore_budget`] fires, and every node it holds was built under that budget, which charges a construction what it builds — so the budget that built an entry is its bound. Charging it besides, against a compilation-wide allowance at the tree footprint of key and reduct, would bill entries that die with the declaration, and bill them by their trees where a reduct is a graph whose tree has `2^n` nodes.
    pub(crate) fn whnf_store(&mut self, term: Term, forced: bool, replay: Replay) {
        self.memos
            .store_whnf(term, forced, self.scope.plain(), replay);
    }

    /// The heaviest declaration this kernel has walked — what it spent, and how deep it went.
    ///
    /// An observation for a measurement: nothing in the kernel reads it, and it exists so a figure can be stated with a probe beside it instead of bisected against a budget from outside the compiler. See [`Consumption`] for why depth is the row it separates out.
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

    /// [`Kernel::levels_eq`] at the positions the family `name` is invariant in: a level its vector calls irrelevant is compared at nothing. A name the registry does not hold, or a position its vector does not reach, compares its level, which is what every level was compared at before a vector existed.
    pub(crate) fn instances_eq(&self, name: &Global, left: &[Level], right: &[Level]) -> bool {
        left.len() == right.len()
            && left
                .iter()
                .zip(right)
                .enumerate()
                .all(|(index, (this, that))| {
                    self.variance(name, index) == Variance::Irrelevant || self.level_eq(this, that)
                })
    }

    /// The family `name`'s variance in its `index`th universe parameter, as its registry entry carries it.
    fn variance(&self, name: &Global, index: usize) -> Variance {
        self.induct_decl(name)
            .map(|declaration| declaration.variance(index))
            .or_else(|| {
                self.struct_decl(name)
                    .map(|declaration| declaration.variance(index))
            })
            .unwrap_or(Variance::Invariant)
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
    ) -> Result<(), Error> {
        if levels.len() != context.parameter_count {
            return Err(Error::Arity {
                counted: Counted::UniverseLevels,
                expected: context.parameter_count,
                actual: levels.len(),
            });
        }

        let instantiate = |level: &Level| -> Result<Level, Error> {
            if level.params().any(|param| param.0 >= levels.len()) {
                return Err(Error::UniverseInstance {
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
                return Err(Error::UniverseInstance { lower, upper });
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

    /// Record a top-level name with a type and no body: an assumption that never unfolds, so a permanent neutral. No walk records one — a module's items are defined with their bodies, and a `foreign` is a term `infer` types rather than a name — so its callers are the fixtures that need an opaque head.
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
        self.reads.signature(name);
        self.globals.induct_decl(name)
    }

    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.reads.signature(name);
        self.globals.struct_decl(name)
    }

    /// What the item being judged has read of other items since the last take, leaving nothing behind for the next.
    pub(crate) fn take_reads(&mut self) -> Reads {
        self.reads.take()
    }

    /// Open a binder: bring `name : type_` into scope for the walk in progress.
    ///
    /// Locals are a stack, and closing them is `Kernel::scoped`'s job rather than the caller's — it is the only bracket there is, so a binder opened here is closed on every path out of the walk that opened it.
    ///
    /// **Nothing the kernel mints afterwards can be `name`.** A local a caller hands in was minted by a counter the kernel never saw, so the kernel's own is raised past it here, where it enters the context, rather than to a floor the caller computes. A binder the kernel minted itself is already below its counter, so raising it for one changes nothing — and a remembered reduct's replay, which counts the kernel's own mints, is untouched. A local that is never assumed is refused before any judgment meets it (`curios_core::free_locals_outside`).
    pub fn assume(&mut self, name: &Free, type_: &Term) {
        if let Some(index) = name.local_index() {
            self.spend.reserve(index);
        }
        // A re-typed local is what a remembered sort of a type naming it was read off.
        if self.scope.assume(name, type_) {
            self.memos.begin_equations();
        }
    }

    /// Open a `let`'s binder: [`Kernel::assume`] for a local that stands for `value`, which the caller has checked at `type_` — `encloses_partial` saying that check enclosed a group that does not descend.
    ///
    /// **A `let` is bound for typing and read by value by everything else.** The tail is typed over the binder, so a value named twice is typed once; a variable is typed by the binder's type. Whatever reads a term for more than its type is handed the term with the name unfolded ([`Kernel::by_value`]), which is the term a substituted `let` leaves, so reduction's tables, an arm's equations, the recorded positions and the graded calls are keyed and read as they are where a `let` is substituted. The local stands for one value for as long as its opening does, so what the typing tables remember of a term naming it stays true while its binder stands — the life they have.
    pub(crate) fn bind(&mut self, name: &Free, type_: &Term, value: &Term, encloses_partial: bool) {
        if let Some(index) = name.local_index() {
            self.spend.reserve(index);
        }
        // A local bound again stands for another value, which is what a term naming it was typed through: the typings go, and nothing else does, since every other table is keyed by value and names no `let`.
        if self.scope.bind(name, type_, value, encloses_partial) {
            self.memos.begin_sizes();
        }
    }

    /// `term` by value — see the scope's `by_value`. A term already by value is handed back as it is.
    pub(crate) fn by_value(&self, term: &Term) -> Term {
        match self.reading {
            true => term.clone(),
            false => self.scope.by_value(term),
        }
    }

    /// Run `read` over terms that are by value: reduction and conversion entered inside it read what they are handed. Entered where either is reached from typing, so a term is unfolded once at the outermost entry and never beneath it, where every term is a subterm or a reduct of one that was.
    pub(crate) fn reading<T>(&mut self, read: impl FnOnce(&mut Self) -> T) -> T {
        let outer = std::mem::replace(&mut self.reading, true);
        let outcome = read(self);
        self.reading = outer;

        outcome
    }

    /// Whether the terms in hand are already by value.
    pub(crate) fn reads_by_value(&self) -> bool {
        self.reading
    }

    /// Run `walk` over terms that may name a `let`-bound local: the bracket typing a `let`'s tail runs in, wherever it is reached from.
    pub(crate) fn naming<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        let outer = std::mem::replace(&mut self.reading, false);
        let outcome = walk(self);
        self.reading = outer;

        outcome
    }

    /// Whether `name` is a local a `let` bound.
    pub(crate) fn binds(&self, name: &Free) -> bool {
        self.scope.binds(name)
    }

    /// The `let`-bound locals in scope — see the scope's `bound_locals`.
    pub(crate) fn bound_locals(&self) -> Vec<LetBound> {
        self.scope.bound_locals()
    }

    /// Step `walk` past its next binder: mint one from the entry's hint, open it at `domain`, and hand it back for the caller's own capture.
    pub(crate) fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        let binder = walk.advance_fresh(|hint| self.fresh(hint));
        self.assume(&binder, domain);
        binder
    }

    /// Run `walk` with every binder it opened — and every case equation it assumed — closed again afterwards, on the failing path as well as the succeeding one.
    ///
    /// **The only way to open a binder scope.** [`Scope`]'s `mark` and `retract` are `pub(super)` and this is their only caller anywhere, which is what makes that true. A judgment that opened a binder and returned early would leak it into the conversion history, where the local context is part of the goal key, and no amount of care spread over a dozen call sites makes that structural. Written as a bracket rather than a guard object because the walks it wraps take `&mut Kernel` throughout, and a guard holding the borrow would leave them nothing to be called with.
    pub(crate) fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        let mark = self.scope.mark();
        // What the call recorder learned inside the bracket — an arm's refinements, a group opened for its bodies — is about the binders the bracket opened, so it retracts with them.
        let calls = self.calls.mark();
        let outcome = walk(self);
        let established = self.calls.retract(calls);
        // Retracting an equation changes what a local-bearing term reduces to, so what was read off the scope under it goes with it; retracting what an arm established for the call recorder changes how a group typed under it closes, so the typings do. A bracket that assumed neither leaves the tables alone — most do, and what they remembered is still true; an answer read under a binder closed here is refused by its `Prefix` instead.
        if self.scope.retract(mark) {
            self.memos.begin_equations();
        } else if established {
            self.memos.begin_sizes();
        }

        outcome
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` — as written, and as its dispatch resolves — is `value`, definitionally. Assumed inside the arm's [`Kernel::scoped`] bracket, which is what scopes it.
    ///
    /// The resolved spelling is computed before the equation is pushed, so it rests only on the equations outside it, which retract no earlier than it does — the view [`Kernel::settle_refinement`] has to reconstruct by withholding, had for free here. Only a local-bearing scrutinee is resolved, since [`Scope::refine`](scope::Scope) records nothing else, and only one with no head a probe presents already. The resolution is a [`Probe`], as a settlement is: a scrutinee with no value at the type level records the written spelling alone.
    pub(crate) fn refine(&mut self, scrutinee: Term, value: Term) -> Result<(), ReduceError> {
        let resolved = match scrutinee.has_local_free() && scrutinee.head_key().is_none() {
            true => whnf::resolved_spelling(self, &scrutinee)
                .probed()?
                .flatten(),
            false => None,
        };
        // What was read off the scope goes where the equations in force change, and an arm that records none — its scrutinee names no local, a literal an outer arm substituted there among them — leaves them as they were.
        if self.scope.refine(scrutinee, resolved, value) {
            self.memos.begin_equations();
        }

        Ok(())
    }

    /// Restate the equations in force under an arm's solution ([`Scope::restate`](scope::Scope)). Which equations answer has changed where any was restated, so the local-bearing reducts remembered before it go.
    pub(crate) fn restate_refinements(&mut self, solutions: &[(Free, Term)]) {
        if self.scope.restate(solutions) {
            self.memos.begin_equations();
        }
    }

    /// The case value `term` is refined to under the written spelling, innermost arm first.
    pub(crate) fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.scope.refinement_of(term)
    }

    /// The case value `term` is refined to under a reduced spelling already settled, innermost arm first.
    pub(crate) fn refinement_of_reduct(&self, term: &Term) -> Option<Term> {
        self.scope.refinement_of_reduct(term)
    }

    /// The equations in force whose reduced spelling is settled and that `candidate` could be a reduct of, innermost first, each as its position, that spelling and its value.
    pub(crate) fn settled_refinements(
        &self,
        candidate: &Term,
        proofs: &[Free],
    ) -> Vec<(usize, Term, Term)> {
        self.scope.settled_refinements(candidate, proofs)
    }

    /// The binders `term` names that are themselves proofs: opened at a proposition, so conversion reads them nowhere, and a stuck form naming one the key of an equation does not may still be that key's term (`curios_analysis::could_reduce_to`).
    ///
    /// **Each binder is asked once**, where a stuck form naming it first reaches the equations in force, and remembered with the binder. It is asked by plain reduction, which no question interrupts, and counted not a proof while it is being asked, so a type whose classification comes back to its own binder ends there, toward refusal. A failure that is no exhaustion of the budget is that the binder is not known to be a proof.
    pub(crate) fn proofs_named(&mut self, term: &Term) -> Result<Vec<Free>, ReduceError> {
        let mut proofs = Vec::new();
        for name in term.free_vars_shared().iter() {
            let proof = match self.scope.classified(name) {
                Some(proof) => proof,
                None => {
                    let Some(type_) = self.scope.local_type(name).cloned() else {
                        continue;
                    };
                    self.scope.classify(name, false);
                    let sort = self.plainly(|kernel| Sort::of(kernel, &type_));
                    let proof = sort.probed_refusal()?.is_some_and(|sort| sort.is_prop());
                    self.scope.classify(name, proof);
                    proof
                }
            };
            if proof {
                proofs.push(*name);
            }
        }
        Ok(proofs)
    }

    /// The innermost equation in force whose reduced spelling has not been asked for and could be `candidate`, as its position and the term to reduce.
    pub(crate) fn unasked_refinement(
        &self,
        candidate: &Term,
        proofs: &[Free],
    ) -> Option<(usize, Term)> {
        self.scope.unasked_refinement(candidate, proofs)
    }

    /// How many equations in force `candidate` could be a reduct of.
    #[cfg(feature = "profile")]
    pub(crate) fn reachable_refinements(&self, candidate: &Term, proofs: &[Free]) -> usize {
        self.scope.reachable_refinements(candidate, proofs)
    }

    /// Settle the reduced spelling of the equation at `index`, reducing `key` with that equation — and every equation inside it — withheld.
    ///
    /// **The whole of what the two-tier key defers.** Recording an equation costs nothing; this is where the reduction happens — at most once per equation, and only because a probe presented a term the written spelling did not answer.
    ///
    /// Withholding is [`Scope::hide_refinements_from`]'s to justify. An error settles the equation as having no reduced spelling, so the attempt is paid once rather than repeated at every later probe; the settlement is a [`Probe`], so exhaustion settles it the same way and propagates besides.
    pub(crate) fn settle_refinement(&mut self, index: usize, key: Term) -> Result<(), ReduceError> {
        // Withholding equations is a change to the set in force, and so is restoring them: the local-bearing reducts remembered on either side of the settlement must not answer on the other.
        let outer = self.scope.hide_refinements_from(index);
        self.narrowed();
        // The reduced spelling is held operand-canonical, the form `refined_reduct` brings a probed value to before comparing — see `canonical_operands`.
        let reduct = whnf(self, key).and_then(|reduct| whnf::canonical_operands(self, &reduct));
        self.scope.show_refinements(outer);
        self.narrowed();

        match reduct.probed() {
            Ok(settled) => {
                self.scope.settle_refinement(index, settled);
                Ok(())
            }
            Err(spent) => {
                self.scope.settle_refinement(index, None);
                Err(spent)
            }
        }
    }

    /// The equations in force were narrowed, or restored, for the reduction running now: what it read off the scope on the other side of that line goes. Only plain reduction runs inside a span of it ([`Kernel::plainly`]), so there what plain reduction remembered goes alone, and what a judgment's reduction remembered stands when the span ends.
    fn narrowed(&mut self) {
        match self.scope.plain() {
            true => self.memos.begin_plain(),
            false => self.memos.begin_equations(),
        }
    }

    /// Whether reduction is plain: it asks the kernel's conversion nothing ([`Kernel::plainly`]).
    pub(crate) fn plain(&self) -> bool {
        self.scope.plain()
    }

    /// Run `read` with reduction plain: it asks the kernel's conversion nothing, at a stuck fold or at a missed equation.
    ///
    /// **Two readers need it.** A question reduction puts to conversion is answered by plain reduction: conversion reduces, and left to ask again, a term that reaches itself through an unfolding would be asked about without end, so a question is one conversion the budget bounds, over the reduction that stood before reduction asked anything. And a shared analysis reads a term by it (`Env::force`): conversion's proof irrelevance and its recurrence rule are sound because acceptance waits on totality, which must therefore read terms on no verdict of conversion's.
    ///
    /// **Each of the two reductions keeps its own answers.** A local-bearing reduct is remembered by the reduction that took it, and so are a sort and a comparison's verdict, which are read through reducts ([`Memos`]); an equation's reduced spelling is settled once for each ([`Scope::plain`]). So what plain reduction made of a term never answers for a judgment's, nor the other way, and no answer depends on which of the two asked first.
    ///
    /// **Nothing is typed under it.** A question is a conversion, which reduces and reads a neutral's sort off its head, and an analysis only reduces. So the types and the checks the kernel remembers, and the erased halves it classifies positions by, are a judgment's alone, and none is filed twice ([`Kernel::record_checked`] holds the line).
    pub(crate) fn plainly<T>(&mut self, read: impl FnOnce(&mut Self) -> T) -> T {
        let previous = self.scope.begin_plain();
        let answer = read(self);
        self.scope.end_plain(previous);

        answer
    }

    /// Whether `term` is the scrutinee of the equation at `index`, whose reduced spelling is `key`, as reduction asks conversion of two stuck forms that are no operations of the algebra.
    ///
    /// Compared with that equation and every equation inside it withheld, which is the view the spelling was settled under ([`Kernel::settle_refinement`]): under its own equation the key reduces to the case value, and nothing would convert with it.
    pub(crate) fn asked_scrutinee(
        &mut self,
        index: usize,
        term: &Term,
        key: &Term,
    ) -> Result<bool, ReduceError> {
        curios_profile::profile!("kernel::asked_scrutinee");
        self.plainly(|kernel| {
            let outer = kernel.scope.hide_refinements_from(index);
            kernel.narrowed();
            let same = convert::grounded(kernel, term, key);
            kernel.scope.show_refinements(outer);
            kernel.narrowed();

            same
        })
    }

    /// Whether any arm's case equation is currently assumed — the judgment-side half of the closed machine's gate.
    pub(crate) fn has_refinements(&self) -> bool {
        self.scope.has_refinements()
    }

    /// The types of the binders currently in scope, outermost first.
    pub(crate) fn local_types(&self) -> Vec<Term> {
        self.scope.local_types()
    }

    /// The context the conversion history keys a goal on: the binders' types with every binder renamed to its position, taken once per binder when it opens — see the scope's `history_context`.
    pub(crate) fn history_context(&self) -> Vec<Term> {
        self.scope.history_context()
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
    /// A definition with universe parameters is refused here rather than answered, which is [`Globals::value`]'s rule applied to the other half of a definition. A bare occurrence denotes no particular instance, so there is no instantiation to report a type at: handing back the stored scheme type reads that scheme's parameters as the ambient item's (see `documentation/design/soundness/formation/universe-instances-and-constraints.md`) and reaches [`Kernel::check_instance`] never, so the scheme's constraints go undischarged. A local is exempt because it is monomorphic: it was opened at one type, and there is no scheme to instantiate.
    pub(crate) fn type_of(&self, name: &Free) -> Result<Option<&Term>, Error> {
        if let Some(local) = self.scope.local_type(name) {
            return Ok(Some(local));
        }

        match self.scheme_of(name) {
            None => Ok(None),
            Some((type_, universes)) => match universes.parameter_count {
                0 => Ok(Some(type_)),
                expected => Err(Error::MissingUniverseInstance {
                    name: *name,
                    expected,
                }),
            },
        }
    }

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(crate) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        if let Some(global) = name.as_global() {
            self.reads.signature(global);
        }
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
        if let Some(global) = name.as_global() {
            self.reads.body(global);
        }
        self.globals.value(name)
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(crate) fn value_at(&self, name: &Free) -> Option<&Term> {
        if let Some(global) = name.as_global() {
            self.reads.body(global);
        }
        self.globals.value_at(name)
    }

    /// Record `term` as an erased position if the type it was judged at makes it one: a term at a `Prop`-sorted type is a proof, and one at a sort is a type.
    ///
    /// Called from both `check` and `infer`, because a term's type is its type however the judgment reached it. The orchestration lives here rather than on [`Positions`] because the middle of it — `erased_half` — needs the whole kernel; see `Positions::begin` on why that bracket cannot be a closure.
    pub(crate) fn record_checked(&mut self, term: &Term, type_: &Term) -> Option<usize> {
        // Typing is a judgment's: nothing is typed under plain reduction (`Kernel::plainly`), which is why the positions classified here and the types remembered beside them are filed once.
        debug_assert!(
            !self.scope.plain(),
            "a term was typed under plain reduction"
        );
        if self.positions.suppressed() {
            return None;
        }

        // The half is filed under the type by value, as its sort is.
        let type_ = &self.by_value(type_);
        let erased = match self.standing(self.memos.half(type_, self.has_refinements())) {
            Some(erased) => erased,
            None => {
                self.positions.begin();
                let outcome = erased_half(self, type_);
                let erased = self.positions.settle(outcome);
                self.memos.store_half(
                    type_.clone(),
                    self.has_refinements(),
                    self.scope.prefix(),
                    erased,
                );

                erased
            }
        };

        let erased = erased?;
        // A position is read for what its term reaches, and a `let`-bound local's name says nothing of what it stands for: the term is recorded by value, with whether a value it names enclosed a group that does not descend — the group was typed where the `let` binds it, not inside this term.
        let position = self.positions.push(&self.by_value(term), erased);
        if self.scope.names_partial(term) {
            self.positions.enclose_partial(position);
        }

        Some(position)
    }

    /// The position recorded at `position`, where there is one, enclosed a group that does not descend.
    pub(crate) fn enclose_partial(&mut self, position: Option<usize>) {
        if let Some(index) = position {
            self.positions.enclose_partial(index);
        }
    }

    /// Take this item's recorded positions and any classification that could not be decided, leaving both empty for the next item.
    pub(crate) fn take_checked(&mut self) -> (Vec<Position>, Option<Error>) {
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
