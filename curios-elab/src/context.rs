mod asking;
pub(crate) use asking::*;

mod attempt;
pub(crate) use attempt::*;

mod caches;
pub(crate) use caches::*;

mod checked;
pub(crate) use checked::*;

mod credits;
pub(crate) use credits::*;

mod declared;
pub(crate) use declared::*;

mod frames;
pub(crate) use frames::*;

mod in_scope;
pub(crate) use in_scope::*;

mod needs;
pub(crate) use needs::*;

mod posture;
pub(crate) use posture::*;

mod program;
pub(crate) use program::*;

mod traced;
pub use traced::*;

mod retyping;
pub(crate) use retyping::*;

mod solutions;
pub(crate) use solutions::*;

mod spend;
pub(crate) use spend::*;

mod typing;
pub(crate) use typing::*;

mod underway;
pub(crate) use underway::*;

#[cfg(test)]
mod awaiting_tests;
#[cfg(test)]
mod restriction_tests;
#[cfg(test)]
mod tests;

use {
    super::{
        DeadArm, Error, Provenance, UniverseMark, UniverseSolver, UniverseStateToken, Witness,
        zonk_universe_levels_scoped,
    },
    crate::{
        Published, Refusal, Sort, hold_at_one_instance, key_spelling, metavar_spines,
        read_by_value, shallow_scrutinee, zonk, zonk_solved_term_metas,
    },
    curios_analysis::{Unfolding, records_case_equation},
    curios_core::{
        Advance, Bound, ClosedMemo, ConceptDecl, Consumption, Cost, Definition, DefinitionKind,
        Extent, Free, FuncType, Global, HeadKey, HeadTag, ImplicitOrigin, Imports, InductDecl,
        Item, Level, Metavar, MetavarId, MetavarOrigin, Minted, Mints, NumLit, Probe, Quality,
        RecGroup, ReduceError, StructDecl, Subterm, Term, Totality, Trace, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseError, UniverseMetaId, UniverseRole,
        Universes, Variance, WitnessKey, WitnessOrigin, instantiate_universes_scoped,
    },
    curios_utilities::{Entropy, Mount, Plicity, Qualifier, SyntaxRegistry},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        mem,
        ops::{Deref, DerefMut},
        rc::Rc,
    },
};

/// Units of reduction work one declaration may spend before its budget is exhausted.
///
/// **Provisional.** A transition costs one unit, a construction what it builds, and a new peak of reduction depth the native frame it takes; it is not yet set against a corpus that replays a memoized construction rather than evaluating it once.
///
/// # What it is set against
///
/// **The prelude floor**, read rather than bisected. Both checkers sample each declaration's consumption as `budget::consumed` where the budget is restored, so the prelude build's profiles carry the floor as that sample's maximum. At `dd1c5dea6` the heaviest declaration `/std` holds spends **941 213 units on the elaborator** and **996 964 on the kernel**. Thirty million keeps a thirtyfold margin over the kernel's, the heavier of the two; the constant is chosen to keep roughly ten over the worst real declaration.
///
/// The two floors are not comparable as a ratio: the elaborator solves metavariables, resolves witnesses and zonks where the kernel rechecks a finished term. `curios-prelude-archive`'s `stored_prelude_measurements` reports the kernel's per unit.
///
/// **What a unit costs in bytes.** A logical unit is eight bytes of payload; process memory per unit runs higher, by the copies and term traffic the price list deliberately does not model.
///
/// # Two things it does not buy
///
/// **A single oversized construction is still affordable, and no default the prelude can build under would refuse it.** `Nat/shl(1, 400000000)` prices at 6 250 004 units and builds fifty megabytes; refusing it outright needs a default of six million, which leaves the prelude's own floor a fraction of the margin the constant keeps. What the charge buys is a ceiling — the same term at a larger numeral refuses instead of taking the machine — not one low enough to call fifty megabytes unreasonable. Squeezing both ends onto one number is the one weighted limit `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` chooses, and these are its numbers.
///
/// **A `Str` literal's ceiling is this constant divided by a per-character price the closed machine sets in transitions, not frames**: on the machine, guarded depth is flat in the length, so the frame row does not enter the price. The current price and ceiling are `curios`' `str_literal_cost_measurements`', and `a_str_literal_costs_transitions_rather_than_frames` holds the shape. Raising this constant raises the ceiling proportionally, and the trade it would make is the one stated above rather than anything about strings.
pub const DEFAULT_STEP_BUDGET: u64 = 30_000_000;

/// Γ frozen in binding order, with birth-time types. `Rc`-shared: every meta born under the same Γ shares one allocation (see [`Context::identity_snapshot`]).
type SharedTelescope = Rc<Vec<(Free, Term)>>;

/// The identity spine over a [`SharedTelescope`] — one `Var::free` per binder — shared the same way.
type SharedSpine = Rc<Vec<Term>>;

/// The frozen scope a settle-synthesized lambda's domain metavariables are born under: the settle site's ambient frame, captured by [`Context::domain_scope`] before the lambda's own binders are assumed and consumed by [`Context::fresh_domain_metavar`]. Opaque outside this module — the walk that holds one can only pass it back.
#[derive(Clone)]
pub(crate) struct DomainScope {
    telescope: SharedTelescope,
    spine: SharedSpine,
}

/// How a metavariable is re-expressed in a context narrower than its own, that of the metavariable whose candidate embeds it ([`Context::metavar_restriction`]) or the one a block leaves standing where it ends ([`Context::block_restriction`]): the stand-in's telescope, drawn from the narrower context's, the arguments it is applied to in the restricted one's birth names, its type over its own binders, and the refinements it is born under. Opaque outside this module — what plans one can only pass it to [`Context::restrict_metavar`].
pub(crate) struct Restriction {
    telescope: SharedTelescope,
    arguments: Vec<Term>,
    result: Term,
    refinements: SharedRefinements,
}

pub(crate) struct UniverseMutation<'a> {
    solver: &'a mut UniverseSolver,
    stamp: &'a Entropy,
    before: UniverseStateToken,
}

impl Deref for UniverseMutation<'_> {
    type Target = UniverseSolver;

    fn deref(&self) -> &Self::Target {
        self.solver
    }
}

impl DerefMut for UniverseMutation<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.solver
    }
}

impl Drop for UniverseMutation<'_> {
    fn drop(&mut self) {
        if self.solver.state_token() != self.before {
            self.stamp.fresh();
        }
    }
}

/// A transaction watermark spanning both unification stores.
#[derive(Debug, Clone, Copy)]
pub(crate) struct SolutionMark {
    term_solution_log_len: usize,
    /// How many times a metavariable's place among those links hold together had been written ([`Solutions::held_len`]).
    held_len: usize,
    universe: UniverseMark,
    credited_len: usize,
    /// How many settled terms the totality obligations had been handed ([`Context::record_checked`]).
    checked_len: usize,
    /// How many questions reduction had put to conversion ([`Context::note_question`]).
    questions: u64,
}

/// The elaborator's ambient state, threaded mutably through elaboration, typing, reduction, conversion, and erasure, in two halves: what any judgment reads and the machine it runs on (`Typing`), and what exists because a unit is being elaborated (`Underway`), which a context that only reads finished terms holds empty. Two lifetimes coexist across them: the *frame-scoped* lexical state (`Frames`), pushed and popped as binders and match arms are entered, and the *flat monotonic facts* about the program (`Solutions`, `Program`), which frames never touch. The `Caches` police both with their write stamps, and this façade is where the stores coordinate: any method that writes a store *and* must stamp or clear a cache lives here, naming both sub-stores explicitly. Reduction is bounded by a step budget restored at every declaration boundary — see [`Context::new`].
#[derive(Debug)]
pub struct Context<'s> {
    // What any judgment reads, and the machine it runs on; see [`Typing`].
    typing: Typing<'s>,
    // What exists because a unit is being elaborated; see [`Underway`].
    underway: Underway,
}

/// What an auto-lift site names as written: the action, and the region's monad — its type without the value slot, as a report shows a monad. A missing embedding is reported through these rather than through the monads unification solved, which are reduced and so spell a type alias by its representation: `Of(Str, (input, offset) => Boundary(input, offset))` where the program wrote `Parse(Str, …)`. The action's monad is read off its head's declaration only when a report asks, since reading it opens the declaration's arrows.
#[derive(Debug, Clone)]
pub(crate) struct EmbeddingSite {
    pub(crate) action: Term,
    pub(crate) region: Term,
}

impl<'s> Context<'s> {
    /// A fresh, empty context at [`DEFAULT_STEP_BUDGET`] — what every caller that is not threading a user-supplied budget wants.
    pub fn with_default_budget(syntax: SyntaxRegistry) -> Self {
        Self::new(DEFAULT_STEP_BUDGET, syntax)
    }

    /// A fresh, empty context in which each declaration may spend `budget` reduction steps, synthesizing the names `syntax` registers. Declarations, definitions, and the metavariable floor arrive later, seeded by `elaborate_module_suffix` as it walks the lowered module.
    ///
    /// The budget is *per declaration*, not per compilation: `elaborate_module_suffix` calls `Context::restore_budget` at every item boundary. A cumulative budget would make whether one declaration typechecks depend on how much the declarations before it had already spent, which is not a property of the declaration. Counting steps rather than elapsed time is what makes the answer a fact about the program instead of about the machine that ran it, so acceptance is reproducible across hosts, loads, and runs.
    ///
    /// The registry is a constructor argument rather than something a later call may or may not install, so an embedding cannot reach a type-directed feature with no vocabulary to emit. Whether the named declarations are actually *in scope* is a separate question the features already answer for themselves — a missing concept registration reports `no witness`, and row subsumption declines and lets the original mismatch speak.
    pub fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            typing: Typing::new(budget, syntax),
            underway: Underway::default(),
        }
    }

    /// Read through to `scope` wherever this context's own stores hold nothing under a name: the units already judged, which a unit is judged against and nothing of which is brought in (`InScope`).
    pub(crate) fn read_through(&mut self, scope: &'s dyn InScope) {
        self.typing.frames.read_through(scope);
        self.typing.program.read_through(scope);
    }

    /// [`Attempt::note_solve_blockers`].
    pub(crate) fn note_solve_blockers(
        &mut self,
        id: MetavarId,
        blockers: Vec<(MetavarId, MetavarOrigin, Option<Extent>)>,
    ) {
        self.underway.attempt.note_solve_blockers(id, blockers);
    }

    /// [`Attempt::solve_blockers`].
    pub(crate) fn solve_blockers(
        &self,
        id: MetavarId,
    ) -> &[(MetavarId, MetavarOrigin, Option<Extent>)] {
        self.underway.attempt.solve_blockers(id)
    }

    /// [`Attempt::note_goal_extent`].
    pub(crate) fn note_goal_extent(&mut self, id: MetavarId, extent: Extent) {
        self.underway.attempt.note_goal_extent(id, extent);
    }

    /// [`Attempt::goal_extent`].
    pub(crate) fn goal_extent(&self, id: MetavarId) -> Option<&Extent> {
        self.underway.attempt.goal_extent(id)
    }

    /// Record what the auto-lift at `extent` named as written.
    pub(crate) fn note_embedding_site(&mut self, extent: Extent, site: EmbeddingSite) {
        self.underway.embedding_sites.push((extent, site));
    }

    /// What the auto-lift at `extent` named as written, where it recorded it — the latest record, since re-validation may elaborate a site again.
    pub(crate) fn embedding_site(&self, extent: &Extent) -> Option<&EmbeddingSite> {
        self.underway
            .embedding_sites
            .iter()
            .rev()
            .find(|(at, _)| at == extent)
            .map(|(_, site)| site)
    }

    /// [`Attempt::note_goal_obligation`].
    pub(crate) fn note_goal_obligation(
        &mut self,
        goals: BTreeSet<MetavarId>,
        this: Term,
        that: Term,
    ) {
        self.underway
            .attempt
            .note_goal_obligation(goals, this, that);
    }

    /// [`Attempt::goal_obligations`].
    pub(crate) fn goal_obligations(&self) -> &[GoalObligation] {
        self.underway.attempt.goal_obligations()
    }

    /// The names this elaboration may synthesize.
    pub fn syntax(&self) -> SyntaxRegistry {
        self.typing.syntax
    }

    /// Install what the unit's `use` declarations brought into scope — each binding's canonical name with the spelling it resolves under, and per definition the ones in scope where it was written — so a goal report may suggest an imported definition the program has not mentioned yet, spelled the way the author would paste it where the goal is.
    pub fn set_imports(&mut self, imports: Imports) {
        self.underway.imports = imports;
    }

    /// The table [`Context::set_imports`] installed.
    pub(crate) fn imports(&self) -> &Imports {
        &self.underway.imports
    }

    /// Record one settled term and the type it settled at — obligation (V)'s seed, collected where elaboration already knows the answer.
    ///
    /// Sort-hood is *not* decided here. The type may still carry unsolved metavariables, and deciding would both reduce on the hot path and risk an answer a later solution invalidates; the gate classifies post-zonk, memoized per distinct type.
    pub(crate) fn record_checked(&mut self, term: &Term, type_: &Term) {
        self.underway.checked.record(term, type_);
    }

    /// Name the definition whose body is elaborating, for (V)'s diagnostics. Returns the previous site so the caller can restore it.
    pub(crate) fn set_checked_site(&mut self, site: Global) -> Option<Global> {
        self.underway.checked.set_site(Some(site))
    }

    /// Restore a site saved by [`Context::set_checked_site`].
    pub(crate) fn restore_checked_site(&mut self, site: Option<Global>) {
        self.underway.checked.set_site(site);
    }

    /// The site being elaborated, as [`Context::set_checked_site`] last set it: `None` is the entry.
    pub(crate) fn checked_site(&self) -> Option<Global> {
        self.underway.checked.site()
    }

    /// Read the recorded terms without consuming them — obligation (T) reads them first, and (V) takes them afterwards.
    pub(crate) fn checked(&self) -> &[(Term, Term, Option<Global>)] {
        self.underway.checked.terms()
    }

    /// How many terms are recorded — where an item begins, so [`Context::truncate_checked`] can forget what a refused item recorded.
    pub(crate) fn checked_mark(&self) -> usize {
        self.underway.checked.mark()
    }

    /// Forget every term recorded since `mark`: a refused item's, which no obligation may read.
    pub(crate) fn truncate_checked(&mut self, mark: usize) {
        self.underway.checked.truncate(mark);
    }

    /// Begin a unit's items: where each of its names is written, and the names its own items declare, none finished yet. No lemma of the unit is on offer until its walk says so ([`Context::offer_lemmas`]).
    pub(crate) fn begin_unit(
        &mut self,
        order: BTreeMap<Global, usize>,
        declared: BTreeSet<Global>,
    ) {
        self.underway.order = order;
        self.underway.needs.begin(declared);
        self.underway.offering = false;
    }

    /// Offer the lemmas of the unit being elaborated to the proofs the elaborator writes from here on: its walk has elaborated the prover's vocabulary, with every declaration it reads.
    pub(crate) fn offer_lemmas(&mut self) {
        self.underway.offering = true;
    }

    /// Whether the unit's lemmas are on offer: what a trace's question about them is asked again by.
    pub(crate) fn offers_lemmas(&self) -> bool {
        self.underway.offering
    }

    /// Begin an attempt at the declaration declaring `names`, with nothing needed yet and in a state of its own ([`Context::begin_state`]).
    pub(crate) fn attempt(&mut self, names: &[&Global]) {
        self.underway.needs.attempt(names);
        self.typing.traced.begin(names);
        self.begin_state();
    }

    /// What the attempt under way asked of the declarations around it, taken: its trace, which is its declaration's where the attempt stands.
    pub(crate) fn take_trace(&mut self) -> Trace<Question, Answered> {
        self.typing.traced.take()
    }

    /// Note that the judgment under way asked `question` of the declaration `name` names, where `name` is a declaration's and not a binder's: the part asked is the declaration's to hold.
    fn asks(&self, name: &Free, question: fn(Global) -> Question) {
        if let Free::Global(global) = name {
            self.typing.traced.note(question(*global), Answered::Part);
        }
    }

    /// Replace what one declaration's elaboration mints and solves with a state no other declaration wrote: its binder identities, its metavariables and their solutions, and its universe levels with the solver that settles them, each counting from what the declaration's own lowering minted ([`Minted`]). What a declaration elaborates to then follows nothing of the declarations elaborated before it — neither how many there were nor what they left solved — and the identities it mints are the ones it would mint elaborated alone. Every other declaration is read as it was published ([`Context::publish`]), which holds none of these.
    fn begin_state(&mut self) {
        let minted = Rc::clone(&self.underway.minted);
        // The items of one declaration were lowered in one space, so any name the attempt declares says which; an attempt that declares none is the entry's.
        let mints = match self.underway.needs.attempted() {
            Some(name) => minted.of(name),
            None => &minted.entry,
        };
        self.exchange(Attempted::above(mints));
    }

    /// The refusal of `term`, a variable nothing binds, with the reason the lowering filed for it where it is a name the lowering could not resolve. Read off what the declaration under way minted, as its state is ([`Context::begin_state`]): a binder's identity is that declaration's, and so is what was filed under it.
    pub(crate) fn unbound_variable(&self, term: Term) -> Error {
        let mints = match self.underway.needs.attempted() {
            Some(name) => self.underway.minted.of(name),
            None => &self.underway.minted.entry,
        };
        let why = match &*term {
            Subterm::Var(var) => var.as_free().and_then(|free| mints.unbound.get(free)),
            _ => None,
        };

        Error::unbound_variable(term, why)
    }

    /// Elaborate in `state` from here on, and hand back the state the context elaborated in until now.
    pub(crate) fn exchange(&mut self, state: Attempted) -> Attempted {
        let replaced = Attempted {
            fresh_names: mem::replace(&mut self.typing.fresh_names, state.fresh_names),
            attempt: mem::replace(&mut self.underway.attempt, state.attempt),
        };
        // Nothing memoized under the state replaced answers for this one.
        self.typing.caches.begin_declaration();
        self.typing.caches.note_write();
        self.typing.caches.note_universe_write();

        replaced
    }

    /// Take the state the attempt that just stood elaborated in, leaving one that has minted nothing: a declaration that wrote goals keeps its own until they are reported.
    pub(crate) fn set_aside(&mut self) -> Attempted {
        self.exchange(Attempted::above(&Mints::default()))
    }

    /// Run `run` in `state`, the one a finished declaration elaborated in, then put the context's own back.
    pub(crate) fn within<T>(&mut self, state: Attempted, run: impl FnOnce(&mut Self) -> T) -> T {
        let own = self.exchange(state);
        let ran = run(self);
        self.exchange(own);

        ran
    }

    /// Where `name` is written among the unit's items, where a declaration of the unit declares it.
    pub(crate) fn written_at(&self, name: &Global) -> Option<usize> {
        self.underway.order.get(name).copied()
    }

    /// Whether the declaration being elaborated wrote a goal ([`Attempt::holds_goals`]).
    pub(crate) fn holds_goals(&self) -> bool {
        self.underway.attempt.holds_goals()
    }

    /// Publish an item that holds a written goal by its types alone: each name stays assumed at its type, zonked, and is bound to nothing. What the item elaborated to holds the metavariables of the state its declaration elaborated in, which no other declaration reads, so what reads this one reads a constant of its type and what would unfold it stays stuck where the goal did. Refused where a type holds a goal itself.
    pub(crate) fn publish_held(&mut self, item: &Item) -> Result<(), Error> {
        let types = match item {
            Item::Let(definition) => vec![(
                definition.name,
                zonk(self, &definition.type_)?,
                definition.universe_context.clone(),
            )],
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .enumerate()
                .map(|(index, definition)| {
                    Ok((
                        definition.name,
                        zonk(self, &rec.group.member_type(index))?,
                        rec.group.universe_context().clone(),
                    ))
                })
                .collect::<Result<_, Error>>()?,
        };
        for (name, type_, universe_context) in types {
            let name = Free::from(&name);
            self.forget(&name);
            self.assume(&name, &type_);
            self.set_assumption_universe_context(&name, universe_context);
        }

        Ok(())
    }

    /// Whether the attempt under way has read nothing unfinished so far.
    pub(crate) fn needs_nothing(&self) -> bool {
        self.underway.needs.nothing()
    }

    /// Publish a finished item: bind each of its names to what it elaborated to and file the registry entries it declares, each zonked ([`zonk_published`](crate::zonk_published)). What another declaration reads of this one then holds no metavariable of the state it was elaborated in, which the next declaration replaces, and is what a recompile that keeps the item reads of its baseline.
    pub(crate) fn publish(&mut self, published: &Published) {
        for (name, declaration) in &published.induct_decls {
            self.update_induct(name, declaration.clone());
        }
        for (name, declaration) in &published.struct_decls {
            self.update_struct(name, declaration.clone());
        }
        for (name, concept) in &published.concepts {
            self.update_concept(name, concept.clone());
        }
        match &published.item {
            Item::Let(definition) => {
                let name = Free::from(&definition.name);
                self.reassume(&name, &definition.type_);
                self.define_published(&name, &definition.body, &definition.kind, definition.proof);
                self.set_assumption_universe_context(&name, definition.universe_context.clone());
                self.record_definition_variances(definition);
                if self.is_witness_declaration(&definition.name) {
                    self.update_witness_scheme(
                        &definition.name,
                        definition.universe_context.clone(),
                        definition.type_.clone(),
                    );
                }
            }
            Item::Rec(rec) => {
                for (index, definition) in rec.definitions.iter().enumerate() {
                    let name = Free::from(&definition.name);
                    let type_ = rec.group.member_type(index);
                    self.reassume(&name, &type_);
                    self.set_assumption_universe_context(
                        &name,
                        rec.group.universe_context().clone(),
                    );
                    self.define_published(
                        &name,
                        &Term::rec_proj(rec.group.clone(), index),
                        &definition.kind,
                        definition.proof,
                    );
                    if self.is_witness_declaration(&definition.name) {
                        self.update_witness_scheme(
                            &definition.name,
                            rec.group.universe_context().clone(),
                            type_,
                        );
                    }
                }
            }
        }
    }

    /// Zonk every term recorded since `mark`, the finished item's, so the erasure obligations read them once the state they were recorded in is gone.
    pub(crate) fn settle_checked(&mut self, mark: usize) -> Result<(), Error> {
        let mut zonked = HashMap::<Term, Term>::new();
        let mut settled = Vec::with_capacity(self.underway.checked.mark() - mark);
        for (term, type_, site) in &self.underway.checked.terms()[mark..] {
            let mut settle = |term: &Term| match zonked.get(term) {
                Some(done) => Ok(done.clone()),
                None => {
                    let done = zonk(self, term)?;
                    zonked.insert(term.clone(), done.clone());
                    Ok::<_, Error>(done)
                }
            };
            settled.push((settle(term)?, settle(type_)?, *site));
        }
        self.underway.checked.replace_since(mark, settled);

        Ok(())
    }

    /// The declaration declaring `names` has finished — kept, refused or withheld — and nothing waits on it any more: a witness among them is the table's to answer for, or no one's.
    pub(crate) fn finish(&mut self, names: &[&Global]) {
        for name in names {
            self.underway.witnesses.settle(name);
        }
        self.underway.needs.finish(names);
    }

    /// Record that the attempt under way read the declaration declaring `name` before it finished. The attempt is void whatever it goes on to conclude, and is made again once that declaration has elaborated: the record is what voids it, so no site that turns an error into a decision can make a void attempt count.
    pub(crate) fn need(&self, name: Global) {
        self.underway.needs.need(name);
    }

    /// What the attempt under way needed, taken: empty for an attempt that read nothing unfinished, the one whose outcome stands.
    pub(crate) fn take_needs(&mut self) -> BTreeSet<Global> {
        self.underway.needs.take()
    }

    /// [`Needs::missed`].
    fn missed(&self, name: &Free) {
        self.underway.needs.missed(name);
    }

    /// [`Needs::reads`].
    fn reads_entry(&self, name: &Global) {
        self.underway.needs.reads(name);
    }

    /// Whether a proof the elaborator writes for the declaration being elaborated may apply `name`. A name another unit declares is applied wherever it is in scope. A name this unit declares is one of the prover's vocabulary, which the unit's walk elaborates before anything else, with every declaration it reads, and offers only from then on: so the vocabulary and what it reads are proved without it, every other declaration finds all of it elaborated, and what a declaration's proof is built from follows what the vocabulary reads, never where either is written or which of them was asked for first. A lemma that is on offer and not in scope was refused, and is applied by nothing.
    ///
    /// **A lemma of the unit is read only where the unit's lemmas are on offer.** Before that the answer is no whatever the lemma is, so what is noted is that none was on offer and nothing of the lemma: a trace asked again then waits for no lemma its elaboration did not read.
    pub(crate) fn proof_may_apply(&self, name: &Global) -> bool {
        if self.underway.order.contains_key(name) {
            self.typing
                .traced
                .note(Question::Lemmas, Answered::Whether(self.underway.offering));
            if !self.underway.offering {
                return false;
            }
        }
        self.asks(&Free::Global(*name), Question::Signature);

        self.typing
            .frames
            .assumption(&Free::Global(*name))
            .is_some()
    }

    /// Name the declarations a lowering could not read. Their dependents are withheld from elaboration before the first item is checked, so a parse failure in one declaration reports as that declaration's and nothing else's.
    pub fn set_broken(&mut self, names: BTreeSet<Global>) {
        self.underway.broken = names;
    }

    pub(crate) fn take_broken(&mut self) -> BTreeSet<Global> {
        mem::take(&mut self.underway.broken)
    }

    pub(crate) fn record_definition_totality(&mut self, name: &Global, totality: Totality) {
        self.typing
            .program
            .record_definition_totality(name, totality);
    }

    pub(crate) fn definition_totality(&self, name: &Global) -> Option<Totality> {
        self.asks(&Free::Global(*name), Question::Totality);
        self.typing.program.definition_totality(name)
    }

    /// File the vector a finished definition carries, for conversion's level alignment: with the length of its type's outer telescope, which is the one spine that applies it in full.
    pub(crate) fn record_definition_variances(&mut self, definition: &Definition) {
        let arity = match &*definition.type_ {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.len(),
            _ => 0,
        };
        self.typing.program.record_definition_variances(
            &definition.name,
            &definition.variances,
            arity,
        );
    }

    pub(crate) fn definition_variances(&self, name: &Global) -> Option<(&[Variance], usize)> {
        self.asks(&Free::Global(*name), Question::Variances);
        self.typing.program.definition_variances(name)
    }

    /// Take the terms recorded since `mark`: a published declaration's, or the entry's, which (V) reads last.
    pub(crate) fn split_checked(&mut self, mark: usize) -> Vec<(Term, Term, Option<Global>)> {
        self.underway.checked.split_off(mark)
    }

    /// Mint a binder nothing else can name, rendering as `hint`.
    ///
    /// The counter starts above every index the declaration's lowering minted ([`Context::seed`]), so a lowered binder and an elaborated one can never be the same identity.
    pub(crate) fn fresh(&mut self, hint: Option<&str>) -> Free {
        self.fresh_for(hint, None)
    }

    /// [`Context::fresh`] for a local opened from a scope's binder, recording where the lowering wrote that binder among its declaration's written binders: so a proof that reads the local credits the binder a lint names ([`Context::credit`]).
    pub(crate) fn fresh_for(&mut self, hint: Option<&str>, written: Option<u32>) -> Free {
        let index = u32::try_from(self.typing.fresh_names.fresh()).expect("binder space exhausted");

        let local = Free::local(index, hint);
        if let Some(written) = written {
            self.underway
                .credits
                .open(local, self.underway.declaration, written);
        }
        local
    }

    /// Elaborate the item `declaration` from here on — `None` for an entry's final term — forgetting where the previous item's locals were opened: a proof is written only while its bound's item elaborates. Its scheme is open until its universe levels are finalized.
    pub(crate) fn enter_item(&mut self, declaration: Option<Global>) {
        self.underway.credits.begin_item();
        self.underway.scheme_open = true;
        self.enter_declaration(declaration);
    }

    /// Elaborate `declaration`, a member of the item being elaborated, from here on: whose written binders the locals opened next count among.
    pub(crate) fn enter_declaration(&mut self, declaration: Option<Global>) {
        self.underway.declaration = declaration;
    }

    /// [`Credits::credit`].
    pub(crate) fn credit(&mut self, proof: &Term) {
        self.underway.credits.credit(proof);
    }

    /// The written binders proofs the elaborator wrote read, each by its declaration and its place among that declaration's written binders — each used, though no written reference reaches it.
    pub fn credited(&self) -> BTreeSet<(Option<Global>, u32)> {
        self.underway.credits.credited()
    }

    /// Take what a unit's lowering minted, declaration by declaration. Each attempt at a declaration begins its state above what that declaration's lowering minted, and the entry's above the entry's ([`Context::begin_state`]), so no identity either mints is one the other already holds.
    ///
    /// `into_core` mints the binders of every lowered scope, a metavariable for every hole and a level for every written type, and elaboration mints more of each; both draw from one identity space per declaration, so the second source starts above the first. Nothing from another declaration or another unit is in that space: no term the scope replays or the context publishes carries a local or a metavariable.
    pub(crate) fn seed(&mut self, minted: &Minted) {
        self.underway.minted = Rc::new(minted.clone());
    }

    /// [`Spend::spend`].
    pub(crate) fn spend(&self, cost: Cost) -> Result<(), ReduceError> {
        self.typing.spend.spend(cost)
    }

    /// [`Spend::enter_level`].
    pub(crate) fn enter_level(&self) -> Result<(), ReduceError> {
        self.typing.spend.enter_level()
    }

    /// [`Spend::leave_level`].
    pub(crate) fn leave_level(&self) {
        self.typing.spend.leave_level();
    }

    /// Restore the full budget for a new declaration ([`Spend::restore`]), and forget what was memoized under the one it closes: nothing a budget paid for outlives it.
    pub(crate) fn restore_budget(&mut self) {
        self.typing.spend.restore();
        self.typing.caches.begin_declaration();
        self.typing.frames.forget_spellings();
    }

    /// What the declaration being elaborated has consumed so far ([`Spend::consumed`]), which this crate's measurements read between two judgments.
    #[cfg(test)]
    pub(crate) fn consumed(&self) -> Consumption {
        self.typing.spend.consumed()
    }

    /// The heaviest single judgment this context has made, the one in progress included: a declaration's elaboration, or one of the judgments of its final terms, each of which spends a budget restored for it (`Context::restore_budget`).
    ///
    /// The counterpart of `curios-cert`'s `Kernel::heaviest_declaration` on the other side of the seam — the two are deliberately the same shape, because comparing them is the point. An observation for a measurement; nothing in elaboration reads it.
    pub fn heaviest_declaration(&self) -> Consumption {
        self.typing.spend.heaviest()
    }

    /// The closed machine's memo for the reduction in progress.
    pub(crate) fn closed_memo_mut(&mut self) -> &mut ClosedMemo {
        let plain = self.typing.asking.plain();

        self.typing.caches.closed_memo(plain)
    }

    /// The read half of the reduction cache. The reducer probes it wherever a term's reduction begins — at entry, and at the scrutinee stack's frame push, where a warm scrutinee dispatches in place instead of framing.
    ///
    /// **A universe metavariable in the term does not exclude it.** Reduction is parametric in levels — no rule reads one, `Type u` being a payload and not a scrutinee, and an instantiation substitutes a parameter with whatever level stands in the instance, a metavariable included — so a reduct is the same function of its term whether the metas in it are solved or not, and the in-place rewrites that do change what a level *spells* (defaulting, finalization, instance closure) clear the cache where they happen. Excluding one would remember nothing of a web of universe-polymorphic definitions inside the declaration that instantiates them, whose occurrences all carry that declaration's level metas, and a reduct shared as a graph would be re-derived once per occurrence — the `2^n` `curios`' `scrutinee_refinement_measurements` guards under `numeric, proved`.
    pub(crate) fn cached_reduced(&mut self, term: &Term) -> Option<Term> {
        self.typing
            .caches
            .reduction_get(term, self.typing.asking.plain())
    }

    /// Record that `term` reduces to `result` — the write half of the reduction cache, hit wherever a reduction's value lands: the reducer's final return, and its scrutinee stack's frame pop. Memoize only closed terms whose WHNF names no *unsolved* term metavariable — `any_metavar` bails on the first one, never building the id set. A solve is monotonic, so it can only invalidate a reduct that still names the metavariable it solved, and reduction gets stuck on (hence surfaces) an unsolved metavariable it actually depends on. Refusing to cache those is what lets `solve_metavar` skip a cache clear; an entry naming only *solved* metavariables stays valid under forward solves (re-validation's `rollback_solutions`, which *un*-solves, clears separately). A *universe* metavariable excludes nothing, for the reason [`Context::cached_reduced`] gives.
    ///
    /// **Stored for nothing.** The entry dies with the declaration — [`Caches::begin_declaration`] clears the table where the budget is restored — so the work budget that built it is its bound.
    pub(crate) fn reduce(&mut self, term: Term, result: &Term) {
        let cacheable =
            term.closed() && !result.any_metavar(&mut |id| self.metavar_solution(id).is_none());

        if cacheable {
            self.typing
                .caches
                .reduction_insert(term, result.clone(), self.typing.asking.plain());
        }
    }

    /// The remembered sort of `type_`, where the context stands as it did when it was filed. [`Caches::sorts`] carries how long that is.
    pub(crate) fn cached_sort(&mut self, type_: &Term) -> Option<Sort> {
        self.typing
            .caches
            .sort_get(type_, self.typing.asking.plain())
    }

    /// Remember `type_`'s sort. Stored for nothing, as a reduct is.
    pub(crate) fn record_sort(&mut self, type_: Term, sort: Sort) {
        self.typing
            .caches
            .sort_insert(type_, self.typing.asking.plain(), sort);
    }

    /// [`Context::kernel_spelling`] of a recorded scrutinee, remembered where no metavariable in it is left unsolved ([`Caches::kernel_spellings`]).
    pub(crate) fn kernel_spelled(&self, term: &Term) -> Term {
        if let Some(known) = self.typing.caches.kernel_spelling_get(term) {
            return known;
        }
        let spelled = self.kernel_spelling(term);
        if !spelled.any_metavar(&mut |id| self.metavar_solution(id).is_none()) {
            self.typing
                .caches
                .kernel_spelling_insert(term.clone(), spelled.clone());
        }
        spelled
    }

    /// Whether the binder `name` is a proof, where that has been asked and remembered ([`Context::remember_proof`]).
    pub(crate) fn cached_proof(&self, name: &Free) -> Option<bool> {
        self.typing.caches.proof_get(name)
    }

    /// Remember whether the binder `name` is a proof, for the declaration.
    pub(crate) fn remember_proof(&mut self, name: Free, proof: bool) {
        self.typing.caches.proof_insert(name, proof);
    }

    /// The settled reduced spelling of the scrutinee entry `key` registered in `frame`: `None` if no probe has asked for it yet, `Some(None)` if reducing it refused. [`Frames::scrutinee_spellings`] carries how long one stands.
    pub(crate) fn settled_key(&self, frame: usize, key: &Term) -> Option<&Option<Settled>> {
        self.typing.frames.settled_spelling(
            frame,
            key,
            self.typing.asking.plain(),
            self.underway.attempt.solutions.solved_len(),
            &self.underway.attempt.universe_solver.state_token(),
        )
    }

    /// Record a settlement. Every one is recorded, whatever its metavariables: the loop that asks the reduced spellings settles the innermost entry *not yet asked* (`reduce`'s `refined_reduct`), so an unrecorded settlement would be asked for again forever.
    ///
    /// `unsolved` says the key or its spelling held an unsolved metavariable, so the settlement is filed beside the solutions committed so far and asked for again once another lands. `levels` is the universe solver's state where a question was declined for a commit while it settled: it is then asked for again once either solver moves.
    pub(crate) fn record_settled_key(
        &mut self,
        frame: usize,
        key: Term,
        settled: Option<Settled>,
        unsolved: bool,
        levels: Option<UniverseStateToken>,
    ) {
        let solved =
            (unsolved || levels.is_some()).then(|| self.underway.attempt.solutions.solved_len());
        self.typing.frames.settle_spelling(
            frame,
            key,
            self.typing.asking.plain(),
            settled,
            solved,
            levels,
        );
    }

    /// Run `attempt` with at most `allowance` units of this declaration's budget in reach, answering `None` when it did not finish inside that.
    ///
    /// **For work whose result is optional and whose cost must not be a program's cost.** Canonicalizing a refinement key is the case: settling it collapses two spellings of one comparison, failing to settle it leaves the two uncollapsed, and neither outcome changes what the program means. A guard over an opaque parameter settles in a handful of steps; one over a subject built by a hundred thousand iterations does not settle at all, and without a ceiling that single attempt spends the whole declaration.
    ///
    /// **What is spent is spent.** A bail is charged the allowance rather than refunded, so this is a cap and not free work — the invariant `Context::spend` states holds through it. What bounds the total is the memo the one caller keeps: an attempt happens once per key, so a declaration pays at most its guard count times this ceiling.
    pub(crate) fn within_allowance<T>(
        &mut self,
        allowance: u64,
        attempt: impl FnOnce(&mut Self) -> Result<T, ReduceError>,
    ) -> Result<Option<T>, ReduceError> {
        let capped = self.typing.spend.cap(allowance);

        let outcome = attempt(self);
        self.typing.spend.lift(&capped);

        // An attempt the declaration's own remainder bound is a probe like any other: its exhaustion is the declaration's verdict, and absorbing it would hand the caller a declined attempt where the truth is that nothing is left to spend, the caller going ahead on that answer. Only an attempt the cap bound below the remainder is the allowance's to decline, its exhaustion with every other failure, since the declaration still holds what the cap withheld.
        match capped.bound_by_the_remainder() {
            true => outcome.probed(),
            false => Ok(outcome.ok()),
        }
    }

    /// The elaboration-level counterpart of the reduction cache ([`Context::cached_reduced`] / [`Context::reduce`]): memoize `(term, expected) → (rebuilt, type)` for subterms whose elaboration can neither read nor write anything context-dependent. Eligibility is O(1) per call (the bits are cached per `Term` node): the term — and the expected type, when checking — must contain no metavariable, no unsolved universe metavariable and no local ([`Term::has_local_free`](curios_core::Term::has_local_free)), so every free name it holds is a global. Writes are detected by snapshotting `mutation_stamp` around the computation: an entry is inserted only when the run minted, solved, parked, defined, and refined nothing — a pure run whose replay would be the identity on the context. Errors are never cached. The one deliberate delta on a hit: the skipped run's `expect` does not drain `retry_parked` at that exact point — safe deferral, since retries re-run at every later `expect` and the module drain reports whatever survives.
    ///
    /// A cached entry additionally names only *already-defined* globals: the insert refuses any result — or `Check` expected — naming a not-yet-defined global (`Context::elaboration_cacheable`), the name analogue of the unsolved-metavariable refusal above. Definedness is the one ambient fact a pure, ground elaboration reads (through `expect`'s conversions, which unfold definitions), so an entry that surfaces only settled globals cannot be invalidated by a later *fresh* `define`. That is what lets `define_entry` keep the memo warm, with no wholesale clear, across the definitions that reduction and the frame elaborators mint within one item. (`restore_budget` still clears at each declaration boundary, and `set_island` at each change of item, so the survival is within-item.)
    ///
    /// The suppression brackets need no insert refusal. Privacy: validity is directional (an entry that passed an island's strict checks is valid under suppression, but not the reverse), so `island.is_some()` is part of the key — a re-validation under the suppression bracket populates and hits its own partition, and `set_island` clears on every item change. Parking: `expect` can only be `Blocked` on unsolved metavariables, which the groundness gate excludes, so suppression is inert for every cacheable run. Refinements: the registrar, frame-exit, and suppression-boundary clears already remove every entry a live refinement could have influenced, on both sides of the flag.
    ///
    /// Whether reduction is plain is part of the key too ([`Context::plainly`]). A question reduction puts to conversion writes nothing, so a run that rests on one is as pure as one that asked nothing, and plain reduction, which asks nothing, would refuse it.
    ///
    /// Without this cache, elaboration tree-walks DAG-shaped lowered terms: a string literal's UTF-8 derivation shares every scan-state chain by `Rc`, so re-elaborating the chain at each link would cost O(N²) work and the chain's depth in native stack; with it, each shared node elaborates once, at O(1) additional depth.
    ///
    /// Probe, compute under the stamp snapshot, record — the one way in, used by every `elaborate_subterm` dispatch. The halves are split out as [`Context::probe_elaborated`] and [`Context::record_elaborated`] because this method is exactly the two of them with the `compute` call spliced in; nothing else calls them.
    pub(crate) fn get_or_init_elaborated<E>(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        compute: impl FnOnce(&mut Self) -> Result<(Term, Term), E>,
    ) -> Result<(Term, Term), E> {
        match self.probe_elaborated(term, expected) {
            ElabProbe::Hit(hit) => Ok(hit),
            // A miss as much as a term the gate refuses: a miss is recorded only when its run wrote nothing, and a run over this declaration's levels writes, so a miss is what the oracle's re-runs were.
            probe if self.oracle_memoizable(term, expected) => {
                let privacy_checked = self.underway.posture.island().is_some();
                let plain = self.typing.asking.plain();
                if let Some(hit) =
                    self.typing
                        .caches
                        .oracle_get(term, expected, privacy_checked, plain)
                {
                    return Ok(hit);
                }
                let result = compute(self)?;
                if let ElabProbe::Miss(stamp) = probe {
                    self.record_elaborated(term, expected, stamp, &result);
                }
                self.typing
                    .caches
                    .oracle_insert(term, expected, privacy_checked, plain, &result);
                Ok(result)
            }
            ElabProbe::Uncacheable => compute(self),
            ElabProbe::Miss(stamp) => {
                let result = compute(self)?;
                self.record_elaborated(term, expected, stamp, &result);
                Ok(result)
            }
        }
    }

    /// Whether an elaboration the cache refuses may be remembered for the live oracle bracket: the term, and the expected type when checking, name no metavariable and no local — only the universe metavariables and the impurity the cache's gate refuses are admitted.
    ///
    /// **An oracle's answer is its verdict.** Its three callers — `convert`'s re-validation of a candidate, `suggest`'s check of one, a test — read `Ok` or `Err` and discard the elaborated term, so what a hit must preserve is the verdict alone. Within one bracket the context only accumulates — every rollback clears the table ([`Caches::invalidate_for_rollback`]), and a nested bracket keeps a table of its own — and a local-free, metavariable-free term reads nothing a later point in the bracket could have changed; so elaborating it again against the same expectation would repeat the first run's writes — mint its own fresh metavariables, solve them alike, add level constraints already present — and reach the same verdict, which is what skipping it hands back. The cache's purity gate exists because its entries outlive the run that made them; these do not outlive the bracket.
    ///
    /// **What it is for.** A candidate is a reduct, and a reduct is a graph whose tree can be exponential in its depth — a text position built a character at a time mentions the one before it four times. Elaborating one writes — the level constraints its instances raise, the metavariables its implicit arguments mint — so the cache keeps almost none of it, and without this table re-validation checks the tree: re-validating a three-character `Str/trim` claim stated in a type asks for some hundred thousand elaborations, most of them misses the cache declines to record, and runs out of steps.
    fn oracle_memoizable(&self, term: &Term, expected: Option<&Term>) -> bool {
        let closed = |t: &Term| !t.has_metavar() && !t.has_local_free();
        self.typing.caches.in_oracle() && closed(term) && expected.is_none_or(closed)
    }

    /// Read half of the elaboration cache (see [`Context::get_or_init_elaborated`] for the full contract). Applies the O(1) groundness gate, then either answers from the cache (`Hit`), reports the term ineligible (`Uncacheable`), or snapshots `mutation_stamp` for the caller to thread back into [`record_elaborated`](Self::record_elaborated) (`Miss`). Pure: it never mutates the context, so a driver may probe speculatively at a frame push.
    pub(crate) fn probe_elaborated(&self, term: &Term, expected: Option<&Term>) -> ElabProbe {
        let ground = |t: &Term| {
            !t.has_metavar() && !self.has_unsolved_universe_meta(t) && !t.has_local_free()
        };
        if !ground(term) || !expected.is_none_or(ground) {
            return ElabProbe::Uncacheable;
        }

        // Locally-nameless discipline: every scope is opened before descent, so a term in elaboration position carries no loose bound indices — which is what makes it keyable without any binder context.
        assert!(term.closed(), "elaboration-cache key has loose indices");

        match self.typing.caches.elaboration_get(
            term,
            expected,
            self.underway.posture.island().is_some(),
            self.typing.asking.plain(),
        ) {
            Some(hit) => ElabProbe::Hit(hit),
            None => ElabProbe::Miss(self.typing.caches.stamps()),
        }
    }

    /// Write half of the elaboration cache, paired with a [`probe_elaborated`](Self::probe_elaborated) `Miss`. Keys the same way the probe did (extents excluded from `Term` equality, so the un-restamped result the caller passes keys identically) and defers to [`insert_elaborated`](Self::insert_elaborated)'s purity/groundness condition against the snapshotted `stamp`.
    pub(crate) fn record_elaborated(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        stamp: ElaborationStamp,
        result: &(Term, Term),
    ) {
        self.insert_elaborated(term, expected, &stamp, result);
    }

    /// Insert-side tail of [`Context::get_or_init_elaborated`], kept out of the caller's frame deliberately: `elaborate` recurses natively once per term level with `get_or_init_elaborated` on the stack, so the insert path's locals must not ride along on every level.
    #[inline(never)]
    fn insert_elaborated(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        stamp: &ElaborationStamp,
        result: &(Term, Term),
    ) {
        if self.elaboration_cacheable(stamp, expected, result) {
            self.typing.caches.elaboration_insert(
                term,
                expected,
                self.underway.posture.island().is_some(),
                self.typing.asking.plain(),
                result,
            );
        }
    }

    /// Whether a [`probe_elaborated`](Self::probe_elaborated) `Miss` may be recorded: the purity and groundness condition, plus the *settled-globals* gate. Every global the entry names — in the result, and in the `Check` `expected` half of the key — must already be defined. Definedness is the one ambient fact a pure, ground elaboration reads (through the conversions in `expect`, which unfold definitions), so an entry that surfaces only settled globals cannot be invalidated by a later *fresh* `define` — the name analogue of the reduction cache's unsolved-metavariable refusal (`Context::reduce`), and what lets [`define_entry`](Self::define_entry) keep the elaboration cache across a fresh definition. A constructor, intrinsic, inductive, or struct is not a free `Var`, so it never trips the gate; only a `/`-qualified definition or a `rec` member does, and a `rec` member is defined (as a slot) before any sibling body elaborates, so it counts as settled here — the slot→member redefinition later clears wholesale.
    fn elaboration_cacheable(
        &self,
        stamp: &ElaborationStamp,
        expected: Option<&Term>,
        result: &(Term, Term),
    ) -> bool {
        let ground = |t: &Term| {
            !t.has_metavar() && !self.has_unsolved_universe_meta(t) && !t.has_local_free()
        };
        let settled = |t: &Term| t.free_vars().iter().all(|name| self.is_defined(name));
        self.typing.caches.stamps_unchanged(stamp)
            && ground(&result.0)
            && ground(&result.1)
            && settled(&result.0)
            && settled(&result.1)
            && expected.is_none_or(settled)
    }

    fn has_unsolved_universe_meta(&self, term: &Term) -> bool {
        let solver = &self.underway.attempt.universe_solver;

        term.any_universe_meta(|meta| match solver.zonk(&Level::meta(meta)) {
            Ok(level) => level.metas().next().is_some(),
            Err(_) => true,
        }) || term.any_quality_meta(|meta| {
            matches!(solver.quality(Quality::Meta(meta)), Quality::Meta(_))
        })
    }

    fn enter_frame(&mut self) {
        self.typing.frames.enter();
    }

    fn leave_frame(&mut self) {
        let (dropped_refinements, dropped_definitions) = self.typing.frames.leave();
        self.typing
            .caches
            .invalidate_frame_exit(dropped_refinements, dropped_definitions);
    }

    pub(crate) fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enter_frame();
        let result = f(self);
        self.leave_frame();

        result
    }

    /// Say that the arm whose frame is being elaborated is never taken: its case is of type `case` and its scrutinee of type `scrutinee`, one family at indices that clash. The reason stands for that frame and every frame inside it, and an error raised there is reported with it: by the arm where the error propagates ([`Error::in_dead_arm`]), and by the record of an implicit minted there where one nothing solved is reported once the arm has closed.
    pub(crate) fn rule_out_arm(&mut self, case: Term, scrutinee: Term) -> Rc<DeadArm> {
        let dead = Rc::new(DeadArm::Indices { case, scrutinee });
        self.typing.frames.rule_out(Rc::clone(&dead));

        dead
    }

    /// [`Frames::assume`], stamping the write.
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.typing.caches.note_write();
        self.typing.frames.assume(name, type_);
    }

    /// Step `walk` past its next binder: mint one from the entry's hint, assume it at `domain`, and hand it back for the caller's own capture.
    pub(crate) fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        let binder = walk.advance_fresh(|hint| self.fresh(hint));
        self.assume(&binder, domain);
        binder
    }

    /// Assume `label : type_` as a `use`-plicity binder: an ordinary assumption that additionally joins the witness scope, where resolution finds it (innermost-first).
    pub(crate) fn assume_witness(&mut self, name: &Free, type_: &Term) {
        self.assume(name, type_);
        self.typing.frames.push_witness_binder(name, type_);
    }

    /// Enter a telescope's member into scope under its mark: assumed at its type, and, where the mark is `use`, in the witness scope as well, so resolution in what follows finds it.
    ///
    /// **The one way a walk enters a member it opens**, whatever the telescope — a function type's, a lambda's, a declaration's parameters, a constructor's payload in a signature or in an arm, a structure's fields — so what a mark means where a telescope is opened is said here and at no site.
    pub(crate) fn enter(&mut self, name: &Free, type_: &Term, mark: Plicity) {
        match mark {
            Plicity::Witness => self.assume_witness(name, type_),
            Plicity::Explicit | Plicity::Implicit => self.assume(name, type_),
        }
    }

    pub(crate) fn witness_scope(&self) -> &[(Free, Term)] {
        self.typing.frames.witness_scope()
    }

    /// [`Frames::reassume`], invalidating the elaboration cache — an entry elaborated between a `rec` group's lowered `assume` and this upgrade could embed the lowered signature.
    pub(crate) fn reassume(&mut self, name: &Free, type_: &Term) {
        self.typing.caches.invalidate_for_reassumption();
        self.typing.frames.reassume(name, type_);
    }

    /// The type `name` is assumed at. A miss on a name whose declaration has not finished is recorded as what the attempt needs ([`Context::need`]).
    pub(crate) fn assumption(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Signature);
        let assumed = self.typing.frames.assumption(name);
        if assumed.is_none() {
            self.missed(name);
        }

        assumed
    }

    /// [`Frames::rec_definitions`]: every top-level `rec` group defined so far, with its members' names.
    pub(crate) fn rec_definitions(&self) -> Vec<(RecGroup, Vec<Global>)> {
        self.typing.frames.rec_definitions()
    }

    /// [`Frames::member_names`]: the globals `group`'s members are defined under, in member order. A group is its members' bodies, so a judgment told which names one goes by has read the body of each.
    pub(crate) fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>> {
        let names = self.typing.frames.member_names(group);
        for name in names.iter().flatten() {
            self.asks(&Free::Global(*name), Question::Body);
        }

        names
    }

    /// `group` at `universes`, remembered as that instance of it for the declaration in progress and read back by [`Context::instance_of`]. An instance holds its levels substituted through every member, so a copy does not say which instance of which group it is, and a term that names a group's members again ([`named_members`](crate::named_members)) knows one by this record alone. So this is the one way an instance is opened here: by the reducer where it unfolds a name, by the closed machine through its host, by the neutral typer where it reads a member's type, and by the reduction table where it serves a reduct at another spelling's levels ([`Caches::reduction_get`]).
    pub(crate) fn opened_instance(
        &mut self,
        group: &RecGroup,
        universes: &Universes,
    ) -> Result<RecGroup, ReduceError> {
        self.typing
            .caches
            .opened_instance(group, universes)
            .map_err(ReduceError::Universe)
    }

    /// The group `instance` was opened from and the instance it was opened at, where it was opened in the declaration in progress.
    pub(crate) fn instance_of(&self, instance: &RecGroup) -> Option<&(RecGroup, Universes)> {
        self.typing.caches.instance_get(instance)
    }

    /// Collect universe metas reachable through a term and through any solved term metavariables it names. Declaration finalization runs before the final term-zonk pass, so a level occurring only in a solved hole must still join the declaration's universe closure. Recursive slots may point back to themselves; `seen` keeps this analysis finite.
    pub(crate) fn universe_metas_in(&self, term: &Term) -> BTreeSet<UniverseMetaId> {
        let mut universes = BTreeSet::new();
        let mut seen_metas = BTreeSet::new();
        let mut pending = vec![term.clone()];
        while let Some(term) = pending.pop() {
            let mut term_metas = BTreeSet::new();
            term.collect_universe_dependencies(&mut universes, &mut term_metas);
            for meta in term_metas {
                if !seen_metas.insert(meta) {
                    continue;
                }
                if let Some(entry) = self.metavar_entry(meta) {
                    match &entry.solution {
                        Some(solution) => pending.push(solution.clone()),
                        None => {
                            // An unsolved meta may survive into parked work, so keep every universe dependency needed to solve it later. A solved meta materializes only its solution through the occurrence spine; its birth result/telescope do not survive zonking.
                            pending.push(entry.result.clone());
                            pending.extend(entry.telescope.iter().map(|(_, type_)| type_.clone()));
                        }
                    }
                }
            }
        }
        universes
    }

    pub(crate) fn instantiate_assumption_universes(
        &mut self,
        name: &Free,
    ) -> Result<Option<(Term, Universes)>, UniverseError> {
        let Some(type_) = self.assumption(name).cloned() else {
            return Ok(None);
        };
        let universe_context = self
            .typing
            .frames
            .assumption_universe_context(name)
            .unwrap_or_default();
        if universe_context.width() == 0 {
            return Ok(Some((type_, Universes::default())));
        }
        let universes = self.universes_mut().instantiate(&universe_context)?;
        let type_ = instantiate_universes_scoped(&type_, &universes)?;
        Ok(Some((type_, universes)))
    }

    pub(crate) fn instantiate_assumption(
        &mut self,
        name: &Free,
    ) -> Result<Option<(Term, Universes)>, Error> {
        self.instantiate_assumption_universes(name)
            .map_err(Error::from)
    }

    pub(crate) fn instantiate_assumption_at(
        &mut self,
        name: &Free,
        universes: &Universes,
    ) -> Result<Option<Term>, Error> {
        let Some(type_) = self.assumption(name).cloned() else {
            return Ok(None);
        };
        let found = self.typing.frames.assumption_universe_context(name);
        #[cfg(feature = "profile")]
        if found
            .as_ref()
            .is_none_or(|context| context.parameter_count != universes.levels.len())
        {
            let (frames, holders) = self.typing.frames.assumption_universe_holders(name);
            curios_profile::note!(
                target: "curios_elab::universe",
                name = %name.faithful(),
                registered = found.is_some(),
                expected = found.as_ref().map_or(0, |context| context.parameter_count),
                got = universes.levels.len(),
                frames,
                ?holders,
                "assumption instance arity mismatch",
            );
        }
        let universe_context = found.unwrap_or_default();
        self.universes_mut()
            .instantiate_at(&universe_context, universes)
            .map_err(Error::from)?;
        let type_ = instantiate_universes_scoped(&type_, universes).map_err(Error::from)?;
        Ok(Some(type_))
    }

    pub(crate) fn instantiate_universe_bound<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
    ) -> Result<(B, Universes), Error> {
        if universe_context.width() == 0 {
            return Ok((value.clone(), Universes::default()));
        }
        let universes = self
            .universes_mut()
            .instantiate(universe_context)
            .map_err(Error::from)?;
        let value = instantiate_universes_scoped(value, &universes).map_err(Error::from)?;
        Ok((value, universes))
    }

    pub(crate) fn instantiate_universe_bound_at<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
        universes: &Universes,
    ) -> Result<B, UniverseError> {
        #[cfg(feature = "profile")]
        if universes.levels.len() != universe_context.parameter_count {
            curios_profile::note!(
                target: "curios_elab::universe",
                expected = universe_context.parameter_count,
                got = universes.levels.len(),
                "bound instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(universe_context, universes)?;
        instantiate_universes_scoped(value, universes)
    }

    pub(crate) fn instantiate_induct_decl_at(
        &mut self,
        induct_decl: &InductDecl,
        universes: &Universes,
    ) -> Result<InductDecl, UniverseError> {
        #[cfg(feature = "profile")]
        if universes.levels.len() != induct_decl.universe_context.parameter_count {
            curios_profile::note!(
                target: "curios_elab::universe",
                module = ?induct_decl.module,
                expected = induct_decl.universe_context.parameter_count,
                got = universes.levels.len(),
                "induct instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(&induct_decl.universe_context, universes)?;
        let mut instantiated = induct_decl.clone();
        instantiated.arity = instantiate_universes_scoped(&instantiated.arity, universes)?;
        instantiated.result_sort =
            instantiate_universes_scoped(&instantiated.result_sort, universes)?;
        for constructor in instantiated.signatures_mut() {
            constructor.telescope =
                instantiate_universes_scoped(&constructor.telescope, universes)?;
        }
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }

    pub(crate) fn instantiate_struct_decl_at(
        &mut self,
        struct_decl: &StructDecl,
        universes: &Universes,
    ) -> Result<StructDecl, UniverseError> {
        #[cfg(feature = "profile")]
        if universes.levels.len() != struct_decl.universe_context.parameter_count {
            curios_profile::note!(
                target: "curios_elab::universe",
                module = ?struct_decl.module,
                expected = struct_decl.universe_context.parameter_count,
                got = universes.levels.len(),
                "struct instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(&struct_decl.universe_context, universes)?;
        let mut instantiated = struct_decl.clone();
        instantiated.arity = instantiate_universes_scoped(&instantiated.arity, universes)?;
        instantiated.result_sort =
            instantiate_universes_scoped(&instantiated.result_sort, universes)?;
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }

    /// [`Frames::set_assumption_universe_context`], with the redefinition cache protocol — a scheme rewritten in place makes cached entries through the old scheme unsound.
    ///
    /// A write of the scheme already registered rewrites nothing and clears nothing. Replaying an established scope writes every declaration's scheme once more, and a clear on each — a wholesale one, thousands of times per compilation — would keep the reduction cache from ever holding a reduct across the mentions of one literal.
    pub(crate) fn set_assumption_universe_context(
        &mut self,
        name: &Free,
        universe_context: UniverseContext,
    ) {
        if self
            .typing
            .frames
            .assumption_universe_context(name)
            .as_ref()
            == Some(&universe_context)
        {
            return;
        }
        self.typing
            .frames
            .set_assumption_universe_context(name, universe_context);
        self.typing.caches.invalidate_for_redefinition();
        self.typing.frames.forget_spellings();
    }

    fn is_defined(&self, name: &Free) -> bool {
        self.typing.frames.is_defined(name)
    }

    pub(crate) fn locals(&self) -> &[(Free, Term)] {
        self.typing.frames.locals()
    }

    fn define_entry(&mut self, name: Free, entry: DefEntry) {
        // A *fresh* definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF (the name analogue of the unsolved-metavariable argument in `Context::reduce`) — so the reduction cache retains every entry whose result does not mention it instead of clearing wholesale. This keeps reducts warm across the definitions one declaration mints, and across erasure's items, which erasure walks under one budget: it re-derives an item right after its `define`, and a cold re-reduction of a deep closed spine (a string literal's scan-state chain) would repeat all of its work.
        //
        // The elaboration cache survives the same fresh definition with no retain at all: its insert gate (`elaboration_cacheable`) already refused every entry naming a not-yet-defined global, so the fresh name appears in no surviving entry. A minted `let` binder — which the frame elaborators mint — is excluded from caching outright (`has_local_free`), and a global is only ever referenced once defined; so not clearing lets a deep spine memoize once across those definitions instead of re-elaborating its shared subterms after each.
        //
        // A *redefinition* voids both arguments — the frame elaborators define under labels that can rebind or shadow, and the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the label — so there both caches clear wholesale.
        if self.typing.frames.is_defined(&name) {
            self.typing.caches.invalidate_for_redefinition();
            self.typing.frames.forget_spellings();
        } else {
            self.typing.caches.retain_reductions_without(&name);
            self.typing.frames.forget_spellings_naming(&name);
        }

        self.typing.frames.define(name, entry);
    }

    /// Define `name`. `kind` is the declaring module item's [`DefinitionKind`], or `None` for a local binding no item declared.
    pub(crate) fn define(&mut self, name: &Free, term: &Term, kind: Option<&DefinitionKind>) {
        self.define_entry(*name, DefEntry::new(term.clone(), kind.cloned()));
    }

    /// Define `name` as a published item binds it: [`Context::define`], and where `proof`, to a term no reduction unfolds it to ([`curios_core::Definition::proof`]). Until its item is published a proof unfolds as any definition does, which is what its own check reads it by.
    fn define_published(&mut self, name: &Free, term: &Term, kind: &DefinitionKind, proof: bool) {
        self.define_entry(
            *name,
            DefEntry::published(term.clone(), kind.clone(), proof),
        );
    }

    pub(crate) fn define_assuming(
        &mut self,
        name: &Free,
        type_: &Term,
        term: &Term,
        kind: Option<&DefinitionKind>,
    ) {
        self.assume(name, type_);
        self.define(name, term, kind);
    }

    pub(crate) fn definition_kind(&self, name: &Free) -> Option<&DefinitionKind> {
        self.asks(name, Question::Kind);
        self.typing.frames.definition_kind(name)
    }

    /// Undo a top-level declaration's bindings ([`Frames::forget`]) — a refused item's, so the scope holds what elaborated and nothing of what did not. The caches clear as for a redefinition: an entry may have consumed the binding.
    pub(crate) fn forget(&mut self, name: &Free) {
        self.typing.frames.forget(name);
        self.typing.caches.invalidate_for_redefinition();
        self.typing.frames.forget_spellings();
    }

    // === Refinements (see [`Frames`]) =======================================

    /// [`Frames::refine`], with the refinement cache protocol — the variable now reduces differently — with every equation that names the variable restated under its value ([`Context::restate_equations`]), and with every equation the refinement closes withheld ([`Context::withhold_closed_equations`]).
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.typing.caches.invalidate_for_refinement();
        self.typing.frames.refine(name, term);
        self.restate_equations(name, term);
        self.withhold_closed_equations();
    }

    /// Restate, for as long as the current frame stands, every equation in force whose scrutinee names `name`, under the value an arm has just refined `name` to: the equation is withheld, and its instance at `value` recorded in its place.
    ///
    /// **The kernel's `Scope::restate`, in the elaborator's store.** The kernel substitutes a case's solution through the arm it checks and assumes each equation in force again at that instance, the recorded one stepping aside. The elaborator keeps the variable spelled and refines it, so an equation recorded outside the arm still names it, and a fact written at the case value — `0 < List/len(xs)` under `match i < List/len(xs)` and then `match i | 0` — meets no key: the entry's reduced spelling is settled with the arm's frame withheld, and names the variable still. The instance is the same hypothesis read where the arm reads it, since what holds of the scrutinee at every value of the variable holds at the one the case fixes.
    ///
    /// **The recorded equation steps aside, as the kernel's does**, so an equation stands once however deep the arms go: kept beside its instance it would double at every nested match on a variable it names. A term in the arm that still names the variable meets the instance because a key and a probe are spelled as the arm spells them ([`Context::spelled_as_refined`]).
    ///
    /// An instance that names no local is recorded by neither checker: [`Context::withhold_closed_equations`], which a refinement runs next, withholds it as it withholds any equation the refinement closes. The value an equation assumes is left as it is, a variable in it being read through its refinement.
    fn restate_equations(&mut self, name: &Free, value: &Term) {
        if !self.typing.frames.has_scrutinee_refinements() {
            return;
        }
        let solution = [(*name, value.clone())];
        let named = self
            .typing
            .frames
            .scrutinee_equations()
            .into_iter()
            .filter_map(|(key, entry)| {
                // Read with its solved metavariables materialized: a metavariable stands over a spine of every local in scope where it was born, and would read as naming the variable.
                let original = zonk_solved_term_metas(self, &entry.original);
                original
                    .mentions_free(name)
                    .then_some((key, entry, original))
            })
            .collect::<Vec<_>>();

        for (key, entry, original) in named {
            let restated = ScrutineeEntry {
                original: original.substitute(&solution),
                value: entry.value.clone(),
                alias: entry.alias,
                withheld: false,
            };
            self.typing.frames.withhold_scrutinee(key, entry);
            let key = shallow_scrutinee(self, &restated.original);
            self.typing.frames.refine_scrutinee(key, restated);
        }
    }

    /// `term` with every variable an arm in force refined spelled as the value it was refined to: how the kernel, which substitutes a case's solution through the arm it checks, spells a term of that arm. A refinement's value may name a variable an arm inside refined in turn, so the substitution is taken until the term names no refined variable.
    ///
    /// A term that names none is returned as it is, for a walk over its free variables: this is read at every probe of a refined head and at every stuck form put to the reduced spellings.
    pub(crate) fn spelled_as_refined(&self, term: &Term) -> Term {
        if !term.has_local_free() || !self.typing.frames.refines_a_variable() {
            return term.clone();
        }
        let refined_in = |spelled: &Term| {
            spelled
                .free_vars_shared()
                .iter()
                .filter_map(|name| Some((*name, self.typing.frames.refinement_of(name)?.clone())))
                .collect::<Vec<_>>()
        };
        let mut refined = refined_in(term);
        if refined.is_empty() {
            return term.clone();
        }
        let mut spelled = term.clone();
        // Each round substitutes at least one refined variable away, so a chain of them is no longer than the variables refined.
        for _ in 0..self.typing.frames.refined_variables().len() {
            spelled = spelled.substitute(&refined);
            refined = refined_in(&spelled);
            if refined.is_empty() {
                break;
            }
        }
        spelled
    }

    /// `term` as the kernel is handed it under the arms in force: its solved metavariables materialized, its local definitions substituted, and every variable an arm refined spelled as the value it was refined to. The kernel substitutes a case's solution through the arm it checks, where the elaborator records it and keeps the variable spelled, so this is the reading a rule shared with the kernel is judged on.
    ///
    /// A solved metavariable is materialized first because it stands over a spine of the locals in scope where it was born, and would read as naming them: a guard written through a concept holds its witness as one, and names no local of its own once the witness is the global it resolved to.
    pub(crate) fn kernel_spelling(&self, term: &Term) -> Term {
        self.spelled_as_refined(
            &Unfolding::everything(self).term(&zonk_solved_term_metas(self, term)),
        )
    }

    /// Withhold, for as long as the current frame stands, every equation in force whose scrutinee names no local as the kernel spells it now ([`Context::kernel_spelling`]).
    ///
    /// **The recording rule, read where the kernel reads it.** No checker records an equation under a spelling that mentions no local (`curios_analysis::records_case_equation`). The kernel substitutes an arm's solution through the equations in force, so a guard over a variable an arm inside refines to a closed value is, to the kernel, an equation about a closed term, which it does not record; the elaborator's key still names the variable, and left answering it would accept in that arm a proof the kernel refuses. A closed scrutinee computes, so nothing is lost where the arm is one a value reaches: the fact holds by reduction. Where it computes to another case than its guard assumed the arm is dead, and a proof resting on the guard is refused where it is written.
    fn withhold_closed_equations(&mut self) {
        if !self.typing.frames.has_scrutinee_refinements() {
            return;
        }
        let closed = self
            .typing
            .frames
            .scrutinee_equations()
            .into_iter()
            .filter(|(_, entry)| !records_case_equation(&self.kernel_spelling(&entry.original)))
            .collect::<Vec<_>>();
        for (key, entry) in closed {
            self.typing.frames.withhold_scrutinee(key, entry);
        }
    }

    /// [`Frames::refine_projection`], with the refinement cache protocol.
    pub(crate) fn refine_projection(&mut self, base: Term, index: usize, value: Term) {
        self.typing.caches.invalidate_for_refinement();
        self.typing.frames.refine_projection(base, index, value);
    }

    /// [`Frames::definition_body`], and a need where the body is missing because its declaration has not finished ([`Context::missed`]): a reduction that asks for one would otherwise go stuck on a name that only has no body yet, and the attempt would be judged on a term it could have reduced.
    pub(crate) fn definition_body(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Body);
        let body = self.typing.frames.definition_body(name);
        if body.is_none() {
            self.missed(name);
        }

        body
    }

    /// [`Frames::var_reduct`], and a need as [`Context::definition_body`] records one.
    pub(crate) fn var_reduct(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Body);
        let reduct = self.typing.frames.var_reduct(name);
        if reduct.is_none() {
            self.missed(name);
        }

        reduct
    }

    /// [`Frames::var_reduct_at`], and a need as [`Context::definition_body`] records one.
    pub(crate) fn var_reduct_at(&self, name: &Free) -> Option<&Term> {
        self.asks(name, Question::Body);
        let reduct = self.typing.frames.var_reduct_at(name);
        if reduct.is_none() {
            self.missed(name);
        }

        reduct
    }

    /// [`Frames::projection_entry`]'s value, with `base` and the base the arm scrutinized held to one universe instance, and declined where they cannot be ([`hold_at_one_instance`]).
    ///
    /// The commitment sits in the accessor for the reason [`Context::scrutinee_reduct`] states, and this store needs one more thing to carry it: its key erases the base, so the unerased spelling is kept beside the value rather than recovered.
    pub(crate) fn proj_reduct(&mut self, base: &Term, index: usize) -> Option<Term> {
        let entry = self.typing.frames.projection_entry(base, index)?;
        let (original, value) = (entry.original.clone(), entry.value.clone());

        hold_at_one_instance(self, base, &original)
            .unwrap_or(false)
            .then_some(value)
    }

    /// Record one guard's equation under every spelling it is met by — each `(canonical, original, alias)` — with the refinement cache protocol run once for all of them rather than once per spelling.
    pub(crate) fn refine_scrutinee_spellings(
        &mut self,
        spellings: Vec<(Term, Term, bool)>,
        value: &Term,
    ) {
        self.typing.caches.invalidate_for_refinement();
        for (canonical, original, alias) in spellings {
            self.typing.frames.refine_scrutinee(
                canonical,
                ScrutineeEntry {
                    original,
                    value: value.clone(),
                    alias,
                    withheld: false,
                },
            );
        }
    }

    pub(crate) fn has_scrutinee_refinements(&self) -> bool {
        self.typing.frames.has_scrutinee_refinements()
    }

    pub(crate) fn scrutinee_head_refined(&self, head: HeadTag<'_>) -> bool {
        self.typing.frames.scrutinee_head_refined(head)
    }

    /// [`Frames::scrutinee_entry`]'s value, with `probe` and the spelling the equation was registered on held to one universe instance, and declined where they cannot be ([`hold_at_one_instance`]). The two are read as their key spells them ([`key_spelling`]), so they differ in levels alone.
    ///
    /// **The commitment is in the accessor, so no read of the store rests on an instance it has not held.** The key cannot carry this: it is computed when an arm is entered, which is before the levels in it are solved, and it is then compared for as long as the arm stands. The kernel needs none of it because it is handed a zonked module, where every instance is already ground and keying on the scrutinee itself is exact ([Case equations and their key](../../documentation/design/soundness/case-equations-and-their-key.md)).
    ///
    /// A universe error declines. That is the direction the whole read moves in — fewer refinements fire, never more — so it can cost a reduction and never admit one.
    pub(crate) fn scrutinee_reduct(&mut self, canonical: &Term, probe: &Term) -> Option<Term> {
        let entry = self.typing.frames.scrutinee_entry(canonical)?;
        if entry.withheld {
            return None;
        }
        let (original, value) = (entry.original.clone(), entry.value.clone());
        let (probe, original) = (key_spelling(self, probe), key_spelling(self, &original));

        hold_at_one_instance(self, &probe, &original)
            .unwrap_or(false)
            .then_some(value)
    }

    /// Every equation in force with the key it is held under, aliases among them, innermost frame first.
    pub(crate) fn scrutinee_equations(&self) -> Vec<(Term, ScrutineeEntry)> {
        self.typing.frames.scrutinee_equations()
    }

    /// Every equation an arm in force withholds, innermost frame first.
    pub(crate) fn withheld_equations(&self) -> Vec<ScrutineeEntry> {
        self.typing.frames.withheld_equations()
    }

    pub(crate) fn visible_scrutinee_entries(
        &self,
    ) -> impl Iterator<Item = (usize, &Term, &ScrutineeEntry)> {
        self.typing.frames.visible_scrutinee_entries()
    }

    pub(crate) fn is_scrutinee_key(&self, canonical: &Term) -> bool {
        self.typing.frames.is_scrutinee_key(canonical)
    }

    pub(crate) fn refinements_suppressed(&self) -> bool {
        self.typing.frames.refinements_suppressed()
    }

    /// The refinements a lookup may read now, of every kind — what a metavariable born here is born under.
    pub(crate) fn refinement_snapshot(&mut self) -> SharedRefinements {
        self.typing.frames.refinement_snapshot()
    }

    /// Whether `refinements` are exactly the ones a lookup may read now, so that judging under them needs no bracket.
    pub(crate) fn refinements_are(&mut self, refinements: &Refinements) -> bool {
        let current = self.typing.frames.refinement_snapshot();
        std::ptr::eq(&*current, refinements) || current.same_as(refinements)
    }

    /// Record `refinements` in the current frame, each with the refinement cache protocol, in order, so an inner one shadows an outer as it did where they were taken.
    pub(crate) fn install_refinements(&mut self, refinements: &Refinements) {
        for (name, value) in &refinements.variables {
            self.refine(name, value);
        }

        for ((_, index), entry) in &refinements.projections {
            // Re-registered from the *unerased* base, never the key: the key is what erasing that base produced, and re-erasing it would lose the spelling the read compares against.
            self.refine_projection(entry.original.clone(), *index, entry.value.clone());
        }

        // In order, so an entry that withholds an equation lands after the equation it withholds and shadows it here as it did where they were taken.
        for (canonical, entry) in &refinements.scrutinees {
            match entry.withheld {
                true => {
                    self.typing.caches.invalidate_for_refinement();
                    self.typing
                        .frames
                        .withhold_scrutinee(canonical.clone(), entry.clone());
                }
                false => self.refine_scrutinee_spellings(
                    vec![(canonical.clone(), entry.original.clone(), entry.alias)],
                    &entry.value,
                ),
            }
        }
    }

    /// Run `f` under exactly the refinements `birth` holds — what a metavariable's solution, or a candidate for a written goal, is judged under. At once where they are the ones a lookup reads already, the common case of a term judged in the arm it was born in; otherwise with every refinement registered so far withheld and `birth`'s reinstalled in a frame above them, since a frame a suppression enters keeps its own refinements live.
    pub(crate) fn with_refinements<R>(
        &mut self,
        birth: &Refinements,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        if self.refinements_are(birth) {
            return f(self);
        }

        self.with_suppressed_refinements(|context| {
            context.with_frame(|context| {
                context.install_refinements(birth);
                f(context)
            })
        })
    }

    /// Whether any refinement of any kind is registered, suppressed or not — the closed machine's gate. Suppression must not open it: a suppressed scrutinee key is *withheld* by the strategy, and the machine evaluating it would hand out the value the suppression exists to withhold.
    pub(crate) fn any_refinements_registered(&self) -> bool {
        self.typing.frames.any_refinements_registered()
    }

    /// Run `f` with the refinements of `frame` and every frame inside it withheld — the bracket a scrutinee entry's reduced spelling is settled under, so it rests only on the equations that outlive it. [`Frames::withhold_refinements_from`] carries the rule and [`Caches::invalidate_settlement_boundary`] the cache protocol at each side.
    pub(crate) fn with_refinements_withheld_from<R>(
        &mut self,
        frame: usize,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.typing
            .caches
            .invalidate_settlement_boundary(self.typing.asking.plain());
        let previous = self.typing.frames.withhold_refinements_from(frame);
        let result = f(self);
        self.typing.frames.restore_withheld_refinements(previous);
        self.typing
            .caches
            .invalidate_settlement_boundary(self.typing.asking.plain());

        result
    }

    /// Run `f` with every refinement registered *so far* suppressed — the slow half of [`Context::with_refinements`], which reinstalls a birth's refinements above it. A frame `f` enters keeps its own refinements live: those belong to the term being judged rather than to the arm the caller sits in, and `Frames::suppress_refinements_below` records why the two are not the same kind.
    ///
    /// Brackets the region with reduction-cache clears so refinement-applied and refinement-suppressed reducts never contaminate each other's cache — but only when some refinement is actually registered. With none, suppressing changes no reduct, so the depth is inert and the clears are pure waste (the common re-validation path: an oracle run outside any match arm). Each boundary is gated on the live state independently, so a refinement added and dropped *inside* `f` — which clears on its own add and exit — does not force a clear here.
    pub(crate) fn with_suppressed_refinements<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        if self.typing.frames.any_refinements_registered() {
            self.typing.caches.invalidate_suppression_boundary();
        }

        let previous = self.typing.frames.suppress_refinements_here();
        let result = f(self);
        self.typing.frames.restore_refinement_suppression(previous);

        if self.typing.frames.any_refinements_registered() {
            self.typing.caches.invalidate_suppression_boundary();
        }

        result
    }

    // === Registries (see [`Program`]) =======================================

    pub(crate) fn register_induct(
        &mut self,
        name: &Global,
        induct_decl: InductDecl,
    ) -> Result<(), Error> {
        self.typing.program.register_induct(name, induct_decl)
    }

    pub(crate) fn update_induct(&mut self, name: &Global, induct_decl: InductDecl) {
        self.typing.program.update_induct(name, induct_decl);
    }

    /// Take a refused declaration's inductive entry out of the registry, if it had one.
    pub(crate) fn remove_induct(&mut self, name: &Global) {
        self.typing.program.remove_induct(name);
        self.typing.caches.note_write();
    }

    /// Take a refused declaration's struct entry out of the registry, if it had one.
    pub(crate) fn remove_struct(&mut self, name: &Global) {
        self.typing.program.remove_struct(name);
        self.typing.caches.note_write();
    }

    /// Take a refused declaration's concept entry out of the registry, if it had one.
    pub(crate) fn remove_concept(&mut self, name: &Global) {
        self.typing.program.remove_concept(name);
        self.typing.caches.note_write();
    }

    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.asks(&Free::Global(*name), Question::Induct);
        self.reads_entry(name);
        self.typing.program.induct_decl(name)
    }

    pub(crate) fn register_struct(
        &mut self,
        name: &Global,
        struct_decl: StructDecl,
    ) -> Result<(), Error> {
        self.typing.program.register_struct(name, struct_decl)
    }

    pub(crate) fn update_struct(&mut self, name: &Global, struct_decl: StructDecl) {
        self.typing.program.update_struct(name, struct_decl);
    }

    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.asks(&Free::Global(*name), Question::Struct);
        self.reads_entry(name);
        self.typing.program.struct_decl(name)
    }

    pub(crate) fn register_concept(
        &mut self,
        name: &Global,
        concept: ConceptDecl,
    ) -> Result<(), Error> {
        self.typing.program.register_concept(name, concept)
    }

    pub(crate) fn concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.asks(&Free::Global(*name), Question::Concept);
        self.reads_entry(name);
        self.typing.program.concept(name)
    }

    /// The concept registered under `name`, the unit's own or a unit's in scope, read as [`Context::concepts`] reads the unit's table: no read is recorded. It is what the registry's validation follows a superclass edge by, which decides nothing of the declaration being elaborated and so is no need of it.
    pub(crate) fn registered_concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.typing.program.concept(name)
    }

    pub(crate) fn update_concept(&mut self, name: &Global, concept: ConceptDecl) {
        self.typing.program.update_concept(name, concept);
    }

    pub(crate) fn concepts(&self) -> &BTreeMap<Global, ConceptDecl> {
        self.typing.program.concepts()
    }

    /// Record the prefixes one seeded module's unit claims. See [`Program::mount`].
    pub(crate) fn mount(&mut self, mounts: &[Mount]) {
        self.typing.program.mount(mounts);
    }

    pub(crate) fn mount_of(&self, name: &Global) -> Option<&Mount> {
        self.typing.program.mount_of(name)
    }

    pub(crate) fn mount_of_head(&self, head: &HeadKey) -> Option<&Mount> {
        self.typing.program.mount_of_head(head)
    }

    pub(crate) fn mark_witness_declaration(&mut self, name: &Global) {
        self.underway.witnesses.mark(name);
    }

    pub(crate) fn is_witness_declaration(&self, name: &Global) -> bool {
        self.underway.witnesses.is_marked(name)
    }

    pub(crate) fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<Witness> {
        let found = self.typing.program.witness(concept, key);
        let question = Question::Witness(*concept, *key);
        // Asked after the answer is in hand, and so under a second lookup only the first time a key is asked.
        if !self.typing.traced.noted(question) {
            let name = found.as_ref().map(|witness| witness.name);
            self.typing.traced.note(question, Answered::Witness(name));
        }

        found
    }

    /// [`DeclaredWitnesses::declare`].
    pub(crate) fn declare_witness(&mut self, name: Global, declared: Declared) {
        self.underway.witnesses.declare(name, declared);
    }

    /// [`DeclaredWitnesses::spelled`].
    pub(crate) fn spelled_witness(&self, name: &Global) -> Option<Option<&(Global, WitnessKey)>> {
        self.underway.witnesses.spelled(name)
    }

    /// Take the declared witness `name` out of what a question is answered by, poisoning the key it was spelled at: it was refused or withheld, and a goal keyed there is its dependent whether or not it had registered.
    pub(crate) fn withdraw_declared_witness(&mut self, name: &Global) {
        if let Some((concept, key)) = self.underway.witnesses.settle(name) {
            self.underway.witnesses.poison(concept, key);
        }
    }

    /// Whether a witness stands under `(concept, key)`: one registered, or one the unit declares there and has not elaborated. A question of membership, answered the same whichever witnesses have elaborated.
    pub(crate) fn witness_declared(&self, concept: &Global, key: &WitnessKey) -> bool {
        let declared = self.witness_stands(concept, key);
        self.typing.traced.note(
            Question::WitnessDeclared(*concept, *key),
            Answered::Whether(declared),
        );

        declared
    }

    /// What [`Context::witness_declared`] answers, noted in no trace: asked by the unit's walk, where it asks a trace's question again.
    pub(crate) fn witness_stands(&self, concept: &Global, key: &WitnessKey) -> bool {
        self.typing.program.witness_registered(concept, key)
            || self.underway.witnesses.at(concept, key).is_some()
    }

    /// The witness [`Context::witness`] finds at `(concept, key)`, by its name and noted in no trace: asked by the unit's walk, where it asks a trace's question again.
    pub(crate) fn witness_standing(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        self.typing.program.witness_name(concept, key)
    }

    /// The witness the unit declares at `(concept, key)` and has not finished: the one that registers there once it elaborates ([`DeclaredWitnesses::at`]).
    pub(crate) fn witness_pending(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        self.underway.witnesses.at(concept, key)
    }

    /// Whether the table's miss at `(concept, key)` is a witness the unit declares there and has not elaborated: the attempt needs it, and is void. `false` is a miss no other declaration answers — the one being attempted registers on its signature, so a miss on its own key is its signature asking for itself.
    pub(crate) fn witness_unfinished(&self, concept: &Global, key: &WitnessKey) -> bool {
        match self.underway.witnesses.at(concept, key) {
            Some(name) if !self.underway.needs.attempts(&name) => {
                self.need(name);
                true
            }
            _ => false,
        }
    }

    /// Every key a witness of `concept` stands under, with the module declaring it: the registered ones, then the ones the unit declares and has not elaborated — the raw material for reachability questions over one concept's edges (the missing-embedding chain report), the same whichever witnesses have elaborated.
    pub(crate) fn witness_keys(&self, concept: &Global) -> Vec<(WitnessKey, Qualifier)> {
        let registered = self.typing.program.witness_keys(concept).into_iter();
        let declared = self
            .underway
            .witnesses
            .keys(concept)
            .filter(|(key, _)| !self.typing.program.witness_registered(concept, key))
            .map(|(key, module)| (*key, *module));

        registered.chain(declared).collect()
    }

    /// [`Program::insert_witness`], stamping the write on an actual insert — a new witness can change which pure elaborations succeed.
    pub(crate) fn insert_witness(
        &mut self,
        concept: Global,
        key: WitnessKey,
        witness: Witness,
    ) -> Option<(Global, Qualifier)> {
        let existing = self.typing.program.insert_witness(concept, key, witness);
        if existing.is_none() {
            self.typing.caches.note_write();
        }
        existing
    }

    /// [`Program::remove_witness`], stamping the write: a witness gone can change which pure elaborations succeed as much as one arriving.
    pub(crate) fn remove_witness(&mut self, name: &Global) -> Vec<(Global, WitnessKey)> {
        let keys = self.typing.program.remove_witness(name);
        if !keys.is_empty() {
            self.typing.caches.note_write();
        }
        keys
    }

    /// Mark `(concept, key)` as a slot a refused declaration's witness stood in — see [`DeclaredWitnesses::poison`].
    pub(crate) fn poison_witness_key(&mut self, concept: Global, key: WitnessKey) {
        self.underway.witnesses.poison(concept, key);
    }

    pub(crate) fn is_poisoned_witness(&self, concept: &Global, key: &WitnessKey) -> bool {
        self.underway.witnesses.is_poisoned(concept, key)
    }

    /// [`Program::update_witness_scheme`], stamping the write.
    pub(crate) fn update_witness_scheme(
        &mut self,
        name: &Global,
        universe_context: UniverseContext,
        signature: Term,
    ) {
        self.typing
            .program
            .update_witness_scheme(name, universe_context, signature);
        self.typing.caches.note_write();
    }

    /// The module whose item is currently being elaborated (the qualifier prefix of its name; empty for the root), or `None` when no surface item is being elaborated — which suppresses the representation-privacy checks (see the field's invariant).
    pub(crate) fn island(&self) -> Option<&Qualifier> {
        self.underway.posture.island()
    }

    /// Set the current module before elaborating an item (see `elaborate_module_suffix`).
    pub(crate) fn set_island(&mut self, island: Qualifier) {
        // Every item boundary also lands a `define_entry` clear, but this one keeps the cache's soundness independent of that ordering.
        self.typing.caches.invalidate_for_island_change();
        self.underway.posture.stand(Some(island));
    }

    /// Run `f` with no island — suppressing the representation-privacy checks for re-derivation of already-elaborated terms, whose machinery-built projections were never subject to surface privacy in the first place. The bracket is the only way to clear an island (mirroring the parking half of the oracle package), so no context can be left permanently altered.
    pub(crate) fn with_suppressed_privacy<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.underway.posture.stand(None);
        let result = f(self);
        self.underway.posture.stand(previous);

        result
    }

    /// Whether the procedure that proves a bound from the facts in scope is running, so it is not asked again from inside its own candidate.
    pub(crate) fn entailing(&self) -> bool {
        self.underway.posture.entailing()
    }

    /// Run `f` as that procedure, which is not asked again until `f` returns.
    pub(crate) fn with_entailing<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.underway.posture.set_entailing(true);
        let result = f(self);
        self.underway.posture.set_entailing(previous);

        result
    }

    /// Whether reduction is plain: it asks the elaborator's conversion nothing ([`Context::plainly`]).
    pub(crate) fn plain(&self) -> bool {
        self.typing.asking.plain()
    }

    /// A judgment's reduction is putting a question to conversion. Its answer rests on the level constraints that stand, so a scope that is rolled back with constraints withdrawn after one was asked inside it clears what was reduced meanwhile ([`Context::rollback_solutions`]).
    pub(crate) fn note_question(&mut self) {
        self.typing.asking.note_question();
    }

    /// A question asked with nothing committed was answered no because answering yes would have committed a solution or a level constraint. The answer can change once the solver moves, so no reduct taken across it is remembered (`reduce`'s `remember`).
    pub(crate) fn note_declined(&mut self) {
        self.typing.asking.note_declined();
    }

    /// How many questions have been declined for a commit.
    pub(crate) fn declined(&self) -> u64 {
        self.typing.asking.declined()
    }

    /// Enter or leave a question asked with nothing committed (`same_uncommitted`), answering what stood before: one is entered with `Some(false)`, and what leaving it reads back says whether a level equation was withheld inside it.
    pub(crate) fn withhold(&mut self, withheld: Option<bool>) -> Option<bool> {
        self.typing.asking.withhold(withheld)
    }

    /// Whether an equation of `left` and `right` is withheld: conversion is answering a question asked with nothing committed, which joins no two levels, and the store does not already read the two as one. One withheld is noted on the question, whose no the solver can then change.
    pub(crate) fn withholds(&mut self, left: &Level, right: &Level) -> Result<bool, ReduceError> {
        if !self.typing.asking.withholding()
            || self
                .universes()
                .reads_as_one(left, right)
                .map_err(ReduceError::Universe)?
        {
            return Ok(false);
        }
        self.typing.asking.note_withheld();

        Ok(true)
    }

    /// The two atoms a classing is asking conversion about, which a classing made meanwhile is held inside (`curios_analysis::Classes::of`).
    pub(crate) fn asked(&self) -> Option<&(Term, Term)> {
        self.typing.asking.asked()
    }

    /// Enter or leave a classing's question about two atoms, answering what stood before.
    pub(crate) fn ask(&mut self, asked: Option<(Term, Term)>) -> Option<(Term, Term)> {
        self.typing.asking.ask(asked)
    }

    /// The universe solver's state now, which a settlement made across a declined question is filed beside.
    pub(crate) fn universe_state(&self) -> UniverseStateToken {
        self.underway.attempt.universe_solver.state_token()
    }

    /// Run `read` with reduction plain: it asks the elaborator's conversion nothing, at a stuck fold or at a missed equation — the kernel's `Kernel::plainly`, for the kernel's two readers. A question reduction puts to conversion is answered by it, which is what ends the regress, and a shared analysis reads a term by it (`Env::force`), since totality may rest on no verdict of conversion's. Each of the two keeps its own answers: the reducts, the sorts read through them and the settled spellings (`Frames::scrutinee_spellings`) are all filed by which reduction took them.
    pub(crate) fn plainly<R>(&mut self, read: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.typing.asking.set_plain(true);
        let answer = read(self);
        self.typing.asking.set_plain(previous);

        answer
    }

    // === Metavariable store =================================================

    /// Materialize a metavariable's birth record ([`Solutions::birth`]), stamping the write.
    pub(crate) fn birth_metavar(
        &mut self,
        id: MetavarId,
        telescope: impl Into<SharedTelescope>,
        result: Term,
    ) {
        self.birth_metavar_as(id, telescope.into(), result, MetaKind::Inference);
    }

    fn birth_metavar_as(
        &mut self,
        id: MetavarId,
        telescope: SharedTelescope,
        result: Term,
        kind: MetaKind,
    ) {
        self.typing.caches.note_write();
        let refinements = self.typing.frames.refinement_snapshot();
        let witnesses = self.witness_scope();
        let witnesses = (!witnesses.is_empty()).then(|| Rc::from(witnesses));
        self.underway.attempt.solutions.birth_with_kind(
            id,
            telescope,
            refinements,
            witnesses,
            result,
            kind,
        );
    }

    /// Allocate the protected placeholder for one member of a recursive group. It has the same contextual spine as an inference metavariable so parked work can carry it across a popped local frame, but only `fill_rec_slot` may solve it.
    ///
    /// `name` is the member this slot stands for, kept so the substitution walks can spell it — see [`Context::rec_slot_name`].
    pub(crate) fn fresh_rec_slot(&mut self, name: &Free, result: Term) -> (MetavarId, Term) {
        self.typing.caches.note_write();
        let id = self.underway.attempt.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        let refinements = self.typing.frames.refinement_snapshot();
        self.underway
            .attempt
            .solutions
            .birth_rec_slot(id, telescope, refinements, result);
        self.underway.attempt.name_rec_slot(id, *name);
        (id, Term::metavar_birthed(id, MetavarOrigin::Hole, spine))
    }

    /// [`Attempt::rec_slot_name`].
    pub(crate) fn rec_slot_name(&self, id: MetavarId) -> Option<&Free> {
        self.underway.attempt.rec_slot_name(id)
    }

    pub(crate) fn is_rec_slot(&self, id: MetavarId) -> bool {
        self.underway.attempt.solutions.is_rec_slot(id)
    }

    /// [`Solutions::owner_fills`].
    pub(crate) fn owner_fills(&self, id: MetavarId) -> bool {
        self.underway.attempt.solutions.owner_fills(id)
    }

    pub(crate) fn fill_rec_slot(&mut self, id: MetavarId, term: Term) {
        let entry = self
            .underway
            .attempt
            .solutions
            .entry(id)
            .expect("recursive slot has a birth entry");
        assert_eq!(entry.kind, MetaKind::RecSlot, "filled a non-rec slot");
        assert!(entry.solution.is_none(), "recursive slot filled twice");
        self.solve_metavar(id, term);
    }

    /// Fill a parked problem's placeholder with the term the problem rebuilt. Only the problem does, and once: conversion waits on a placeholder ([`Solutions::owner_fills`]), and whatever runs a parked problem holds it out of the store, so a second fill would be a second elaboration of one written term.
    pub(crate) fn fill_placeholder(&mut self, id: MetavarId, term: Term) {
        let entry = self
            .underway
            .attempt
            .solutions
            .entry(id)
            .expect("placeholder has a birth entry");
        assert_eq!(
            entry.kind,
            MetaKind::Placeholder,
            "filled a hole that is no placeholder"
        );
        assert!(entry.solution.is_none(), "placeholder filled twice");
        self.solve_metavar(id, term);
    }

    /// [`Frames::is_top_level`]: whether `name` is a declaration's, bound wherever a solution may stand. That a declaration is there at all is its signature's to say, so the question is noted as one about it: a candidate a solver admits for naming a declaration is another candidate once the declaration is gone.
    pub(crate) fn is_top_level(&self, name: &Free) -> bool {
        self.asks(name, Question::Signature);
        self.typing.frames.is_top_level(name)
    }

    pub(crate) fn identity_snapshot(&mut self) -> (SharedTelescope, SharedSpine) {
        self.typing.frames.identity_snapshot()
    }

    /// Mint a metavariable for an omitted implicit argument and birth it immediately — frozen local Γ, the binder's instantiated type as `result` — so the id always has a birth record. Returns its id beside the metavariable term carrying the *call site's* extent and the insertion provenance (which rides on the node; see [`Metavar::origin`]). `proposition` is whether `result` is one, decided by the caller, which has the sort in hand; it is kept on the birth record for the unsolved report, which cannot ask.
    pub(crate) fn fresh_metavar(
        &mut self,
        result: Term,
        extent: Option<Extent>,
        origin: ImplicitOrigin,
        proposition: bool,
        reduct: Option<Term>,
    ) -> (MetavarId, Term) {
        let (id, metavar) =
            self.fresh_metavar_with(result, extent, MetavarOrigin::Implicit(origin));
        if proposition {
            self.underway.attempt.solutions.mark_proposition(id);
        }
        if let Some(reduct) = reduct {
            self.underway.attempt.solutions.note_reduct(id, reduct);
        }
        if let Some(dead) = self.typing.frames.dead_arm().cloned() {
            self.underway.attempt.solutions.note_dead_arm(id, dead);
        }
        (id, metavar)
    }

    /// Record what an unsolved bound's type reduced to when a later attempt asked, for the report the hole becomes.
    pub(crate) fn note_reduct(&mut self, id: MetavarId, reduct: Term) {
        self.underway.attempt.solutions.note_reduct(id, reduct);
    }

    /// Record why the procedure that proves a bound from the facts in scope proved nothing, for the report the hole becomes.
    pub(crate) fn note_refusal(&mut self, id: MetavarId, refusal: Refusal) {
        self.underway.attempt.solutions.note_refusal(id, refusal);
    }

    /// Mint a metavariable for an omitted `use` argument — like [`Context::fresh_metavar`] but carrying witness provenance, and returning the id so the caller can register the resolution goal.
    pub(crate) fn fresh_witness_metavar(
        &mut self,
        result: Term,
        extent: Option<Extent>,
        origin: WitnessOrigin,
    ) -> (MetavarId, Term) {
        self.fresh_metavar_with(result, extent, MetavarOrigin::Witness(origin))
    }

    /// Mint a silent hole — the stand-in type a written goal in synthesis position gets, so the goal survives to zonk's report instead of dying with `CannotInfer` (`elaborate_metavar`).
    pub(crate) fn fresh_hole_metavar(&mut self, result: Term, extent: Option<Extent>) -> Term {
        self.fresh_hole(result, extent).1
    }

    /// [`Context::fresh_hole_metavar`] with the hole's identity beside it, for a site that reads whether unification solved it: an operator's operand type, defaulted where no operand pins it.
    pub(crate) fn fresh_hole(&mut self, result: Term, extent: Option<Extent>) -> (MetavarId, Term) {
        self.fresh_metavar_with(result, extent, MetavarOrigin::Hole)
    }

    /// Mint a metavariable identity with no birth record, for a term built ahead of its elaboration exactly as `into_core` builds one: an omitted motive, a list literal's element type, a witness goal with a provenance of its own. Birth happens where elaboration first meets the term, in checking position, as it does for every lowered hole.
    pub(crate) fn mint_metavar(&mut self) -> MetavarId {
        self.underway.attempt.solutions.mint()
    }

    /// The frozen ambient scope a settle-synthesized lambda's domain metavariables are born under — captured before the settle walk assumes a single lambda binder, so a domain's birth context is never wider than the expectation it will inhabit, which the embedded-metavariable exemption in `solve` relies on ([`Context::metavar_context_contained`]).
    pub(crate) fn domain_scope(&mut self) -> DomainScope {
        let (telescope, spine) = self.identity_snapshot();
        DomainScope { telescope, spine }
    }

    /// Mint the metavariable standing for a settle-synthesized lambda's unannotated domain — like [`Context::fresh_hole_metavar`] but carrying the binder's name, so zonk's unsolved report can point at the parameter whose type was never determined instead of raising a bare "cannot infer", and born at the settle site's ambient `scope` rather than under the lambda's own binders.
    pub(crate) fn fresh_domain_metavar(
        &mut self,
        scope: &DomainScope,
        result: Term,
        extent: Option<Extent>,
        binder: String,
    ) -> Term {
        let id = self.underway.attempt.solutions.mint();
        self.birth_metavar(id, scope.telescope.clone(), result);
        let metavar = Term::metavar_birthed(id, MetavarOrigin::Domain(binder), scope.spine.clone());
        match extent {
            Some(extent) => metavar.with_extent(extent),
            None => metavar,
        }
    }

    /// Whether `inner`'s birth context is contained in `outer`'s: every binder name of `inner`'s birth telescope is a binder name of `outer`'s. Binder names are entropy-fresh, so name identity is context identity, and the type at a shared name is the one both frames recorded. `solve`'s embedded-metavariable guard asks this to tell a candidate metavariable that could smuggle a name past `outer`'s scope from one that provably cannot.
    pub(crate) fn metavar_context_contained(&self, inner: MetavarId, outer: MetavarId) -> bool {
        let (Some(inner_entry), Some(outer_entry)) =
            (self.metavar_entry(inner), self.metavar_entry(outer))
        else {
            return false;
        };
        // Refinements as well as names: a `Bool` arm opens no binder, so a metavariable born inside `match k < n | true =>` has the telescope of one born just outside it, and a solution resting on the arm's guard would escape through a containment that read names alone.
        self.metavar_names_contained(inner, outer)
            && inner_entry.refinements.within(&outer_entry.refinements)
    }

    /// Whether a candidate for `outer` binds what `inner`'s birth context holds beyond `outer`'s: `inner` was born under no refinement `outer` was not, and each binder of its telescope that is none of `outer`'s stands, at every one of `occurrences`, as a variable the candidate binds itself. The occurrence then sits where `inner` was born, a binder of the candidate's own in the binder's place, and whatever `inner` comes to arrives through that spine and names nothing the candidate does not bind. It is what a settled lambda's codomain is in the function type the lambda states: born under the lambda's binders, so that the body's type may name them.
    pub(crate) fn metavar_bound_by_candidate(
        &self,
        inner: MetavarId,
        occurrences: &[Rc<Vec<Term>>],
        outer: MetavarId,
    ) -> bool {
        let (Some(inner_entry), Some(outer_entry)) =
            (self.metavar_entry(inner), self.metavar_entry(outer))
        else {
            return false;
        };
        if occurrences.is_empty() || !inner_entry.refinements.within(&outer_entry.refinements) {
            return false;
        }
        let outer_names: BTreeSet<&Free> =
            outer_entry.telescope.iter().map(|(name, _)| name).collect();

        occurrences.iter().all(|spine| {
            spine.len() == inner_entry.telescope.len()
                && spine
                    .iter()
                    .zip(inner_entry.telescope.iter())
                    .all(|(entry, (name, _))| {
                        outer_names.contains(name)
                            || matches!(&**entry, Subterm::Var(var) if var.as_free().is_none())
                    })
        })
    }

    /// The names half of [`Context::metavar_context_contained`]: every binder of `inner`'s birth telescope is one of `outer`'s.
    fn metavar_names_contained(&self, inner: MetavarId, outer: MetavarId) -> bool {
        let (Some(inner_entry), Some(outer_entry)) =
            (self.metavar_entry(inner), self.metavar_entry(outer))
        else {
            return false;
        };
        let outer_names: BTreeSet<&Free> =
            outer_entry.telescope.iter().map(|(name, _)| name).collect();
        inner_entry
            .telescope
            .iter()
            .all(|(name, _)| outer_names.contains(name))
    }

    /// How the unsolved `inner`, occurring at `occurrences` in a candidate for the occurrence `outer` and not contained in `outer`'s birth context, may be re-expressed in that context rather than hold the candidate back ([`Context::restrict_metavar`]): a stand-in born over binders of `outer`'s telescope, and what `inner` is in terms of it.
    ///
    /// A candidate for `outer` is inverted through `outer`'s spine — its variables back to their binders, its other entries abstracted — so whatever `inner` contributes must be spelled in those entries, and the stand-in takes them as its arguments. An `inner` contained by names keeps its own telescope. One whose telescope holds binders `outer`'s lacks is applied to `outer`'s entries spelled in its own birth names: an implicit born under a match arm's `v`, embedded in the match's type `?M(r, success(v))`, becomes `?E′(r, success(v))` for a stand-in over `?M`'s binders. No solution `outer` could take is lost — one of `inner` using `v` outside `success(v)` was never one the inversion admits, and one using it inside is the stand-in's — and an entry `inner` cannot spell is one it could never have contributed to. This is pruning (Abel and Pientka, *Higher-Order Dynamic Pattern Unification*, 2011), which restricts one pattern substitution by the inverse of another, over the solver's wider inverse; dropping `v` instead, as a context approximation does, would lose exactly the solutions through `success(v)`, which is how a match's inferred type depends on its scrutinee.
    ///
    /// What cannot be spelled that way waits: occurrences that disagree or are not a renaming, a binder the stand-in needs whose type depends on one it cannot take, and a type of `inner`'s own that mentions one. So does a hole something else fills: a written goal reports by its identity, a witness hole is filled by resolution, a bound by its discharge, a parked check's placeholder by that check, and a recursive group's slot by its group.
    pub(crate) fn metavar_restriction(
        &self,
        inner: MetavarId,
        origin: &MetavarOrigin,
        occurrences: &[Rc<Vec<Term>>],
        outer: &Metavar,
    ) -> Option<Restriction> {
        let (inner_entry, outer_entry) =
            (self.metavar_entry(inner)?, self.metavar_entry(outer.id)?);
        if !inner_entry.unification_fills(origin) {
            return None;
        }
        // Born under the refinements `inner` shares with `outer`'s birth: whatever reaches `outer` through `inner` must hold where `outer` is, so `inner`'s later solution can no longer rest on a guard `outer` would carry out of its arm.
        let refinements = || {
            Rc::new(
                inner_entry
                    .refinements
                    .shared_with(&outer_entry.refinements),
            )
        };

        if self.metavar_names_contained(inner, outer.id) {
            return Some(Restriction {
                arguments: inner_entry
                    .telescope
                    .iter()
                    .map(|(name, _)| Term::free_var(name))
                    .collect(),
                telescope: Rc::clone(&inner_entry.telescope),
                result: inner_entry.result.clone(),
                refinements: refinements(),
            });
        }

        // `outer`'s entries are spelled in `inner`'s birth names through its occurrence, which is one renaming wherever it occurs.
        let (spine, others) = occurrences.split_first()?;
        if others.iter().any(|other| other != spine)
            || spine.len() != inner_entry.telescope.len()
            || outer.spine.len() != outer_entry.telescope.len()
        {
            return None;
        }
        let mut births = BTreeMap::<&Free, &Free>::new();
        for (argument, (birth, _)) in spine.iter().zip(inner_entry.telescope.iter()) {
            let Subterm::Var(var) = &**argument else {
                return None;
            };
            let name = var.as_free()?;
            if self.is_top_level(name) || births.insert(name, birth).is_some() {
                return None;
            }
        }
        let spelled = |term: &Term| {
            term.free_vars_shared()
                .iter()
                .all(|name| births.contains_key(name) || self.is_top_level(name))
        };

        let entries = outer
            .spine
            .iter()
            .map(|entry| zonk_solved_term_metas(self, entry))
            .collect::<Vec<_>>();
        if entries
            .iter()
            .any(|entry| entry.metavars().contains(&inner))
        {
            return None;
        }
        let keep = entries.iter().map(spelled).collect::<Vec<_>>();
        let dropped = outer_entry
            .telescope
            .iter()
            .zip(&keep)
            .filter(|(_, kept)| !**kept)
            .map(|((name, _), _)| name)
            .collect::<BTreeSet<_>>();
        let telescope = outer_entry
            .telescope
            .iter()
            .zip(&keep)
            .filter(|(_, kept)| **kept)
            .map(|(binder, _)| binder.clone())
            .collect::<Vec<_>>();
        if telescope.iter().any(|(_, type_)| {
            type_
                .free_vars_shared()
                .iter()
                .any(|name| dropped.contains(name))
        }) {
            return None;
        }

        let (currents, birth_names): (Vec<&Free>, Vec<Term>) = births
            .iter()
            .map(|(current, birth)| (*current, Term::free_var(birth)))
            .unzip();
        let birth_refs = birth_names.iter().collect::<Vec<_>>();
        let arguments = entries
            .iter()
            .zip(&keep)
            .filter(|(_, kept)| **kept)
            .map(|(entry, _)| entry.capture(&currents).release(&birth_refs))
            .collect::<Vec<_>>();

        // `inner`'s type, spelled over the stand-in's binders: every birth name it mentions is one of the arguments.
        let mut mentioned = Vec::new();
        let mut binders = Vec::new();
        for name in inner_entry.result.free_vars_shared() {
            if self.is_top_level(name) {
                continue;
            }
            let position = arguments.iter().position(
                |argument| matches!(&**argument, Subterm::Var(var) if var.as_free() == Some(name)),
            )?;
            mentioned.push(name);
            binders.push(Term::free_var(&telescope[position].0));
        }
        let binder_refs = binders.iter().collect::<Vec<_>>();
        let result = inner_entry.result.capture(&mentioned).release(&binder_refs);

        Some(Restriction {
            telescope: Rc::new(telescope),
            arguments,
            result,
            refinements: refinements(),
        })
    }

    /// How the unsolved `inner`, occurring at `occurrences` in the type a block answers where it ends, may be re-expressed without the block's labels rather than leave the block naming them ([`Context::restrict_metavar`]): a stand-in born where the block began, over the binders of `inner`'s telescope before the block's own, at `inner`'s type with each label read as the value `values` pairs it with, in binding order.
    ///
    /// A block's term is closed over its labels where the block ends, and its type is not: a label in the spine of a metavariable the type holds is bound by nothing there, and a solution that names its binder, which a problem parked in the block may still give, reads at that occurrence as a name the kernel refuses. A label is a definition, so no solution depends on it that does not depend on its value as well, and none is lost with its binder: the stand-in takes a candidate that names the label by its value (`Convert::solve`). It is the restriction a candidate's embedded metavariable takes ([`Context::metavar_restriction`]), at the other place a metavariable leaves the context it was born in, and what Lean makes of a metavariable where a `let` is closed (`elimMVarDeps`). The spine stays the renaming a solution is inverted through, where the label's value written into it would be an entry the inversion cannot read, and a second name for whatever variable the value is.
    ///
    /// What cannot be spelled that way is read by value where it stands, its spine with it: an occurrence that is not the metavariable read where it was born, one born under a binder inside the block's tail, whose refinements and binders the block's own may type, one whose type still holds an unsolved metavariable naming a label, and a hole something else fills.
    pub(crate) fn block_restriction(
        &self,
        inner: MetavarId,
        origin: &MetavarOrigin,
        occurrences: &[Rc<Vec<Term>>],
        values: &[(Free, Term)],
    ) -> Option<Restriction> {
        let inner_entry = self.metavar_entry(inner)?;
        if !inner_entry.unification_fills(origin) {
            return None;
        }

        // Read where it was born: every binder as itself, so a value is spelled in the birth's names as it stands.
        let telescope = &inner_entry.telescope;
        let born = |spine: &Rc<Vec<Term>>| {
            spine.len() == telescope.len()
                && spine.iter().zip(telescope.iter()).all(|(entry, (binder, _))| {
                    matches!(&**entry, Subterm::Var(var) if var.as_free() == Some(binder))
                })
        };
        if occurrences.is_empty() || !occurrences.iter().all(born) {
            return None;
        }

        // Born in the block's own frame: its telescope ends with the block's labels, and what stands before them was bound before the block and names none.
        let labelled = |binder: &Free| values.iter().any(|(label, _)| label == binder);
        let kept = telescope.iter().position(|(binder, _)| labelled(binder))?;
        if !telescope[kept..].iter().all(|(binder, _)| labelled(binder)) {
            return None;
        }

        let result = zonk_solved_term_metas(self, &inner_entry.result);
        if self.names_in_unsolved_spine(&result, values) {
            return None;
        }

        Some(Restriction {
            telescope: Rc::new(telescope[..kept].to_vec()),
            arguments: telescope[..kept]
                .iter()
                .map(|(name, _)| Term::free_var(name))
                .collect(),
            result: read_by_value(result, values),
            refinements: Rc::clone(&inner_entry.refinements),
        })
    }

    /// Whether `term` holds an unsolved metavariable whose spine names one of the labels `values` pairs with a value: what a block's end restricts before it reads the rest by value ([`Context::block_restriction`]).
    pub(crate) fn names_in_unsolved_spine(&self, term: &Term, values: &[(Free, Term)]) -> bool {
        term.metavars().iter().any(|id| {
            self.metavar_solution(*id).is_none()
                && metavar_spines(term, *id).iter().any(|spine| {
                    spine
                        .iter()
                        .any(|entry| values.iter().any(|(label, _)| entry.mentions_free(label)))
                })
        })
    }

    /// Solve the unsolved `inner` to a fresh stand-in as `restriction` plans it ([`Context::metavar_restriction`], [`Context::block_restriction`]), born under the refinements the plan states. The stand-in keeps `inner`'s origin and `extent`, so an unsolved one reports as `inner` would have. Birth records stay frozen; the restriction is a solution, rolled back and reported like any other.
    pub(crate) fn restrict_metavar(
        &mut self,
        inner: MetavarId,
        (origin, extent): Provenance,
        restriction: Restriction,
    ) {
        let Some(inner_entry) = self.metavar_entry(inner) else {
            return;
        };
        let witnesses = inner_entry.witnesses.clone();
        let Restriction {
            telescope,
            arguments,
            result,
            refinements,
        } = restriction;

        self.typing.caches.note_write();
        let narrowed = self.underway.attempt.solutions.mint();
        self.underway
            .attempt
            .solutions
            .birth(narrowed, telescope, refinements, witnesses, result);
        let stand_in = Term::metavar_birthed(narrowed, origin, arguments);
        let stand_in = match extent {
            Some(extent) => stand_in.with_extent(extent),
            None => stand_in,
        };
        curios_profile::note!(
            target: "curios_elab::solve",
            meta = inner.0,
            %stand_in,
            "restricted",
        );
        self.solve_metavar(inner, stand_in);
    }

    fn fresh_metavar_with(
        &mut self,
        result: Term,
        extent: Option<Extent>,
        origin: MetavarOrigin,
    ) -> (MetavarId, Term) {
        let id = self.underway.attempt.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar(id, telescope, result);
        let metavar = Term::metavar_birthed(id, origin, spine);

        let metavar = match extent {
            Some(extent) => metavar.with_extent(extent),
            None => metavar,
        };

        (id, metavar)
    }

    pub(crate) fn metavar_entry(&self, id: MetavarId) -> Option<&MetaEntry> {
        self.underway.attempt.solutions.entry(id)
    }

    pub(crate) fn witness_hole(&self, term: &Term) -> Option<(WitnessOrigin, Term)> {
        self.underway.attempt.solutions.witness_hole(term)
    }

    pub(crate) fn metavar_solution(&self, id: MetavarId) -> Option<&Term> {
        self.underway.attempt.solutions.solution(id)
    }

    pub(crate) fn resolve_metavar(&self, metavar: &Metavar) -> Option<Term> {
        self.underway.attempt.solutions.resolve(metavar)
    }

    /// The type of `metavar` at its occurrence ([`Solutions::type_at`]).
    pub(crate) fn metavar_type_at(&self, metavar: &Metavar) -> Option<Term> {
        self.underway.attempt.solutions.type_at(metavar)
    }

    /// Commit a metavariable's solution ([`Solutions::solve`]), stamping the write. Needs no reduction-cache clear: a WHNF that still named an unsolved metavariable was never memoized (see `Context::reduce`), and a solve is monotonic, so every surviving entry stays valid. (Re-validation's [`Context::rollback_solutions`], which *un*-solves, does clear.)
    pub(crate) fn solve_metavar(&mut self, id: MetavarId, term: Term) {
        self.typing.caches.note_write();
        self.underway.attempt.solutions.solve(id, term);
    }

    /// Close a [`Context::solution_mark`] transaction, keeping whatever it left in place. Undoing is [`Context::rollback_solutions`]; a path that undoes still ends here.
    pub(crate) fn end_solutions(&mut self, mark: SolutionMark) {
        self.underway.attempt.universe_solver.release(mark.universe);
    }

    /// Watermark for [`Context::rollback_solutions`]: how many solutions have been committed so far. Spans both unification stores, which is why it lives here rather than on either.
    ///
    /// Opens a speculative scope on the universe solver, which [`Context::end_solutions`] closes. The two are paired by hand at every site: a closure bracket in the manner of [`Context::with_frame`] would be safer, but these sit on elaboration's recursions, where its body is a stack frame per level.
    pub(crate) fn solution_mark(&mut self) -> SolutionMark {
        SolutionMark {
            term_solution_log_len: self.underway.attempt.solutions.solved_len(),
            held_len: self.underway.attempt.solutions.held_len(),
            universe: self.underway.attempt.universe_solver.mark(),
            credited_len: self.underway.credits.mark(),
            checked_len: self.underway.checked.mark(),
            questions: self.typing.asking.questions(),
        }
    }

    /// How many term solutions have been committed so far: the watermark [`Context::solutions_since`] reads after.
    pub(crate) fn solutions_committed(&self) -> usize {
        self.underway.attempt.solutions.solved_len()
    }

    /// The solutions committed since `watermark` that still stand, each with its id.
    pub(crate) fn solutions_since(&self, watermark: usize) -> Vec<(MetavarId, Term)> {
        self.underway.attempt.solutions.solved_since(watermark)
    }

    /// Rewrite standing solutions in place to terms they already denote — what a recursive group does to its own members once its generalization has minted their instance — clearing every cache as a rollback does, since a cached reduct may have gone through a spelling replaced here.
    pub(crate) fn restamp_solutions(&mut self, solutions: Vec<(MetavarId, Term)>) {
        if solutions.is_empty() {
            return;
        }
        self.typing.caches.note_write();
        for (id, term) in solutions {
            self.underway.attempt.solutions.restamp(id, term);
        }
        self.typing.caches.invalidate_for_rollback();
        self.typing.frames.forget_spellings();
    }

    /// Unwind every solution committed since `mark` — the transactional bracket around re-validation. Validating a candidate runs full elaboration, which can solve *other* metavariables along the way; if the candidate is ultimately rejected, those nested solutions were derived from an equation that never held and must not survive the verdict. Removes the unwound ids from the wake signals.
    ///
    /// **It invalidates what can rest on what it undid, and nothing more.** An unwound term solution can sit inside any reduct or elaboration cached since, so every cache clears. A universe solver that moved without one — a constraint or a level solution withdrawn — is read by no reduct, reduction being parametric in levels, but an elaboration may have certified its purity against what was withdrawn, so the elaborations clear, as they do where a universe transaction closes. A rollback that undid nothing clears nothing: a witness probe rolls back after every trial — hundreds of times re-validating one `Str/split_once` stated in a type, few of which unwind a solution — and a clear at each would throw away the reducts and elaborations the next node needs.
    pub(crate) fn rollback_solutions(&mut self, mark: SolutionMark) {
        let unwinds_terms =
            self.underway.attempt.solutions.solved_len() > mark.term_solution_log_len;
        let universes_before = self.underway.attempt.universe_solver.state_token();
        self.underway
            .attempt
            .solutions
            .unwind_to(mark.term_solution_log_len, mark.held_len);
        self.underway
            .attempt
            .universe_solver
            .rollback(mark.universe);
        // A proof written inside what is rolled back reads nothing that stands.
        self.underway.credits.truncate(mark.credited_len);
        // Nor does a term settled inside it settle anything. It belonged to a candidate that is discarded, and the type it settled at may be a metavariable whose solution has just been unwound: left recorded, the totality obligations would zonk it at the item's end and report an implicit argument of a call the program never kept.
        self.truncate_checked(mark.checked_len);

        if unwinds_terms {
            self.typing.caches.invalidate_for_rollback();
            self.typing.frames.forget_spellings();
        } else if self.underway.attempt.universe_solver.state_token() != universes_before {
            self.typing.caches.note_universe_write();
            self.typing.caches.invalidate_for_universe_transaction();
            // A question asked inside the scope was answered on the level constraints it held, and a reduct or a settled spelling may rest on the answer: those go with the constraints. A scope that asked none leaves the reducts alone, which no rule of reduction reads a level for.
            if self.typing.asking.questions() != mark.questions {
                self.typing.caches.invalidate_for_withdrawn_answers();
                self.typing.frames.forget_spellings();
            }
        }
    }

    pub(crate) fn universes(&self) -> &UniverseSolver {
        &self.underway.attempt.universe_solver
    }

    /// Mutably borrow the universe solver and advance the authoritative [`Entropy`] stamp on guard drop only if solver state actually changed. Normalized ground/reflexive comparisons are read-equivalent and must not make an otherwise pure elaboration-cache computation look impure. Rollback performs the conservative cache clear separately.
    pub(crate) fn universes_mut(&mut self) -> UniverseMutation<'_> {
        let before = self.underway.attempt.universe_solver.state_token();
        UniverseMutation {
            solver: &mut self.underway.attempt.universe_solver,
            stamp: self.typing.caches.universe_stamp(),
            before,
        }
    }

    pub(crate) fn finish_universe_transaction(&mut self) {
        let before = self.underway.attempt.universe_solver.state_token();
        self.universes_mut().clear_constraints();
        if self.underway.attempt.universe_solver.state_token() != before {
            self.typing.caches.invalidate_for_universe_transaction();
        }
    }

    /// Close every speculative universe scope a refusal left open — see [`UniverseSolver::abandon_speculation`] for where that is sound.
    pub(crate) fn abandon_universe_speculation(&mut self) {
        self.underway.attempt.universe_solver.abandon_speculation();
    }

    /// A fresh level standing for `beside` at an occurrence of its own ([`UniverseSolver::fresh_beside`]).
    pub(crate) fn fresh_universe_beside(
        &mut self,
        beside: &Level,
        origin: Option<UniverseConstraintOrigin>,
    ) -> Result<Level, UniverseError> {
        self.universes_mut()
            .fresh_beside(beside, origin)
            .map(Level::meta)
    }

    /// The sort of a type left to inference: its quality and its level each a metavariable of its own ([`UniverseSolver::fresh_quality`]), so the first sort it is held to decides whether it is a proposition's or a type's.
    pub(crate) fn fresh_sort(&mut self, kind: &str) -> Term {
        let origin = UniverseConstraintOrigin::new(UniverseConstraintKind::Other(kind.into()));
        let quality = self.universes_mut().fresh_quality(Some(origin.clone()));
        let level = self
            .universes_mut()
            .fresh(UniverseRole::Flexible, Some(origin));
        Term::sort_at(quality, Level::meta(level))
    }

    /// The sort of a proposition left to inference: [`Context::fresh_sort`] with its quality stated, for a part of what is known a proposition — every field of a record of propositions is one.
    pub(crate) fn fresh_prop_sort(&mut self, kind: &str) -> Term {
        let origin = UniverseConstraintOrigin::new(UniverseConstraintKind::Other(kind.into()));
        let level = self
            .universes_mut()
            .fresh(UniverseRole::Flexible, Some(origin));
        Term::sort_at(Quality::Prop, Level::meta(level))
    }

    /// Close the qualities nothing decided, each as a type's ([`UniverseSolver::default_qualities`]). An item closes them once it has drained what it parked: a problem that waited on a metavariable may be the one that puts a proposition under a sort left to inference, and it is retried after the item's levels have closed.
    pub(crate) fn close_qualities(&mut self) {
        self.universes_mut().default_qualities();
    }

    /// Hold two sorts' qualities one, as conversion compares them: an unsolved one solved to the other, and two that each state a sort one where they state the same. Where conversion answers a question asked with nothing committed it solves none, as it joins no two levels there ([`Context::withholds`]), and an equation it would have made is noted on the question.
    pub(crate) fn same_quality(&mut self, this: Quality, that: Quality) -> bool {
        self.same_quality_at(this, that, None)
    }

    /// [`Context::same_quality`] asked on behalf of the term at `site`: what a `Sort` this decides is reported with.
    pub(crate) fn same_quality_at(
        &mut self,
        this: Quality,
        that: Quality,
        site: Option<Extent>,
    ) -> bool {
        let this = self.universes().quality(this);
        let that = self.universes().quality(that);
        if this == that {
            return true;
        }
        if self.typing.asking.withholding() {
            if matches!(this, Quality::Meta(_)) || matches!(that, Quality::Meta(_)) {
                self.typing.asking.note_withheld();
            }
            return false;
        }

        self.universes_mut().unify_qualities(this, that, site)
    }

    pub(crate) fn default_universes(&mut self, terms: &[&Term]) -> Result<Vec<Term>, Error> {
        let metas = terms
            .iter()
            .flat_map(|term| self.universe_metas_in(term))
            .collect::<BTreeSet<_>>();
        self.universes_mut().default(metas).map_err(Error::from)?;
        self.universes_mut().default_qualities();
        self.underway.scheme_open = false;
        let solver = self.underway.attempt.universe_solver.clone();
        let terms = terms
            .iter()
            .map(|term| zonk_universe_levels_scoped(*term, &solver).map_err(Error::from))
            .collect::<Result<Vec<_>, _>>()?;
        self.typing.caches.invalidate_for_universe_rewrite();
        self.typing.frames.forget_spellings();
        Ok(terms)
    }

    /// Finalize a declaration's levels — see `UniverseSolver::finalize`.
    pub(crate) fn finalize_universe_metas(
        &mut self,
        interface: BTreeSet<UniverseMetaId>,
        internal: BTreeSet<UniverseMetaId>,
        compared: BTreeSet<UniverseMetaId>,
    ) -> Result<UniverseContext, Error> {
        curios_profile::profile!("ctx::finalize_universe_metas");
        let mut universe_context = self
            .universes_mut()
            .finalize(interface, internal, compared)
            .map_err(Error::from)?;
        universe_context.quality_count = self.generalize_written_sorts()?;
        self.underway.scheme_open = false;
        self.typing.caches.invalidate_for_universe_rewrite();
        self.typing.frames.forget_spellings();
        Ok(universe_context)
    }

    /// Close the `Sort`s the declaration wrote as its parameters ([`UniverseSolver::generalize_qualities`]), answering how many it leaves to its caller. Refused where the declaration decided one, at the term that decided it where one was recorded and at the written `Sort` otherwise; where it leaves more than a declaration may; and where it formed a record type over two of them.
    fn generalize_written_sorts(&mut self) -> Result<usize, Error> {
        let count = self
            .universes_mut()
            .generalize_qualities()
            .map_err(|decided| {
                let prop = decided.sort.is_prop();
                match (decided.decided, decided.written) {
                    (Some(site), _) => Error::sort_decided(prop, true).at(site),
                    (None, written) => Error::sort_decided(prop, false).at_opt(written),
                }
            })?;
        if count > UniverseContext::QUALITY_LIMIT {
            return Err(Error::too_many_sorts(count));
        }
        if let Some(site) = self.universes().open_quality_join() {
            return Err(Error::record_over_two_sorts().at_opt(site));
        }

        Ok(count)
    }

    pub(crate) fn finalize_universe_metas_at_instance(
        &mut self,
        metas: BTreeSet<UniverseMetaId>,
        instance: &Universes,
        parameter_count: usize,
    ) -> Result<(), Error> {
        self.universes_mut()
            .finalize_at_instance(metas, instance, parameter_count)
            .map_err(Error::from)?;
        self.underway.scheme_open = false;
        self.typing.caches.invalidate_for_universe_rewrite();
        self.typing.frames.forget_spellings();
        Ok(())
    }

    /// Settle the levels a witness's scheme minted at one use: pinned to what its goal fixes, and closed there only once the declaration that raised the goal has finalized.
    pub(crate) fn close_universe_instance(
        &mut self,
        minted: &[Level],
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), Error> {
        match self.underway.scheme_open {
            true => self.universes_mut().pin_instance(instance, determined),
            false => self
                .universes_mut()
                .close_instance(minted, instance, determined),
        }
        .map_err(Error::from)?;
        self.typing.caches.invalidate_for_universe_rewrite();
        self.typing.frames.forget_spellings();
        Ok(())
    }

    pub(crate) fn zonk_universe_levels<B: Bound>(&self, value: &B) -> Result<B, Error> {
        zonk_universe_levels_scoped(value, &self.underway.attempt.universe_solver)
            .map_err(Error::from)
    }

    // === Parked constraints ============================================

    pub(crate) fn freeze_frame(&self) -> FrozenFrame {
        self.typing.frames.freeze()
    }

    /// Decide a parked problem in exactly the context it froze: enter a frame, hide every local frame below it ([`Frames::hide_frames_below_here`]), restore `frame` above the floor, and run `work` there.
    ///
    /// A retry is scheduled wherever a solution lands — a turnaround inside the same item, a nested or sibling arm, the drain after the item — and the live context at that point is not the problem's. Running on top of it would go wrong both ways: an arm's re-typed local would lose to the same name live at its unspecialized type when the retry runs outside the arm, and a nested arm's re-typings and refinements would decide a problem from an arm outside it. The floor makes the verdict a function of the problem and its frozen context alone, which is what parking promises.
    ///
    /// The caches are told when the floor hides a refinement, exactly as a suppression boundary tells them: a reduct computed with it visible must not answer under the floor, nor the other way round.
    pub(crate) fn with_retry_frame<R>(
        &mut self,
        frame: &FrozenFrame,
        work: impl FnOnce(&mut Self) -> R,
    ) -> R {
        curios_profile::profile!("ctx::retry_frame");
        self.with_frame(|context| {
            let (previous, hid_refinements) = context.typing.frames.hide_frames_below_here();
            if hid_refinements {
                context.typing.caches.invalidate_suppression_boundary();
            }

            context.restore_frame(frame);
            let result = work(context);

            context.typing.frames.restore_hidden_frames(previous);
            if hid_refinements {
                context.typing.caches.invalidate_suppression_boundary();
            }

            result
        })
    }

    /// Reapply a frozen frame above the floor [`Context::with_retry_frame`] set, restoring the context the parked problem's origin saw.
    ///
    /// Every assumption is reapplied, in binding order, so a name the frozen frame re-typed — an arm's specialization of a local — ends at its innermost type. Re-assuming a live identity doubles nothing: the floor hides whatever was live, and a birth telescope keeps one entry per name, so no metavariable born here gets the non-linear identity spine inversion cannot invert, and nothing is skipped.
    fn restore_frame(&mut self, frame: &FrozenFrame) {
        for (name, type_) in &frame.assumptions {
            self.assume(name, type_);
        }

        // Definitions take the same test as the assumptions above, and for a sharper reason: a name still defined with the frozen body *is* the frozen definition, and re-defining it reads to the cache protocol as a redefinition, which clears both caches wholesale. A witness parked under a hundred `let` binders would be retried a hundred clears at a time, every reduct the region had memoized recomputed after each.
        for (name, entry) in &frame.definitions {
            if self.definition_body(name) == Some(entry.body())
                && self.definition_kind(name) == entry.kind()
            {
                continue;
            }
            self.define_entry(*name, entry.clone());
        }

        self.install_refinements(&frame.refinements);
        if let Some(dead) = &frame.dead_arm {
            self.typing.frames.rule_out(Rc::clone(dead));
        }

        // The witness binders were already re-assumed by the loop above (they are a subset of `assumptions`); only the scope membership is restored here. The enclosing frame's mark truncates it on exit.
        self.typing
            .frames
            .extend_witness_scope(&frame.witness_binders);
    }

    /// Read `body` as a term that awaits: a metavariable born while it runs is awaited by it, and once it has read its parts and met its expectation it takes the choices left among the problems it awaits ([`Context::complete`]). A refusal completes nothing.
    pub(crate) fn awaiting<R>(
        &mut self,
        body: impl FnOnce(&mut Self) -> Result<R, Error>,
    ) -> Result<R, Error> {
        let (term, enclosing) = self.underway.attempt.solutions.enter_awaiting();
        let result = body(self).and_then(|read| self.complete(Some(term)).map(|()| read));
        self.underway
            .attempt
            .solutions
            .leave_awaiting(term, enclosing);

        result
    }

    /// Run `work`, a parked problem's retry, as awaited by the term that awaits what the problem watched, where one still stands, and by none where none does: the terms being read where the wake found it are not the problem's.
    pub(crate) fn retried_under<R>(
        &mut self,
        watching: &BTreeSet<MetavarId>,
        work: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let solutions = &mut self.underway.attempt.solutions;
        let awaiting = solutions.outermost(watching.iter().copied());
        let interrupted = solutions.read_under(awaiting);
        let result = work(self);
        self.underway.attempt.solutions.read_under(interrupted);

        result
    }

    /// Whether a standing term awaits an unsolved metavariable `term` mentions.
    pub(crate) fn awaits_any(&self, term: &Term) -> bool {
        let solutions = &self.underway.attempt.solutions;
        term.has_metavar()
            && term
                .metavars()
                .into_iter()
                .any(|id| solutions.solution(id).is_none() && solutions.awaited(id).is_some())
    }

    /// Have the term being read await the unsolved metavariables of `term` that no standing term awaits ([`Solutions::adopt`]).
    pub(crate) fn adopt(&mut self, term: &Term) {
        if term.has_metavar() {
            self.underway.attempt.solutions.adopt(term.metavars());
        }
    }

    /// [`Solutions::waiting_numerals`].
    pub(crate) fn waiting_numerals(&self) -> BTreeMap<MetavarId, Vec<NumLit>> {
        self.underway.attempt.solutions.waiting_numerals()
    }

    /// [`Solutions::choices_awaited_by`].
    pub(crate) fn choices_awaited_by(
        &self,
        term: Option<AwaitId>,
    ) -> (Vec<ParkedChoice>, Vec<BTreeSet<MetavarId>>) {
        self.underway.attempt.solutions.choices_awaited_by(term)
    }

    /// [`Solutions::conversions_awaited_by`].
    pub(crate) fn conversions_awaited_by(&self, term: Option<AwaitId>) -> Vec<ParkedConversion> {
        self.underway.attempt.solutions.conversions_awaited_by(term)
    }

    /// [`Solutions::drop_conversions`].
    pub(crate) fn drop_conversions(&mut self, at: &BTreeSet<usize>) {
        self.underway.attempt.solutions.drop_conversions(at);
    }

    /// [`Solutions::witnesses_awaited_by`].
    pub(crate) fn witnesses_awaited_by(&self, term: Option<AwaitId>) -> Vec<MetavarId> {
        self.underway.attempt.solutions.witnesses_awaited_by(term)
    }

    /// [`Solutions::take_witness`].
    pub(crate) fn take_witness(&mut self, slot: MetavarId) -> Option<ParkedProblem> {
        self.underway.attempt.solutions.take_witness(slot)
    }

    /// [`Solutions::choice_waiting_on`].
    pub(crate) fn choice_waiting_on(&self, id: MetavarId) -> Option<ParkedChoice> {
        self.underway.attempt.solutions.choice_waiting_on(id)
    }

    /// [`Solutions::note_shaped`].
    pub(crate) fn note_shaped(&mut self, choice: ParkedChoice) {
        self.underway.attempt.solutions.note_shaped(choice);
    }

    /// [`Solutions::take_shaped`].
    pub(crate) fn take_shaped(&mut self) -> Vec<ParkedChoice> {
        self.underway.attempt.solutions.take_shaped()
    }

    /// Park blocked work: freeze the live local frame around it and record which unsolved metavariables could unblock it.
    pub(crate) fn park(&mut self, work: ParkedWork, origin: Term) {
        curios_profile::profile!("ctx::park");
        let frame = self.freeze_frame();
        self.repark(work, origin, frame);
    }

    /// Re-park work that is still blocked after a retry, keeping its originally frozen frame ([`Solutions::park`]), stamping the write.
    pub(crate) fn repark(&mut self, work: ParkedWork, origin: Term, frame: FrozenFrame) {
        self.typing.caches.note_write();
        self.underway.attempt.solutions.park(work, origin, frame);
    }

    /// Mint the placeholder metavariable for a parked checking problem: birthed like any hole — frozen Γ, identity spine — with no insertion provenance, and of its own kind, since its check is what fills it ([`MetaKind::Placeholder`]). If it survives unsolved, the item drain reports the parked problem at its origin before zonk could ever meet the placeholder.
    pub(crate) fn fresh_placeholder(
        &mut self,
        result: Term,
        extent: Option<Extent>,
    ) -> (MetavarId, Term) {
        let id = self.underway.attempt.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar_as(id, telescope, result, MetaKind::Placeholder);
        let term = Term::metavar_birthed(id, MetavarOrigin::Hole, spine);

        let term = match extent {
            Some(extent) => term.with_extent(extent),
            None => term,
        };

        (id, term)
    }

    pub(crate) fn wake_parked(&mut self) -> Vec<ParkedProblem> {
        curios_profile::profile!("ctx::wake_parked");
        self.underway.attempt.solutions.wake_parked()
    }

    pub(crate) fn take_parked(&mut self) -> Vec<ParkedProblem> {
        self.underway.attempt.solutions.take_parked()
    }

    /// [`Solutions::take_checking`].
    pub(crate) fn take_checking(&mut self, placeholder: MetavarId) -> Option<ParkedProblem> {
        self.underway.attempt.solutions.take_checking(placeholder)
    }

    pub(crate) fn parked_len(&self) -> usize {
        self.underway.attempt.solutions.parked_len()
    }

    pub(crate) fn has_newly_solved(&self) -> bool {
        self.underway.attempt.solutions.has_newly_solved()
    }

    pub(crate) fn parking_suppressed(&self) -> bool {
        self.underway.attempt.solutions.parking_suppressed()
    }

    /// [`Solutions::imitates`].
    pub(crate) fn imitates(&self) -> bool {
        self.underway.attempt.solutions.imitates()
    }

    /// Run `work`, a `!` reading its region's monad, with an application of an unsolved metavariable imitated where conversion meets one: the region is the `!`'s expectation, so nothing read later bears on what its monad is.
    pub(crate) fn imitating<R>(&mut self, work: impl FnOnce(&mut Self) -> R) -> R {
        let before = self.underway.attempt.solutions.set_imitating(true);
        let result = work(self);
        self.underway.attempt.solutions.set_imitating(before);

        result
    }

    /// [`Solutions::holds_spines`].
    pub(crate) fn holds_spines(&self) -> bool {
        self.underway.attempt.solutions.holds_spines()
    }

    /// Run `work`, a conversion a completing term awaits asked again, with spines held: two applications of one definition met in it agree where their spines can be made to, by whatever that solves, and an application of an unsolved metavariable is imitated ([`Context::imitates`]).
    pub(crate) fn holding_spines<R>(&mut self, work: impl FnOnce(&mut Self) -> R) -> R {
        let before = self.underway.attempt.solutions.set_holding(true);
        let result = work(self);
        self.underway.attempt.solutions.set_holding(before);

        result
    }

    /// Whether work that cannot be judged yet may park. Inside an oracle it may not, and the oracle records that a site wanted to ([`Context::declined_to_park`]): the refusal that site goes on to raise says its term could not be judged, not that it is wrong. A site asks this last, once everything else says it would park, so the record is made only where a park was wanted.
    pub(crate) fn may_park(&mut self) -> bool {
        self.underway.attempt.solutions.may_park()
    }

    /// Whether a site inside the oracle in progress wanted to park and was refused. Read inside the oracle, by a reader with three answers: where the question failed and this holds, the failure is undecided rather than a refusal. It errs one way only — a check that recovered from a declined park and then failed on something rigid reads as undecided, and the rigid failure is reported when the work is retried.
    pub(crate) fn declined_to_park(&self) -> bool {
        self.underway.attempt.solutions.declined_to_park()
    }

    /// Run `f` as a yes/no *oracle* around full elaboration — a solution's re-validation, a goal candidate's fit — under the refinements `birth` holds and no others: the ones the checked term's metavariable was born under ([`Context::with_refinements`]). Parking is suppressed — `expect` treats `Blocked` as a mismatch and `retry_parked` is a no-op, so provisional success can neither leak into the verdict nor consume a parked obligation whose error the oracle would swallow — and so are the representation-privacy checks: an oracle candidate is a unification artifact that can embed machinery-built projections (eta-expansions, witness splices) whose privacy elaboration already adjudicated, and a swallowed privacy error would silently flip the verdict. The suppressions are a package: an oracle that set only some would be subtly unsound, which is why the parking half has no public setter. What the parking half suppressed is recorded for the oracle's own reader ([`Context::may_park`]): a goal candidate's fit and an entailment read a failure as no, and re-validation, which has a third answer, reads a failure where a park was declined as not judged yet. The bracket also keeps an elaboration table of its own, for what only a verdict may reuse — see [`Context::oracle_memoizable`].
    pub(crate) fn with_oracle<R>(
        &mut self,
        birth: &Refinements,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.typing.caches.begin_oracle();
        let result = self.with_suppressed_parking(|context| {
            context.with_refinements(birth, |context| context.with_suppressed_privacy(f))
        });
        self.typing.caches.end_oracle();

        result
    }

    fn with_suppressed_parking<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let enclosing = self.underway.attempt.solutions.enter_oracle();
        let result = f(self);
        self.underway.attempt.solutions.leave_oracle(enclosing);

        result
    }
}
