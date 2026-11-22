mod asking;
pub use asking::*;

mod birth_telescope;
pub use birth_telescope::*;

mod attempt;
use attempt::*;
pub use attempt::{Attempted, GoalObligation};

mod caches;
use caches::*;

mod checked;
use checked::*;

mod credits;
use credits::*;

mod sites;
use sites::*;
pub use sites::{Binding, Observed};

mod declared;
pub use declared::Declared;
use declared::*;

mod frame;
use frame::*;
pub use frame::{Height, HeldFrames, ScrutineeEntry, Settled};

mod in_scope;
pub use in_scope::*;

mod needs;
use needs::*;

mod posture;
use posture::*;

mod program;
pub use program::*;

mod traced;
use traced::*;
pub use traced::{Answered, ArchivedAnswered, ArchivedQuestion, Question};

mod solutions;
use solutions::*;
pub use solutions::{
    AwaitId, Choice, MetaEntry, ParkedChoice, ParkedConversion, ParkedProblem, ParkedWork, Scoped,
};

mod spend;
use spend::*;

mod typing;
use typing::*;

mod underway;
use underway::*;
pub use underway::{ReaderWrites, WalkListener, Walked};

mod scope;

mod universes;

mod registries;

mod budget;

mod reduction;

mod metavariables;

mod parked;

#[cfg(any(test, feature = "test-support"))]
mod test_support;

#[cfg(test)]
mod tests;

use {
    super::{Error, UniverseMark},
    crate::{Classified, Published, ZonkPass, zonk},
    curios_common::SyntaxRegistry,
    curios_core::{
        Bound, Declarations, DefinitionKind, Extent, Free, Global, Imports, Item, MetavarSpine,
        Mints, Nominal, Subterm, Term, WrittenBinder,
    },
    std::{
        collections::{BTreeSet, HashMap},
        mem,
        rc::Rc,
    },
};

/// Units of reduction work one declaration may spend before its budget is exhausted.
///
/// **Provisional.** A transition costs one unit, a construction what it builds, and a new peak of reduction depth the native frame it takes; it is not yet set against a corpus that replays a memoized construction rather than evaluating it once.
///
/// # What it is set against
///
/// **The prelude floor**, read rather than bisected. Both checkers sample each declaration's consumption as `budget::consumed` where the budget is restored, so the prelude build's profiles carry the floor as that sample's maximum. At `dd1c5dea6` the heaviest declaration `/std` holds spends **941 213 units on the elaborator** and **996 964 on the certifier**. Thirty million keeps a thirtyfold margin over the certifier's, the heavier of the two; the constant is chosen to keep roughly ten over the worst real declaration.
///
/// The two floors are not comparable as a ratio: the elaborator solves metavariables, resolves witnesses and zonks where the certifier rechecks a finished term. `curios-prelude`'s `stored_prelude_measurements` reports the certifier's per unit.
///
/// **What a unit costs in bytes.** A logical unit is eight bytes of payload; process memory per unit runs higher, by the copies and term traffic the price list deliberately does not model.
///
/// # Two things it does not buy
///
/// **A single oversized construction is still affordable, and no default the prelude can build under would refuse it.** `Nat/shl(1, 400000000)` prices at 6 250 004 units and builds fifty megabytes; refusing it outright needs a default of six million, which leaves the prelude's own floor a fraction of the margin the constant keeps. What the charge buys is a ceiling — the same term at a larger numeral refuses instead of taking the machine — not one low enough to call fifty megabytes unreasonable. Squeezing both ends onto one number is the one weighted limit `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` chooses, and these are its numbers.
///
/// **A `Str` literal's ceiling is this constant divided by a per-character price the closed machine sets in transitions, not frames**: on the machine, guarded depth is flat in the length, so the frame row does not enter the price. The current price and ceiling are `curios`' `str_literal_cost_measurements`', and `a_str_literal_costs_transitions_rather_than_frames` holds the shape. Raising this constant raises the ceiling proportionally, and the trade it would make is the one stated above rather than anything about strings.
pub const DEFAULT_BUDGET: u64 = 30_000_000;

/// What deciding the class of one written site may spend, apart from its declaration's budget ([`DEFAULT_BUDGET`]), which it leaves as it found it: a class reads a type's weak head, through a few structs of one field, and an allowance run out is no class, never a refusal. See `context/sites.rs`.
///
/// The least power of ten above the most a site of `/std`, `programs/` and `language.md`'s examples spends, which `curios`' `classification_allowance_measurements` counts and holds.
pub const LINT_ALLOWANCE: u64 = 100_000;

/// The identity spine over a [`BirthTelescope`] — one `Var::free` per binder — shared with the scope as the telescope is.
type SharedSpine = MetavarSpine;

/// The frozen scope a settle-synthesized lambda's domain metavariables are born under: the settle site's ambient frame, captured by [`Context::domain_scope`] before the lambda's own binders are assumed and consumed by [`Context::fresh_domain_metavar`]. Opaque outside this module — the walk that holds one can only pass it back.
#[derive(Clone)]
pub struct DomainScope {
    telescope: BirthTelescope,
    spine: SharedSpine,
    frames: HeldFrames,
}

/// How a metavariable is re-expressed in a context narrower than its own, that of the metavariable whose candidate embeds it ([`Context::metavar_restriction`]) or the one a block leaves standing where it ends ([`Context::block_restriction`]): the stand-in's telescope, drawn from the narrower context's, the arguments it is applied to in the restricted one's birth names, its type over its own binders, and the frames it holds. Opaque outside this module — what plans one can only pass it to [`Context::restrict_metavar`].
pub struct Restriction {
    telescope: BirthTelescope,
    arguments: Vec<Term>,
    result: Term,
    frames: HeldFrames,
}

/// Where the context stood when a question was asked with nothing committed ([`Context::asked_at`]): the two write stamps, and how many questions had been declined and how many pairs left apart.
#[derive(Debug)]
pub struct AskedAt {
    stamps: ElaborationStamp,
    declined: u64,
    apart: u64,
}

/// A transaction watermark spanning both unification stores.
#[derive(Debug, Clone, Copy)]
pub struct SolutionMark {
    term_solution_log_len: usize,
    /// How many times a metavariable's place among those links hold together had been written ([`Solutions::held_len`]).
    held_len: usize,
    universe: UniverseMark,
    credited_len: usize,
    /// How many written sites had been opened ([`Sites::mark`]).
    sites_len: usize,
    /// How many settled terms the totality obligations had been handed ([`Context::record_checked`]).
    checked_len: usize,
    /// How many questions reduction had put to conversion ([`Context::note_question`]).
    questions: u64,
    /// How many reducts had been filed ([`Caches::filings`]): what a rollback keeps.
    filed: u64,
}

/// The elaborator's ambient state, threaded mutably through elaboration, typing, reduction, conversion, and erasure, in two halves: what any judgment reads and the machine it runs on (`Typing`), and what exists because a unit is being elaborated (`Underway`), which a context that only reads finished terms holds empty. Two lifetimes coexist across them: the *frame-scoped* lexical state (`Frames`), pushed and popped as binders and match arms are entered, and the *flat monotonic facts* about the program (`Solutions`, `Program`), which frames never touch. The `Caches` police both with their write stamps, and this façade is where the stores coordinate: any method that writes a store *and* must stamp or clear a cache lives here, naming both sub-stores explicitly. Reduction is bounded by a step budget restored at every declaration boundary — see [`Context::new`].
#[derive(Debug)]
pub struct Context<'s> {
    // What any judgment reads, and the machine it runs on; see [`Typing`].
    typing: Typing<'s>,
    // What exists because a unit is being elaborated; see [`Underway`].
    underway: Underway<'s>,
}

/// What an auto-lift site names as written: the action, and the region's monad — its type without the value slot, as a report shows a monad, where the region applies a name; a nominal region, `Job(A)`, is spelled by its reduced form already. A missing embedding is reported through these rather than through the monads unification solved, which are reduced and so spell a type alias by its representation: `Of(Str, (input, offset) => Boundary(input, offset))` where the program wrote `Parse(Str, …)`. The action's monad is read off its head's declaration only when a report asks, since reading it opens the declaration's arrows.
#[derive(Debug, Clone)]
pub struct EmbeddingSite {
    pub action: Term,
    pub region: Option<Term>,
    /// Whether a `!` runs the action there, or the action is what the region returns: what the report of a missing edge says it was needed for.
    pub sequenced: bool,
}

impl<'s> Context<'s> {
    /// A fresh, empty context at [`DEFAULT_BUDGET`] — what every caller that is not threading a user-supplied budget wants.
    pub fn with_default_budget(syntax: SyntaxRegistry) -> Self {
        Self::new(DEFAULT_BUDGET, syntax)
    }

    /// A fresh, empty context in which each declaration may spend `budget` reduction steps, synthesizing the names `syntax` registers. Declarations, definitions, and the metavariable floor arrive later, seeded by `elaborate_definitions_suffix` as it walks the lowered module.
    ///
    /// The budget is *per declaration*, not per compilation: `elaborate_definitions_suffix` calls `Context::restore_budget` at every item boundary. A cumulative budget would make whether one declaration typechecks depend on how much the declarations before it had already spent, which is not a property of the declaration. Counting steps rather than elapsed time is what makes the answer a fact about the program instead of about the machine that ran it, so acceptance is reproducible across hosts, loads, and runs.
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

    /// The names this elaboration may synthesize.
    pub fn syntax(&self) -> SyntaxRegistry {
        self.typing.syntax
    }

    /// Install what the unit's `use` declarations brought into scope — each binding's canonical name with the spelling it resolves under, and per definition the ones in scope where it was written — so a goal report may suggest an imported definition the program has not mentioned yet, spelled the way the author would paste it where the goal is.
    pub fn set_imports(&mut self, imports: Imports) {
        self.underway.imports = imports;
    }

    /// Install what the unit's reader writes ([`ReaderWrites`]): the driver's answer, made of the tables a report spells a name by, which the driver holds once the unit is lowered and a report reads only after its sides are reduced.
    pub fn set_reader(&mut self, writes: ReaderWrites<'s>) {
        self.underway.reader = Reader(Some(writes));
    }

    /// Install whoever is told how far the walk over the unit's declarations has come ([`WalkListener`]): a driver that reports a check as it runs.
    pub fn listen(&mut self, listener: WalkListener<'s>) {
        self.underway.listening = Listening(Some(listener));
    }

    /// Tell whoever listens how far the walk has come, where anyone does.
    pub fn tell_walked(&mut self, walked: Walked<'_>) {
        if let Some(listener) = &mut self.underway.listening.0 {
            listener(walked);
        }
    }

    /// Whether the reader of the declaration being elaborated writes `declaration` by some name, standing in the module its item stands in. True where no driver said what its reader writes, and where no item's module stands, as the display it is asked for then reduces as it did before a reader was known.
    pub(crate) fn reader_writes(&self, declaration: &Global) -> bool {
        let (Some(writes), Some(island)) = (&self.underway.reader.0, self.island()) else {
            return true;
        };

        writes(island, self.underway.declaration.as_ref(), declaration)
    }

    /// The table [`Context::set_imports`] installed.
    pub(crate) fn imports(&self) -> &Imports {
        &self.underway.imports
    }

    /// Record one settled term and the type it settled at — proof totality's seed, collected where elaboration already knows the answer.
    ///
    /// Sort-hood is *not* decided here. The type may still carry unsolved metavariables, and deciding would both reduce on the hot path and risk an answer a later solution invalidates; the gate classifies post-zonk, memoized per distinct type.
    pub(crate) fn record_checked(&mut self, term: &Term, type_: &Term) {
        self.underway.checked.record(term, type_);
    }

    /// Name the definition whose body is elaborating, for proof totality's diagnostics. Returns the previous site so the caller can restore it.
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

    /// Read the recorded terms without consuming them — type totality reads them first, and proof totality takes them afterwards.
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

    /// Each term recorded since `mark`, its type and what each binding recorded since stands for, through `restate`: what a declaration recorded while its members were locals, which the obligations read after it has closed over them.
    pub(crate) fn restate_checked(
        &mut self,
        mark: usize,
        restate: impl Fn(&Self, &Term) -> Result<Term, Error>,
    ) -> Result<(), Error> {
        let recorded = self.underway.checked.split_off(mark);
        let mut restated = Vec::with_capacity(recorded.len());
        for (term, type_, site) in recorded {
            restated.push((restate(self, &term)?, restate(self, &type_)?, site));
        }
        self.underway.checked.replace_since(mark, restated);
        let mut labels = mem::take(&mut self.underway.checked);
        let restated = labels.restate_labels(mark, |value| restate(self, value));
        self.underway.checked = labels;

        restated
    }

    /// Record what each of `labels`, the bindings of a block that is ending, stands for, read by value while the block stands: a term recorded in the block names a binding by its label, and the erasure obligations read it as that value ([`Context::checked_labels`]).
    pub(crate) fn record_labels(&mut self, labels: &[Free]) {
        for label in labels {
            let value = self.by_value(&Term::free_var(label));
            self.underway.checked.record_label(*label, value);
        }
    }

    /// Each binding recorded since `mark`, with what it stands for: what a term recorded since `mark` names by a label is read as.
    pub(crate) fn checked_labels(&self, mark: usize) -> Vec<(Free, Term)> {
        self.underway
            .checked
            .labels_since(mark)
            .map(|(_, label, value)| (*label, value.clone()))
            .collect()
    }

    /// Begin a unit's items: the declarations they make, which place each of its names, and the names of those its walk is to decide, none finished yet; every other declaration of the unit is a baseline's, and finished. No lemma of the unit is on offer until its walk says so ([`Context::offer_lemmas`]).
    pub(crate) fn begin_unit(&mut self, declarations: Declarations, unfinished: BTreeSet<Global>) {
        self.underway.declarations = declarations;
        self.underway.needs.begin(unfinished);
        self.underway.offering = false;
    }

    /// Begin an attempt at the declaration declaring `names`, with nothing needed yet and in a state of its own ([`Context::begin_state`]).
    pub(crate) fn attempt(&mut self, names: &[Global]) {
        self.underway.needs.attempt(names);
        self.typing.traced.begin(names);
        self.begin_state();
    }

    /// Replace what one declaration's elaboration mints and solves with a state no other declaration wrote: its binder identities, its metavariables and their solutions, and its universe levels with the solver that settles them, each counting from what the declaration's own lowering minted ([`Minted`](curios_core::Minted)). What a declaration elaborates to then follows nothing of the declarations elaborated before it — neither how many there were nor what they left solved — and the identities it mints are the ones it would mint elaborated alone. Every other declaration is read as it was published ([`Context::publish`]), which holds none of these.
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
    fn exchange(&mut self, state: Attempted) -> Attempted {
        let (fresh_names, attempt) = state.into_parts();
        let replaced = Attempted::new(
            mem::replace(&mut self.typing.fresh_names, fresh_names),
            mem::replace(&mut self.underway.attempt, attempt),
        );
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

    /// Publish an item that holds a written goal by its types alone: each name stays assumed at its type, zonked, and is bound to nothing. What the item elaborated to holds the metavariables of the state its declaration elaborated in, which no other declaration reads, so what reads this one reads a constant of its type and what would unfold it stays stuck where the goal did. Refused where a type holds a goal itself.
    pub(crate) fn publish_held(&mut self, item: &Item) -> Result<(), Error> {
        let types = match item {
            Item::Let(definition) => vec![(
                definition.name,
                zonk(self, &definition.type_)?,
                definition.universe_context.clone(),
            )],
            Item::Rec(rec) => rec
                .exports()
                .iter()
                .enumerate()
                .map(|(index, definition)| {
                    Ok((
                        definition.name,
                        zonk(self, &rec.group().member_type(index))?,
                        rec.universe_context().clone(),
                    ))
                })
                .collect::<Result<_, Error>>()?,
            Item::Nominal(_) => Vec::new(),
        };
        // A declaration's formers and a family's constructors are made of its elaborated entries, which hold no goal: each is declared at its type.
        let item = self.carrying(item.clone());
        let made = item
            .formers()
            .into_iter()
            .chain(item.constructors())
            .map(|made| Ok((made.name, zonk(self, &made.type_)?, made.universe_context)))
            .collect::<Result<Vec<_>, Error>>()?;
        for (name, type_, universe_context) in types.into_iter().chain(made) {
            self.forget(&name);
            self.declare(&name, &type_);
            self.set_universe_context(&name, universe_context);
        }

        Ok(())
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
                let name = definition.name;
                self.redeclare(&name, &definition.type_);
                self.define_published(
                    &name,
                    &definition.body,
                    Some(&definition.kind),
                    definition.proof,
                );
                self.set_universe_context(&name, definition.universe_context.clone());
                // The sort a value inhabits is published with the body, by the mark that withholds both (`curios_core::Definition::published`).
                self.typing
                    .program
                    .record_definition_value_sort(&definition.name, definition.published().1);
                if self.is_witness_declaration(&definition.name) {
                    self.update_witness_scheme(
                        &definition.name,
                        definition.universe_context.clone(),
                        definition.type_.clone(),
                    );
                }
            }
            Item::Rec(rec) => {
                for (index, definition) in rec.exports().iter().enumerate() {
                    let name = definition.name;
                    let type_ = rec.group().member_type(index);
                    self.redeclare(&name, &type_);
                    self.set_universe_context(&name, rec.universe_context().clone());
                    self.define_published(
                        &name,
                        &Term::rec_proj(rec.group().clone(), index),
                        Some(&definition.kind),
                        definition.proof,
                    );
                    if self.is_witness_declaration(&definition.name) {
                        self.update_witness_scheme(
                            &definition.name,
                            rec.universe_context().clone(),
                            type_,
                        );
                    }
                }
            }
            Item::Nominal(_) => {}
        }
        // A declaration's formers and a family's constructors are names its item declares and holds no definition of: each declared here and bound to what the entry just published makes of it, at its declaration's universes, and total by construction.
        let item = self.carrying(published.item.clone());
        for made in item.formers().into_iter().chain(item.constructors()) {
            let name = made.name;
            self.forget(&name);
            self.declare(&name, &made.type_);
            self.define_published(&name, &made.body, None, false);
            self.set_universe_context(&name, made.universe_context.clone());
            self.record_definition_totality(&name, curios_core::Totality::Total);
            self.typing
                .program
                .record_definition_value_sort(&name, made.value_sort.as_ref());
        }
    }

    /// `item` holding the registry entries this context holds under each name it declares a former under: what a finished item is read and assembled as. An entry is elaborated in the context's tables and settled there as its item is published, so the one the item holds is replaced here, where the item leaves the elaboration that made it; a name the context holds nothing under keeps the one it has.
    pub(crate) fn carrying(&self, item: Item) -> Item {
        let Item::Nominal(nominals) = item else {
            return item;
        };

        Item::Nominal(nominals.with_entries(|name, held| {
            Some(match held {
                Nominal::Inductive(_) => Nominal::Inductive(self.induct_decl(name)?.clone()),
                Nominal::Struct(_) => Nominal::Struct(self.struct_decl(name)?.clone()),
                Nominal::Concept { .. } => Nominal::Concept {
                    concept: self.concept(name)?.clone(),
                    record: self.struct_decl(name)?.clone(),
                },
            })
        }))
    }

    /// Publish `item`, an item a candidate made, as its elaboration published it ([`Context::publish`]), the registry entries it holds being the ones its elaboration rebuilt, with what that elaboration concluded of each of its definitions, as `classified` holds it by name: what the declarations that read it read of it.
    pub(crate) fn publish_adopted(&mut self, item: &Item, classified: &[(Global, Classified)]) {
        let mut published = Published {
            item: item.clone(),
            induct_decls: Vec::new(),
            struct_decls: Vec::new(),
            concepts: Vec::new(),
        };
        for (name, nominal) in item.nominals() {
            published.induct_decls.extend(
                nominal
                    .inductive()
                    .cloned()
                    .map(|declaration| (*name, declaration)),
            );
            published
                .struct_decls
                .extend(nominal.record().cloned().map(|record| (*name, record)));
            published
                .concepts
                .extend(nominal.concept().cloned().map(|concept| (*name, concept)));
        }
        // An elaboration assumes each name before it publishes it, which publishing replaces: a definition, or a former as its entry makes it.
        let types = match item {
            Item::Let(definition) => vec![(definition.name, definition.type_.clone())],
            Item::Rec(rec) => rec
                .exports()
                .iter()
                .enumerate()
                .map(|(index, definition)| (definition.name, rec.group().member_type(index)))
                .collect(),
            Item::Nominal(_) => item
                .formers()
                .into_iter()
                .map(|former| (former.name, former.type_))
                .collect(),
        };
        for (name, type_) in &types {
            self.forget(name);
            self.declare(name, type_);
        }
        self.publish(&published);
        for (name, type_) in &types {
            let filed = classified
                .iter()
                .find(|(held, _)| held == name)
                .map(|(_, classified)| classified.clone())
                .unwrap_or_default();
            self.record_classified(name, type_, &filed);
        }
    }

    /// Zonk every term recorded since `mark`, the finished item's, so the erasure obligations read them once the state they were recorded in is gone.
    ///
    /// One pass across them all, the solutions standing still while it runs: the terms are each other's subterms, a deep expression's recorded once at each of its levels, and zonked apart they would cost the sum of their sizes.
    ///
    /// **A recorded term keeps its place and its binders' names.** The memo is keyed on a term, whose equality leaves both out, so what it answers is the first equal term recorded. A term holding nothing unsolved is its own answer and goes past the memo, as the zonk answers one before its own; a term the zonk rebuilds is placed again where the one asked stands, as `elaborate` places what its cache answers. So a second read of a local settles at its own place, and a type is spelled with the names its own binders were written with.
    pub(crate) fn settle_checked(&mut self, mark: usize) -> Result<(), Error> {
        let mut zonked = HashMap::<Term, Term>::new();
        let mut pass = ZonkPass::default();
        let mut settled = Vec::with_capacity(self.underway.checked.mark() - mark);
        for (term, type_, site) in &self.underway.checked.terms()[mark..] {
            let mut settle = |term: &Term| {
                if !term.has_metavar() && !term.has_universe_meta() {
                    return Ok::<_, Error>(term.clone());
                }
                match zonked.get(term) {
                    Some(done) => Ok(done.clone()),
                    None => {
                        let done = pass.zonk(self, term)?;
                        zonked.insert(term.clone(), done.clone());
                        Ok(done)
                    }
                }
            };
            let placed = match (settle(term)?, term.extent()) {
                (rebuilt, Some(extent)) if rebuilt.extent() != Some(extent) => {
                    rebuilt.with_extent(extent)
                }
                (settled, _) => settled,
            };
            settled.push((placed, settle(type_)?, *site));
        }
        self.underway.checked.replace_since(mark, settled);
        let mut labels = mem::take(&mut self.underway.checked);
        let settled = labels.restate_labels(mark, |value| match zonked.get(value) {
            Some(done) => Ok(done.clone()),
            None => pass.zonk(self, value),
        });
        self.underway.checked = labels;

        settled
    }

    /// The declaration declaring `names` has finished — kept, refused or withheld — and nothing waits on it any more: a witness among them is the table's to answer for, or no one's.
    pub(crate) fn finish(&mut self, names: &[Global]) {
        for name in names {
            self.underway.witnesses.settle(name);
        }
        self.underway.needs.finish(names);
    }

    /// Take the terms recorded since `mark`: a published declaration's, or the entry's, which proof totality reads last. The bindings recorded since go with them.
    pub(crate) fn split_checked(&mut self, mark: usize) -> Vec<(Term, Term, Option<Global>)> {
        self.underway.checked.drop_labels(mark);
        self.underway.checked.split_off(mark)
    }

    /// Elaborate the item `declaration` from here on — `None` for an entry's final term — forgetting where the previous item's locals were opened: a proof is written only while its bound's item elaborates. Its scheme is open until its universe levels are finalized.
    pub(crate) fn enter_item(&mut self, declaration: Option<Global>) {
        self.underway.credits.begin_item();
        self.underway.attempt.universe_solver.open_scheme();
        self.underway.performed = false;
        self.enter_declaration(declaration);
    }

    /// Elaborate the value of a declaration of `kind` from here on: a test's body is a function's ([`Context::sequences`]).
    pub(crate) fn enter_value(&mut self, kind: &DefinitionKind) {
        self.underway.performed = matches!(kind, DefinitionKind::Test);
    }

    /// Elaborate an entry's final term from here on: an item of no declaration, whose body is a function's, of none, the runner calls ([`Context::sequences`]).
    pub(crate) fn enter_entry(&mut self) {
        self.enter_item(None);
        self.underway.performed = true;
    }

    /// Elaborate `declaration`, a member of the item being elaborated, from here on: whose written binders the locals opened next count among.
    pub(crate) fn enter_declaration(&mut self, declaration: Option<Global>) {
        self.underway.declaration = declaration;
    }

    /// [`Credits::written_of`].
    pub(crate) fn written_of(&self, local: &Free) -> Option<WrittenBinder> {
        self.underway.credits.written_of(local)
    }

    /// [`Credits::credit`].
    pub(crate) fn credit(&mut self, proof: &Term) {
        self.underway.credits.credit(proof);
    }

    /// Where the declaration under way writes its binder at `ordinal` among its written binders, as its lowering filed the place ([`curios_core::Mints::written`]): what a refusal about the binder stands on. The binder is credited as the refusal's to speak for ([`Credits::credit_refused`]), so no lint speaks of it again.
    pub(crate) fn refused_binder(&mut self, ordinal: u32) -> Option<Extent> {
        let declaration = self.underway.declaration;
        self.underway.credits.credit_refused(declaration, ordinal);
        let mints = match self.underway.needs.attempted() {
            Some(name) => self.underway.minted.of(name),
            None => &self.underway.minted.entry,
        };
        let binder = WrittenBinder {
            declaration,
            ordinal,
        };

        mints
            .written
            .iter()
            .find(|(written, _, _)| *written == binder)
            .map(|(_, extent, _)| *extent)
    }

    /// [`Credits::credited_in`].
    pub(crate) fn credited_in(&self, names: &[Global]) -> Vec<(Global, u32)> {
        self.underway.credits.credited_in(names)
    }

    /// The written binders proofs the elaborator wrote read, each by its declaration and its place among that declaration's written binders — each used, though no written reference reaches it.
    pub fn credited(&self) -> BTreeSet<(Option<Global>, u32)> {
        self.underway.credits.credited()
    }
}
