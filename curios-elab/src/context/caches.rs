//! The elaborator's memo tables and the write stamps that police them.
//!
//! The reduction and elaboration caches are sound only under an explicit invalidation protocol: every store write that could change a cached answer must either bump a stamp (so a pending insert is refused), clear a cache, or retain selectively. Each combination is a named method carrying its own justification, so a new mutation site chooses a protocol instead of improvising one.
//!
//! **Every table lives one declaration**, and that is what lets a hit on it be free: [`Caches::begin_declaration`] clears them where the budget is restored, so which entries are present is a fact about the declaration under judgment rather than about the ones compiled before it. `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` states the rule for both checkers.
//!
//! **Within it, an answer is what asking again would give, for as long as what it read stands**, as that decision states: how long a table keeps an answer is a cost, and never what a declaration elaborates to. Under [`Caches::wholesale`] every boundary clears every table, and `curios-pipeline`'s `std_memo_census` requires `/std` to elaborate to one module both ways.
//!
//! **A reduct has one of two lives within it**, as the kernel's memos have them (`curios-cert`'s `memos` module). One whose term and reduct are both [`lasting`] reads nothing a scope holds: no equation is recorded under a spelling that names no local, reduction puts no question to conversion about a term that names none, and such a term names no local definition and no level a rewrite moves. It stands for the declaration, and only a change to a definition it may have read clears it: a redefinition, and a rollback, since a definition still being elaborated may hold a solution the rollback unwinds. Every other reduct goes at each boundary of the scope as well.
//!
//! **A bracket of the scope is no boundary of the window around it.** A settlement withholds the frames from its entry's inwards and restores them, and a judgment standing on held frames sets the live ones aside and takes them back; what the live window remembered is what asking again gives once either closes, so each sets it aside at its entry and hands it back at its exit ([`Caches::enter_window`]). What a settlement remembered is kept for the next settlement made under the same refinements, for as long as the deepest frame it read still stands and what else it rests on does (`Frames::begin_reads`, `Frame::settled_windows`, [`Footing`]).
//!
//! The *policies* — what is cacheable, and what a probe's groundness gate admits — stay on `Context`, which alone can read the solution and universe stores they consult. This type owns the storage and the write discipline.

#[cfg(any(test, feature = "test-support"))]
mod test_support;

use {
    super::{SolutionPrefix, Solutions},
    crate::{EveryPosition, Sort, UniverseSolver, UniverseStateToken},
    curios_analysis::Classification,
    curios_core::{
        AsideAnswers, Bound, ClosedMemo, Enter, Free, Global, Level, LevelHead, Quality,
        QualityMetaId, Rec, RecGroup, Sharing, Subterm, Term, UniverseError, UniverseMetaId,
        Universes, lasting, rewrite_universe_qualities, substitute_universe_metas_scoped,
    },
    curios_utilities::{Entropy, Keyed},
    std::{
        cell::RefCell,
        collections::{HashMap, HashSet},
        convert::Infallible,
        mem,
        rc::Rc,
    },
};

/// Key of one memoized `elaborate` call: the lowered term, the `Check` expected type (`None` for `Infer`), whether an island's representation-privacy checks were live, and whether reduction was plain. Validity under suppressed privacy is directional — an entry that passed strict checks would be valid under suppression, but not the reverse — so checked and suppressed runs each answer only their own partition. A run under plain reduction, which asks the elaborator's conversion nothing (`Context::plainly`), refuses what a judgment's run accepts by a fold or an equation its conversion decided, so a plain run and a judgment's each answer only their own partition as well.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ElaborationKey {
    term: Term,
    expected: Option<Term>,
    privacy_checked: bool,
    plain: bool,
}

impl ElaborationKey {
    /// The key of one `elaborate` call as its table holds it, told apart from another by its hash before either term is compared.
    fn of(term: &Term, expected: Option<&Term>, privacy_checked: bool, plain: bool) -> Keyed<Self> {
        Keyed::new(Self {
            term: term.clone(),
            expected: expected.cloned(),
            privacy_checked,
            plain,
        })
    }
}

/// Outcome of `Context::probe_elaborated` — the read half of the elaboration cache, torn out of `Context::get_or_init_elaborated`'s bracket so the iterative `elaborate` driver can probe at a frame push and record at the matching pop (the reduction cache's `cached_reduced`/`reduce` split, one level up). `Hit` carries the memoized, unplaced `(rebuilt, type)`; `Miss` carries the state snapshot the caller threads back into `Context::record_elaborated` as its purity witness; `Uncacheable` marks a term the groundness gate excludes — the caller elaborates it but records nothing.
pub enum ElabProbe {
    Hit((Term, Term)),
    Miss(ElaborationStamp),
    Uncacheable,
}

/// What a settled spelling or a settlement's kept window rests on beyond the frames it was read under, filed with it and asked later whether it all still stands (`Context::stands`). Each part is filed only where what it was taken of read what the part names, so a write reaches an answer only where the answer read what the write moves; under [`Caches::wholesale`] every part is filed.
#[derive(Debug, Clone)]
pub struct Footing {
    /// The solutions committed when it was taken: it stands while they all still are.
    solutions: SolutionPrefix,
    /// Whether a solution committed since moves it: what it was taken of held an unsolved metavariable, or a question was declined on the way.
    exact: bool,
    /// The universe solver's state, where a question put to conversion on the way was declined for a commit: it stands while that state does.
    levels: Option<UniverseStateToken>,
    /// How many times levels had been rewritten in place, where what it holds names a universe metavariable: reduction reads no level, so a rewrite moves only an answer holding one it rewrites.
    rewrites: Option<usize>,
    /// How many times answers had been withdrawn, where a question was put to conversion on the way.
    withdrawals: Option<usize>,
    /// How many times the definitions in view had changed at once: at a redefinition, and at a declaration's boundary, which nothing a budget paid for outlives.
    redefinitions: usize,
}

impl Footing {
    /// Whether everything it rests on still stands: every solution it read is committed as it was, and none since where it was taken of an unsolved metavariable; the universe solver stands where it was where a question was declined; and no rewrite, withdrawal or redefinition it reads has landed since.
    pub fn stands(
        &self,
        solutions: &Solutions,
        universes: &UniverseSolver,
        caches: &Caches,
    ) -> bool {
        solutions.holds(self.solutions, self.exact)
            && self
                .levels
                .is_none_or(|levels| levels == universes.state_token())
            && caches.counts_stand(self)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ElaborationStamp {
    terms: Entropy,
    universes: Entropy,
}

/// The reduction and elaboration caches with their two write stamps. See the module documentation for the protocol; `Context` holds exactly one of these.
#[derive(Debug, Default)]
pub struct Caches {
    /// Reducts that may read the scope, for as long as it stands as it did: every boundary of the scope clears them. [`Caches::begin_declaration`] clears every table where the budget is restored, so every node one holds was built under that budget, a hit on it is free, and nothing charges an insertion; a table that outlived the declaration would carry across an item boundary what the work after finalization reduced — the witness goals retried and the parked ones drained — and whether a declaration could afford its reductions would depend on that.
    ///
    /// **One table for each of the two reductions**, as the kernel's local-bearing memos have: the first for a judgment's, which may ask the elaborator's conversion at a stuck fold, at a missed equation and at a stuck elimination's atoms, and the second for plain reduction, which asks nothing (`Context::plainly`). A term's reduct under one is not its reduct under the other.
    reduction: [HashMap<Term, Term>; 2],
    /// The reducts that stand for the declaration: a [`lasting`] term's, where its reduct is lasting too. See the module documentation for what clears them.
    lasting: [HashMap<Term, Term>; 2],
    /// The second door into [`Caches::lasting`], as [`Caches::reduction_erased`] is into [`Caches::reduction`]: it lives with the table it indexes.
    lasting_erased: [HashMap<Term, Term>; 2],
    /// What a closed term is forced to (`reduce`'s `force_rec`), kept apart from its reduct as the kernel's memos and the closed machine's keep the two demands apart: forced, a recursive call is the value its fold computes, and plain, it is the folded call itself. Served to a plain probe, a forced value would answer a term the plain reduction leaves folded with its unfolding, as a forced reduction happened to have run first. Of the two lives as the reducts are, the scoped one first.
    forced: [HashMap<Term, Term>; 2],
    lasting_forced: [HashMap<Term, Term>; 2],
    /// The reduction table's second door, for the terms mentioning no local binder: each universe-erased spelling to the first exact key stored under it. A reduct is the same function of its term whatever the levels in it and whatever sorts its instances state, so two spellings differing only in those — two occurrences of one term, each at universe metavariables of its own, or one written with metavariables and read with them solved — are one computation; the exact table cannot see that, and every one of them re-ran the fold. A probe that misses the exact table asks here, and a hit is served through [`AcrossUniverses`], which rewrites the stored reduct's levels and sorts to the asking spelling's where the asking spelling is the stored one carried, or declines.
    reduction_erased: [HashMap<Term, Term>; 2],
    /// What the closed machine's runs have answered ([`ClosedMemo`]), one for each of the two reductions as the reducts have: each answer discarded and kept wherever a reduct of its life is.
    closed: [ClosedMemo; 2],
    /// What classifying each stuck operation found (`curios_analysis::Classification`), by the operation as reduction left it, for the stuck fold above it to extend rather than walk the operation again. One no conversion class entered reads the operation alone — its walk reduces nothing and asks nothing — so it stands for the declaration and goes with every table at [`Caches::begin_declaration`]; one the elaborator's conversion classed rests on what that question read, as a judgment's reduct does, and goes wherever the reducts go. Only a judgment's reduction takes a stuck fold again, so there is no table for plain reduction.
    classifications: HashMap<Term, Rc<Classification>>,
    classified: HashMap<Term, Rc<Classification>>,
    /// The sort of each type classified since the context was last written to — `Sort::of_in`'s own answer, remembered so that a type whose graph shares a field is classified once per node where the walk alone classifies it once per path.
    ///
    /// **Valid for a quiet stretch, and that is as long as it needs to be.** A sort is derived by reduction and reads more besides: the type a local is assumed at, what a metavariable is solved to and has for a type, a declaration's sort, the levels the universe solver holds. Each of those changes by a stamped write, by a frame's exit — which closes binders and stamps nothing — or where a reduct is cleared, so the table is emptied at the first probe after either stamp has moved, wherever a frame is left, and wherever the reducts are. One classification does none of the three: it opens its binders beside the context rather than in it, which is why `Sort::of_in` threads them, and it is the classification that is per path without the table.
    ///
    /// **Filed by the reduction that read it**, a table for each as the reducts have: a sort is read through reducts, and a term's reduct under a judgment's reduction is not its reduct under plain reduction.
    sorts: [HashMap<Term, Sort>; 2],
    /// What two terms were answered to be, asked with nothing committed (`same_uncommitted`), for the quiet stretch the sorts stand for and filed as they are, by the reduction that asked: a verdict is read through reducts, and reads the types locals are assumed at, the solutions, the levels and the equations in force, as a sort does. It is cleared wherever the sorts are, since nothing clears one and may leave the other.
    ///
    /// **Only a question that wrote nothing is filed** ([`Caches::same_insert`]'s caller holds it to that), so answering one from here leaves the context as asking it again would: no name is minted that the comparison would have minted.
    same: [HashMap<(Term, Term), bool>; 2],
    /// The stamps `sorts` and `same` were filled under.
    sorts_at: ElaborationStamp,
    /// Each instance of a universe-polymorphic `rec` group reduction has opened in the declaration in progress, beside the group it instantiates and the levels it was opened at. An instance holds its levels substituted through every member, so the pair is what says which instance of which group a copy is without reading the levels back off it. It lives one declaration, as the levels' metavariables do.
    instances: HashMap<Keyed<RecGroup>, (RecGroup, Universes)>,
    /// Each top-level `rec` group reduction has opened by unfolding a name in the declaration in progress, beside the first name it was opened from: what a term the declaration commits names a stuck call on it by ([`named_members`](crate::named_members)). It lives one declaration, as the instances do. Behind a cell because a name is unfolded through a shared reference.
    opened: RefCell<HashMap<Keyed<RecGroup>, Global>>,
    elaboration: HashMap<Keyed<ElaborationKey>, (Term, Term)>,
    /// Elaborations inside an oracle bracket (`Context::with_oracle`) that the table above must refuse, one table per live bracket, innermost last: see `Context::get_or_init_elaborated` for what they admit and why. Cleared wherever the table above is, and discarded with their bracket.
    oracle: Vec<HashMap<Keyed<ElaborationKey>, (Term, Term)>>,
    /// Whether a binder is itself a proof — assumed at a proposition — for each binder a stuck form naming it has put to the entries in scope (`reduce`'s `proofs_named`). A binder's identity is minted once, so an entry is never another binder's, and the table goes with the declaration.
    proofs: HashMap<Free, bool>,
    /// One tick per *write* to any kernel store — definitions, refinements, assumptions, name/metavariable minting, solves, parked/deferred work, the witness table. `Context::get_or_init_elaborated` snapshots it around a candidate sub-elaboration: an unchanged stamp certifies the run was pure (replaying it would be the identity on the context), which is what makes skipping the replay on a later cache hit sound.
    mutation_stamp: Entropy,
    /// A recorded scrutinee against itself as the kernel spells it — solved metavariables materialized, local definitions substituted, refined variables as their values — kept where no metavariable in it is left unsolved: what the filter in front of an entry reads (`reduce`'s `could_settle`), which would otherwise rebuild the scrutinee at every stuck form. It rests on the solutions, the definitions and the refinements in the window, so it goes wherever the reducts do, and a bracket sets it aside with them ([`Caches::enter_window`]). Behind a cell because the filter runs while the entries are borrowed.
    kernel_spellings: RefCell<HashMap<Term, Term>>,
    /// Every name a lookup found no definition for, and not defined since: what a remembered reduct can be stuck on — in a table of either life, a window set aside or one a frame keeps — and so the names a fresh definition can unstick one of ([`Caches::retain_reductions_without`]). Kept across every clear of the tables, since a window outlives one; a name leaves it where it is defined. A name minted for a binding is defined before anything reduces under it, so its definition reads no reduct. Behind a cell because a definition is looked up through a shared reference.
    absent: RefCell<HashSet<Free>>,
    /// Monotonic universe-solver writes are tracked separately. Elaboration entries may survive them only when their keys and results contain no transitively unresolved universe meta; reducts survive them outright, being parametric in levels (see `Context::cached_reduced`); rollback/finalization clears every cache at the non-monotonic boundaries.
    universe_mutation_stamp: Entropy,
    /// Whether every boundary clears every table, as each did before an answer was kept for as long as what it read stands ([`Caches::wholesale`]).
    wholesale: bool,
    /// The windows of the scope the brackets in progress set aside, innermost last ([`Caches::enter_window`]).
    windows: Vec<Window>,
    /// How many times levels have been rewritten in place ([`Caches::invalidate_for_universe_rewrite`]), answers withdrawn with the level constraints they were given on ([`Caches::invalidate_for_withdrawn_answers`]), and the definitions in view changed at once ([`Caches::note_redefinition`]): what a [`Footing`] reads, each where what it was taken of read what the count counts.
    rewrites: usize,
    withdrawals: usize,
    redefinitions: usize,
    /// The canonical node of each lasting key the reducts and the sorts are filed and asked by, for the declaration ([`Caches::key`]).
    keys: Sharing,
}

/// A bracket of the scope: a span in which the frames in view are not the ones around it, after which they are again.
#[derive(Debug, Clone, Copy)]
pub enum Bracket {
    /// A settlement, made by plain reduction where `plain`: the frames from its entry inwards are withheld and restored, and nothing but its reduction runs between.
    Settlement { plain: bool },
    /// A judgment standing on held frames: the live frames are set aside and the held ones stand in their place.
    Stand,
}

impl Bracket {
    /// The reductions whose tables the bracket reaches: plain reduction's alone inside a span of it, since nothing else runs there, and both otherwise.
    fn reductions(self) -> &'static [usize] {
        match self {
            Bracket::Settlement { plain: true } => &[1],
            Bracket::Settlement { plain: false } | Bracket::Stand => &[0, 1],
        }
    }

    /// Whether the sorts and the verdicts are handed back with the reducts. A settlement runs reduction alone, which writes nothing a sort or a verdict reads; a stand runs a judgment, which may.
    fn keeps_judgments(self) -> bool {
        matches!(self, Bracket::Settlement { .. })
    }
}

/// What a window of the scope remembered, of the reductions a bracket reaches: the live window's, set aside while the bracket runs, or a settlement's own, which the frame of the entry it settles keeps for the next settlement made there ([`Caches::leave_window`]).
#[derive(Debug)]
pub struct Window {
    bracket: Bracket,
    scoped: [Scoped; 2],
    kernel_spellings: HashMap<Term, Term>,
    /// The classifications the elaborator's conversion classed, a judgment's alone, set aside where the bracket reaches a judgment's reduction.
    classified: HashMap<Term, Rc<Classification>>,
    /// The stamps its sorts and verdicts were judged under.
    judged_at: ElaborationStamp,
}

impl Window {
    /// What [`Caches::retain_reductions_without`] keeps of the live window, kept of this one.
    pub fn retain_without(&mut self, name: &Free) {
        for scoped in &mut self.scoped {
            scoped.retain_without(name);
        }
    }
}

/// One reduction's scoped answers.
#[derive(Debug, Default)]
struct Scoped {
    reduction: HashMap<Term, Term>,
    reduction_erased: HashMap<Term, Term>,
    forced: HashMap<Term, Term>,
    closed: AsideAnswers,
    sorts: HashMap<Term, Sort>,
    same: HashMap<(Term, Term), bool>,
}

impl Scoped {
    fn clear(&mut self) {
        self.reduction.clear();
        self.reduction_erased.clear();
        self.forced.clear();
        self.closed.clear();
        self.sorts.clear();
        self.same.clear();
    }

    /// What [`Caches::retain_reductions_without`] keeps of the live window, kept of this one.
    fn retain_without(&mut self, name: &Free) {
        self.reduction
            .retain(|_, reduct| !reduct.mentions_free(name));
        let reduction = &self.reduction;
        self.reduction_erased
            .retain(|_, key| reduction.contains_key(key));
        self.forced.retain(|_, value| !value.mentions_free(name));
        self.closed.retain_without(name);
        self.sorts.clear();
        self.same.clear();
    }
}

impl Caches {
    pub fn new() -> Self {
        Self::default()
    }

    /// The definitions in view changed at once: what a settled spelling or a kept window read of them may have, and nothing names what.
    pub fn note_redefinition(&mut self) {
        self.redefinitions += 1;
    }

    /// The footing of an answer taken now, under the journal of solutions as `solutions` names it ([`Footing`]): `exact` where a solution committed since moves it, `levels` the universe solver's state where a question was declined on the way, `universes` where it holds a universe metavariable, `asked` where a question was put to conversion on the way.
    pub fn footing(
        &self,
        solutions: SolutionPrefix,
        exact: bool,
        levels: Option<UniverseStateToken>,
        universes: bool,
        asked: bool,
    ) -> Footing {
        Footing {
            solutions,
            exact: exact || self.wholesale,
            levels,
            rewrites: (universes || self.wholesale).then_some(self.rewrites),
            withdrawals: (asked || self.wholesale).then_some(self.withdrawals),
            redefinitions: self.redefinitions,
        }
    }

    /// Whether no rewrite, withdrawal or redefinition `footing` reads has landed since it was taken.
    pub fn counts_stand(&self, footing: &Footing) -> bool {
        footing
            .rewrites
            .is_none_or(|rewrites| rewrites == self.rewrites)
            && footing
                .withdrawals
                .is_none_or(|withdrawals| withdrawals == self.withdrawals)
            && footing.redefinitions == self.redefinitions
    }

    /// Record a write to a kernel store, poisoning any elaboration-cache insert whose computation spans it.
    pub fn note_write(&mut self) {
        self.mutation_stamp.fresh();
    }

    /// Record a universe-solver write the guard could not see (seeding); `universe_stamp` covers the guarded path.
    pub fn note_universe_write(&mut self) {
        self.universe_mutation_stamp.fresh();
    }

    /// The universe stamp itself, for `UniverseMutation`'s drop guard: solver mutations bump it only when solver state actually changed, so read-equivalent solver calls stay cache-pure.
    pub fn universe_stamp(&self) -> &Entropy {
        &self.universe_mutation_stamp
    }

    /// Snapshot both stamps — a `Miss`'s purity witness.
    pub fn stamps(&self) -> ElaborationStamp {
        ElaborationStamp {
            terms: self.mutation_stamp.clone(),
            universes: self.universe_mutation_stamp.clone(),
        }
    }

    /// Whether nothing has been written since `stamps` was taken — the purity half of the elaboration cache's insert gate.
    pub fn stamps_unchanged(&self, stamp: &ElaborationStamp) -> bool {
        self.mutation_stamp == stamp.terms && self.universe_mutation_stamp == stamp.universes
    }

    /// The remembered reduct of `term`, as plain reduction took it where `plain` and as a judgment's did otherwise.
    ///
    /// A reduct served through the second door holds the instances reduction opened for the stored spelling, each carried to the asking spelling's levels. They are the instances reduction would have opened for the asking one, and are opened as those ([`Caches::opened_instance`]): a term that names a group's members again knows an instance by that record alone, so one served unrecorded would stand in a solution as a copy of its group. A hit whose instances do not open is declined, as one whose levels do not map is.
    ///
    /// A spelling holding a term metavariable, and one whose stored reduct holds one, is answered by the exact table alone: a solution can hold a universe metavariable the key holds too, which carrying would move in the key and not in the solution ([`AcrossUniverses`]).
    pub fn reduction_get(&mut self, term: &Term, plain: bool) -> Option<Term> {
        let plain = usize::from(plain);
        let key = self.key(term);
        if let Some(reduct) = self.lasting[plain]
            .get(&key)
            .or_else(|| self.reduction[plain].get(&key))
        {
            return Some(reduct.clone());
        }
        if term.has_local_free() {
            return None;
        }
        let erased = term.erased_universes();
        let stored = |door: &HashMap<Term, Term>, table: &HashMap<Term, Term>| {
            let key = door.get(&erased)?;
            Some((key.clone(), table.get(key)?.clone()))
        };
        let (key, reduct) = stored(&self.reduction_erased[plain], &self.reduction[plain])
            .or_else(|| stored(&self.lasting_erased[plain], &self.lasting[plain]))?;
        // An entry recording that a term is its own weak-head form says the same of every spelling the door equates with it, and the asking spelling is the one to hand back: the stored one was written elsewhere, and a Π-type served in its place carried that spelling's binder names into a report about this one.
        if reduct == key {
            curios_profile::sample!("reduction::across_levels", 1);
            return Some(term.clone());
        }
        let adapted = (!term.has_metavar() && !reduct.has_metavar())
            .then(|| AcrossUniverses::between(&key, term))
            .flatten()
            .and_then(|across| Some((across.carried(&reduct)?, across)));
        curios_profile::sample!("reduction::across_levels", u64::from(adapted.is_some()));
        let (adapted, across) = adapted?;
        for (group, universes) in self.instances_carried(&reduct, &across)? {
            self.opened_instance(&group, &universes).ok()?;
        }

        Some(adapted)
    }

    /// Each instance `reduct` holds that reduction opened, as the group it instantiates and the universes it stands at carried across `across`: the instances the asking spelling's reduct holds in their place. `None` where the universes of one do not carry.
    fn instances_carried(
        &self,
        reduct: &Term,
        across: &AcrossUniverses,
    ) -> Option<Vec<(RecGroup, Universes)>> {
        if !reduct.mentions_rec() {
            return Some(Vec::new());
        }
        // A node is entered once, and an instance not at all: its members name one another by position.
        let mut found = (HashSet::new(), Some(Vec::new()));
        reduct.walk(
            &mut found,
            |(seen, carried), node| {
                if !seen.insert(node.clone()) {
                    return Enter::Skip(());
                }
                let Subterm::Rec(Rec { group, .. }) = &**node else {
                    return Enter::Descend;
                };
                let Some((scheme, universes)) = self.instances.get(&Keyed::new(group.clone()))
                else {
                    return Enter::Descend;
                };
                match (carried.as_mut(), across.carried_universes(universes)) {
                    (Some(carried), Some(universes)) => carried.push((scheme.clone(), universes)),
                    _ => *carried = None,
                }
                Enter::Skip(())
            },
            |_, _, _| (),
        );

        found.1
    }

    /// The closed machine's memo for the reduction `plain` names.
    pub fn closed_memo(&mut self, plain: bool) -> &mut ClosedMemo {
        &mut self.closed[usize::from(plain)]
    }

    pub fn reduction_insert(&mut self, term: Term, reduct: Term, plain: bool) {
        let plain = usize::from(plain);
        let term = self.key(&term);
        let (table, door) = match lasting(&term) && lasting(&reduct) {
            true => (&mut self.lasting[plain], &mut self.lasting_erased[plain]),
            false => (
                &mut self.reduction[plain],
                &mut self.reduction_erased[plain],
            ),
        };
        if !term.has_local_free() {
            door.entry(term.erased_universes())
                .or_insert_with(|| term.clone());
        }
        table.insert(term, reduct);
    }

    /// What `term` was forced to, as plain reduction forced it where `plain` and as a judgment's did otherwise ([`Caches::forced`]).
    pub fn forced_get(&self, term: &Term, plain: bool) -> Option<Term> {
        let plain = usize::from(plain);
        let key = self.key(term);

        self.lasting_forced[plain]
            .get(&key)
            .or_else(|| self.forced[plain].get(&key))
            .cloned()
    }

    pub fn forced_insert(&mut self, term: Term, value: Term, plain: bool) {
        let plain = usize::from(plain);
        let term = self.key(&term);
        let table = match lasting(&term) && lasting(&value) {
            true => &mut self.lasting_forced[plain],
            false => &mut self.forced[plain],
        };
        table.insert(term, value);
    }

    /// What classifying the stuck operation `term` found, where it is remembered.
    pub fn classification(&self, term: &Term) -> Option<Rc<Classification>> {
        self.classifications
            .get(term)
            .or_else(|| self.classified.get(term))
            .cloned()
    }

    /// Remember what classifying the stuck operation `term` found, for as long as what it read stands: stored for nothing, as a reduct is.
    pub fn classification_insert(&mut self, term: Term, classification: Classification) {
        let table = match classification.classed() {
            true => &mut self.classified,
            false => &mut self.classifications,
        };
        table.insert(term, Rc::new(classification));
    }

    /// Every reduct and forced value of both reductions and both lives, the second doors indexing the reducts, the classifications the elaborator's conversion classed, the sorts read through them, and the scrutinees remembered as the kernel spells them.
    fn clear_reductions(&mut self) {
        self.clear_lasting();
        self.clear_scope();
    }

    /// What a boundary of the scope can reach: [`Caches::clear_scope`]. The lasting reducts stay, but under [`Caches::wholesale`].
    fn clear_scoped_reductions(&mut self) {
        if self.wholesale {
            self.clear_lasting();
        }
        self.clear_scope();
    }

    /// The reducts and forced values that stand for the declaration, their second doors, and every closed run's answer.
    fn clear_lasting(&mut self) {
        let tables = self
            .lasting
            .iter_mut()
            .chain(&mut self.lasting_erased)
            .chain(&mut self.lasting_forced);
        for table in tables {
            table.clear();
        }
        for closed in &mut self.closed {
            closed.clear();
        }
    }

    /// The reducts and forced values that may read the scope, with the reducts' second doors and the closed runs' answers of that life, the classifications the elaborator's conversion classed, the sorts, and the scrutinees remembered as the kernel spells them.
    fn clear_scope(&mut self) {
        let tables = self
            .reduction
            .iter_mut()
            .chain(&mut self.reduction_erased)
            .chain(&mut self.forced);
        for table in tables {
            table.clear();
        }
        for closed in &mut self.closed {
            closed.clear_scoped();
        }
        self.classified.clear();
        self.clear_sorts();
        self.kernel_spellings.get_mut().clear();
    }

    /// A lookup found no definition for `name`: a reduct remembered from here on may be stuck on its absence.
    pub fn note_absent(&self, name: &Free) {
        self.absent.borrow_mut().insert(*name);
    }

    /// Every remembered sort and every verdict filed beside them, of both reductions.
    fn clear_sorts(&mut self) {
        for sorts in &mut self.sorts {
            sorts.clear();
        }
        for same in &mut self.same {
            same.clear();
        }
    }

    /// The remembered sort of `type_`, where nothing was written since it was filed: as plain reduction read it where `plain`, and as a judgment's did otherwise.
    pub fn sort_get(&mut self, type_: &Term, plain: bool) -> Option<Sort> {
        self.settle_sorts();

        self.sorts[usize::from(plain)]
            .get(&self.key(type_))
            .cloned()
    }

    pub fn sort_insert(&mut self, type_: Term, plain: bool, sort: Sort) {
        self.settle_sorts();
        let type_ = self.key(&type_);
        self.sorts[usize::from(plain)].insert(type_, sort);
    }

    /// `term` as the reducts and the sorts are filed and asked by: its canonical node where it is [`lasting`], and itself otherwise.
    ///
    /// **A lasting term is one node however often it is built.** A type written once and checked against at each level of a value is built again at each, and two equal terms in two allocations are compared node by node at every probe that finds one, so a nesting asks its depth at each of its levels. A key that names a local or a metavariable is the scope's, and stands as it is: a metavariable's spine is as wide as the scope it was born in, which consing it would walk.
    fn key(&self, term: &Term) -> Term {
        match lasting(term) {
            true => self.keys.share(term),
            false => term.clone(),
        }
    }

    /// What `this` and `that` were answered to be, in either order, where nothing was written since: as plain reduction asked where `plain`, and as a judgment's did otherwise.
    pub fn same_get(&mut self, this: &Term, that: &Term, plain: bool) -> Option<bool> {
        self.settle_sorts();

        let same = &self.same[usize::from(plain)];
        if same.is_empty() {
            return None;
        }
        same.get(&(this.clone(), that.clone()))
            .or_else(|| same.get(&(that.clone(), this.clone())))
            .copied()
    }

    pub fn same_insert(&mut self, this: Term, that: Term, plain: bool, same: bool) {
        self.settle_sorts();
        self.same[usize::from(plain)].insert((this, that), same);
    }

    /// Empty the sorts and the verdicts where either stamp has moved since they were filed, and file what follows under the stamps as they stand.
    fn settle_sorts(&mut self) {
        if !self.stamps_unchanged(&self.sorts_at) {
            self.clear_sorts();
            self.sorts_at = self.stamps();
        }
    }

    /// `term` as the kernel spells it, where that has been remembered.
    pub fn kernel_spelling_get(&self, term: &Term) -> Option<Term> {
        self.kernel_spellings.borrow().get(term).cloned()
    }

    pub fn kernel_spelling_insert(&self, term: Term, spelled: Term) {
        self.kernel_spellings.borrow_mut().insert(term, spelled);
    }

    /// Whether the binder `name` is a proof, where that has been asked.
    pub fn proof_get(&self, name: &Free) -> Option<bool> {
        self.proofs.get(name).copied()
    }

    pub fn proof_insert(&mut self, name: Free, proof: bool) {
        self.proofs.insert(name, proof);
    }

    /// A new declaration: every table is discarded — the reducts and the elaborations — so that what one declaration can afford is decided by nothing the declarations before it left behind.
    pub fn begin_declaration(&mut self) {
        self.keys = Sharing::new();
        self.note_redefinition();
        self.proofs.clear();
        self.instances.clear();
        self.opened.get_mut().clear();
        self.classifications.clear();
        self.clear_reductions();
        self.clear_elaborations();
        // A bracket in progress hands back what its window held under the state replaced, which answers for nothing here.
        for window in &mut self.windows {
            for scoped in &mut window.scoped {
                scoped.clear();
            }
            window.kernel_spellings.clear();
        }
    }

    /// `group` at `universes`, remembered as that instance of it: the one way an instance is opened, so that every one a term comes to hold is known for the instance it is.
    pub fn opened_instance(
        &mut self,
        group: &RecGroup,
        universes: &Universes,
    ) -> Result<RecGroup, UniverseError> {
        let instance = group.instantiate_universes(universes)?;
        self.instances.insert(
            Keyed::new(instance.clone()),
            (group.clone(), universes.clone()),
        );

        Ok(instance)
    }

    /// `group` opened by unfolding `name`, kept where no name opened it before in the declaration in progress.
    pub fn note_opened(&self, group: &RecGroup, name: Global) {
        self.opened
            .borrow_mut()
            .entry(Keyed::new(group.clone()))
            .or_insert(name);
    }

    /// The name reduction first opened `group` from in the declaration in progress.
    pub fn opened_from(&self, group: &RecGroup) -> Option<Global> {
        self.opened
            .borrow()
            .get(&Keyed::new(group.clone()))
            .copied()
    }

    /// The group `instance` was opened from and the instance it was opened at, where reduction opened it in the declaration in progress.
    pub fn instance_get(&self, instance: &RecGroup) -> Option<&(RecGroup, Universes)> {
        self.instances.get(&Keyed::new(instance.clone()))
    }

    pub fn elaboration_get(
        &self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
        plain: bool,
    ) -> Option<(Term, Term)> {
        self.elaboration
            .get(&ElaborationKey::of(term, expected, privacy_checked, plain))
            .cloned()
    }

    pub fn elaboration_insert(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
        plain: bool,
        result: &(Term, Term),
    ) {
        self.elaboration.insert(
            ElaborationKey::of(term, expected, privacy_checked, plain),
            result.clone(),
        );
    }

    /// Open a table for an oracle bracket being entered.
    pub fn begin_oracle(&mut self) {
        self.oracle.push(HashMap::new());
    }

    /// Discard the table of the oracle bracket being left.
    pub fn end_oracle(&mut self) {
        self.oracle.pop();
    }

    /// Whether an oracle bracket is live, so an elaboration the table above refuses may be remembered for it.
    pub fn in_oracle(&self) -> bool {
        !self.oracle.is_empty()
    }

    pub fn oracle_get(
        &self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
        plain: bool,
    ) -> Option<(Term, Term)> {
        self.oracle
            .last()?
            .get(&ElaborationKey::of(term, expected, privacy_checked, plain))
            .cloned()
    }

    pub fn oracle_insert(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
        plain: bool,
        result: &(Term, Term),
    ) {
        if let Some(table) = self.oracle.last_mut() {
            table.insert(
                ElaborationKey::of(term, expected, privacy_checked, plain),
                result.clone(),
            );
        }
    }

    /// Every remembered elaboration: the table that outlives a bracket and every live bracket's. An oracle's entries rest on everything the table above's do and on more — they may carry level metavariables and certify no purity — so every protocol below that clears the one clears the others through this.
    fn clear_elaborations(&mut self) {
        self.elaboration.clear();
        for table in &mut self.oracle {
            table.clear();
        }
    }

    /// A counterfactual refinement was registered: a refinement key can be a `#`-free stuck application of globals, so it can have influenced any entry that may read the scope — the scoped reducts and the elaborations clear wholesale, and the write is stamped. A lasting reduct stays: no equation answers a term that names no local.
    pub fn invalidate_for_refinement(&mut self) {
        self.note_write();
        self.clear_scoped_reductions();
        self.clear_elaborations();
    }

    /// A name was *re*defined (or an assumption's universe scheme rewritten in place): the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the name, leaving nothing for a selective retain to key on — both caches clear wholesale, and the write is stamped.
    pub fn invalidate_for_redefinition(&mut self) {
        self.note_redefinition();
        self.note_write();
        self.clear_reductions();
        self.clear_elaborations();
    }

    /// A name was *freshly* defined. A fresh definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF — so the reduction cache retains every entry whose result does not mention it instead of clearing. A name no lookup found absent unstuck nothing, and no reduct is read for it ([`Caches::note_absent`]): the definitions a declaration mints cost what they define, not what was remembered before them. The elaboration cache survives untouched: its insert gate already refused every entry naming a not-yet-defined global. No stamp — definition is the one ambient fact a pure run may read, and the settled-globals gate covers it.
    pub fn retain_reductions_without(&mut self, name: &Free) {
        if self.absent.get_mut().remove(name) {
            let lives = self
                .reduction
                .iter_mut()
                .zip(&mut self.reduction_erased)
                .chain(self.lasting.iter_mut().zip(&mut self.lasting_erased));
            for (reduction, erased) in lives {
                reduction.retain(|_, reduct| !reduct.mentions_free(name));
                // The second door indexes exact keys, so it keeps exactly the ones the exact table kept.
                erased.retain(|_, key| reduction.contains_key(key));
            }
            for forced in self.forced.iter_mut().chain(&mut self.lasting_forced) {
                forced.retain(|_, value| !value.mentions_free(name));
            }
            // A window set aside is handed back after the definition, which may outlive the bracket.
            for window in &mut self.windows {
                for scoped in &mut window.scoped {
                    scoped.retain_without(name);
                }
            }
            for closed in &mut self.closed {
                closed.retain_without(name);
            }
        }
        // A sort is read through reducts and names none, so it has nothing to be retained by.
        self.clear_sorts();
    }

    /// An assumption's type was replaced in place (`reassume`): an entry elaborated between a `rec` group's lowered `assume` and this upgrade could embed the lowered signature, so the elaboration cache clears; reducts never read assumption types, so the reduction cache survives. Stamped.
    pub fn invalidate_for_reassumption(&mut self) {
        self.note_write();
        self.clear_elaborations();
    }

    /// A local frame was dropped. A dropped refinement can have influenced any entry that may read the scope, so the scoped reducts and the elaborations clear; a dropped frame *definition* clears only the scoped reducts — `reduce_let` defines under written binder labels a reduct can fold in, while elaboration-position terms name only `/`-qualified globals and `#`-minted locals, so no elaboration entry can reference a written frame label. A lasting reduct names no local, so neither reaches it. No stamp: the frame's own writes were stamped when they landed.
    pub fn invalidate_frame_exit(&mut self, dropped_refinements: bool, dropped_definitions: bool) {
        // The frame's binders go with it, and a sort is read off a binder's type: the write that opened one was stamped, and nothing stamps its closing.
        self.clear_sorts();
        if dropped_refinements {
            self.clear_scoped_reductions();
            self.clear_elaborations();
        } else if dropped_definitions {
            self.clear_scoped_reductions();
        }
    }

    /// A bracket of the scope begins: the live window's scoped answers, of the reductions `bracket` reaches, are set aside, and the bracket starts from `kept`, what the last settlement under its window remembered — its sorts and verdicts only where nothing has been written since they were judged — or from none, as a boundary would leave it. What the live window remembered is what asking again gives once the bracket closes — a settlement commits nothing and files a stuck form only once every entry it could match has been settled, and a stand writes nothing to the frames it sets aside — so [`Caches::leave_window`] hands it back. What a bracket clears inside it is its own: a rollback there unwinds what it committed, a rewrite of levels pins the levels it minted, and a redefinition rebinds a label of its frame. Under [`Caches::wholesale`] each side of a bracket is a boundary that clears, and nothing is kept.
    pub fn enter_window(&mut self, bracket: Bracket, kept: Option<Window>) {
        if self.wholesale {
            self.clear_reductions();
            return;
        }
        // A sort or a verdict set aside answers again once the bracket closes, so it is one that answers now.
        self.settle_sorts();
        let live = self.take_window(bracket);
        self.windows.push(live);
        if let Some(mut kept) = kept {
            // A sort or a verdict kept answers only where nothing was written since it was judged: a binder opened or a solution committed since may move what it read.
            if !self.stamps_unchanged(&kept.judged_at) {
                for scoped in &mut kept.scoped {
                    scoped.sorts.clear();
                    scoped.same.clear();
                }
            }
            self.install_window(kept);
        }
    }

    /// The bracket [`Caches::enter_window`] began closes, and the window it set aside is handed back. What a settlement remembered is handed to its caller, to keep under the refinements it read for as long as what it read stands (`Frames::keep_settled_window`). A stand's goes. A settlement's sorts and verdicts come back with its reducts and answer under the stamps as they stand, since reduction alone ran in between; a stand's go.
    pub fn leave_window(&mut self) -> Option<Window> {
        if self.wholesale {
            self.clear_reductions();
            return None;
        }
        let live = self.windows.pop().expect("a bracket left was entered");
        let bracket = live.bracket;
        let own = self.take_window(bracket);
        self.install_window(live);
        match bracket.keeps_judgments() {
            true => self.sorts_at = self.stamps(),
            false => {
                for &index in bracket.reductions() {
                    self.sorts[index].clear();
                    self.same[index].clear();
                }
            }
        }

        matches!(bracket, Bracket::Settlement { .. }).then_some(own)
    }

    /// The scoped tables of the reductions `bracket` reaches, taken out and none left in their place.
    fn take_window(&mut self, bracket: Bracket) -> Window {
        let mut window = Window {
            bracket,
            scoped: Default::default(),
            kernel_spellings: mem::take(self.kernel_spellings.get_mut()),
            classified: HashMap::new(),
            judged_at: self.sorts_at.clone(),
        };
        for &index in bracket.reductions() {
            let scoped = &mut window.scoped[index];
            scoped.reduction = mem::take(&mut self.reduction[index]);
            scoped.reduction_erased = mem::take(&mut self.reduction_erased[index]);
            scoped.forced = mem::take(&mut self.forced[index]);
            scoped.closed = self.closed[index].set_aside();
            scoped.sorts = mem::take(&mut self.sorts[index]);
            scoped.same = mem::take(&mut self.same[index]);
        }
        if bracket.reductions().contains(&0) {
            window.classified = mem::take(&mut self.classified);
        }

        window
    }

    /// `window`'s tables in place of the scoped tables of the reductions its bracket reaches.
    fn install_window(&mut self, mut window: Window) {
        for &index in window.bracket.reductions() {
            let scoped = mem::take(&mut window.scoped[index]);
            self.reduction[index] = scoped.reduction;
            self.reduction_erased[index] = scoped.reduction_erased;
            self.forced[index] = scoped.forced;
            self.closed[index].hand_back(scoped.closed);
            self.sorts[index] = scoped.sorts;
            self.same[index] = scoped.same;
        }
        *self.kernel_spellings.get_mut() = window.kernel_spellings;
        if window.bracket.reductions().contains(&0) {
            self.classified = window.classified;
        }
    }

    /// A judgment stands on held frames, or leaves them, across a refinement in either set of frames or a definition they state otherwise: an elaboration taken on one side must not answer on the other, so the elaborations clear. The reducts go into the window the stand sets aside ([`Caches::enter_window`]).
    pub fn invalidate_for_stand(&mut self) {
        self.clear_elaborations();
    }

    /// Universe levels were rewritten in place (defaulting, finalization, instance closure): cached reducts and elaborations may embed the pre-rewrite levels, so the scoped reducts and the elaborations clear. A lasting reduct names no universe metavariable, so no rewrite reaches it. The solver write itself is stamped by the `UniverseMutation` guard.
    pub fn invalidate_for_universe_rewrite(&mut self) {
        self.rewrites += 1;
        self.clear_scoped_reductions();
        self.clear_elaborations();
    }

    /// Solutions were rolled back — the one *un*-monotonic store transition. Reducts may have been cached through the unwound solutions, a lasting one through a definition still being elaborated among them, so both caches clear whole and both stamps tick.
    pub fn invalidate_for_rollback(&mut self) {
        // How many reducts the clear throws away.
        curios_profile::sample!(
            "caches::rollback_dropped",
            self.lasting
                .iter()
                .chain(&self.reduction)
                .map(HashMap::len)
                .sum::<usize>() as u64
        );
        self.note_write();
        self.note_universe_write();
        self.clear_reductions();
        // Entries are metavar-free on both key and value, so an un-solve cannot invalidate them in principle; cleared anyway while the rollback bracket is young — conservative and cheap.
        self.clear_elaborations();
    }

    /// The elaboration island changed (a new top-level item): representation-privacy checks are island-relative, so an entry elaborated under one item's island must not answer for another's. Reducts are island-independent and survive.
    pub fn invalidate_for_island_change(&mut self) {
        self.clear_elaborations();
    }

    /// Universe constraints were discarded at a transaction boundary with actual solver-state change: elaboration entries may have certified purity against constraints that no longer exist.
    pub fn invalidate_for_universe_transaction(&mut self) {
        self.clear_elaborations();
    }

    /// Level constraints were withdrawn from a scope in which a judgment's reduction put a question to conversion: a reduct taken there may rest on an answer those constraints gave, so the scoped reducts go. A lasting one asked nothing: reduction puts no question to conversion about a term that names no local. No stamp: the withdrawal itself was stamped.
    pub fn invalidate_for_withdrawn_answers(&mut self) {
        self.withdrawals += 1;
        // How many reducts the clear throws away.
        curios_profile::sample!(
            "caches::withdrawn_dropped",
            (self.reduction[0].len() + self.reduction[1].len()) as u64
        );
        self.clear_scoped_reductions();
    }
}

/// How a spelling stored in the reduction table is carried to another that shares its universe-erased projection: each universe metavariable of the stored spelling to the level the asking one states in its place, and each sort metavariable to the sort it states, so that the stored spelling carried is the asking one. A reduct stored under the one is the answer for the other once carried ([`Self::carried`]).
///
/// **A matching, sound by level-parametricity.** No reduction rule reads a level — `Type u` is a payload, never a scrutinee — so a reduct is a function of its term's spelling, every universe metavariable in it a variable of the match whether the solver has solved it or not, and the stored reduct carried is the reduct of the stored spelling carried. What else a reduction reads stays out of the reduct's levels: a definition is closed and instantiated at the key's own levels, an answer conversion gave from a level constraint leaves the reduction remembering nothing (`reduce`'s `remember`), an instance the reduct holds is opened again at the carried levels ([`Caches::reduction_get`]), and a spelling holding a term metavariable, whose solution can hold a universe metavariable the key holds, is kept from the door.
///
/// **Proposed by the walk over the pair, accepted by the contract.** The substitution is read off the pairs the two spellings differ in, by the walk conversion reads levels by ([`Term::level_differences`]) under [`EveryPosition`]: a pair whose stored side is no bare metavariable, a pair under a universe binder, which is its group's to read, and a metavariable sent to two levels each decline ([`Self::between`]). The hit is served only where the stored spelling carried *is* the asking one, which holds what the walk does not report: a metavariable the asking spelling keeps where the substitution would move it, and a level kept as it stands that names a metavariable the substitution moves. Two spellings apart in more than their universes, a recursive group that binds its own levels against one that binds none among them, the walk refuses.
///
/// **Carried as a solution is.** A level a metavariable is sent to is stated at universe-binder depth zero, and is shifted past the binders above each place it lands at by the walk the solver's zonk runs ([`substitute_universe_metas_scoped`]).
///
/// **A sort an instance states is a payload as a level is.** The erased projection drops an instance whole, the sorts it states with its levels, so two spellings it equates may state different sorts: an occurrence of a family at `Prop` beside one at `Type`, or two whose sorts are metavariables of their own. No reduction rule reads one, and the reduct holds them, so they are mapped as the levels are: a stored metavariable to the one sort the asking spelling states in its place, and a stored `Prop`, `Type` or parameter to nothing but itself. Served with the stored sorts, the second occurrence would be read at the first one's.
struct AcrossUniverses {
    levels: Rc<HashMap<UniverseMetaId, Level>>,
    sorts: Rc<HashMap<QualityMetaId, Quality>>,
}

impl AcrossUniverses {
    /// From `key`, a stored spelling, to `query`, which shares its universe-erased projection: the substitution read off the pairs the two differ in, where it carries the one to the other.
    fn between(key: &Term, query: &Term) -> Option<Self> {
        // A spelling with no universe data has nothing to carry.
        if !key.has_universe_data() || !query.has_universe_data() {
            return None;
        }
        let differences = key.level_differences(query, |_| false, &EveryPosition)?;

        let mut levels: HashMap<UniverseMetaId, Level> = HashMap::new();
        for (depth, stored, asked) in differences.compared {
            if depth > 0 {
                return None;
            }
            match levels.insert(bare_meta(&stored)?, asked.clone()) {
                Some(previous) if previous != asked => return None,
                _ => {}
            }
        }

        let mut sorts: HashMap<QualityMetaId, Quality> = HashMap::new();
        for (stored, asked) in differences.sorts {
            let Quality::Meta(meta) = stored else {
                return None;
            };
            match sorts.insert(meta, asked) {
                Some(previous) if previous != asked => return None,
                _ => {}
            }
        }

        let across = Self {
            levels: Rc::new(levels),
            sorts: Rc::new(sorts),
        };
        (across.carried(key)? == *query).then_some(across)
    }

    /// `level`, stated at the stored spelling and at universe-binder depth zero, as the asking one states it.
    fn level(
        levels: &HashMap<UniverseMetaId, Level>,
        level: &Level,
    ) -> Result<Level, UniverseError> {
        level.substitute(|head| match head {
            LevelHead::Meta(meta) => levels.get(&meta).cloned(),
            LevelHead::Param(_) => None,
        })
    }

    /// `quality`, stated at the stored spelling, as the asking one states it.
    fn quality(sorts: &HashMap<QualityMetaId, Quality>, quality: &Quality) -> Quality {
        match quality {
            Quality::Meta(meta) => sorts.get(meta).copied().unwrap_or(*quality),
            stated => *stated,
        }
    }

    /// `term`, written at the stored spelling's universes, at the asking spelling's.
    fn carried(&self, term: &Term) -> Option<Term> {
        let levelled = match self.levels.is_empty() {
            true => term.clone(),
            false => {
                let levels = Rc::clone(&self.levels);
                substitute_universe_metas_scoped(term, move |meta| Ok(levels.get(&meta).cloned()))
                    .ok()?
            }
        };
        if self.sorts.is_empty() {
            return Some(levelled);
        }

        let sorts = Rc::clone(&self.sorts);
        rewrite_universe_qualities(&levelled, move |quality| {
            Ok::<_, Infallible>(Self::quality(&sorts, quality))
        })
        .ok()
    }

    /// `universes`, an instance stated at the stored spelling's universes, at the asking spelling's.
    fn carried_universes(&self, universes: &Universes) -> Option<Universes> {
        Some(Universes {
            levels: universes
                .levels
                .iter()
                .map(|level| Self::level(&self.levels, level))
                .collect::<Result<_, _>>()
                .ok()?,
            qualities: universes
                .qualities
                .iter()
                .map(|quality| Self::quality(&self.sorts, quality))
                .collect(),
        })
    }
}

/// The metavariable `level` is, when it is one bare metavariable and nothing else.
fn bare_meta(level: &Level) -> Option<UniverseMetaId> {
    if level.constant != 0 {
        return None;
    }
    let mut atoms = level.atoms();
    match (atoms.next(), atoms.next()) {
        (Some((LevelHead::Meta(meta), 0)), None) => Some(meta),
        _ => None,
    }
}
