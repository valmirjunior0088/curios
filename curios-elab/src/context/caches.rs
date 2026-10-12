//! The elaborator's memo tables and the write stamps that police them.
//!
//! The reduction and elaboration caches are sound only under an explicit invalidation protocol: every store write that could change a cached answer must either bump a stamp (so a pending insert is refused), clear a cache, or retain selectively. Each combination is a named method carrying its own justification, so a new mutation site chooses a protocol instead of improvising one.
//!
//! **Every table lives one declaration**, and that is what lets a hit on it be free: [`Caches::begin_declaration`] clears them where the budget is restored, so which entries are present is a fact about the declaration under judgment rather than about the ones compiled before it. `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` states the rule for both checkers.
//!
//! The *policies* — what is cacheable, and what a probe's groundness gate admits — stay on `Context`, which alone can read the solution and universe stores they consult. This type owns the storage and the write discipline.

use {
    crate::Sort,
    curios_core::{
        ClosedMemo, Enter, Free, Level, LevelHead, Quality, QualityMetaId, Rec, RecGroup, Subterm,
        Term, UniverseError, UniverseMetaId, Universes, rewrite_universe_levels_scoped_shared,
        rewrite_universe_qualities, universes_in_order,
    },
    curios_utilities::{Entropy, Keyed},
    std::{
        cell::RefCell,
        collections::{HashMap, HashSet},
        convert::Infallible,
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
pub(crate) enum ElabProbe {
    Hit((Term, Term)),
    Miss(ElaborationStamp),
    Uncacheable,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct ElaborationStamp {
    terms: Entropy,
    universes: Entropy,
}

/// The reduction and elaboration caches with their two write stamps. See the module documentation for the protocol; `Context` holds exactly one of these.
#[derive(Debug, Default)]
pub(crate) struct Caches {
    /// Reducts, for the declaration in progress: [`Caches::begin_declaration`] clears the table where the budget is restored, so every node it holds was built under that budget, a hit on it is free, and nothing charges an insertion.
    ///
    /// One table serves closed and local-bearing terms alike, since both live exactly one declaration: a table that outlived it would carry across an item boundary what the work after finalization reduced — the witness goals retried and the parked ones drained — and whether a declaration could afford its reductions would depend on that.
    ///
    /// **One table for each of the two reductions**, as the kernel's local-bearing memos have: the first for a judgment's, which may ask the elaborator's conversion at a stuck fold and at a missed equation, and the second for plain reduction, which asks nothing (`Context::plainly`). A term's reduct under one is not its reduct under the other.
    reduction: [HashMap<Term, Term>; 2],
    /// The reduction table's second door, for the terms mentioning no local binder: each universe-erased spelling to the first exact key stored under it. A reduct is the same function of its term whatever the levels in it and whatever sorts its instances state, so two spellings differing only in those — the one checking wrote with universe metavariables and the one totality reads with them solved — are one computation; the exact table cannot see that, and every phase re-ran the fold. A probe that misses the exact table asks here, and a hit is served through [`adapted_across_universes`], which rewrites the stored reduct's levels and sorts to the asking spelling's or declines.
    reduction_erased: [HashMap<Term, Term>; 2],
    /// What the closed machine's runs have answered ([`ClosedMemo`]), one for each of the two reductions as the reducts have: discarded and kept wherever they are.
    closed: [ClosedMemo; 2],
    /// The sort of each type classified since the context was last written to — `Sort::of_in`'s own answer, remembered so that a type whose graph shares a field is classified once per node where the walk alone classifies it once per path.
    ///
    /// **Valid for a quiet stretch, and that is as long as it needs to be.** A sort is derived by reduction and reads more besides: the type a local is assumed at, what a metavariable is solved to and has for a type, a declaration's sort, the levels the universe solver holds. Each of those changes by a stamped write, by a frame's exit — which closes binders and stamps nothing — or where a reduct is cleared, so the table is emptied at the first probe after either stamp has moved, wherever a frame is left, and wherever the reducts are. One classification does none of the three: it opens its binders beside the context rather than in it, which is why `Sort::of_in` threads them, and it is the classification that is per path without the table.
    ///
    /// **Filed by the reduction that read it**, a table for each as the reducts have: a sort is read through reducts, and a term's reduct under a judgment's reduction is not its reduct under plain reduction.
    sorts: [HashMap<Term, Sort>; 2],
    /// The stamps `sorts` was filled under.
    sorts_at: ElaborationStamp,
    /// Each instance of a universe-polymorphic `rec` group reduction has opened in the declaration in progress, beside the group it instantiates and the levels it was opened at. An instance holds its levels substituted through every member, so the pair is what says which instance of which group a copy is without reading the levels back off it. It lives one declaration, as the levels' metavariables do.
    instances: HashMap<Keyed<RecGroup>, (RecGroup, Universes)>,
    elaboration: HashMap<Keyed<ElaborationKey>, (Term, Term)>,
    /// Elaborations inside an oracle bracket (`Context::with_oracle`) that the table above must refuse, one table per live bracket, innermost last: see `Context::get_or_init_elaborated` for what they admit and why. Cleared wherever the table above is, and discarded with their bracket.
    oracle: Vec<HashMap<Keyed<ElaborationKey>, (Term, Term)>>,
    /// Whether a binder is itself a proof — assumed at a proposition — for each binder a stuck form naming it has put to the entries in scope (`reduce`'s `proofs_named`). A binder's identity is minted once, so an entry is never another binder's, and the table goes with the declaration.
    proofs: HashMap<Free, bool>,
    /// One tick per *write* to any kernel store — definitions, refinements, assumptions, name/metavariable minting, solves, parked/deferred work, the witness table. `Context::get_or_init_elaborated` snapshots it around a candidate sub-elaboration: an unchanged stamp certifies the run was pure (replaying it would be the identity on the context), which is what makes skipping the replay on a later cache hit sound.
    mutation_stamp: Entropy,
    /// A recorded scrutinee against itself as the kernel spells it — solved metavariables materialized, local definitions substituted, refined variables as their values — kept where no metavariable in it is left unsolved: what the filter in front of an entry reads (`reduce`'s `could_settle`), which would otherwise rebuild the scrutinee at every stuck form. It rests on the solutions, the definitions and the refinements in the window, so it goes wherever the reducts do, and at a settlement's boundary whichever reduction crosses it. Behind a cell because the filter runs while the entries are borrowed.
    kernel_spellings: RefCell<HashMap<Term, Term>>,
    /// Monotonic universe-solver writes are tracked separately. Elaboration entries may survive them only when their keys and results contain no transitively unresolved universe meta; reducts survive them outright, being parametric in levels (see `Context::cached_reduced`); rollback/finalization clears every cache at the non-monotonic boundaries.
    universe_mutation_stamp: Entropy,
}

impl Caches {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Record a write to a kernel store, poisoning any elaboration-cache insert whose computation spans it.
    pub(crate) fn note_write(&mut self) {
        self.mutation_stamp.fresh();
    }

    /// Record a universe-solver write the guard could not see (seeding); `universe_stamp` covers the guarded path.
    pub(crate) fn note_universe_write(&mut self) {
        self.universe_mutation_stamp.fresh();
    }

    /// The universe stamp itself, for `UniverseMutation`'s drop guard: solver mutations bump it only when solver state actually changed, so read-equivalent solver calls stay cache-pure.
    pub(crate) fn universe_stamp(&self) -> &Entropy {
        &self.universe_mutation_stamp
    }

    /// Snapshot both stamps — a `Miss`'s purity witness.
    pub(crate) fn stamps(&self) -> ElaborationStamp {
        ElaborationStamp {
            terms: self.mutation_stamp.clone(),
            universes: self.universe_mutation_stamp.clone(),
        }
    }

    /// Whether nothing has been written since `stamps` was taken — the purity half of the elaboration cache's insert gate.
    pub(crate) fn stamps_unchanged(&self, stamp: &ElaborationStamp) -> bool {
        self.mutation_stamp == stamp.terms && self.universe_mutation_stamp == stamp.universes
    }

    /// The remembered reduct of `term`, as plain reduction took it where `plain` and as a judgment's did otherwise.
    ///
    /// A reduct served through the second door holds the instances reduction opened for the stored spelling, each carried to the asking spelling's levels. They are the instances reduction would have opened for the asking one, and are opened as those ([`Caches::opened_instance`]): a term that names a group's members again knows an instance by that record alone, so one served unrecorded would stand in a solution as a copy of its group. A hit whose instances do not open is declined, as one whose levels do not map is.
    pub(crate) fn reduction_get(&mut self, term: &Term, plain: bool) -> Option<Term> {
        let plain = usize::from(plain);
        if let Some(reduct) = self.reduction[plain].get(term) {
            return Some(reduct.clone());
        }
        if term.has_local_free() {
            return None;
        }
        let key = self.reduction_erased[plain].get(&term.erased_universes())?;
        let reduct = self.reduction[plain].get(key)?;
        // An entry recording that a term is its own weak-head form says the same of every spelling the door equates with it, and the asking spelling is the one to hand back: the stored one was written elsewhere, and a Π-type served in its place carried that spelling's binder names into a report about this one.
        if reduct == key {
            curios_profile::sample!("reduction::across_levels", 1);
            return Some(term.clone());
        }
        let adapted = AcrossUniverses::between(key, term)
            .and_then(|across| Some((across.carried(reduct)?, across)));
        curios_profile::sample!("reduction::across_levels", u64::from(adapted.is_some()));
        let (adapted, across) = adapted?;
        for (group, universes) in self.instances_carried(reduct, &across)? {
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
        if across.is_identity() || !reduct.mentions_rec() {
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
    pub(crate) fn closed_memo(&mut self, plain: bool) -> &mut ClosedMemo {
        &mut self.closed[usize::from(plain)]
    }

    pub(crate) fn reduction_insert(&mut self, term: Term, reduct: Term, plain: bool) {
        let plain = usize::from(plain);
        if !term.has_local_free() {
            self.reduction_erased[plain]
                .entry(term.erased_universes())
                .or_insert_with(|| term.clone());
        }
        self.reduction[plain].insert(term, reduct);
    }

    /// Every reduct of both reductions, the second doors indexing them, the sorts read through them, and the scrutinees remembered as the kernel spells them.
    fn clear_reductions(&mut self) {
        for table in self.reduction.iter_mut().chain(&mut self.reduction_erased) {
            table.clear();
        }
        for closed in &mut self.closed {
            closed.clear();
        }
        self.clear_sorts();
        self.kernel_spellings.get_mut().clear();
    }

    /// Every remembered sort, of both reductions.
    fn clear_sorts(&mut self) {
        for sorts in &mut self.sorts {
            sorts.clear();
        }
    }

    /// The remembered sort of `type_`, where nothing was written since it was filed: as plain reduction read it where `plain`, and as a judgment's did otherwise.
    pub(crate) fn sort_get(&mut self, type_: &Term, plain: bool) -> Option<Sort> {
        self.settle_sorts();

        self.sorts[usize::from(plain)].get(type_).cloned()
    }

    pub(crate) fn sort_insert(&mut self, type_: Term, plain: bool, sort: Sort) {
        self.settle_sorts();
        self.sorts[usize::from(plain)].insert(type_, sort);
    }

    /// Empty the sorts where either stamp has moved since they were filed, and file what follows under the stamps as they stand.
    fn settle_sorts(&mut self) {
        if !self.stamps_unchanged(&self.sorts_at) {
            self.clear_sorts();
            self.sorts_at = self.stamps();
        }
    }

    /// `term` as the kernel spells it, where that has been remembered.
    pub(crate) fn kernel_spelling_get(&self, term: &Term) -> Option<Term> {
        self.kernel_spellings.borrow().get(term).cloned()
    }

    pub(crate) fn kernel_spelling_insert(&self, term: Term, spelled: Term) {
        self.kernel_spellings.borrow_mut().insert(term, spelled);
    }

    /// Whether the binder `name` is a proof, where that has been asked.
    pub(crate) fn proof_get(&self, name: &Free) -> Option<bool> {
        self.proofs.get(name).copied()
    }

    pub(crate) fn proof_insert(&mut self, name: Free, proof: bool) {
        self.proofs.insert(name, proof);
    }

    /// A new declaration: every table is discarded — the reducts and the elaborations — so that what one declaration can afford is decided by nothing the declarations before it left behind.
    pub(crate) fn begin_declaration(&mut self) {
        self.proofs.clear();
        self.instances.clear();
        self.clear_reductions();
        self.clear_elaborations();
    }

    /// `group` at `universes`, remembered as that instance of it: the one way an instance is opened, so that every one a term comes to hold is known for the instance it is.
    pub(crate) fn opened_instance(
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

    /// The group `instance` was opened from and the instance it was opened at, where reduction opened it in the declaration in progress.
    pub(crate) fn instance_get(&self, instance: &RecGroup) -> Option<&(RecGroup, Universes)> {
        self.instances.get(&Keyed::new(instance.clone()))
    }

    pub(crate) fn elaboration_get(
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

    pub(crate) fn elaboration_insert(
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
    pub(crate) fn begin_oracle(&mut self) {
        self.oracle.push(HashMap::new());
    }

    /// Discard the table of the oracle bracket being left.
    pub(crate) fn end_oracle(&mut self) {
        self.oracle.pop();
    }

    /// Whether an oracle bracket is live, so an elaboration the table above refuses may be remembered for it.
    pub(crate) fn in_oracle(&self) -> bool {
        !self.oracle.is_empty()
    }

    pub(crate) fn oracle_get(
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

    pub(crate) fn oracle_insert(
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

    /// A counterfactual refinement was registered: a refinement key can be a `#`-free stuck application of globals, so it can have influenced any entry — both caches clear wholesale, and the write is stamped.
    pub(crate) fn invalidate_for_refinement(&mut self) {
        self.note_write();
        self.clear_reductions();
        self.clear_elaborations();
    }

    /// A name was *re*defined (or an assumption's universe scheme rewritten in place): the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the name, leaving nothing for a selective retain to key on — both caches clear wholesale, and the write is stamped.
    pub(crate) fn invalidate_for_redefinition(&mut self) {
        self.note_write();
        self.clear_reductions();
        self.clear_elaborations();
    }

    /// A name was *freshly* defined. A fresh definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF — so the reduction cache retains every entry whose result does not mention it instead of clearing. The elaboration cache survives untouched: its insert gate already refused every entry naming a not-yet-defined global. No stamp — definition is the one ambient fact a pure run may read, and the settled-globals gate covers it.
    pub(crate) fn retain_reductions_without(&mut self, name: &Free) {
        for (reduction, erased) in self.reduction.iter_mut().zip(&mut self.reduction_erased) {
            reduction.retain(|_, reduct| !reduct.mentions_free(name));
            // The second door indexes exact keys, so it keeps exactly the ones the exact table kept.
            erased.retain(|_, key| reduction.contains_key(key));
        }
        for closed in &mut self.closed {
            closed.retain_without(name);
        }
        // A sort is read through reducts and names none, so it has nothing to be retained by.
        self.clear_sorts();
    }

    /// An assumption's type was replaced in place (`reassume`): an entry elaborated between a `rec` group's lowered `assume` and this upgrade could embed the lowered signature, so the elaboration cache clears; reducts never read assumption types, so the reduction cache survives. Stamped.
    pub(crate) fn invalidate_for_reassumption(&mut self) {
        self.note_write();
        self.clear_elaborations();
    }

    /// A local frame was dropped. A dropped refinement can have influenced any entry, so both caches clear; a dropped frame *definition* clears only the reduction cache — `reduce_let` defines under written binder labels a reduct can fold in, while elaboration-position terms name only `/`-qualified globals and `#`-minted locals, so no elaboration entry can reference a written frame label. No stamp: the frame's own writes were stamped when they landed.
    pub(crate) fn invalidate_frame_exit(
        &mut self,
        dropped_refinements: bool,
        dropped_definitions: bool,
    ) {
        // The frame's binders go with it, and a sort is read off a binder's type: the write that opened one was stamped, and nothing stamps its closing.
        self.clear_sorts();
        if dropped_refinements {
            self.clear_reductions();
            self.clear_elaborations();
        } else if dropped_definitions {
            self.clear_reductions();
        }
    }

    /// A settlement is withholding, or has stopped withholding, the refinements from the entry it settles inwards: the suppression boundary's reason, facing the other way — what was reduced on one side of that line must not answer on the other — so the reduction tables clear on both sides as they do there.
    ///
    /// **Inside a span of plain reduction only what plain reduction remembered goes.** Nothing but plain reduction runs between the two sides of a settlement made there, so what a judgment's reduction remembered is as true when the refinements are restored as it was before they were withheld.
    pub(crate) fn invalidate_settlement_boundary(&mut self, plain: bool) {
        match plain {
            true => {
                self.reduction[1].clear();
                self.reduction_erased[1].clear();
                self.closed[1].clear();
                self.sorts[1].clear();
                // A scrutinee's spelling reads the refinements in the window, which this boundary moves.
                self.kernel_spellings.get_mut().clear();
            }
            false => {
                self.clear_reductions();
            }
        }
    }

    /// A refinement-suppression boundary is being crossed with refinements registered: refinement-applied and refinement-suppressed reducts must never contaminate each other's cache, so both clear — on both sides of the bracket, unstamped (the flag flip itself writes nothing).
    pub(crate) fn invalidate_suppression_boundary(&mut self) {
        // How many reducts the clear throws away.
        curios_profile::sample!(
            "caches::suppression_dropped",
            (self.reduction[0].len() + self.reduction[1].len()) as u64
        );
        self.clear_reductions();
        self.clear_elaborations();
    }

    /// Universe levels were rewritten in place (defaulting, finalization, instance closure): cached reducts and elaborations may embed the pre-rewrite levels, so both clear. The solver write itself is stamped by the `UniverseMutation` guard.
    pub(crate) fn invalidate_for_universe_rewrite(&mut self) {
        self.clear_reductions();
        self.clear_elaborations();
    }

    /// Solutions were rolled back — the one *un*-monotonic store transition. Reducts may have been cached through the unwound solutions, so both caches clear and both stamps tick.
    pub(crate) fn invalidate_for_rollback(&mut self) {
        // How many reducts the clear throws away.
        curios_profile::sample!(
            "caches::rollback_dropped",
            (self.reduction[0].len() + self.reduction[1].len()) as u64
        );
        self.note_write();
        self.note_universe_write();
        self.clear_reductions();
        // Entries are metavar-free on both key and value, so an un-solve cannot invalidate them in principle; cleared anyway while the rollback bracket is young — conservative and cheap.
        self.clear_elaborations();
    }

    /// The elaboration island changed (a new top-level item): representation-privacy checks are island-relative, so an entry elaborated under one item's island must not answer for another's. Reducts are island-independent and survive.
    pub(crate) fn invalidate_for_island_change(&mut self) {
        self.clear_elaborations();
    }

    /// Universe constraints were discarded at a transaction boundary with actual solver-state change: elaboration entries may have certified purity against constraints that no longer exist.
    pub(crate) fn invalidate_for_universe_transaction(&mut self) {
        self.clear_elaborations();
    }

    /// Level constraints were withdrawn from a scope in which a judgment's reduction put a question to conversion: a reduct taken there may rest on an answer those constraints gave, so the reducts go. No stamp: the withdrawal itself was stamped.
    pub(crate) fn invalidate_for_withdrawn_answers(&mut self) {
        // How many reducts the clear throws away.
        curios_profile::sample!(
            "caches::withdrawn_dropped",
            (self.reduction[0].len() + self.reduction[1].len()) as u64
        );
        self.clear_reductions();
    }
}

/// How what is written at a spelling stored in the reduction table is carried to another that shares its universe-erased projection: each universe metavariable of the stored spelling to the level the asking one states in its place, and each sort metavariable to the sort it states. A reduct stored under the one is the answer for the other once carried ([`Self::carried`]).
///
/// **Sound by level-parametricity.** No reduction rule reads a level — `Type u` is a payload, never a scrutinee — so a reduct is the same function of its term whatever the levels in it, and rewriting the stored reduct's levels through the map the two spellings determine yields exactly what reducing the asking spelling would have. The map is exact or there is none ([`Self::between`]): a stored level that is not a bare metavariable and differs from the asked one, a metavariable asked for two levels, itself counted as one of them, a level kept as it stands that names a metavariable the map sends elsewhere, or spellings whose level positions do not align — one carrying an instance the other erased, which projection equates and this cannot map — each decline, and the caller reduces as it would have.
///
/// **A sort an instance states is a payload as a level is.** The erased projection drops an instance whole, the sorts it states with its levels, so two spellings it equates may state different sorts: an occurrence of a family at `Prop` beside one at `Type`, or two whose sorts are metavariables of their own. No reduction rule reads one, and the reduct holds them, so they are mapped as the levels are: a stored metavariable to the one sort the asking spelling states in its every place, itself included, and a stored `Prop`, `Type` or parameter to nothing but itself. Served with the stored sorts, the second occurrence would be read at the first one's.
struct AcrossUniverses {
    levels: Rc<HashMap<UniverseMetaId, Level>>,
    sorts: Rc<HashMap<QualityMetaId, Quality>>,
}

impl AcrossUniverses {
    /// From `key`, a stored spelling, to `query`, which shares its universe-erased projection: the stored spelling's levels and the sorts its instances state are matched to the asking spelling's position by position.
    fn between(key: &Term, query: &Term) -> Option<Self> {
        if !key.has_universe_data() || !query.has_universe_data() {
            return None;
        }

        // Once per occurrence, deliberately: the sequences are the answer, and two spellings are aligned by position in them, so a walk that skipped a revisit on one side would pair the wrong members.
        let (stored_levels, stored_sorts) = universes_in_order(key);
        let (asked_levels, asked_sorts) = universes_in_order(query);
        if stored_levels.len() != asked_levels.len() || stored_sorts.len() != asked_sorts.len() {
            return None;
        }

        let mut levels: HashMap<UniverseMetaId, Level> = HashMap::new();
        for (stored, asked) in stored_levels.iter().zip(&asked_levels) {
            let Some(meta) = bare_meta(stored) else {
                if stored == asked {
                    continue;
                }
                return None;
            };
            match levels.insert(meta, asked.clone()) {
                Some(previous) if &previous != asked => return None,
                _ => {}
            }
        }
        levels.retain(|meta, asked| bare_meta(asked) != Some(*meta));
        // A level kept as it stands is kept in the reduct only where the map leaves its every metavariable alone.
        let rewritten = |level: &Level| {
            level.atoms().any(
                |(head, _)| matches!(head, LevelHead::Meta(meta) if levels.contains_key(&meta)),
            )
        };
        if stored_levels
            .iter()
            .any(|stored| bare_meta(stored).is_none() && rewritten(stored))
        {
            return None;
        }

        let mut sorts: HashMap<QualityMetaId, Quality> = HashMap::new();
        for (stored, asked) in stored_sorts.iter().zip(&asked_sorts) {
            let Quality::Meta(meta) = stored else {
                if stored == asked {
                    continue;
                }
                return None;
            };
            match sorts.insert(*meta, *asked) {
                Some(previous) if previous != *asked => return None,
                _ => {}
            }
        }
        sorts.retain(|meta, asked| *asked != Quality::Meta(*meta));

        Some(Self {
            levels: Rc::new(levels),
            sorts: Rc::new(sorts),
        })
    }

    /// Whether the two spellings state the same universes, so that nothing is rewritten.
    fn is_identity(&self) -> bool {
        self.levels.is_empty() && self.sorts.is_empty()
    }

    /// `level`, stated at the stored spelling, as the asking one states it.
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
                rewrite_universe_levels_scoped_shared(term, move |_, level| {
                    Self::level(&levels, level)
                })
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
