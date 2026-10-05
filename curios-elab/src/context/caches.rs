//! The elaborator's memo tables and the write stamps that police them.
//!
//! The reduction, elaboration and canonical-key caches are sound only under an explicit invalidation protocol: every store write that could change a cached answer must either bump a stamp (so a pending insert is refused), clear a cache, or retain selectively. Each combination is a named method carrying its own justification, so a new mutation site chooses a protocol instead of improvising one.
//!
//! **Every table lives one declaration**, and that is what lets a hit on it be free: [`Caches::begin_declaration`] clears them where the budget is restored, so which entries are present is a fact about the declaration under judgment rather than about the ones compiled before it. `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` states the rule for both checkers.
//!
//! The *policies* — what is cacheable, and what a probe's groundness gate admits — stay on `Context`, which alone can read the solution and universe stores they consult. This type owns the storage and the write discipline.

use {
    crate::Sort,
    curios_core::{
        Free, Level, LevelHead, Term, UniverseMetaId, rewrite_universe_levels_scoped,
        rewrite_universe_levels_scoped_shared,
    },
    curios_utilities::Entropy,
    std::{cell::RefCell, collections::HashMap, rc::Rc},
};

/// Key of one memoized `elaborate` call: the lowered term, the `Check` expected type (`None` for `Infer`), whether an island's representation-privacy checks were live, and whether reduction was plain. Validity under suppressed privacy is directional — an entry that passed strict checks would be valid under suppression, but not the reverse — so checked and suppressed runs each answer only their own partition. A run under plain reduction, which asks the elaborator's conversion nothing (`Context::plainly`), refuses what a judgment's run accepts by a fold or an equation its conversion decided, so a plain run and a judgment's each answer only their own partition as well.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ElaborationKey {
    term: Term,
    expected: Option<Term>,
    privacy_checked: bool,
    plain: bool,
}

/// Outcome of `Context::probe_elaborated` — the read half of the elaboration cache, torn out of `Context::get_or_init_elaborated`'s bracket so the iterative `elaborate` driver can probe at a frame push and record at the matching pop (the reduction cache's `cached_reduced`/`reduce` split, one level up). `Hit` carries the memoized, un-span-stamped `(rebuilt, type)`; `Miss` carries the state snapshot the caller threads back into `Context::record_elaborated` as its purity witness; `Uncacheable` marks a term the groundness gate excludes — the caller elaborates it but records nothing.
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

/// The reduction, elaboration and canonical-key caches with their two write stamps. See the module documentation for the protocol; `Context` holds exactly one of these.
#[derive(Debug, Default)]
pub(crate) struct Caches {
    /// Reducts, for the declaration in progress: [`Caches::begin_declaration`] clears the table where the budget is restored, so every node it holds was built under that budget, a hit on it is free, and nothing charges an insertion.
    ///
    /// One table serves closed and local-bearing terms alike, since both live exactly one declaration: a table that outlived it would carry across an item boundary what the work after finalization reduced — the witness goals retried and the parked ones drained — and whether a declaration could afford its reductions would depend on that.
    ///
    /// **One table for each of the two reductions**, as the kernel's local-bearing memos have: the first for a judgment's, which may ask the elaborator's conversion at a stuck fold and at a missed equation, and the second for plain reduction, which asks nothing (`Context::plainly`). A term's reduct under one is not its reduct under the other.
    reduction: [HashMap<Term, Term>; 2],
    /// The reduction table's second door, for the terms mentioning no local binder: each universe-erased spelling to the first exact key stored under it. A reduct is the same function of its term whatever the levels in it, so two spellings differing only in their levels — the one checking wrote with universe metavariables and the one totality reads with them solved — are one computation; the exact table cannot see that, and every phase re-ran the fold. A probe that misses the exact table asks here, and a hit is served through [`adapted_across_levels`], which rewrites the stored reduct's levels to the asking spelling's or declines.
    reduction_erased: [HashMap<Term, Term>; 2],
    /// A registered refinement key against the canonical form the escalation compares it at (`reduce::canonical_scrutinee`: head verbatim, arguments and operands in weak-head normal form).
    ///
    /// **Per *key*, where the escalation is per *probe*.** A store entry is recorded as the guard was written and every occurrence the reducer meets has been reduced, so the two meet only through a canonical form — which nothing but reduction produces. Recomputing it at each probe re-derives one guard's subject once per node of that operation in the declaration, and a subject that reduces to a *stuck* form is not held by the reduction table either, since that one caches reducts rather than the walks that failed to settle. Filled on the first escalation that needs it, so a guard whose fact is never probed against still costs nothing to register — the property `reduce::shallow_scrutinee` exists to keep.
    ///
    /// A key the allowance stopped short is memoized *as itself*, so a bail is paid once rather than at every probe.
    ///
    /// Derived by reduction, so it is invalidated wherever a reduct is.
    canonical_keys: HashMap<(Term, bool), Term>,
    /// The sort of each type classified since the context was last written to — `Sort::of_in`'s own answer, remembered so that a type whose graph shares a field is classified once per node where the walk alone classifies it once per path.
    ///
    /// **Valid for a quiet stretch, and that is as long as it needs to be.** A sort is derived by reduction and reads more besides: the type a local is assumed at, what a metavariable is solved to and has for a type, a declaration's sort, the levels the universe solver holds. Each of those changes by a stamped write, by a frame's exit — which closes binders and stamps nothing — or where a reduct is cleared, so the table is emptied at the first probe after either stamp has moved, wherever a frame is left, and wherever the reducts are. One classification does none of the three: it opens its binders beside the context rather than in it, which is why `Sort::of_in` threads them, and it is the classification that is per path without the table.
    ///
    /// **Filed by the reduction that read it**, as a canonical key is: a sort is read through reducts, and a term's reduct under a judgment's reduction is not its reduct under plain reduction.
    sorts: HashMap<(Term, bool), Sort>,
    /// The stamps `sorts` was filled under.
    sorts_at: ElaborationStamp,
    elaboration: HashMap<ElaborationKey, (Term, Term)>,
    /// Elaborations inside an oracle bracket (`Context::with_oracle`) that the table above must refuse, one table per live bracket, innermost last: see `Context::get_or_init_elaborated` for what they admit and why. Cleared wherever the table above is, and discarded with their bracket.
    oracle: Vec<HashMap<ElaborationKey, (Term, Term)>>,
    /// Whether a binder is itself a proof — assumed at a proposition — for each binder a stuck form naming it has put to the entries in scope (`reduce`'s `proofs_named`). A binder's identity is minted once, so an entry is never another binder's, and the table goes with the declaration.
    proofs: HashMap<Free, bool>,
    /// One tick per *write* to any kernel store — definitions, refinements, assumptions, name/metavariable minting, solves, parked/deferred work, the witness table. `Context::get_or_init_elaborated` snapshots it around a candidate sub-elaboration: an unchanged stamp certifies the run was pure (replaying it would be the identity on the context), which is what makes skipping the replay on a later cache hit sound.
    mutation_stamp: Entropy,
    /// A recorded scrutinee against itself with its solved metavariables materialized, kept where none is left unsolved: what the filter in front of an entry reads (`reduce`'s `could_settle`), which would otherwise rebuild the scrutinee at every stuck form. It rests on solutions alone, so it goes wherever a canonical key goes, which is wherever one can be withdrawn. Behind a cell because the filter runs while the entries are borrowed.
    materialized: RefCell<HashMap<Term, Term>>,
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
    pub(crate) fn reduction_get(&self, term: &Term, plain: bool) -> Option<Term> {
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
        let adapted = adapted_across_levels(key, term, reduct);
        curios_profile::sample!("reduction::across_levels", u64::from(adapted.is_some()));
        adapted
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

    /// Every reduct of both reductions, the second doors indexing them, and the sorts read through them.
    fn clear_reductions(&mut self) {
        for table in self.reduction.iter_mut().chain(&mut self.reduction_erased) {
            table.clear();
        }
        self.sorts.clear();
    }

    /// The remembered sort of `type_`, where nothing was written since it was filed: as plain reduction read it where `plain`, and as a judgment's did otherwise.
    pub(crate) fn sort_get(&mut self, type_: &Term, plain: bool) -> Option<Sort> {
        self.settle_sorts();

        self.sorts.get(&(type_.clone(), plain)).cloned()
    }

    pub(crate) fn sort_insert(&mut self, type_: Term, plain: bool, sort: Sort) {
        self.settle_sorts();
        self.sorts.insert((type_, plain), sort);
    }

    /// Empty the sorts where either stamp has moved since they were filed, and file what follows under the stamps as they stand.
    fn settle_sorts(&mut self) {
        if !self.stamps_unchanged(&self.sorts_at) {
            self.sorts.clear();
            self.sorts_at = self.stamps();
        }
    }

    /// `term` with its solved metavariables materialized, where that has been remembered.
    pub(crate) fn materialized_get(&self, term: &Term) -> Option<Term> {
        self.materialized.borrow().get(term).cloned()
    }

    pub(crate) fn materialized_insert(&self, term: Term, materialized: Term) {
        self.materialized.borrow_mut().insert(term, materialized);
    }

    /// Whether the binder `name` is a proof, where that has been asked.
    pub(crate) fn proof_get(&self, name: &Free) -> Option<bool> {
        self.proofs.get(name).copied()
    }

    pub(crate) fn proof_insert(&mut self, name: Free, proof: bool) {
        self.proofs.insert(name, proof);
    }

    /// A new declaration: every table is discarded — the reducts, the canonical refinement keys, and the elaborations — so that what one declaration can afford is decided by nothing the declarations before it left behind.
    pub(crate) fn begin_declaration(&mut self) {
        self.proofs.clear();
        self.clear_reductions();
        self.canonical_keys.clear();
        self.materialized.get_mut().clear();
        self.clear_elaborations();
    }

    /// A key's canonical form, filed as a reduct is: as plain reduction took it where `plain`, and as a judgment's did otherwise.
    pub(crate) fn canonical_key_get(&self, key: &Term, plain: bool) -> Option<Term> {
        self.canonical_keys.get(&(key.clone(), plain)).cloned()
    }

    pub(crate) fn canonical_key_insert(&mut self, key: Term, plain: bool, canonical: Term) {
        self.canonical_keys.insert((key, plain), canonical);
    }

    pub(crate) fn elaboration_get(
        &self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
        plain: bool,
    ) -> Option<(Term, Term)> {
        self.elaboration
            .get(&ElaborationKey {
                term: term.clone(),
                expected: expected.cloned(),
                privacy_checked,
                plain,
            })
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
            ElaborationKey {
                term: term.clone(),
                expected: expected.cloned(),
                privacy_checked,
                plain,
            },
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
            .get(&ElaborationKey {
                term: term.clone(),
                expected: expected.cloned(),
                privacy_checked,
                plain,
            })
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
                ElaborationKey {
                    term: term.clone(),
                    expected: expected.cloned(),
                    privacy_checked,
                    plain,
                },
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
        self.canonical_keys.clear();
        self.materialized.get_mut().clear();
        self.clear_elaborations();
    }

    /// A name was *re*defined (or an assumption's universe scheme rewritten in place): the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the name, leaving nothing for a selective retain to key on — both caches clear wholesale, and the write is stamped.
    pub(crate) fn invalidate_for_redefinition(&mut self) {
        self.note_write();
        self.clear_reductions();
        self.canonical_keys.clear();
        self.materialized.get_mut().clear();
        self.clear_elaborations();
    }

    /// A name was *freshly* defined. A fresh definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF — so the reduction cache retains every entry whose result does not mention it instead of clearing. The elaboration cache survives untouched: its insert gate already refused every entry naming a not-yet-defined global. No stamp — definition is the one ambient fact a pure run may read, and the settled-globals gate covers it.
    pub(crate) fn retain_reductions_without(&mut self, name: &Free) {
        for (reduction, erased) in self.reduction.iter_mut().zip(&mut self.reduction_erased) {
            reduction.retain(|_, reduct| !reduct.mentions_free(name));
            // The second door indexes exact keys, so it keeps exactly the ones the exact table kept.
            erased.retain(|_, key| reduction.contains_key(key));
        }
        // A sort is read through reducts and names none, so it has nothing to be retained by.
        self.sorts.clear();
        // A canonical key is a reduct of the same kind, retained by the same test.
        self.canonical_keys
            .retain(|_, canonical| !canonical.mentions_free(name));
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
        self.sorts.clear();
        if dropped_refinements {
            self.clear_reductions();
            self.canonical_keys.clear();
            self.materialized.get_mut().clear();
            self.clear_elaborations();
        } else if dropped_definitions {
            self.clear_reductions();
            self.canonical_keys.clear();
            self.materialized.get_mut().clear();
        }
    }

    /// A settlement is withholding, or has stopped withholding, the refinements from the entry it settles inwards: the suppression boundary's reason, facing the other way — what was reduced on one side of that line must not answer on the other — so the reduction tables and canonical keys clear on both sides as they do there.
    ///
    /// **Inside a span of plain reduction only what plain reduction remembered goes.** Nothing but plain reduction runs between the two sides of a settlement made there, so what a judgment's reduction remembered is as true when the refinements are restored as it was before they were withheld.
    pub(crate) fn invalidate_settlement_boundary(&mut self, plain: bool) {
        match plain {
            true => {
                self.reduction[1].clear();
                self.reduction_erased[1].clear();
                self.sorts.retain(|(_, plain), _| !plain);
                self.canonical_keys.retain(|(_, plain), _| !plain);
            }
            false => {
                self.clear_reductions();
                self.canonical_keys.clear();
                self.materialized.get_mut().clear();
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
        self.canonical_keys.clear();
        self.materialized.get_mut().clear();
        self.clear_elaborations();
    }

    /// Universe levels were rewritten in place (defaulting, finalization, instance closure): cached reducts and elaborations may embed the pre-rewrite levels, so both clear. The solver write itself is stamped by the `UniverseMutation` guard.
    pub(crate) fn invalidate_for_universe_rewrite(&mut self) {
        self.clear_reductions();
        self.canonical_keys.clear();
        self.materialized.get_mut().clear();
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
        self.canonical_keys.clear();
        self.materialized.get_mut().clear();
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
}

/// `reduct`, stored under `key`, as the answer for `query`, which shares `key`'s universe-erased projection: the stored spelling's levels are matched to the asking spelling's position by position, and the reduct's levels rewritten through the metavariables that differ.
///
/// **Sound by level-parametricity.** No reduction rule reads a level — `Type u` is a payload, never a scrutinee — so a reduct is the same function of its term whatever the levels in it, and rewriting the stored reduct's levels through the map the two spellings determine yields exactly what reducing the asking spelling would have. The map is exact or the hit is declined: a stored level that is not a bare metavariable and differs from the asked one, a metavariable asked for two levels, or spellings whose level positions do not align — one carrying an instance the other erased, which projection equates and this cannot map — each decline, and the caller reduces as it would have.
fn adapted_across_levels(key: &Term, query: &Term, reduct: &Term) -> Option<Term> {
    if !key.has_universe_data() || !query.has_universe_data() {
        return None;
    }

    let stored = levels_in_order(key);
    let asked = levels_in_order(query);
    if stored.len() != asked.len() {
        return None;
    }

    let mut map: HashMap<UniverseMetaId, Level> = HashMap::new();
    for (stored, asked) in stored.iter().zip(&asked) {
        if stored == asked {
            continue;
        }
        let meta = bare_meta(stored)?;
        match map.insert(meta, asked.clone()) {
            Some(previous) if &previous != asked => return None,
            _ => {}
        }
    }

    if map.is_empty() {
        return Some(reduct.clone());
    }

    rewrite_universe_levels_scoped_shared(reduct, move |_, level| {
        level.substitute(|head| match head {
            LevelHead::Meta(meta) => map.get(&meta).cloned(),
            LevelHead::Param(_) => None,
        })
    })
    .ok()
}

/// Every level a term carries, in the order the level-rewriting walk meets them — the order two spellings of one term share.
fn levels_in_order(term: &Term) -> Vec<Level> {
    let levels = Rc::new(RefCell::new(Vec::new()));
    let sink = Rc::clone(&levels);
    // Once per occurrence, deliberately: the sequence is the answer, and two spellings are aligned by position in it, so a walk that skipped a revisit on one side would pair the wrong levels.
    let _ = rewrite_universe_levels_scoped(term, move |_, level: &Level| {
        sink.borrow_mut().push(level.clone());
        Ok::<_, ()>(level.clone())
    });
    Rc::try_unwrap(levels)
        .map(RefCell::into_inner)
        .unwrap_or_else(|shared| shared.borrow().clone())
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
