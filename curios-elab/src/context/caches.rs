//! The kernel's three memo tables and the write stamps that police them.
//!
//! The reduction, elaboration and canonical-key caches are sound only under an explicit invalidation protocol: every store write that could change a cached answer must either bump a stamp (so a pending insert is refused), clear a cache, or retain selectively. Those combinations used to be hand-rolled at each mutation site; here each one is a named method carrying its own justification, so a new mutation site chooses a protocol instead of improvising one.
//!
//! The *policies* — what is cacheable, and what a probe's groundness gate admits — stay on `Context`, which alone can read the solution and universe stores they consult. This type owns the storage and the write discipline.

use {
    curios_core::{Free, Level, LevelHead, Term, UniverseMetaId, rewrite_universe_levels_scoped},
    curios_utilities::Entropy,
    std::{cell::RefCell, collections::HashMap, rc::Rc},
};

/// Key of one memoized `elaborate` call: the lowered term, the `Check` expected type (`None` for `Infer`), and whether an island's representation-privacy checks were live. Validity under suppressed privacy is directional — an entry that passed strict checks would be valid under suppression, but not the reverse — so checked and suppressed runs each answer only their own partition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ElaborationKey {
    term: Term,
    expected: Option<Term>,
    privacy_checked: bool,
}

/// Outcome of `Context::probe_elaborated` — the read half of the elaboration cache, torn out of `Context::get_or_init_elaborated`'s bracket so the iterative `elaborate` driver can probe at a frame push and record at the matching pop (the reduction cache's `cached_reduced`/`reduce` split, one level up). `Hit` carries the memoized, un-span-stamped `(rebuilt, type)`; `Miss` carries the state snapshot the caller threads back into `Context::record_elaborated` as its purity witness; `Uncacheable` marks a term the groundness gate excludes — the caller elaborates it but records nothing.
pub(crate) enum ElabProbe {
    Hit((Term, Term)),
    Miss(ElaborationStamp),
    Uncacheable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ElaborationStamp {
    terms: Entropy,
    universes: Entropy,
}

/// The reduction, elaboration and canonical-key caches with their two write stamps. See the module documentation for the protocol; `Context` holds exactly one of these.
#[derive(Debug, Default)]
pub(crate) struct Caches {
    /// Reducts of terms mentioning no local binder — the table that survives item boundaries, so closed reducts stay warm across the definitions reduction and erasure mint, and the one the retention allowance prices.
    reduction: HashMap<Term, Term>,
    /// The closed table's second door: each universe-erased spelling to the first exact key stored under it. A reduct is the same function of its term whatever the levels in it, so two spellings differing only in their levels — the one checking wrote with universe metavariables and the one totality reads with them solved — are one computation; the exact table cannot see that, and every phase re-ran the fold. A probe that misses the exact table asks here, and a hit is served through [`adapted_across_levels`], which rewrites the stored reduct's levels to the asking spelling's or declines.
    reduction_erased: HashMap<Term, Term>,
    /// Reducts of terms mentioning a local binder. Declaration-scoped by nature — a binder is minted once and never recurs, so no later declaration can ask about one — and therefore bounded by the work budget that built every node it holds rather than by the retention allowance: [`Caches::begin_declaration`] clears it where the budget is restored, and nothing charges an insertion. Apart from the closed table so the clear is one operation rather than a walk.
    reduction_local: HashMap<Term, Term>,
    /// A registered refinement key against the canonical form the escalation compares it at (`reduce::canonical_scrutinee`: head verbatim, arguments and operands in weak-head normal form).
    ///
    /// **Per *key*, where the escalation is per *probe*.** A store entry is recorded as the guard was written and every occurrence the reducer meets has been reduced, so the two meet only through a canonical form — which nothing but reduction produces. Recomputing it at each probe re-derives one guard's subject once per node of that operation in the declaration, and a subject that reduces to a *stuck* form is not held by the reduction table either, since that one caches reducts rather than the walks that failed to settle. Filled on the first escalation that needs it, so a guard whose fact is never probed against still costs nothing to register — the property `reduce::shallow_scrutinee` exists to keep.
    ///
    /// A key the allowance stopped short is memoized *as itself*, so a bail is paid once rather than at every probe.
    ///
    /// Derived by reduction, so it is invalidated wherever a reduct is.
    canonical_keys: HashMap<Term, Term>,
    elaboration: HashMap<ElaborationKey, (Term, Term)>,
    /// One tick per *write* to any kernel store — definitions, refinements, assumptions, name/metavariable minting, solves, parked/deferred work, the witness table. `Context::get_or_init_elaborated` snapshots it around a candidate sub-elaboration: an unchanged stamp certifies the run was pure (replaying it would be the identity on the context), which is what makes skipping the replay on a later cache hit sound.
    mutation_stamp: Entropy,
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

    pub(crate) fn reduction_get(&self, term: &Term) -> Option<Term> {
        if term.has_local_free() {
            return self.reduction_local.get(term).cloned();
        }
        if let Some(reduct) = self.reduction.get(term) {
            return Some(reduct.clone());
        }
        let key = self.reduction_erased.get(&term.erased_universes())?;
        let reduct = self.reduction.get(key)?;
        // An entry recording that a term is its own weak-head form says the same of every spelling the door equates with it, and the asking spelling is the one to hand back: the stored one is another declaration's, and a Π-type served in its place carried that declaration's binder names into a report about this one.
        if reduct == key {
            curios_profile::sample!("reduction::across_levels", 1);
            return Some(term.clone());
        }
        let adapted = adapted_across_levels(key, term, reduct);
        curios_profile::sample!("reduction::across_levels", u64::from(adapted.is_some()));
        adapted
    }

    pub(crate) fn reduction_insert(&mut self, term: Term, reduct: Term) {
        if term.has_local_free() {
            self.reduction_local.insert(term, reduct);
            return;
        }
        self.reduction_erased
            .entry(term.erased_universes())
            .or_insert_with(|| term.clone());
        self.reduction.insert(term, reduct);
    }

    /// Every reduct, whichever table holds it. The two tables differ in lifetime and in what prices them, never in what invalidates them: a write that could change a reduct changes a local-bearing one exactly as it changes a closed one, so every protocol below that clears reducts clears both through this, and [`Caches::retain_reductions_without`] retains on both.
    fn clear_reductions(&mut self) {
        self.reduction.clear();
        self.reduction_erased.clear();
        self.reduction_local.clear();
    }

    /// A new declaration: the tables whose entries cannot outlive one are discarded. The local-bearing reducts, whose keys name binders no later declaration can mention, and the canonical refinement keys, which are facts about one declaration's arms.
    pub(crate) fn begin_declaration(&mut self) {
        self.reduction_local.clear();
        self.canonical_keys.clear();
    }

    pub(crate) fn canonical_key_get(&self, key: &Term) -> Option<Term> {
        self.canonical_keys.get(key).cloned()
    }

    pub(crate) fn canonical_key_insert(&mut self, key: Term, canonical: Term) {
        self.canonical_keys.insert(key, canonical);
    }

    pub(crate) fn elaboration_get(
        &self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
    ) -> Option<(Term, Term)> {
        self.elaboration
            .get(&ElaborationKey {
                term: term.clone(),
                expected: expected.cloned(),
                privacy_checked,
            })
            .cloned()
    }

    pub(crate) fn elaboration_insert(
        &mut self,
        term: &Term,
        expected: Option<&Term>,
        privacy_checked: bool,
        result: &(Term, Term),
    ) {
        self.elaboration.insert(
            ElaborationKey {
                term: term.clone(),
                expected: expected.cloned(),
                privacy_checked,
            },
            result.clone(),
        );
    }

    /// A counterfactual refinement was registered: a refinement key can be a `#`-free stuck application of globals, so it can have influenced any entry — both caches clear wholesale, and the write is stamped.
    pub(crate) fn invalidate_for_refinement(&mut self) {
        self.note_write();
        self.clear_reductions();
        self.canonical_keys.clear();
        self.elaboration.clear();
    }

    /// A name was *re*defined (or an assumption's universe scheme rewritten in place): the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the name, leaving nothing for a selective retain to key on — both caches clear wholesale, and the write is stamped.
    pub(crate) fn invalidate_for_redefinition(&mut self) {
        self.note_write();
        self.clear_reductions();
        self.canonical_keys.clear();
        self.elaboration.clear();
    }

    /// A name was *freshly* defined. A fresh definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF — so the reduction cache retains every entry whose result does not mention it instead of clearing. The elaboration cache survives untouched: its insert gate already refused every entry naming a not-yet-defined global. No stamp — definition is the one ambient fact a pure run may read, and the settled-globals gate covers it.
    pub(crate) fn retain_reductions_without(&mut self, name: &Free) {
        self.reduction
            .retain(|_, reduct| !reduct.mentions_free(name));
        // The second door indexes exact keys, so it keeps exactly the ones the exact table kept.
        let reduction = &self.reduction;
        self.reduction_erased
            .retain(|_, key| reduction.contains_key(key));
        self.reduction_local
            .retain(|_, reduct| !reduct.mentions_free(name));
        // A canonical key is a reduct of the same kind, retained by the same test.
        self.canonical_keys
            .retain(|_, canonical| !canonical.mentions_free(name));
    }

    /// An assumption's type was replaced in place (`reassume`): an entry elaborated between a `rec` group's lowered `assume` and this upgrade could embed the lowered signature, so the elaboration cache clears; reducts never read assumption types, so the reduction cache survives. Stamped.
    pub(crate) fn invalidate_for_reassumption(&mut self) {
        self.note_write();
        self.elaboration.clear();
    }

    /// A local frame was dropped. A dropped refinement can have influenced any entry, so both caches clear; a dropped frame *definition* clears only the reduction cache — `reduce_let` defines under written binder labels a reduct can fold in, while elaboration-position terms name only `/`-qualified globals and `#`-minted locals, so no elaboration entry can reference a written frame label. No stamp: the frame's own writes were stamped when they landed.
    pub(crate) fn invalidate_frame_exit(
        &mut self,
        dropped_refinements: bool,
        dropped_definitions: bool,
    ) {
        if dropped_refinements {
            self.clear_reductions();
            self.canonical_keys.clear();
            self.elaboration.clear();
        } else if dropped_definitions {
            self.clear_reductions();
            self.canonical_keys.clear();
        }
    }

    /// A refinement-suppression boundary is being crossed with refinements registered: refinement-applied and refinement-suppressed reducts must never contaminate each other's cache, so both clear — on both sides of the bracket, unstamped (the flag flip itself writes nothing).
    pub(crate) fn invalidate_suppression_boundary(&mut self) {
        self.clear_reductions();
        self.canonical_keys.clear();
        self.elaboration.clear();
    }

    /// Universe levels were rewritten in place (defaulting, finalization, instance closure): cached reducts and elaborations may embed the pre-rewrite levels, so both clear. The solver write itself is stamped by the `UniverseMutation` guard.
    pub(crate) fn invalidate_for_universe_rewrite(&mut self) {
        self.clear_reductions();
        self.canonical_keys.clear();
        self.elaboration.clear();
    }

    /// Solutions were rolled back — the one *un*-monotonic store transition. Reducts may have been cached through the unwound solutions, so both caches clear and both stamps tick.
    pub(crate) fn invalidate_for_rollback(&mut self) {
        self.note_write();
        self.note_universe_write();
        self.clear_reductions();
        self.canonical_keys.clear();
        // Entries are metavar-free on both key and value, so an un-solve cannot invalidate them in principle; cleared anyway while the rollback bracket is young — conservative and cheap.
        self.elaboration.clear();
    }

    /// The elaboration island changed (a new top-level item): representation-privacy checks are island-relative, so an entry elaborated under one item's island must not answer for another's. Reducts are island-independent and survive.
    pub(crate) fn invalidate_for_island_change(&mut self) {
        self.elaboration.clear();
    }

    /// Universe constraints were discarded at a transaction boundary with actual solver-state change: elaboration entries may have certified purity against constraints that no longer exist.
    pub(crate) fn invalidate_for_universe_transaction(&mut self) {
        self.elaboration.clear();
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

    rewrite_universe_levels_scoped(reduct, move |_, level| {
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
