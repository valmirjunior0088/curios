//! The kernel's two memo tables and the write stamps that police them.
//!
//! The reduction and elaboration caches are sound only under an explicit invalidation protocol: every store write that could change a cached answer must either bump a stamp (so a pending insert is refused), clear a cache, or retain selectively. Those combinations used to be hand-rolled at each mutation site; here each one is a named method carrying its own justification, so a new mutation site chooses a protocol instead of improvising one.
//!
//! The *policies* — what is cacheable, and what a probe's groundness gate admits — stay on `Context`, which alone can read the solution and universe stores they consult. This type owns the storage and the write discipline.

use {
    curios_base::Entropy,
    curios_core::{Free, Term},
    std::collections::HashMap,
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

/// The reduction and elaboration caches with their two write stamps. See the module documentation for the protocol; `Context` holds exactly one of these.
#[derive(Debug, Default)]
pub(crate) struct Caches {
    reduction: HashMap<Term, Term>,
    elaboration: HashMap<ElaborationKey, (Term, Term)>,
    /// Whether a definition reaches an operation the host performs — `curios_cert::carries_effect`'s memo. Unstamped: it reads definition *bodies* only, which is the one ambient fact a pure run may read, and it is refreshed by exactly the two events that can change a body out from under it.
    effects: HashMap<Free, bool>,
    /// One tick per *write* to any kernel store — definitions, refinements, assumptions, name/metavariable minting, solves, parked/deferred work, the witness table. `Context::get_or_init_elaborated` snapshots it around a candidate sub-elaboration: an unchanged stamp certifies the run was pure (replaying it would be the identity on the context), which is what makes skipping the replay on a later cache hit sound.
    mutation_stamp: Entropy,
    /// Monotonic universe-solver writes are tracked separately. Cache entries may survive them only when their keys and results contain no transitively unresolved universe meta; rollback/finalization clears the cache at the non-monotonic boundaries.
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
        self.reduction.get(term).cloned()
    }

    pub(crate) fn reduction_insert(&mut self, term: Term, reduct: Term) {
        self.reduction.insert(term, reduct);
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
        self.reduction.clear();
        self.elaboration.clear();
    }

    /// A name was *re*defined (or an assumption's universe scheme rewritten in place): the old value may sit consumed inside a reduct or an elaboration result that no longer mentions the name, leaving nothing for a selective retain to key on — both caches clear wholesale, and the write is stamped.
    pub(crate) fn invalidate_for_redefinition(&mut self) {
        self.note_write();
        self.reduction.clear();
        self.elaboration.clear();
        self.effects.clear();
    }

    /// Where `curios_cert::carries_effect` remembers what a definition reaches.
    pub(crate) fn effects_mut(&mut self) -> &mut HashMap<Free, bool> {
        &mut self.effects
    }

    /// A name was *freshly* defined. A fresh definition can only unstick reductions that read this name's absence, and a stuck read always leaves the name free in the WHNF — so the reduction cache retains every entry whose result does not mention it instead of clearing. The elaboration cache survives untouched: its insert gate already refused every entry naming a not-yet-defined global. No stamp — definition is the one ambient fact a pure run may read, and the settled-globals gate covers it.
    pub(crate) fn retain_reductions_without(&mut self, name: &Free) {
        self.reduction
            .retain(|_, reduct| !reduct.mentions_free(name));
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
            self.reduction.clear();
            self.elaboration.clear();
        } else if dropped_definitions {
            self.reduction.clear();
        }

        // A frame that dropped definitions leaves entries about bodies no longer in scope. They are unreachable rather than wrong — a `Free` is minted once and never reused — but the purity memo is cleared with the reducts on the same event, so that "what a definition reaches" is never remembered longer than the definition is.
        if dropped_definitions {
            self.effects.clear();
        }
    }

    /// A refinement-suppression boundary is being crossed with refinements registered: refinement-applied and refinement-suppressed reducts must never contaminate each other's cache, so both clear — on both sides of the bracket, unstamped (the flag flip itself writes nothing).
    pub(crate) fn invalidate_suppression_boundary(&mut self) {
        self.reduction.clear();
        self.elaboration.clear();
    }

    /// Universe levels were rewritten in place (defaulting, finalization, instance closure): cached reducts and elaborations may embed the pre-rewrite levels, so both clear. The solver write itself is stamped by the `UniverseMutation` guard.
    pub(crate) fn invalidate_for_universe_rewrite(&mut self) {
        self.reduction.clear();
        self.elaboration.clear();
    }

    /// Solutions were rolled back — the one *un*-monotonic store transition. Reducts may have been cached through the unwound solutions, so both caches clear and both stamps tick.
    pub(crate) fn invalidate_for_rollback(&mut self) {
        self.note_write();
        self.note_universe_write();
        self.reduction.clear();
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
