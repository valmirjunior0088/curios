//! The unification state: metavariable birth records and solutions, the solve journal, and the parked-work store.
//!
//! Everything here is frame-independent — a metavariable's record freezes the Γ it was born under, and parked work freezes the frame it blocked in, so neither is touched by `enter_frame`/`leave_frame`. Writes that must reach the cache stamps are stamped by the `Context` façade; the transactional watermark (`SolutionMark`) also stays there, because it spans this store and the universe solver together.

use {
    super::{FrozenFrame, SharedTelescope},
    crate::Goal,
    curios_base::Entropy,
    curios_core::{Bound, MetaId, Metavar, MetavarOrigin, Subterm, Term, WitnessOrigin},
    std::{collections::BTreeSet, mem},
};

/// One metavariable's record. Everything here is frozen at birth except `solution`, which transitions `None -> Some(_)` on solve — rolled back to `None` if re-validation rejects the candidate that solved it (`Context::rollback_solutions`).
#[derive(Debug)]
pub(crate) struct MetaEntry {
    /// Γ frozen at birth: the local assumption context in binding order, with birth-time types. Drives the scope check and re-validation.
    pub telescope: SharedTelescope,
    /// The metavariable's type — the `expected` it was checked against at birth.
    pub result: Term,
    /// `None` while unsolved; `Some(t)` once solved. `t`'s free `Var`s are a subset of `telescope`'s names.
    pub solution: Option<Term>,
    /// Ordinary inference holes may be solved by conversion. Recursive elaboration slots are filled only by the owning `rec` elaborator; conversion treats an unfilled slot as a blocking dependency.
    pub kind: MetaKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MetaKind {
    Inference,
    RecSlot,
}

/// The work a parked problem will retry.
#[derive(Debug)]
pub(crate) enum ParkedWork {
    /// A conversion constraint that quiesced blocked on unsolved metavariables.
    Conversion(Goal),
    /// A whole checking problem: a checked-only introduction form met an expected type whose structure is still an unsolved metavariable. The `placeholder` metavariable stands in for the rebuilt term in the tree and is solved with it once the check can run — the spine machinery then splices it everywhere the occurrence travelled.
    Checking {
        term: Term,
        expected: Term,
        placeholder: MetaId,
    },
    /// A witness-resolution goal whose key type is not yet rigid: `slot` is the metavariable standing in the omitted `use`-argument's place, `goal` its (concept application) type. Woken when a watched metavariable solves; resolution then retries under the frozen frame.
    Witness {
        slot: MetaId,
        goal: Term,
        provenance: WitnessOrigin,
    },
}

/// A problem parked by `expect` (or a blocked intro-form check) to outlive its call. Like a [`MetaEntry`], it freezes the local frame it was born under.
#[derive(Debug)]
pub(crate) struct ParkedGoal {
    pub work: ParkedWork,
    /// The term being checked at park time; its span anchors the eventual error if the problem never resolves.
    pub origin: Term,
    pub frame: FrozenFrame,
    /// The unsolved metavariables whose solutions could unblock this — solving any of them is the wake signal.
    pub watching: BTreeSet<MetaId>,
}

/// The flat, frame-independent unification stores. `Context` holds exactly one of these; see the module documentation for the frame-independence contract.
#[derive(Debug)]
pub(crate) struct Solutions {
    /// Metavariable records indexed by `Metavar::id` — monotonic facts about the program being elaborated, not lexically-scoped bindings.
    entries: Vec<Option<MetaEntry>>,
    /// The next metavariable id this store may mint (implicit-argument insertion). Seeded by `elaborate_module_suffix` with its `metavar_floor` argument so core-minted ids sit strictly above `into_core`'s.
    next_metavar: Entropy<MetaId>,
    /// Parked conversion constraints — frame-independent, like the entries.
    parked: Vec<ParkedGoal>,
    /// Ids solved since the last `wake_parked` sweep: the wake signal.
    newly_solved: Vec<MetaId>,
    /// Journal of every committed solution id, in commit order — never consumed, only marked and rolled back. The watermark/rollback pair lets re-validation unwind solutions that landed while validating a candidate it then rejected.
    solved_log: Vec<MetaId>,
    /// While set, `expect` may not park: conversion is being used as a yes/no oracle (re-validation) and provisional success would leak into it.
    suppress_parking: bool,
    /// Witness goals whose key is rigid but has no table entry *yet*: a later item may register the missing witness (the table is program-wide while items elaborate in order), so these defer — retried after each item, reported as errors only when the whole module has been elaborated.
    deferred_witnesses: Vec<ParkedGoal>,
}

impl Solutions {
    pub(crate) fn new() -> Self {
        Self {
            entries: Vec::new(),
            next_metavar: Entropy::<MetaId>::new(),
            parked: Vec::new(),
            newly_solved: Vec::new(),
            solved_log: Vec::new(),
            suppress_parking: false,
            deferred_witnesses: Vec::new(),
        }
    }

    /// Mint the next metavariable id.
    pub(crate) fn mint(&mut self) -> MetaId {
        self.next_metavar.fresh()
    }

    /// Raise the minting floor: every id [`Solutions::mint`] hands out will be `>= floor`.
    pub(crate) fn seed_floor(&mut self, floor: usize) {
        self.next_metavar.seed(floor);
    }

    /// Materialize a metavariable's birth record. The store grows to cover `id`; births happen exactly once per id (each `_` is distinct and occurs once). The façade stamps the write.
    pub(crate) fn birth(&mut self, id: MetaId, telescope: SharedTelescope, result: Term) {
        self.birth_with_kind(id, telescope, result, MetaKind::Inference);
    }

    /// [`Solutions::birth`] for a recursive-elaboration slot, which only `fill_rec_slot` may solve.
    pub(crate) fn birth_rec_slot(&mut self, id: MetaId, telescope: SharedTelescope, result: Term) {
        self.birth_with_kind(id, telescope, result, MetaKind::RecSlot);
    }

    fn birth_with_kind(
        &mut self,
        id: MetaId,
        telescope: SharedTelescope,
        result: Term,
        kind: MetaKind,
    ) {
        if id.0 >= self.entries.len() {
            self.entries.resize_with(id.0 + 1, || None);
        }
        self.entries[id.0] = Some(MetaEntry {
            telescope,
            result,
            solution: None,
            kind,
        });
    }

    pub(crate) fn entry(&self, id: MetaId) -> Option<&MetaEntry> {
        self.entries.get(id.0).and_then(Option::as_ref)
    }

    pub(crate) fn solution(&self, id: MetaId) -> Option<&Term> {
        self.entry(id).and_then(|entry| entry.solution.as_ref())
    }

    pub(crate) fn is_rec_slot(&self, id: MetaId) -> bool {
        self.entry(id)
            .is_some_and(|entry| entry.kind == MetaKind::RecSlot)
    }

    /// Commit a solution: sets the entry, records the wake signal, and journals for rollback. The façade stamps the write.
    pub(crate) fn solve(&mut self, id: MetaId, term: Term) {
        if let Some(Some(entry)) = self.entries.get_mut(id.0) {
            entry.solution = Some(term);
            self.newly_solved.push(id);
            self.solved_log.push(id);
        }
    }

    /// If `term` is a witness-resolution hole, its provenance and the concept goal it stands for (e.g. `Add(Nat)`). A conversion left stuck between two such holes is really an unresolved witness — reported as one rather than as a bare metavariable mismatch between two anonymous placeholders.
    pub(crate) fn witness_hole(&self, term: &Term) -> Option<(WitnessOrigin, Term)> {
        let Subterm::Metavar(metavar) = &**term else {
            return None;
        };
        let Some(MetavarOrigin::Witness(origin)) = &metavar.origin else {
            return None;
        };
        let goal = self.entry(metavar.id)?.result.clone();
        Some((origin.clone(), goal))
    }

    /// Resolve a solved metavariable *at its occurrence*: the stored solution is spelled with the birth telescope's names, and the occurrence's spine records what each of those binders corresponds to here — so resolution is the solution with birth names rewritten through the spine. An empty spine (a never-rebuilt `into_core` hole) resolves as the identity. `None` while unsolved.
    pub(crate) fn resolve(&self, metavar: &Metavar) -> Option<Term> {
        let entry = self.entry(metavar.id)?;
        let solution = entry.solution.as_ref()?;

        if metavar.spine.is_empty() {
            return Some(solution.clone());
        }

        let binders = entry
            .telescope
            .iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>();

        let spine = metavar.spine.iter().collect::<Vec<_>>();

        Some(solution.capture(&binders).release(&spine))
    }

    /// How many solutions have been committed so far — the term half of `Context::solution_mark`'s watermark.
    pub(crate) fn solved_len(&self) -> usize {
        self.solved_log.len()
    }

    /// Unwind every solution committed since the watermark, removing the unwound ids from the wake signals. The façade clears the caches.
    pub(crate) fn unwind_to(&mut self, solved_len: usize) {
        let unwound = self
            .solved_log
            .split_off(solved_len.min(self.solved_log.len()));

        for id in &unwound {
            #[cfg(feature = "profile")]
            curios_profile::tracing::debug!(target: "curios_elab::solve", meta = id.0, "solution unwound");
            if let Some(Some(entry)) = self.entries.get_mut(id.0) {
                entry.solution = None;
            }
        }

        self.newly_solved.retain(|id| !unwound.contains(id));
    }

    /// Park work under its frozen frame, recomputing the watch set from the work's currently unsolved metavariables.
    pub(crate) fn park(&mut self, work: ParkedWork, origin: Term, frame: FrozenFrame) {
        let watching = match &work {
            ParkedWork::Conversion(goal) => goal
                .this
                .metavars()
                .into_iter()
                .chain(goal.that.metavars())
                .filter(|id| self.solution(*id).is_none())
                .collect(),
            ParkedWork::Checking { expected, .. } => expected
                .metavars()
                .into_iter()
                .filter(|id| self.solution(*id).is_none())
                .collect(),
            ParkedWork::Witness { goal, .. } => goal
                .metavars()
                .into_iter()
                .filter(|id| self.solution(*id).is_none())
                .collect(),
        };

        self.parked.push(ParkedGoal {
            work,
            origin,
            frame,
            watching,
        });
    }

    /// Take the parked goals woken by solutions landed since the last sweep. Consumes the wake signals; empty when nothing new has been solved.
    pub(crate) fn wake_parked(&mut self) -> Vec<ParkedGoal> {
        if self.newly_solved.is_empty() || self.parked.is_empty() {
            self.newly_solved.clear();

            return Vec::new();
        }

        let solved = self.newly_solved.drain(..).collect::<BTreeSet<_>>();

        let (woken, kept) = mem::take(&mut self.parked)
            .into_iter()
            .partition(|parked| parked.watching.iter().any(|id| solved.contains(id)));

        self.parked = kept;

        woken
    }

    /// Take every parked goal — the drain's final sweep.
    pub(crate) fn take_parked(&mut self) -> Vec<ParkedGoal> {
        mem::take(&mut self.parked)
    }

    pub(crate) fn parked_len(&self) -> usize {
        self.parked.len()
    }

    pub(crate) fn has_newly_solved(&self) -> bool {
        !self.newly_solved.is_empty()
    }

    pub(crate) fn parking_suppressed(&self) -> bool {
        self.suppress_parking
    }

    /// Flip the parking-suppression flag, returning the previous state — `Context::with_suppressed_parking`'s bracket intrinsic.
    pub(crate) fn set_parking_suppressed(&mut self, suppressed: bool) -> bool {
        mem::replace(&mut self.suppress_parking, suppressed)
    }

    /// Defer a witness goal whose key is rigid but has no table entry yet. The façade stamps the write.
    pub(crate) fn defer_witness(&mut self, goal: ParkedGoal) {
        self.deferred_witnesses.push(goal);
    }

    /// Take every deferred witness goal for a retry sweep.
    pub(crate) fn take_deferred_witnesses(&mut self) -> Vec<ParkedGoal> {
        mem::take(&mut self.deferred_witnesses)
    }
}
