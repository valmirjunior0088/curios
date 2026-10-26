//! The context's doors to reduction's caches and to the protocol by which reduction asks conversion.

use {
    super::{ElabProbe, ElaborationStamp, Footing},
    crate::{AskedAt, Context, Settled, Sort, UniverseStateToken},
    curios_analysis::Classification,
    curios_core::{Bound, ClosedMemo, Free, Level, ReduceError, Term},
    std::rc::Rc,
};

impl<'s> Context<'s> {
    /// The closed machine's memo for the reduction in progress.
    pub fn closed_memo_mut(&mut self) -> &mut ClosedMemo {
        let plain = self.typing.asking.plain();

        self.typing.caches.closed_memo(plain)
    }

    /// The read half of the reduction cache. The reducer probes it wherever a term's reduction begins — at entry, and at the scrutinee stack's frame push, where a warm scrutinee dispatches in place instead of framing.
    ///
    /// **A universe metavariable in the term does not exclude it.** Reduction is parametric in levels — no rule reads one, `Type u` being a payload and not a scrutinee, and an instantiation substitutes a parameter with whatever level stands in the instance, a metavariable included — so a reduct is the same function of its term whether the metas in it are solved or not, and the in-place rewrites that do change what a level *spells* (defaulting, finalization, instance closure) clear the cache where they happen. Excluding one would remember nothing of a web of universe-polymorphic definitions inside the declaration that instantiates them, whose occurrences all carry that declaration's level metas, and a reduct shared as a graph would be re-derived once per occurrence — the `2^n` `curios`' `scrutinee_refinement_measurements` guards under `numeric, proved`.
    pub fn cached_reduced(&mut self, term: &Term) -> Option<Term> {
        self.typing
            .caches
            .reduction_get(term, self.typing.asking.plain())
    }

    /// Record that `term` reduces to `result` — the write half of the reduction cache, hit wherever a reduction's value lands: the reducer's final return, and its scrutinee stack's frame pop. Memoize only closed terms whose WHNF names no *unsolved* term metavariable — `any_metavar` bails on the first one, never building the id set. A solve is monotonic, so it can only invalidate a reduct that still names the metavariable it solved, and reduction gets stuck on (hence surfaces) an unsolved metavariable it actually depends on. Refusing to cache those is what lets `solve_metavar` skip a cache clear; an entry naming only *solved* metavariables stays valid under forward solves (re-validation's `rollback_solutions`, which *un*-solves, clears separately). A *universe* metavariable excludes nothing, for the reason [`Context::cached_reduced`] gives.
    ///
    /// **Stored for nothing.** The entry dies with the declaration — [`Caches::begin_declaration`](super::Caches::begin_declaration) clears the table where the budget is restored — so the work budget that built it is its bound.
    pub(crate) fn reduce(&mut self, term: Term, result: &Term) {
        let cacheable =
            term.closed() && !result.any_metavar(&mut |id| self.metavar_solution(id).is_none());

        if cacheable {
            self.typing
                .caches
                .reduction_insert(term, result.clone(), self.typing.asking.plain());
        }
    }

    /// What classifying the stuck operation `term` found, where it is remembered ([`Caches::classification`](super::Caches::classification)).
    pub(crate) fn classification(&self, term: &Term) -> Option<Rc<Classification>> {
        self.typing.caches.classification(term)
    }

    /// Remember what classifying the stuck operation `term` found, unless a question was declined for a commit since `declined` was read, where the classing began: a classification its conversion classed across a declined question is one a later solution can change, as a reduct taken across one is (`reduce`'s `remember`). One no question entered reads the operation alone and is kept.
    pub fn keep_classification(
        &mut self,
        term: Term,
        classification: Classification,
        declined: u64,
    ) {
        if term.closed() && (!classification.classed() || self.declined() == declined) {
            self.typing
                .caches
                .classification_insert(term, classification);
        }
    }

    /// What `term` was forced to, under the reduction in progress ([`Caches::forced`](super::Caches::forced)).
    pub(crate) fn cached_forced(&self, term: &Term) -> Option<Term> {
        self.typing
            .caches
            .forced_get(term, self.typing.asking.plain())
    }

    /// Record that `term` is forced to `value`: [`Context::reduce`] for the forced demand, under the same gate.
    pub(crate) fn remember_forced(&mut self, term: Term, value: &Term) {
        let cacheable =
            term.closed() && !value.any_metavar(&mut |id| self.metavar_solution(id).is_none());

        if cacheable {
            self.typing
                .caches
                .forced_insert(term, value.clone(), self.typing.asking.plain());
        }
    }

    /// The remembered sort of `type_`, where the context stands as it did when it was filed. [`Caches::sorts`](super::Caches::sorts) carries how long that is.
    pub(crate) fn cached_sort(&mut self, type_: &Term) -> Option<Sort> {
        self.typing
            .caches
            .sort_get(type_, self.typing.asking.plain())
    }

    /// Remember `type_`'s sort. Stored for nothing, as a reduct is.
    pub fn record_sort(&mut self, type_: Term, sort: Sort) {
        self.typing
            .caches
            .sort_insert(type_, self.typing.asking.plain(), sort);
    }

    /// What [`is_erasable_in`](crate::is_erasable_in) answered of the closed `type_`, where no scrutinee refinement stands ([`Caches::erasable`](super::Caches::erasable) carries why, and how long an answer is kept).
    pub(crate) fn remembered_erasable(&mut self, type_: &Term) -> Option<bool> {
        if self.typing.frames.has_scrutinee_refinements() {
            return None;
        }
        self.typing
            .caches
            .erasable_get(type_, self.typing.asking.plain())
    }

    /// Remember what [`is_erasable_in`](crate::is_erasable_in) answered of the closed `type_`, where no scrutinee refinement stands.
    pub(crate) fn remember_erasable(&mut self, type_: Term, erasable: bool) {
        if !self.typing.frames.has_scrutinee_refinements() {
            self.typing
                .caches
                .erasable_insert(type_, self.typing.asking.plain(), erasable);
        }
    }

    /// [`Context::kernel_spelling`] of a recorded scrutinee, remembered where no metavariable in it is left unsolved ([`Caches::kernel_spellings`](super::Caches::kernel_spellings)).
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
    pub fn cached_proof(&self, name: &Free) -> Option<bool> {
        self.typing.caches.proof_get(name)
    }

    /// Remember whether the binder `name` is a proof, for the declaration.
    pub fn remember_proof(&mut self, name: Free, proof: bool) {
        self.typing.caches.proof_insert(name, proof);
    }

    /// The settled reduced spelling of the scrutinee entry `key` registered in `frame`: `None` if no probe has asked for it yet, `Some(None)` if reducing it refused. [`Frame::scrutinee_spellings`](super::Frame::scrutinee_spellings) carries how long one stands.
    pub(crate) fn settled_key(&self, frame: usize, key: &Term) -> Option<Option<Settled>> {
        let spelling = self
            .typing
            .frames
            .spelling(frame, key, self.typing.asking.plain())?;

        spelling.settled_while(|footing| self.stands(footing))
    }

    /// Record a settlement, on what it rests on ([`Context::footing`]). Every one is recorded, whatever its metavariables: the loop that asks the reduced spellings settles the innermost entry *not yet asked* (`reduce`'s `refined_reduct`), so an unrecorded settlement would be asked for again forever.
    pub(crate) fn record_settled_key(
        &mut self,
        frame: usize,
        key: Term,
        settled: Option<Settled>,
        footing: Footing,
    ) {
        self.typing.frames.settle_spelling(
            frame,
            key,
            self.typing.asking.plain(),
            settled,
            footing,
        );
    }

    /// What an answer taken now rests on beyond the frames it is read under ([`Footing`]): the solutions committed, and the definitions in view. `unsolved` says what it was taken of held an unsolved metavariable, so a solution committed since moves it; `levels` is the universe solver's state where a question was declined for a commit on the way, which moves it once either solver moves; `universes` says it holds a universe metavariable, which a rewrite of levels moves; `asked` says a question was put to conversion on the way, whose answer goes where its level constraints are withdrawn.
    pub(crate) fn footing(
        &self,
        unsolved: bool,
        levels: Option<UniverseStateToken>,
        universes: bool,
        asked: bool,
    ) -> Footing {
        self.typing.caches.footing(
            self.underway.attempt.solutions.prefix(),
            unsolved || levels.is_some(),
            levels,
            universes,
            asked,
        )
    }

    /// [`Footing::stands`] against the stores in force.
    pub(super) fn stands(&self, footing: &Footing) -> bool {
        footing.stands(
            &self.underway.attempt.solutions,
            &self.underway.attempt.universe_solver,
            &self.typing.caches,
        )
    }

    /// How many questions reduction has put to conversion.
    pub fn questions(&self) -> u64 {
        self.typing.asking.questions()
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
    /// **An oracle's answer is its verdict.** Its three callers — `convert`'s re-validation of a candidate, `suggest`'s check of one, a test — read `Ok` or `Err` and discard the elaborated term, so what a hit must preserve is the verdict alone. Within one bracket the context only accumulates — every rollback clears the table ([`Caches::invalidate_for_rollback`](super::Caches::invalidate_for_rollback)), and a nested bracket keeps a table of its own — and a local-free, metavariable-free term reads nothing a later point in the bracket could have changed; so elaborating it again against the same expectation would repeat the first run's writes — mint its own fresh metavariables, solve them alike, add level constraints already present — and reach the same verdict, which is what skipping it hands back. The cache's purity gate exists because its entries outlive the run that made them; these do not outlive the bracket.
    ///
    /// **What it is for.** A candidate is a reduct, and a reduct is a graph whose tree can be exponential in its depth — a text position built a character at a time mentions the one before it four times. Elaborating one writes — the level constraints its instances raise, the metavariables its implicit arguments mint — so the cache keeps almost none of it, and without this table re-validation checks the tree: re-validating a three-character `Str/trim` claim stated in a type asks for some hundred thousand elaborations, most of them misses the cache declines to record, and runs out of steps.
    fn oracle_memoizable(&self, term: &Term, expected: Option<&Term>) -> bool {
        let closed = |t: &Term| !t.has_metavar() && !t.has_local_free();
        self.typing.caches.in_oracle() && closed(term) && expected.is_none_or(closed)
    }

    /// Read half of the elaboration cache (see [`Context::get_or_init_elaborated`] for the full contract). Applies the O(1) groundness gate, then either answers from the cache (`Hit`), reports the term ineligible (`Uncacheable`), or snapshots `mutation_stamp` for the caller to thread back into [`record_elaborated`](Self::record_elaborated) (`Miss`). Pure: it never mutates the context, so a driver may probe speculatively at a frame push.
    fn probe_elaborated(&self, term: &Term, expected: Option<&Term>) -> ElabProbe {
        let ground = |t: &Term| {
            !t.has_metavar()
                && !self.underway.attempt.universe_solver.has_unsolved_meta(t)
                && !t.has_local_free()
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
    fn record_elaborated(
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
            !t.has_metavar()
                && !self.underway.attempt.universe_solver.has_unsolved_meta(t)
                && !t.has_local_free()
        };
        let settled = |t: &Term| t.free_vars().iter().all(|name| self.is_defined(name));
        self.typing.caches.stamps_unchanged(stamp)
            && ground(&result.0)
            && ground(&result.1)
            && settled(&result.0)
            && settled(&result.1)
            && expected.is_none_or(settled)
    }

    /// Whether reduction is plain: it asks the elaborator's conversion nothing ([`Context::plainly`]).
    pub fn plain(&self) -> bool {
        self.typing.asking.plain()
    }

    /// A judgment's reduction is putting a question to conversion. Its answer rests on the level constraints that stand, so a scope that is rolled back with constraints withdrawn after one was asked inside it clears what was reduced meanwhile ([`Context::rollback_solutions`]).
    pub fn note_question(&mut self) {
        self.typing.asking.note_question();
    }

    /// A question asked with nothing committed was answered no because answering yes would have committed a solution or a level constraint. The answer can change once the solver moves, so no reduct taken across it is remembered (`reduce`'s `remember`).
    pub fn note_declined(&mut self) {
        self.typing.asking.note_declined();
    }

    /// How many questions have been declined for a commit.
    pub fn declined(&self) -> u64 {
        self.typing.asking.declined()
    }

    /// [`Asking::note_apart`](crate::Asking::note_apart).
    pub(crate) fn note_apart(&mut self) {
        self.typing.asking.note_apart();
    }

    /// Where the context stands as a question is asked with nothing committed: what its answer is held against before it is filed ([`Context::remember_same`]).
    pub fn asked_at(&self) -> AskedAt {
        AskedAt {
            stamps: self.typing.caches.stamps(),
            declined: self.typing.asking.declined(),
            apart: self.typing.asking.apart(),
        }
    }

    /// What `this` and `that` were answered to be, asked with nothing committed, where the context stands as it did then ([`Caches::same`](super::Caches::same)).
    pub(crate) fn remembered_same(&mut self, this: &Term, that: &Term) -> Option<bool> {
        self.typing
            .caches
            .same_get(this, that, self.typing.asking.plain())
    }

    /// Remember what the question asked at `asked` answered, where it is a verdict of the two terms alone: it wrote nothing, so answering it again from the table leaves the context as asking it again would; no question inside it was declined for a commit, which the solver's next move could answer otherwise; and no pair was left apart inside its own unfolding, a verdict of that path, which is the rule the kernel files its own by.
    pub fn remember_same(&mut self, asked: &AskedAt, this: &Term, that: &Term, same: bool) {
        if self.typing.caches.stamps_unchanged(&asked.stamps)
            && self.typing.asking.declined() == asked.declined
            && self.typing.asking.apart() == asked.apart
        {
            self.typing.caches.same_insert(
                this.clone(),
                that.clone(),
                self.typing.asking.plain(),
                same,
            );
        }
    }

    /// Enter or leave a question asked with nothing committed (`same_uncommitted`), answering what stood before: one is entered with `Some(false)`, and what leaving it reads back says whether a level equation was withheld inside it.
    pub fn withhold(&mut self, withheld: Option<bool>) -> Option<bool> {
        self.typing.asking.withhold(withheld)
    }

    /// Whether an equation of `left` and `right` is withheld: conversion is answering a question asked with nothing committed, which joins no two levels, and the store does not already read the two as one. One withheld is noted on the question, whose no the solver can then change.
    pub fn withholds(&mut self, left: &Level, right: &Level) -> Result<bool, ReduceError> {
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

    /// Run `read` with reduction plain: it asks the elaborator's conversion nothing, at a stuck fold, at a missed equation or at a stuck elimination's atoms — the kernel's `Kernel::plainly`, for the kernel's two readers. A question reduction puts to conversion is answered by it, which is what ends the regress, and a shared analysis reads a term by it (`Env::force`), since totality may rest on no verdict of conversion's. Each of the two keeps its own answers: the reducts, the sorts read through them and the settled spellings (`Frame::scrutinee_spellings`) are all filed by which reduction took them.
    pub fn plainly<R>(&mut self, read: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.typing.asking.set_plain(true);
        let answer = read(self);
        self.typing.asking.set_plain(previous);

        answer
    }
}
