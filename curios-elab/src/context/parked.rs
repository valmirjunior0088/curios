//! The context's doors to parked work: what waits on an unsolved metavariable and the frames it waits under, and the brackets that suppress, report or decline parking.

use {
    super::{Bracket, MetaKind},
    crate::{
        AwaitId, Context, Error, HeldFrames, ParkedChoice, ParkedConversion, ParkedProblem,
        ParkedWork, ReaderScope, Scoped,
    },
    curios_core::{Bound, Extent, MetavarId, MetavarOrigin, NumLit, Term},
    std::collections::{BTreeMap, BTreeSet},
};

impl<'s> Context<'s> {
    /// [`Frames::hold`](super::Frames::hold): the frames above the base as they stand, which a parked problem takes where it parks and a metavariable where it is born.
    pub(crate) fn hold_frames(&mut self) -> HeldFrames {
        self.typing.frames.hold()
    }

    /// Run `work`, a judgment made for what holds `held`, standing on those frames: the live frames above the base are set aside, the held ones stand in their place, `work` runs in a frame of its own above them, and the live ones come back. A parked problem's retry is this, decided in exactly the context the problem parked in.
    ///
    /// A retry is scheduled wherever a solution lands — a turnaround inside the same item, a nested or sibling arm, the drain after the item — and the live context at that point is not the problem's. Running on top of it would go wrong both ways: an arm's re-typed local would lose to the same name live at its unspecialized type when the retry runs outside the arm, and a nested arm's re-typings and refinements would decide a problem from an arm outside it. Standing on the frames held makes the verdict a function of the problem and its context alone, which is what parking promises.
    ///
    /// **Nothing is assumed, defined or registered again**, so the frame an entry stands in is the frame it was registered in: two guards, one read under the other, stand one under the other, and a guard's reduced spelling is the one its frame settled. What `work` writes is written to its own frame and goes with it, and what parks or is born meanwhile holds the frames stood on and that frame.
    ///
    /// **The caches are told what a context rebuilt told them.** A reduct taken with a refinement in view must not answer where it is not, nor the other way round, and neither must one taken under a definition the frames held state otherwise than the live context does. So where the frames set aside or stood on hold a refinement, whether or not they are the same frames, or define a name otherwise, the stand is a bracket: the live window's reducts are set aside and handed back where it ends ([`Caches::enter_window`](super::Caches::enter_window)), what is reduced standing on the held frames is its own, and the elaborations clear at both ends. A definition the held frames bring into view that the live context lacks is a fresh one to the reducts stuck on its absence, and goes at the end as a frame's definitions go; and the binders in view are a write.
    pub(crate) fn standing_on<R>(
        &mut self,
        held: &HeldFrames,
        work: impl FnOnce(&mut Self) -> R,
    ) -> R {
        curios_profile::profile!("ctx::standing_on");
        let set_aside_refined = self.typing.frames.locally_refined();
        let stood_on_refined = held.refined();
        // What the frames held define that the frames in view do not, or define otherwise.
        let mut fresh = Vec::new();
        let mut redefined = false;
        for (name, entry) in self.typing.frames.held_definitions_apart(held) {
            match self.typing.frames.definition(&name) {
                Some(live) if live.body() == entry.body() && live.kind() == entry.kind() => {}
                Some(_) => redefined = true,
                None => fresh.push(name),
            }
        }

        let windowed = set_aside_refined || stood_on_refined || redefined;
        if windowed {
            self.typing.caches.enter_window(Bracket::Stand, None);
            self.typing.caches.invalidate_for_stand();
        }
        if held.binds() || stood_on_refined || redefined {
            self.typing.caches.note_write();
        }
        if redefined {
            self.typing.caches.note_redefinition();
        }
        for name in &fresh {
            self.typing.caches.retain_reductions_without(name);
            self.typing.frames.forget_spellings_naming(name);
        }

        let aside = self.typing.frames.stand_on(held);
        let result = self.with_frame(work);
        self.typing.frames.stand_back(aside);

        match windowed {
            true => {
                self.typing.caches.leave_window();
                self.typing.caches.invalidate_for_stand();
            }
            false => self
                .typing
                .caches
                .invalidate_frame_exit(false, !fresh.is_empty()),
        }

        result
    }

    /// Read `body` as a term that awaits: a metavariable born while it runs is awaited by it, and once it has read its parts and met its expectation it takes the choices left among the problems it awaits ([`Context::complete`]). A refusal completes nothing.
    pub fn awaiting<R>(
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
    pub fn retried_under<R>(
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

    /// [`Solutions::awaits_any`](super::Solutions::awaits_any).
    pub(crate) fn awaits_any(&self, term: &Term) -> bool {
        self.underway.attempt.solutions.awaits_any(term)
    }

    /// Have the term being read await the unsolved metavariables of `term` that no standing term awaits ([`Solutions::adopt`](super::Solutions::adopt)).
    pub(crate) fn adopt(&mut self, term: &Term) {
        if term.has_metavar() {
            self.underway.attempt.solutions.adopt(term.metavars());
        }
    }

    /// [`Solutions::waiting_numerals`](super::Solutions::waiting_numerals).
    pub(crate) fn waiting_numerals(&self) -> BTreeMap<MetavarId, Vec<NumLit>> {
        self.underway.attempt.solutions.waiting_numerals()
    }

    /// [`Solutions::choices_awaited_by`](super::Solutions::choices_awaited_by).
    pub(crate) fn choices_awaited_by(
        &self,
        term: Option<AwaitId>,
    ) -> (Vec<ParkedChoice>, Vec<BTreeSet<MetavarId>>) {
        self.underway.attempt.solutions.choices_awaited_by(term)
    }

    /// [`Solutions::conversions_awaited_by`](super::Solutions::conversions_awaited_by).
    pub(crate) fn conversions_awaited_by(&self, term: Option<AwaitId>) -> Vec<ParkedConversion> {
        self.underway.attempt.solutions.conversions_awaited_by(term)
    }

    /// [`Solutions::drop_conversions`](super::Solutions::drop_conversions).
    pub(crate) fn drop_conversions(&mut self, at: &BTreeSet<u64>) {
        self.underway.attempt.solutions.drop_conversions(at);
    }

    /// [`Solutions::witnesses_awaited_by`](super::Solutions::witnesses_awaited_by).
    pub(crate) fn witnesses_awaited_by(&self, term: Option<AwaitId>) -> Vec<MetavarId> {
        self.underway.attempt.solutions.witnesses_awaited_by(term)
    }

    /// [`Solutions::take_witness`](super::Solutions::take_witness).
    pub(crate) fn take_witness(&mut self, slot: MetavarId) -> Option<ParkedProblem> {
        self.underway.attempt.solutions.take_witness(slot)
    }

    /// [`Solutions::choice_waiting_on`](super::Solutions::choice_waiting_on).
    pub(crate) fn choice_waiting_on(&self, id: MetavarId) -> Option<ParkedChoice> {
        self.underway.attempt.solutions.choice_waiting_on(id)
    }

    /// [`Solutions::note_shaped`](super::Solutions::note_shaped).
    pub(crate) fn note_shaped(&mut self, choice: ParkedChoice) {
        self.underway.attempt.solutions.note_shaped(choice);
    }

    /// [`Solutions::take_shaped`](super::Solutions::take_shaped).
    pub(crate) fn take_shaped(&mut self) -> Vec<ParkedChoice> {
        self.underway.attempt.solutions.take_shaped()
    }

    /// Park blocked work: hold the local frames as they stand around it and record which unsolved metavariables could unblock it.
    pub fn park(&mut self, work: ParkedWork, origin: Term) {
        curios_profile::profile!("ctx::park");
        let frames = self.hold_frames();
        self.repark(work, origin, frames);
    }

    /// Re-park work that is still blocked after a retry, with the frames it held ([`Solutions::park`](super::Solutions::park)), stamping the write.
    pub(crate) fn repark(&mut self, work: ParkedWork, origin: Term, frames: HeldFrames) {
        self.typing.caches.note_write();
        self.underway.attempt.solutions.park(work, origin, frames);
    }

    /// Mint the placeholder metavariable for a parked checking problem: birthed like any hole — frozen Γ, identity spine — with no insertion provenance, and of its own kind, since its check is what fills it ([`MetaKind::Placeholder`]). If it survives unsolved, the item drain reports the parked problem at its origin before zonk could ever meet the placeholder.
    pub(crate) fn fresh_placeholder(
        &mut self,
        result: Term,
        extent: Option<Extent>,
    ) -> (MetavarId, Term) {
        let id = self.underway.attempt.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar_as(id, telescope, Scoped::Held, result, MetaKind::Placeholder);
        let term = Term::metavar_birthed(id, MetavarOrigin::Hole, spine);

        let term = match extent {
            Some(extent) => term.with_extent(extent),
            None => term,
        };

        (id, term)
    }

    pub fn wake_parked(&mut self) -> Vec<ParkedProblem> {
        curios_profile::profile!("ctx::wake_parked");
        self.underway.attempt.solutions.wake_parked()
    }

    pub fn take_parked(&mut self) -> Vec<ParkedProblem> {
        self.underway.attempt.solutions.take_parked()
    }

    /// [`Solutions::take_checking`](super::Solutions::take_checking).
    pub(crate) fn take_checking(&mut self, placeholder: MetavarId) -> Option<ParkedProblem> {
        self.underway.attempt.solutions.take_checking(placeholder)
    }

    pub fn parked_len(&self) -> usize {
        self.underway.attempt.solutions.parked_len()
    }

    pub fn has_newly_solved(&self) -> bool {
        self.underway.attempt.solutions.has_newly_solved()
    }

    pub fn parking_suppressed(&self) -> bool {
        self.underway.attempt.solutions.parking_suppressed()
    }

    /// [`Solutions::imitates`](super::Solutions::imitates).
    pub(crate) fn imitates(&self) -> bool {
        self.underway.attempt.solutions.imitates()
    }

    /// Run `work`, a `!` reading its region's monad, with an application of an unsolved metavariable imitated where conversion meets one: the region is the `!`'s expectation, so nothing read later bears on what its monad is.
    pub fn imitating<R>(&mut self, work: impl FnOnce(&mut Self) -> R) -> R {
        let before = self.underway.attempt.solutions.set_imitating(true);
        let result = work(self);
        self.underway.attempt.solutions.set_imitating(before);

        result
    }

    /// [`Solutions::holds_spines`](super::Solutions::holds_spines).
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
    pub fn may_park(&mut self) -> bool {
        self.underway.attempt.solutions.may_park()
    }

    /// Whether a site inside the oracle in progress wanted to park and was refused. Read inside the oracle, by a reader with three answers: where the question failed and this holds, the failure is undecided rather than a refusal. It errs one way only — a check that recovered from a declined park and then failed on something rigid reads as undecided, and the rigid failure is reported when the work is retried.
    pub fn declined_to_park(&self) -> bool {
        self.underway.attempt.solutions.declined_to_park()
    }

    /// Run `f` as a yes/no *oracle* around full elaboration — a solution's re-validation, a goal candidate's fit. Parking is suppressed — `expect` treats `Blocked` as a mismatch and `retry_parked` is a no-op, so provisional success can neither leak into the verdict nor consume a parked obligation whose error the oracle would swallow — and so are the representation-privacy checks: an oracle candidate is a unification artifact that can embed machinery-built projections (eta-expansions, witness splices) whose privacy elaboration already adjudicated, and a swallowed privacy error would silently flip the verdict. The suppressions are a package: an oracle that set only some would be subtly unsound, which is why the parking half has no public setter. What the parking half suppressed is recorded for the oracle's own reader ([`Context::may_park`]): a goal candidate's fit and an entailment read a failure as no, and re-validation, which has a third answer, reads a failure where a park was declined as not judged yet. The bracket also keeps an elaboration table of its own, for what only a verdict may reuse — see [`Context::oracle_memoizable`]. Where the verdict is one of a metavariable, the oracle is asked where the metavariable was born ([`Context::at_birth`]).
    pub(crate) fn with_oracle<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.typing.caches.begin_oracle();
        let result = self.with_suppressed_parking(|context| context.with_suppressed_privacy(f));
        self.typing.caches.end_oracle();

        result
    }

    fn with_suppressed_parking<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let enclosing = self.underway.attempt.solutions.enter_oracle();
        let result = f(self);
        self.underway.attempt.solutions.leave_oracle(enclosing);

        result
    }

    /// Run `question`, what a report asks of a term it shows, as an oracle ([`Context::with_oracle`]), with every solution it lands rolled back: the answer is all it leaves.
    pub(crate) fn asking<R>(&mut self, question: impl FnOnce(&mut Self) -> R) -> R {
        self.with_oracle(|context| {
            let mark = context.solution_mark();
            let answered = question(context);
            context.rollback_solutions(mark);
            context.end_solutions(mark);

            answered
        })
    }

    /// Run `work`, a report's own judgment of the terms it shows, leaving the attempt under way as it found it: what `work` needs of a declaration that has not finished does not void the attempt, and what it asks of the declarations around it is no part of the attempt's trace. The budget is restored for it, as for a declaration's.
    pub fn reporting<R>(&mut self, work: impl FnOnce(&mut Self) -> R) -> R {
        let needed = self.underway.needs.take();
        let traced = self.typing.traced.set_aside();
        self.restore_budget();
        let result = work(self);
        self.underway.needs.put_back(needed);
        self.typing.traced.put_back(traced);

        result
    }

    /// Where a report about the judgment under way stands: the witness binders in scope, and the frames standing, which its terms are settled on ([`Error::settled`]).
    pub(crate) fn reader_scope(&mut self) -> ReaderScope {
        let witnesses = self
            .witness_scope()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>();

        ReaderScope::held(witnesses, self.hold_frames())
    }
}
