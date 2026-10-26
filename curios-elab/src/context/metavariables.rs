//! The context's doors to the metavariables: their births, solutions and restrictions, and what an attempt notes of the goals it wrote and of what blocked a solve.

use {
    super::{MetaKind, SharedSpine},
    crate::{
        BirthTelescope, Context, DeadArm, DomainScope, GoalObligation, HeldFrames, MetaEntry,
        Provenance, Question, Refusal, Restriction, Scoped, SolutionMark, metavar_spines,
        read_by_value, zonk_solved_term_metas,
    },
    curios_core::{
        Bound, Extent, Free, ImplicitOrigin, Metavar, MetavarId, MetavarOrigin, MetavarSpine,
        Names, Subterm, Term, WitnessOrigin,
    },
    std::collections::{BTreeMap, BTreeSet},
};

impl<'s> Context<'s> {
    /// [`Solutions::note_solve_blockers`](super::Solutions::note_solve_blockers).
    pub fn note_solve_blockers(
        &mut self,
        id: MetavarId,
        blockers: Vec<(MetavarId, MetavarOrigin, Option<Extent>)>,
    ) {
        self.underway
            .attempt
            .solutions
            .note_solve_blockers(id, blockers);
    }

    /// [`Solutions::solve_blockers`](super::Solutions::solve_blockers).
    pub fn solve_blockers(&self, id: MetavarId) -> &[(MetavarId, MetavarOrigin, Option<Extent>)] {
        self.underway.attempt.solutions.solve_blockers(id)
    }

    /// [`Goals::note_extent`](super::Goals::note_extent).
    pub fn note_goal_extent(&mut self, id: MetavarId, extent: Extent) {
        self.underway.attempt.goals.note_extent(id, extent);
    }

    /// [`Goals::extent`](super::Goals::extent).
    pub fn goal_extent(&self, id: MetavarId) -> Option<&Extent> {
        self.underway.attempt.goals.extent(id)
    }

    /// [`Goals::note_obligation`](super::Goals::note_obligation).
    pub fn note_goal_obligation(&mut self, goals: BTreeSet<MetavarId>, this: Term, that: Term) {
        self.underway
            .attempt
            .goals
            .note_obligation(goals, this, that);
    }

    /// [`Goals::obligations`](super::Goals::obligations).
    pub fn goal_obligations(&self) -> &[GoalObligation] {
        self.underway.attempt.goals.obligations()
    }

    /// Whether the declaration being elaborated wrote a goal ([`Goals::any`](super::Goals::any)).
    pub fn holds_goals(&self) -> bool {
        self.underway.attempt.goals.any()
    }

    /// Materialize a metavariable's birth record over a telescope stated for it ([`Solutions::birth`](super::Solutions::birth), [`Scoped::Stated`]), under the frames that stand, stamping the write: how a fixture states its metavariables, where elaboration births one where it stands ([`Context::birth_here`]).
    #[cfg(test)]
    pub fn birth_metavar(
        &mut self,
        id: MetavarId,
        telescope: impl Into<BirthTelescope>,
        result: Term,
    ) {
        self.birth_metavar_as(
            id,
            telescope.into(),
            Scoped::Stated,
            result,
            MetaKind::Inference,
        );
    }

    /// Materialize a metavariable's birth record where the context stands — its telescope the local binders in scope, taken with the frames that stand ([`Scoped::Held`]) — and hand back its identity spine.
    pub fn birth_here(&mut self, id: MetavarId, result: Term) -> SharedSpine {
        let (telescope, spine) = self.identity_snapshot();
        self.birth_metavar_as(id, telescope, Scoped::Held, result, MetaKind::Inference);

        spine
    }

    pub(super) fn birth_metavar_as(
        &mut self,
        id: MetavarId,
        telescope: BirthTelescope,
        scoped: Scoped,
        result: Term,
        kind: MetaKind,
    ) {
        let frames = self.hold_frames();
        self.birth_under(id, telescope, scoped, frames, result, kind);
    }

    /// [`Solutions::birth`](super::Solutions::birth) under the frames `frames`, stamping the write: a metavariable born where the context stands holds the frames that stand, and one born for another context, a lambda's domain or a restriction's stand-in, the frames of that one.
    fn birth_under(
        &mut self,
        id: MetavarId,
        telescope: BirthTelescope,
        scoped: Scoped,
        frames: HeldFrames,
        result: Term,
        kind: MetaKind,
    ) {
        self.typing.caches.note_write();
        self.underway
            .attempt
            .solutions
            .birth(id, telescope, scoped, frames, result, kind);
    }

    /// Bring the telescope a metavariable was born over into scope, where a judgment of it stands on the frames it was born under ([`Context::at_birth`]): nothing where the telescope was taken with those frames and no binder was bound again in view since, the frames binding it as they did, and every binder again otherwise. Assuming it every time would cost each judgment the scope the metavariable was born in.
    pub fn assume_birth_scope(
        &mut self,
        telescope: &BirthTelescope,
        scoped: Scoped,
        born: &HeldFrames,
    ) {
        if scoped == Scoped::Held && self.typing.frames.binds_as(born) {
            return;
        }
        for (name, type_) in telescope.iter() {
            self.assume(name, type_);
        }
    }

    /// Allocate the protected placeholder for one member of a recursive group. It has the same contextual spine as an inference metavariable so parked work can carry it across a popped local frame, but only `fill_rec_slot` may solve it.
    ///
    /// `name` is the member this slot stands for, kept so the substitution walks can spell it — see [`Context::rec_slot_name`].
    pub fn fresh_rec_slot(&mut self, name: &Free, result: Term) -> (MetavarId, Term) {
        self.typing.caches.note_write();
        let id = self.underway.attempt.solutions.mint();
        let (telescope, spine) = self.identity_snapshot();
        let frames = self.hold_frames();
        self.underway
            .attempt
            .solutions
            .birth_rec_slot(id, telescope, frames, result);
        self.underway.attempt.solutions.name_rec_slot(id, *name);
        (id, Term::metavar_birthed(id, MetavarOrigin::Hole, spine))
    }

    /// [`Solutions::rec_slot_name`](super::Solutions::rec_slot_name).
    pub fn rec_slot_name(&self, id: MetavarId) -> Option<&Free> {
        self.underway.attempt.solutions.rec_slot_name(id)
    }

    pub fn is_rec_slot(&self, id: MetavarId) -> bool {
        self.underway.attempt.solutions.is_rec_slot(id)
    }

    /// [`Solutions::owner_fills`](super::Solutions::owner_fills).
    pub fn owner_fills(&self, id: MetavarId) -> bool {
        self.underway.attempt.solutions.owner_fills(id)
    }

    pub fn fill_rec_slot(&mut self, id: MetavarId, term: Term) {
        let entry = self
            .underway
            .attempt
            .solutions
            .entry(id)
            .expect("recursive slot has a birth entry");
        assert_eq!(entry.kind(), MetaKind::RecSlot, "filled a non-rec slot");
        assert!(entry.solution().is_none(), "recursive slot filled twice");
        self.solve_metavar(id, term);
    }

    /// Fill a parked problem's placeholder with the term the problem rebuilt. Only the problem does, and once: conversion waits on a placeholder ([`Solutions::owner_fills`](super::Solutions::owner_fills)), and whatever runs a parked problem holds it out of the store, so a second fill would be a second elaboration of one written term.
    pub fn fill_placeholder(&mut self, id: MetavarId, term: Term) {
        let entry = self
            .underway
            .attempt
            .solutions
            .entry(id)
            .expect("placeholder has a birth entry");
        assert_eq!(
            entry.kind(),
            MetaKind::Placeholder,
            "filled a hole that is no placeholder"
        );
        assert!(entry.solution().is_none(), "placeholder filled twice");
        self.solve_metavar(id, term);
    }

    /// [`Frames::is_top_level`](super::Frames::is_top_level): whether `name` is a declaration's, bound wherever a solution may stand. That a declaration is there at all is its signature's to say, so the question is noted as one about it: a candidate a solver admits for naming a declaration is another candidate once the declaration is gone.
    pub fn is_top_level(&self, name: &Free) -> bool {
        self.asks(name, Question::Signature);
        self.typing.frames.is_top_level(name)
    }

    pub fn identity_snapshot(&self) -> (BirthTelescope, SharedSpine) {
        self.typing.frames.identity_snapshot()
    }

    /// Mint a metavariable for an omitted implicit argument and birth it immediately — frozen local Γ, the binder's instantiated type as `result` — so the id always has a birth record. Returns its id beside the metavariable term carrying the *call site's* extent and the insertion provenance (which rides on the node; see [`Metavar::origin`]). `proposition` is whether `result` is one, decided by the caller, which has the sort in hand; it is kept on the birth record for the unsolved report, which cannot ask.
    pub fn fresh_metavar(
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
        (id, metavar)
    }

    /// Record what an unsolved bound's type reduced to when a later attempt asked, for the report the hole becomes.
    pub fn note_reduct(&mut self, id: MetavarId, reduct: Term) {
        self.underway.attempt.solutions.note_reduct(id, reduct);
    }

    /// Record why the procedure that proves a bound from the facts in scope proved nothing, for the report the hole becomes.
    pub fn note_refusal(&mut self, id: MetavarId, refusal: Refusal) {
        self.underway.attempt.solutions.note_refusal(id, refusal);
    }

    /// [`Solutions::note_dead_arm`](super::Solutions::note_dead_arm).
    pub fn note_dead_arm(&mut self, id: MetavarId, dead: DeadArm) {
        self.underway.attempt.solutions.note_dead_arm(id, dead);
    }

    /// [`Solutions::note_written`](super::Solutions::note_written).
    pub fn note_written(&mut self, id: MetavarId, extent: Extent) {
        self.underway.attempt.solutions.note_written(id, extent);
    }

    /// Mint a metavariable for an omitted `use` argument — like [`Context::fresh_metavar`] but carrying witness provenance, and returning the id so the caller can register the resolution goal.
    pub fn fresh_witness_metavar(
        &mut self,
        result: Term,
        extent: Option<Extent>,
        origin: WitnessOrigin,
    ) -> (MetavarId, Term) {
        self.fresh_metavar_with(result, extent, MetavarOrigin::Witness(origin))
    }

    /// An open witness slot as a report's term holds one: what elaboration mints for a `use` argument a program leaves out, minted for a report's printer alone. No metavariable is born for it, so nothing but a printer reads it.
    pub fn open_witness_slot(&mut self, origin: WitnessOrigin) -> Term {
        let id = self.underway.attempt.solutions.mint();

        Term::metavar_birthed(id, MetavarOrigin::Witness(origin), Vec::new())
    }

    /// Mint a silent hole — the stand-in type a written goal in synthesis position gets, so the goal survives to zonk's report instead of dying with `CannotInfer` (`elaborate_metavar`).
    pub fn fresh_hole_metavar(&mut self, result: Term, extent: Option<Extent>) -> Term {
        self.fresh_hole(result, extent).1
    }

    /// [`Context::fresh_hole_metavar`] with the hole's identity beside it, for a site that reads whether unification solved it: an operator's operand type, defaulted where no operand pins it.
    pub fn fresh_hole(&mut self, result: Term, extent: Option<Extent>) -> (MetavarId, Term) {
        self.fresh_metavar_with(result, extent, MetavarOrigin::Hole)
    }

    /// Mint a metavariable identity with no birth record, for a term built ahead of its elaboration exactly as `into_core` builds one: an omitted motive, a list literal's element type, a witness goal with a provenance of its own. Birth happens where elaboration first meets the term, in checking position, as it does for every lowered hole.
    pub fn mint_metavar(&mut self) -> MetavarId {
        self.underway.attempt.solutions.mint()
    }

    /// The frozen ambient scope a settle-synthesized lambda's domain metavariables are born under — captured before the settle walk assumes a single lambda binder, so a domain's birth context is never wider than the expectation it will inhabit, which the embedded-metavariable exemption in `solve` relies on ([`Context::metavar_context_contained`]).
    pub fn domain_scope(&mut self) -> DomainScope {
        let (telescope, spine) = self.identity_snapshot();
        let frames = self.hold_frames();
        DomainScope {
            telescope,
            spine,
            frames,
        }
    }

    /// Mint the metavariable standing for a settle-synthesized lambda's unannotated domain — like [`Context::fresh_hole_metavar`] but carrying the binder's name where it has one, so zonk's unsolved report can point at the parameter whose type was never determined instead of raising a bare "cannot infer", and born at the settle site's ambient `scope` rather than under the lambda's own binders.
    pub fn fresh_domain_metavar(
        &mut self,
        scope: &DomainScope,
        result: Term,
        extent: Option<Extent>,
        binder: Option<String>,
    ) -> Term {
        let id = self.underway.attempt.solutions.mint();
        self.birth_under(
            id,
            scope.telescope.clone(),
            Scoped::Held,
            scope.frames.clone(),
            result,
            MetaKind::Inference,
        );
        let metavar = Term::metavar_birthed(id, MetavarOrigin::Domain(binder), scope.spine.clone());
        match extent {
            Some(extent) => metavar.with_extent(extent),
            None => metavar,
        }
    }

    /// Whether `inner`'s birth context is contained in `outer`'s: every binder name of `inner`'s birth telescope is a binder name of `outer`'s. Binder names are entropy-fresh, so name identity is context identity, and the type at a shared name is the one both frames recorded. `solve`'s embedded-metavariable guard asks this to tell a candidate metavariable that could smuggle a name past `outer`'s scope from one that provably cannot.
    pub fn metavar_context_contained(&self, inner: MetavarId, outer: MetavarId) -> bool {
        let (Some(inner_entry), Some(outer_entry)) =
            (self.metavar_entry(inner), self.metavar_entry(outer))
        else {
            return false;
        };
        // Refinements as well as names: a `Bool` arm opens no binder, so a metavariable born inside `match k < n | true =>` has the telescope of one born just outside it, and a solution resting on the arm's guard would escape through a containment that read names alone.
        self.metavar_names_contained(inner, outer)
            && inner_entry
                .frames()
                .refinements_within(outer_entry.frames())
    }

    /// Whether a candidate for `outer` binds what `inner`'s birth context holds beyond `outer`'s: `inner` was born under no refinement `outer` was not, and each binder of its telescope that is none of `outer`'s stands, at every one of `occurrences`, as a variable the candidate binds itself. The occurrence then sits where `inner` was born, a binder of the candidate's own in the binder's place, and whatever `inner` comes to arrives through that spine and names nothing the candidate does not bind. It is what a settled lambda's codomain is in the function type the lambda states: born under the lambda's binders, so that the body's type may name them.
    pub fn metavar_bound_by_candidate(
        &self,
        inner: MetavarId,
        occurrences: &[MetavarSpine],
        outer: MetavarId,
    ) -> bool {
        let (Some(inner_entry), Some(outer_entry)) =
            (self.metavar_entry(inner), self.metavar_entry(outer))
        else {
            return false;
        };
        if occurrences.is_empty()
            || !inner_entry
                .frames()
                .refinements_within(outer_entry.frames())
        {
            return false;
        }
        let outer_names: BTreeSet<&Free> = outer_entry
            .telescope()
            .iter()
            .map(|(name, _)| name)
            .collect();

        occurrences.iter().all(|spine| {
            spine.len() == inner_entry.telescope().len()
                && spine
                    .iter()
                    .zip(inner_entry.telescope().iter())
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
        let outer_names: BTreeSet<&Free> = outer_entry
            .telescope()
            .iter()
            .map(|(name, _)| name)
            .collect();
        inner_entry
            .telescope()
            .iter()
            .all(|(name, _)| outer_names.contains(name))
    }

    /// How the unsolved `inner`, occurring at `occurrences` in a candidate for the occurrence `outer` and not contained in `outer`'s birth context, may be re-expressed in that context rather than hold the candidate back ([`Context::restrict_metavar`]): a stand-in born over binders of `outer`'s telescope, and what `inner` is in terms of it.
    ///
    /// A candidate for `outer` is inverted through `outer`'s spine — its variables back to their binders, its other entries abstracted — so whatever `inner` contributes must be spelled in those entries, and the stand-in takes them as its arguments. An `inner` contained by names keeps its own telescope. One whose telescope holds binders `outer`'s lacks is applied to `outer`'s entries spelled in its own birth names: an implicit born under a match arm's `v`, embedded in the match's type `?M(r, success(v))`, becomes `?E′(r, success(v))` for a stand-in over `?M`'s binders. No solution `outer` could take is lost — one of `inner` using `v` outside `success(v)` was never one the inversion admits, and one using it inside is the stand-in's — and an entry `inner` cannot spell is one it could never have contributed to. This is pruning (Abel and Pientka, *Higher-Order Dynamic Pattern Unification*, 2011), which restricts one pattern substitution by the inverse of another, over the solver's wider inverse; dropping `v` instead, as a context approximation does, would lose exactly the solutions through `success(v)`, which is how a match's inferred type depends on its scrutinee.
    ///
    /// What cannot be spelled that way waits: occurrences that disagree or are not a renaming, a binder the stand-in needs whose type depends on one it cannot take, and a type of `inner`'s own that mentions one. So does a hole something else fills: a written goal reports by its identity, a witness hole is filled by resolution, a bound by its discharge, a parked check's placeholder by that check, and a recursive group's slot by its group.
    pub fn metavar_restriction(
        &self,
        inner: MetavarId,
        origin: &MetavarOrigin,
        occurrences: &[MetavarSpine],
        outer: &Metavar,
    ) -> Option<Restriction> {
        let (inner_entry, outer_entry) =
            (self.metavar_entry(inner)?, self.metavar_entry(outer.id)?);
        if !inner_entry.unification_fills(origin) {
            return None;
        }
        // It holds the frames both births hold, outermost first for as far as they are the same frames, each as the earlier of the two held it: whatever reaches `outer` through `inner` must hold where `outer` is, so `inner`'s later solution can no longer rest on a guard `outer` would carry out of its arm.
        let frames = || match inner < outer.id {
            true => inner_entry.frames().shared_with(outer_entry.frames()),
            false => outer_entry.frames().shared_with(inner_entry.frames()),
        };

        if self.metavar_names_contained(inner, outer.id) {
            return Some(Restriction {
                arguments: inner_entry
                    .telescope()
                    .iter()
                    .map(|(name, _)| Term::free_var(name))
                    .collect(),
                telescope: inner_entry.telescope().clone(),
                result: inner_entry.result().clone(),
                frames: frames(),
            });
        }

        // `outer`'s entries are spelled in `inner`'s birth names through its occurrence, which is one renaming wherever it occurs.
        let (spine, others) = occurrences.split_first()?;
        if others.iter().any(|other| other != spine)
            || spine.len() != inner_entry.telescope().len()
            || outer.spine.len() != outer_entry.telescope().len()
        {
            return None;
        }
        let mut births = BTreeMap::<&Free, &Free>::new();
        for (argument, (birth, _)) in spine.iter().zip(inner_entry.telescope().iter()) {
            let Subterm::Var(var) = &**argument else {
                return None;
            };
            let name = var.as_free()?;
            if self.is_top_level(name) || births.insert(name, birth).is_some() {
                return None;
            }
        }
        let spelled = |term: &Term| {
            term.free_vars()
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
            .telescope()
            .iter()
            .zip(&keep)
            .filter(|(_, kept)| !**kept)
            .map(|((name, _), _)| name)
            .collect::<BTreeSet<_>>();
        let telescope = outer_entry
            .telescope()
            .iter()
            .zip(&keep)
            .filter(|(_, kept)| **kept)
            .map(|(binder, _)| binder.clone())
            .collect::<Vec<_>>();
        if telescope
            .iter()
            .any(|(_, type_)| type_.free_vars().iter().any(|name| dropped.contains(name)))
        {
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
        let named = inner_entry.result().free_vars();
        for name in &named {
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
        let result = inner_entry
            .result()
            .capture(&mentioned)
            .release(&binder_refs);

        Some(Restriction {
            telescope: telescope.into(),
            arguments,
            result,
            frames: frames(),
        })
    }

    /// How the unsolved `inner`, occurring at `occurrences` in the type a block answers where it ends, may be re-expressed without the block's labels rather than leave the block naming them ([`Context::restrict_metavar`]): a stand-in born where the block began, over the binders of `inner`'s telescope before the block's own, at `inner`'s type with each label read as the value `values` pairs it with, in binding order.
    ///
    /// A block's term is closed over its labels where the block ends, and its type is not: a label in the spine of a metavariable the type holds is bound by nothing there, and a solution that names its binder, which a problem parked in the block may still give, reads at that occurrence as a name the kernel refuses. A label is a definition, so no solution depends on it that does not depend on its value as well, and none is lost with its binder: the stand-in takes a candidate that names the label by its value (`Convert::solve`). It is the restriction a candidate's embedded metavariable takes ([`Context::metavar_restriction`]), at the other place a metavariable leaves the context it was born in, and what Lean makes of a metavariable where a `let` is closed (`elimMVarDeps`). The spine stays the renaming a solution is inverted through, where the label's value written into it would be an entry the inversion cannot read, and a second name for whatever variable the value is.
    ///
    /// What cannot be spelled that way is read by value where it stands, its spine with it: an occurrence that is not the metavariable read where it was born, one born under a binder inside the block's tail, whose refinements and binders the block's own may type, one whose type still holds an unsolved metavariable naming a label, and a hole something else fills.
    pub fn block_restriction(
        &self,
        inner: MetavarId,
        origin: &MetavarOrigin,
        occurrences: &[MetavarSpine],
        values: &[(Free, Term)],
    ) -> Option<Restriction> {
        let inner_entry = self.metavar_entry(inner)?;
        if !inner_entry.unification_fills(origin) {
            return None;
        }

        // Read where it was born: every binder as itself, so a value is spelled in the birth's names as it stands. A spine known for the identity is one, unread.
        let telescope = &inner_entry.telescope();
        let born = |spine: &MetavarSpine| {
            spine.len() == telescope.len()
                && (spine.is_identity()
                    || spine.iter().zip(telescope.iter()).all(|(entry, (binder, _))| {
                        matches!(&**entry, Subterm::Var(var) if var.as_free() == Some(binder))
                    }))
        };
        if occurrences.is_empty() || !occurrences.iter().all(born) {
            return None;
        }

        // Born in the block's own frame: its telescope ends with the block's labels, and what stands before them was bound before the block and names none.
        let labelled = |binder: &Free| values.iter().any(|(label, _)| label == binder);
        let kept = telescope.iter().position(|(binder, _)| labelled(binder))?;
        if !telescope
            .iter()
            .skip(kept)
            .all(|(binder, _)| labelled(binder))
        {
            return None;
        }

        let result = zonk_solved_term_metas(self, inner_entry.result());
        if self.names_in_unsolved_spine(&result, values) {
            return None;
        }

        Some(Restriction {
            telescope: telescope.prefix(kept),
            arguments: telescope
                .iter()
                .take(kept)
                .map(|(name, _)| Term::free_var(name))
                .collect(),
            result: read_by_value(result, values),
            frames: inner_entry
                .frames()
                .beneath(self.typing.frames.innermost_frame()),
        })
    }

    /// Whether `term` holds an unsolved metavariable whose spine names one of the labels `values` pairs with a value: what a block's end restricts before it reads the rest by value ([`Context::block_restriction`]).
    pub fn names_in_unsolved_spine(&self, term: &Term, values: &[(Free, Term)]) -> bool {
        let labels = Names::new(values.iter().map(|(label, _)| *label));
        term.metavars().iter().any(|id| {
            self.metavar_solution(*id).is_none()
                && metavar_spines(term, *id)
                    .iter()
                    .any(|spine| spine.mentions_any(&labels))
        })
    }

    /// Solve the unsolved `inner` to a fresh stand-in as `restriction` plans it ([`Context::metavar_restriction`], [`Context::block_restriction`]), holding the frames the plan states. The stand-in keeps `inner`'s origin and `extent`, so an unsolved one reports as `inner` would have. Birth records stay frozen; the restriction is a solution, rolled back and reported like any other.
    pub fn restrict_metavar(
        &mut self,
        inner: MetavarId,
        (origin, extent): Provenance,
        restriction: Restriction,
    ) {
        if self.metavar_entry(inner).is_none() {
            return;
        }
        let Restriction {
            telescope,
            arguments,
            result,
            frames,
        } = restriction;

        let narrowed = self.underway.attempt.solutions.mint();
        self.birth_under(
            narrowed,
            telescope,
            Scoped::Stated,
            frames,
            result,
            MetaKind::Inference,
        );
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
        let spine = self.birth_here(id, result);
        let metavar = Term::metavar_birthed(id, origin, spine);

        let metavar = match extent {
            Some(extent) => {
                self.note_written(id, extent);
                metavar.with_extent(extent)
            }
            None => metavar,
        };

        (id, metavar)
    }

    pub fn metavar_entry(&self, id: MetavarId) -> Option<&MetaEntry> {
        self.underway.attempt.solutions.entry(id)
    }

    pub fn witness_hole(&self, term: &Term) -> Option<(WitnessOrigin, Term)> {
        self.underway.attempt.solutions.witness_hole(term)
    }

    pub fn metavar_solution(&self, id: MetavarId) -> Option<&Term> {
        self.underway.attempt.solutions.solution(id)
    }

    pub fn resolve_metavar(&self, metavar: &Metavar) -> Option<Term> {
        self.underway.attempt.solutions.resolve(metavar)
    }

    /// The type of `metavar` at its occurrence ([`Solutions::type_at`](super::Solutions::type_at)).
    pub fn metavar_type_at(&self, metavar: &Metavar) -> Option<Term> {
        self.underway.attempt.solutions.type_at(metavar)
    }

    /// Commit a metavariable's solution ([`Solutions::solve`](super::Solutions::solve)), stamping the write. Needs no reduction-cache clear: a WHNF that still named an unsolved metavariable was never memoized (see `Context::reduce`), and a solve is monotonic, so every surviving entry stays valid. (Re-validation's [`Context::rollback_solutions`], which *un*-solves, does clear.)
    pub fn solve_metavar(&mut self, id: MetavarId, term: Term) {
        self.typing.caches.note_write();
        self.underway.attempt.solutions.solve(id, term);
    }

    /// Close a [`Context::solution_mark`] transaction, keeping whatever it left in place. Undoing is [`Context::rollback_solutions`]; a path that undoes still ends here.
    pub fn end_solutions(&mut self, mark: SolutionMark) {
        self.underway.attempt.universe_solver.release(mark.universe);
    }

    /// Watermark for [`Context::rollback_solutions`]: how many solutions have been committed so far. Spans both unification stores, which is why it lives here rather than on either.
    ///
    /// Opens a speculative scope on the universe solver, which [`Context::end_solutions`] closes. The two are paired by hand at every site: a closure bracket in the manner of [`Context::with_frame`] would be safer, but these sit on elaboration's recursions, where its body is a stack frame per level.
    pub fn solution_mark(&mut self) -> SolutionMark {
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
    pub fn solutions_committed(&self) -> usize {
        self.underway.attempt.solutions.solved_len()
    }

    /// The solutions committed since `watermark` that still stand, each with its id.
    pub fn solutions_since(&self, watermark: usize) -> Vec<(MetavarId, Term)> {
        self.underway.attempt.solutions.solved_since(watermark)
    }

    /// Rewrite standing solutions in place to terms they already denote — what a recursive group does to its own members once its generalization has minted their instance — clearing every cache as a rollback does, since a cached reduct may have gone through a spelling replaced here.
    pub fn restamp_solutions(&mut self, solutions: Vec<(MetavarId, Term)>) {
        if solutions.is_empty() {
            return;
        }
        self.typing.caches.note_write();
        for (id, term) in solutions {
            self.underway.attempt.solutions.restamp(id, term);
        }
        self.typing.caches.invalidate_for_rollback();
    }

    /// Unwind every solution committed since `mark` — the transactional bracket around re-validation. Validating a candidate runs full elaboration, which can solve *other* metavariables along the way; if the candidate is ultimately rejected, those nested solutions were derived from an equation that never held and must not survive the verdict. Removes the unwound ids from the wake signals.
    ///
    /// **It invalidates what can rest on what it undid, and nothing more.** An unwound term solution can sit inside any reduct or elaboration cached since, so every cache clears. A universe solver that moved without one — a constraint or a level solution withdrawn — is read by no reduct, reduction being parametric in levels, but an elaboration may have certified its purity against what was withdrawn, so the elaborations clear, as they do where a universe transaction closes. A rollback that undid nothing clears nothing: a witness probe rolls back after every trial — hundreds of times re-validating one `Str/split_once` stated in a type, few of which unwind a solution — and a clear at each would throw away the reducts and elaborations the next node needs.
    pub fn rollback_solutions(&mut self, mark: SolutionMark) {
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

        let universes_moved =
            self.underway.attempt.universe_solver.state_token() != universes_before;
        if unwinds_terms {
            self.typing.caches.invalidate_for_rollback();
        } else if universes_moved {
            self.typing.caches.note_universe_write();
            self.typing.caches.invalidate_for_universe_transaction();
        }
        // A question asked inside the scope was answered on the level constraints it held, and a reduct or a settled spelling may rest on the answer: those go with the constraints, whether or not a solution went with them. A scope that asked none leaves the reducts alone, which no rule of reduction reads a level for.
        if universes_moved && self.typing.asking.questions() != mark.questions {
            self.typing.caches.invalidate_for_withdrawn_answers();
        }
    }
}
