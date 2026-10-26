//! The context's doors to the universes: the solver's levels and qualities, how a declaration's close into its scheme, and declarations and assumptions read at a universe instance.

use {
    crate::{
        Context, Error, Occurrence, UniverseMutation, UniverseSolver, UniverseStateToken,
        stamp_if_moved, zonk_universe_levels_scoped,
    },
    curios_core::{
        Bound, Extent, Free, Global, InductDecl, InductInstance, Level, Quality, RecGroup,
        ReduceError, StructDecl, StructInstance, Term, UniverseContext, UniverseError,
        UniverseMetaId, Universes, instantiate_universes_scoped,
    },
    std::collections::BTreeSet,
};

impl<'s> Context<'s> {
    /// `group` at `universes`, remembered as that instance of it for the declaration in progress and read back by [`Context::instance_of`]. An instance holds its levels substituted through every member, so a copy does not say which instance of which group it is, and a term that names a group's members again ([`named_members`](crate::named_members)) knows one by this record alone. So this is the one way an instance is opened here: by the reducer where it unfolds a name, by the closed machine through its host, by the neutral typer where it reads a member's type, and by the reduction table where it serves a reduct at another spelling's levels ([`Caches::reduction_get`](super::Caches::reduction_get)).
    pub(crate) fn opened_instance(
        &mut self,
        group: &RecGroup,
        universes: &Universes,
    ) -> Result<RecGroup, ReduceError> {
        self.typing
            .caches
            .opened_instance(group, universes)
            .map_err(ReduceError::Universe)
    }

    /// The group `instance` was opened from and the instance it was opened at, where it was opened in the declaration in progress.
    pub fn instance_of(&self, instance: &RecGroup) -> Option<&(RecGroup, Universes)> {
        self.typing.caches.instance_get(instance)
    }

    /// Collect universe metas reachable through a term and through any solved term metavariables it names. Declaration finalization runs before the final term-zonk pass, so a level occurring only in a solved hole must still join the declaration's universe closure. Recursive slots may point back to themselves; `seen` keeps this analysis finite.
    pub fn universe_metas_in(&self, term: &Term) -> BTreeSet<UniverseMetaId> {
        let mut universes = BTreeSet::new();
        let mut seen_metas = BTreeSet::new();
        let mut pending = vec![term.clone()];
        while let Some(term) = pending.pop() {
            let mut term_metas = BTreeSet::new();
            term.collect_universe_dependencies(&mut universes, &mut term_metas);
            for meta in term_metas {
                if !seen_metas.insert(meta) {
                    continue;
                }
                if let Some(entry) = self.metavar_entry(meta) {
                    match entry.solution() {
                        Some(solution) => pending.push(solution.clone()),
                        None => {
                            // An unsolved meta may survive into parked work, so keep every universe dependency needed to solve it later. A solved meta materializes only its solution through the occurrence spine; its birth result/telescope do not survive zonking.
                            pending.push(entry.result().clone());
                            pending
                                .extend(entry.telescope().iter().map(|(_, type_)| type_.clone()));
                        }
                    }
                }
            }
        }
        universes
    }

    /// The type `name` is assumed at, at a fresh instance of the scheme it was generalized over: the instance of the occurrence written at `extent`, where a term wrote one, which the scheme's constraints are restated under ([`Occurrence`]).
    pub(crate) fn instantiate_assumption_universes(
        &mut self,
        name: &Free,
        extent: Option<Extent>,
    ) -> Result<Option<(Term, Universes)>, UniverseError> {
        let Some(type_) = self.assumption(name).cloned() else {
            return Ok(None);
        };
        let universe_context = self
            .typing
            .frames
            .assumption_universe_context(name)
            .unwrap_or_default();
        if universe_context.width() == 0 {
            return Ok(Some((type_, Universes::default())));
        }
        let universes = self
            .universes_mut()
            .instantiate(&universe_context, Occurrence::naming(name, extent))?;
        let type_ = instantiate_universes_scoped(&type_, &universes)?;
        Ok(Some((type_, universes)))
    }

    pub fn instantiate_assumption(
        &mut self,
        name: &Free,
        extent: Option<Extent>,
    ) -> Result<Option<(Term, Universes)>, Error> {
        self.instantiate_assumption_universes(name, extent)
            .map_err(Error::from)
    }

    pub fn instantiate_assumption_at(
        &mut self,
        name: &Free,
        universes: &Universes,
        extent: Option<Extent>,
    ) -> Result<Option<Term>, Error> {
        let Some(type_) = self.assumption(name).cloned() else {
            return Ok(None);
        };
        let found = self.typing.frames.assumption_universe_context(name);
        #[cfg(feature = "profile")]
        if found
            .as_ref()
            .is_none_or(|context| context.parameter_count != universes.levels.len())
        {
            let (frames, holders) = self.typing.frames.assumption_universe_holders(name);
            curios_profile::note!(
                target: "curios_elab::universe",
                name = %name.faithful(),
                registered = found.is_some(),
                expected = found.as_ref().map_or(0, |context| context.parameter_count),
                got = universes.levels.len(),
                frames,
                ?holders,
                "assumption instance arity mismatch",
            );
        }
        let universe_context = found.unwrap_or_default();
        self.universes_mut()
            .instantiate_at(
                &universe_context,
                universes,
                Occurrence::naming(name, extent),
            )
            .map_err(Error::from)?;
        let type_ = instantiate_universes_scoped(&type_, universes).map_err(Error::from)?;
        Ok(Some(type_))
    }

    pub fn instantiate_universe_bound<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
        occurrence: Option<Occurrence>,
    ) -> Result<(B, Universes), Error> {
        if universe_context.width() == 0 {
            return Ok((value.clone(), Universes::default()));
        }
        let universes = self
            .universes_mut()
            .instantiate(universe_context, occurrence)
            .map_err(Error::from)?;
        let value = instantiate_universes_scoped(value, &universes).map_err(Error::from)?;
        Ok((value, universes))
    }

    pub fn instantiate_universe_bound_at<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
        universes: &Universes,
    ) -> Result<B, UniverseError> {
        #[cfg(feature = "profile")]
        if universes.levels.len() != universe_context.parameter_count {
            curios_profile::note!(
                target: "curios_elab::universe",
                expected = universe_context.parameter_count,
                got = universes.levels.len(),
                "bound instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(universe_context, universes, None)?;
        instantiate_universes_scoped(value, universes)
    }

    pub fn instantiate_induct_decl_at(
        &mut self,
        induct_decl: &InductDecl,
        universes: &Universes,
    ) -> Result<InductDecl, UniverseError> {
        #[cfg(feature = "profile")]
        if universes.levels.len() != induct_decl.universe_context.parameter_count {
            curios_profile::note!(
                target: "curios_elab::universe",
                module = ?induct_decl.module,
                expected = induct_decl.universe_context.parameter_count,
                got = universes.levels.len(),
                "induct instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(&induct_decl.universe_context, universes, None)?;
        induct_decl.at(universes).whole()
    }

    /// What `read` takes of the inductive declaration `name` at the occurrence `universes`, the declaration's constraints instantiated where the occurrence stands, or `None` where `name` declares no inductive: a read instantiates the facet it takes — the sort, the arity, one constructor — and pays for no other, where [`Context::instantiate_induct_decl_at`] instantiates every constructor.
    pub fn induct_facet<T>(
        &mut self,
        name: &Global,
        universes: &Universes,
        read: impl FnOnce(InductInstance<'_>) -> Result<T, UniverseError>,
    ) -> Option<Result<T, UniverseError>> {
        let (universe_context, read) = {
            let induct_decl = self.induct_decl(name)?;
            (
                induct_decl.universe_context.clone(),
                read(induct_decl.at(universes)),
            )
        };

        Some(
            self.universes_mut()
                .instantiate_at(&universe_context, universes, None)
                .and(read),
        )
    }

    /// [`Context::induct_facet`] for the structure declaration `name`: a read of the sort, the parameters or one field pays for that facet, where [`Context::instantiate_struct_decl_at`] instantiates every field.
    pub fn struct_facet<T>(
        &mut self,
        name: &Global,
        universes: &Universes,
        read: impl FnOnce(StructInstance<'_>) -> Result<T, UniverseError>,
    ) -> Option<Result<T, UniverseError>> {
        let (universe_context, read) = {
            let struct_decl = self.struct_decl(name)?;
            (
                struct_decl.universe_context.clone(),
                read(struct_decl.at(universes)),
            )
        };

        Some(
            self.universes_mut()
                .instantiate_at(&universe_context, universes, None)
                .and(read),
        )
    }

    pub fn instantiate_struct_decl_at(
        &mut self,
        struct_decl: &StructDecl,
        universes: &Universes,
    ) -> Result<StructDecl, UniverseError> {
        #[cfg(feature = "profile")]
        if universes.levels.len() != struct_decl.universe_context.parameter_count {
            curios_profile::note!(
                target: "curios_elab::universe",
                module = ?struct_decl.module,
                expected = struct_decl.universe_context.parameter_count,
                got = universes.levels.len(),
                "struct instance arity mismatch",
            );
        }
        self.universes_mut()
            .instantiate_at(&struct_decl.universe_context, universes, None)?;
        let mut instantiated = struct_decl.clone();
        instantiated.arity = instantiate_universes_scoped(&instantiated.arity, universes)?;
        instantiated.result_sort =
            instantiate_universes_scoped(&instantiated.result_sort, universes)?;
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }

    /// [`Frames::set_assumption_universe_context`](super::Frames::set_assumption_universe_context), with the redefinition cache protocol — a scheme rewritten in place makes cached entries through the old scheme unsound.
    ///
    /// A write of the scheme already registered rewrites nothing and clears nothing. Replaying an established scope writes every declaration's scheme once more, and a clear on each — a wholesale one, thousands of times per compilation — would keep the reduction cache from ever holding a reduct across the mentions of one literal.
    pub(crate) fn set_assumption_universe_context(
        &mut self,
        name: &Free,
        universe_context: UniverseContext,
    ) {
        if self
            .typing
            .frames
            .assumption_universe_context(name)
            .as_ref()
            == Some(&universe_context)
        {
            return;
        }
        self.typing
            .frames
            .set_assumption_universe_context(name, universe_context);
        self.typing.caches.invalidate_for_redefinition();
    }

    /// The universe solver's state now, which a settlement made across a declined question is filed beside.
    pub fn universe_state(&self) -> UniverseStateToken {
        self.underway.attempt.universe_solver.state_token()
    }

    pub fn universes(&self) -> &UniverseSolver {
        &self.underway.attempt.universe_solver
    }

    /// The universe solver for a judgment's writes ([`UniverseMutation`]), advancing the elaboration cache's universe stamp where they move it. Rollback performs the conservative cache clear separately.
    pub(crate) fn universes_mut(&mut self) -> UniverseMutation<'_> {
        UniverseMutation::new(
            &mut self.underway.attempt.universe_solver,
            self.typing.caches.universe_stamp(),
        )
    }

    /// The universe solver for closing a declaration's levels, which only the context holds: what closing moves advances the universe stamp as a write does ([`stamp_if_moved`]), and each door that closes clears the caches the closing rewrites.
    fn closing_universes<T>(&mut self, close: impl FnOnce(&mut UniverseSolver) -> T) -> T {
        let solver = &mut self.underway.attempt.universe_solver;
        let before = solver.state_token();
        let closed = close(solver);
        stamp_if_moved(solver, before, self.typing.caches.universe_stamp());
        closed
    }

    pub fn finish_universe_transaction(&mut self) {
        let before = self.underway.attempt.universe_solver.state_token();
        self.closing_universes(UniverseSolver::clear_constraints);
        if self.underway.attempt.universe_solver.state_token() != before {
            self.typing.caches.invalidate_for_universe_transaction();
        }
    }

    /// Close every speculative universe scope a refusal left open — see [`UniverseSolver::abandon_speculation`] for where that is sound.
    pub(crate) fn abandon_universe_speculation(&mut self) {
        self.underway.attempt.universe_solver.abandon_speculation();
    }

    /// Close the qualities nothing decided, each as a type's ([`UniverseSolver::default_qualities`]). An item closes them once it has drained what it parked: a problem that waited on a metavariable may be the one that puts a proposition under a sort left to inference, and it is retried after the item's levels have closed.
    pub(crate) fn close_qualities(&mut self) {
        self.closing_universes(UniverseSolver::default_qualities);
    }

    /// Hold two sorts' qualities one, as conversion compares them: an unsolved one solved to the other, and two that each state a sort one where they state the same. Where conversion answers a question asked with nothing committed it solves none, as it joins no two levels there ([`Context::withholds`]), and an equation it would have made is noted on the question.
    pub fn same_quality(&mut self, this: Quality, that: Quality) -> bool {
        self.same_quality_at(this, that, None)
    }

    /// [`Context::same_quality`] asked on behalf of the term at `site`: what a `Sort` this decides is reported with.
    pub fn same_quality_at(&mut self, this: Quality, that: Quality, site: Option<Extent>) -> bool {
        let this = self.universes().quality(this);
        let that = self.universes().quality(that);
        if this == that {
            return true;
        }
        if self.typing.asking.withholding() {
            if matches!(this, Quality::Meta(_)) || matches!(that, Quality::Meta(_)) {
                self.typing.asking.note_withheld();
            }
            return false;
        }

        self.universes_mut().unify_qualities(this, that, site)
    }

    pub fn default_universes(&mut self, terms: &[&Term]) -> Result<Vec<Term>, Error> {
        let metas = terms
            .iter()
            .flat_map(|term| self.universe_metas_in(term))
            .collect::<BTreeSet<_>>();
        self.closing_universes(|solver| {
            solver.default(metas)?;
            solver.default_qualities();
            Ok::<_, UniverseError>(())
        })
        .map_err(Error::from)?;
        self.underway.attempt.universe_solver.close_scheme();
        let solver = &self.underway.attempt.universe_solver;
        let terms = terms
            .iter()
            .map(|term| zonk_universe_levels_scoped(*term, solver).map_err(Error::from))
            .collect::<Result<Vec<_>, _>>()?;
        self.typing.caches.invalidate_for_universe_rewrite();
        Ok(terms)
    }

    /// Finalize a declaration's levels — see `UniverseSolver::finalize`.
    pub fn finalize_universe_metas(
        &mut self,
        interface: BTreeSet<UniverseMetaId>,
        internal: BTreeSet<UniverseMetaId>,
        compared: BTreeSet<UniverseMetaId>,
    ) -> Result<UniverseContext, Error> {
        curios_profile::profile!("ctx::finalize_universe_metas");
        let mut universe_context = self
            .closing_universes(|solver| solver.finalize(interface, internal, compared))
            .map_err(Error::from)?;
        universe_context.quality_count = self.generalize_written_sorts()?;
        self.underway.attempt.universe_solver.close_scheme();
        self.typing.caches.invalidate_for_universe_rewrite();
        Ok(universe_context)
    }

    /// Close the `Sort`s the declaration wrote as its parameters ([`UniverseSolver::generalize_qualities`]), answering how many it leaves to its caller. Refused where the declaration decided one, at the term that decided it where one was recorded and at the written `Sort` otherwise; where it leaves more than a declaration may; and where it formed a record type over two of them.
    fn generalize_written_sorts(&mut self) -> Result<usize, Error> {
        let count = self
            .closing_universes(UniverseSolver::generalize_qualities)
            .map_err(|decided| {
                let prop = decided.sort.is_prop();
                match (decided.decided, decided.written) {
                    (Some(site), _) => Error::sort_decided(prop, true).at(site),
                    (None, written) => Error::sort_decided(prop, false).at_opt(written),
                }
            })?;
        if count > UniverseContext::QUALITY_LIMIT {
            return Err(Error::too_many_sorts(count));
        }
        if let Some(site) = self.universes().open_quality_join() {
            return Err(Error::record_over_two_sorts().at_opt(site));
        }

        Ok(count)
    }

    pub fn finalize_universe_metas_at_instance(
        &mut self,
        metas: BTreeSet<UniverseMetaId>,
        instance: &Universes,
        parameter_count: usize,
    ) -> Result<(), Error> {
        self.closing_universes(|solver| {
            solver.finalize_at_instance(metas, instance, parameter_count)
        })
        .map_err(Error::from)?;
        self.underway.attempt.universe_solver.close_scheme();
        self.typing.caches.invalidate_for_universe_rewrite();
        Ok(())
    }

    /// Settle the levels a witness's scheme minted at one use: pinned to what its goal fixes, and closed there only once the declaration that raised the goal has finalized.
    pub fn close_universe_instance(
        &mut self,
        minted: &[Level],
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), Error> {
        self.closing_universes(|solver| solver.settle_instance(minted, instance, determined))
            .map_err(Error::from)?;
        self.typing.caches.invalidate_for_universe_rewrite();
        Ok(())
    }

    pub fn zonk_universe_levels<B: Bound>(&self, value: &B) -> Result<B, Error> {
        zonk_universe_levels_scoped(value, &self.underway.attempt.universe_solver)
            .map_err(Error::from)
    }
}
