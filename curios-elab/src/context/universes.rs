//! The context's doors to the universes: the solver's levels and qualities, how a declaration's close into its scheme, and declarations and assumptions read at a universe instance.

use {
    crate::{
        Context, Error, Occurrence, UniverseMutation, UniverseSolver, UniverseStateToken,
        stamp_if_moved, zonk_universe_levels,
    },
    curios_core::{
        Bound, CalleeId, Extent, Global, InductInstance, Level, Mention, Quality, StructInstance,
        Term, UniverseContext, UniverseError, UniverseMetaId, Universes, held_at,
        instantiate_universes,
    },
    std::collections::BTreeSet,
};

impl<'s> Context<'s> {
    /// Collect universe metas reachable through a term and through any solved term metavariables it names. Declaration finalization runs before the final term-zonk pass, so a level occurring only in a solved hole must still join the declaration's universe closure. Recursive slots may point back to themselves; `seen` keeps this analysis finite.
    pub(crate) fn universe_metas_in(&self, term: &Term) -> BTreeSet<UniverseMetaId> {
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

    /// The type the declaration `name` is read at, at a fresh instance of the scheme it was generalized over: the instance of the occurrence written at `extent`, where a term wrote one, which the scheme's constraints are restated under ([`Occurrence`]).
    pub(crate) fn instantiate_declaration_universes(
        &mut self,
        name: &Global,
        extent: Option<Extent>,
    ) -> Result<Option<(Term, Universes)>, UniverseError> {
        let Some(type_) = self.declared_type(name).cloned() else {
            return Ok(None);
        };
        let universe_context = self
            .typing
            .frames
            .universe_context(name)
            .unwrap_or_default();
        if universe_context.width() == 0 {
            return Ok(Some((type_, Universes::default())));
        }
        let occurrence = Occurrence {
            instantiated: *name,
            extent,
        };
        let universes = self
            .universes_mut()
            .instantiate(&universe_context, occurrence)?;
        let type_ = instantiate_universes(&type_, &universes)?;
        Ok(Some((type_, universes)))
    }

    /// [`Context::instantiate_declaration_universes`], refused as the elaborator refuses.
    pub(crate) fn instantiate_declaration(
        &mut self,
        name: &Global,
        extent: Option<Extent>,
    ) -> Result<Option<(Term, Universes)>, Error> {
        self.instantiate_declaration_universes(name, extent)
            .map_err(Error::from)
    }

    /// The type the declaration `name` is read at, at the instance `universes` a reference states, under which the scheme's constraints are restated.
    pub(crate) fn instantiate_declaration_at(
        &mut self,
        name: &Global,
        universes: &Universes,
        extent: Option<Extent>,
    ) -> Result<Option<Term>, Error> {
        let Some(type_) = self.declared_type(name).cloned() else {
            return Ok(None);
        };
        let found = self.typing.frames.universe_context(name);
        if found
            .as_ref()
            .is_none_or(|context| context.parameter_count != universes.levels.len())
        {
            curios_common::note!(
                target: "curios_elab::universe",
                name = %name.faithful(),
                registered = found.is_some(),
                expected = found.as_ref().map_or(0, |context| context.parameter_count),
                got = universes.levels.len(),
                "declaration instance arity mismatch",
            );
        }
        let universe_context = found.unwrap_or_default();
        self.universes_mut()
            .instantiate_at(
                &universe_context,
                universes,
                Occurrence {
                    instantiated: *name,
                    extent,
                },
            )
            .map_err(Error::from)?;
        let type_ = instantiate_universes(&type_, universes).map_err(Error::from)?;
        Ok(Some(type_))
    }

    /// `value`, stated under `universe_context`, at a fresh instance of it the occurrence `occurrence` mints, whose bounds are restated there.
    pub(crate) fn instantiate_universe_bound<B: Bound>(
        &mut self,
        universe_context: &UniverseContext,
        value: &B,
        occurrence: Occurrence,
    ) -> Result<(B, Universes), Error> {
        if universe_context.width() == 0 {
            return Ok((value.clone(), Universes::default()));
        }
        let universes = self
            .universes_mut()
            .instantiate(universe_context, occurrence)
            .map_err(Error::from)?;
        let value = instantiate_universes(value, &universes).map_err(Error::from)?;
        Ok((value, universes))
    }

    /// What `read` takes of the inductive declaration `name` at `universes`, an instance a term already holds, raising no bound, or `None` where `name` declares no inductive: a read instantiates the facet it takes — the sort, the arity, one constructor — and pays for no other, where a derivation reads every constructor.
    pub(crate) fn induct_facet<T>(
        &self,
        name: &Global,
        universes: &Universes,
        read: impl FnOnce(InductInstance<'_>) -> Result<T, UniverseError>,
    ) -> Option<Result<T, UniverseError>> {
        let induct_decl = self.induct_decl(name)?;

        Some(
            induct_decl
                .universe_context()
                .check_instance(universes)
                .and_then(|()| read(induct_decl.at(universes))),
        )
    }

    /// [`Context::induct_facet`] for the structure declaration `name`: a read of the sort, the parameters or one field pays for that facet, where a derivation reads every field.
    pub(crate) fn struct_facet<T>(
        &self,
        name: &Global,
        universes: &Universes,
        read: impl FnOnce(StructInstance<'_>) -> Result<T, UniverseError>,
    ) -> Option<Result<T, UniverseError>> {
        let struct_decl = self.struct_decl(name)?;

        Some(
            struct_decl
                .universe_context
                .check_instance(universes)
                .and_then(|()| read(struct_decl.at(universes))),
        )
    }

    /// [`Frames::set_universe_context`](super::Frames::set_universe_context), with the redefinition cache protocol — a scheme rewritten in place makes cached entries through the old scheme unsound.
    ///
    /// A write of the scheme already registered rewrites nothing and clears nothing. Replaying an established scope writes every declaration's scheme once more, and a clear on each — a wholesale one, thousands of times per compilation — would keep the reduction cache from ever holding a reduct across the mentions of one literal.
    pub(crate) fn set_universe_context(
        &mut self,
        name: &Global,
        universe_context: UniverseContext,
    ) {
        if self.typing.frames.universe_context(name).as_ref() == Some(&universe_context) {
            return;
        }
        self.typing
            .frames
            .set_universe_context(name, universe_context);
        self.typing.caches.invalidate_for_redefinition();
    }

    /// The universe solver's state now, which a settlement made across a declined question is filed beside.
    pub(crate) fn universe_state(&self) -> UniverseStateToken {
        self.underway.attempt.universe_solver.state_token()
    }

    pub(crate) fn universes(&self) -> &UniverseSolver {
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

    pub(crate) fn finish_universe_transaction(&mut self) {
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
    pub(crate) fn same_quality(&mut self, this: Quality, that: Quality) -> bool {
        self.same_quality_at(this, that, None)
    }

    /// [`Context::same_quality`] asked on behalf of the term at `site`: what a `Sort` this decides is reported with.
    pub(crate) fn same_quality_at(
        &mut self,
        this: Quality,
        that: Quality,
        site: Option<Extent>,
    ) -> bool {
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

    pub(crate) fn default_universes(&mut self, terms: &[&Term]) -> Result<Vec<Term>, Error> {
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
            .map(|term| zonk_universe_levels(*term, solver).map_err(Error::from))
            .collect::<Result<Vec<_>, _>>()?;
        self.typing.caches.invalidate_for_universe_rewrite();
        Ok(terms)
    }

    /// Finalize the levels of the declarations `owned` — see `UniverseSolver::finalize`.
    pub(crate) fn finalize_universe_metas(
        &mut self,
        owned: &BTreeSet<Global>,
        interface: BTreeSet<UniverseMetaId>,
        internal: BTreeSet<UniverseMetaId>,
        compared: BTreeSet<UniverseMetaId>,
    ) -> Result<UniverseContext, Error> {
        curios_common::profile!("ctx::finalize_universe_metas");
        let mut universe_context = self
            .closing_universes(|solver| solver.finalize(interface, internal, compared))
            .map_err(|error| universe_conflict(error, owned))?;
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

    /// Settle the levels a witness's scheme minted at one use: pinned to what its goal fixes, and closed there only once the declaration that raised the goal has finalized.
    pub(crate) fn close_universe_instance(
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

    pub(crate) fn zonk_universe_levels<B: Bound>(&self, value: &B) -> Result<B, Error> {
        zonk_universe_levels(value, &self.underway.attempt.universe_solver).map_err(Error::from)
    }
}

/// `error`, refused where the levels of the declarations `owned` settle, as its reader is told it: a conflict whose cycle holds a slot calling one of `owned` is one that declaration's own calls make, a group being monomorphic in its own levels.
fn universe_conflict(error: UniverseError, owned: &BTreeSet<Global>) -> Error {
    match error {
        UniverseError::Inconsistency { lower, upper, path } => {
            let recursive = held_at(&path).iter().any(|site| {
                matches!(
                    site.callee,
                    CalleeId::Function(Mention::Declaration(name)) if owned.contains(&name)
                )
            });
            Error::UniverseInconsistency {
                lower,
                upper,
                path,
                recursive,
            }
        }
        error => Error::from(error),
    }
}
