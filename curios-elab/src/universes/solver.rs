//! The transactional universe constraint solver.
//!
//! Elaboration machinery, not representation: it owns the live inequality store, which the shared least model decides (`curios_analysis::satisfiable`), and the marks that roll a speculative branch back. The levels, contexts, and schemes it solves over live in `curios-core`'s `universe`, which knows nothing about any of this.
//!
//! **A constraint's origin is live, and a scheme's constraint has none.** Where each constraint was raised — the written sort a level was minted for, the check that put one level below another, the use that instantiated a scheme — is held beside it in the store and read by the report of a conflict; what generalization hands out is the inequalities alone (`UniverseConstraint`), so a scheme says what it means wherever it is held, and an instance raises its scheme's constraints again where the use is written ([`UniverseSolver::instantiate`]).

mod constraints;
use constraints::*;

mod qualities;
use qualities::*;

#[cfg(test)]
mod test_support;

#[cfg(test)]
mod tests;

use {
    curios_analysis::{LevelModel, model, satisfiable},
    curios_common::recurse,
    curios_core::{
        Extent, Global, Level, LevelHead, Quality, QualityMetaId, Term, UniverseConstraint,
        UniverseConstraintOrigin, UniverseContext, UniverseError, UniverseMetaId, UniverseParam,
        UniverseRole, UniverseSeed, Universes,
    },
    curios_surface::FaithfulUniverse,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, VecDeque},
    },
};

/// Where a scheme is instantiated: the declaration an occurrence names, and where that occurrence is written, when a term of the declaration being elaborated wrote it. A constraint the scheme states is restated under both ([`UniverseSolver::instantiate_at`]).
#[derive(Debug, Clone, Copy)]
pub struct Occurrence {
    pub instantiated: Global,
    pub extent: Option<Extent>,
}

/// A stable point to which all universe assignments and constraints can be rolled back after a speculative elaboration branch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UniverseMark {
    constraints: StoreScope,
    solution_log_len: usize,
    quality_log_len: usize,
    quality_joins_len: usize,
    weak_len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UniverseStateToken {
    next_meta: usize,
    constraints: StoreMark,
    solution_log_len: usize,
    next_quality: usize,
    quality_log_len: usize,
    /// A weak equation is a write like any other to whoever asks whether the solver moved: it changes where the declaration's levels settle, so an elaboration that recorded one is not a pure function of its inputs.
    weak_len: usize,
}

impl UniverseStateToken {
    /// Whether the solver requires of every level here what it required at `other`: no metavariable minted, no constraint added and no level solved between the two. A weak equation requires nothing, the two instances it came from being one type whatever its levels are, so one recorded between the two is no difference here, though it is a write to whoever asks whether the solver moved at all.
    pub fn requires_the_same_as(&self, other: &Self) -> bool {
        self.next_meta == other.next_meta
            && self.constraints == other.constraints
            && self.solution_log_len == other.solution_log_len
            && self.next_quality == other.next_quality
            && self.quality_log_len == other.quality_log_len
    }
}

#[derive(Debug, Clone)]
struct UniverseMeta {
    provenance: Provenance,
    origin: Option<UniverseConstraintOrigin>,
    solution: Option<Level>,
}

/// Where a universe meta came from, which decides which member of an identified pair represents it and how finalization settles it.
///
/// Rocq keeps the same three apart: minimization touches no "user-introduced universe (i.e. coming from a user-written anonymous Type) … only the fresh universes generated for each global application" (`dev/doc/universes.md`), and `choose_canonical` keeps a rigid universe as its class's representative over a flexible one. Ordered weakest first, so an identified pair keeps its greater member.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Provenance {
    /// A classifier the elaborator invented, or a `Type` written outside an input position: minimized where finalization reaches it.
    Inferred,
    /// A level [`UniverseSolver::instantiate`] minted for one use's instance.
    Occurrence,
    /// A `Type` written in an input position — a level a caller chooses.
    Chosen,
}

impl From<UniverseRole> for Provenance {
    /// A seed or a fresh meta is written or invented, never an occurrence: only [`UniverseSolver::instantiate`] mints one of those.
    fn from(role: UniverseRole) -> Self {
        match role {
            UniverseRole::Generalizable => Provenance::Chosen,
            UniverseRole::Flexible => Provenance::Inferred,
        }
    }
}

/// The mutable algebraic-universe inference state.
#[derive(Debug, Clone)]
pub struct UniverseSolver {
    metas: Vec<UniverseMeta>,
    constraints: ConstraintStore,
    solution_log: Vec<UniverseMetaId>,
    next_meta: usize,
    /// The quality metavariables, each with what it was solved to, and the ones solved in the order they were, which a rollback unwinds ([`qualities`]).
    qualities: Vec<QualityMeta>,
    quality_log: Vec<QualityMetaId>,
    /// Each record type formed over two open qualities apart, with its extent ([`UniverseSolver::note_quality_join`]).
    quality_joins: Vec<(Quality, Quality, Option<Extent>)>,
    /// The pairs conversion met at positions a nominal family or a definition is irrelevant in, in the order it met them. None is a constraint, since no checker compares the two levels of one; each is tried where its declaration's levels settle ([`UniverseSolver::join_weak_equations`]).
    weak: Vec<(Level, Level)>,
    /// What [`UniverseSolver::zonk`] has made of a solved metavariable, kept once it names no metavariable at all, so a chain of solutions is followed once rather than at every read of a level that names its head ([`ZonkedLevels`]).
    zonked: RefCell<ZonkedLevels>,
    /// Whether the declaration being elaborated can still settle a universe level: from `Context::enter_item` until its levels are finalized. A witness resolved while this holds leaves the levels its scheme minted to that finalization; one resolved after has none left, and closes them where it resolves ([`UniverseSolver::settle_instance`]).
    scheme_open: bool,
}

/// The zonked solutions [`UniverseSolver::zonk`] keeps, each with how many solutions stood when it was taken.
///
/// A zonked solution naming no metavariable reads only solutions committed before it was taken, and a commitment never changes, so it stands until a rollback unwinds one of those: [`UniverseSolver::rollback`] drops each taken past what it keeps. Kept in the order they were taken, since the count of standing solutions never falls among those that survive, so a rollback drops a suffix.
#[derive(Debug, Clone, Default)]
struct ZonkedLevels {
    solutions: BTreeMap<UniverseMetaId, Level>,
    taken: Vec<(usize, UniverseMetaId)>,
}

impl ZonkedLevels {
    fn keep(&mut self, meta: UniverseMetaId, zonked: &Level, standing: usize) {
        if zonked.metas().next().is_none() && self.solutions.insert(meta, zonked.clone()).is_none()
        {
            self.taken.push((standing, meta));
        }
    }

    /// Drop each zonked solution taken while more than `standing` solutions stood.
    fn unwind(&mut self, standing: usize) {
        while let Some(&(taken, meta)) = self.taken.last()
            && taken > standing
        {
            self.taken.pop();
            self.solutions.remove(&meta);
        }
    }
}

/// The meta `level` is, when it is one bare meta at offset zero.
fn bare_meta(level: &Level) -> Option<UniverseMetaId> {
    match (level.constant, level.atoms.iter().next()) {
        (0, Some((&LevelHead::Meta(meta), &0))) if level.atoms.len() == 1 => Some(meta),
        _ => None,
    }
}

impl UniverseSolver {
    /// Whether `term` holds a universe or quality metavariable this solver has not settled.
    pub fn has_unsolved_meta(&self, term: &Term) -> bool {
        term.any_universe_meta(|meta| match self.zonk(&Level::meta(meta)) {
            Ok(level) => level.metas().next().is_some(),
            Err(_) => true,
        }) || term
            .any_quality_meta(|meta| matches!(self.quality(Quality::Meta(meta)), Quality::Meta(_)))
    }

    /// The metas reachable from `metas` through constraints and solutions — the declaration's universe closure.
    ///
    /// Reachability is a graph search over the occurrence index, so each constraint is visited once per newly reached level rather than once per pass over the whole store.
    fn connected_metas(
        &self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
    ) -> BTreeSet<UniverseMetaId> {
        let mut relevant = BTreeSet::new();
        let mut pending = metas.into_iter().collect::<Vec<_>>();
        while let Some(meta) = pending.pop() {
            if !relevant.insert(meta) {
                continue;
            }
            if let Some(solution) = self.solution(meta) {
                pending.extend(solution.metas());
            }
            for position in self.constraints.mentioning(LevelHead::Meta(meta)) {
                let constraint = self
                    .constraints
                    .get(position)
                    .expect("the occurrence index names a live constraint");
                pending.extend(constraint.lower.metas().chain(constraint.upper.metas()));
            }
        }
        relevant
    }

    fn discard_constraints(&mut self, metas: &BTreeSet<UniverseMetaId>) {
        self.constraints.retain(|constraint| {
            constraint
                .lower
                .metas()
                .chain(constraint.upper.metas())
                .all(|meta| !metas.contains(&meta))
        });
    }

    pub fn new(meta_floor: usize) -> Self {
        Self {
            metas: (0..meta_floor)
                .map(|_| UniverseMeta {
                    provenance: Provenance::Chosen,
                    origin: None,
                    solution: None,
                })
                .collect(),
            constraints: ConstraintStore::default(),
            solution_log: Vec::new(),
            next_meta: meta_floor,
            qualities: Vec::new(),
            quality_log: Vec::new(),
            quality_joins: Vec::new(),
            weak: Vec::new(),
            zonked: RefCell::default(),
            scheme_open: false,
        }
    }

    /// Open the scheme of the item about to elaborate: its levels may settle until it closes.
    pub fn open_scheme(&mut self) {
        self.scheme_open = true;
    }

    /// Close the item's scheme, its levels finalized.
    pub fn close_scheme(&mut self) {
        self.scheme_open = false;
    }

    /// Settle the levels a witness's scheme minted at one use: pinned to what its goal fixes while the item's scheme is open ([`Self::pin_instance`]), and closed there once it has closed ([`Self::close_instance`]).
    pub fn settle_instance(
        &mut self,
        minted: &[Level],
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), UniverseError> {
        match self.scheme_open {
            true => self.pin_instance(instance, determined),
            false => self.close_instance(minted, instance, determined),
        }
    }

    pub fn seed(&mut self, seeds: &[UniverseSeed]) {
        assert!(
            self.metas.is_empty(),
            "universes may only seed a fresh elaboration context"
        );
        self.metas = seeds
            .iter()
            .map(|seed| UniverseMeta {
                provenance: seed.role.into(),
                origin: seed.written.clone().map(UniverseConstraintOrigin::Written),
                solution: None,
            })
            .collect();
        self.next_meta = seeds.len();
    }

    pub fn fresh(
        &mut self,
        role: UniverseRole,
        origin: Option<UniverseConstraintOrigin>,
    ) -> UniverseMetaId {
        self.mint(role.into(), origin)
    }

    fn mint(
        &mut self,
        provenance: Provenance,
        origin: Option<UniverseConstraintOrigin>,
    ) -> UniverseMetaId {
        let id = UniverseMetaId(self.next_meta);
        self.next_meta += 1;
        self.metas.push(UniverseMeta {
            provenance,
            origin,
            solution: None,
        });
        id
    }

    /// Mint a level that stands for `beside` at an occurrence of its own: with the provenance of the level `beside` is, where it is one level, and an invented classifier's otherwise. A copy of a chosen level is a level a caller chooses, as the `Type` written in its place would be; a copy of a constant or of a maximum is nobody's until a position holds it.
    pub fn fresh_beside(
        &mut self,
        beside: &Level,
        origin: Option<UniverseConstraintOrigin>,
    ) -> Result<UniverseMetaId, UniverseError> {
        let provenance = bare_meta(&self.zonk(beside)?)
            .and_then(|meta| self.provenance(meta))
            .unwrap_or(Provenance::Inferred);

        Ok(self.mint(provenance, origin))
    }

    fn provenance(&self, meta: UniverseMetaId) -> Option<Provenance> {
        self.metas.get(meta.0).map(|entry| entry.provenance)
    }

    pub fn origin(&self, meta: UniverseMetaId) -> Option<&UniverseConstraintOrigin> {
        self.metas
            .get(meta.0)
            .and_then(|entry| entry.origin.as_ref())
    }

    /// Open a speculative scope, which [`UniverseSolver::release`] closes. Paired by hand at every site — see `Context::solution_mark` for why a closure bracket is not used here.
    pub fn mark(&mut self) -> UniverseMark {
        UniverseMark {
            constraints: self.constraints.enter(),
            solution_log_len: self.solution_log.len(),
            quality_log_len: self.quality_log.len(),
            quality_joins_len: self.quality_joins.len(),
            weak_len: self.weak.len(),
        }
    }

    /// End a scope, keeping whatever it left in place. Restoring rather than decrementing, so closing an outer scope also closes any inner one left open.
    pub fn release(&mut self, mark: UniverseMark) {
        self.constraints.release(mark.constraints);
    }

    pub fn state_token(&self) -> UniverseStateToken {
        UniverseStateToken {
            next_meta: self.next_meta,
            constraints: self.constraints.mark(),
            solution_log_len: self.solution_log.len(),
            next_quality: self.qualities.len(),
            quality_log_len: self.quality_log.len(),
            weak_len: self.weak.len(),
        }
    }

    /// Restore both stores to `mark`. Solutions are unwound first: the constraint journal's pre-images were taken *before* the assignments that rewrote them, so the two unwind in the same direction. A weak equation met since the mark goes with what met it.
    pub fn rollback(&mut self, mark: UniverseMark) {
        while self.solution_log.len() > mark.solution_log_len {
            let meta = self.solution_log.pop().unwrap();
            self.metas[meta.0].solution = None;
        }
        self.zonked.get_mut().unwind(mark.solution_log_len);
        self.unsolve_qualities(mark.quality_log_len);
        self.quality_joins.truncate(mark.quality_joins_len);
        self.constraints.rollback(mark.constraints);
        self.weak.truncate(mark.weak_len);
    }

    /// Release inference constraints after their enclosing declaration has finalized. Any relation that remains externally meaningful has already been projected into that declaration's [`UniverseContext`]; later uses reinsert the stored residual context at fresh instances.
    pub fn clear_constraints(&mut self) {
        self.constraints.clear();
        self.weak.clear();
    }

    /// Close every speculative scope at once and drop the journal. Sound only where no bracket is live: an item boundary a refusal reached by unwinding the recursion the hand-paired brackets sit on, past every `release` they would have called. Anywhere a scope is still open the journal is what its rollback restores from, which is why this is the one place the depth is reset outright rather than restored.
    pub fn abandon_speculation(&mut self) {
        self.constraints.abandon();
    }

    pub fn solution(&self, meta: UniverseMetaId) -> Option<&Level> {
        self.metas
            .get(meta.0)
            .and_then(|entry| entry.solution.as_ref())
    }

    /// Substitute every solved metavariable in `level`, and in whatever those solutions name, to a fixed point.
    ///
    /// Guarded by [`recurse`] rather than walked on a frame stack. The depth here is the length of a chain of metavariables each solved to a level naming the next, which the fixed prelude drives deep enough that a native recursion over it would be the tallest in the whole build. That is data-shaped depth: a chain's length is a function of how many constraints reached the declaration, not of anything anyone wrote.
    ///
    /// The cycle guard is *path*-scoped, which the recursion carries for free: a metavariable is on the path exactly while its own call is open, so one already open resolves to itself, unsubstituted. That is also what forbids memoizing a completed level and reusing it elsewhere — off that path the same metavariable does substitute, so a shared answer would be wrong.
    ///
    /// Each level is rebuilt through [`Level::substitute`] once its children have landed, so the offset, `max`, and constant arithmetic stays in one place and this walk only decides *what* each head resolves to.
    pub fn zonk(&self, level: &Level) -> Result<Level, UniverseError> {
        /// `visiting` is the chain of metavariables whose solutions are open above this call — the path the guard is scoped to.
        fn walk(
            solver: &UniverseSolver,
            level: &Level,
            visiting: &mut BTreeSet<UniverseMetaId>,
        ) -> Result<Level, UniverseError> {
            recurse(|| {
                let mut resolved = BTreeMap::new();

                // A head with nothing to resolve to — a parameter, an unsolved metavariable, or one already on the path — records no substitution, and the rebuild below leaves it standing as itself.
                for (head, _) in level.atoms() {
                    let LevelHead::Meta(meta) = head else {
                        continue;
                    };
                    if visiting.contains(&meta) {
                        continue;
                    }
                    if let Some(zonked) = solver.zonked.borrow().solutions.get(&meta) {
                        resolved.insert(head, zonked.clone());
                        continue;
                    }
                    let Some(solution) = solver.solution(meta).cloned() else {
                        continue;
                    };

                    visiting.insert(meta);
                    let zonked = walk(solver, &solution, visiting);
                    visiting.remove(&meta);
                    let zonked = zonked?;

                    solver
                        .zonked
                        .borrow_mut()
                        .keep(meta, &zonked, solver.solution_log.len());
                    resolved.insert(head, zonked);
                }

                level.substitute(|head| resolved.get(&head).cloned())
            })
        }

        walk(self, level, &mut BTreeSet::new())
    }

    /// Whether the store already reads `left` and `right` as one level, by the level algebra alone once both are zonked: an equation of the two ([`UniverseSolver::add_eq`]) would add no constraint and solve no metavariable.
    pub fn reads_as_one(&self, left: &Level, right: &Level) -> Result<bool, UniverseError> {
        let left = self.zonk(left)?;
        let right = self.zonk(right)?;

        Ok(left.structurally_leq(&right) && right.structurally_leq(&left))
    }

    pub fn add_leq(
        &mut self,
        lower: Level,
        upper: Level,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        self.add_constraint(UniverseConstraint { lower, upper }, origin)
    }

    pub fn add_eq(
        &mut self,
        left: Level,
        right: Level,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        // Hand-paired rather than bracketed: conversion reaches this from inside its own recursion, where a closure body is a stack frame per level.
        let mark = self.mark();
        if let Err(error) = self.default_shape_equal(&left, &right) {
            self.rollback(mark);
            self.release(mark);
            return Err(error);
        }
        if let Err(error) = self.add_leq(left.clone(), right.clone(), origin.clone()) {
            self.rollback(mark);
            self.release(mark);
            return Err(error);
        }
        if let Err(error) = self.add_leq(right, left, origin) {
            self.rollback(mark);
            self.release(mark);
            return Err(error);
        }
        self.release(mark);
        Ok(())
    }

    /// Record that `left` and `right` met at a position a nominal family is irrelevant in.
    ///
    /// The two instances are one type whatever these levels are, so nothing is required of them, and an equation here would refuse a program both checkers accept. They are recorded because one instance is what a declaration should hold where it can. Left alone, each occurrence settles from its own bounds: a level nothing else bounds becomes a parameter of the declaration, a former's level floats above its argument's, and one family's occurrences in one signature land apart. Rocq records the same pair, "'weak' constraints … when comparing universes in an irrelevant position", and says of doing without them that it "may produce arbitrary numbers of universes".
    pub fn add_weak_eq(&mut self, left: Level, right: Level) {
        if left != right {
            self.weak.push((left, right));
        }
    }

    /// Join every weak equation the store can hold that decides no level the declaration holds, and drop the rest.
    ///
    /// Asked where a declaration's levels are about to settle, so every requirement its elaboration raised is in the store. A pair with no open level on either side names no level to choose and is dropped unread: two constants, or a constant and a parameter. The rest are tried together, one check for the declaration; where that is refused they are retaken in the order they were met, each kept where the store stays consistent and says of the held levels what it said, a run of them at a time ([`Self::join_run`]).
    ///
    /// Where every pair is kept, the store that results is the one an equation at each of those positions would have given, so a declaration that elaborates with those equations required elaborates to the same levels with them weak. Where one is not, dropping it refuses nothing and decides nothing a checker reads: the two instances it came from convert as they stand. What the order chooses, there alone, is which level an irrelevant occurrence lands on.
    ///
    /// Rocq unifies a weak pair "when neither is smaller than the other and one is flexible". This joins a pair one side of which already bounds the other too: a former's level left above its argument's is slack a signature then generalizes.
    ///
    /// **A join puts nothing between two levels the declaration holds.** `held` are the levels that stand where conversion compares them, and a pair is kept only where the store then says of them, one against another and each against zero, what it said before ([`Self::held_relations`]). A weak equation is met at a position nothing reads, so it has no standing to decide a level something does. Two arms answering a `Result` at their own constructors' levels would otherwise make those two levels one through the declared `Result`'s, which no caller could then tell apart, and a small action sequenced into a region would hold the region's payload to the action's level, zero. The occurrence it leaves unjoined settles from its own bounds, at the join of the held levels below it.
    fn join_weak_equations(&mut self, held: &BTreeSet<UniverseMetaId>) {
        curios_common::profile!("universe::join_weak_equations");
        let mut met = BTreeSet::new();
        let pairs = std::mem::take(&mut self.weak)
            .into_iter()
            .filter_map(|(left, right)| {
                let left = self.zonk(&left).ok()?;
                let right = self.zonk(&right).ok()?;
                let open = left.metas().chain(right.metas()).next().is_some();
                (left != right && open).then_some((left, right))
            })
            // A pair met again is tried once. Whether the store holds a pair only falls as the pairs kept before it grow, so trying a repeat asks what its first copy's answer says, save where a pair kept between the two took one of its sides out of the shape `add_eq`'s default pins and the plain equation is asked instead — and what a weak pair decides is only where an irrelevant occurrence lands. A pair is ordered, where Rocq keeps its weak constraints as unordered pairs (`UPairSet`): its direction picks which meta that default pins, so `(u, v)` and `(v, u)` may leave different levels standing.
            .filter(|pair| met.insert(pair.clone()))
            .collect::<Vec<_>>();
        if pairs.is_empty() {
            return;
        }
        curios_common::sample!("universe::weak_equations", pairs.len());
        curios_common::sample!("universe::held_levels", held.len());

        let before = self.held_relations(held);
        self.join_run(&pairs, held, before.as_deref());
    }

    /// Join the pairs of `pairs` the store can hold, in order: each is kept where the store, with the pairs kept before it, stays consistent and says of the held levels what `before` says. `before` is `None` where reading the held levels met a loop in the store before any pair was tried, or one did not read, and no run is kept.
    ///
    /// **A run the store holds whole is kept whole, and one it does not is halved.** Whether a store holds a set of equations only falls as the set grows, so a run that is held together is one each of whose pairs would have been kept in its turn, and nothing is asked of them one by one; a run that is not held is tried as its two halves, the first before the second, down to the pair that is dropped. The pairs kept are the ones trying each alone in order keeps, and a declaration with a few hundred of them, a handful of which the store refuses, asks a few dozen times where it asked once for each.
    fn join_run(
        &mut self,
        pairs: &[(Level, Level)],
        held: &BTreeSet<UniverseMetaId>,
        before: Option<&[Vec<Option<u64>>]>,
    ) {
        curios_common::sample!("universe::weak_run", pairs.len());
        let mark = self.mark();
        let joined = pairs.iter().all(|(left, right)| {
            self.add_eq(
                left.clone(),
                right.clone(),
                UniverseConstraintOrigin::Converted,
            )
            .is_ok()
        }) && self.consistent()
            && before.is_some_and(|before| self.relations_stand(held, before));
        if joined {
            self.release(mark);
            return;
        }
        self.rollback(mark);
        self.release(mark);

        // The empty run is held, so a run that is not holds a pair: alone it is the one dropped.
        if pairs.len() > 1 {
            let (first, rest) = pairs.split_at(pairs.len() / 2);
            self.join_run(first, held, before);
            self.join_run(rest, held, before);
        } else {
            curios_common::note!(
                target: "curios_elab::universe",
                left = %pairs[0].0.faithful(),
                right = %pairs[0].1.faithful(),
                "dropped a weak equation the store refuses",
            );
        }
    }

    /// The levels a declaration holds, zero before them, each as the store reads it now: what [`Self::held_relations`] relates, in the order it relates them. `None` where one does not read.
    fn held_levels(&self, held: &BTreeSet<UniverseMetaId>) -> Option<Vec<Level>> {
        std::iter::once(Ok(Level::zero()))
            .chain(held.iter().map(|meta| self.zonk(&Level::meta(*meta))))
            .collect::<Result<_, _>>()
            .ok()
    }

    /// How far each of `levels` stands below `top` as the store entails it — the widest `k` with `level + k ≤ top`, and none where it entails no order between them — read off the least model of what the store derives below `top`. `None` where that reading meets a loop in the store.
    fn relations_below(&self, top: &Level, levels: &[Level]) -> Option<Vec<Option<u64>>> {
        let model = LevelModel::below(top, self.constraints.iter())?;

        Some(levels.iter().map(|level| model.admits(level)).collect())
    }

    /// What the store says of the levels a declaration holds, one against another and each against zero: for each, how far every one stands below it ([`Self::relations_below`]). Two stores that answer alike here leave a caller the same choice of those levels. `None` where reading them meets a loop in the store, or a held level does not read.
    fn held_relations(&self, held: &BTreeSet<UniverseMetaId>) -> Option<Vec<Vec<Option<u64>>>> {
        if held.is_empty() {
            return Some(Vec::new());
        }
        curios_common::profile!("universe::held_relations");
        let levels = self.held_levels(held)?;

        levels
            .iter()
            .map(|top| self.relations_below(top, &levels))
            .collect()
    }

    /// Whether the store says of the held levels what `before`, their relations as [`Self::held_relations`] read them of an earlier store this one extends, says.
    ///
    /// **One model a level.** A store that extends another entails all it did, so a relation can only have widened; the levels are read one top at a time, and the first whose relations moved ends the reading.
    fn relations_stand(
        &self,
        held: &BTreeSet<UniverseMetaId>,
        before: &[Vec<Option<u64>>],
    ) -> bool {
        if held.is_empty() {
            return true;
        }
        curios_common::profile!("universe::relations_stand");
        let Some(levels) = self.held_levels(held) else {
            return false;
        };

        levels
            .iter()
            .zip(before)
            .all(|(top, was)| self.relations_below(top, &levels).as_ref() == Some(was))
    }

    /// Record `lower ≤ upper` where the store stays consistent with it, and answer whether it did. A relation a use needs and nothing has settled is one the declaration comes to require, as every level settles by where it stands; one the store refuses is taken back, and the use is answered no. A speculative judgment around the use takes the relation back with everything else it recorded.
    pub fn settle_leq(
        &mut self,
        lower: Level,
        upper: Level,
        origin: UniverseConstraintOrigin,
    ) -> bool {
        let mark = self.mark();
        let held = self.add_leq(lower, upper, origin).is_ok() && self.consistent();
        if !held {
            self.rollback(mark);
        }
        self.release(mark);

        held
    }

    /// Default an equation whose zonked sides differ at exactly one meta atom with a shared offset: `max(s, α+k) = max(s, β+k)` pins one meta to the other instead of parking two inequalities the bound propagators cannot decompose. Without this, independently instantiated spellings of one written annotation meet only here, both metas survive to declaration finalization, and the scheme generalizes two parameters where the program wrote one universe.
    ///
    /// The direction solves the lesser [`Provenance`] toward the greater — an inferred level toward an occurrence's, an occurrence's toward a chosen one — so the pair keeps the member that decides how it settles; a pair of one provenance takes a fixed direction. The commitment is deliberately incomplete — `max(1, α) = max(1, β)` also admits solutions with the metas apart below the shared constant — so a program that genuinely needs distinct shape-equal instances is refused rather than over-generalized.
    fn default_shape_equal(&mut self, left: &Level, right: &Level) -> Result<(), UniverseError> {
        let left = self.zonk(left)?;
        let right = self.zonk(right)?;
        if left.constant != right.constant {
            return Ok(());
        }
        let only_left: Vec<_> = left
            .atoms
            .iter()
            .filter(|(head, offset)| right.atoms.get(head) != Some(offset))
            .collect();
        let only_right: Vec<_> = right
            .atoms
            .iter()
            .filter(|(head, offset)| left.atoms.get(head) != Some(offset))
            .collect();
        let ([(left_head, left_offset)], [(right_head, right_offset)]) =
            (only_left.as_slice(), only_right.as_slice())
        else {
            return Ok(());
        };
        if left_offset != right_offset {
            return Ok(());
        }
        let (LevelHead::Meta(a), LevelHead::Meta(b)) = (**left_head, **right_head) else {
            return Ok(());
        };
        let (Some(of_a), Some(of_b)) = (self.provenance(a), self.provenance(b)) else {
            return Ok(());
        };
        let (from, to) = match of_a.cmp(&of_b) {
            std::cmp::Ordering::Less => (a, b),
            std::cmp::Ordering::Greater => (b, a),
            std::cmp::Ordering::Equal if of_a == Provenance::Inferred => (a, b),
            std::cmp::Ordering::Equal => (b, a),
        };
        self.assign(from, Level::meta(to))
    }

    /// Record an inequality. Consistency is *not* decided here.
    ///
    /// Deciding it at every insertion — push, run a full consistency check, pop on refusal — would pay a least model over the whole store at each of the prelude's many insertions, and nothing would buy it. The rendered diagnostic is the least conflict the store holds, read off the store by [`Self::check_consistent`] wherever it is asked, so a check taken later names it as well; the declaring item comes from `Error::in_declaration` at the item boundary, not from the insertion site; and every caller outside this module propagates the refusal with `?` rather than recovering from it, so no decision depended on learning it early.
    ///
    /// What decides consistency is what needs a *verdict* rather than a diagnostic: the speculative commit in `close_stalled_components`, and the declaration-boundary checks in `finalize` and `solve_flexible_in`. An inconsistent set can therefore exist between an insertion and the next of those, which is the price — the boundary refuses the declaration either way, and `curios-cert` decides every context it assumes independently.
    pub fn add_constraint(
        &mut self,
        mut constraint: UniverseConstraint,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        constraint.lower = self.zonk(&constraint.lower)?;
        constraint.upper = self.zonk(&constraint.upper)?;
        if constraint.lower.structurally_leq(&constraint.upper) {
            return Ok(());
        }
        if constraint.lower.atoms.is_empty() && constraint.upper.atoms.is_empty() {
            return if constraint.lower.constant <= constraint.upper.constant {
                Ok(())
            } else {
                Err(UniverseError::Inconsistency {
                    lower: constraint.lower,
                    upper: constraint.upper,
                    path: vec![origin],
                })
            };
        }
        self.constraints.push(constraint, origin);
        Ok(())
    }

    /// Whether `meta` is unsolved and eligible for minimization.
    fn is_open_flexible(&self, meta: UniverseMetaId) -> bool {
        self.metas.get(meta.0).is_some_and(|entry| {
            entry.provenance == Provenance::Inferred && entry.solution.is_none()
        })
    }

    /// The bound `lower ≤ max(c, atom + k)` places on `atom`, or `None` when it places none.
    ///
    /// With `c = 0` the atom must cover `lower` outright, and cancelling `k` answers directly. With `c > 0` the upper side already covers everything up to `c` on its own, so only the part of `lower` exceeding `c` constrains the atom — and whether `c` covers `lower` is decidable only when `lower` is a known constant. A larger constant *is* determined: `c` cannot supply it, so the atom must, even though the upper side is not a bare atom.
    fn atom_lower_bound(upper_constant: u32, upper_offset: u32, lower: Level) -> Option<Level> {
        if upper_constant != 0 {
            if lower.atoms().next().is_some() {
                return None;
            }
            if lower.constant_part() <= upper_constant {
                return None;
            }
        }
        lower.cancel_offset(upper_offset)
    }

    /// The least level satisfying every `lower ≤ meta + k` bound, or `None` when some bound still mentions an unsolved flexible level and the answer would not yet be final.
    ///
    /// Cancelling `k` yields the principal solution only when every lower atom carries at least that offset. Otherwise the solution would need a predecessor expression, which the level algebra deliberately does not contain, and finalization retains the relation as constrained polymorphism instead.
    fn principal_lower_bound(&self, meta: UniverseMetaId) -> Result<Option<Level>, UniverseError> {
        let head = LevelHead::Meta(meta);
        let mut lowers = Vec::new();
        for position in self.constraints.mentioning(head).collect::<Vec<_>>() {
            let constraint = self
                .constraints
                .get(position)
                .expect("the occurrence index names a live constraint");
            if constraint.upper.atoms.len() != 1 {
                continue;
            }
            let Some(&upper_offset) = constraint.upper.atoms.get(&head) else {
                continue;
            };
            let lower = constraint
                .lower
                .substitute(|lower_head| (lower_head == head).then(Level::zero))?;
            let Some(lower) =
                Self::atom_lower_bound(constraint.upper.constant, upper_offset, lower)
            else {
                continue;
            };
            if lower.metas().any(|other| self.is_open_flexible(other)) {
                return Ok(None);
            }
            if !lower.is_zero() {
                lowers.push(lower);
            }
        }
        Ok((!lowers.is_empty()).then(|| Level::max(lowers)))
    }

    /// The least level satisfying the *currently known* part of every `lower ≤ meta + k` bound, reading each still-open flexible level in a lower position at the floor `floors` has found for it, and at its least value, zero, where it has found none.
    ///
    /// [`Self::principal_lower_bound`] refuses to answer while any lower bound mentions an open level, because the answer could still grow. That is the right rule while propagation can still make progress, but a cycle of mutual bounds never does: every level in it is waiting for another. Weakening the open atoms to a floor already implied yields an *implied* bound — a level is monotone in its atoms, so the result is a sound floor rather than a guess — which is enough to break the tie and resume propagation.
    fn grounded_lower_bound(
        &self,
        meta: UniverseMetaId,
        floors: &BTreeMap<UniverseMetaId, Level>,
    ) -> Result<Option<Level>, UniverseError> {
        let head = LevelHead::Meta(meta);
        let mut lowers = Vec::new();
        for position in self.constraints.mentioning(head).collect::<Vec<_>>() {
            let constraint = self
                .constraints
                .get(position)
                .expect("the occurrence index names a live constraint");
            if constraint.upper.atoms.len() != 1 {
                continue;
            }
            let Some(&upper_offset) = constraint.upper.atoms.get(&head) else {
                continue;
            };
            let lower = constraint.lower.substitute(|lower_head| {
                if lower_head == head {
                    return Some(Level::zero());
                }
                match lower_head {
                    LevelHead::Meta(other) if self.is_open_flexible(other) => {
                        Some(floors.get(&other).cloned().unwrap_or_else(Level::zero))
                    }
                    _ => None,
                }
            })?;
            let Some(lower) =
                Self::atom_lower_bound(constraint.upper.constant, upper_offset, lower)
            else {
                continue;
            };
            if !lower.is_zero() {
                lowers.push(lower);
            }
        }
        Ok((!lowers.is_empty()).then(|| Level::max(lowers)))
    }

    /// Whether any constraint genuinely bounds `meta` from below — names it on its upper side. A level with no such bound has the unconditional least solution zero.
    fn is_bounded_below(&self, meta: UniverseMetaId) -> bool {
        self.constraints
            .mentioning(LevelHead::Meta(meta))
            .filter_map(|position| self.constraints.get(position))
            .any(|constraint| {
                !constraint.lower.structurally_leq(&constraint.upper)
                    && constraint.upper.metas().any(|candidate| candidate == meta)
            })
    }

    /// The levels whose own bounds mention `meta`, and which may therefore become solvable once `meta` is. Collected before the assignment, since committing it substitutes `meta` out of exactly these constraints.
    fn dependents_of(&self, meta: UniverseMetaId) -> BTreeSet<UniverseMetaId> {
        self.constraints
            .mentioning(LevelHead::Meta(meta))
            .filter_map(|position| self.constraints.get(position))
            .flat_map(|constraint| constraint.upper.metas())
            .filter(|other| *other != meta)
            .collect()
    }

    /// Minimize the flexible levels in `metas` to their least solutions.
    ///
    /// Solving is worklist-driven: a level is revisited only when one of the levels its bounds mention has just been solved. Rescanning every constraint for every level after every assignment would make this quadratic in the size of a declaration's universe closure.
    fn solve_flexible_in(&mut self, metas: &BTreeSet<UniverseMetaId>) -> Result<(), UniverseError> {
        curios_common::profile!("universe::solve_flexible_in");
        self.merge_forced_equalities(metas)?;

        let mut queue = metas.iter().copied().collect::<VecDeque<_>>();
        let mut queued = metas.clone();
        loop {
            while let Some(meta) = queue.pop_front() {
                queued.remove(&meta);
                if !self.is_open_flexible(meta) {
                    continue;
                }
                let Some(level) = self.principal_lower_bound(meta)? else {
                    continue;
                };
                let dependents = self.dependents_of(meta);
                self.assign(meta, level)?;
                for other in dependents {
                    if metas.contains(&other) && queued.insert(other) {
                        queue.push_back(other);
                    }
                }
            }

            // A flexible level that occurs only in lower positions is zero. Default the lowest such id, then resume propagation: for `v + 1 ≤ u`, defaulting `v` first derives `u = 1`. Taking these before the stalled levels below is what keeps that derivation available, since a level nothing bounds from below can never be the one holding a cycle together.
            if let Some(meta) = metas
                .iter()
                .copied()
                .find(|meta| self.is_open_flexible(*meta) && !self.is_bounded_below(*meta))
            {
                let dependents = self.dependents_of(meta);
                self.assign(meta, Level::zero())?;
                for other in dependents {
                    if metas.contains(&other) && queued.insert(other) {
                        queue.push_back(other);
                    }
                }
                continue;
            }

            // Everything still open is now bounded above by a level that is itself unsolved, so propagation alone will never resume: each member is waiting on another.
            let stalled = metas
                .iter()
                .copied()
                .filter(|meta| self.is_open_flexible(*meta))
                .collect::<BTreeSet<_>>();
            if stalled.is_empty() {
                break;
            }
            let woken = self.close_stalled_components(&stalled)?;
            if stalled.iter().all(|meta| self.is_open_flexible(*meta)) {
                break;
            }
            for other in woken {
                if metas.contains(&other) && queued.insert(other) {
                    queue.push_back(other);
                }
            }
        }
        self.check_consistent()
    }

    /// Break a stalled set by closing each of its components at the floors its bounds imply, each round reading the floors the round before found until none rises, keeping only the closures that survive a consistency check. Returns the levels whose bounds mentioned a committed one.
    ///
    /// A stall has two shapes, and only one of them may be closed. A *cycle* of mutual bounds — `max(1, ?u) ≤ max(1, ?v)` with its converse, which is what witness dispatch emits — has a least solution, and the floor assignment witnesses it. A *disjunction* like `1 ≤ max(?u, ?v)` has none: either level may carry the bound, so choosing one is arbitrary and would silently strip a declaration of polymorphism it is entitled to keep. Attempting the floor and rolling back on inconsistency distinguishes them without either shape having to be recognized syntactically: the disjunction's floor reduces to `1 ≤ 0` and fails, while the cycle's reduces to `1 ≤ 1` and holds.
    ///
    /// A cycle's floors feed one another: `1 ≤ ?v` with `?v ≤ ?u` puts `?v` at one in the first round and `?u` at one only in the next, so one round from zero would leave `?u` below `?v` and the closure refused as a disjunction's is. A consistent store's least solution is reached within as many rounds as the component has members, since a derivation of a floor repeats no member, and one that is not is refused by the check.
    ///
    /// Components are closed independently so that one unresolvable disjunction cannot veto an unrelated cycle elsewhere in the same declaration.
    fn close_stalled_components(
        &mut self,
        stalled: &BTreeSet<UniverseMetaId>,
    ) -> Result<BTreeSet<UniverseMetaId>, UniverseError> {
        let mut remaining = stalled.clone();
        let mut woken = BTreeSet::new();
        while let Some(&seed) = remaining.iter().next() {
            let component = self
                .connected_metas([seed])
                .intersection(&remaining)
                .copied()
                .collect::<BTreeSet<_>>();
            remaining = remaining.difference(&component).copied().collect();

            // Every floor is read from the same pre-assignment state, so the component closes simultaneously rather than each member seeing the levels committed before it; the rounds end where none rises.
            let mut floors = component
                .iter()
                .map(|meta| (*meta, Level::zero()))
                .collect::<BTreeMap<_, _>>();
            for _ in 0..=component.len() {
                let mut raised = false;
                let mut next = BTreeMap::new();
                for meta in &component {
                    let floor = self
                        .grounded_lower_bound(*meta, &floors)?
                        .unwrap_or_else(Level::zero);
                    raised |= floors.get(meta) != Some(&floor);
                    next.insert(*meta, floor);
                }
                floors = next;
                if !raised {
                    break;
                }
            }
            let dependents = component
                .iter()
                .flat_map(|meta| self.dependents_of(*meta))
                .collect::<BTreeSet<_>>();

            let mark = self.mark();
            let committed = floors
                .into_iter()
                .try_for_each(|(meta, floor)| self.assign(meta, floor))
                .is_ok()
                && self.consistent();
            if !committed {
                self.rollback(mark);
            }
            self.release(mark);
            if committed {
                woken.extend(dependents);
            }
        }
        Ok(woken)
    }

    /// Solve every level the constraints determine before allocating generalized parameters, so a scheme generalizes only the levels a caller can actually choose.
    ///
    /// Three shapes determine a level, and each is an equation every solution of the store satisfies — so solving it is principal, and the scheme keeps exactly the instances it had with fewer parameters to state them in:
    ///
    /// - **Mutual atoms.** `a ≤ b` and `b ≤ a` between two bare atoms at one offset: the equality classes conversion generates, collapsed onto the member of greatest [`Provenance`], the lower id between two of one.
    /// - **A level bounded by zero.** `max(…, a, …) ≤ 0` holds over the naturals only with `a` at zero, so each bare meta on the lower side of a constraint whose upper side is the zero level is zero. A part at a positive offset makes the store unsatisfiable, which is the consistency check's to report.
    /// - **A lone level equal to a maximum.** `a ≤ E` and `E ≤ a` with `a` a bare meta that `E` does not mention: `a` *is* `E`. This is solving for `a`, not unifying two maxima — `max(a, b) = max(c, d)` determines none of them and stays two constraints.
    ///
    /// The last two are what conversion leaves behind when an equation's sides differ in more than one atom, which `add_eq`'s shape-equal default does not take. Left unsolved, a level bounded by zero is a parameter every caller has to supply as zero, and a lone equality is two constraints the certifier's entailment pays for in time; solving them removes more constraints than levels, since a bound on a solved level is often trivially true.
    ///
    /// Each round collects *every* determined level rather than the first. Solving can expose new ones — `a ≤ c` and `c ≤ b` become mutual once `a` and `b` coincide — so rounds repeat to a fixpoint, but a round costs one pass over the store instead of one pass per solution. Within one round a solution may not mention a level another solution replaces, nor replace one another solution mentions, so no two solutions of a round refer to each other; the next round sees the store they rewrote.
    fn merge_forced_equalities(
        &mut self,
        metas: &BTreeSet<UniverseMetaId>,
    ) -> Result<(), UniverseError> {
        curios_common::profile!("universe::merge_forced_equalities");
        loop {
            let mut assignments = Vec::new();
            {
                let inequalities = self
                    .constraints
                    .iter()
                    .filter(|constraint| {
                        constraint.lower.constant == 0
                            && constraint.upper.constant == 0
                            && constraint.lower.atoms.len() == 1
                            && constraint.upper.atoms.len() == 1
                    })
                    .map(|constraint| (&constraint.lower, &constraint.upper))
                    .collect::<BTreeSet<_>>();

                let mut replaced = BTreeSet::new();
                let mut mentioned = BTreeSet::new();
                for (lower, upper) in &inequalities {
                    if !inequalities.contains(&(*upper, *lower)) {
                        continue;
                    }
                    let (&lower_head, &lower_offset) = lower.atoms.iter().next().unwrap();
                    let (&upper_head, &upper_offset) = upper.atoms.iter().next().unwrap();
                    if lower_offset != upper_offset || lower_head == upper_head {
                        continue;
                    }
                    let assignment = match (lower_head, upper_head) {
                        (LevelHead::Meta(meta), LevelHead::Param(param))
                        | (LevelHead::Param(param), LevelHead::Meta(meta))
                            if metas.contains(&meta) =>
                        {
                            Some((meta, Level::param(param)))
                        }
                        (LevelHead::Meta(left), LevelHead::Meta(right))
                            if metas.contains(&left) && metas.contains(&right) =>
                        {
                            let (replace, retain) =
                                match self.provenance(left).cmp(&self.provenance(right)) {
                                    std::cmp::Ordering::Less => (left, right),
                                    std::cmp::Ordering::Greater => (right, left),
                                    std::cmp::Ordering::Equal if left > right => (left, right),
                                    std::cmp::Ordering::Equal => (right, left),
                                };
                            Some((replace, Level::meta(retain)))
                        }
                        _ => None,
                    };
                    if let Some((meta, level)) = assignment
                        && replaced.insert(meta)
                    {
                        mentioned.extend(level.metas());
                        assignments.push((meta, level));
                    }
                }

                let mut determine = |meta: UniverseMetaId, level: Level| {
                    if !metas.contains(&meta)
                        || replaced.contains(&meta)
                        || mentioned.contains(&meta)
                        || level
                            .metas()
                            .any(|other| other == meta || replaced.contains(&other))
                    {
                        return;
                    }
                    replaced.insert(meta);
                    mentioned.extend(level.metas());
                    assignments.push((meta, level));
                };

                // The store is read once, and only the half a lone equality looks up is indexed: the constraints bounding a maximum by a bare meta. Ordering every constraint to find the few of that shape would cost several times the merge it serves.
                let bounding_a_meta = self
                    .constraints
                    .iter()
                    .filter(|constraint| {
                        bare_meta(&constraint.upper).is_some()
                            && bare_meta(&constraint.lower).is_none()
                    })
                    .map(|constraint| (&constraint.lower, &constraint.upper))
                    .collect::<BTreeSet<_>>();
                for constraint in self.constraints.iter() {
                    let (lower, upper) = (&constraint.lower, &constraint.upper);
                    if upper.is_zero() {
                        for (&head, &offset) in &lower.atoms {
                            if let (LevelHead::Meta(meta), 0) = (head, offset) {
                                determine(meta, Level::zero());
                            }
                        }
                        continue;
                    }
                    // Two bare atoms are the mutual case above; a lone meta against any other level is this one.
                    if let Some(meta) = bare_meta(lower)
                        && bare_meta(upper).is_none()
                        && bounding_a_meta.contains(&(upper, lower))
                    {
                        determine(meta, upper.clone());
                    }
                }
            }
            if assignments.is_empty() {
                return Ok(());
            }
            for (meta, level) in assignments {
                self.assign(meta, level)?;
            }
        }
    }

    /// Commit a solution, then normalize the constraints that mention it.
    ///
    /// Substituting here rather than re-zonking at every read is what keeps the store's normalization invariant: the work is proportional to the solved level's degree, and every later reader sees a settled inequality. A second assignment to one meta is ignored, so a solution is committed at most once and the journal stays a faithful inverse.
    fn assign(&mut self, meta: UniverseMetaId, level: Level) -> Result<(), UniverseError> {
        curios_common::profile!("universe::assign");
        let entry = self
            .metas
            .get_mut(meta.0)
            .ok_or(UniverseError::UnknownMeta(meta))?;
        if entry.solution.is_some() {
            return Ok(());
        }
        entry.solution = Some(level);
        self.solution_log.push(meta);

        let head = LevelHead::Meta(meta);
        let solution = self.zonk(&Level::meta(meta))?;
        self.constraints.substitute_head(head, &solution)
    }

    /// Instantiate a closed context for one use, `occurrence`, returning its fresh argument vector after inserting the substituted residual constraints transactionally. The levels it mints are the use's own — `Provenance::Occurrence`.
    pub fn instantiate(
        &mut self,
        context: &UniverseContext,
        occurrence: Occurrence,
    ) -> Result<Universes, UniverseError> {
        let levels = (0..context.parameter_count)
            .map(|_| Level::meta(self.mint(Provenance::Occurrence, None)))
            .collect::<Vec<_>>();
        // A sort the declaration leaves to its caller is this use's to state: a quality nothing has decided yet.
        let qualities = (0..context.quality_count)
            .map(|_| self.fresh_quality(None))
            .collect::<Vec<_>>();
        let universes = Universes { levels, qualities };
        self.instantiate_at(context, &universes, occurrence)?;
        Ok(universes)
    }

    /// Insert a closed context's residual constraints at an already chosen occurrence instance, each restated under `occurrence`: at the place the occurrence is written, and naming the declaration it instantiates. Used when re-elaborating an explicit `Instance`: its stored arguments are authoritative and must not be replaced by a second fresh instantiation.
    ///
    /// **A scheme's bounds are raised where an occurrence mints its instance, and nowhere else.** A term that holds an instance holds one whose bounds the occurrence that minted it raised, or one a definition's body states under the scheme that definition was checked under, whose bounds the restatement of that scheme at its own occurrence entails; so a reader of a facet at an instance a term already holds asks only that it states the scheme ([`UniverseContext::check_instance`]), and every bound restated names whose it is.
    ///
    /// A stored context is validated where it is built ([`Self::generalize`]) and where it is restored (`validate_universes`), not here: every occurrence of every polymorphic binding instantiates, and re-deciding a context's consistency per occurrence repeats a whole constraint solve for an invariant that cannot have changed.
    pub fn instantiate_at(
        &mut self,
        context: &UniverseContext,
        universes: &Universes,
        occurrence: Occurrence,
    ) -> Result<(), UniverseError> {
        context.check_instance(universes)?;
        let levels = &universes.levels;
        let mark = self.mark();
        for constraint in &context.constraints {
            let substitute = |level: &Level| {
                level.substitute(|head| match head {
                    LevelHead::Param(param) if param.0 < context.parameter_count => {
                        Some(levels[param.0].clone())
                    }
                    LevelHead::Param(param) => Some(Level::param(UniverseParam(
                        param.0 - context.parameter_count,
                    ))),
                    LevelHead::Meta(_) => None,
                })
            };
            let instantiated = match (substitute(&constraint.lower), substitute(&constraint.upper))
            {
                (Ok(lower), Ok(upper)) => UniverseConstraint { lower, upper },
                (Err(error), _) | (_, Err(error)) => {
                    self.release(mark);
                    return Err(error);
                }
            };
            // A scheme stores its inequalities alone, so where its declaration raised one is no part of it. A conflict is found once the terms that forced it are checked and left behind, so nothing places it on its way out: the constraint is raised again where the occurrence that instantiates it is written and names the declaration instantiated, and the conflict it closes is reported at a term its reader wrote, with whose bound it is.
            if let Err(error) = self.add_constraint(
                instantiated,
                UniverseConstraintOrigin::Restated {
                    declaration: occurrence.instantiated,
                    extent: occurrence.extent,
                },
            ) {
                self.rollback(mark);
                self.release(mark);
                return Err(error);
            }
        }
        self.release(mark);
        Ok(())
    }

    /// Generalize the requested unsolved metas in deterministic id order, rewriting all constraints that mention only those metas into a closed declaration context.
    pub fn generalize(
        &self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
    ) -> Result<(UniverseContext, BTreeMap<UniverseMetaId, Level>), UniverseError> {
        let metas = metas
            .into_iter()
            .filter(|meta| self.solution(*meta).is_none())
            .collect::<BTreeSet<_>>();
        let replacement = metas
            .iter()
            .enumerate()
            .map(|(index, meta)| (*meta, Level::param(UniverseParam(index))))
            .collect::<BTreeMap<_, _>>();
        let local_parameter_count = replacement.len();
        let rewrite = |level: &Level| {
            if level
                .params()
                .any(|param| param.0.checked_add(local_parameter_count).is_none())
            {
                return Err(UniverseError::OffsetOverflow);
            }
            level.substitute(|head| match head {
                LevelHead::Meta(meta) => replacement.get(&meta).cloned(),
                LevelHead::Param(param) => {
                    Some(Level::param(UniverseParam(param.0 + local_parameter_count)))
                }
            })
        };
        let mut constraints = Vec::new();
        for constraint in self.constraints.iter() {
            let mentioned = constraint
                .lower
                .metas()
                .chain(constraint.upper.metas())
                .collect::<BTreeSet<_>>();
            if mentioned.is_disjoint(&metas) {
                continue;
            }
            if mentioned.iter().all(|meta| metas.contains(meta)) {
                constraints.push(UniverseConstraint {
                    lower: rewrite(&constraint.lower)?,
                    upper: rewrite(&constraint.upper)?,
                });
            } else {
                return Err(UniverseError::EscapingLevel);
            }
        }
        constraints
            .sort_by(|left, right| (&left.lower, &left.upper).cmp(&(&right.lower, &right.upper)));
        constraints.dedup_by(|left, right| left.lower == right.lower && left.upper == right.upper);
        constraints.retain(|constraint| !constraint.lower.structurally_leq(&constraint.upper));
        let context = UniverseContext {
            parameter_count: replacement.len(),
            quality_count: 0,
            constraints,
        };
        universe_context_validate(&context)?;
        Ok((context, replacement))
    }

    /// The levels that carry `metas` ([`Self::representatives`]), each kept from minimization: one an invented classifier or a `Type` written outside an input position stood for is an occurrence's from here on.
    fn hold(
        &mut self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
    ) -> BTreeSet<UniverseMetaId> {
        let held = self.representatives(metas);
        for meta in &held {
            if let Some(entry) = self.metas.get_mut(meta.0)
                && entry.provenance == Provenance::Inferred
            {
                entry.provenance = Provenance::Occurrence;
            }
        }

        held
    }

    /// Solve `metas` as if each were an inferred output, restoring their declared roles afterwards. Solving ranges over `scope` so a minimized level can still be determined by its relations to levels outside the requested set.
    ///
    /// Minimization is a property of the *position* a level occupies rather than of the level itself: one written `Type` is an input where a use site can choose it and an ordinary classifier where it cannot.
    fn minimize(
        &mut self,
        metas: &BTreeSet<UniverseMetaId>,
        scope: &BTreeSet<UniverseMetaId>,
    ) -> Result<(), UniverseError> {
        curios_common::profile!("universe::minimize");
        let roles = metas
            .iter()
            .map(|meta| {
                let entry = self
                    .metas
                    .get_mut(meta.0)
                    .ok_or(UniverseError::UnknownMeta(*meta))?;
                let provenance = entry.provenance;
                entry.provenance = Provenance::Inferred;
                Ok((*meta, provenance))
            })
            .collect::<Result<Vec<_>, UniverseError>>()?;
        let solved = self.solve_flexible_in(scope);
        for (meta, provenance) in roles {
            self.metas[meta.0].provenance = provenance;
        }
        solved
    }

    /// The unsolved metas that carry `metas`, following each solution to the metas it names. Following one link is not enough, because the meta a link lands on may itself be solved; a level solved to a constant carries no meta and needs no parameter.
    ///
    /// Each meta is followed once, whichever meta led to it: a chain of solutions followed a link a round over every meta still in hand costs its length at each of its links.
    pub fn representatives(
        &self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
    ) -> BTreeSet<UniverseMetaId> {
        let mut carrying = BTreeSet::new();
        let mut followed = BTreeSet::new();
        let mut pending = metas.into_iter().collect::<Vec<_>>();
        while let Some(meta) = pending.pop() {
            if !followed.insert(meta) {
                continue;
            }
            match self.solution(meta) {
                Some(level) => pending.extend(level.metas()),
                None => {
                    carrying.insert(meta);
                }
            }
        }

        carrying
    }

    /// Settle `metas` at their least solutions, round after round until one settles nothing.
    ///
    /// A level with a principal lower bound takes it, and one nothing bounds from below takes zero. One whose only lower bounds are disjunctive stays, for the reason [`Self::close_stalled_components`] declines a disjunction, and so does one a lower bound of which still names an open level. A round commits every settlement it finds and checks consistency once, since a check reads the whole store; a round the check refuses is retaken one level at a time, keeping each settlement that stays consistent.
    fn settle(&mut self, metas: &BTreeSet<UniverseMetaId>) -> Result<(), UniverseError> {
        curios_common::profile!("universe::settle");
        loop {
            let mark = self.mark();
            let settled = self.settle_round(metas, false)?;
            if settled == 0 {
                self.release(mark);
                return Ok(());
            }
            if self.consistent() {
                self.release(mark);
                continue;
            }
            self.rollback(mark);
            self.release(mark);
            if self.settle_round(metas, true)? == 0 {
                return Ok(());
            }
        }
    }

    /// One pass of [`Self::settle`], each level read after the settlements before it, answering how many it settled. `checked` commits a settlement only where the store stays consistent.
    fn settle_round(
        &mut self,
        metas: &BTreeSet<UniverseMetaId>,
        checked: bool,
    ) -> Result<usize, UniverseError> {
        let mut settled = 0;
        for &meta in metas {
            if self.solution(meta).is_some() {
                continue;
            }
            let level = match self.principal_lower_bound(meta)? {
                Some(level) => level,
                None if !self.is_bounded_below(meta) => Level::zero(),
                None => continue,
            };
            if !checked {
                self.assign(meta, level)?;
                settled += 1;
                continue;
            }
            let mark = self.mark();
            let committed = self.assign(meta, level).is_ok() && self.consistent();
            if !committed {
                self.rollback(mark);
            }
            self.release(mark);
            if committed {
                settled += 1;
            }
        }
        Ok(settled)
    }

    /// Identify each chosen level bounded only from above, by one other chosen level, with that level.
    ///
    /// Such a level occurs where a caller passes a type — `pure(@A: Type, A) -> M(A)`'s `A`, bounded by `M`'s domain — and cumulativity already lets a caller pass anything smaller, so identifying it with its bound takes no program from a caller. What it takes away is an implementation's licence to answer at a smaller level: generalized apart, `Monad`'s three method levels would be five parameters in all, a witness would pin them at zero and refuse `!` at a large payload, and left generic they would put maxima into the domains of the witnesses built over them, which a use could only meet as a disjunction. Lean's `Pure (f : Type u → Type v)` declares `pure {α : Type u}`, and Rocq's manual writes its monad `monad@{i}` with `unit : forall (A : Type@{i}), A -> m A`. A level with two bounds names no one level to be, and stays.
    fn identify_bounded_choices(
        &mut self,
        metas: &BTreeSet<UniverseMetaId>,
    ) -> Result<(), UniverseError> {
        loop {
            let mut identified = false;
            for &meta in metas {
                if self.solution(meta).is_some()
                    || self.provenance(meta) != Some(Provenance::Chosen)
                {
                    continue;
                }
                let head = LevelHead::Meta(meta);
                let mut bound = None;
                let mut sole = true;
                for position in self.constraints.mentioning(head).collect::<Vec<_>>() {
                    let constraint = self
                        .constraints
                        .get(position)
                        .expect("the occurrence index names a live constraint");
                    if constraint.lower.structurally_leq(&constraint.upper) {
                        continue;
                    }
                    let upper = match (bare_meta(&constraint.lower), bare_meta(&constraint.upper)) {
                        (Some(lower), Some(upper))
                            if lower == meta
                                && upper != meta
                                && self.provenance(upper) == Some(Provenance::Chosen) =>
                        {
                            upper
                        }
                        _ => {
                            sole = false;
                            break;
                        }
                    };
                    if bound.is_some_and(|bound| bound != upper) {
                        sole = false;
                        break;
                    }
                    bound = Some(upper);
                }
                if let (true, Some(bound)) = (sole, bound) {
                    self.assign(meta, Level::meta(bound))?;
                    identified = true;
                }
            }
            if !identified {
                return Ok(());
            }
        }
    }

    /// Settle the levels no caller can choose and bind every surviving one as a deterministic declaration parameter.
    ///
    /// `interface` is the declaration's externally visible universe surface — its type and the registry signatures a use site instantiates. `internal` levels occur only in the body, so no occurrence could ever choose them; they are minimized instead of becoming parameters a caller cannot supply. An internal level with no principal solution is still generalized, because the residual context must stay closed.
    ///
    /// `compared` are the interface's levels that stand where conversion compares them: in a domain, or in a result that is no sort, at a position no family or definition is irrelevant in. Each is held with every level joined to it: the declaration's parameter with its bounds, neither minimized nor settled, since a caller that needs it higher has no other way to say so, and a level it was settled at is one a later use can tell from the one it needed. Read off the signature's term and followed by class, so which member of an identified pair survives decides nothing.
    ///
    /// Two kinds of level are settled rather than generalized, by `Self::settle`: a level the constraints reach that neither the type nor the body mentions, and an interface level whose representative is an occurrence's and which nothing holds. The last is Rocq's minimization — at an application, "a `j ≤ i` constraint will be generated. It is however often the case that an equation `j = i` would be more appropriate, when `f`'s universes are fresh" — and Agda's under `--cumulativity`, which instantiates a level bounded only from below to the join of its bounds. It keeps a signature from carrying a level for each occurrence: nothing compares such a level, so the least one takes nothing from a caller. Both checkers size a tuple type or a Π from its parts as they are written, so where an occurrence lands decides no sort either would dispute.
    pub fn finalize(
        &mut self,
        interface: impl IntoIterator<Item = UniverseMetaId>,
        internal: impl IntoIterator<Item = UniverseMetaId>,
        compared: impl IntoIterator<Item = UniverseMetaId>,
    ) -> Result<UniverseContext, UniverseError> {
        curios_common::profile!("universe::finalize");
        // A held level is no classifier to minimize, whatever wrote it: a `Type` written in a result is an occurrence's from here on, which keeps it its class's representative over an invented one. Read first, while every level elaboration left open is still open, since the join that follows is told which levels it may not decide.
        let held = self.hold(compared);
        // A weak equation joins two open levels, and what follows settles the classes that leaves.
        self.join_weak_equations(&held);
        // A join can have aliased a held level onto another, which is then held with it.
        let held = self.hold(held);
        // Follow each interface level to the metas that actually carry it. Conversion aliases one meta onto another whenever two spellings of a level are forced equal — keeping the member of greater provenance, but not by the signature's choice — so the set the caller computed from the declaration's type can name metas that were solved away, several links back from the ones still standing. Those representatives are then reached by `universe_metas_in(&body)` instead and land on the internal side, where `minimize` takes least solutions for them; the declaration comes back at a ground level rather than generalized over a level its own signature mentions, and every polymorphic caller is refused by the certifier for supplying a parameter where it demands a constant.
        let interface = self.representatives(interface);
        // An internal level is followed as an interface one is. A weak equation aliases a body's level onto one the body never spells, and that one is the level to minimize: left out, it is no level of the body's and none of the signature's until an occurrence of the signature settles at it as its lower bound, which makes it a parameter nothing reads.
        let internal = self
            .representatives(internal)
            .into_iter()
            .filter(|meta| !interface.contains(meta))
            .collect::<BTreeSet<_>>();
        let relevant = self.connected_metas(interface.iter().chain(&internal).copied());

        // The per-declaration magnitudes of the solver's work, recorded only when a profile is listening: the metas a declaration's interface names, the ones its body alone names, the connected component finalization must consider, and the constraint store it minimizes over. These are magnitudes rather than call counts, which is why the profile stream keeps them.
        curios_common::sample!("universe::finalize_interface_metas", interface.len());
        curios_common::sample!("universe::finalize_internal_metas", internal.len());
        curios_common::sample!("universe::finalize_connected_metas", relevant.len());
        curios_common::sample!("universe::finalize_store_len", self.constraints.len());
        curios_common::sample!("universe::finalize_held_metas", held.len());
        self.minimize(&internal, &relevant)?;
        // Minimizing solves body-only levels after the determined ones were merged, and a solution can determine another: `a ≤ b` with `b ≤ max(a, c)` becomes mutual once `c` is zero. So the merge is asked once more of what minimizing left.
        self.merge_forced_equalities(&relevant)?;
        // A merge can have joined a held level to another, which is then held with it.
        let held = self.hold(held);
        // An occurrence's level in the signature that nothing holds settles at its least value: `List(Nat)` in a domain sits at `Type 0` where its reduct does, rather than at a parameter no caller needs, and a level nothing bounds from below sits at zero, no position reading it. A chosen level identified with an occurrence stays with it, being the class's representative.
        let occurrences = interface
            .iter()
            .copied()
            .filter(|meta| {
                self.provenance(*meta) == Some(Provenance::Occurrence) && !held.contains(meta)
            })
            .collect::<BTreeSet<_>>();
        self.settle(&occurrences)?;
        // Last, a level the constraints reach but neither the type nor the body mentions — read after the occurrences settle, since settling one at a chosen level's bound makes that level the one the signature names; read before them, `Try`'s `Monad` witness would take its payload at zero while the lambda binder that chose it goes unmentioned. What is left is one nothing can observe — `/sys/List`'s body level, joined to its result by `v ≤ u` and nothing else — so it is settled rather than generalized, which is Rocq's restriction of a context to the universes its terms use.
        let interface = self.representatives(interface);
        let unmentioned = relevant
            .iter()
            .copied()
            .filter(|meta| !interface.contains(meta) && !internal.contains(meta))
            .collect::<BTreeSet<_>>();
        self.settle(&unmentioned)?;
        // Settling can leave two levels mutual, and a chosen level bounded by one chosen level alone, so the merge and the identification are asked of what it left.
        self.merge_forced_equalities(&relevant)?;
        self.identify_bounded_choices(&relevant)?;
        let metas = relevant
            .iter()
            .copied()
            .filter(|meta| self.solution(*meta).is_none())
            .collect::<BTreeSet<_>>();
        let (context, replacement) = self.generalize(metas)?;
        for (meta, level) in replacement {
            self.assign(meta, level)?;
        }
        self.check_consistent()?;
        self.discard_constraints(&relevant);
        Ok(context)
    }

    /// [`Self::pin_instance`], then every level of `minted` the pin left open minimized from its lower bounds: what a goal resolved after its consuming declaration finalized is closed by.
    ///
    /// The goal fixes only the levels its own application mentions. A witness scheme may carry more — the levels its *body* needs, which no goal could determine — so `minted` names the whole instance. Minimizing what is left is how [`Self::finalize`] treats a level reachable only through a body, applied here because the declaration that would have done it has already closed. A goal resolved while that declaration is open is pinned alone, and its finalization settles the rest with everything else the declaration said.
    pub fn close_instance(
        &mut self,
        minted: &[Level],
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), UniverseError> {
        self.pin_instance(instance, determined)?;
        // The declaration that would have joined them has closed, so what this resolution met is joined here, after the goal has fixed what it fixes.
        self.join_weak_equations(&BTreeSet::new());

        let open = minted
            .iter()
            .flat_map(Level::metas)
            .filter(|meta| self.solution(*meta).is_none())
            .collect::<BTreeSet<_>>();
        if !open.is_empty() {
            let scope = self.connected_metas(open.iter().copied());
            self.minimize(&open, &scope)?;
        }
        self.check_consistent()
    }

    /// Commit `instance` to the levels `determined` already fixes, position by position.
    ///
    /// A witness inhabits its goal and no other, so the levels its scheme introduces at a use site that the goal's application names carry no freedom — the goal fixes them. Conversion alone does not say so: it equates two applications' levels (`compare_levels`), but an equation it cannot turn into an alias — against a level already generalized, or a maximum — stays two constraints, and a level held only by constraints is left for declaration finalization.
    ///
    /// These must therefore be *solutions*, not constraints. A goal a parked constraint wakes after its consuming declaration finalized has no finalization left to turn a bound into a value, and the enclosing item's `clear_constraints` would discard a constraint unsolved.
    ///
    /// Positions whose instance level is already solved, or whose determining level is itself still open, are left alone: this pins what is knowable and never invents a solution.
    pub fn pin_instance(
        &mut self,
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), UniverseError> {
        if instance.len() != determined.len() {
            return Err(UniverseError::InstanceArity {
                expected: determined.len(),
                got: instance.len(),
            });
        }
        for (level, target) in instance.iter().zip(determined) {
            let level = self.zonk(level)?;
            let target = self.zonk(target)?;
            if target.metas().next().is_some() || level.constant_part() != 0 {
                continue;
            }
            let meta = {
                let mut atoms = level.atoms();
                match (atoms.next(), atoms.next()) {
                    (Some((LevelHead::Meta(meta), 0)), None) => meta,
                    _ => continue,
                }
            };
            self.assign(meta, target)?;
        }
        Ok(())
    }

    /// Close a non-reusable result (the module entrypoint) at its least universe solution instead of introducing a scheme, and a level the least leaves open at a model of what bounds it: a disjunction has no least solution ([`Self::close_stalled_components`]), which a declaration keeps as polymorphism and a result no one reuses has none of to keep, so any model serves it.
    pub fn default(
        &mut self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
    ) -> Result<(), UniverseError> {
        self.join_weak_equations(&BTreeSet::new());
        let relevant = self.connected_metas(metas);
        self.minimize(&relevant, &relevant)?;
        self.check_consistent()?;
        self.close_at_a_model(&relevant)?;
        self.discard_constraints(&relevant);
        Ok(())
    }

    /// Assign each of `metas` still open the level a model of the constraints mentioning it gives ([`model`]), each read with every solved level in place. A store the assignment leaves inconsistent is put back as it was.
    fn close_at_a_model(&mut self, metas: &BTreeSet<UniverseMetaId>) -> Result<(), UniverseError> {
        let open = metas
            .iter()
            .copied()
            .filter(|meta| self.solution(*meta).is_none())
            .collect::<BTreeSet<_>>();
        if open.is_empty() {
            return Ok(());
        }
        let positions = open
            .iter()
            .flat_map(|meta| self.constraints.mentioning(LevelHead::Meta(*meta)))
            .collect::<BTreeSet<_>>();
        let constraints = positions
            .into_iter()
            .map(|position| {
                let constraint = self
                    .constraints
                    .get(position)
                    .expect("the occurrence index names a live constraint");
                Ok(UniverseConstraint {
                    lower: self.zonk(&constraint.lower)?,
                    upper: self.zonk(&constraint.upper)?,
                })
            })
            .collect::<Result<Vec<_>, UniverseError>>()?;
        let Some(assignment) = model(constraints.iter()) else {
            return Ok(());
        };

        let mark = self.mark();
        let committed = open
            .iter()
            .try_for_each(|meta| {
                let value = assignment
                    .get(&LevelHead::Meta(*meta))
                    .copied()
                    .unwrap_or(0);
                self.assign(
                    *meta,
                    Level::constant(u32::try_from(value).unwrap_or(u32::MAX)),
                )
            })
            .is_ok()
            && self.consistent();
        if !committed {
            self.rollback(mark);
        }
        self.release(mark);
        Ok(())
    }

    /// Whether the store has a model in the naturals, as the shared least model decides it: what a speculative step asks before it keeps what it wrote, and the whole of what a check costs where nothing is refused.
    fn consistent(&self) -> bool {
        curios_common::profile!("universe::check_consistent");
        satisfiable(self.constraints.iter())
    }

    /// The store's consistency, or the conflict it holds, for a caller that reports one: a least set of its constraints with no model ([`Self::conflict`]).
    fn check_consistent(&self) -> Result<(), UniverseError> {
        if self.consistent() {
            return Ok(());
        }

        Err(self.inconsistency(&self.conflict()))
    }

    /// A least set of the store's constraints with no model, in the order they were raised: each is dropped in turn where the rest without it still has none, so every one kept is one the conflict needs.
    ///
    /// A satisfiability run a constraint, paid only where a refusal is reported; why not a loop traced through the model is `curios-analysis`'s README's.
    fn conflict(&self) -> Vec<usize> {
        curios_common::profile!("universe::conflict");
        let mut kept = (0..self.constraints.len()).collect::<Vec<_>>();
        let mut at = 0;
        while at < kept.len() {
            let without = kept
                .iter()
                .enumerate()
                .filter(|(index, _)| *index != at)
                .filter_map(|(_, position)| self.constraints.get(*position));
            if satisfiable(without) {
                at += 1;
            } else {
                kept.remove(at);
            }
        }

        kept
    }

    /// Word a conflict: the constraint of it raised last as what is required, and as where it was required, the origins of every constraint in it and of every level they name.
    fn inconsistency(&self, conflict: &[usize]) -> UniverseError {
        let constraint = |index: usize| {
            self.constraints
                .get(index)
                .expect("a conflict names constraints the store holds")
        };
        let mut origins = conflict
            .iter()
            .filter_map(|index| self.constraints.origin(*index).cloned())
            .collect::<Vec<_>>();
        for index in conflict.iter().copied() {
            for meta in constraint(index)
                .lower
                .metas()
                .chain(constraint(index).upper.metas())
            {
                if let Some(origin) = self.origin(meta)
                    && !origins.contains(origin)
                {
                    origins.push(origin.clone());
                }
            }
        }
        let witness = constraint(
            *conflict
                .last()
                .expect("a store with no model holds a constraint"),
        );

        UniverseError::Inconsistency {
            lower: witness.lower.clone(),
            upper: witness.upper.clone(),
            path: origins,
        }
    }
}

/// Validate that every constraint in `context` mentions only that context's own parameters and no metavariable, and that the set is satisfiable.
///
/// A context is always closed. Universe polymorphism belongs to declarations, so there is no enclosing scheme whose parameters a context could still reference.
///
/// A free function on the solver side rather than a method on [`UniverseContext`]: deciding satisfiability is a judgment, and it runs a solver. The context itself is data and knows nothing about how it is checked.
///
/// *Closure* is the other case, and it is [`UniverseContext::is_closed`], decided once on the data and called by both checkers: a second loop would be a second opinion worth nothing about a predicate too simple to have two implementations.
pub fn universe_context_validate(context: &UniverseContext) -> Result<(), UniverseError> {
    if !context.is_closed() {
        return Err(UniverseError::EscapingLevel);
    }

    let mut solver = UniverseSolver::new(0);
    // A stored scheme holds no origin: each constraint is raised again as its scheme's, at no place.
    for constraint in &context.constraints {
        solver.add_constraint(constraint.clone(), UniverseConstraintOrigin::Stated)?;
    }
    // Insertion records; it does not decide (see `add_constraint`). Satisfiability is this function's whole question, so the boundary it is decided at is here.
    solver.check_consistent()
}
