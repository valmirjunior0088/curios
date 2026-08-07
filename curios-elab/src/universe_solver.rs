//! The transactional universe constraint solver.
//!
//! Elaboration machinery, not representation: it owns the live inequality store, the difference graph that decides consistency, and the marks that roll a speculative branch back. The levels, contexts, and schemes it solves over live in `universe`, which knows nothing about any of this.

mod constraints;
use constraints::{ConstraintStore, StoreMark};

#[cfg(test)]
mod tests;

use {
    curios_core::{
        Level, LevelHead, UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseError, UniverseMetaId, UniverseParam, UniverseRole, UniverseSeed,
    },
    std::collections::{BTreeMap, BTreeSet, VecDeque},
};

/// A stable point to which all universe assignments and constraints can be rolled back after a speculative elaboration branch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UniverseMark {
    constraints: StoreMark,
    solution_log_len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct UniverseStateToken {
    next_meta: usize,
    constraints: StoreMark,
    solution_log_len: usize,
}
#[derive(Debug, Clone)]
struct UniverseMeta {
    role: UniverseRole,
    origin: Option<UniverseConstraintOrigin>,
    solution: Option<Level>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DifferenceNode {
    Zero,
    Head(LevelHead),
}

/// The difference constraint `to - from ≤ weight`.
#[derive(Debug, Clone)]
struct DifferenceEdge {
    from: DifferenceNode,
    to: DifferenceNode,
    weight: i64,
    origin: Option<usize>,
}

#[derive(Debug, Clone)]
struct DifferenceGraph {
    nodes: Vec<DifferenceNode>,
    positions: BTreeMap<DifferenceNode, usize>,
    edges: Vec<DifferenceEdge>,
    outgoing: Vec<Vec<usize>>,
    distance: Vec<i128>,
    predecessor: Vec<Option<usize>>,
}

impl DifferenceGraph {
    fn new() -> Self {
        Self {
            nodes: vec![DifferenceNode::Zero],
            positions: BTreeMap::from([(DifferenceNode::Zero, 0)]),
            edges: Vec::new(),
            outgoing: vec![Vec::new()],
            distance: vec![0],
            predecessor: vec![None],
        }
    }

    fn ensure_node(&mut self, node: DifferenceNode) {
        if self.positions.contains_key(&node) {
            return;
        }
        let index = self.nodes.len();
        self.nodes.push(node);
        self.positions.insert(node, index);
        self.outgoing.push(Vec::new());
        self.distance.push(0);
        self.predecessor.push(None);
        if node != DifferenceNode::Zero {
            self.push_edge(DifferenceEdge {
                from: node,
                to: DifferenceNode::Zero,
                weight: 0,
                origin: None,
            });
        }
    }

    fn push_edge(&mut self, edge: DifferenceEdge) {
        let from = self.positions[&edge.from];
        let index = self.edges.len();
        self.edges.push(edge);
        self.outgoing[from].push(index);
    }

    fn extend(
        &mut self,
        nodes: impl IntoIterator<Item = DifferenceNode>,
        edges: impl IntoIterator<Item = DifferenceEdge>,
    ) -> Result<(), Vec<usize>> {
        for node in nodes {
            self.ensure_node(node);
        }
        let first_new_edge = self.edges.len();
        for edge in edges {
            self.ensure_node(edge.from);
            self.ensure_node(edge.to);
            self.push_edge(edge);
        }
        self.relax_from(first_new_edge)
    }

    fn relax_from(&mut self, first_new_edge: usize) -> Result<(), Vec<usize>> {
        let mut queue = VecDeque::new();
        let mut queued = vec![false; self.nodes.len()];
        for edge_index in first_new_edge..self.edges.len() {
            if let Some(to) = self.relax_edge(edge_index)? {
                queued[to] = true;
                queue.push_back(to);
            }
        }
        while let Some(from) = queue.pop_front() {
            queued[from] = false;
            let outgoing = self.outgoing[from].clone();
            for edge_index in outgoing {
                if let Some(to) = self.relax_edge(edge_index)?
                    && !queued[to]
                {
                    queued[to] = true;
                    queue.push_back(to);
                }
            }
        }
        Ok(())
    }

    fn relax_edge(&mut self, edge_index: usize) -> Result<Option<usize>, Vec<usize>> {
        let edge = &self.edges[edge_index];
        let from = self.positions[&edge.from];
        let to = self.positions[&edge.to];
        let candidate = self.distance[from] + i128::from(edge.weight);
        if self.distance[to] <= candidate {
            return Ok(None);
        }

        // Predecessors form a forest while the graph is feasible. Making an ancestor depend on its descendant closes a cycle; the strict distance improvement proves that cycle has negative total weight.
        let mut cursor = from;
        let mut path = vec![edge_index];
        loop {
            if cursor == to {
                let mut origins = path
                    .into_iter()
                    .filter_map(|index| self.edges[index].origin)
                    .collect::<Vec<_>>();
                origins.reverse();
                origins.dedup();
                return Err(origins);
            }
            let Some(predecessor) = self.predecessor[cursor] else {
                break;
            };
            path.push(predecessor);
            cursor = self.positions[&self.edges[predecessor].from];
        }

        self.distance[to] = candidate;
        self.predecessor[to] = Some(edge_index);
        Ok(Some(to))
    }
}

#[derive(Debug, Clone)]
struct ConsistencyCache {
    constraint_len: usize,
    graph: DifferenceGraph,
}

fn atomic_difference_edge(
    lower: (Option<LevelHead>, u32),
    upper: (Option<LevelHead>, u32),
    origin: usize,
) -> Result<Option<DifferenceEdge>, ()> {
    match (lower, upper) {
        ((None, lower), (None, upper)) => {
            if lower <= upper {
                Ok(None)
            } else {
                Err(())
            }
        }
        ((Some(lower), lower_offset), (None, upper)) => {
            if lower_offset > upper {
                Err(())
            } else {
                Ok(Some(DifferenceEdge {
                    from: DifferenceNode::Zero,
                    to: DifferenceNode::Head(lower),
                    weight: i64::from(upper) - i64::from(lower_offset),
                    origin: Some(origin),
                }))
            }
        }
        ((None, lower), (Some(upper), upper_offset)) => {
            Ok((lower > upper_offset).then_some(DifferenceEdge {
                from: DifferenceNode::Head(upper),
                to: DifferenceNode::Zero,
                weight: i64::from(upper_offset) - i64::from(lower),
                origin: Some(origin),
            }))
        }
        ((Some(lower), lower_offset), (Some(upper), upper_offset)) => {
            if lower == upper {
                if lower_offset <= upper_offset {
                    Ok(None)
                } else {
                    Err(())
                }
            } else {
                Ok(Some(DifferenceEdge {
                    from: DifferenceNode::Head(upper),
                    to: DifferenceNode::Head(lower),
                    weight: i64::from(upper_offset) - i64::from(lower_offset),
                    origin: Some(origin),
                }))
            }
        }
    }
}

/// The difference-graph fragment one constraint contributes: the nodes it names and the edges relating them.
type DifferenceFragment = (BTreeSet<DifferenceNode>, Vec<DifferenceEdge>);

/// Encode a constraint whose right-hand maxima have one forced viable choice. `Ok(None)` means that the exact disjunctive solver is required.
fn forced_difference_edges(
    constraint: &UniverseConstraint,
    index: usize,
) -> Result<Option<DifferenceFragment>, Vec<usize>> {
    let nodes = BTreeSet::from_iter(
        constraint
            .lower
            .atoms()
            .chain(constraint.upper.atoms())
            .map(|(head, _)| DifferenceNode::Head(head))
            .chain([DifferenceNode::Zero]),
    );
    let mut lower_parts = constraint
        .lower
        .atoms()
        .map(|(head, offset)| (Some(head), offset))
        .collect::<Vec<_>>();
    if constraint.lower.constant != 0 {
        lower_parts.push((None, constraint.lower.constant));
    }
    let mut upper_parts = constraint
        .upper
        .atoms()
        .map(|(head, offset)| (Some(head), offset))
        .collect::<Vec<_>>();
    if upper_parts.is_empty() || constraint.upper.constant != 0 {
        upper_parts.push((None, constraint.upper.constant));
    }

    let mut edges = Vec::new();
    for lower in lower_parts {
        let mut choices = upper_parts
            .iter()
            .copied()
            .filter_map(|upper| atomic_difference_edge(lower, upper, index).ok())
            .collect::<Vec<_>>();
        choices.sort_by_key(|edge| {
            edge.as_ref()
                .map_or((DifferenceNode::Zero, DifferenceNode::Zero, 0), |edge| {
                    (edge.from, edge.to, edge.weight)
                })
        });
        choices.dedup_by(|left, right| match (left, right) {
            (None, None) => true,
            (Some(left), Some(right)) => {
                left.from == right.from && left.to == right.to && left.weight == right.weight
            }
            _ => false,
        });
        match choices.as_slice() {
            [] => return Err(vec![index]),
            [None] => {}
            [Some(edge)] => edges.push(edge.clone()),
            _ => return Ok(None),
        }
    }
    Ok(Some((nodes, edges)))
}

/// The mutable algebraic-universe inference state.
#[derive(Debug, Clone)]
pub struct UniverseSolver {
    metas: Vec<UniverseMeta>,
    constraints: ConstraintStore,
    solution_log: Vec<UniverseMetaId>,
    next_meta: usize,
    consistency: Option<ConsistencyCache>,
}

impl UniverseSolver {
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
        self.consistency = None;
    }

    pub fn new(meta_floor: usize) -> Self {
        Self {
            metas: (0..meta_floor)
                .map(|_| UniverseMeta {
                    role: UniverseRole::Generalizable,
                    origin: None,
                    solution: None,
                })
                .collect(),
            constraints: ConstraintStore::default(),
            solution_log: Vec::new(),
            next_meta: meta_floor,
            consistency: None,
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
                role: seed.role,
                origin: seed.origin.clone(),
                solution: None,
            })
            .collect();
        self.next_meta = seeds.len();
        self.consistency = None;
    }

    pub fn fresh(
        &mut self,
        role: UniverseRole,
        origin: Option<UniverseConstraintOrigin>,
    ) -> UniverseMetaId {
        let id = UniverseMetaId(self.next_meta);
        self.next_meta += 1;
        self.metas.push(UniverseMeta {
            role,
            origin,
            solution: None,
        });
        id
    }

    pub fn role(&self, meta: UniverseMetaId) -> Option<UniverseRole> {
        self.metas.get(meta.0).map(|entry| entry.role)
    }

    pub fn origin(&self, meta: UniverseMetaId) -> Option<&UniverseConstraintOrigin> {
        self.metas
            .get(meta.0)
            .and_then(|entry| entry.origin.as_ref())
    }

    pub fn mark(&self) -> UniverseMark {
        UniverseMark {
            constraints: self.constraints.mark(),
            solution_log_len: self.solution_log.len(),
        }
    }

    pub(crate) fn state_token(&self) -> UniverseStateToken {
        UniverseStateToken {
            next_meta: self.next_meta,
            constraints: self.constraints.mark(),
            solution_log_len: self.solution_log.len(),
        }
    }

    /// Restore both stores to `mark`. Solutions are unwound first: the constraint journal's pre-images were taken *before* the assignments that rewrote them, so the two unwind in the same direction.
    pub fn rollback(&mut self, mark: UniverseMark) {
        while self.solution_log.len() > mark.solution_log_len {
            let meta = self.solution_log.pop().unwrap();
            self.metas[meta.0].solution = None;
        }
        self.constraints.rollback(mark.constraints);
        self.consistency = None;
    }

    pub fn constraints(&self) -> &[UniverseConstraint] {
        self.constraints.as_slice()
    }

    /// Release inference constraints after their enclosing declaration has finalized. Any relation that remains externally meaningful has already been projected into that declaration's [`UniverseContext`]; later uses reinsert the stored residual context at fresh instances.
    pub(crate) fn clear_constraints(&mut self) {
        self.constraints.clear();
        self.consistency = None;
    }

    pub fn solution(&self, meta: UniverseMetaId) -> Option<&Level> {
        self.metas
            .get(meta.0)
            .and_then(|entry| entry.solution.as_ref())
    }

    pub fn zonk(&self, level: &Level) -> Result<Level, UniverseError> {
        fn go(
            solver: &UniverseSolver,
            level: &Level,
            visiting: &mut BTreeSet<UniverseMetaId>,
        ) -> Result<Level, UniverseError> {
            level.substitute(|head| match head {
                LevelHead::Param(_) => None,
                LevelHead::Meta(meta) => {
                    let solution = solver.solution(meta)?.clone();
                    if !visiting.insert(meta) {
                        return None;
                    }
                    let zonked = go(solver, &solution, visiting).ok();
                    visiting.remove(&meta);
                    zonked
                }
            })
        }
        go(self, level, &mut BTreeSet::new())
    }

    pub fn add_leq(
        &mut self,
        lower: Level,
        upper: Level,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        self.add_constraint(UniverseConstraint {
            lower,
            upper,
            origin,
        })
    }

    pub fn add_eq(
        &mut self,
        left: Level,
        right: Level,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        let mark = self.mark();
        if let Err(error) = self.default_shape_equal(&left, &right) {
            self.rollback(mark);
            return Err(error);
        }
        if let Err(error) = self.add_leq(left.clone(), right.clone(), origin.clone()) {
            self.rollback(mark);
            return Err(error);
        }
        if let Err(error) = self.add_leq(right, left, origin) {
            self.rollback(mark);
            return Err(error);
        }
        Ok(())
    }

    /// Default an equation whose zonked sides differ at exactly one meta atom with a shared offset: `max(s, α+k) = max(s, β+k)` pins one meta to the other instead of parking two inequalities the bound propagators cannot decompose. Without this, independently instantiated spellings of one written annotation meet only here, both metas survive to declaration finalization, and the scheme generalizes two parameters where the program wrote one universe.
    ///
    /// The direction solves a flexible meta toward a generalizable one when the roles differ, so an occurrence instance keeps its identity; a same-role pair takes a fixed arbitrary direction. The commitment is deliberately incomplete — `max(1, α) = max(1, β)` also admits solutions with the metas apart below the shared constant — so a program that genuinely needs distinct shape-equal instances refuses where it previously over-generalized.
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
        let role =
            |solver: &Self, meta: UniverseMetaId| solver.metas.get(meta.0).map(|entry| entry.role);
        let (Some(role_a), Some(_)) = (role(self, a), role(self, b)) else {
            return Ok(());
        };
        let (from, to) = match role_a {
            UniverseRole::Flexible => (a, b),
            UniverseRole::Generalizable => (b, a),
        };
        self.assign(from, Level::meta(to))
    }

    /// Record an inequality. Consistency is *not* decided here.
    ///
    /// It used to be: every insertion pushed, ran a full consistency check, and popped on refusal. That check is an incremental cycle detection over the difference graph — one relaxation pass per constraint — and it measured at 67 of the fixed prelude's 200 seconds of elaboration, across seventy-five thousand insertions at roughly a millisecond each. Nothing was buying that. The rendered diagnostic is `lower ≤ upper` plus a step count, both read off the *graph* by `inconsistency_from_path`, so a check taken later names the same cycle; the declaring item comes from `Error::in_declaration` at the item boundary, not from the insertion site; and every caller outside this module propagates the refusal with `?` rather than recovering from it, so no decision depended on learning it early.
    ///
    /// What still decides consistency is what always used a *verdict* rather than a diagnostic: the speculative commit in `close_stalled_components`, and the declaration-boundary checks in `finalize`, `finalize_at_instance`, and `solve_flexible`. An inconsistent set can therefore exist between an insertion and the next of those, which is the price — the boundary refuses the declaration either way, and `curios-cert` validates the universes it archives independently.
    pub fn add_constraint(
        &mut self,
        mut constraint: UniverseConstraint,
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
                    path: vec![constraint.origin],
                })
            };
        }
        self.constraints.push(constraint);
        Ok(())
    }

    /// Minimize flexible metas from their current lower bounds. Repeating to a fixpoint handles classifier chains; unconstrained flexible metas become zero. Generalizable metas are left for declaration finalization.
    pub fn solve_flexible(&mut self) -> Result<(), UniverseError> {
        let metas = (0..self.metas.len())
            .map(UniverseMetaId)
            .collect::<BTreeSet<_>>();
        self.solve_flexible_in(&metas)
    }

    /// Whether `meta` is unsolved and eligible for minimization.
    fn is_open_flexible(&self, meta: UniverseMetaId) -> bool {
        self.metas
            .get(meta.0)
            .is_some_and(|entry| entry.role == UniverseRole::Flexible && entry.solution.is_none())
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

    /// The least level satisfying the *currently known* part of every `lower ≤ meta + k` bound, reading each still-open flexible level in a lower position as its least value, zero.
    ///
    /// [`Self::principal_lower_bound`] refuses to answer while any lower bound mentions an open level, because the answer could still grow. That is the right rule while propagation can still make progress, but a cycle of mutual bounds never does: every level in it is waiting for another. Weakening the open atoms to zero yields an *implied* bound — a level is monotone in its atoms, so the result is a sound floor rather than a guess — which is enough to break the tie and resume propagation.
    fn grounded_lower_bound(&self, meta: UniverseMetaId) -> Result<Option<Level>, UniverseError> {
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
                    LevelHead::Meta(other) if self.is_open_flexible(other) => Some(Level::zero()),
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

    /// Whether any constraint genuinely bounds `meta` from above. A level with no such bound has the unconditional least solution zero.
    fn is_upper_bounded(&self, meta: UniverseMetaId) -> bool {
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
    /// Solving is worklist-driven: a level is revisited only when one of the levels its bounds mention has just been solved. Rescanning every constraint for every level after every assignment is what made this quadratic in the size of a declaration's universe closure.
    fn solve_flexible_in(&mut self, metas: &BTreeSet<UniverseMetaId>) -> Result<(), UniverseError> {
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

            // A flexible level that occurs only in lower positions is zero. Default the lowest such id, then resume propagation: for `v + 1 ≤ u`, defaulting `v` first derives `u = 1`. Taking these before the stalled levels below is what keeps that derivation available, since a level nothing bounds from above can never be the one holding a cycle together.
            if let Some(meta) = metas
                .iter()
                .copied()
                .find(|meta| self.is_open_flexible(*meta) && !self.is_upper_bounded(*meta))
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

    /// Break a stalled set by closing each of its components at the grounded floor, keeping only the closures that survive a consistency check. Returns the levels whose bounds mentioned a committed one.
    ///
    /// A stall has two shapes, and only one of them may be closed. A *cycle* of mutual bounds — `max(1, ?u) ≤ max(1, ?v)` with its converse, which is what witness dispatch emits — has a least solution, and the floor assignment witnesses it. A *disjunction* like `1 ≤ max(?u, ?v)` has none: either level may carry the bound, so choosing one is arbitrary and would silently strip a declaration of polymorphism it is entitled to keep. Attempting the floor and rolling back on inconsistency distinguishes them without either shape having to be recognized syntactically: the disjunction's floor reduces to `1 ≤ 0` and fails, while the cycle's reduces to `1 ≤ 1` and holds.
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

            // Every floor is read from the same pre-assignment state, so the component closes simultaneously rather than each member seeing the levels committed before it.
            let floors = component
                .iter()
                .map(|meta| {
                    let floor = self
                        .grounded_lower_bound(*meta)?
                        .unwrap_or_else(Level::zero);
                    Ok((*meta, floor))
                })
                .collect::<Result<Vec<_>, UniverseError>>()?;
            let dependents = component
                .iter()
                .flat_map(|meta| self.dependents_of(*meta))
                .collect::<BTreeSet<_>>();

            let mark = self.mark();
            let committed = floors
                .into_iter()
                .try_for_each(|(meta, floor)| self.assign(meta, floor))
                .and_then(|()| self.check_consistent())
                .is_ok();
            if committed {
                woken.extend(dependents);
            } else {
                self.rollback(mark);
            }
        }
        Ok(woken)
    }

    /// Collapse exact bidirectional atom inequalities before allocating generalized parameters. This handles the equality classes generated by conversion without pretending that arbitrary max equalities are syntactically unifiable. Each round collects *every* forced equality rather than the first. Merging can expose new ones — `a ≤ c` and `c ≤ b` become mutual once `a` and `b` coincide — so rounds still repeat to a fixpoint, but a round costs one pass over the store instead of one pass per merge.
    fn merge_forced_equalities(
        &mut self,
        metas: &BTreeSet<UniverseMetaId>,
    ) -> Result<(), UniverseError> {
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
                            let (replace, retain) = if left > right {
                                (left, right)
                            } else {
                                (right, left)
                            };
                            Some((replace, Level::meta(retain)))
                        }
                        _ => None,
                    };
                    if let Some((meta, level)) = assignment
                        && replaced.insert(meta)
                    {
                        assignments.push((meta, level));
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
        let entry = self
            .metas
            .get_mut(meta.0)
            .ok_or(UniverseError::UnknownMeta(meta))?;
        if entry.solution.is_some() {
            return Ok(());
        }
        entry.solution = Some(level);
        self.solution_log.push(meta);
        self.consistency = None;

        let head = LevelHead::Meta(meta);
        let solution = self.zonk(&Level::meta(meta))?;
        self.constraints.substitute(head, |level| {
            level.substitute(|found| (found == head).then(|| solution.clone()))
        })?;
        Ok(())
    }

    /// Instantiate a closed context, returning its fresh argument vector after inserting the substituted residual constraints transactionally.
    pub fn instantiate(
        &mut self,
        context: &UniverseContext,
        role: UniverseRole,
    ) -> Result<Vec<Level>, UniverseError> {
        let levels = (0..context.parameter_count)
            .map(|_| Level::meta(self.fresh(role, None)))
            .collect::<Vec<_>>();
        self.instantiate_at(context, &levels)?;
        Ok(levels)
    }

    /// Insert a closed context's residual constraints at an already chosen occurrence instance. Used when re-elaborating an explicit `UniverseInst`: its stored arguments are authoritative and must not be replaced by a second fresh instantiation.
    ///
    /// A stored context is validated where it is built ([`Self::generalize`]) and where it is restored (`validate_universes`), not here: every occurrence of every polymorphic binding instantiates, and re-deciding a context's consistency per occurrence repeats a whole constraint solve for an invariant that cannot have changed.
    pub fn instantiate_at(
        &mut self,
        context: &UniverseContext,
        levels: &[Level],
    ) -> Result<(), UniverseError> {
        if levels.len() != context.parameter_count {
            return Err(UniverseError::InstanceArity {
                expected: context.parameter_count,
                got: levels.len(),
            });
        }
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
            let instantiated = UniverseConstraint {
                lower: substitute(&constraint.lower)?,
                upper: substitute(&constraint.upper)?,
                origin: UniverseConstraintOrigin {
                    kind: UniverseConstraintKind::SchemeInstantiation,
                    ..constraint.origin.clone()
                },
            };
            if let Err(error) = self.add_constraint(instantiated) {
                self.rollback(mark);
                return Err(error);
            }
        }
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
                    origin: constraint.origin.clone(),
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
            constraints,
        };
        universe_context_validate(&context)?;
        Ok((context, replacement))
    }

    /// Solve `metas` as if each were an inferred output, restoring their declared roles afterwards. Solving ranges over `scope` so a minimized level can still be determined by its relations to levels outside the requested set.
    ///
    /// Minimization is a property of the *position* a level occupies rather than of the level itself: one written `Type` is an input where a use site can choose it and an ordinary classifier where it cannot.
    fn minimize(
        &mut self,
        metas: &BTreeSet<UniverseMetaId>,
        scope: &BTreeSet<UniverseMetaId>,
    ) -> Result<(), UniverseError> {
        let roles = metas
            .iter()
            .map(|meta| {
                let entry = self
                    .metas
                    .get_mut(meta.0)
                    .ok_or(UniverseError::UnknownMeta(*meta))?;
                let role = entry.role;
                entry.role = UniverseRole::Flexible;
                Ok((*meta, role))
            })
            .collect::<Result<Vec<_>, UniverseError>>()?;
        let solved = self.solve_flexible_in(scope);
        for (meta, role) in roles {
            self.metas[meta.0].role = role;
        }
        solved
    }

    /// Minimize inference-only levels and bind every surviving input meta as a deterministic declaration parameter.
    ///
    /// `interface` is the declaration's externally visible universe surface — its type and the registry signatures a use site instantiates. `internal` levels occur only in the body, so no occurrence could ever choose them; they are minimized instead of becoming parameters a caller cannot supply. An internal level with no principal solution is still generalized, because the residual context must stay closed.
    pub fn finalize(
        &mut self,
        interface: impl IntoIterator<Item = UniverseMetaId>,
        internal: impl IntoIterator<Item = UniverseMetaId>,
    ) -> Result<UniverseContext, UniverseError> {
        let interface = interface.into_iter().collect::<BTreeSet<_>>();
        let internal = internal
            .into_iter()
            .filter(|meta| !interface.contains(meta))
            .collect::<BTreeSet<_>>();
        let relevant = self.connected_metas(interface.iter().chain(&internal).copied());
        self.minimize(&internal, &relevant)?;
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

    /// Issue a declaration into an *inherited* context, binding `instance` to that context's parameters by position.
    ///
    /// A concept method wrapper is not independently polymorphic. It is issued in its concept's context, and the levels its `use w : C(…)` binder carries are that context's parameters in declaration order, so solving position `i` to `Param(i)` is the identity substitution. Every level the instance does not name directly is related to one that it does by the constraints unification already recorded, and minimizing from those lower bounds settles it at the parameter that forced it.
    ///
    /// This is deliberately not [`Self::finalize`]: finalization mints parameters for the metas it finds in ascending meta-id order, and a wrapper's own binder metas are minted before its instance metas, so that order need not agree with the concept's.
    pub fn finalize_at_instance(
        &mut self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
        instance: &[Level],
        parameter_count: usize,
    ) -> Result<(), UniverseError> {
        if instance.len() != parameter_count {
            return Err(UniverseError::InstanceArity {
                expected: parameter_count,
                got: instance.len(),
            });
        }
        let relevant = self.connected_metas(metas);
        for (index, level) in instance.iter().enumerate() {
            let parameter = Level::param(UniverseParam(index));
            let level = self.zonk(level)?;
            if level == parameter {
                continue;
            }
            let mut atoms = level.atoms();
            match (level.constant_part(), atoms.next(), atoms.next()) {
                (0, Some((LevelHead::Meta(meta), 0)), None) => self.assign(meta, parameter)?,
                // An argument that is neither the parameter itself nor an open meta was already forced elsewhere, and nothing in the declaration can denote the concept's parameter in its place.
                _ => return Err(UniverseError::EscapingLevel),
            }
        }
        let open = self.relevant_open_metas(&relevant).collect::<BTreeSet<_>>();
        self.minimize(&open, &relevant)?;
        if self.relevant_open_metas(&relevant).next().is_some() {
            return Err(UniverseError::EscapingLevel);
        }
        self.check_consistent()?;
        self.discard_constraints(&relevant);
        Ok(())
    }

    /// Commit `instance` to the levels `determined` already fixes, position by position.
    ///
    /// A witness inhabits its goal and no other, so the levels its scheme introduces at a use site carry no freedom — the goal fixes them. Conversion alone does not say so: cumulativity makes a concept application's universe arguments a bound rather than an equation, and a bounded-but-unsolved level is left for declaration finalization.
    ///
    /// These must therefore be *solutions*, not constraints. A goal that deferred resolves after its consuming declaration finalized, so no finalization remains to turn a bound into a value, and the enclosing item's `clear_constraints` would discard a constraint unsolved.
    ///
    /// Positions whose instance level is already solved, or whose determining level is itself still open, are left alone: this pins what is knowable and never invents a solution.
    ///
    /// The goal fixes only the levels its own application mentions. A witness scheme may carry more — `satisfy Monad(Async)` generalizes levels its *body* needs, which no goal could determine — so `minted` names the whole instance, and whatever the goal leaves open is minimized from its lower bounds. That is precisely how [`Self::finalize`] treats a level reachable only through a body, applied here because the declaration that would have done it has already closed.
    pub fn close_instance(
        &mut self,
        minted: &[Level],
        instance: &[Level],
        determined: &[Level],
    ) -> Result<(), UniverseError> {
        self.pin_instance(instance, determined)?;

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

    fn pin_instance(
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

    fn relevant_open_metas<'a>(
        &'a self,
        relevant: &'a BTreeSet<UniverseMetaId>,
    ) -> impl Iterator<Item = UniverseMetaId> + 'a {
        relevant
            .iter()
            .copied()
            .filter(|meta| self.solution(*meta).is_none())
    }

    /// Close a non-reusable result (the module entrypoint) at its least universe solution instead of introducing a scheme.
    pub fn default(
        &mut self,
        metas: impl IntoIterator<Item = UniverseMetaId>,
    ) -> Result<(), UniverseError> {
        let relevant = self.connected_metas(metas);
        self.minimize(&relevant, &relevant)?;
        self.check_consistent()?;
        self.discard_constraints(&relevant);
        Ok(())
    }

    /// Explain an inconsistency from the constraint indices that closed the cycle, widening the path with the origins of the levels it names.
    fn inconsistency_from_path(&self, path: Vec<usize>) -> UniverseError {
        let constraints = self.constraints.as_slice();
        let witness = path.first().copied().unwrap_or(0);
        let mut origins = path
            .iter()
            .map(|index| constraints[*index].origin.clone())
            .collect::<Vec<_>>();
        for index in path.iter().copied().chain([witness]) {
            for meta in constraints[index]
                .lower
                .metas()
                .chain(constraints[index].upper.metas())
            {
                if let Some(origin) = self.origin(meta)
                    && !origins.contains(origin)
                {
                    origins.push(origin.clone());
                }
            }
        }
        UniverseError::Inconsistency {
            lower: constraints[witness].lower.clone(),
            upper: constraints[witness].upper.clone(),
            path: origins,
        }
    }

    fn check_consistent(&mut self) -> Result<(), UniverseError> {
        if let Some(mut cache) = self.consistency.take() {
            if cache.constraint_len == self.constraints.len() {
                self.consistency = Some(cache);
                return Ok(());
            }
            if cache.constraint_len + 1 == self.constraints.len() {
                let index = cache.constraint_len;
                match forced_difference_edges(
                    self.constraints
                        .get(index)
                        .expect("the cache trails the store by one"),
                    index,
                ) {
                    Ok(Some((nodes, edges))) => match cache.graph.extend(nodes, edges) {
                        Ok(()) => {
                            cache.constraint_len += 1;
                            self.consistency = Some(cache);
                            return Ok(());
                        }
                        Err(path) => return Err(self.inconsistency_from_path(path)),
                    },
                    Err(path) => return Err(self.inconsistency_from_path(path)),
                    Ok(None) => {}
                }
            }
        }

        let constraints = self.constraints.as_slice();
        let mut nodes = BTreeSet::new();
        let mut edges = Vec::new();
        let mut forced = true;
        for (index, constraint) in constraints.iter().enumerate() {
            match forced_difference_edges(constraint, index) {
                Ok(Some((new_nodes, new_edges))) => {
                    nodes.extend(new_nodes);
                    edges.extend(new_edges);
                }
                Ok(None) => {
                    forced = false;
                    break;
                }
                Err(path) => {
                    return Err(self.inconsistency_from_path(path));
                }
            }
        }
        if forced {
            let mut graph = DifferenceGraph::new();
            if let Err(path) = graph.extend(nodes, edges) {
                return Err(self.inconsistency_from_path(path));
            }
            self.consistency = Some(ConsistencyCache {
                constraint_len: constraints.len(),
                graph,
            });
            return Ok(());
        }

        self.check_consistent_full()
    }

    /// Decide consistency from scratch, branching on genuine right-hand maxima. Reads the store directly: every stored constraint is already normalized, so there is no separate zonked copy to drift from.
    fn check_consistent_full(&self) -> Result<(), UniverseError> {
        let constraints = self.constraints.as_slice();
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        enum Node {
            Zero,
            Head(LevelHead),
        }

        /// The difference constraint `to - from ≤ weight`.
        #[derive(Debug, Clone)]
        struct Edge {
            from: Node,
            to: Node,
            weight: i64,
            origin: Option<usize>,
        }

        fn atomic_edge(
            lower: (Option<LevelHead>, u32),
            upper: (Option<LevelHead>, u32),
            origin: usize,
        ) -> Result<Option<Edge>, ()> {
            match (lower, upper) {
                ((None, lower), (None, upper)) => {
                    if lower <= upper {
                        Ok(None)
                    } else {
                        Err(())
                    }
                }
                ((Some(lower), lower_offset), (None, upper)) => {
                    if lower_offset > upper {
                        Err(())
                    } else {
                        Ok(Some(Edge {
                            from: Node::Zero,
                            to: Node::Head(lower),
                            weight: i64::from(upper) - i64::from(lower_offset),
                            origin: Some(origin),
                        }))
                    }
                }
                ((None, lower), (Some(upper), upper_offset)) => Ok((lower > upper_offset)
                    .then_some(Edge {
                        from: Node::Head(upper),
                        to: Node::Zero,
                        weight: i64::from(upper_offset) - i64::from(lower),
                        origin: Some(origin),
                    })),
                ((Some(lower), lower_offset), (Some(upper), upper_offset)) => {
                    if lower == upper {
                        if lower_offset <= upper_offset {
                            Ok(None)
                        } else {
                            Err(())
                        }
                    } else {
                        Ok(Some(Edge {
                            from: Node::Head(upper),
                            to: Node::Head(lower),
                            weight: i64::from(upper_offset) - i64::from(lower_offset),
                            origin: Some(origin),
                        }))
                    }
                }
            }
        }

        /// One difference constraint over *indexed* nodes.
        ///
        /// The search resolves node identity once, up front. Keeping `Node` keys here instead made every relaxation step a `BTreeMap` lookup, inside the innermost loop of an exponential search.
        #[derive(Debug, Clone, Copy)]
        struct Arc {
            from: usize,
            to: usize,
            weight: i64,
            origin: Option<usize>,
        }

        /// A feasible potential over the committed arcs, maintained across the branch search.
        ///
        /// The invariant is that `distance` satisfies every arc currently in `arcs`. Committing one more arc restores it by relaxing outward from that arc alone, and backtracking replays the entries it changed. Re-deriving the whole potential per search node instead — a full Bellman-Ford over every arc — is what made this search dominate elaboration.
        struct Search {
            arcs: Vec<Arc>,
            outgoing: Vec<Vec<usize>>,
            distance: Vec<i128>,
            predecessor: Vec<Option<usize>>,
        }

        /// What one committed arc changed, in the order it changed it.
        struct Undo {
            arcs: usize,
            from: usize,
            touched: Vec<(usize, i128, Option<usize>)>,
        }

        impl Search {
            fn new(node_count: usize) -> Self {
                Self {
                    arcs: Vec::new(),
                    outgoing: vec![Vec::new(); node_count],
                    // Every node has an implicit zero-weight edge from a super-source, so the all-zero potential is feasible for the empty arc set.
                    distance: vec![0; node_count],
                    predecessor: vec![None; node_count],
                }
            }

            /// Tighten `to` along one arc. `Ok(None)` means the potential already satisfied it; `Err` means committing it closes a negative cycle, reported as the origins around that cycle.
            fn relax(
                &mut self,
                arc: usize,
                touched: &mut Vec<(usize, i128, Option<usize>)>,
            ) -> Result<Option<usize>, Vec<usize>> {
                let Arc {
                    from, to, weight, ..
                } = self.arcs[arc];
                let candidate = self.distance[from] + i128::from(weight);
                if self.distance[to] <= candidate {
                    return Ok(None);
                }

                // Predecessors form a forest while the potential is feasible. Making an ancestor depend on its descendant closes a cycle, and the strict improvement proves its weight is negative.
                let mut cursor = from;
                let mut path = vec![arc];
                loop {
                    if cursor == to {
                        let mut origins = path
                            .into_iter()
                            .filter_map(|arc| self.arcs[arc].origin)
                            .collect::<Vec<_>>();
                        origins.reverse();
                        origins.dedup();
                        return Err(origins);
                    }
                    let Some(predecessor) = self.predecessor[cursor] else {
                        break;
                    };
                    path.push(predecessor);
                    cursor = self.arcs[predecessor].from;
                }

                touched.push((to, self.distance[to], self.predecessor[to]));
                self.distance[to] = candidate;
                self.predecessor[to] = Some(arc);
                Ok(Some(to))
            }

            fn propagate(
                &mut self,
                seed: usize,
                touched: &mut Vec<(usize, i128, Option<usize>)>,
            ) -> Result<(), Vec<usize>> {
                let mut queue = VecDeque::from([seed]);
                let mut queued = vec![false; self.distance.len()];
                queued[seed] = true;
                while let Some(node) = queue.pop_front() {
                    queued[node] = false;
                    for index in 0..self.outgoing[node].len() {
                        let arc = self.outgoing[node][index];
                        if let Some(to) = self.relax(arc, touched)?
                            && !queued[to]
                        {
                            queued[to] = true;
                            queue.push_back(to);
                        }
                    }
                }
                Ok(())
            }

            /// Commit one arc, restoring feasibility. On failure the search is left exactly as it was, so a refuted branch costs nothing.
            fn commit(&mut self, arc: Arc) -> Result<Undo, Vec<usize>> {
                let index = self.arcs.len();
                self.arcs.push(arc);
                self.outgoing[arc.from].push(index);
                let mut undo = Undo {
                    arcs: index,
                    from: arc.from,
                    touched: Vec::new(),
                };
                let relaxed = self
                    .relax(index, &mut undo.touched)
                    .and_then(|seed| match seed {
                        Some(seed) => self.propagate(seed, &mut undo.touched),
                        None => Ok(()),
                    });
                match relaxed {
                    Ok(()) => Ok(undo),
                    Err(path) => {
                        self.revert(undo);
                        Err(path)
                    }
                }
            }

            fn revert(&mut self, undo: Undo) {
                for (node, distance, predecessor) in undo.touched.into_iter().rev() {
                    self.distance[node] = distance;
                    self.predecessor[node] = predecessor;
                }
                self.outgoing[undo.from].pop();
                self.arcs.truncate(undo.arcs);
            }
        }

        /// One suspended clause of the search: which alternative to try next, the arc committed for the alternative currently being explored (to revert on the way back), and the longest refutation any alternative has produced so far.
        struct Frame {
            clause: usize,
            next: usize,
            undo: Option<Undo>,
            best_failure: Option<Vec<usize>>,
        }

        /// Where the driver is in the walk: descending into a clause, carrying a finished clause's verdict back to its parent, or picking a suspended clause's next alternative.
        enum Step {
            Descend(usize),
            Resume(Result<(), Option<Vec<usize>>>),
            Advance,
        }

        /// `budget` bounds the nodes this search may visit. Exhausting it is reported as `Err(None)`, distinct from a refuted branch, so the caller can name the clause shape instead of spinning.
        ///
        /// Reaching the last clause needs no final check: feasibility is the search's invariant, so an assignment that committed is a model.
        ///
        /// The walk is driven by an explicit stack rather than the call stack. Depth here is the number of *branching* clauses, which the budget does not bound — it counts visits — so a constraint set wide enough to need many decisions overflowed the native stack before the budget ever noticed. `/std/Async/block_on` reached four hundred branches. The order of exploration and the decision reached are unchanged.
        fn choose(
            clauses: &[(usize, Vec<Option<Arc>>)],
            start: usize,
            search: &mut Search,
            budget: &mut u64,
        ) -> Result<(), Option<Vec<usize>>> {
            let mut stack = Vec::<Frame>::new();
            let mut step = Step::Descend(start);

            loop {
                step = match step {
                    Step::Descend(clause) => match budget.checked_sub(1) {
                        None => Step::Resume(Err(None)),
                        Some(remaining) => {
                            *budget = remaining;
                            match clause == clauses.len() {
                                true => Step::Resume(Ok(())),
                                false => {
                                    stack.push(Frame {
                                        clause,
                                        next: 0,
                                        undo: None,
                                        best_failure: None,
                                    });
                                    Step::Advance
                                }
                            }
                        }
                    },
                    // A clause finished. Its parent owns the arc committed to reach it, and reverts before reading the verdict — the order the recursive form's `revert`-then-`match` had.
                    Step::Resume(result) => {
                        let Some(frame) = stack.last_mut() else {
                            return result;
                        };
                        if let Some(undo) = frame.undo.take() {
                            search.revert(undo);
                        }
                        match result {
                            Ok(()) => {
                                stack.pop();
                                Step::Resume(Ok(()))
                            }
                            Err(None) => {
                                stack.pop();
                                Step::Resume(Err(None))
                            }
                            Err(Some(path)) => {
                                record_failure(&mut frame.best_failure, path);
                                Step::Advance
                            }
                        }
                    }
                    Step::Advance => {
                        let frame = stack.last_mut().expect("a frame is suspended");
                        let clause = frame.clause;
                        let choices = &clauses[clause].1;
                        if frame.next == choices.len() {
                            let failed = frame
                                .best_failure
                                .take()
                                .unwrap_or_else(|| vec![clauses[clause].0]);
                            stack.pop();
                            Step::Resume(Err(Some(failed)))
                        } else {
                            let choice = choices[frame.next];
                            frame.next += 1;
                            match choice {
                                // A clause alternative that needs no arc is already satisfied by the committed potential.
                                None => {
                                    frame.undo = None;
                                    Step::Descend(clause + 1)
                                }
                                Some(arc) => match search.commit(arc) {
                                    Ok(undo) => {
                                        frame.undo = Some(undo);
                                        Step::Descend(clause + 1)
                                    }
                                    Err(path) => {
                                        record_failure(&mut frame.best_failure, path);
                                        Step::Advance
                                    }
                                },
                            }
                        }
                    }
                };
            }
        }

        /// Keep the longest refutation seen for a clause: the deeper path names more of what forced the contradiction, which is what the caller renders.
        fn record_failure(best: &mut Option<Vec<usize>>, path: Vec<usize>) {
            if best.as_ref().is_none_or(|kept| path.len() > kept.len()) {
                *best = Some(path);
            }
        }

        // A maximum on the left is conjunctive. A maximum on the right is a finite disjunction: under any satisfying valuation, each left atom is dominated by at least one right atom. Enumerating those symbolic choices and checking their difference graphs decides consistency without searching numeric universe assignments.
        let mut nodes = BTreeSet::from([Node::Zero]);
        for constraint in constraints {
            nodes.extend(
                constraint
                    .lower
                    .atoms()
                    .chain(constraint.upper.atoms())
                    .map(|(head, _)| Node::Head(head)),
            );
        }
        let positions = nodes
            .iter()
            .enumerate()
            .map(|(index, node)| (*node, index))
            .collect::<BTreeMap<_, _>>();
        let zero = positions[&Node::Zero];
        let arc = |edge: Edge| Arc {
            from: positions[&edge.from],
            to: positions[&edge.to],
            weight: edge.weight,
            origin: edge.origin,
        };

        let mut clauses = Vec::<(usize, Vec<Option<Arc>>)>::new();
        for (index, constraint) in constraints.iter().enumerate() {
            let mut lower_parts = constraint
                .lower
                .atoms()
                .map(|(head, offset)| (Some(head), offset))
                .collect::<Vec<_>>();
            if constraint.lower.constant != 0 {
                lower_parts.push((None, constraint.lower.constant));
            }
            let mut upper_parts = constraint
                .upper
                .atoms()
                .map(|(head, offset)| (Some(head), offset))
                .collect::<Vec<_>>();
            // Canonical normalization leaves a zero constant beside atoms only when every atom dominates it for every natural valuation. Keeping that redundant branch would multiply the consistency search by two for nearly every ordinary max constraint.
            if upper_parts.is_empty() || constraint.upper.constant != 0 {
                upper_parts.push((None, constraint.upper.constant));
            }

            for lower in lower_parts {
                let mut choices = upper_parts
                    .iter()
                    .copied()
                    .filter_map(|upper| atomic_edge(lower, upper, index).ok())
                    .map(|edge| edge.map(&arc))
                    .collect::<Vec<_>>();
                choices.sort_by_key(|choice| {
                    choice.map_or((0, 0, 0), |arc| (arc.from, arc.to, arc.weight))
                });
                choices.dedup_by(|left, right| match (left, right) {
                    (None, None) => true,
                    (Some(left), Some(right)) => {
                        left.from == right.from
                            && left.to == right.to
                            && left.weight == right.weight
                    }
                    _ => false,
                });
                clauses.push((index, choices));
            }
        }

        let mut search = Search::new(nodes.len());
        // Universe heads range over naturals: `0 - head ≤ 0`.
        let grounded = (0..nodes.len())
            .filter(|index| *index != zero)
            .map(|index| Arc {
                from: index,
                to: zero,
                weight: 0,
                origin: None,
            })
            // Ordinary difference constraints have exactly one symbolic RHS choice; they hold under every branch assignment, so they belong to the base potential rather than the search.
            .chain(
                clauses
                    .iter()
                    .filter_map(|(_, choices)| match choices.as_slice() {
                        [choice] => *choice,
                        _ => None,
                    }),
            )
            .collect::<Vec<_>>();
        for arc in grounded {
            if let Err(path) = search.commit(arc) {
                return Err(self.inconsistency_from_path(path));
            }
        }

        // Narrowest clause first. A clause whose alternatives are all refuted by the committed arcs fails at the shallowest possible depth, so ordering by width prunes the tree before it is built rather than after. The decision is unchanged — only the order in which the same finite set of assignments is explored.
        let mut branches = clauses
            .into_iter()
            .filter(|(_, choices)| choices.len() != 1)
            .collect::<Vec<_>>();
        branches.sort_by_key(|(_, choices)| choices.len());

        // Bounding the search is what makes this decision procedure total.
        //
        // Accepting is always justified: a satisfying branch assignment is a model of the original constraints, so any `Ok` here is sound however few branches were explored. Only *refuting* needs the whole tree, so exhausting the budget means "not decided", and the caller reports it rather than continuing an exponential walk.
        const SEARCH_BUDGET: u64 = 200_000;
        let mut budget = SEARCH_BUDGET;
        let consistency = choose(&branches, 0, &mut search, &mut budget);

        consistency.map_err(|path| match path {
            Some(path) => self.inconsistency_from_path(path),
            None => UniverseError::SearchExhausted {
                constraints: constraints.len(),
                branches: branches.len(),
                widths: branches
                    .iter()
                    .map(|(_, choices)| choices.len())
                    .take(16)
                    .collect(),
            },
        })
    }
}

/// Validate that every constraint in `context` mentions only that context's own parameters and no metavariable, and that the set is satisfiable.
///
/// A context is always closed. Universe polymorphism belongs to declarations, so there is no enclosing scheme whose parameters a context could still reference.
///
/// A free function on the solver side rather than a method on [`UniverseContext`]: deciding satisfiability is a judgment, and it runs a solver. The context itself is data and knows nothing about how it is checked.
///
/// *Closure* is the other case, and it used to be spelled out here as well as in `curios-cert` — two identical loops, which is a second opinion worth nothing about a predicate too simple to have two implementations. It is [`UniverseContext::is_closed`] now, decided once on the data, which is the line this function's own note draws and did not apply to both halves.
pub(crate) fn universe_context_validate(context: &UniverseContext) -> Result<(), UniverseError> {
    if !context.is_closed() {
        return Err(UniverseError::EscapingLevel);
    }

    let mut solver = UniverseSolver::new(0);
    for constraint in &context.constraints {
        solver.add_constraint(constraint.clone())?;
    }
    // Insertion records; it does not decide (see `add_constraint`). Satisfiability is this function's whole question, so the boundary it is decided at is here.
    solver.check_consistent()
}
