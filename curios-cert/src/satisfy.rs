//! Whether a declaration's universe constraints can be satisfied at all.
//!
//! The kernel *assumes* an item's [`UniverseContext`] while checking it — that is what lets a correct polymorphic definition through, since its recorded constraints are exactly the hypotheses its level questions need. An unsatisfiable set is therefore not a harmless oddity but a hypothesis set from which everything follows: [`entails`](crate::entails) starts proving whatever it is asked, `check_instance` stops discharging anything, and the universe discipline that keeps the paradox out stops applying.
//!
//! `curios-elab` decides the same question in its solver, and this is deliberately a *second* implementation rather than a copy. A transcription would inherit whatever the original gets wrong and agree for that reason, which is the failure the shared analyses already demonstrate; written from the constraint semantics instead, the two can disagree, and a disagreement is a signal.
//!
//! # The decision
//!
//! A constraint is `max(lower parts) ≤ max(upper parts)`, where a part is a constant or a parameter at an offset. Left maxima decompose exactly — every lower part must hold — so each constraint contributes one *clause* per lower part, and a clause is the disjunction over the upper parts of "this lower part is bounded by that one". Comparing two parts yields one of three things: the relation holds outright, it is impossible, or it holds exactly when a difference `to - from ≤ weight` does.
//!
//! Satisfying the whole set means choosing one alternative from every clause such that the chosen differences have a solution, which they do exactly when they close no negative cycle. The search commits one alternative at a time against a feasible potential and backtracks when a commitment closes such a cycle; the potential it carries is a model of everything committed so far, so reaching the last clause needs no separate check.
//!
//! Right-hand maxima are what make this a search rather than a walk: `max(1, P0) ≤ max(1, P1)` bounds `P0` by either `P1` or the constant, and nothing local says which. A set whose clauses are all single-alternative is decided without branching at all.

#[cfg(test)]
mod tests;

use curios_core::{Level, LevelHead, UniverseConstraint};

/// Search nodes this decision will visit before giving up.
///
/// Exhausting it *refuses*, which is this crate's standing direction: a context whose satisfiability could not be established is one the kernel declines to assume. Real contexts are the residue of generalization — a handful of constraints over a handful of parameters — so reaching this bound means a shape nothing in the language produces.
const BUDGET: usize = 100_000;

/// Whether some assignment of naturals to parameters satisfies every constraint.
pub fn satisfiable(constraints: &[UniverseConstraint]) -> bool {
    let mut nodes = vec![Node::Zero];
    for constraint in constraints {
        for (head, _) in constraint.lower.atoms().chain(constraint.upper.atoms()) {
            if !nodes.contains(&Node::Head(head)) {
                nodes.push(Node::Head(head));
            }
        }
    }

    let mut clauses = Vec::new();
    for constraint in constraints {
        for lower in parts(&constraint.lower) {
            let mut alternatives = Vec::new();
            for upper in parts(&constraint.upper) {
                match bound(&nodes, lower, upper) {
                    // Nothing can make this clause true, so nothing can make the set true.
                    Bound::Impossible => continue,
                    Bound::Holds => alternatives.push(None),
                    Bound::Difference(arc) => alternatives.push(Some(arc)),
                }
            }
            if alternatives.is_empty() {
                return false;
            }
            clauses.push(alternatives);
        }
    }

    let mut search = Search::new(nodes.len());
    let mut budget = BUDGET;

    choose(&clauses, 0, &mut search, &mut budget)
}

/// One side of a constraint as the parts a maximum is taken over: each atom, and the constant when it contributes.
fn parts(level: &Level) -> Vec<(Option<LevelHead>, u32)> {
    let mut parts = level
        .atoms()
        .map(|(head, offset)| (Some(head), offset))
        .collect::<Vec<_>>();
    if level.constant_part() != 0 || parts.is_empty() {
        parts.push((None, level.constant_part()));
    }

    parts
}

/// What it takes for one part to be bounded by another.
enum Bound {
    Holds,
    Impossible,
    Difference(Arc),
}

/// `to - from ≤ weight`, over node positions.
#[derive(Debug, Clone, Copy)]
struct Arc {
    from: usize,
    to: usize,
    weight: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Node {
    Zero,
    Head(LevelHead),
}

fn position(nodes: &[Node], node: Node) -> usize {
    nodes
        .iter()
        .position(|candidate| *candidate == node)
        .expect("every head was collected before the clauses were built")
}

/// Whether `lower` is bounded by `upper`, and at what cost.
///
/// A parameter ranges over the naturals, so the zero node stands for the constant part: bounding an atom by a constant is a relation to zero, and being bounded *by* an atom is free whenever that atom's offset already covers the constant.
fn bound(
    nodes: &[Node],
    lower: (Option<LevelHead>, u32),
    upper: (Option<LevelHead>, u32),
) -> Bound {
    let zero = position(nodes, Node::Zero);

    match (lower, upper) {
        ((None, lower), (None, upper)) => match lower <= upper {
            true => Bound::Holds,
            false => Bound::Impossible,
        },
        ((Some(lower_head), lower_offset), (None, upper)) => match lower_offset > upper {
            true => Bound::Impossible,
            false => Bound::Difference(Arc {
                from: zero,
                to: position(nodes, Node::Head(lower_head)),
                weight: i64::from(upper) - i64::from(lower_offset),
            }),
        },
        ((None, lower), (Some(upper_head), upper_offset)) => match lower <= upper_offset {
            true => Bound::Holds,
            false => Bound::Difference(Arc {
                from: position(nodes, Node::Head(upper_head)),
                to: zero,
                weight: i64::from(upper_offset) - i64::from(lower),
            }),
        },
        ((Some(lower_head), lower_offset), (Some(upper_head), upper_offset)) => {
            if lower_head == upper_head {
                return match lower_offset <= upper_offset {
                    true => Bound::Holds,
                    false => Bound::Impossible,
                };
            }

            Bound::Difference(Arc {
                from: position(nodes, Node::Head(upper_head)),
                to: position(nodes, Node::Head(lower_head)),
                weight: i64::from(upper_offset) - i64::from(lower_offset),
            })
        }
    }
}

/// A potential that satisfies every arc committed so far.
///
/// Kept across the search rather than rebuilt per node: committing one arc restores the invariant by relaxing outward from it alone, and backtracking replays exactly what that relaxation changed.
struct Search {
    arcs: Vec<Arc>,
    outgoing: Vec<Vec<usize>>,
    distance: Vec<i128>,
    predecessor: Vec<Option<usize>>,
}

/// What one commitment changed, in the order it changed it.
struct Undo {
    arcs: usize,
    from: usize,
    touched: Vec<(usize, i128, Option<usize>)>,
}

impl Search {
    fn new(nodes: usize) -> Self {
        Self {
            arcs: Vec::new(),
            outgoing: vec![Vec::new(); nodes],
            // The all-zero potential satisfies the empty arc set.
            distance: vec![0; nodes],
            predecessor: vec![None; nodes],
        }
    }

    /// Tighten this arc's target. `None` means the potential already satisfied it; `Err` means committing it closes a negative cycle.
    fn relax(
        &mut self,
        arc: usize,
        touched: &mut Vec<(usize, i128, Option<usize>)>,
    ) -> Result<Option<usize>, ()> {
        let Arc { from, to, weight } = self.arcs[arc];
        let candidate = self.distance[from] + i128::from(weight);
        if self.distance[to] <= candidate {
            return Ok(None);
        }

        // Predecessors form a forest while the potential is feasible, so making an ancestor depend on its descendant closes a cycle — and the strict improvement proves that cycle negative.
        let mut cursor = from;
        loop {
            if cursor == to {
                return Err(());
            }
            let Some(predecessor) = self.predecessor[cursor] else {
                break;
            };
            cursor = self.arcs[predecessor].from;
        }

        touched.push((to, self.distance[to], self.predecessor[to]));
        self.distance[to] = candidate;
        self.predecessor[to] = Some(arc);

        Ok(Some(to))
    }

    /// Commit one arc, restoring feasibility by relaxing outward from it.
    fn commit(&mut self, arc: Arc) -> Result<Undo, Undo> {
        let mut undo = Undo {
            arcs: self.arcs.len(),
            from: self.outgoing[arc.from].len(),
            touched: Vec::new(),
        };

        let index = self.arcs.len();
        self.arcs.push(arc);
        self.outgoing[arc.from].push(index);

        let mut pending = match self.relax(index, &mut undo.touched) {
            Ok(None) => Vec::new(),
            Ok(Some(node)) => vec![node],
            Err(()) => return Err(undo),
        };

        while let Some(node) = pending.pop() {
            for outgoing in self.outgoing[node].clone() {
                match self.relax(outgoing, &mut undo.touched) {
                    Ok(None) => {}
                    Ok(Some(next)) => pending.push(next),
                    Err(()) => return Err(undo),
                }
            }
        }

        Ok(undo)
    }

    /// Put back exactly what a commitment changed.
    fn rollback(&mut self, undo: Undo) {
        for (node, distance, predecessor) in undo.touched.into_iter().rev() {
            self.distance[node] = distance;
            self.predecessor[node] = predecessor;
        }
        if let Some(arc) = self.arcs.get(undo.arcs) {
            self.outgoing[arc.from].truncate(undo.from);
        }
        self.arcs.truncate(undo.arcs);
    }
}

/// Choose an alternative for every clause from `clause` onward, backtracking over the ones that close a cycle.
fn choose(
    clauses: &[Vec<Option<Arc>>],
    clause: usize,
    search: &mut Search,
    budget: &mut usize,
) -> bool {
    if *budget == 0 {
        return false;
    }
    *budget -= 1;

    // Feasibility is the invariant, so an assignment that committed is a model.
    if clause == clauses.len() {
        return true;
    }

    for alternative in &clauses[clause] {
        let Some(arc) = alternative else {
            // Already satisfied by the committed potential, with nothing to undo.
            if choose(clauses, clause + 1, search, budget) {
                return true;
            }
            continue;
        };

        match search.commit(*arc) {
            Ok(undo) => {
                if choose(clauses, clause + 1, search, budget) {
                    return true;
                }
                search.rollback(undo);
            }
            Err(undo) => search.rollback(undo),
        }
    }

    false
}
