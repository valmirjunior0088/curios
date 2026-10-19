//! The size lattice, and the call matrices the size-change principle is stated over.
//!
//! One matrix per recursive call site grades every callee argument against every caller parameter. Composition is the (join, compose) semiring over [`Size`]: the best relation reachable through any intermediate position. The group is accepted when every **idempotent** matrix in the transitive closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Nothing here reads a term. What a call argument *is* is [`Shape`](super::Shape)'s question, and grading one against a parameter is `Shape::against`; this module only composes the answers.

use std::collections::HashSet;

/// How far a group's transitive closure may outgrow one call for each ordered pair of its members.
///
/// Closure is worst-case exponential in the number of call sites. The prelude's largest group closes in tens of matrices; a group that blows past its bound is classified `Partial`, which is the conservative direction. The bound grows with the group ([`closure_limit`]), since members that all reach each other close to a call for every ordered pair of them whatever they descend on, and this is the room left for the matrices a pair may hold beyond its first.
pub(super) const CLOSURE_LIMIT: usize = 4096;

/// The largest closure a group of `members` may reach: a call for each ordered pair of them, and [`CLOSURE_LIMIT`] beside.
pub(super) fn closure_limit(members: usize) -> usize {
    CLOSURE_LIMIT.saturating_add(members.saturating_mul(members))
}

/// How a call argument's size compares to the caller parameter it is graded against.
///
/// Ordered `Unknown ⊏ Same ⊏ Less`, so joining several routes to the same entry keeps the most informative one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Size {
    /// No relation the analysis can establish.
    Unknown,
    /// The argument is the parameter's own expanded value.
    Same,
    /// The argument is a proper subterm of the parameter's expanded value.
    Less,
}

impl Size {
    /// Sequential composition: what one call followed by another establishes.
    ///
    /// `Unknown` annihilates — a link that says nothing breaks the chain. `Same` is the identity, and `Less` absorbs, because a chain containing one strict decrease is a strict decrease.
    pub(super) fn compose(self, other: Size) -> Size {
        match (self, other) {
            (Size::Unknown, _) | (_, Size::Unknown) => Size::Unknown,
            (Size::Less, _) | (_, Size::Less) => Size::Less,
            (Size::Same, Size::Same) => Size::Same,
        }
    }

    /// Least upper bound: two routes between the same pair of positions keep the stronger claim.
    pub(super) fn join(self, other: Size) -> Size {
        self.max(other)
    }
}

/// One call's size relation: `entry(row, column)` grades the callee's `column`th argument against the caller's `row`th parameter.
///
/// Held by row, each row the entries that claim something in column order, as Agda's termination checker holds its matrices: a call grades most arguments against no parameter, so a member handing its parameters on permuted has one entry a row, and composing, comparing and hashing cost the entries a matrix claims rather than its area. `Unknown` is never stored, so two matrices that grade alike are stored alike, and the derived equality, order and hash are the relation's.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Matrix {
    pub(super) rows: usize,
    pub(super) columns: usize,
    pub(super) entries: Vec<Vec<(usize, Size)>>,
}

impl Matrix {
    /// The matrix that claims nothing — what an unanalyzable call contributes.
    pub(super) fn unknown(rows: usize, columns: usize) -> Self {
        Self {
            rows,
            columns,
            entries: vec![Vec::new(); rows],
        }
    }

    pub(super) fn entry(&self, row: usize, column: usize) -> Size {
        let claimed = &self.entries[row];
        match claimed.binary_search_by_key(&column, |(at, _)| *at) {
            Ok(index) => claimed[index].1,
            Err(_) => Size::Unknown,
        }
    }

    pub(super) fn set(&mut self, row: usize, column: usize, size: Size) {
        let claimed = &mut self.entries[row];
        match (claimed.binary_search_by_key(&column, |(at, _)| *at), size) {
            (Ok(index), Size::Unknown) => {
                claimed.remove(index);
            }
            (Ok(index), size) => claimed[index].1 = size,
            (Err(_), Size::Unknown) => {}
            (Err(index), size) => claimed.insert(index, (column, size)),
        }
    }

    /// `self` followed by `other`, in the (join, compose) semiring: the best relation reachable through any intermediate position. Only entries both claim meet, since `Unknown` annihilates.
    pub(super) fn compose(&self, other: &Matrix) -> Option<Matrix> {
        if self.columns != other.rows {
            return None;
        }
        let entries = self
            .entries
            .iter()
            .map(|claimed| {
                let mut reached = claimed
                    .iter()
                    .flat_map(|(middle, first)| {
                        other.entries[*middle]
                            .iter()
                            .map(|(column, second)| (*column, first.compose(*second)))
                    })
                    .collect::<Vec<_>>();
                reached.sort_unstable_by_key(|(column, _)| *column);
                reached.dedup_by(|later, kept| {
                    let same = later.0 == kept.0;
                    if same {
                        kept.1 = kept.1.join(later.1);
                    }
                    same
                });
                reached
            })
            .collect();

        Some(Matrix {
            rows: self.rows,
            columns: other.columns,
            entries,
        })
    }

    /// Whether this matrix describes a call path that composes with itself unchanged — the paths that can repeat forever.
    pub(super) fn is_idempotent(&self) -> bool {
        self.rows == self.columns && self.compose(self).as_ref() == Some(self)
    }

    /// Whether some parameter strictly decreases along this path.
    pub(super) fn descends(&self) -> bool {
        (0..self.rows.min(self.columns)).any(|index| self.entry(index, index) == Size::Less)
    }
}

/// One graded call inside a group: from member `caller` to member `callee`, with the size relation its arguments bear to the caller's parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub caller: usize,
    pub callee: usize,
    pub matrix: Matrix,
}

/// Close the call matrices transitively, or `None` if the closure outgrows [`closure_limit`] for the members the calls name.
///
/// The closure is what makes mutual recursion work without the analysis knowing which members were declared together: `/std/Cli/check`'s `problems` calls `beneath` which calls back, and only the composite path is a cycle.
///
/// By generator extension: every product of call matrices is a shorter product followed by its last factor, so extending each discovered element by the *generators* alone reaches the whole closure — `|closure| × |calls|` compositions, not `|closure|²`. The difference matters for a group whose closure dwarfs its calls, as one member handing its parameters on permuted closes to every permutation of them. The set is hashed rather than ordered because its one consumer runs an order-independent `all`.
pub(super) fn close(calls: Vec<Call>) -> Option<Vec<Call>> {
    let members = calls
        .iter()
        .flat_map(|call| [call.caller, call.callee])
        .collect::<HashSet<_>>()
        .len();
    let limit = closure_limit(members);
    let mut closed: HashSet<Call> = HashSet::new();
    let mut frontier: Vec<Call> = Vec::new();
    let mut generators: Vec<Call> = Vec::new();
    for call in calls {
        if closed.insert(call.clone()) {
            frontier.push(call.clone());
            generators.push(call);
        }
    }

    while let Some(first) = frontier.pop() {
        let mut discovered = Vec::new();
        for second in &generators {
            if first.callee == second.caller
                && let Some(matrix) = first.matrix.compose(&second.matrix)
            {
                discovered.push(Call {
                    caller: first.caller,
                    callee: second.callee,
                    matrix,
                });
            }
        }

        for candidate in discovered {
            if closed.insert(candidate.clone()) {
                if closed.len() > limit {
                    return None;
                }
                frontier.push(candidate);
            }
        }
    }

    Some(closed.into_iter().collect())
}
