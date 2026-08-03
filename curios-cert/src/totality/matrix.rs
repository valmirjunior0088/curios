//! The size lattice, and the call matrices the size-change principle is stated over.
//!
//! One matrix per recursive call site grades every callee argument against every caller parameter. Composition is the (join, compose) semiring over [`Size`]: the best relation reachable through any intermediate position. The group is accepted when every **idempotent** matrix in the transitive closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Nothing here reads a term. What a call argument *is* is [`Shape`](super::Shape)'s question, and grading one against a parameter is `Shape::against`; this module only composes the answers.

use std::collections::HashSet;

/// The largest transitive closure a group's call matrices may reach.
///
/// Closure is worst-case exponential in the number of call sites. The prelude's largest group closes in tens of matrices; a group that blows past this is classified `Partial`, which is the conservative direction.
const CLOSURE_LIMIT: usize = 4096;

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct Matrix {
    pub(super) rows: usize,
    pub(super) columns: usize,
    pub(super) entries: Vec<Size>,
}

impl Matrix {
    /// The matrix that claims nothing — what an unanalyzable call contributes.
    pub(super) fn unknown(rows: usize, columns: usize) -> Self {
        Self {
            rows,
            columns,
            entries: vec![Size::Unknown; rows * columns],
        }
    }

    pub(super) fn entry(&self, row: usize, column: usize) -> Size {
        self.entries[row * self.columns + column]
    }

    pub(super) fn set(&mut self, row: usize, column: usize, size: Size) {
        self.entries[row * self.columns + column] = size;
    }

    /// `self` followed by `other`, in the (join, compose) semiring: the best relation reachable through any intermediate position.
    pub(super) fn compose(&self, other: &Matrix) -> Option<Matrix> {
        if self.columns != other.rows {
            return None;
        }
        let mut composed = Matrix::unknown(self.rows, other.columns);
        for row in 0..self.rows {
            for column in 0..other.columns {
                let mut best = Size::Unknown;
                for middle in 0..self.columns {
                    best = best.join(self.entry(row, middle).compose(other.entry(middle, column)));
                }
                composed.set(row, column, best);
            }
        }
        Some(composed)
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

/// Close the call matrices transitively, or `None` if the closure outgrows [`CLOSURE_LIMIT`].
///
/// The closure is what makes mutual recursion work without the analysis knowing which members were declared together: `raw_comm` calls `raw_swap_step` which calls back, and only the composite path is a cycle.
///
/// By generator extension: every product of call matrices is a shorter product followed by its last factor, so extending each discovered element by the *generators* alone reaches the whole closure — `|closure| × |calls|` compositions, not `|closure|²`, and not `|closure|²` per round as the original fixpoint paid. The distinction was measured, on the one group that makes it matter: `/std/BigNat/add/raw_assoc`'s 88 calls close to 1,599 matrices, at fifty seconds per round-based closure, twenty-two semi-naive over all pairs, and under a second this way. The set is hashed rather than ordered because its one consumer runs an order-independent `all`.
pub(super) fn close(calls: Vec<(usize, usize, Matrix)>) -> Option<Vec<(usize, usize, Matrix)>> {
    let mut closed: HashSet<(usize, usize, Matrix)> = HashSet::new();
    let mut frontier: Vec<(usize, usize, Matrix)> = Vec::new();
    let mut generators: Vec<(usize, usize, Matrix)> = Vec::new();
    for call in calls {
        if closed.insert(call.clone()) {
            frontier.push(call.clone());
            generators.push(call);
        }
    }

    while let Some((from, middle, first)) = frontier.pop() {
        let mut discovered = Vec::new();
        for (start, to, second) in &generators {
            if middle == *start
                && let Some(composed) = first.compose(second)
            {
                discovered.push((from, *to, composed));
            }
        }

        for candidate in discovered {
            if closed.insert(candidate.clone()) {
                if closed.len() > CLOSURE_LIMIT {
                    return None;
                }
                frontier.push(candidate);
            }
        }
    }

    Some(closed.into_iter().collect())
}
