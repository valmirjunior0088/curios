//! The size lattice, and the call matrices the size-change principle is stated over.
//!
//! Representation, not analysis. A matrix grades what one call or one application hands against what its caller was handed, and composition is the (join, compose) semiring over [`Size`]: the best relation reachable through any intermediate position. Nothing here reads a term: what an argument *is* is `curios-analysis`'s `Shape`'s question, grading it against a parameter `Shape::against`'s, and closing a group's calls to a verdict `decide`'s. A matrix belongs here because a certifier's record stores the ones a definition's applications are graded by, as a registry entry's [`Polarity`](crate::Polarity) vector is stored beside it.

/// How a call argument's size compares to the caller parameter it is graded against.
///
/// Ordered `Unknown ⊏ Same ⊏ Less`, so joining several routes to the same entry keeps the most informative one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub enum Size {
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
    pub fn compose(self, other: Size) -> Size {
        match (self, other) {
            (Size::Unknown, _) | (_, Size::Unknown) => Size::Unknown,
            (Size::Less, _) | (_, Size::Less) => Size::Less,
            (Size::Same, Size::Same) => Size::Same,
        }
    }

    /// Least upper bound: two routes between the same pair of positions keep the stronger claim.
    pub fn join(self, other: Size) -> Size {
        self.max(other)
    }
}

/// One call's size relation: `entry(row, column)` grades the callee's `column`th argument against the caller's `row`th parameter.
///
/// Held by row, each row the entries that claim something in column order, as Agda's termination checker holds its matrices: a call grades most arguments against no parameter, so a member handing its parameters on to a sibling claims one entry a row where a dense matrix would hold the square of its arity.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct CallMatrix {
    rows: usize,
    columns: usize,
    entries: Vec<Vec<(usize, Size)>>,
}

impl CallMatrix {
    /// The matrix that claims nothing — what an unanalyzable call contributes.
    pub fn unknown(rows: usize, columns: usize) -> Self {
        Self {
            rows,
            columns,
            entries: vec![Vec::new(); rows],
        }
    }

    /// How many parameters of the caller it grades against.
    pub fn rows(&self) -> usize {
        self.rows
    }

    /// How many arguments of the call it grades.
    pub fn columns(&self) -> usize {
        self.columns
    }

    pub fn entry(&self, row: usize, column: usize) -> Size {
        let claimed = &self.entries[row];
        match claimed.binary_search_by_key(&column, |(at, _)| *at) {
            Ok(index) => claimed[index].1,
            Err(_) => Size::Unknown,
        }
    }

    pub fn set(&mut self, row: usize, column: usize, size: Size) {
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
    pub fn compose(&self, other: &CallMatrix) -> Option<CallMatrix> {
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

        Some(CallMatrix {
            rows: self.rows,
            columns: other.columns,
            entries,
        })
    }

    /// Whether this matrix describes a call path that composes with itself unchanged — the paths that can repeat forever.
    pub fn is_idempotent(&self) -> bool {
        self.rows == self.columns && self.compose(self).as_ref() == Some(self)
    }

    /// Whether some parameter strictly decreases along this path.
    pub fn descends(&self) -> bool {
        (0..self.rows.min(self.columns)).any(|index| self.entry(index, index) == Size::Less)
    }
}
