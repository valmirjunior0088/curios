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
    /// The argument is no larger than the parameter's expanded value: that value itself, or an arithmetic result that never grows past it.
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

    /// `self` over `rows` parameters where it grades fewer: its own rows, and the rows after them claiming nothing. What was graded against a part's first parameters, read against every one of them.
    pub fn over(mut self, rows: usize) -> CallMatrix {
        if self.rows < rows {
            self.entries.resize(rows, Vec::new());
            self.rows = rows;
        }
        self
    }

    /// `self`'s columns followed by `other`'s, over the same rows: a call whose first arguments `self` grades and whose later ones `other` does — a member's own arguments, then a field lambda's. `None` where the rows differ.
    pub fn beside(&self, other: &CallMatrix) -> Option<CallMatrix> {
        if self.rows != other.rows {
            return None;
        }
        let entries = self
            .entries
            .iter()
            .zip(&other.entries)
            .map(|(first, second)| {
                first
                    .iter()
                    .copied()
                    .chain(
                        second
                            .iter()
                            .map(|(column, size)| (column + self.columns, *size)),
                    )
                    .collect()
            })
            .collect();

        Some(CallMatrix {
            rows: self.rows,
            columns: self.columns + other.columns,
            entries,
        })
    }
}

/// How a definition applies its parameters, one summary for each of its parts ([`PartApplications`]): its leading lambdas, at the empty path, and each field lambda of the record they answer, at its path.
///
/// What a certifier's record stores of a definition beside its totality, so a call a callback makes is graded where the definition handed it applies it: `/std/List/any` applies `pred` to an element below `l`, `/std/List/eql` applies its witness's method, `e.0(x, y)`, to elements below its lists, and `List`'s witness's method, its field at `[0]`, applies its `use` parameter's as `List/eql` does.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Applications {
    parts: Vec<PartApplications>,
}

/// How one part of a definition applies each of its parameters — the definition's, then a field lambda's: for each, every matrix grading what an application of it hands against the part's parameters, and every matrix grading what an application of a field of it hands, or nothing where the parameter is used otherwise than applied, a field of it applied, or handed on unchanged.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct PartApplications {
    path: Vec<usize>,
    parameters: Vec<Option<Vec<CallMatrix>>>,
    fields: Vec<FieldApplications>,
}

/// The applications of one field of a parameter, reached by projecting `path` out of the parameter at `position`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct FieldApplications {
    pub position: usize,
    pub path: Vec<usize>,
    pub applied: Vec<CallMatrix>,
}

impl Applications {
    /// The summaries of a definition's parts.
    pub fn new(parts: Vec<PartApplications>) -> Self {
        Self { parts }
    }

    /// The summary of the part at `path`: the leading lambdas' at the empty path.
    pub fn part(&self, path: &[usize]) -> Option<&PartApplications> {
        self.parts.iter().find(|part| part.path == path)
    }

    /// Whether no part applies anything where every use of it is seen: what a record files nothing for.
    pub fn is_empty(&self) -> bool {
        self.parts.iter().all(PartApplications::is_empty)
    }
}

impl PartApplications {
    /// The applications by the part at `path` of each of its parameters in order, `None` for one used otherwise, and of each field path of one, where the parameter is not used otherwise.
    pub fn new(
        path: Vec<usize>,
        parameters: Vec<Option<Vec<CallMatrix>>>,
        fields: Vec<FieldApplications>,
    ) -> Self {
        Self {
            path,
            parameters,
            fields,
        }
    }

    /// How many parameters the part has.
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

    /// Every matrix grading an application of parameter `index`, where every use of it is one; `None` where it is used otherwise, and for a parameter the definition has not.
    pub fn of(&self, index: usize) -> Option<&[CallMatrix]> {
        self.parameters.get(index)?.as_deref()
    }

    /// Each field path of parameter `index` the definition applies, with every matrix grading an application of it: none where the parameter is used otherwise.
    pub fn fields_of(&self, index: usize) -> impl Iterator<Item = (&[usize], &[CallMatrix])> {
        let seen = self.of(index).is_some();
        self.fields
            .iter()
            .filter(move |field| seen && field.position == index)
            .map(|field| (field.path.as_slice(), field.applied.as_slice()))
    }

    /// Whether no parameter, and no field of one, is applied where every use of it is seen: what a record files nothing for.
    pub fn is_empty(&self) -> bool {
        self.parameters
            .iter()
            .all(|applied| applied.as_ref().is_none_or(Vec::is_empty))
            && self.fields.iter().all(|field| field.applied.is_empty())
    }
}
