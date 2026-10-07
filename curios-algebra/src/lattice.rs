//! `min` and `max` as the meet and the join of a total order: how each stands to its operands, and which of its operands answer for the rest.
//!
//! A meet is at most each operand and at least whatever is at most all of them; a join is the mirror. Those four facts are all a comparison reads of one, and each is put here over the verdicts the comparison reached of the operands, so it is stated once for `Nat` and for `Int` and tested over closed values.
//!
//! **An operand another answers for is dropped.** `min(a, b)` is `a` where `a <= b`, so a leaf some other leaf is at most adds nothing to a meet, and [`Lattice::keeps`] says which of two leaves a verdict leaves standing. Dropping is by the comparison's verdicts alone, which is why the order they decide has to be transitive: a leaf dropped for one that is dropped in turn must have been at least the one that stays.

use crate::{Comparison, Operation};

/// A lattice operation of a total order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Lattice {
    /// `min`: the greatest of what is at most every operand.
    Meet,
    /// `max`: the least of what is at least every operand.
    Join,
}

/// What a verdict on two leaves of one lattice operation leaves standing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kept {
    /// The first answers for the second.
    First,
    /// The second answers for the first.
    Second,
    /// Neither is known to answer for the other.
    Both,
}

impl Operation {
    /// The lattice operation this is: `min` the meet and `max` the join. `None` for any other operation.
    pub fn lattice(self) -> Option<Lattice> {
        match self {
            Operation::Min => Some(Lattice::Meet),
            Operation::Max => Some(Lattice::Join),
            _ => None,
        }
    }
}

impl Lattice {
    /// Which of two leaves stands, given the verdict of the first against the second: a meet keeps a leaf the other is at least, a join one the other is at most, and two equal leaves are the first.
    pub fn keeps(self, verdict: Comparison) -> Kept {
        match (self, verdict) {
            (_, Comparison::Eq) => Kept::First,
            (Lattice::Meet, Comparison::Lt | Comparison::Le)
            | (Lattice::Join, Comparison::Gt | Comparison::Ge) => Kept::First,
            (Lattice::Meet, Comparison::Gt | Comparison::Ge)
            | (Lattice::Join, Comparison::Lt | Comparison::Le) => Kept::Second,
            _ => Kept::Both,
        }
    }

    /// The verdict of this operation against a term, forced by `verdicts`, each operand's against that term: a meet is at most the term where one operand is, and a join where every operand is. `None` where they force nothing at most.
    pub fn below(self, verdicts: &[Comparison]) -> Option<Comparison> {
        match self {
            Lattice::Meet => some_at_most(verdicts),
            Lattice::Join => all_at_most(verdicts),
        }
    }

    /// The verdict of a term against this operation, forced by `verdicts`, the term's against each operand: the term is at most a meet where it is at most every operand, and at most a join where it is at most one. `None` where they force nothing at most.
    pub fn above(self, verdicts: &[Comparison]) -> Option<Comparison> {
        match self {
            Lattice::Meet => all_at_most(verdicts),
            Lattice::Join => some_at_most(verdicts),
        }
    }
}

/// Whether a verdict is at most, and strictly.
fn at_most(verdict: Comparison) -> Option<bool> {
    match verdict {
        Comparison::Lt => Some(true),
        Comparison::Le | Comparison::Eq => Some(false),
        _ => None,
    }
}

/// `<` or `<=` by whether the step was strict.
fn stepped(strict: bool) -> Comparison {
    match strict {
        true => Comparison::Lt,
        false => Comparison::Le,
    }
}

/// At most where some verdict is, and strictly where some one is strict.
fn some_at_most(verdicts: &[Comparison]) -> Option<Comparison> {
    let steps = verdicts
        .iter()
        .filter_map(|verdict| at_most(*verdict))
        .collect::<Vec<_>>();
    (!steps.is_empty()).then(|| stepped(steps.into_iter().any(|strict| strict)))
}

/// At most where every verdict is, and strictly where every one is strict.
fn all_at_most(verdicts: &[Comparison]) -> Option<Comparison> {
    let steps = verdicts
        .iter()
        .map(|verdict| at_most(*verdict))
        .collect::<Option<Vec<_>>>()?;
    (!steps.is_empty()).then(|| stepped(steps.into_iter().all(|strict| strict)))
}

#[cfg(test)]
mod tests;
