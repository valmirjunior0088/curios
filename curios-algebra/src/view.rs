//! The canonical linear form of a comparison: the difference of its two sides, over atoms in one order.
//!
//! **One relation, one form.** `a ⋈ b` holds exactly when `a - b ⋈ 0` over ℤ, so a comparison is read as that difference: every monomial once, its coefficient over ℤ, the constant separated, and the monomials in the order of their atoms' ranks. Two comparisons of one relation whose sums differ only in spelling, association, order, or which side a term was written on have one form — and a form with no monomial left is decided by its constant's sign alone.
//!
//! The order is total over a caller's atoms: by rank, then — only where two atoms share a rank — by the order the caller handed them out in. The second key is the one place a form can depend on how its sums were written, and it is reached only when the caller's ranks collide; it can order two atoms, never merge them.

use {
    crate::{Atom, Combination, Monomial, Operation, Summand},
    curios_num::Integer,
    std::cmp::Ordering,
};

/// The difference of a comparison's two sides over ℤ: its constant, its monomials in canonical order each with its coefficient, and the atoms known to be non-negative — every atom of a `Nat` side, and every atom an `Int` side read through the embedding of ℕ.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearForm {
    pub constant: Integer,
    pub terms: Vec<(Integer, Monomial)>,
    pub nonnegative: Vec<Atom>,
}

impl LinearForm {
    /// `left - right`, from two combinations read over one table of atoms, with `nonnegative` the atoms that stand for naturals.
    pub fn difference<O>(
        left: Combination<Integer, O>,
        right: Combination<Integer, O>,
        mut nonnegative: Vec<Atom>,
    ) -> Self {
        let mut constant = left.constant.clone() - right.constant.clone();
        let negated = right.summands.into_iter().map(|summand| Summand {
            coefficient: -summand.coefficient,
            ..summand
        });
        let difference =
            Combination::collect(Integer::from(0), left.summands.into_iter().chain(negated))
                .without_zeros();
        // A monomial over no atom is a constant, wherever in a sum it was written.
        let mut terms = Vec::with_capacity(difference.summands.len());
        for summand in difference.summands {
            match summand.monomial.atoms().is_empty() {
                true => constant = constant + summand.coefficient,
                false => terms.push((summand.coefficient, summand.monomial)),
            }
        }
        terms.sort_by(|(_, left), (_, right)| canonical(left, right));
        // Only the atoms a monomial still stands on: one that cancelled says nothing about this difference.
        nonnegative.retain(|atom| {
            terms
                .iter()
                .any(|(_, monomial)| monomial.atoms().contains(atom))
        });
        nonnegative.sort_by(|left, right| atom_order(*left, *right));
        nonnegative.dedup();
        LinearForm {
            constant,
            terms,
            nonnegative,
        }
    }

    /// The comparison `relation` states of this difference against zero, where every monomial cancelled and the constant alone decides it; `None` where a monomial is left.
    pub fn decided(&self, relation: Operation) -> Option<bool> {
        if !self.terms.is_empty() {
            return None;
        }
        let zero = Integer::from(0);
        match relation {
            Operation::Less => Some(self.constant < zero),
            Operation::AtMost => Some(self.constant <= zero),
            Operation::Equal => Some(self.constant == zero),
            Operation::Unequal => Some(self.constant != zero),
            _ => None,
        }
    }
}

/// Atoms by rank, then by the order they were handed out in.
fn atom_order(left: Atom, right: Atom) -> Ordering {
    left.rank()
        .cmp(&right.rank())
        .then(left.index().cmp(&right.index()))
}

/// Monomials by their atoms, lexicographically in [`atom_order`].
fn canonical(left: &Monomial, right: &Monomial) -> Ordering {
    left.atoms()
        .iter()
        .zip(right.atoms())
        .map(|(left, right)| atom_order(*left, *right))
        .find(|ordering| ordering.is_ne())
        .unwrap_or_else(|| left.atoms().len().cmp(&right.atoms().len()))
}
