//! `min` and `max` trees over `Nat` and `Int`: which node is one, and the leaves a tree holds.
//!
//! **A literal added to every operand leaves.** `min(a, b) + 1` is `min(a + 1, b + 1)`, so a node of one operation under a literal offset is read through, the offset going to each of its operands. A tree's leaves are therefore the same whether a fold hoisted an offset out of a node or left it on the operands, and two trees of one operation are one value where they hold one set of leaves (`peel_lattice`).
//!
//! **An offset is a `Nat`'s successor floor or an `Int`'s constant**, each the literal its carrier's sum normal form keeps apart, so reading one off a reduced term and putting one back build nothing a fold would respell.

use {
    crate::{
        Declaration, Intrinsic, Nat, Operands, Subterm, Term, int_from_linear, int_linear,
        int_terms,
    },
    curios_algebra::{Carrier, Lattice},
    curios_num::{Integer, Natural},
};

/// A lattice operation at the carrier it is over: `min` or `max`, on `Nat` or on `Int`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Extremum {
    pub carrier: Carrier,
    pub lattice: Lattice,
}

impl Extremum {
    /// The lattice operation `intrinsic` is over `Nat` or `Int`, with its two operands. `None` for any other intrinsic, a `Flt`'s `min` and `max` among them: those commute, and are no semilattice conversion decides.
    pub fn of(intrinsic: &Intrinsic) -> Option<(Self, &Term, &Term)> {
        match intrinsic.algebra() {
            Declaration::Operation {
                carrier: carrier @ (Carrier::Natural | Carrier::Integer),
                operation,
                operands: Operands::Two([left, right]),
            } => {
                let lattice = operation.lattice()?;
                Some((Extremum { carrier, lattice }, left, right))
            }
            _ => None,
        }
    }

    /// This operation over two operands.
    pub fn node(self, left: Term, right: Term) -> Intrinsic {
        match (self.carrier, self.lattice) {
            (Carrier::Natural, Lattice::Meet) => Intrinsic::NatMin(left, right),
            (Carrier::Natural, Lattice::Join) => Intrinsic::NatMax(left, right),
            (_, Lattice::Meet) => Intrinsic::IntMin(left, right),
            (_, Lattice::Join) => Intrinsic::IntMax(left, right),
        }
    }

    /// The literal offset a reduced term carries, and the term without it: a `Nat`'s floor over its inner, an `Int`'s constant beside its summands.
    pub fn offset(self, term: &Term) -> (Integer, Term) {
        match self.carrier {
            Carrier::Natural => {
                let (floor, inner) = Nat::decompose(term);
                (Integer::from(floor), inner)
            }
            _ => {
                let (constant, summands) = int_terms(term);
                let rest = int_from_linear(Integer::from(0i32), int_linear(summands));
                (constant, rest)
            }
        }
    }

    /// `rest + offset` in the carrier's normal form, `rest` carrying no offset of its own. An offset below zero under a `Nat` is none a caller builds, and is read as zero.
    pub fn offset_by(self, rest: Term, offset: &Integer) -> Term {
        match self.carrier {
            Carrier::Natural => {
                let floor = Natural::try_from(offset).unwrap_or_else(|_| Natural::zero());
                Nat::rebuild(floor, rest)
            }
            _ => {
                let (_, summands) = int_terms(&rest);
                int_from_linear(offset.clone(), int_linear(summands))
            }
        }
    }

    /// `term` read as a node of this operation under a literal offset: its two operands, each carrying the offset. `None` where it is no such node.
    pub fn operands(self, term: &Term) -> Option<[Term; 2]> {
        let (offset, rest) = self.offset(term);
        let Subterm::Intrinsic(intrinsic) = &*rest else {
            return None;
        };
        let (this, left, right) = Extremum::of(intrinsic)?;
        (this == self).then(|| [self.raised(left, &offset), self.raised(right, &offset)])
    }

    /// The leaves of the tree `term` is at this operation, left to right, each carrying the offsets of the nodes above it. A term that is no node of this operation is one leaf. An explicit worklist, the tree's depth being data-shaped.
    pub fn leaves(self, term: &Term) -> Vec<Term> {
        let mut leaves = Vec::new();
        let mut pending = vec![term.clone()];
        while let Some(term) = pending.pop() {
            match self.operands(&term) {
                Some([left, right]) => {
                    pending.push(right);
                    pending.push(left);
                }
                None => leaves.push(term),
            }
        }
        leaves
    }

    /// `term + by`, where `term` may carry an offset of its own.
    pub fn raised(self, term: &Term, by: &Integer) -> Term {
        let (offset, rest) = self.offset(term);
        self.offset_by(rest, &(offset + by.clone()))
    }
}
