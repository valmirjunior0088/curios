//! Reading a boolean scrutinee as a comparison between a tracked binder and a `Nat` literal.
//!
//! This is the *only* shape from which an arm can conclude that a binder is not zero, and that conclusion is what licenses an arithmetic decrease: `n / k` and `n - k` are below `n` only where `n` is nonzero, since both saturate. `/std/Nat/to_str` descends on exactly this — the false arm of `n < 10` gives `n >= 10`.
//!
//! The relation table below is therefore an accepting rule, and each row says what it takes for the arm's fact to exclude zero. `documentation/PERIMETER.md`'s `record_totality` entry records the probes.

use {
    curios_core::{Free, Prim, Subterm, Term},
    num_bigint::BigUint,
};

/// How a [`Guard`] relates its binder to its literal, always read with the binder on the left.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Relation {
    Lt,
    Lte,
    Gt,
    Gte,
    Eql,
    Neq,
}

impl Relation {
    /// The same relation with the operands exchanged, for a guard written with its literal first (`10 > n`).
    pub(super) fn flipped(self) -> Relation {
        match self {
            Relation::Lt => Relation::Gt,
            Relation::Lte => Relation::Gte,
            Relation::Gt => Relation::Lt,
            Relation::Gte => Relation::Lte,
            Relation::Eql => Relation::Eql,
            Relation::Neq => Relation::Neq,
        }
    }
}

/// A boolean scrutinee read as a comparison between a tracked binder and a `Nat` literal — the only shape from which an arm can conclude that the binder is not zero.
pub(super) struct Guard {
    pub(super) atom: Free,
    pub(super) literal: BigUint,
    pub(super) relation: Relation,
}

impl Guard {
    pub(super) fn read(term: &Term) -> Option<Guard> {
        let (left, right, relation) = match &**term {
            Subterm::Prim(Prim::NatLt(left, right)) => (left, right, Relation::Lt),
            Subterm::Prim(Prim::NatLte(left, right)) => (left, right, Relation::Lte),
            Subterm::Prim(Prim::NatGt(left, right)) => (left, right, Relation::Gt),
            Subterm::Prim(Prim::NatGte(left, right)) => (left, right, Relation::Gte),
            Subterm::Prim(Prim::NatEql(left, right)) => (left, right, Relation::Eql),
            Subterm::Prim(Prim::NatNeq(left, right)) => (left, right, Relation::Neq),
            _ => return None,
        };

        let atom = |term: &Term| match &**term {
            Subterm::Var(var) => var.as_free().cloned(),
            _ => None,
        };
        let literal = |term: &Term| term.as_nat().and_then(|nat| nat.to_big_uint());

        if let (Some(atom), Some(literal)) = (atom(left), literal(right)) {
            return Some(Guard {
                atom,
                literal,
                relation,
            });
        }
        match (literal(left), atom(right)) {
            (Some(literal), Some(atom)) => Some(Guard {
                atom,
                literal,
                relation: relation.flipped(),
            }),
            _ => None,
        }
    }

    /// Whether the arm in which this guard evaluated to `taken` proves the binder is not zero.
    ///
    /// Each row is the arm's fact about `atom` followed by what it takes for that fact to exclude zero: `atom >= k` excludes it only for `k >= 1`, while `atom > k` excludes it for every `k`.
    pub(super) fn establishes_nonzero(&self, taken: bool) -> bool {
        let zero = BigUint::from(0usize);
        let one = BigUint::from(1usize);

        match (self.relation, taken) {
            // atom > k, hence atom >= k + 1 >= 1.
            (Relation::Gt, true) | (Relation::Lte, false) => true,
            // atom >= k.
            (Relation::Gte, true) | (Relation::Lt, false) => self.literal >= one,
            // atom == k.
            (Relation::Eql, true) | (Relation::Neq, false) => self.literal >= one,
            // atom != k.
            (Relation::Neq, true) | (Relation::Eql, false) => self.literal == zero,
            // atom < k and atom <= k both admit zero.
            (Relation::Lt, true) | (Relation::Lte, true) => false,
            (Relation::Gt, false) | (Relation::Gte, false) => false,
        }
    }
}
