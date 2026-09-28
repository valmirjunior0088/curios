//! The canonical linear view of a `Nat` or `Int` comparison, and the contract of what conversion decides about one — published for procedures outside the converters, which rely on this statement rather than on reading the folds.
//!
//! # Atoms
//!
//! A view is over atoms as [`crate::atoms`] identifies them for the numeric carriers: **one atom per term up to universe instances**, a level never being part of which number a term is. A product is read as the product of its factors, each an atom; a sum's summands are the monomials. Two terms that are one atom only up to conversion — `f(x + y)` and `f(y + x)` — are two atoms here, which is the declining direction.
//!
//! **The order is total.** Atoms are ordered by the structural hash of their projected term, and two atoms of one hash by the order the view read them in; a monomial's atoms are put in that order, and a view's monomials are ordered lexicographically by their atoms. The second key is the only way a view can depend on how its sums were written, and it is reached only on a hash collision, where it can order two atoms but never merge them.
//!
//! # The view
//!
//! A comparison `a ⋈ b` is read as the difference `a - b ⋈ 0` over ℤ: every monomial once with its integer coefficient, the constant separated. A `Nat` comparison is read through the embedding ℕ → ℤ, which is a semiring homomorphism, and every one of its atoms is recorded as non-negative; an `Int` side reads a widened natural, `Nat/to_int(t)`, as the atom `t`, non-negative, so a view does not tell a natural from its image. So two comparisons of one relation whose sums differ in spelling, association, order, or the side a term was written on have one view.
//!
//! The view is read where it is asked for and rebuilds nothing: it reads the operands as they stand — a caller hands it reduced ones — forces no term, and distributes no product the fold left standing, which stays one atom of its own.
//!
//! # What conversion decides
//!
//! Conversion decides a `Nat` or `Int` comparison, in both checkers and with the context's hypotheses out of it, where one of these holds of its view — the rules `compare_nat` and `compare_int` apply, `Int`'s through the preimages of widened naturals:
//!
//! 1. **Every atom cancelled**: the constant's sign is the answer. `x + y + 2 <= y + x + 3`.
//! 2. **Every monomial of one sign over non-negative atoms**, and a constant that does not oppose them: a sum of naturals is at least its constant, so `0 < x + y + 1` is true and `x < 0` false, while `0 < x` stays open. At `Int` this holds where every atom is a widened natural.
//! 3. **A side bounded by a literal, or through an operand it never exceeds**: `x % 7 < 7`, `x - y <= x` — the bounds oracle and domination, which `documentation/soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md` states.
//! 4. **An equality whose constant the gcd of its coefficients does not divide**, which is false: `2 · x + 1 == 2 · y`.
//!
//! **It decides nothing else about a comparison whose view keeps an atom** — `x + 1 <= y`, `0 < x`, `i + 1 > 0` stay open. Each clause is a held row of `curios`'s law grid, carrier "What conversion decides about a comparison", with its complement a control beside it.
//!
//! A view is read-only: it prepares and reports, and it neither searches nor commits anything.

use {
    super::{Atoms, Intrinsic, Nat, Operands, Subterm, Term, int_monomial, int_terms},
    crate::Declaration,
    curios_algebra::{Atom, Carrier, Combination, LinearForm, Monomial, Operation, Summand},
    curios_num::Integer,
};

/// A `Nat` or `Int` comparison as the canonical linear view reads it: the relation, and the difference of its sides.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearView {
    relation: Operation,
    form: LinearForm,
}

/// Reads views over one table of atoms, so the views it reads can be compared: a view's atoms are handles, and two handles are one atom only when one reader handed them out. Two comparisons of one relation that agree in their view are one proposition.
#[derive(Default)]
pub struct LinearViews {
    atoms: Atoms,
}

impl LinearViews {
    /// The view of `comparison`, when it is a `Nat` or `Int` ordering or equality — `<`, `<=`, `==` or `!=`, which is every comparison the roster builds — and `None` for anything else.
    pub fn view(&mut self, comparison: &Intrinsic) -> Option<LinearView> {
        let Declaration::Numeric {
            carrier,
            operation,
            operands: Operands::Two([left, right]),
        } = comparison.algebra()
        else {
            return None;
        };
        if !matches!(
            operation,
            Operation::Less | Operation::AtMost | Operation::Equal | Operation::Unequal
        ) {
            return None;
        }

        let mut nonnegative = Vec::new();
        let left = side(&mut self.atoms, &mut nonnegative, carrier, left);
        let right = side(&mut self.atoms, &mut nonnegative, carrier, right);
        Some(LinearView {
            relation: operation,
            form: LinearForm::difference(left, right, nonnegative),
        })
    }
}

impl LinearView {
    /// The view of one comparison, read by a reader of its own — [`LinearViews::view`] for a view compared with nothing.
    pub fn of(comparison: &Intrinsic) -> Option<Self> {
        LinearViews::default().view(comparison)
    }

    /// The relation the comparison states of its difference against zero.
    pub fn relation(&self) -> Operation {
        self.relation
    }

    /// The difference of the comparison's sides.
    pub fn form(&self) -> &LinearForm {
        &self.form
    }

    /// The comparison's value where its view is a constant — the case the contract says conversion decides by the view alone; `None` where an atom is left.
    pub fn decided(&self) -> Option<bool> {
        self.form.decided(self.relation)
    }
}

/// One side of a comparison over `carrier`, read as a combination over ℤ, the atoms it reads as naturals added to `nonnegative`.
fn side(
    atoms: &mut Atoms,
    nonnegative: &mut Vec<Atom>,
    carrier: Carrier,
    term: &Term,
) -> Combination<Integer, ()> {
    match carrier {
        Carrier::Natural => {
            let (floor, inner) = Nat::decompose(term);
            let summands = Nat::summands(&inner)
                .iter()
                .map(|summand| {
                    let (coefficient, factors) = Nat::monomial(summand);
                    let factors = factors
                        .iter()
                        .map(|factor| {
                            let atom = atoms.numeric(factor);
                            nonnegative.push(atom);
                            atom
                        })
                        .collect();
                    Summand {
                        coefficient: Integer::from(coefficient),
                        monomial: Monomial::product(factors),
                        origin: (),
                    }
                })
                .collect::<Vec<_>>();
            Combination::collect(Integer::from(floor), summands).without_zeros()
        }
        Carrier::Integer => {
            let (constant, summands) = int_terms(term);
            let summands = summands
                .iter()
                .map(|summand| {
                    let (coefficient, factors) = int_monomial(summand);
                    let factors = factors
                        .iter()
                        .map(|factor| match &**factor {
                            Subterm::Intrinsic(Intrinsic::NatToInt(natural)) => {
                                let atom = atoms.numeric(natural);
                                nonnegative.push(atom);
                                atom
                            }
                            _ => atoms.numeric(factor),
                        })
                        .collect();
                    Summand {
                        coefficient,
                        monomial: Monomial::product(factors),
                        origin: (),
                    }
                })
                .collect::<Vec<_>>();
            Combination::collect(constant, summands).without_zeros()
        }
    }
}

#[cfg(test)]
mod tests;
