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
//! And between two comparisons: 5. **two comparisons whose views agree once aligned** — `<` read as the `<=` of its successor, an equality oriented by its atoms — are one proposition, whichever carrier each is at: `x + 1 <= y` and `x < y`, `Nat/to_int(m) + 1 <= Nat/to_int(n)` and `m < n`. Otherwise both are respelled from their views before the congruence compares them ([`LinearViews::align`]).
//!
//! **It decides nothing else about a comparison whose view keeps an atom** — `x + 1 <= y`, `0 < x`, `i + 1 > 0` stay open. Each clause is a held row of `curios`'s law grid, carrier "What conversion decides about a comparison", with its complement a control beside it.
//!
//! A view is read-only: it prepares and reports, and it neither searches nor commits anything.

use {
    super::{
        Atoms, Intrinsic, Nat, Operands, Subterm, Term, int_from_linear, int_monomial, int_terms,
    },
    crate::Declaration,
    curios_algebra::{Atom, Carrier, Combination, LinearForm, Monomial, Operation, Part, Summand},
    curios_num::{Integer, Natural},
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
        self.read(comparison).map(|(_, view)| view)
    }

    /// [`LinearViews::view`], beside the carrier the comparison is at, which a view does not keep: it does not tell a natural from its image.
    fn read(&mut self, comparison: &Intrinsic) -> Option<(Carrier, LinearView)> {
        let Declaration::Operation {
            carrier,
            operation,
            operands: Operands::Two([left, right]),
        } = comparison.algebra()
        else {
            return None;
        };
        if !matches!(carrier, Carrier::Natural | Carrier::Integer)
            || !matches!(
                operation,
                Operation::Less | Operation::AtMost | Operation::Equal | Operation::Unequal
            )
        {
            return None;
        }

        let mut nonnegative = Vec::new();
        let left = side(&mut self.atoms, &mut nonnegative, carrier, left);
        let right = side(&mut self.atoms, &mut nonnegative, carrier, right);
        Some((
            carrier,
            LinearView {
                relation: operation,
                form: LinearForm::difference(left, right, nonnegative),
            },
        ))
    }

    /// Two comparisons aligned through their views: one proposition where their views agree once aligned — `x + 1 <= y` and `x < y`, `Nat/to_int(m) < Nat/to_int(n)` and `m < n`, `i < j` and `+0 < j - i` — and otherwise both respelled in the one spelling their views give, so a congruence after meets aligned operands: `?n < y + 1` against `x <= y` becomes `?n <= y` against `x <= y`, which solves `?n` as `x`. `None` where either is no `Nat` or `Int` comparison.
    ///
    /// **One reading replaces three.** The successor step between `<` and `<=`, `Int`'s difference split by sign, and the pull-back of an `Int` comparison of widened naturals to `Nat` are each a case of reading both comparisons as the difference of their sides over ℤ in one canonical form.
    pub fn align(&mut self, this: &Intrinsic, that: &Intrinsic) -> Option<Aligned> {
        let (this_carrier, this) = self.read(this)?;
        let (that_carrier, that) = self.read(that)?;
        let (this_relation, this_form) = this.form.aligned(this.relation);
        let (that_relation, that_form) = that.form.aligned(that.relation);
        if this_relation == that_relation && this_form == that_form {
            return Some(Aligned::Same);
        }
        Some(Aligned::Respelled(Box::new((
            self.respell(this_carrier, this_relation, &this_form),
            self.respell(that_carrier, that_relation, &that_form),
        ))))
    }

    /// The comparison an aligned view spells: the positive part of its difference on the left, the negated negative part on the right, at the comparison's own carrier — or at `Nat` where every atom is a widened natural, which is what makes an `Int` comparison of widened naturals the `Nat` comparison of their preimages. Rebuilt through the carriers' own sums, so the operands are in the normal form a fold leaves.
    fn respell(&self, carrier: Carrier, relation: Operation, form: &LinearForm) -> Intrinsic {
        let (left, right) = form.sides();
        let every_atom_natural = form.terms.iter().all(|(_, monomial)| {
            monomial
                .atoms()
                .iter()
                .all(|atom| form.nonnegative.contains(atom))
        });
        match carrier == Carrier::Natural || every_atom_natural {
            true => {
                let (left, right) = (self.nat_side(&left), self.nat_side(&right));
                match relation {
                    Operation::AtMost => Intrinsic::NatLe(left, right),
                    Operation::Equal => Intrinsic::NatEql(left, right),
                    _ => Intrinsic::NatNeq(left, right),
                }
            }
            false => {
                let (left, right) = (self.int_side(&left, form), self.int_side(&right, form));
                match relation {
                    Operation::AtMost => Intrinsic::IntLe(left, right),
                    Operation::Equal => Intrinsic::IntEql(left, right),
                    _ => Intrinsic::IntNeq(left, right),
                }
            }
        }
    }

    /// One side as a `Nat`: each monomial the product of its atoms' natural terms, summed over the side's constant.
    fn nat_side(&self, side: &Part) -> Term {
        let summands = side
            .terms
            .iter()
            .map(|(coefficient, monomial)| {
                let factor = monomial
                    .atoms()
                    .iter()
                    .map(|atom| self.atoms.term(*atom).clone())
                    .reduce(|left, right| Nat::multiply(&left, &right))
                    .expect("a monomial of a view stands on an atom");
                (
                    Natural::try_from(coefficient).expect("a side's coefficients are positive"),
                    factor,
                )
            })
            .collect();
        let floor = Natural::try_from(&side.constant).expect("a side's constant is non-negative");
        Nat::from_linear(summands, floor)
    }

    /// One side as an `Int`: each monomial over its atoms, a widened natural widened back.
    fn int_side(&self, side: &Part, form: &LinearForm) -> Term {
        let monomials = side
            .terms
            .iter()
            .map(|(coefficient, monomial)| {
                let factors = monomial
                    .atoms()
                    .iter()
                    .map(|atom| {
                        let term = self.atoms.term(*atom).clone();
                        match form.nonnegative.contains(atom) {
                            true => Term::intrinsic(Intrinsic::NatToInt(term)),
                            false => term,
                        }
                    })
                    .collect();
                (coefficient.clone(), factors)
            })
            .collect();
        int_from_linear(side.constant.clone(), monomials)
    }
}

/// What aligning two comparisons concluded.
pub enum Aligned {
    /// The two are one proposition: their aligned views agree.
    Same,
    /// The pair, each in the spelling its view gives.
    Respelled(Box<(Intrinsic, Intrinsic)>),
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
        Carrier::Boolean | Carrier::Byte | Carrier::Float | Carrier::Packed(_) | Carrier::List => {
            unreachable!("a view is read only of a `Nat` or `Int` comparison")
        }
    }
}

#[cfg(test)]
mod tests;
