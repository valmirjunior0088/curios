//! Deciding two `Bool` terms equal by a truth table over their atoms.
//!
//! The fold beside each connective takes the laws one node can see — a unit, an absorber, a repeated operand, an operand beside its own negation — and the peel compares two `&&` trees, or two `||` trees, as sets of leaves. Neither reaches a law that relates *different* connectives: De Morgan, absorption, distribution. Those have no local statement, and a normal form that would make them structural is a rewriting one, which the record rejects twice over — a guard's refinement is keyed on its written spelling (`documentation/design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md`), and a fold that normalized a tree whole paid for the whole tree at every leaf.
//!
//! So the decision is made where two terms are *compared*, asked for by name in both converters as [`super::normalize_bool`] and [`super::align_comparisons`] are, and it changes no spelling. Both sides are read as formulas over their atoms — every leaf that is not itself a connective or a literal, forced first, since the fold leaves a stuck connective's right operand as written — and evaluated at every assignment of those atoms.
//!
//! **Agreement everywhere is equality; anything else is nothing.** Treating the atoms as independent *over*-approximates the values they can take together: `x < y` and `x < y + 1` are two atoms that are not independent at all, and the table still visits every combination the real values can reach, so two formulas that agree at every assignment agree at the real ones. For the same reason a disagreement proves nothing — the assignment it happens at may be one no value reaches — so this never answers a disequality, and inversion is handed no impossibility.
//!
//! **The cap is the theory's.** A table doubles with every atom, so past `curios-algebra`'s `BOOL_ATOM_CAP` the question is declined rather than priced out of the budget by accident, and what is evaluated is charged before the first assignment. The cap is a constant of the language, not of the host: the same two terms are decided, or not, on every target.

use {
    super::dual_comparison,
    crate::{Cost, Intrinsic, Probe, ReduceError, Reducer, Subterm, Term},
    curios_algebra::{Formula, Node},
    curios_utilities::recurse,
};

/// Two formulas being read, over one shared list of atoms: the terms each atom stands for, and the formula `curios-algebra` evaluates.
#[derive(Default)]
struct Table {
    atoms: Vec<Term>,
    formula: Formula,
}

impl Table {
    /// Read `term` into the table and answer its root's position, or `None` once the atoms pass the cap. Every node is forced before it is read, which is what makes a leaf the value's rather than the spelling's — a [`Probe`], so a node with no value at the type level is read as written, an atom like any other.
    fn read(
        &mut self,
        reducer: &mut impl Reducer,
        term: Term,
    ) -> Result<Option<usize>, ReduceError> {
        recurse(|| {
            let forced = reducer
                .reduce_forced(term.clone())
                .probed()?
                .unwrap_or(term);
            let node = match &*forced {
                Subterm::Intrinsic(Intrinsic::Bool(value)) => Node::Literal(*value),
                Subterm::Intrinsic(
                    connective @ (Intrinsic::BoolAnd(left, right)
                    | Intrinsic::BoolOr(left, right)
                    | Intrinsic::BoolXor(left, right)
                    | Intrinsic::BoolEql(left, right)
                    | Intrinsic::BoolNeq(left, right)),
                ) => {
                    let Some(left) = self.read(reducer, left.clone())? else {
                        return Ok(None);
                    };
                    let Some(right) = self.read(reducer, right.clone())? else {
                        return Ok(None);
                    };
                    match connective {
                        Intrinsic::BoolAnd(..) => Node::And(left, right),
                        Intrinsic::BoolOr(..) => Node::Or(left, right),
                        Intrinsic::BoolEql(..) => Node::Eql(left, right),
                        // `!=` on `Bool` is `xor`, which is how `/sys` lowers it.
                        _ => Node::Xor(left, right),
                    }
                }
                _ => match self.atom(&forced) {
                    Some(atom) => atom,
                    None => return Ok(None),
                },
            };
            Ok(Some(self.formula.push(node)))
        })
    }

    /// The atom `leaf` is: one already met, at the polarity it was met at or — where `leaf` is a comparison on a total order — as the negation of its dual, the table `dual_comparison` keeps; otherwise a new one, or `None` past the cap. A `Bool` atom is compared as written, universe levels included: two terms are one atom only where they are one term.
    fn atom(&mut self, leaf: &Term) -> Option<Node> {
        let dual = match &**leaf {
            Subterm::Intrinsic(comparison) => dual_comparison(comparison).map(Term::intrinsic),
            _ => None,
        };
        for (index, atom) in self.atoms.iter().enumerate() {
            if atom == leaf {
                return Some(Node::Atom {
                    index,
                    negated: false,
                });
            }
            if dual.as_ref() == Some(atom) {
                return Some(Node::Atom {
                    index,
                    negated: true,
                });
            }
        }
        let index = self.formula.new_atom()?;
        self.atoms.push(leaf.clone());
        Some(Node::Atom {
            index,
            negated: false,
        })
    }
}

/// Whether a reduced term is headed by a `Bool` connective — the gate [`decide_bool`] stands behind, exported so a converter can ask it of a pair before it has a reducer's attention to spend.
pub fn is_bool_connective(term: &Term) -> bool {
    matches!(
        &**term,
        Subterm::Intrinsic(
            Intrinsic::BoolAnd(..)
                | Intrinsic::BoolOr(..)
                | Intrinsic::BoolXor(..)
                | Intrinsic::BoolEql(..)
                | Intrinsic::BoolNeq(..)
        )
    )
}

/// Whether `this` and `that` are one `Bool` at every assignment of their atoms: `true` is a definitional equality, `false` is *undecided* and never a disequality — see the module documentation for why both halves of that are forced. Declines at once unless a side is headed by a connective, so a pair this has nothing to say about costs two shape tests.
pub fn decide_bool(
    reducer: &mut impl Reducer,
    this: &Term,
    that: &Term,
) -> Result<bool, ReduceError> {
    if !is_bool_connective(this) && !is_bool_connective(that) {
        return Ok(false);
    }
    curios_profile::profile!("truth::decide_bool");

    let mut table = Table::default();
    let Some(left) = table.read(reducer, this.clone())? else {
        return Ok(false);
    };
    let Some(right) = table.read(reducer, that.clone())? else {
        return Ok(false);
    };

    // What the table costs, before its first assignment: one visit per node per assignment, and the one row of values it is evaluated in.
    let (visits, row) = table.formula.evaluation_size();
    reducer.spend(
        Cost::STEP
            .saturating_mul(visits)
            .saturating_add(Cost::buffer(row)),
    )?;

    Ok(table.formula.agree(left, right))
}
