//! Deciding two `Bool` terms equal by a truth table over their atoms.
//!
//! The fold beside each connective takes the laws one node can see — a unit, an absorber, a repeated operand, an operand beside its own negation — and the peel compares two `&&` trees, or two `||` trees, as sets of leaves. Neither reaches a law that relates *different* connectives: De Morgan, absorption, distribution. Those have no local statement, and a normal form that would make them structural is a rewriting one, which the record rejects twice over — a guard's refinement is keyed on its written spelling (`documentation/design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md`), and a fold that normalized a tree whole paid for the whole tree at every leaf.
//!
//! So the decision is made where two terms are *compared*, asked for by name in both converters as [`super::normalize_bool`] and [`super::align_comparisons`] are, and it changes no spelling. Both sides are read as formulas over their atoms — every leaf that is not itself a connective or a literal, forced first, since the fold leaves a stuck connective's right operand as written — and evaluated at every assignment of those atoms.
//!
//! **Agreement everywhere is equality; anything else is nothing.** Treating the atoms as independent *over*-approximates the values they can take together: `x < y` and `x < y + 1` are two atoms that are not independent at all, and the table still visits every combination the real values can reach, so two formulas that agree at every assignment agree at the real ones. For the same reason a disagreement proves nothing — the assignment it happens at may be one no value reaches — so this never answers a disequality, and inversion is handed no impossibility.
//!
//! **The cap is the theory's.** A table doubles with every atom, so past [`BOOL_ATOM_CAP`] the question is declined rather than priced out of the budget by accident, and what is evaluated is charged before the first assignment. The cap is a constant of the language, not of the host: the same two terms are decided, or not, on every target.

use {
    super::dual_comparison,
    crate::{Cost, Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_utilities::recurse,
};

/// How many distinct atoms two `Bool` terms may hold between them and still be decided by table: `2⁸` assignments, each one walk of both formulas. Past it the decision declines, which costs completeness and never soundness.
pub const BOOL_ATOM_CAP: usize = 8;

/// One node of a formula in postorder, its operands named by their positions in the same list — flat so that evaluating it is a loop however deep the written tree was, the depth of a `&&` chain being data-shaped.
enum Node {
    Literal(bool),
    /// An atom by its index, read negated where the leaf was the atom's dual: `y <= x` beside `x < y` is one atom at two polarities.
    Atom {
        index: usize,
        negated: bool,
    },
    And(usize, usize),
    Or(usize, usize),
    Xor(usize, usize),
    Eql(usize, usize),
}

/// The two formulas being compared, over one shared list of atoms.
#[derive(Default)]
struct Table {
    atoms: Vec<Term>,
    nodes: Vec<Node>,
}

impl Table {
    /// Read `term` into the table and answer its root's position, or `None` once the atoms pass the cap. Every node is forced before it is read, which is what makes a leaf the value's rather than the spelling's.
    fn read(
        &mut self,
        reducer: &mut impl Reducer,
        term: Term,
    ) -> Result<Option<usize>, ReduceError> {
        recurse(|| {
            let forced = reducer.reduce_forced(term)?;
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
            self.nodes.push(node);
            Ok(Some(self.nodes.len() - 1))
        })
    }

    /// The atom `leaf` is: one already met, at the polarity it was met at or — where `leaf` is a comparison on a total order — as the negation of its dual, the table `dual_comparison` keeps; otherwise a new one, or `None` past the cap.
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
        if self.atoms.len() == BOOL_ATOM_CAP {
            return None;
        }
        self.atoms.push(leaf.clone());
        Some(Node::Atom {
            index: self.atoms.len() - 1,
            negated: false,
        })
    }

    /// Every node's value at one assignment, bit `index` of which is atom `index`. Postorder, so an operand is always filled before the node that reads it.
    fn fill(&self, values: &mut [bool], assignment: u32) {
        for (position, node) in self.nodes.iter().enumerate() {
            values[position] = match *node {
                Node::Literal(value) => value,
                Node::Atom { index, negated } => (assignment >> index & 1 == 1) != negated,
                Node::And(left, right) => values[left] && values[right],
                Node::Or(left, right) => values[left] || values[right],
                Node::Xor(left, right) => values[left] != values[right],
                Node::Eql(left, right) => values[left] == values[right],
            };
        }
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
    let nodes = table.nodes.len() as u64;
    reducer.spend(
        Cost::STEP
            .saturating_mul(nodes << table.atoms.len())
            .saturating_add(Cost::buffer(nodes)),
    )?;

    let mut values = vec![false; table.nodes.len()];
    Ok((0..1u32 << table.atoms.len()).all(|assignment| {
        table.fill(&mut values, assignment);
        values[left] == values[right]
    }))
}
