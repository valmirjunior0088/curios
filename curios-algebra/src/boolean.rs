//! The two-element Boolean algebra: the identities its connectives satisfy, agreement of two leaf sets, and agreement of two formulas by a truth table over their atoms.
//!
//! **Agreement everywhere is equality; anything else is nothing.** A truth table treats its atoms as independent, which *over*-approximates the values they can take together — `x < y` and `x < y + 1` are two atoms that are not independent at all — so every combination the real values reach is among those visited, and two formulas that agree at every assignment agree at the real ones. For the same reason a disagreement proves nothing: the assignment it happens at may be one no value reaches. So [`Formula::agree`] answers equality or nothing, never a disequality, and inversion is handed no impossibility.
//!
//! **The cap is the theory's.** A table doubles with every atom, so past [`BOOL_ATOM_CAP`] the question is declined rather than priced out of a budget by accident, and [`Formula::evaluation_size`] states what the table costs before its first assignment. The cap is a constant of the language, not of the host: the same two formulas are decided, or not, on every target.

use crate::{Atom, Operation};

/// How many distinct atoms two formulas may hold between them and still be decided by table: `2⁸` assignments, each one walk of both formulas. Past it the decision declines, which costs completeness and never soundness.
pub const BOOL_ATOM_CAP: usize = 8;

/// One node of a formula in postorder, its operands named by their positions in the same formula — flat, so evaluating it is a loop however deep the written tree was.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Node {
    Literal(bool),
    /// An atom by its index, read negated where the leaf was the atom's negation: a comparison and its dual are one atom at two polarities.
    Atom {
        index: usize,
        negated: bool,
    },
    And(usize, usize),
    Or(usize, usize),
    Xor(usize, usize),
    Eql(usize, usize),
}

/// Formulas over one shared list of at most [`BOOL_ATOM_CAP`] atoms, built node by node in postorder by a caller that reads its terms into them.
#[derive(Default)]
pub struct Formula {
    nodes: Vec<Node>,
    atoms: usize,
}

impl Formula {
    /// A fresh atom's index, or `None` once the cap is reached — the point at which a caller stops reading.
    pub fn new_atom(&mut self) -> Option<usize> {
        if self.atoms == BOOL_ATOM_CAP {
            return None;
        }
        self.atoms += 1;
        Some(self.atoms - 1)
    }

    /// `node`, appended; its position, for a later node to name as an operand.
    pub fn push(&mut self, node: Node) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    /// What evaluating the table costs, stated before the first assignment: one visit per node per assignment, and the one row of values it is evaluated in.
    pub fn evaluation_size(&self) -> (u64, u64) {
        let nodes = self.nodes.len() as u64;
        (
            nodes.checked_shl(self.atoms as u32).unwrap_or(u64::MAX),
            nodes,
        )
    }

    /// Whether the formulas rooted at `left` and `right` agree at every assignment of the atoms: `true` is equality, `false` is *undecided* and never a disequality.
    pub fn agree(&self, left: usize, right: usize) -> bool {
        let mut values = vec![false; self.nodes.len()];
        (0..1u32 << self.atoms).all(|assignment| {
            self.fill(&mut values, assignment);
            values[left] == values[right]
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

/// Whether two leaf sets of one idempotent, commutative, associative connective are one set — which makes the two trees one value. Anything else is undecided and never unequal: two different leaf sets may still agree as values, `x && y` against `x` when `y` is `true`.
pub fn same_leaves(left: &[Atom], right: &[Atom]) -> bool {
    let covers = |these: &[Atom], those: &[Atom]| these.iter().all(|leaf| those.contains(leaf));
    covers(left, right) && covers(right, left)
}

/// What a connective's two operands were observed to be, as its identities read them: each one's literal value where it is one, whether they are one term, whether one is the other's negation, and — for `xor` — which operand of a nested `xor` beside the other cancels.
#[derive(Clone, Copy, Debug, Default)]
pub struct BooleanPair {
    pub left: Option<bool>,
    pub right: Option<bool>,
    pub same: bool,
    pub complementary: bool,
    /// Where one operand is an `xor` one of whose operands is the other operand: the operand of that `xor` that is left once the pair cancels, `(a ⊕ c) ⊕ c = a`.
    pub nested: Option<Nested>,
}

/// Which operand of which nested `xor` is left once `(a ⊕ c) ⊕ c` cancels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Nested {
    LeftFirst,
    LeftSecond,
    RightFirst,
    RightSecond,
}

/// What a connective is by an identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Connected {
    Left,
    Right,
    Literal(bool),
    /// The negation of an operand, which `xor` with `true` spells.
    NegatedLeft,
    NegatedRight,
    /// The operand of a nested `xor` left once it cancels.
    Nested(Nested),
}

impl Operation {
    /// The identities of the Boolean connectives, which read no atom's value: `&&` with `true` as unit and `||` with `false` — the other literal absorbing, a repeated operand itself, an operand beside its negation the absorber, which is the complement law and holds by cases; `xor` with `false` as unit, a repeated operand `false`, and a shared operand cancelling through one nesting, which takes `not(not(b))` back to `b`; `==` and `!=` with identical operands decided, complementary ones decided the other way, and a literal equal to the relation's own truth giving the other operand and the opposite literal negating it. `None` where no identity applies, or for any other operation.
    pub fn boolean_identity(self, operands: BooleanPair) -> Option<Connected> {
        let lattice = |unit: bool| match operands {
            BooleanPair {
                left: Some(literal),
                ..
            } => Some(match literal == unit {
                true => Connected::Right,
                false => Connected::Left,
            }),
            BooleanPair {
                right: Some(literal),
                ..
            } => Some(match literal == unit {
                true => Connected::Left,
                false => Connected::Right,
            }),
            BooleanPair { same: true, .. } => Some(Connected::Left),
            BooleanPair {
                complementary: true,
                ..
            } => Some(Connected::Literal(!unit)),
            _ => None,
        };
        let equality = |same: bool| match operands {
            BooleanPair { same: true, .. } => Some(Connected::Literal(same)),
            BooleanPair {
                complementary: true,
                ..
            } => Some(Connected::Literal(!same)),
            BooleanPair {
                left: Some(literal),
                ..
            } => Some(match literal == same {
                true => Connected::Right,
                false => Connected::NegatedRight,
            }),
            BooleanPair {
                right: Some(literal),
                ..
            } => Some(match literal == same {
                true => Connected::Left,
                false => Connected::NegatedLeft,
            }),
            _ => None,
        };
        match self {
            Operation::And => lattice(true),
            Operation::Or => lattice(false),
            Operation::Equal => equality(true),
            Operation::Unequal => equality(false),
            Operation::Xor => match operands {
                BooleanPair {
                    left: Some(false), ..
                } => Some(Connected::Right),
                BooleanPair {
                    right: Some(false), ..
                } => Some(Connected::Left),
                BooleanPair { same: true, .. } => Some(Connected::Literal(false)),
                BooleanPair {
                    nested: Some(nested),
                    ..
                } => Some(Connected::Nested(nested)),
                _ => None,
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests;
