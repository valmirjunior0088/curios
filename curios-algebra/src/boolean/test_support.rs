//! The truth table the decision diagram is held to, and the formulas it is held to it over.

use super::{Formula, Node};

/// Whether the formulas rooted at `left` and `right` agree at every assignment of `formula`'s atoms, each evaluated at every one: the truth table, which needs nothing but the formula's meaning.
pub(super) fn tabled(formula: &Formula, left: usize, right: usize) -> bool {
    let mut values = vec![false; formula.nodes.len()];
    (0..1u64 << formula.atoms).all(|assignment| {
        for (position, node) in formula.nodes.iter().enumerate() {
            values[position] = match *node {
                Node::Literal(value) => value,
                Node::Atom { index, negated } => (assignment >> index & 1 == 1) != negated,
                Node::And(left, right) => values[left] && values[right],
                Node::Or(left, right) => values[left] || values[right],
                Node::Xor(left, right) => values[left] != values[right],
                Node::Eql(left, right) => values[left] == values[right],
            };
        }
        values[left] == values[right]
    })
}

/// Whether the diagram finds the formulas rooted at `left` and `right` to agree, charged nothing.
pub(super) fn diagrammed(formula: &Formula, left: usize, right: usize) -> bool {
    let mut agreement = formula.agreement(left, right);
    loop {
        if let Ok(Some(agree)) = agreement.step(|_| Ok::<(), ()>(())) {
            return agree;
        }
    }
}

/// What the diagram charges for deciding the formulas rooted at `left` and `right`, in its units.
pub(super) fn charged(formula: &Formula, left: usize, right: usize) -> u64 {
    let mut agreement = formula.agreement(left, right);
    let mut spent = 0u64;
    loop {
        let step = agreement.step(|units| {
            spent = spent.saturating_add(units);
            Ok::<(), ()>(())
        });
        if let Ok(Some(_)) = step {
            return spent;
        }
    }
}

/// A fixed sequence of numbers, the same in every run, that formulas are drawn from.
pub(super) struct Draws(u64);

impl Draws {
    pub(super) fn new() -> Self {
        Draws(0x9E37_79B9_7F4A_7C15)
    }

    /// A number below `bound`.
    pub(super) fn below(&mut self, bound: usize) -> usize {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        (self.0 >> 33) as usize % bound
    }

    /// Two formulas of up to twelve leaves each over the first `atoms` atoms of `formula`, appended; their roots' positions.
    pub(super) fn pair(&mut self, formula: &mut Formula, atoms: usize) -> (usize, usize) {
        let size = 1 + self.below(12);
        let left = self.formula(formula, atoms, size);
        let size = 1 + self.below(12);
        let right = self.formula(formula, atoms, size);
        (left, right)
    }

    /// A formula of `size` leaves over the first `atoms` atoms of `formula`, each connective and each leaf's atom and polarity drawn, appended; its root's position.
    pub(super) fn formula(&mut self, formula: &mut Formula, atoms: usize, size: usize) -> usize {
        if size <= 1 {
            return match self.below(8) {
                0 => formula.push(Node::Literal(self.below(2) == 0)),
                _ => formula.push(Node::Atom {
                    index: self.below(atoms),
                    negated: self.below(3) == 0,
                }),
            };
        }
        let split = 1 + self.below(size - 1);
        let left = self.formula(formula, atoms, split);
        let right = self.formula(formula, atoms, size - split);
        formula.push(match self.below(4) {
            0 => Node::And(left, right),
            1 => Node::Or(left, right),
            2 => Node::Xor(left, right),
            _ => Node::Eql(left, right),
        })
    }
}
