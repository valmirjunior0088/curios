//! The two-element Boolean algebra: the identities its connectives satisfy, agreement of two leaf sets, and agreement of two formulas over their atoms, decided by a reduced ordered decision diagram built for one comparison.
//!
//! **Agreement everywhere is equality; anything else is nothing.** A diagram treats its atoms as independent, which *over*-approximates the values they can take together — `x < y` and `x < y + 1` are two atoms that are not independent at all — so every combination the real values reach is among those it reads, and two formulas that agree at every assignment agree at the real ones. For the same reason a disagreement proves nothing: the assignment it happens at may be one no value reaches. So [`Agreement`] answers equality or nothing, never a disequality, and inversion is handed no impossibility.
//!
//! **The price is the theory's, and it is paid a step at a time.** Deciding two formulas equivalent is co-NP-complete, so no procedure is cheap on every pair, and a limit on the atoms a pair holds breaks transitivity and closure under substitution at any value it takes: two contradictions over more than half of it each agree with `false` and not with each other. So [`Agreement`] declines nothing. It reports each step's charge before it takes it — a unit a step, and the logical size of each node and memo entry it stores — its caller spends that from the reduction budget, and a pair the budget cannot finish exhausts it, which a larger budget does not. The units are a function of the two formulas as read, their graph and the order their atoms were met in, and of no host, so one pair costs the same on every target.
//!
//! **A diagram is canonical under its order** (Bryant, *Graph-Based Algorithms for Boolean Function Manipulation*, 1986): two formulas agree exactly where their roots are one node, and applying a connective to diagrams of sizes `m` and `n` takes at most `m · n` steps. Its size follows its order, which is the atoms' indices, the order a reader met them in walking the pair; no order is searched for, finding the best being NP-complete. Nothing a comparison builds outlives it.

use {
    crate::{Atom, Operation},
    std::collections::HashMap,
};

/// One node of a formula in postorder, its operands named by their positions in the same formula — flat, so building it is a loop however deep the written tree was.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Node {
    Literal(bool),
    /// An atom by its index, read negated where the leaf was the atom's negation: a comparison and its negation are one atom at two polarities.
    Atom {
        index: usize,
        negated: bool,
    },
    And(usize, usize),
    Or(usize, usize),
    Xor(usize, usize),
    Eql(usize, usize),
}

/// Formulas over one shared list of atoms, built node by node in postorder by a caller that reads its terms into them.
#[derive(Default)]
pub struct Formula {
    nodes: Vec<Node>,
    atoms: usize,
}

impl Formula {
    /// A fresh atom's index. Atoms are numbered in the order their caller meets them, which is the order a diagram reads them in.
    pub fn new_atom(&mut self) -> usize {
        self.atoms += 1;
        self.atoms - 1
    }

    /// `node`, appended; its position, for a later node to name as an operand.
    pub fn push(&mut self, node: Node) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    /// The comparison of the formulas rooted at `left` and `right`: a machine its caller steps, spending each step's charge before the step is taken.
    pub fn agreement(&self, left: usize, right: usize) -> Agreement<'_> {
        Agreement {
            formula: self,
            left,
            right,
            nodes: Vec::new(),
            unique: HashMap::new(),
            computed: HashMap::new(),
            built: Vec::with_capacity(self.nodes.len()),
            frames: Vec::new(),
            results: Vec::new(),
            set_up: false,
        }
    }
}

/// A diagram node's handle; [`FALSE`] and [`TRUE`] are the two constants, and every other node is at its index plus two.
type Id = u32;

const FALSE: Id = 0;
const TRUE: Id = 1;

/// What storing a diagram node costs: its variable's level and its two branches.
const NODE: u64 = 3;

/// What remembering one application costs: the connective, its two operands and its result.
const MEMO: u64 = 4;

/// What one waiting frame of the machine's own stack costs.
const FRAME: u64 = 4;

/// The most nodes a diagram may hold before its handles would overflow: past it a step is charged as no budget affords.
const MOST: usize = (Id::MAX - 2) as usize;

/// A binary connective as a diagram applies it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Gate {
    And,
    Or,
    Xor,
    Eql,
}

impl Gate {
    /// The connective at `u` and `v` where an identity gives it without reading a variable: an operand that is a constant, or one diagram twice, which between them cover every pair of constants.
    fn shortcut(self, u: Id, v: Id) -> Option<Id> {
        match self {
            Gate::And if u == FALSE || v == FALSE => Some(FALSE),
            Gate::And if u == TRUE => Some(v),
            Gate::And if v == TRUE || u == v => Some(u),
            Gate::Or if u == TRUE || v == TRUE => Some(TRUE),
            Gate::Or if u == FALSE => Some(v),
            Gate::Or if v == FALSE || u == v => Some(u),
            Gate::Xor if u == v => Some(FALSE),
            Gate::Xor if u == FALSE => Some(v),
            Gate::Xor if v == FALSE => Some(u),
            Gate::Eql if u == v => Some(TRUE),
            Gate::Eql if u == TRUE => Some(v),
            Gate::Eql if v == TRUE => Some(u),
            _ => None,
        }
    }
}

/// What is left of one connective's application, innermost last.
enum Frame {
    /// Apply the connective to two diagrams.
    Enter(Gate, Id, Id),
    /// Join the two branches the frames above computed under the variable at a level, and remember the result as the connective applied to the two diagrams it split.
    Join(Gate, Id, Id, usize),
}

/// Two formulas compared by a reduced ordered decision diagram, one step at a time: each side's diagram is built over one unique table, in the order of the formula's postorder and with its atoms at the levels of their indices, and the two agree where their roots are one node. A formula node met again is the diagram it was built into, so what is built is the formula's graph and never its tree.
pub struct Agreement<'a> {
    formula: &'a Formula,
    left: usize,
    right: usize,
    /// Each node past the two constants: its variable's level, its low branch and its high branch.
    nodes: Vec<(usize, Id, Id)>,
    unique: HashMap<(usize, Id, Id), Id>,
    /// Each application made, by its connective and its two operands in order of their handles, every connective here being commutative.
    computed: HashMap<(Gate, Id, Id), Id>,
    /// The diagram of each formula node built so far.
    built: Vec<Id>,
    frames: Vec<Frame>,
    /// What finished frames computed, for the join below them.
    results: Vec<Id>,
    /// Whether what the comparison keeps whatever it builds was charged: a diagram a formula node, and a stack two frames deep a level.
    set_up: bool,
}

impl Agreement<'_> {
    /// Takes one step, `charge`d its units before it stores anything, and answers once the two diagrams stand: `Some(true)` where the formulas agree at every assignment of their atoms, `Some(false)` where they do not, which is undecided and never a disequality. A failed charge is handed back with nothing taken.
    pub fn step<E>(
        &mut self,
        charge: impl FnOnce(u64) -> Result<(), E>,
    ) -> Result<Option<bool>, E> {
        if !self.set_up {
            let levels = self.formula.atoms as u64;
            charge(
                (self.formula.nodes.len() as u64).saturating_add(
                    FRAME.saturating_mul(levels.saturating_mul(2).saturating_add(3)),
                ),
            )?;
            self.set_up = true;
            return Ok(None);
        }
        match self.frames.pop() {
            Some(Frame::Enter(gate, u, v)) => self.enter(gate, u, v, charge)?,
            Some(Frame::Join(gate, u, v, level)) => {
                let high = self.results.pop().expect("a join waits on its high branch");
                let low = self.results.pop().expect("a join waits on its low branch");
                let joined = self.make(level, low, high, MEMO, charge)?;
                self.computed.insert((gate, u, v), joined);
                self.finish(joined);
            }
            None => {
                let position = self.built.len();
                let Some(&node) = self.formula.nodes.get(position) else {
                    return Ok(Some(self.built[self.left] == self.built[self.right]));
                };
                match node {
                    Node::Literal(value) => {
                        charge(1)?;
                        self.built.push(match value {
                            true => TRUE,
                            false => FALSE,
                        });
                    }
                    Node::Atom { index, negated } => {
                        let (low, high) = match negated {
                            true => (TRUE, FALSE),
                            false => (FALSE, TRUE),
                        };
                        let atom = self.make(index, low, high, 0, charge)?;
                        self.built.push(atom);
                    }
                    Node::And(left, right) => self.start(Gate::And, left, right, charge)?,
                    Node::Or(left, right) => self.start(Gate::Or, left, right, charge)?,
                    Node::Xor(left, right) => self.start(Gate::Xor, left, right, charge)?,
                    Node::Eql(left, right) => self.start(Gate::Eql, left, right, charge)?,
                }
            }
        }
        Ok(None)
    }

    /// A connective's application to the diagrams two earlier formula nodes were built into, as its first step.
    fn start<E>(
        &mut self,
        gate: Gate,
        left: usize,
        right: usize,
        charge: impl FnOnce(u64) -> Result<(), E>,
    ) -> Result<(), E> {
        self.enter(gate, self.built[left], self.built[right], charge)
    }

    /// One step of an application: an identity or one made already answers it, and otherwise both diagrams are split at the higher of their top variables, the two halves applied in turn and joined.
    fn enter<E>(
        &mut self,
        gate: Gate,
        u: Id,
        v: Id,
        charge: impl FnOnce(u64) -> Result<(), E>,
    ) -> Result<(), E> {
        charge(1)?;
        let (u, v) = (u.min(v), u.max(v));
        if let Some(answer) = gate
            .shortcut(u, v)
            .or_else(|| self.computed.get(&(gate, u, v)).copied())
        {
            self.finish(answer);
            return Ok(());
        }
        let level = self.level(u).min(self.level(v));
        let (u_low, u_high) = self.branches(u, level);
        let (v_low, v_high) = self.branches(v, level);
        self.frames.push(Frame::Join(gate, u, v, level));
        self.frames.push(Frame::Enter(gate, u_high, v_high));
        self.frames.push(Frame::Enter(gate, u_low, v_low));
        Ok(())
    }

    /// The node at `level` over `low` and `high`, reduced: one branch where both are one, the node already stored where there is one, and otherwise a node stored. Charged one step, `kept` units more for what the caller stores beside it, and a node's size where one is stored.
    fn make<E>(
        &mut self,
        level: usize,
        low: Id,
        high: Id,
        kept: u64,
        charge: impl FnOnce(u64) -> Result<(), E>,
    ) -> Result<Id, E> {
        if low == high {
            charge(1 + kept)?;
            return Ok(low);
        }
        if let Some(&node) = self.unique.get(&(level, low, high)) {
            charge(1 + kept)?;
            return Ok(node);
        }
        // A handle past the last a node can have is a node no budget affords, so it is charged as one: the caller refuses it before anything is stored.
        charge(match self.nodes.len() < MOST {
            true => 1 + kept + NODE,
            false => u64::MAX,
        })?;
        let node = Id::try_from(self.nodes.len() + 2)
            .expect("a node past the last handle was charged as no budget affords");
        self.nodes.push((level, low, high));
        self.unique.insert((level, low, high), node);
        Ok(node)
    }

    /// Where an application's result goes: to the join waiting on it, or, where none waits, to the formula node it was made for.
    fn finish(&mut self, answer: Id) {
        match self.frames.is_empty() {
            true => self.built.push(answer),
            false => self.results.push(answer),
        }
    }

    /// The level of `node`'s variable; the constants stand below every variable.
    fn level(&self, node: Id) -> usize {
        match node {
            FALSE | TRUE => usize::MAX,
            _ => self.nodes[node as usize - 2].0,
        }
    }

    /// `node`'s two branches at `level`: its own where its variable stands there, and itself twice where it stands lower.
    fn branches(&self, node: Id, level: usize) -> (Id, Id) {
        match node {
            FALSE | TRUE => (node, node),
            _ => match self.nodes[node as usize - 2] {
                (at, low, high) if at == level => (low, high),
                _ => (node, node),
            },
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

#[cfg(test)]
mod test_support;
