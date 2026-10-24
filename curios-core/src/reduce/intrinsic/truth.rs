//! Deciding two `Bool` terms equal by a decision diagram over their atoms.
//!
//! The fold beside each connective takes the laws one node can see — a unit, an absorber, a repeated operand, an operand beside its own negation — and the peel compares two `&&` trees, or two `||` trees, as sets of leaves. Neither reaches a law that relates *different* connectives: De Morgan, absorption, distribution. Those have no local statement, and a normal form that would make them structural is a rewriting one, refused twice over — a guard's refinement is keyed on its written spelling (`documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md`), and a fold that normalizes a tree whole pays for the whole tree at every leaf.
//!
//! So the decision is made where two terms are *compared*, asked for by name in both converters as [`super::normalize_bool`] and [`super::align_comparisons`] are, and it changes no spelling. Both sides are read as formulas over their atoms — every leaf that is not itself a connective or a literal, forced first, since the fold leaves a stuck connective's right operand as written — and compared by a reduced ordered decision diagram over those atoms (`curios-algebra`'s `Agreement`), which agrees exactly where the two agree at every assignment of them. A leaf is known as `Bool` knows its atoms (`crate::atoms`): a comparison by what it states, so `x + 1 <= y` is the atom `x < y` is, and `y < x + 1` that atom negated.
//!
//! **Agreement everywhere is equality; anything else is nothing.** Treating the atoms as independent *over*-approximates the values they can take together: `x < y` and `x < y + 1` are two atoms that are not independent at all, and the diagram still reads every combination the real values can reach, so two formulas that agree at every assignment agree at the real ones. For the same reason a disagreement proves nothing — the assignment it happens at may be one no value reaches — so this never answers a disequality, and inversion is handed no impossibility.
//!
//! **A term is read once.** The two sides are graphs, a reduct naming one node many times over, so the read remembers where each term it has read stands and a term met again is that node: what is forced, read, built and charged is the graph, never its tree (`documentation/design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md`). The memory changes no answer, since a term read again adds no atom and stands for the value it stood for.
//!
//! **The price is the theory's, paid a step at a time.** No pair is declined: the diagram reports each step's charge before it takes it and the reducer spends it, so a pair the budget cannot finish exhausts the budget and is refused as every exhaustion is, and a larger budget decides it. A cap on the atoms a pair holds would break transitivity and closure under substitution at any value it took. The charge is a function of the pair as read, its graph and the order its atoms were met in, so the same two terms cost the same on every target.

use {
    crate::{Atoms, Cost, Intrinsic, Probe, ReduceError, Reducer, Subterm, Term},
    curios_algebra::{Formula, Node},
    curios_utilities::recurse,
    std::collections::HashMap,
};

/// Two formulas being read, over one shared list of atoms: the handles the leaves were known by, how many of them the formula holds, the formula `curios-algebra` decides, and where each term read so far stands in it.
#[derive(Default)]
struct Table {
    atoms: Atoms,
    read: usize,
    formula: Formula,
    /// The position of every term read, as written and as forced. A term met again is the node it was read into, so the formula is its terms' graph and never their tree: a shared operand is forced once, read once and charged once.
    positions: HashMap<Term, usize>,
}

impl Table {
    /// Read `term` into the table and answer its root's position. Every node is forced before it is read, which is what makes a leaf the value's rather than the spelling's — a [`Probe`], so a node with no value at the type level is read as written, an atom like any other.
    fn read(&mut self, reducer: &mut impl Reducer, term: Term) -> Result<usize, ReduceError> {
        if let Some(&position) = self.positions.get(&term) {
            return Ok(position);
        }

        recurse(|| {
            let forced = reducer
                .reduce_forced(term.clone())
                .probed()?
                .unwrap_or_else(|| term.clone());
            if let Some(&position) = self.positions.get(&forced) {
                self.positions.insert(term, position);
                return Ok(position);
            }

            let node = match &*forced {
                Subterm::Intrinsic(Intrinsic::Bool(value)) => Node::Literal(*value),
                Subterm::Intrinsic(
                    connective @ (Intrinsic::BoolAnd(left, right)
                    | Intrinsic::BoolOr(left, right)
                    | Intrinsic::BoolXor(left, right)
                    | Intrinsic::BoolEql(left, right)
                    | Intrinsic::BoolNeq(left, right)),
                ) => {
                    let left = self.read(reducer, left.clone())?;
                    let right = self.read(reducer, right.clone())?;
                    match connective {
                        Intrinsic::BoolAnd(..) => Node::And(left, right),
                        Intrinsic::BoolOr(..) => Node::Or(left, right),
                        Intrinsic::BoolEql(..) => Node::Eql(left, right),
                        // `!=` on `Bool` is `xor`, which is how `/sys` lowers it.
                        _ => Node::Xor(left, right),
                    }
                }
                _ => self.atom(&forced),
            };
            let position = self.formula.push(node);
            self.positions.insert(term, position);
            self.positions.insert(forced, position);
            Ok(position)
        })
    }

    /// The atom `leaf` is, at the polarity it is read at: a `Bool` atom is known as its carrier's atoms are ([`Atoms::signed`]), so two calls apart in a universe instance are one atom and a comparison is known by what it states, its negation however spelled being the same atom negated. A handle met for the first time is a new atom of the formula, numbered in the order the read meets it, which is the order the diagram decides in.
    fn atom(&mut self, leaf: &Term) -> Node {
        let (atom, negated) = self.atoms.signed(leaf);
        let index = atom.index() as usize;
        if index == self.read {
            self.formula.new_atom();
            self.read += 1;
        }
        Node::Atom { index, negated }
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

/// Whether `this` and `that` are one `Bool` at every assignment of their atoms: `true` is a definitional equality, `false` is *undecided* and never a disequality — see the module documentation for why both halves of that are forced. Declines at once unless a side is headed by a connective, so a pair this has nothing to say about costs two shape tests; past that it declines nothing, and a pair its budget cannot finish exhausts it.
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
    let left = table.read(reducer, this.clone())?;
    let right = table.read(reducer, that.clone())?;

    // Each step of the diagram, charged before it is taken: a unit a step and what it stores, in the units the budget counts.
    let mut agreement = table.formula.agreement(left, right);
    loop {
        if let Some(agree) = agreement.step(|units| reducer.spend(Cost::decision(units)))? {
            return Ok(agree);
        }
    }
}
