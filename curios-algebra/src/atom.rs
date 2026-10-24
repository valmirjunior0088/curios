//! Atoms and the monomials over them.

#[cfg(test)]
mod tests;

use std::{any::Any, cmp::Ordering, fmt, hash, rc::Rc};

/// What breaks a tie between two ranks of one key, in the caller's hands: the caller states what orders two of its atoms whose keys are one, and this crate asks it nothing else.
pub trait Tie: fmt::Debug {
    /// This against `other`, which the caller reads back as one of its own through [`Tie::as_any`]; `Equal` where it is none, which costs the order two atoms are put in and never a merge.
    fn tie(&self, other: &dyn Tie) -> Ordering;

    fn as_any(&self) -> &dyn Any;
}

/// What an atom is ordered by: a key compared first, and where two keys are one, a tie the caller breaks.
///
/// **A rank is compared, never read.** This crate puts atoms in one order by it and hashes it by its key, and asks nothing else of it, so a caller is free to state in it anything that orders — a hash of what an atom is known by, then whatever breaks that hash's ties — and nothing here can come to depend on what the key is beyond its order.
#[derive(Clone, Debug)]
pub enum Rank {
    /// One key, the rank of nearly every atom: held in place, since an atom is handed out at every reading.
    One(u64),
    /// A key beside what breaks a tie between two ranks of that key.
    Tied(u64, Rc<dyn Tie>),
}

impl Rank {
    /// The key compared first, and hashed.
    pub fn key(&self) -> u64 {
        match self {
            Rank::One(key) | Rank::Tied(key, _) => *key,
        }
    }
}

impl From<u64> for Rank {
    fn from(key: u64) -> Self {
        Rank::One(key)
    }
}

impl PartialEq for Rank {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Rank {}

impl PartialOrd for Rank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rank {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key()
            .cmp(&other.key())
            .then_with(|| match (self, other) {
                (Rank::Tied(_, this), Rank::Tied(_, that)) => this.tie(&**that),
                _ => Ordering::Equal,
            })
    }
}

/// By the key alone, so two ranks one order holds equal hash alike whatever their ties.
impl hash::Hash for Rank {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.key());
    }
}

/// An opaque atom: a handle its caller hands out, one per value the caller's identity decides is one, with the rank the caller orders it by.
///
/// Two handles are two atoms. This crate never asks whether two of them might denote one value — that is the caller's identity, settled before a handle is handed over — so a collision in whatever the caller keys its handles by can cost it a merge, never manufacture one here.
///
/// **The rank orders; it never identifies.** A product's atoms are put in one order by rank, so `x · y` and `y · x` are one monomial, and two atoms of equal rank keep the order they were given — a caller whose ranks collide therefore loses, on a collision, only that the two products meet, never that the two atoms are two.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Atom {
    index: u32,
    rank: Rank,
}

impl Atom {
    /// The atom with handle `index`, ordered by `rank`.
    pub fn new(index: u32, rank: impl Into<Rank>) -> Self {
        Atom {
            index,
            rank: rank.into(),
        }
    }

    /// The handle this atom was made from.
    pub fn index(&self) -> u32 {
        self.index
    }

    /// What this atom is ordered by.
    pub fn rank(&self) -> &Rank {
        &self.rank
    }
}

/// A product of atoms, in the order its caller gave them.
///
/// Two monomials are one when they hold the same atoms in the same order. A caller for which a product's factor order does not matter hands its factors over in one order; one for which a whole product is a single value hands it over as a single atom.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Monomial(Vec<Atom>);

impl Monomial {
    /// The product of `atoms`, in the order given.
    pub fn new(atoms: Vec<Atom>) -> Self {
        Monomial(atoms)
    }

    /// The atoms this monomial is the product of.
    pub fn atoms(&self) -> &[Atom] {
        &self.0
    }

    /// The product of `atoms` in canonical order: by rank, atoms of equal rank keeping the order given.
    pub fn product(mut atoms: Vec<Atom>) -> Self {
        atoms.sort_by(|left, right| left.rank.cmp(&right.rank));
        Monomial(atoms)
    }
}
