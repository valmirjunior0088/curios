//! Atoms and the monomials over them.

use std::{cmp::Ordering, hash, rc::Rc};

/// What an atom is ordered by: numbers compared in turn, the first the caller's coarse key and each after it what breaks a tie the ones before it leave.
///
/// **A rank is compared, never read.** This crate puts atoms in one order by it and asks nothing else of it, so a caller is free to state in it anything that orders — a hash of what an atom is known by, then whatever breaks that hash's ties — and nothing here can come to depend on what the numbers are beyond their order.
#[derive(Clone, Debug)]
pub enum Rank {
    /// One key, the rank of nearly every atom: held in place, since an atom is handed out at every reading.
    One(u64),
    /// Several keys, in the order they are compared.
    Many(Rc<[u64]>),
}

impl Rank {
    /// The rank whose keys are `keys`, in the order they are compared.
    pub fn of(keys: &[u64]) -> Self {
        match keys {
            [key] => Rank::One(*key),
            _ => Rank::Many(Rc::from(keys)),
        }
    }

    /// The keys, in the order they are compared.
    pub fn keys(&self) -> &[u64] {
        match self {
            Rank::One(key) => std::slice::from_ref(key),
            Rank::Many(keys) => keys,
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
        self.keys() == other.keys()
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
        self.keys().cmp(other.keys())
    }
}

impl hash::Hash for Rank {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.keys().hash(state);
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
