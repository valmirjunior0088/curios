//! Atoms and the monomials over them.

/// An opaque atom: a handle its caller hands out, one per value the caller's identity decides is one, with the rank the caller orders it by.
///
/// Two handles are two atoms. This crate never asks whether two of them might denote one value — that is the caller's identity, settled before a handle is handed over — so a collision in whatever the caller keys its handles by can cost it a merge, never manufacture one here.
///
/// **The rank orders; it never identifies.** A product's atoms are put in one order by rank, so `x · y` and `y · x` are one monomial, and two atoms of equal rank keep the order they were given — a caller that ranks by a hash therefore loses, on a collision, only that the two products meet, never that the two atoms are two.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Atom {
    index: u32,
    rank: u64,
}

impl Atom {
    /// The atom with handle `index`, ordered by `rank`.
    pub fn new(index: u32, rank: u64) -> Self {
        Atom { index, rank }
    }

    /// The handle this atom was made from.
    pub fn index(self) -> u32 {
        self.index
    }

    /// What this atom is ordered by.
    pub fn rank(self) -> u64 {
        self.rank
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
        atoms.sort_by_key(|atom| atom.rank);
        Monomial(atoms)
    }
}
