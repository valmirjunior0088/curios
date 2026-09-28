//! Atoms and the monomials over them.

/// An opaque atom: a handle its caller hands out, one per value the caller's identity decides is one.
///
/// Two handles are two atoms. This crate never asks whether two of them might denote one value — that is the caller's identity, settled before a handle is handed over — so a collision in whatever the caller keys its handles by can cost it a merge, never manufacture one here.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Atom(u32);

impl Atom {
    /// The atom with handle `index`.
    pub fn new(index: u32) -> Self {
        Atom(index)
    }

    /// The handle this atom was made from.
    pub fn index(self) -> u32 {
        self.0
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
}
