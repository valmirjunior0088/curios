//! The atoms of one algebraic operation: the terms `curios-algebra` reasons over, each handed to it as a handle.
//!
//! **Atom identity is the carrier's, and it is stated here, once.** `curios-algebra` never asks whether two handles might denote one value, so what "the same atom" means at a carrier is exactly which terms [`same_atom`] holds one: the terms a table maps to one handle ([`Atoms::handle`]), and the operands a reader that keeps no table compares directly. The rank each handle carries is the order it is put in.
//!
//! **An atom of a carrier whose values hold no type is the same up to universe instances** ([`Carrier::holds_types`]). Two occurrences of one polymorphic name are instantiated independently, and Core offers no elimination from a type or a level into a number, a truth value, a byte, a float or a packed sequence of them, so a level is never part of the answer to "are these the same value": `len(xs)` written twice is one atom, which is what lets a bound over it cancel against itself, and `even(t)` written twice is one leaf of a conjunction. The projection is those carriers' licence and nothing wider: it does not make terms equal in general, it is not how a refinement is keyed, and an element of a list is known by its term as written, a list holding values of any type.
//!
//! **A product's factors are atoms as written** ([`Atoms::exact`]), ranked by their structural hash: distribution builds each product's spine from its factors in that order, so two spellings of one factor must stay two factors or the rebuilt spine would change which one it spells. The sum those products are merged into then keys them by the carrier, as every sum is.
//!
//! **A collision never merges.** Both identities key on a term compared by equality, so two terms whose structural hashes collide are told apart: a collision costs a probe, and at most the order two atoms of one rank are put in.
//!
//! A table lives as long as the operation that made it, and its handles mean nothing outside that operation.

#[cfg(test)]
mod tests;

use {
    super::Term,
    curios_algebra::{Atom, Carrier},
    std::collections::HashMap,
};

/// Whether `this` and `that` are one atom of `carrier`: one term, or, where the carrier's values hold no type, one term up to universe instances.
pub fn same_atom(carrier: Carrier, this: &Term, that: &Term) -> bool {
    this == that || (!carrier.holds_types() && this.erased_universes() == that.erased_universes())
}

/// The handles one operation has handed out, keyed by the carrier's identity, with the first term each stood for.
#[derive(Default)]
pub(crate) struct Atoms {
    index: HashMap<Term, Atom>,
    terms: Vec<Term>,
}

impl Atoms {
    /// The handle for `term` as an atom of `carrier`: one handle for the terms [`same_atom`] holds one, ranked by the structural hash of what they are known by, so the rank is as blind to instances as the identity is.
    pub(crate) fn handle(&mut self, carrier: Carrier, term: &Term) -> Atom {
        match carrier.holds_types() {
            true => self.keyed(term.clone(), term),
            false => self.keyed(term.erased_universes(), term),
        }
    }

    /// The handle for `term` as written: one handle per term, ranked by its own structural hash.
    pub(crate) fn exact(&mut self, term: &Term) -> Atom {
        self.keyed(term.clone(), term)
    }

    /// The first term `atom` was handed out for.
    pub(crate) fn term(&self, atom: Atom) -> &Term {
        &self.terms[atom.index() as usize]
    }

    fn keyed(&mut self, key: Term, term: &Term) -> Atom {
        if let Some(&atom) = self.index.get(&key) {
            return atom;
        }
        let atom = Atom::new(self.terms.len() as u32, key.structural_hash());
        self.index.insert(key, atom);
        self.terms.push(term.clone());
        atom
    }
}
