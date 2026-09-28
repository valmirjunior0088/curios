//! The atoms of one algebraic operation: the terms `curios-algebra` reasons over, each handed to it as a handle.
//!
//! **Atom identity is the carrier's, and it is decided here, once per operation.** `curios-algebra` never asks whether two handles might denote one value, so what "the same atom" means for a carrier is exactly which terms this table maps to one handle, and the rank each handle carries is the order it is put in.
//!
//! **`Nat` and `Int` atoms are the same up to universe instances** ([`Atoms::numeric`]). Two occurrences of one polymorphic name are instantiated independently, and Core offers no elimination from a type or a level into a number, so a level is never part of the answer to "are these the same number": `len(xs)` written twice is one atom, which is what lets a bound over it cancel against itself. The projection is the carrier's licence and nothing wider — it does not make terms equal in general, and it is not how a refinement is keyed.
//!
//! **A product's factors are atoms as written** ([`Atoms::exact`]), ranked by their structural hash: distribution builds each product's spine from its factors in that order, so two spellings of one factor must stay two factors or the rebuilt spine would change which one it spells. The sum those products are merged into then keys them numerically, as every sum is.
//!
//! **A collision never merges.** Both identities key on a term compared by equality, so two terms whose structural hashes collide are told apart: a collision costs a probe, and at most the order two atoms of one rank are put in.
//!
//! A table lives as long as the operation that made it, and its handles mean nothing outside that operation.

use {
    super::{Term, project_erased_universes},
    curios_algebra::Atom,
    std::collections::HashMap,
};

/// The handles one operation has handed out, keyed by the carrier's identity, with the first term each stood for.
#[derive(Default)]
pub(crate) struct Atoms {
    index: HashMap<Term, Atom>,
    terms: Vec<Term>,
}

impl Atoms {
    /// The handle for `term` as a `Nat` or `Int` atom: one handle per term up to universe instances, ranked by the projected term's structural hash, so the rank is as blind to instances as the identity is.
    pub(crate) fn numeric(&mut self, term: &Term) -> Atom {
        self.handle(project_erased_universes(term), term)
    }

    /// The handle for `term` as written: one handle per term, ranked by its own structural hash.
    pub(crate) fn exact(&mut self, term: &Term) -> Atom {
        self.handle(term.clone(), term)
    }

    /// The first term `atom` was handed out for.
    pub(crate) fn term(&self, atom: Atom) -> &Term {
        &self.terms[atom.index() as usize]
    }

    fn handle(&mut self, key: Term, term: &Term) -> Atom {
        if let Some(&atom) = self.index.get(&key) {
            return atom;
        }
        let atom = Atom::new(self.terms.len() as u32, key.structural_hash());
        self.index.insert(key, atom);
        self.terms.push(term.clone());
        atom
    }
}
