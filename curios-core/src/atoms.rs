//! The atoms of one algebraic operation: the terms `curios-algebra` reasons over, each handed to it as a handle.
//!
//! **Atom identity is the carrier's, and it is decided here, once per operation.** `curios-algebra` never asks whether two handles might denote one value, so what "the same atom" means for a carrier is exactly which terms this table maps to one handle.
//!
//! **`Nat` and `Int` atoms are the same up to universe instances.** Two occurrences of one polymorphic name are instantiated independently, and Core offers no elimination from a type or a level into a number, so a level is never part of the answer to "are these the same number": `len(xs)` written twice is one atom, which is what lets a bound over it cancel against itself. The projection is the carrier's licence and nothing wider — it does not make terms equal in general, and it is not how a refinement is keyed. The key is the projected term itself, so two terms whose structural hashes collide are told apart by equality: a collision costs a probe, never a merge.
//!
//! A table lives as long as the operation that made it, and its handles mean nothing outside that operation.

use {
    super::{Term, project_erased_universes},
    curios_algebra::Atom,
    std::collections::HashMap,
};

/// The handles one operation has handed out, keyed by the carrier's identity.
#[derive(Default)]
pub(crate) struct Atoms {
    index: HashMap<Term, Atom>,
}

impl Atoms {
    /// The handle for `term` as a `Nat` or `Int` atom: one handle per term up to universe instances.
    pub(crate) fn numeric(&mut self, term: &Term) -> Atom {
        let key = project_erased_universes(term);
        let next = Atom::new(self.index.len() as u32);
        *self.index.entry(key).or_insert(next)
    }
}
