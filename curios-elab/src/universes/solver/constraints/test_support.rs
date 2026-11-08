//! What the suites read of the constraint store's speculation and no judgment does: how deep it stands and what its journal holds.

use super::*;

impl ConstraintStore {
    /// How many speculative scopes are open.
    pub fn speculation(&self) -> usize {
        self.speculation
    }

    /// How many pre-images the journal holds.
    pub fn journal_len(&self) -> usize {
        self.rewrites.len()
    }

    /// Each constraint the store holds, beside where it was raised.
    pub fn raised(&self) -> impl Iterator<Item = (&UniverseConstraint, &UniverseConstraintOrigin)> {
        self.raised
            .iter()
            .map(|raised| (&raised.constraint, &raised.origin))
    }
}
