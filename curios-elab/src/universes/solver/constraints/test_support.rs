//! What the suites read of the constraint store's speculation and no judgment does: how deep it stands and what its journal holds.

use super::ConstraintStore;

impl ConstraintStore {
    /// How many speculative scopes are open.
    pub fn speculation(&self) -> usize {
        self.speculation
    }

    /// How many pre-images the journal holds.
    pub fn journal_len(&self) -> usize {
        self.rewrites.len()
    }
}
