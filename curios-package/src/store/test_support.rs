//! What a suite asks of a store that no product does: a store of its own, whatever the machine offers.

use super::*;

impl Store {
    /// The store a project at `project` uses, sharing nothing this machine offers whether or not `CURIOS_CACHE` names a store: what a history of the soundness board files its units in and rewrites a record of, which must never reach a store a build reads.
    pub fn alone(project: PathBuf) -> Self {
        Self {
            shared: project.join(STORE),
            project,
        }
    }
}
