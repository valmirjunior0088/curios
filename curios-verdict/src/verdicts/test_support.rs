//! What a suite asks of the store's cache that no product does: a cache over a store of its own.

use super::*;

impl Verdicts {
    /// The cabinet beside `root` and no other, whatever this machine shares ([`Cabinet::alone`]).
    pub fn alone(root: PathBuf) -> Self {
        Self {
            cabinet: Some(Cabinet::alone(&root)),
            ..Self::at(PathBuf::new())
        }
    }
}
