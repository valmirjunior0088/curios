//! What a suite asks of the store's cache that no product does: a cache over a store of its own.

use super::*;

impl Verdicts {
    /// The store beside `root` and no other, whatever this machine shares ([`Store::alone`]).
    pub fn alone(root: PathBuf) -> Self {
        Self {
            store: Some(Store::alone(root)),
            ..Self::at(PathBuf::new())
        }
    }
}
