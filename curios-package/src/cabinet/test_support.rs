//! What a suite asks of a cabinet that no product does: the directory a family stands in, for a test that lists, damages or blocks what is filed there, computed as the cabinet computes the paths inside it.

use super::*;

impl Cabinet {
    /// The directory every unit's slot is filed in.
    pub fn verdicts(&self) -> PathBuf {
        self.directory.join(VERDICTS)
    }

    /// The directory the packs of every slot's address stand in.
    pub fn packs(&self) -> PathBuf {
        self.directory.join(OBJECTS)
    }

    /// The directory `target`'s payloads are filed in.
    pub fn payloads(&self, target: Target) -> PathBuf {
        self.directory.join(PAYLOADS).join(target.spelled())
    }
}
