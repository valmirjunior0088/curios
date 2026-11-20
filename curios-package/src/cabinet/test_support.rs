//! What a suite asks of a cabinet that no product does: the directory a family stands in, for a test that lists, damages or blocks what is filed there, computed as the cabinet computes the paths inside it.

use super::*;

impl Cabinet {
    /// The directory every unit's slot is filed in.
    pub fn verdicts(&self) -> PathBuf {
        self.directory.join(VERDICTS)
    }

    /// The [`verdict`](Self::verdict) of every slot filed in the cabinet, in no order: what a test reads, damages or writes again of a slot. None where nothing is filed.
    pub fn verdict_files(&self) -> Vec<PathBuf> {
        let Ok(slots) = std::fs::read_dir(self.verdicts()) else {
            return Vec::new();
        };

        slots
            .map(|slot| slot.expect("a listed slot").path().join(VERDICT))
            .collect()
    }

    /// The directory `target`'s payloads are filed in.
    pub fn payloads(&self, target: Target) -> PathBuf {
        self.directory.join(PAYLOADS).join(target.spelled())
    }
}
