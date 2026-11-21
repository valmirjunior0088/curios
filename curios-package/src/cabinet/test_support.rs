//! What a suite asks of a cabinet that no product does: the directory a family stands in, for a test that lists, damages or blocks what is filed there, computed as the cabinet computes the paths inside it.

use super::*;

impl Cabinet {
    /// The directory every unit's object is kept in.
    pub fn objects(&self) -> PathBuf {
        self.directory.join(OBJECTS)
    }

    /// The [`unit`](Self::unit) file of every object kept in the cabinet, in no order: what a test reads, damages or writes again of an object. None where nothing is filed.
    pub fn unit_files(&self) -> Vec<PathBuf> {
        let Ok(objects) = std::fs::read_dir(self.objects()) else {
            return Vec::new();
        };

        objects
            .map(|object| object.expect("a listed object").path().join(UNIT))
            .collect()
    }

    /// The directory `target`'s payloads are filed in.
    pub fn payloads(&self, target: Target) -> PathBuf {
        self.directory.join(PAYLOADS).join(target.spelled())
    }
}
