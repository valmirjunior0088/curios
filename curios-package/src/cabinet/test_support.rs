//! What a suite asks of a cabinet that no product does: the directory a family stands in, for a test that lists, damages or blocks what is filed there, computed as the cabinet computes the paths inside it.

use {super::*, std::collections::BTreeSet};

impl Cabinet {
    /// The directory every object is kept in, under the compiler that filed it: the family's.
    pub fn objects(&self) -> PathBuf {
        self.directory.join(OBJECTS)
    }

    /// Every object kept in the cabinet, each by its directory, whichever compiler filed it, in no order. None where nothing is filed.
    pub fn object_directories(&self) -> Vec<PathBuf> {
        let Ok(compilers) = std::fs::read_dir(self.objects()) else {
            return Vec::new();
        };

        compilers
            .filter_map(|compiler| {
                std::fs::read_dir(compiler.expect("a listed compiler").path()).ok()
            })
            .flatten()
            .map(|object| object.expect("a listed object").path())
            .collect()
    }

    /// The address of every unit's object kept in the cabinet, whichever compiler filed it.
    pub fn addresses(&self) -> BTreeSet<String> {
        self.unit_files()
            .iter()
            .filter_map(|file| address(file))
            .collect()
    }

    /// The payload for `target` of every executable's object kept in the cabinet, each by its file, whichever compiler filed it, in no order.
    pub fn payload_files(&self, target: Target) -> Vec<PathBuf> {
        self.object_directories()
            .into_iter()
            .map(|object| object.join(target.spelled()))
            .filter(|payload| payload.is_file())
            .collect()
    }

    /// The address of every object kept in the cabinet that holds a payload for `target`.
    pub fn payload_addresses(&self, target: Target) -> BTreeSet<String> {
        self.payload_files(target)
            .iter()
            .filter_map(|file| address(file))
            .collect()
    }

    /// The [`unit`](Self::unit) file of every object kept in the cabinet, in no order: what a test reads, damages or writes again of an object. None where nothing is filed.
    pub fn unit_files(&self) -> Vec<PathBuf> {
        self.object_directories()
            .into_iter()
            .map(|object| object.join(UNIT))
            .filter(|unit| unit.is_file())
            .collect()
    }
}

/// The address of the object the file at `path` stands in: its directory's name.
fn address(path: &Path) -> Option<String> {
    Some(path.parent()?.file_name()?.to_string_lossy().into_owned())
}
