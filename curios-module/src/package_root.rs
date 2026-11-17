//! Where a package stands on this machine, and the one statement of how its files are named.

use {
    curios_common::SourceName,
    std::path::{Path, PathBuf},
};

/// A package's name and the directory its manifest sits in, which every file of the package lies below: what a unit's source names each file it reads by.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageRoot {
    pub name: String,
    pub directory: PathBuf,
}

impl PackageRoot {
    /// The package named `name` whose manifest sits in `directory`.
    pub fn new(name: impl Into<String>, directory: impl Into<PathBuf>) -> Self {
        Self {
            name: name.into(),
            directory: directory.into(),
        }
    }

    /// What the file at `file` is named in this package: its path below the package's directory, spelled as it is given before any link is followed, so a file reached through a link keeps the name its package gives it. `None` for a file outside the directory.
    pub fn name_of(&self, file: &Path) -> Option<SourceName> {
        SourceName::of(&self.name, file.strip_prefix(&self.directory).ok()?)
    }
}
