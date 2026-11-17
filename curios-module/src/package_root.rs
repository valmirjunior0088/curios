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

    /// What the file at `file` is named in this package: its path below the package's directory. `None` for a file outside the directory.
    pub fn name_of(&self, file: &Path) -> Option<SourceName> {
        SourceName::of(&self.name, file.strip_prefix(&self.directory).ok()?)
    }

    /// The file `name` names on this machine: its path below the package's directory, where `name` is one of this package's; `None` for a name of another package, or of no package.
    pub fn file_of(&self, name: &SourceName) -> Option<PathBuf> {
        let (package, path) = name.in_package()?;

        (package == self.name).then(|| self.directory.join(path))
    }
}
