//! What a source is named wherever it outlives the process that read it.

#[cfg(test)]
mod tests;

use {
    crate::is_identifier,
    std::{
        fmt,
        path::{Component, Path},
    },
};

/// The name a package's file goes by in everything stored, recorded or keyed beyond one process: the package's name, then the file's path below the package's root, `/std/Nat/le.crs`, `/app/src/main.crs`.
///
/// **No path of a machine.** It is built only from a package's name and a path whose every component is a plain name, so a root, a prefix, `.` and `..` cannot stand in one, and it is spelled as it is held, never read as a directory of whoever reads it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct SourceName {
    spelled: String,
}

impl SourceName {
    /// The name of the file at `path` below the root of the package `package`: `None` where `package` is no name a package takes, where `path` is empty, or where one of its components is anything but a plain name written in UTF-8.
    pub fn of(package: &str, path: &Path) -> Option<Self> {
        if !is_identifier(package) {
            return None;
        }

        let mut spelled = format!("/{package}");
        let mut components = path.components().peekable();
        components.peek()?;
        for component in components {
            let Component::Normal(component) = component else {
                return None;
            };
            spelled.push('/');
            spelled.push_str(component.to_str()?);
        }

        Some(Self { spelled })
    }

    /// The name as it is spelled.
    pub fn as_str(&self) -> &str {
        &self.spelled
    }
}

impl fmt::Display for SourceName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.spelled)
    }
}
