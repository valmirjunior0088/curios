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

/// What a program read from standard input is called: on the line that names a tree's subject, and, as its source's name, in the place any diagnostic about it closes on, `<stdin>:2:1`. One constant, so a reader is never told two names for one program. Angle brackets because no file is spelled that way, so neither reads as naming something openable.
pub const STDIN_NAME: &str = "<stdin>";

/// The name a package's file goes by in everything stored, recorded or keyed beyond one process: the package's name, then the file's path below the package's root, `/std/Nat/le.crs`, `/app/src/main.crs`; and [`STDIN_NAME`] for the text read from standard input, which no package holds.
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

    /// The name of the text read from standard input.
    pub fn stdin() -> Self {
        Self {
            spelled: STDIN_NAME.to_string(),
        }
    }

    /// The package the name is in and the file's path below the package's root, `("std", "Nat/le.crs")`; `None` for the text read from standard input.
    pub fn in_package(&self) -> Option<(&str, &str)> {
        self.spelled.strip_prefix('/')?.split_once('/')
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
