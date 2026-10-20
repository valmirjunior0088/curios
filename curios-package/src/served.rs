//! What a `[foreign.browser.static]` table serves, read against the disk: every file of each entry, by the page path it is served at.
//!
//! **An entry is a file or a directory of the package's own.** A file is served at its key, and a directory as every file under it, at its key joined with the file's path inside, so a page lays out a directory as the package holds it. Inside a directory a file or directory whose name starts with `.` is skipped — a `.git`, a `.DS_Store`, an editor's swap file — which a person serving a directory does not mean to publish.
//!
//! **Refused rather than followed: a symlink, wherever it stands.** A page serves the package's own files, and a link may name anything on the machine; the tree hash refuses a symlink in a delivered tree for the same reason. An entry whose path names nothing is refused by the entry, and so is a name inside a directory that no page path can spell.

#[cfg(test)]
mod tests;

use {
    crate::Gateway,
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

/// One file a `static` entry serves.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Served {
    /// The entry's page path, which names it.
    pub entry: String,
    /// Where the page serves the file, from the package's root.
    pub page: String,
    /// Where the file is read.
    pub file: PathBuf,
}

impl Gateway {
    /// Every file this gateway's `static` table serves, read against `directory`, its package's own, for the foreign row `row`: in page-path order within each entry, the entries in theirs.
    pub fn served(&self, row: &str, directory: &Path) -> Result<Vec<Served>, String> {
        let mut served = Vec::new();

        for (entry, path) in &self.statics {
            let subject = format!("the static entry {entry:?} of the foreign row {row:?}");
            let read = linkless(directory, path, &subject)?;

            match fs::metadata(&read) {
                Ok(metadata) if metadata.is_dir() => {
                    walked(&read, entry, &subject, entry, &mut served)?;
                }
                Ok(metadata) if metadata.is_file() => served.push(Served {
                    entry: entry.clone(),
                    page: entry.clone(),
                    file: read,
                }),
                Ok(_) => {
                    return Err(format!(
                        "{subject} reads {}, which is neither a file nor a directory",
                        read.display()
                    ));
                }
                Err(error) => {
                    return Err(format!(
                        "{subject} reads {}, which holds nothing: {error}",
                        read.display()
                    ));
                }
            }
        }

        Ok(served)
    }
}

/// `path` under `directory`, refused for `subject` where it, or a directory on the way to it, is a symlink.
fn linkless(directory: &Path, path: &Path, subject: &str) -> Result<PathBuf, String> {
    let mut read = directory.to_path_buf();

    for component in path.components() {
        read.push(component);

        if fs::symlink_metadata(&read).is_ok_and(|metadata| metadata.file_type().is_symlink()) {
            return Err(linked(subject, &read));
        }
    }

    Ok(read)
}

/// The refusal of the symlink at `path`, which `subject` reaches.
fn linked(subject: &str, path: &Path) -> String {
    format!(
        "{subject} reaches {}, which is a symbolic link; a page serves the package's own files, and a link may name anything",
        path.display()
    )
}

/// Every file under `directory`, served at `page` joined with its path inside, onto `served` for the entry `entry`, which `subject` names.
fn walked(
    directory: &Path,
    page: &str,
    subject: &str,
    entry: &str,
    served: &mut Vec<Served>,
) -> Result<(), String> {
    let unreadable =
        |error: std::io::Error| format!("{subject} reads {}: {error}", directory.display());

    let mut children = fs::read_dir(directory)
        .map_err(unreadable)?
        .map(|child| child.map(|child| child.path()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(unreadable)?;
    children.sort();

    for child in children {
        let Some(name) = child.file_name().and_then(|name| name.to_str()) else {
            return Err(format!(
                "{subject} holds {}, whose name no page path can spell",
                child.display()
            ));
        };
        if name.starts_with('.') {
            continue;
        }

        let metadata = fs::symlink_metadata(&child)
            .map_err(|error| format!("{subject} reads {}: {error}", child.display()))?;
        let page = format!("{page}/{name}");

        if metadata.file_type().is_symlink() {
            return Err(linked(subject, &child));
        } else if metadata.is_dir() {
            walked(&child, &page, subject, entry, served)?;
        } else if metadata.is_file() {
            served.push(Served {
                entry: entry.to_string(),
                page,
                file: child,
            });
        }
    }

    Ok(())
}
