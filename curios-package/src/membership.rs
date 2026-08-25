//! What a file is *part of*, for a question that executes nothing.
//!
//! `run` reads a file argument as standalone everywhere, and deliberately: for it, form and scope are one decision, and a file compiled with its project's library would be a capability escalated by supplying context. A question is different. Nothing is executed, so nothing is escalated; what is at stake is only whether the answer is true, and a library module analysed without its library reports every import unresolved. So a file asked about is placed in the unit that declares it — the library whose namespace directory holds it, or the executable whose entry or stem directory it is — and is standalone only when no unit does.
//!
//! **The project is decided from the file's own location, never from the working directory.** An editor asks about documents all over a tree; the manifest that governs a file is the nearest one above it, and `--manifest` overrides that as it overrides everything else.

use {
    crate::{Governing, LIBRARY, MANIFEST, order},
    curios_text::{RootSource, identity},
    std::path::{Path, PathBuf},
};

/// The unit a file belongs to.
pub enum Membership {
    /// No package declares it: compiled alone, against nothing but the prelude.
    Standalone,
    /// The library of the package whose directory holds it. `units` is the whole scope in dependency order, that library last, so asking about the last unit is asking about the file.
    Library {
        root: PathBuf,
        units: Vec<RootSource>,
    },
    /// An executable's entry, or a module under its stem directory: `entry` compiled against `units`.
    Executable {
        name: String,
        entry: PathBuf,
        root: PathBuf,
        units: Vec<RootSource>,
    },
}

impl Membership {
    /// What `file` is part of, under the nearest manifest above it or the one `manifest` names.
    pub fn of(file: &Path, manifest: Option<&Path>) -> Result<Self, String> {
        let file = identity(file);

        let governing = match manifest {
            Some(manifest) => Governing::at(manifest)?,
            None => match nearest_manifest(&file) {
                Some(directory) => Governing::of(&directory)?,
                None => return Ok(Self::Standalone),
            },
        };

        let directory = identity(&governing.directory);
        if !file.starts_with(&directory) {
            return Ok(Self::Standalone);
        }

        // An executable's own entry, or a module under its stem directory — a row always wins over the library's namespace, exactly as the layout rule gives a header's stem directory to that header.
        for executable in &governing.package.executables {
            let entry = identity(&governing.directory.join(&executable.path));
            let stem = entry.with_extension("");
            if file == entry || file.starts_with(&stem) {
                return Ok(Self::Executable {
                    name: executable.name.clone(),
                    entry,
                    root: governing.root.clone(),
                    units: order(&governing)?,
                });
            }
        }

        // Everything else under the package directory is the library's namespace — when there is a library to have one.
        if !governing.directory.join(LIBRARY).is_file() {
            return Ok(Self::Standalone);
        }

        Ok(Self::Library {
            root: governing.root.clone(),
            units: order(&governing)?,
        })
    }
}

/// The directory of the nearest manifest at or above `file`'s own.
fn nearest_manifest(file: &Path) -> Option<PathBuf> {
    file.ancestors()
        .skip(1)
        .find(|directory| directory.join(MANIFEST).is_file())
        .map(Path::to_path_buf)
}
