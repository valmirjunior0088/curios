//! Which manifest governs an invocation.
//!
//! The walk goes upward from the working directory to the nearest package manifest, and then upward again for an umbrella — **which governs only if it enumerates that package** (law 1). Enumeration is what bounds the walk, and that bound is the whole point: Cargo's walk is unconditional, and the ambiguity of "which workspace am I in" is what it never resolved. Here a directory that nothing enumerates is governed by nothing above it, however deep it sits.
//!
//! Only the declared-artifact forms of `run` walk at all. A file argument triggers no walk, so project scope is reachable only through something the manifest declares — the scratch-file hazard is not mitigated but unconstructible.

#[cfg(test)]
mod tests;

use {
    crate::{MANIFEST, Manifest, Package, Umbrella},
    std::path::{Path, PathBuf},
};

/// What governs an invocation: the package it is inside, and the umbrella enumerating that package when one does.
pub struct Governing {
    /// The package the invocation is inside.
    pub package: Package,
    /// Where that package's manifest sits, which is its namespace directory.
    pub directory: PathBuf,
    /// The umbrella that enumerates it, if any. Membership organizes and dependency compiles (law 3), so this decides where the store goes and what a `member` row can resolve to — never what is compiled.
    pub umbrella: Option<Umbrella>,
    /// The governing root: the umbrella's directory when one governs, and the package's otherwise. `.curios/` sits beside it.
    pub root: PathBuf,
}

impl Governing {
    /// What governs an invocation, by the manifest it named when it named one and by the walk otherwise.
    pub fn found(manifest: Option<&Path>, directory: &Path) -> Result<Self, String> {
        match manifest {
            Some(path) => Self::at(path),
            None => Self::of(directory),
        }
    }

    /// What governs an invocation started in `directory`.
    pub fn of(directory: &Path) -> Result<Self, String> {
        let directory = directory
            .canonicalize()
            .map_err(|error| format!("{}: {error}", directory.display()))?;

        let (package, at) = package_above(&directory)?;

        Self::rooted(package, at)
    }

    /// What governs an invocation whose manifest was named outright, walking upward only for the umbrella.
    ///
    /// The explicit override exists for scripting, and it overrides exactly the search: which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.
    pub fn at(manifest: &Path) -> Result<Self, String> {
        let directory = manifest
            .parent()
            .unwrap_or(Path::new("."))
            .canonicalize()
            .map_err(|error| format!("{}: {error}", manifest.display()))?;

        let Manifest::Package(package) = Manifest::from_path(manifest)? else {
            return Err(format!(
                "{} declares an umbrella, and an umbrella compiles nothing of its own: name one of its members instead",
                manifest.display()
            ));
        };

        Self::rooted(package, directory)
    }

    /// `package` at `directory`, under whichever umbrella enumerates it.
    fn rooted(package: Package, at: PathBuf) -> Result<Self, String> {
        let umbrella = umbrella_over(&at)?;

        Ok(match umbrella {
            Some((umbrella, root)) => Self {
                package,
                directory: at,
                umbrella: Some(umbrella),
                root,
            },
            None => Self {
                package,
                umbrella: None,
                root: at.clone(),
                directory: at,
            },
        })
    }
}

/// The nearest package manifest at or above `directory`, and the directory it sits in.
fn package_above(directory: &Path) -> Result<(Package, PathBuf), String> {
    for at in directory.ancestors() {
        let path = at.join(MANIFEST);
        if !path.is_file() {
            continue;
        }

        // An umbrella above the working directory is not a governing package, and neither is it an error yet: the package this invocation belongs to may still sit further up. What it is not is something `run` can compile.
        if let Manifest::Package(package) = Manifest::from_path(&path)? {
            return Ok((package, at.to_path_buf()));
        }
    }

    Err(format!(
        "no `{MANIFEST}` declaring a package governs {}; run a `.crs` file by name, or work inside a package",
        directory.display()
    ))
}

/// The umbrella above `directory` that enumerates it, and the directory that umbrella's manifest sits in.
///
/// The first umbrella found decides the answer either way. Umbrellas do not nest, so there is no second one above it to ask, and one that does not enumerate this package governs nothing rather than deferring upward.
fn umbrella_over(directory: &Path) -> Result<Option<(Umbrella, PathBuf)>, String> {
    for at in directory.ancestors().skip(1) {
        let path = at.join(MANIFEST);
        if !path.is_file() {
            continue;
        }

        let Manifest::Umbrella(umbrella) = Manifest::from_path(&path)? else {
            continue;
        };

        let enumerated = umbrella
            .members
            .iter()
            .any(|member| same_directory(&at.join(member), directory));

        return Ok(enumerated.then(|| (umbrella, at.to_path_buf())));
    }

    Ok(None)
}

/// Whether two paths name one directory, compared through the filesystem rather than by spelling: `members` may point deep (`"tools/cli"`), and a path that does not resolve simply is not this one.
fn same_directory(left: &Path, right: &Path) -> bool {
    match (left.canonicalize(), right.canonicalize()) {
        (Ok(left), Ok(right)) => left == right,
        _ => false,
    }
}
