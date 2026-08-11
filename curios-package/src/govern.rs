//! Which manifest governs an invocation.
//!
//! **The package is the one whose manifest sits in the working directory**, never one found by searching above it. Then the walk goes upward exactly once more, for an umbrella — **which governs only if it enumerates that package** (law 1). Enumeration is what bounds *that* walk, and the bound is the whole point: Cargo's is unconditional, and the ambiguity of "which workspace am I in" is what it never resolved.
//!
//! So there are two questions and only the second is answered by looking around: *which package* is answered by one directory, and *what governs it* by an enumeration somebody wrote. A directory nothing enumerates is governed by nothing above it however deep it sits, and a directory with no manifest is not a package at all — a subdirectory that wants to be one declares itself one. The cost is that `curios run` deep inside a package no longer finds it; the gain is that what an invocation compiles is visible in one `ls`.
//!
//! Only the declared-artifact forms of `run` reach any of this. A file argument triggers no lookup, so project scope stays reachable only through something a manifest declares — the scratch-file hazard is not mitigated but unconstructible.

#[cfg(test)]
mod tests;

use {
    crate::{MANIFEST, Manifest, Package, Store, Umbrella},
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
    /// Where this project's generated things go — its own beside it, and the shared ones in the cache every project uses.
    pub fn store(&self) -> Store {
        Store::at(self.root.clone())
    }

    /// What governs an invocation, by the manifest it named when it named one and by the working directory's otherwise.
    pub fn found(manifest: Option<&Path>, directory: &Path) -> Result<Self, String> {
        match manifest {
            Some(path) => Self::at(path),
            None => Self::of(directory),
        }
    }

    /// What governs an invocation started where the process is standing.
    ///
    /// The working directory is read here rather than by each caller, because "where the invocation started" is this lookup's own input and every caller had to spell the same two lines to supply it.
    pub fn here(manifest: Option<&Path>) -> Result<Self, String> {
        let directory = std::env::current_dir().map_err(|error| error.to_string())?;

        Self::found(manifest, &directory)
    }

    /// What governs an invocation started in `directory`, whose own manifest is the package.
    pub fn of(directory: &Path) -> Result<Self, String> {
        let directory = directory
            .canonicalize()
            .map_err(|error| format!("{}: {error}", directory.display()))?;

        let (package, at) = package_here(&directory)?;

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

/// The package `directory` itself declares, and the directory it sits in — which is `directory`.
///
/// **A package governs the directory its manifest is in, and no other.** Deciding that by a walk would mean a directory's meaning depends on what sits above it, which is the ambiguity the umbrella rule below already refuses to inherit; the same objection applies one level down, and answering it with "the manifest is here or there is none" costs a `cd` and buys an invocation whose scope is visible in one `ls`. A subdirectory that wants to be a package declares itself one — and a package declared *inside* an umbrella's tree still needs that umbrella to enumerate it before anything above governs it at all.
fn package_here(directory: &Path) -> Result<(Package, PathBuf), String> {
    let path = directory.join(MANIFEST);

    if !path.is_file() {
        return Err(format!(
            "no `{MANIFEST}` in {}; a package governs the directory its manifest is in, so run a `.crs` file by name, or work in a package's own directory",
            directory.display()
        ));
    }

    // An umbrella declares no definitions, so standing in its root there is nothing to compile — and unlike a missing manifest, this one is worth naming for what it is.
    let Manifest::Package(package) = Manifest::from_path(&path)? else {
        return Err(format!(
            "{} declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead",
            path.display()
        ));
    };

    Ok((package, directory.to_path_buf()))
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
