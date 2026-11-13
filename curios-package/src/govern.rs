//! Which manifest governs an invocation.
//!
//! **The package is the one whose manifest is nearest**: in the directory the invocation stands in, or the first directory above it that holds one. Then the walk goes upward exactly once more, for an umbrella — **which governs only if it enumerates that package** (law 1). Enumeration is what bounds *that* walk, and the bound is the whole point: Cargo's is unconditional, and the ambiguity of "which workspace am I in" is what it never resolved.
//!
//! So there are two questions, and neither is answered by a guess: *which package* is the nearest manifest, which is where a command run anywhere inside a package — in a module's directory, say — means to land, and *what governs it* is an enumeration somebody wrote. The first walk stops at the first manifest it finds, so a package nested in another's directory is its own, and an umbrella found first governs no package, since it declares none. A walk makes a directory's meaning depend on what sits above it, and that is paid for by saying so: a build names the manifest when it is not in the working directory.
//!
//! Only the declared-artifact forms of `run` reach any of this. A file argument triggers no lookup, so project scope stays reachable only through something a manifest declares — the scratch-file hazard is not mitigated but unconstructible.

#[cfg(test)]
mod tests;

use {
    crate::{
        FormatSettings, MANIFEST, Manifest, Package, Refused, Step, Store, Umbrella,
        compiler_requirement, format_owner, require_compiler,
    },
    curios_common::Report,
    curios_module::identity,
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// What governs an invocation: the package it is inside, and the umbrella enumerating that package when one does.
pub struct Governing {
    /// The package the invocation is inside.
    pub package: Package,
    /// Where that package's manifest sits, which is its namespace directory.
    pub directory: PathBuf,
    /// The manifest that was read: the `curios.toml` the walk found, or the file `--manifest` named — what a report names when the invocation does not stand beside it.
    pub manifest: PathBuf,
    /// The umbrella that enumerates it, if any. Membership organizes and dependency compiles (law 3), so this decides where the store goes and what a `member` row can resolve to — never what is compiled.
    pub umbrella: Option<Umbrella>,
    /// The umbrella's members by declared name, each with the directory its manifest sits in — empty when no umbrella governs, which is what makes every marker a mismatch there.
    ///
    /// Read once, here, because every question about a member is answered against it: the dependency walk asks whether a `member` row names one and whether a direct pin may not, and `curate` asks where to look further. Two readers spelling the lookup for themselves could come to two answers — one refusing a member nothing declared, the other walking past it — and fetch on behalf of an umbrella the compile then refuses.
    pub members: BTreeMap<String, PathBuf>,
    /// The governing root: the umbrella's directory when one governs, and the package's otherwise. `.curios/` sits beside it.
    pub root: PathBuf,
}

impl Governing {
    /// Check the selected package's effective minimum before compiling or reusing a cached executable.
    pub(crate) fn check_compiler(&self) -> Result<(), Box<Report>> {
        require_compiler(
            &self.package,
            &self.directory,
            self.umbrella
                .as_ref()
                .map(|umbrella| (umbrella, self.root.as_path())),
        )
    }

    /// The governing package's format, with the manifest that owns it: the enumerating umbrella's, or the package's own.
    pub(crate) fn format(&self) -> Result<(Option<&FormatSettings>, PathBuf), Box<Report>> {
        format_owner(
            &self.package,
            &self.directory,
            self.umbrella
                .as_ref()
                .map(|umbrella| (umbrella, self.root.as_path())),
        )
    }

    /// Where this project's generated things go — its own beside it, and the shared ones in the cache every project uses.
    pub fn store(&self) -> Store {
        Store::at(self.root.clone())
    }

    /// What governs an invocation, by the manifest it named when it named one and by the one nearest `directory` otherwise.
    pub fn found(manifest: Option<&Path>, directory: &Path) -> Result<Self, Box<Report>> {
        match manifest {
            Some(path) => Self::at(path),
            None => Self::of(directory),
        }
    }

    /// What governs an invocation started where the process is standing.
    ///
    /// The working directory is read here rather than by each caller, because "where the invocation started" is this lookup's own input and every caller would otherwise spell the same two lines to supply it.
    pub fn here(manifest: Option<&Path>) -> Result<Self, Box<Report>> {
        let directory = std::env::current_dir().map_err(|error| {
            Refused::of("unreadable directory")
                .stating("the working directory could not be read")
                .stating(error)
                .about_named("the working directory")
        })?;

        Self::found(manifest, &directory)
    }

    /// What governs an invocation started in `directory`: the package whose manifest is nearest, at `directory` or above it.
    pub fn of(directory: &Path) -> Result<Self, Box<Report>> {
        let directory = directory
            .canonicalize()
            .map_err(|error| unreadable(directory, &error))?;

        let (package, at) = nearest_package(&directory)?;

        Self::rooted(package, at.join(MANIFEST), at)
    }

    /// What governs an invocation whose manifest was named outright, walking upward only for the umbrella.
    ///
    /// The explicit override exists for scripting, and it overrides exactly the search: which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.
    pub fn at(manifest: &Path) -> Result<Self, Box<Report>> {
        // Canonical through `identity`, which is what reads a bare `curios.toml` as the working directory's: its parent is `""`, which canonicalizes to nothing, so canonicalizing the parent directly would refuse the file as missing.
        let manifest = identity(manifest);

        let Manifest::Package(package) = Manifest::from_path(&manifest)? else {
            return Err(Refused::of("not a package")
                .stating("this manifest declares an umbrella")
                .stating("an umbrella compiles nothing of its own")
                .stating("name one of its members")
                .about_path(&manifest));
        };

        // Read after the manifest, because a file that was read is a canonical path with a directory over it.
        let directory = manifest
            .parent()
            .expect("a manifest that was read sits in a directory")
            .to_path_buf();

        Self::rooted(package, manifest, directory)
    }

    /// What governs `file`: the package `manifest` names, which must hold it, or the package whose manifest is nearest above it. `None` where no manifest is above it, or where the first one found is an umbrella's, which declares no package — the file is then no package's.
    pub fn of_file(file: &Path, manifest: Option<&Path>) -> Result<Option<Self>, Box<Report>> {
        let spelled = identity(file);

        match manifest {
            Some(named) => {
                let governing = Self::at(named)?;
                if !spelled.starts_with(identity(&governing.directory)) {
                    return Err(Refused::of("outside the package")
                        .stating(format_args!(
                            "this file is outside the package {} declares",
                            named.display()
                        ))
                        .stating("`--manifest` cannot place it")
                        .about_path(file));
                }
                Ok(Some(governing))
            }
            None => match spelled.parent().and_then(nearest_manifest) {
                Some(at) => match Manifest::from_path(&at.join(MANIFEST))? {
                    Manifest::Package(_) => Self::of(&at).map(Some),
                    Manifest::Umbrella(_) => Ok(None),
                },
                None => Ok(None),
            },
        }
    }

    /// `package`, read from `manifest` at `at`, under whichever umbrella enumerates it.
    fn rooted(package: Package, manifest: PathBuf, at: PathBuf) -> Result<Self, Box<Report>> {
        let umbrella = umbrella_over(&at)?;
        let umbrella_at = umbrella
            .as_ref()
            .map(|(umbrella, root)| (umbrella, root.as_path()));
        compiler_requirement(&package, &at, umbrella_at)?;
        format_owner(&package, &at, umbrella_at)?;

        Ok(match umbrella {
            Some((umbrella, root)) => Self {
                package,
                directory: at,
                manifest,
                members: members(&root, &umbrella)?,
                umbrella: Some(umbrella),
                root,
            },
            None => Self {
                package,
                manifest,
                umbrella: None,
                members: BTreeMap::new(),
                root: at.clone(),
                directory: at,
            },
        })
    }
}

/// Each member's declared name, and the directory its manifest sits in.
///
/// Every refusal here is the umbrella's: an entry no manifest answers, an entry that is itself an umbrella, and two entries declaring one name — each named against the umbrella's manifest, since that is the file somebody wrote (law 4).
fn members(root: &Path, umbrella: &Umbrella) -> Result<BTreeMap<String, PathBuf>, Box<Report>> {
    let mut members = BTreeMap::new();
    // Which entry placed each name, for a refusal that quotes the entry as the umbrella wrote it.
    let mut entries = BTreeMap::new();

    for (index, member) in umbrella.members.iter().enumerate() {
        let directory = root.join(member);
        let manifest = directory.join(MANIFEST);
        // Every refusal of an entry stands at the entry, in the umbrella's manifest.
        let listed = |refused: Refused| {
            refused
                .entry([Step::key("members"), Step::Row(index)])
                .located_at(&root.join(MANIFEST))
        };

        // Asked before the read, because the read's refusal names the file it could not open and nothing else: an entry somebody mistyped is a fault in the umbrella's row (law 4), and the reader has to be sent there rather than to a path they never spelled.
        if !manifest.is_file() {
            return Err(listed(
                Refused::of("missing member")
                    .stating(format_args!(
                        "the umbrella lists the member `{}`",
                        member.display()
                    ))
                    .stating(format_args!(
                        "no `{MANIFEST}` sits in {}",
                        directory.display()
                    )),
            ));
        }

        let Manifest::Package(package) = Manifest::from_path(&manifest)? else {
            return Err(listed(
                Refused::of("nested umbrella")
                    .stating(format_args!(
                        "the member `{}` declares an umbrella",
                        member.display()
                    ))
                    .stating("umbrellas do not nest"),
            ));
        };

        // Canonical, as every location the walk compares is: `base` and `./base` are one member, and a refusal that read them as two would send the reader after a second package that does not exist.
        let directory = directory
            .canonicalize()
            .map_err(|error| unreadable(&directory, &error))?;

        if let Some(earlier) = members.insert(package.name.clone(), directory.clone()) {
            let written: &PathBuf = &entries[&package.name];
            return Err(listed(match earlier == directory {
                true => Refused::of("duplicate member")
                    .stating(format_args!("the umbrella lists `{}` twice", package.name))
                    .stating(format_args!(
                        "as `{}` and as `{}`",
                        written.display(),
                        member.display()
                    )),
                false => Refused::of("duplicate name")
                    .stating(format_args!(
                        "two members declare the name `{}`",
                        package.name
                    ))
                    .stating(format_args!(
                        "{} and {}",
                        earlier.display(),
                        directory.display()
                    )),
            }));
        }
        entries.insert(package.name, member.clone());
    }

    Ok(members)
}

/// The package whose manifest is nearest `directory`, and the directory that manifest sits in.
///
/// **The first manifest found decides.** A package nested in another's directory is its own, since its manifest is nearer; and an umbrella found first governs no package, since it declares none, so standing in its tree outside every member is refused for what it is rather than walked past to something further up.
fn nearest_package(directory: &Path) -> Result<(Package, PathBuf), Box<Report>> {
    let Some(at) = nearest_manifest(directory) else {
        return Err(Refused::of("no manifest")
            .stating(format_args!(
                "no `{MANIFEST}` in this directory or any directory above it"
            ))
            .stating("run a `.crs` file by name, or work inside a package")
            .about_path(directory));
    };
    let path = at.join(MANIFEST);

    // An umbrella declares no definitions, so standing in its tree there is nothing to compile — and unlike a missing manifest, this one is worth naming for what it is.
    let Manifest::Package(package) = Manifest::from_path(&path)? else {
        return Err(Refused::of("not a package")
            .stating("this manifest declares an umbrella")
            .stating("an umbrella compiles nothing of its own")
            .stating("work in one of its members")
            .about_path(&path));
    };

    Ok((package, at))
}

/// The nearest directory at or above `directory` that holds a manifest — the one walk both an invocation's directory and a file's own are placed by.
pub fn nearest_manifest(directory: &Path) -> Option<PathBuf> {
    directory
        .ancestors()
        .find(|at| at.join(MANIFEST).is_file())
        .map(Path::to_path_buf)
}

/// The umbrella above `directory` that enumerates it, and the directory that umbrella's manifest sits in.
///
/// The first umbrella found decides the answer either way. Umbrellas do not nest, so there is no second one above it to ask, and one that does not enumerate this package governs nothing rather than deferring upward.
pub fn umbrella_over(directory: &Path) -> Result<Option<(Umbrella, PathBuf)>, Box<Report>> {
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

/// The refusal of a directory the system would not resolve, in the system's words.
fn unreadable(directory: &Path, error: &std::io::Error) -> Box<Report> {
    Refused::of("unreadable directory")
        .stating(error)
        .about_path(directory)
}

/// Whether two paths name one directory, compared through the filesystem rather than by spelling: `members` may point deep (`"tools/cli"`), and a path that does not resolve simply is not this one.
fn same_directory(left: &Path, right: &Path) -> bool {
    match (left.canonicalize(), right.canonicalize()) {
        (Ok(left), Ok(right)) => left == right,
        _ => false,
    }
}
