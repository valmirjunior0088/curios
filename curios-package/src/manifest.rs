//! `curios.toml`: the two modes a manifest is in, and the refusals it earns before anything is compiled.
//!
//! Reading one is two passes over a file. The first is TOML's, into a [`Document`] whose every field is optional — *presence* decides the mode, so a manifest declaring both modes is a refusal this crate writes rather than a deserialization failure naming whichever shape happened to be tried second. The second pass is [`Document::classify`]: it turns present fields into the modes below, refusing a name no path could spell, a row missing the pin its source requires, and an executable claiming a stem that is already spoken for.
//!
//! There is no code escape and there never will be one by accident: the document is data, and a computed configuration would be a new decision rather than a latent capability. There is also no privilege field in either mode — a mounted package is `RootKind::Ordinary` because the loader hands `insert_root` that argument, and this parser has no path to it.

#[cfg(test)]
mod tests;

use {
    crate::TreeHash,
    curios_base::{is_identifier, is_keyword},
    serde::Deserialize,
    std::{
        collections::BTreeMap,
        fmt, fs,
        path::{Path, PathBuf},
        str::FromStr,
    },
};

/// The file a project is declared in.
pub const MANIFEST: &str = "curios.toml";

/// The library header a package's `name` obligates, beside the manifest. Its stem enters no qualified name: the manifest names that namespace, so `lib` is a spelling nothing can refer to.
pub const LIBRARY: &str = "lib.crs";

/// The package's own executable, found beside the manifest when no `[[executables]]` row claims the package's name. Like [`LIBRARY`], presence is the declaration and the stem enters no qualified name.
pub const EXECUTABLE: &str = "exe.crs";

/// The extension every Curios source file carries, which is what makes an executable's declared name enough to locate it.
pub const EXTENSION: &str = "crs";

/// A parsed `curios.toml`, in exactly one of two mutually exclusive modes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Manifest {
    /// A namespace for definitions.
    Package(Package),
    /// A namespace for packages.
    Umbrella(Umbrella),
}

impl Manifest {
    /// The manifest `path` holds, with every refusal named against that file — and the package's own executable, when `exe.crs` sits beside it.
    ///
    /// Discovery happens here rather than in [`FromStr`], because only this spelling knows where the manifest *is*. The text alone can be parsed without a filesystem, which is what the tests and the umbrella walk rely on.
    pub fn from_path(path: &Path) -> Result<Self, String> {
        let source =
            fs::read_to_string(path).map_err(|error| format!("{}: {error}", path.display()))?;

        let manifest: Self = source
            .parse()
            .map_err(|refusal| format!("{}: {refusal}", path.display()))?;

        Ok(match (manifest, path.parent()) {
            (Self::Package(package), Some(directory)) => {
                Self::Package(package.discovered(directory))
            }
            (manifest, _) => manifest,
        })
    }
}

impl FromStr for Manifest {
    type Err = String;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        toml::from_str::<Document>(source)
            .map_err(|error| error.to_string())?
            .classify()
    }
}

/// A namespace for definitions: it names itself, states what it depends on, and enumerates its entry programs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Package {
    /// The canonical name, which is this package's mount prefix and the only way any consumer refers to it. Declared here and nowhere else — no umbrella contributes to it, so reorganizing a tree renames nothing.
    pub name: String,
    /// Which executable a bare `curios run` means. `None` when there is at most one for it to mean.
    pub default: Option<String>,
    /// What this package depends on, by the canonical name each dependency declares for itself.
    pub dependencies: BTreeMap<String, Dependency>,
    /// The entry programs this package declares, in the order written.
    pub executables: Vec<Executable>,
}

impl Package {
    /// This package, plus its own executable when `exe.crs` sits in `directory` and no row already claims that name.
    ///
    /// **Presence is the declaration, exactly as it is for `lib.crs`.** A package's own program and its own library are the two things it *is*, and neither should cost a manifest entry to admit to. The asymmetry this crate used to state — that a vanished executable "fails by silently not being there" — does not survive contact: deleting `exe.crs` makes `curios run` say the package declares no executable, which is a refusal naming the problem rather than a silence.
    ///
    /// A row wins over the file: somebody who wrote `[[executables]]` for this name meant the path they gave.
    fn discovered(mut self, directory: &Path) -> Self {
        let claimed = self
            .executables
            .iter()
            .any(|executable| executable.name == self.name);

        if !claimed && directory.join(EXECUTABLE).is_file() {
            self.executables.push(Executable {
                name: self.name.clone(),
                path: PathBuf::from(EXECUTABLE),
            });
        }

        self
    }
}

/// A namespace for packages: the ones inside its tree, and the pinned rows those may share.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Umbrella {
    /// Where each member's own manifest sits, relative to this one. A member may lie arbitrarily deep on disk; umbrellas themselves do not nest.
    pub members: Vec<PathBuf>,
    /// The rows a member reaches by declaring `source = "catalog"`. A row here fetches nothing by itself — activation lives in the package that names it.
    pub catalog: BTreeMap<String, Dependency>,
}

/// One declared entry program: a name to run it by, and the file it is compiled from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Executable {
    /// A single legal identifier, which is what keeps `curios run <name>` and `curios run <file>.crs` two spaces that cannot overlap.
    pub name: String,
    /// `<name>.crs` beside the manifest, unless the row stated one — or `exe.crs` for the package's own executable, which is found rather than declared. Deriving a path from a declared name is not disk discovery: for a row, declaration still decides existence.
    pub path: PathBuf,
}

/// One dependency row: the resolver that answers for a name, and what that resolver needs in order to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dependency {
    /// A live member of the governing umbrella, unpinned because live code has no pin.
    Member,
    /// The governing umbrella's `[catalog]` row of this name.
    Catalog,
    /// A repository at an exact revision, accepted by hash.
    Git {
        url: String,
        /// The fetch instruction — the thing a remote can be asked for. Opaque: compared for equality, never interpreted, so the compiler needs no notion of a registry, a version scheme, or a VCS.
        rev: String,
        hash: TreeHash,
    },
    /// A live sibling on disk, for a project with no umbrella over it.
    Path { path: PathBuf },
}

impl Dependency {
    /// The snapshot this row pins, for a fetchable source. Live code pins nothing, so two live rows agree only by resolving to one place.
    pub fn snapshot(&self) -> Option<Snapshot> {
        match self {
            Self::Git { rev, hash, .. } => Some(Snapshot {
                rev: rev.clone(),
                hash: hash.clone(),
            }),
            _ => None,
        }
    }
}

/// Exactly what a row pins: the instruction that fetches it, and the criterion that accepts it.
///
/// **`url` is not part of it, and that omission is the rule.** Acceptance is by hash, so any transport may deliver the bytes — which makes two dependents pinning one tree through different mirrors an *agreement*, not a conflict. This is the type the resolver compares, so what it leaves out is what a conflict is allowed to differ in. Where a fetch must actually be performed, `Acquisition` carries this alongside the transport that answers for it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Snapshot {
    pub rev: String,
    pub hash: TreeHash,
}

/// The manifest as TOML spells it, with every field optional.
///
/// Deserializing straight into one shape per mode would make "declares both" a failure naming whichever shape was tried second, and "declares neither" a failure naming both. Those are this crate's refusals to write, so the deserializer is handed a shape that accepts either and nothing is decided until [`Document::classify`].
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Document {
    name: Option<String>,
    default: Option<String>,
    dependencies: Option<BTreeMap<String, DependencyRow>>,
    executables: Option<Vec<ExecutableRow>>,
    members: Option<Vec<String>>,
    catalog: Option<BTreeMap<String, DependencyRow>>,
}

impl Document {
    /// Which mode this document is in, and what that answer costs.
    fn classify(self) -> Result<Manifest, String> {
        let package = declared(&[
            ("name", self.name.is_some()),
            ("default", self.default.is_some()),
            ("dependencies", self.dependencies.is_some()),
            ("executables", self.executables.is_some()),
        ]);
        let umbrella = declared(&[
            ("members", self.members.is_some()),
            ("catalog", self.catalog.is_some()),
        ]);

        match (package.is_empty(), umbrella.is_empty()) {
            (false, false) => Err(format!(
                "a manifest is a package or an umbrella, never both: this one declares {package} beside {umbrella}. A package is a namespace for definitions and an umbrella a namespace for packages; an umbrella whose first member is this package is one directory away."
            )),
            (true, true) => Err(format!(
                "a manifest declares a package or an umbrella, and this one declares neither: a package states `name`, an umbrella states `members`. A bare `.crs` file needs no {MANIFEST} at all."
            )),
            (false, true) => self.package().map(Manifest::Package),
            (true, false) => self.umbrella().map(Manifest::Umbrella),
        }
    }

    /// This document read as a package.
    fn package(self) -> Result<Package, String> {
        let Some(spelling) = self.name else {
            return Err(
                "a package declares its own canonical name, and this one declares none: `name` is both its mount prefix and every consumer's only way to refer to it".to_string(),
            );
        };
        let name = canonical(&spelling, "package name")?;

        let mut executables = Vec::new();
        for row in self.executables.unwrap_or_default() {
            executables.push(row.executable(&executables)?);
        }

        // `default` is a reference like any other, so a dangling one is refused here rather than discovered by a `curios run` that finds nothing.
        if let Some(default) = &self.default
            && !executables
                .iter()
                .any(|executable| &executable.name == default)
        {
            return Err(format!(
                "`default` names the executable {default:?}, which this package does not declare"
            ));
        }

        Ok(Package {
            name,
            default: self.default,
            dependencies: table(self.dependencies.unwrap_or_default(), "dependency row")?,
            executables,
        })
    }

    /// This document read as an umbrella.
    fn umbrella(self) -> Result<Umbrella, String> {
        let catalog = table(self.catalog.unwrap_or_default(), "catalog row")?;

        // A catalog row is what a member's marker resolves *to*, so it cannot be a marker itself: umbrellas do not nest, and nothing sits above one to answer a marker it wrote.
        if let Some((name, _)) = catalog
            .iter()
            .find(|(_, row)| matches!(row, Dependency::Member | Dependency::Catalog))
        {
            return Err(format!(
                "the catalog row {name:?} names a marker source; a catalog row names a fetchable source or `path`"
            ));
        }

        Ok(Umbrella {
            members: self
                .members
                .unwrap_or_default()
                .into_iter()
                .map(PathBuf::from)
                .collect(),
            catalog,
        })
    }
}

/// One dependency row as TOML spells it: a resolver, and the union of every field any resolver takes.
///
/// The union rather than one shape per source, because which fields a row may carry is a refusal this crate writes — `source` is what a reader looks for first, and a row missing its pin should be told which pin, not told that no shape matched.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DependencyRow {
    source: Source,
    url: Option<String>,
    rev: Option<String>,
    hash: Option<String>,
    path: Option<String>,
}

impl DependencyRow {
    /// What this row resolves to, or which field the source it names cannot use — or requires. `subject` names the row for a refusal: the table it sits in, and the name it is filed under.
    fn dependency(self, subject: &str) -> Result<Dependency, String> {
        let Self {
            source,
            url,
            rev,
            hash,
            path,
        } = self;

        // Exactly the fields this source takes. Both directions are checked against it, so a field is never quietly ignored and never quietly defaulted: a `rev` on a live row is a pin somebody believed in.
        let takes: &[&str] = match source {
            Source::Member | Source::Catalog => &[],
            Source::Git => &["url", "rev", "hash"],
            Source::Path => &["path"],
        };
        let stated = [
            ("url", url.is_some()),
            ("rev", rev.is_some()),
            ("hash", hash.is_some()),
            ("path", path.is_some()),
        ];

        if let Some((field, _)) = stated
            .iter()
            .find(|(field, stated)| *stated && !takes.contains(field))
        {
            return Err(format!(
                "the {subject} is `source = \"{source}\"`, which takes no `{field}`"
            ));
        }

        let require = |field: &str, value: Option<String>| {
            value.ok_or_else(|| {
                format!("the {subject} is `source = \"{source}\"`, which requires `{field}`")
            })
        };

        match source {
            Source::Member => Ok(Dependency::Member),
            Source::Catalog => Ok(Dependency::Catalog),
            Source::Git => Ok(Dependency::Git {
                url: require("url", url)?,
                rev: require("rev", rev)?,
                hash: TreeHash::parse(&require("hash", hash)?)
                    .map_err(|refusal| format!("the {subject}: {refusal}"))?,
            }),
            Source::Path => Ok(Dependency::Path {
                path: require("path", path)?.into(),
            }),
        }
    }
}

/// The resolvers a row may name. `member` and `catalog` are markers: each names exactly the umbrella-side list that answers it.
#[derive(Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Source {
    Member,
    Catalog,
    Git,
    Path,
}

impl fmt::Display for Source {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Member => "member",
            Self::Catalog => "catalog",
            Self::Git => "git",
            Self::Path => "path",
        })
    }
}

/// One `[[executables]]` row as TOML spells it.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ExecutableRow {
    name: String,
    path: Option<String>,
}

impl ExecutableRow {
    /// This row as a declared entry program, against the ones already declared before it.
    fn executable(self, declared: &[Executable]) -> Result<Executable, String> {
        let Self { name, path } = self;

        if !is_identifier(&name) {
            return Err(format!(
                "the executable {name:?} is not named by a single identifier, so `curios run` could not tell it from a file argument"
            ));
        }

        // One rule for every row, including the package's own: a declared executable is compiled from the file named after it. `exe.crs` is not a default here — it is what `Package::discovered` finds when *no* row claims the package's name, so writing a row always means what it says and never silently redirects an existing package's file.
        let path = path.map_or_else(
            || PathBuf::from(format!("{name}.{EXTENSION}")),
            PathBuf::from,
        );

        if let Some(previous) = declared.iter().find(|executable| executable.name == name) {
            return Err(format!(
                "the executable {name:?} is declared twice, from {} and from {}",
                previous.path.display(),
                path.display()
            ));
        }

        // The package root has one stem space, and the library header holds a stem in it.
        if path == Path::new(LIBRARY) {
            return Err(format!(
                "the executable {name:?} is compiled from `{LIBRARY}`, which is the package's library header"
            ));
        }

        Ok(Executable { name, path })
    }
}

/// The keys of `keys` that are present, listed for a refusal that names both parties.
fn declared(keys: &[(&'static str, bool)]) -> String {
    keys.iter()
        .filter(|(_, present)| *present)
        .map(|(key, _)| format!("`{key}`"))
        .collect::<Vec<_>>()
        .join(" and ")
}

/// Every row of one table, by the canonical name each is filed under.
fn table(
    rows: BTreeMap<String, DependencyRow>,
    what: &str,
) -> Result<BTreeMap<String, Dependency>, String> {
    rows.into_iter()
        .map(|(written, row)| {
            Ok((
                canonical(&written, what)?,
                row.dependency(&format!("{what} {written:?}"))?,
            ))
        })
        .collect()
}

/// The canonical name `written` states, refused when it is not one.
///
/// **A name is one word, and that is the whole rule.** It is the prefix the package mounts at, and a prefix is an atom: `myorg` in a hypothetical `myorg/json` would denote nothing on its own, so admitting segments here would make every package silently declare namespaces nobody wrote. A segment of names is a real thing to want, but it belongs to a package's own modules, where somebody declares each one.
pub(crate) fn canonical(written: &str, what: &str) -> Result<String, String> {
    let refuse = |reason: &str| {
        format!(
            "the {what} {written:?} is no name a path could spell: {reason}. A name is a single Curios identifier — no dashes, no `/`, and no keyword."
        )
    };

    match written {
        "" => Err(refuse("it is empty")),
        // `is_identifier` rejects a `/` along with everything else a segment could not be, so the one-word rule needs no separate check for it.
        _ if !is_identifier(written) => Err(refuse("it is no identifier")),
        _ if is_keyword(written) => Err(refuse("it is a keyword")),
        _ => Ok(written.to_string()),
    }
}
