//! `Curios.toml`: the two modes a manifest is in, and the refusals it earns before anything is compiled.
//!
//! Reading one is two passes over a file. The first is TOML's, into a [`Document`] whose every field is optional — *presence* decides the mode, so a manifest declaring both modes is a refusal this crate writes rather than a deserialization failure naming whichever shape happened to be tried second. The second pass is [`Document::classify`]: it turns present fields into the modes below, refusing a name no path could spell, a row missing the pin its source requires, and an executable claiming a stem that is already spoken for.
//!
//! There is no code escape and there never will be one by accident: the document is data, and a computed configuration would be a new decision rather than a latent capability. There is also no privilege field in either mode — a mounted package is `RootKind::Ordinary` because the loader hands `insert_root` that argument, and this parser has no path to it.

#[cfg(test)]
mod tests;

use {
    crate::TreeHash,
    curios_base::{Qualifier, is_identifier, is_keyword},
    serde::Deserialize,
    std::{
        collections::BTreeMap,
        fmt, fs,
        path::{Path, PathBuf},
        str::FromStr,
    },
};

/// The file a project is declared in.
pub const MANIFEST: &str = "Curios.toml";

/// The library header a package's `name` obligates, beside the manifest. Its stem enters no qualified name: the manifest names that namespace, so `lib` is a spelling nothing can refer to.
pub const LIBRARY: &str = "lib.crs";

/// The extension every Curios source file carries, which is what makes an executable's declared name enough to locate it.
pub const SOURCE: &str = "crs";

/// A parsed `Curios.toml`, in exactly one of two mutually exclusive modes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Manifest {
    /// A namespace for definitions.
    Package(Package),
    /// A namespace for packages.
    Umbrella(Umbrella),
}

impl Manifest {
    /// The manifest `path` holds, with every refusal named against that file.
    pub fn from_path(path: &Path) -> Result<Self, String> {
        let source =
            fs::read_to_string(path).map_err(|error| format!("{}: {error}", path.display()))?;

        source
            .parse()
            .map_err(|refusal| format!("{}: {refusal}", path.display()))
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
    pub name: Qualifier,
    /// Which executable a bare `curios run` means. `None` when there is at most one for it to mean.
    pub default: Option<String>,
    /// What this package depends on, by the canonical name each dependency declares for itself.
    pub dependencies: BTreeMap<Qualifier, Dependency>,
    /// The entry programs this package declares, in the order written.
    pub executables: Vec<Executable>,
}

/// A namespace for packages: the ones inside its tree, and the pinned rows those may share.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Umbrella {
    /// Where each member's own manifest sits, relative to this one. A member may lie arbitrarily deep on disk; umbrellas themselves do not nest.
    pub members: Vec<PathBuf>,
    /// The rows a member reaches by declaring `source = "catalog"`. A row here fetches nothing by itself — activation lives in the package that names it.
    pub catalog: BTreeMap<Qualifier, Dependency>,
}

/// One declared entry program: a name to run it by, and the file it is compiled from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Executable {
    /// A single legal identifier, which is what keeps `curios run <name>` and `curios run <file>.crs` two spaces that cannot overlap.
    pub name: String,
    /// `<name>.crs` beside the manifest, unless the row stated one. Deriving a path from a declared name is not disk discovery: declaration still decides existence.
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

/// The manifest as TOML spells it, with every field optional.
///
/// Deserializing straight into one shape per mode would make "declares both" a failure naming whichever shape was tried second, and "declares neither" a failure naming both. Those are this crate's refusals to write, so the deserializer is handed a shape that accepts either and nothing is decided until [`Document::classify`].
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Document {
    name: Option<String>,
    default: Option<String>,
    dependencies: Option<BTreeMap<String, Row>>,
    executables: Option<Vec<ExecutableRow>>,
    members: Option<Vec<String>>,
    catalog: Option<BTreeMap<String, Row>>,
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
            dependencies: table(self.dependencies.unwrap_or_default(), "dependency")?,
            executables,
        })
    }

    /// This document read as an umbrella.
    fn umbrella(self) -> Result<Umbrella, String> {
        let catalog = table(self.catalog.unwrap_or_default(), "catalog entry")?;

        // A catalog row is what a member's marker resolves *to*, so it cannot be a marker itself: umbrellas do not nest, and nothing sits above one to answer a marker it wrote.
        if let Some((name, _)) = catalog
            .iter()
            .find(|(_, row)| matches!(row, Dependency::Member | Dependency::Catalog))
        {
            return Err(format!(
                "the catalog entry {:?} names a marker source; a catalog row names a fetchable source or `path`",
                spelling(name)
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
struct Row {
    source: Source,
    url: Option<String>,
    rev: Option<String>,
    hash: Option<String>,
    path: Option<String>,
}

impl Row {
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

        let path = path.map_or_else(|| PathBuf::from(format!("{name}.{SOURCE}")), PathBuf::from);

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
    rows: BTreeMap<String, Row>,
    what: &str,
) -> Result<BTreeMap<Qualifier, Dependency>, String> {
    rows.into_iter()
        .map(|(written, row)| {
            Ok((
                canonical(&written, what)?,
                row.dependency(&format!("{what} {written:?}"))?,
            ))
        })
        .collect()
}

/// The canonical name `written` states, refused when no path could spell it.
///
/// A name is an atom: its segments are spelling, not structure, and `myorg` in `myorg/json` denotes nothing on its own. What the segments must survive is being written as a path, because that is how every consumer reaches the mount.
fn canonical(written: &str, what: &str) -> Result<Qualifier, String> {
    let refuse = |reason: String| {
        format!(
            "the {what} {written:?} is no name a path could spell: {reason}. A name's segments are separated by `/`, and each is a Curios identifier — no dashes, and no keyword."
        )
    };

    if written.is_empty() {
        return Err(refuse("it is empty".to_string()));
    }

    for segment in written.split('/') {
        if segment.is_empty() {
            return Err(refuse("it has an empty segment".to_string()));
        }

        if !is_identifier(segment) {
            return Err(refuse(format!("the segment {segment:?} is no identifier")));
        }

        if is_keyword(segment) {
            return Err(refuse(format!("the segment {segment:?} is a keyword")));
        }
    }

    Ok(Qualifier::from(written.split('/')))
}

/// A canonical name as a manifest spells it: segments separated by `/`, with no leading one.
fn spelling(name: &Qualifier) -> String {
    name.segments().join("/")
}
