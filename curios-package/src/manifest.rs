//! `curios.toml`: the two modes a manifest is in, and the refusals it earns before anything is compiled.
//!
//! Reading one is two passes over a file. The first is TOML's, into a [`Document`] whose every field is optional — *presence* decides the mode, so a manifest declaring both modes is a refusal this crate writes rather than a deserialization failure naming whichever shape happened to be tried second. The second pass is [`Document::classify`]: it turns present fields into the modes below, refusing a name no path could spell, a row missing the pin its source requires, and an executable claiming a stem that is already spoken for.
//!
//! There is no code escape and there never will be one by accident: the document is data, and a computed configuration would be a new decision rather than a latent capability. There is also no privilege field in either mode — a mounted package is `RootKind::Ordinary` because the loader hands `insert_root` that argument, and this parser has no path to it.

#[cfg(test)]
mod test_support;

#[cfg(test)]
mod dependency_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod package_tests;

use {
    crate::{
        CompilerVersion, FileHash, FormatSettings, Refused, STORE, Step, Store, Subject, TreeHash,
    },
    curios_common::{About, Report, Said, Source, is_identifier, is_keyword},
    serde::Deserialize,
    std::{
        collections::BTreeMap,
        fmt, fs,
        path::{Component, Path, PathBuf},
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
    pub fn from_path(path: &Path) -> Result<Self, Box<Report>> {
        let source = Source::read(path).map_err(|error| {
            let mut said = Said::line("unreadable file");
            said.stating(error);

            Report::about(About::Path(path.to_path_buf()), said)
        })?;

        let manifest: Self = source
            .text()
            .parse()
            .map_err(|refused: Refused| refused.located(&source))?;

        Ok(match (manifest, path.parent()) {
            (Self::Package(package), Some(directory)) => {
                Self::Package(package.discovered(directory))
            }
            (manifest, _) => manifest,
        })
    }
}

impl FromStr for Manifest {
    type Err = Refused;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        toml::from_str::<Document>(source)
            .map_err(|error| {
                let refused = Refused::of("syntax error").stating(error.message());

                match error.span() {
                    Some(range) => refused.range(range),
                    None => refused,
                }
            })?
            .classify()
    }
}

/// A namespace for definitions: it names itself, states what it depends on, and enumerates its entry programs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Package {
    /// The canonical name, which is this package's mount prefix and the only way any consumer refers to it. Declared here and nowhere else — no umbrella contributes to it, so reorganizing a tree renames nothing.
    pub name: String,
    /// The standalone package's minimum compiler. An umbrella member leaves this absent and inherits its umbrella's requirement.
    pub compiler: Option<CompilerVersion>,
    /// How the standalone package's files are formatted, `None` where it states no `[format]`. An umbrella member leaves this absent and inherits its umbrella's, as it does `compiler`.
    pub format: Option<FormatSettings>,
    /// What the package is, in a sentence or a few, for the landing page of its documentation. A fact about the package as a whole, which is what the manifest declares and no source file does — the library root is declared by no `mod`, so it has nowhere else to carry prose.
    pub description: Option<String>,
    /// Which executable a bare `curios run` means. `None` when there is at most one for it to mean.
    pub default: Option<String>,
    /// What this package depends on, by the canonical name each dependency declares for itself.
    pub dependencies: BTreeMap<String, Dependency>,
    /// The entry programs this package declares, in the order written.
    pub executables: Vec<Executable>,
    /// What implements this package's `foreign` declarations, in the order written. Empty for the overwhelming majority of packages, which declare none.
    pub foreign: Vec<Foreign>,
}

impl Package {
    /// This package, plus its own executable when `exe.crs` sits in `directory` and no row already claims that name.
    ///
    /// **Presence is the declaration, exactly as it is for `lib.crs`**, for the reasons the `layout` module states.
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

    /// `executable` as a refusal names it: a row by its name, and the package's own program by the file that declares it. Presence is that program's declaration, so a reader who wrote no row for it would otherwise look for the name in the manifest and not find it there.
    pub fn describe(&self, executable: &Executable) -> String {
        match executable.name == self.name && executable.path == Path::new(EXECUTABLE) {
            true => format!(
                "the package's own executable {:?} that `{EXECUTABLE}` beside the manifest declares",
                executable.name
            ),
            false => format!("the executable {:?}", executable.name),
        }
    }
}

/// A namespace for packages: the ones inside its tree, and the pinned rows those may share.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Umbrella {
    /// The minimum compiler every enumerated member inherits.
    pub compiler: CompilerVersion,
    /// How every enumerated member's files are formatted, `None` where it states no `[format]`.
    pub format: Option<FormatSettings>,
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
    compiler: Option<String>,
    format: Option<FormatSettings>,
    description: Option<String>,
    default: Option<String>,
    dependencies: Option<BTreeMap<String, DependencyRow>>,
    executables: Option<Vec<ExecutableRow>>,
    foreign: Option<Vec<ForeignRow>>,
    members: Option<Vec<String>>,
    catalog: Option<BTreeMap<String, DependencyRow>>,
}

impl Document {
    /// Which mode this document is in, and what that answer costs.
    fn classify(self) -> Result<Manifest, Refused> {
        let package = declared(&[
            ("name", self.name.is_some()),
            ("description", self.description.is_some()),
            ("default", self.default.is_some()),
            ("dependencies", self.dependencies.is_some()),
            ("executables", self.executables.is_some()),
            ("foreign", self.foreign.is_some()),
        ]);
        let umbrella = declared(&[
            ("members", self.members.is_some()),
            ("catalog", self.catalog.is_some()),
        ]);

        match (package.is_empty(), umbrella.is_empty()) {
            (false, false) => Err(Refused::of("ambiguous manifest")
                .stating(format_args!(
                    "this manifest declares {package} beside {umbrella}"
                ))
                .stating("a manifest is a package or an umbrella, never both")
                .stating("a package is a namespace for definitions, and an umbrella a namespace for packages")
                .stating("make an umbrella one directory up, with this package as its first member")
                .entry([match self.members.is_some() {
                    true => "members",
                    false => "catalog",
                }])),
            (true, true) => Err(Refused::of("empty manifest")
                .stating("this manifest declares neither a package nor an umbrella")
                .stating("a package states `name`, and an umbrella states `members`")
                .stating(format_args!("a bare `.crs` file needs no {MANIFEST}"))),
            (false, true) => self.package().map(Manifest::Package),
            (true, false) => self.umbrella().map(Manifest::Umbrella),
        }
    }

    /// This document read as a package.
    fn package(self) -> Result<Package, Refused> {
        let Some(spelling) = self.name else {
            return Err(Refused::of("missing name")
                .stating("this package declares no `name`")
                .stating("a name is its mount prefix, and every consumer's way to refer to it"));
        };
        let name = canonical(&spelling).map_err(|refused| refused.value(["name"]))?;

        let mut executables = Vec::new();
        for (index, row) in self.executables.unwrap_or_default().into_iter().enumerate() {
            let placed =
                |refused: Refused| refused.under([Step::key("executables"), Step::Row(index)]);
            executables.push(row.executable(&executables).map_err(placed)?);
        }

        // `default` is a reference like any other, so a dangling one is refused here rather than discovered by a `curios run` that finds nothing. The package's own name is exempt: it is the one name [`Package::discovered`] may still add, from an `exe.crs` this pass cannot see — and when that file turns out to be absent too, `curios run` refuses it by name before anything elaborates.
        if let Some(default) = &self.default
            && default != &name
            && !executables
                .iter()
                .any(|executable| &executable.name == default)
        {
            return Err(Refused::of("unknown executable")
                .stating(format_args!("`default` names `{default}`"))
                .stating("this package declares no executable of that name")
                .value(["default"]));
        }

        let mut foreign = Vec::new();
        for (index, row) in self.foreign.unwrap_or_default().into_iter().enumerate() {
            let placed = |refused: Refused| refused.under([Step::key("foreign"), Step::Row(index)]);
            foreign.push(row.foreign(&name, &foreign).map_err(placed)?);
        }

        Ok(Package {
            name,
            compiler: self.compiler.map(|version| version.parse()).transpose()?,
            format: self.format,
            description: self.description,
            default: self.default,
            dependencies: table(self.dependencies.unwrap_or_default(), "dependencies")?,
            executables,
            foreign,
        })
    }

    /// This document read as an umbrella.
    fn umbrella(self) -> Result<Umbrella, Refused> {
        let catalog = table(self.catalog.unwrap_or_default(), "catalog")?;

        // A catalog row is what a member's marker resolves *to*, so it cannot be a marker itself: umbrellas do not nest, and nothing sits above one to answer a marker it wrote.
        if let Some((name, _)) = catalog
            .iter()
            .find(|(_, row)| matches!(row, Dependency::Member | Dependency::Catalog))
        {
            return Err(Refused::of("misplaced marker")
                .stating(format_args!(
                    "the catalog row `{name}` names a marker source"
                ))
                .stating("a catalog row is what a marker resolves to")
                .stating("it names a fetchable source or `path`")
                .value(["catalog", name.as_str(), "source"]));
        }

        Ok(Umbrella {
            compiler: self
                .compiler
                .ok_or_else(|| {
                    Refused::of("missing compiler version")
                        .stating("this umbrella states no `compiler`")
                        .stating(format_args!(
                            "an umbrella states the one its members inherit, as `compiler = \"{}\"`",
                            CompilerVersion::current()
                        ))
                })?
                .parse()?,
            format: self.format,
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
    source: Resolver,
    url: Option<String>,
    rev: Option<String>,
    hash: Option<String>,
    path: Option<String>,
}

impl DependencyRow {
    /// What this row resolves to, or which field the source it names cannot use — or requires.
    fn dependency(self) -> Result<Dependency, Refused> {
        let Self {
            source,
            url,
            rev,
            hash,
            path,
        } = self;

        // Exactly the fields this source takes. Both directions are checked against it, so a field is never quietly ignored and never quietly defaulted: a `rev` on a live row is a pin somebody believed in.
        let takes: &[&str] = match source {
            Resolver::Member | Resolver::Catalog => &[],
            Resolver::Git => &["url", "rev", "hash"],
            Resolver::Path => &["path"],
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
            return Err(Refused::of("surplus field")
                .stating(format_args!("this row is `source = \"{source}\"`"))
                .stating(format_args!("that source takes no `{field}`"))
                .entry([*field]));
        }

        let require = |field: &str, value: Option<String>| {
            value.ok_or_else(|| {
                Refused::of("missing field")
                    .stating(format_args!("this row is `source = \"{source}\"`"))
                    .stating(format_args!("that source requires `{field}`"))
            })
        };

        match source {
            Resolver::Member => Ok(Dependency::Member),
            Resolver::Catalog => Ok(Dependency::Catalog),
            Resolver::Git => Ok(Dependency::Git {
                url: optionless("url", require("url", url)?)?,
                rev: optionless("rev", require("rev", rev)?)?,
                hash: TreeHash::parse(&require("hash", hash)?)
                    .map_err(|said| Refused::saying(said).value(["hash"]))?,
            }),
            Resolver::Path => Ok(Dependency::Path {
                path: require("path", path)?.into(),
            }),
        }
    }
}

/// `value`, refused when it would reach a fetch as an option rather than as itself.
///
/// **`curate` hands a `url` and a `rev` to a fetcher as positional arguments, and both `git` and the file fetcher a `[[foreign]]` row's `url` reaches read a leading `-` as an option wherever it sits.** The hash is what a *delivery* is accepted against, and it is checked after the fetch has already run — so it answers for the bytes and not for the invocation that obtained them. This is the check that answers for the invocation, and it belongs here beside the hash's, because a row is refused where a row is read.
///
/// It matters past the manifest somebody wrote: `curate` walks each fetched dependency's own manifest for further rows, so a pinned tree supplies these fields for fetches on its consumer's machine. Vouching for a tree's bytes is not authorizing it to pass options to that consumer's `git`.
///
/// A leading `-` and nothing more. Demanding a full object name would forbid pinning by tag or branch, which is a decision about what a pin may be rather than a repair — and `--` is no alternative to either, since `git checkout --detach -- <rev>` reads what follows as a path.
fn optionless(field: &str, value: String) -> Result<String, Refused> {
    match value.starts_with('-') {
        true => Err(Refused::of("invalid value")
            .stating(format_args!("`{field}` begins with `-`"))
            .stating("`curios curate` hands it to a fetcher, which would read it as an option")
            .value([field])),
        false => Ok(value),
    }
}

/// The resolvers a row may name. `member` and `catalog` are markers: each names exactly the umbrella-side list that answers it.
#[derive(Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Resolver {
    Member,
    Catalog,
    Git,
    Path,
}

impl fmt::Display for Resolver {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Member => "member",
            Self::Catalog => "catalog",
            Self::Git => "git",
            Self::Path => "path",
        })
    }
}

/// Where the WebAssembly module implementing a package's `foreign` declarations comes from.
///
/// Exclusive, as a [`Dependency`]'s sources are and for the same reason: a row states one or the other, never both, so nothing has to decide which wins. What the module must *be* is not here — that is the row's `hash`, which both deliveries state, because it answers a different question from where the bytes arrived.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Module {
    /// A file the package carries, at a plain relative path from the manifest. What a small package that would rather commit its module than publish one writes, and what a project building its own into a gitignored directory names.
    Filed(PathBuf),
    /// A file fetched once and filed in the shared store. What a package whose module is too large to live in a source tree writes.
    Fetched(String),
}

/// What a program is built to run on: the native host, or a browser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    Native,
    Browser,
}

impl Target {
    /// Every target, in the order a reader meets them.
    pub const ALL: [Target; 2] = [Target::Native, Target::Browser];

    /// The target's one spelling: what `--target` takes, the `[foreign.<target>]` table a row states its delivery in, and the `.curios/<target>/` entry its products are written under.
    pub fn spelled(self) -> &'static str {
        match self {
            Target::Native => "native",
            Target::Browser => "browser",
        }
    }
}

/// The names a browser page holds for itself — its page, its loader and its module — which no file of any package may stand at: a `static` key, a carried gateway's path, or a fetched gateway's `<row>.js`.
pub const PAGE_FILES: [&str; 3] = ["index.html", "index.js", "index.wasm"];

/// One declared provider of `foreign` implementations: what each target links, and the export of it that answers each of the package's declarations it is named for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Foreign {
    /// A single legal identifier, which exists for the same reason an [`Executable`]'s does — so a command can name this row. Nothing in Curios source refers to it: a `foreign` declaration is reached by its own qualified name, never through the module that implements it.
    pub name: String,
    /// The WebAssembly module the native target links, where the row states `[foreign.native]`.
    pub native: Option<Delivery>,
    /// The JavaScript gateway a browser links, and the files its page serves beside it, where the row states `[foreign.browser]`.
    pub browser: Option<Gateway>,
    /// The export that answers each declaration, as `fully qualified path -> export name`: keyed by what is answered, so a row states one export a declaration, and a declaration two rows claim is refused where the manifest is read. The manifest names each from the package's own root, and the qualified path is made of it and the package's name where the row is read (`own_declaration`), so every path here lies under the package's prefix. A row no entry names is the host's to answer, or is refused by name where a program reaches it.
    pub answers: BTreeMap<String, String>,
}

/// One file a target links for a `[[foreign]]` row: where it comes from, and what it must be.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Delivery {
    pub module: Module,
    /// What the file must be, whichever way it arrived.
    ///
    /// **Stated for a carried file too, and that is the case it matters most for.** A fetched one is obviously unvouched-for until it is checked. A carried one *looks* covered by the tree hash its consumer pinned — and is, when the package was fetched from git. But the entry package is hashed against nothing, and `source = "path"` and `member` dependencies are deliberately unpinned, because live code has no pin. That is exactly where a locally built module lives, so without this the common case is the unchecked one.
    pub hash: FileHash,
}

/// What a browser links for a `[[foreign]]` row: its gateway, and the files the page serves beside it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Gateway {
    /// The `.js` module whose exports answer the row's declarations.
    pub delivery: Delivery,
    /// Each file or directory the page serves, by the page path it is served at, beside the package path it is read from, both plain relative: `[foreign.browser.static]`.
    pub statics: BTreeMap<String, PathBuf>,
}

impl Gateway {
    /// Where the gateway of the foreign row `name` stands in a page, from its package's root.
    pub fn stands(&self, name: &str) -> String {
        stands(&self.delivery.module, name)
    }
}

/// Where a gateway delivered as `module` for the foreign row `name` stands in a page, from its package's root: at its path where the package carries it, and at `<name>.js` where it is fetched, since a fetched one has no path in the package.
fn stands(module: &Module, name: &str) -> String {
    match module {
        Module::Filed(path) => path
            .components()
            .map(|component| component.as_os_str().to_string_lossy())
            .collect::<Vec<_>>()
            .join("/"),
        Module::Fetched(_) => format!("{name}.js"),
    }
}

impl Foreign {
    /// What `target` links for this row, where the row states its table.
    pub fn delivery(&self, target: Target) -> Option<&Delivery> {
        match target {
            Target::Native => self.native.as_ref(),
            Target::Browser => self.browser.as_ref().map(|gateway| &gateway.delivery),
        }
    }
}

/// One `[[foreign]]` row as TOML spells it: its name, each target's delivery in a table of its own, and what its exports answer.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ForeignRow {
    name: String,
    native: Option<DeliveryRow>,
    browser: Option<GatewayRow>,
    answers: Option<BTreeMap<String, String>>,
}

/// A `[[foreign]]` row's `[foreign.browser]` table as TOML spells it: a delivery, and the files its page serves.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct GatewayRow {
    path: Option<String>,
    url: Option<String>,
    hash: Option<String>,
    #[serde(rename = "static")]
    statics: Option<BTreeMap<String, String>>,
}

/// One target's table of a `[[foreign]]` row as TOML spells it — the union of every field either delivery takes, for the reason [`DependencyRow`] is a union.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DeliveryRow {
    path: Option<String>,
    url: Option<String>,
    hash: Option<String>,
}

impl Delivery {
    /// The file's bytes, accepted against this delivery's `hash`, for the row `name`'s table of `target`.
    ///
    /// **One check for both deliveries, at the moment the bytes are used.** A fetched module was already accepted once, when `curate` placed it — checking again costs one digest over a file about to be compiled, which is nothing beside that, and it means a store entry corrupted after the fact is caught rather than linked. A carried module has no fetch to have been checked at, so this is its only check and the reason the row states a hash at all.
    ///
    /// `directory` is the package's own, which a carried module's path is relative to; `store` answers for a fetched one, where the entry's name is already the digest.
    pub fn bytes(
        &self,
        name: &str,
        target: Target,
        directory: &Path,
        store: &Store,
    ) -> Result<Vec<u8>, String> {
        let path = match &self.module {
            Module::Filed(path) => directory.join(path),
            Module::Fetched(_) => store.foreign(&self.hash),
        };

        let bytes = fs::read(&path).map_err(|error| {
            format!(
                "the foreign module {name:?} is not at {}: {error}",
                path.display()
            )
        })?;

        let delivered = FileHash::of_bytes(&bytes);
        if delivered != self.hash {
            // The two conditions rather than one instruction: a re-pin is the ordinary answer here and the alarming one, and which it is depends on something only the reader knows.
            return Err(format!(
                "the foreign module {name:?} at {} is not what it is pinned to\n  expected {}\n  delivered {delivered}\n  this is intended if: you rebuilt the module from source you control — then `{} --refresh`\n  pay close attention if: you did not rebuild it, or a dependency delivered it rather than you building it",
                path.display(),
                self.hash,
                Subject::Foreign(target).invocation(name),
            ));
        }

        Ok(bytes)
    }
}

impl ForeignRow {
    /// This row as a declared provider of the package `package`, against the ones already declared before it.
    fn foreign(self, package: &str, declared: &[Foreign]) -> Result<Foreign, Refused> {
        let Self {
            name,
            native,
            browser,
            answers,
        } = self;

        let name = canonical(&name).map_err(|refused| refused.value(["name"]))?;

        if declared.iter().any(|foreign| foreign.name == name) {
            return Err(Refused::of("duplicate foreign row")
                .stating(format_args!("`{name}` is declared twice")));
        }

        if native.is_none() && browser.is_none() {
            return Err(Refused::of("missing module")
                .stating(format_args!("`{name}` names no module"))
                .stating("state `[foreign.native]` for what the native target links")
                .stating("or `[foreign.browser]` for what a browser does")
                .stating("each with a `path` for a file the package carries or a `url` for one it fetches, beside its `hash`"));
        }
        let native = native
            .map(|row| row.delivery(&name, Target::Native))
            .transpose()
            .map_err(|refused| refused.under(["native"]))?;
        let browser = browser
            .map(|row| row.gateway(&name))
            .transpose()
            .map_err(|refused| refused.under(["browser"]))?;

        // Each declaration is claimed once across the package's rows: nothing chooses between two answers, and the rows of one manifest are where two claims on one declaration can be written.
        let mut claimed = BTreeMap::new();
        for (spelled, export) in answers.unwrap_or_default() {
            let answer = |refused: Refused| refused.entry(["answers", spelled.as_str()]);
            let declaration = own_declaration(package, &spelled, &name).map_err(answer)?;

            if let Some(earlier) = declared
                .iter()
                .find(|foreign| foreign.answers.contains_key(&declaration))
            {
                return Err(answer(
                    Refused::of("duplicate answer")
                        .stating(format_args!(
                            "`{}` and `{name}` both name an export that answers `{declaration}`",
                            earlier.name
                        ))
                        .stating("a declaration is answered by one export of one module"),
                ));
            }

            claimed.insert(declaration, export);
        }

        Ok(Foreign {
            name,
            native,
            browser,
            answers: claimed,
        })
    }
}

impl DeliveryRow {
    /// This table of the foreign row `name`, the one for `target`, as what that target links.
    fn delivery(self, name: &str, target: Target) -> Result<Delivery, Refused> {
        let Self { path, url, hash } = self;

        delivered(path, url, hash, name, target)
    }
}

impl GatewayRow {
    /// This `[foreign.browser]` table of the foreign row `name`, as what a browser links for it.
    ///
    /// **The page's own names are refused here, for every package.** A gateway the package carries stands at its path in the page, a fetched one at `<row>.js`, and a `static` key at itself, so each of [`PAGE_FILES`] is refused where the manifest is read rather than where a page is put together, and in a library as in a program, so no package can come to depend on standing there.
    fn gateway(self, name: &str) -> Result<Gateway, Refused> {
        let Self {
            path,
            url,
            hash,
            statics,
        } = self;

        let delivery = delivered(path, url, hash, name, Target::Browser)?;
        held_by_the_page(
            &stands(&delivery.module, name),
            &format!("the gateway of the foreign row `{name}`"),
        )?;

        let mut served = BTreeMap::new();
        for (page, package) in statics.unwrap_or_default() {
            let subject = format!("the static file `{page}` of the foreign row `{name}`");
            let entry = |refused: Refused| refused.entry(["static", page.as_str()]);
            let valued = |refused: Refused| refused.value(["static", page.as_str()]);

            plain_static(&page, &subject, "page path").map_err(entry)?;
            held_by_the_page(&page, &subject).map_err(entry)?;
            let read = plain_static(&package, &subject, "package path").map_err(valued)?;
            // The store is the toolchain's, and a page that served it would serve every build product the project holds.
            if read.starts_with(STORE) {
                return Err(Refused::of("invalid path")
                    .stating(format_args!(
                        "{subject} is read from `{package}`, under `{STORE}`"
                    ))
                    .stating("that directory is the toolchain's, and no file a page serves")
                    .value(["static", page.as_str()]));
            }

            served.insert(page, read);
        }

        Ok(Gateway {
            delivery,
            statics: served,
        })
    }
}

/// Refuse a page path `stands` that one of [`PAGE_FILES`] is, for `subject`.
fn held_by_the_page(stands: &str, subject: &str) -> Result<(), Refused> {
    match PAGE_FILES.contains(&stands) {
        true => Err(Refused::of("reserved path")
            .stating(format_args!("{subject} would stand at `{stands}`"))
            .stating("the page holds that path for itself")
            .stating(format_args!(
                "no file of a package may stand at {}",
                PAGE_FILES.map(|file| format!("`{file}`")).join(", ")
            ))),
        false => Ok(()),
    }
}

/// `path`, one side of a `static` entry for `subject`, as a plain relative path naming something: no `.`, `..`, leading `/` or empty spelling.
fn plain_static(path: &str, subject: &str, side: &str) -> Result<PathBuf, Refused> {
    let spelled = PathBuf::from(path);

    match spelled
        .components()
        .find(|component| !matches!(component, Component::Normal(_)))
    {
        _ if path.is_empty() => Err(Refused::of("invalid path")
            .stating(format_args!("{subject} has an empty {side}"))),
        Some(component) => Err(Refused::of("invalid path")
            .stating(format_args!("{subject} has the {side} `{path}`"))
            .stating(format_args!(
                "its `{}` is no plain relative path",
                component.as_os_str().to_string_lossy()
            ))
            .stating("a `static` entry names a page path and a file of the package, each with no `.`, `..` or leading `/`")),
        None => Ok(spelled),
    }
}

/// The delivery a target's table states, for the foreign row `name`.
fn delivered(
    path: Option<String>,
    url: Option<String>,
    hash: Option<String>,
    name: &str,
    target: Target,
) -> Result<Delivery, Refused> {
    let invocation = Subject::Foreign(target).invocation(name);
    let target = target.spelled();

    // Where the file comes from: one spelling or the other, checked in both directions so neither is quietly ignored nor quietly defaulted — `DependencyRow`'s rule, applied to the other thing a manifest delivers.
    let module = match (path, url) {
        (Some(path), None) => Module::Filed(plain_relative(&path, name)?),
        (None, Some(url)) => Module::Fetched(optionless("url", url)?),
        (Some(_), Some(_)) => {
            return Err(Refused::of("conflicting fields")
                .stating(format_args!(
                    "`{name}` states `path` beside `url` in `[foreign.{target}]`"
                ))
                .stating("a file is carried by the package or fetched, never both")
                .entry(["url"]));
        }
        (None, None) => {
            return Err(Refused::of("missing field")
                .stating(format_args!(
                    "`{name}` names no file in `[foreign.{target}]`"
                ))
                .stating("state `path` for one the package carries, or `url` for one it fetches"));
        }
    };

    // Both deliveries state it, because it answers what the file *is* rather than where it came from — and the carried case is the one that would otherwise go unchecked, since the tree holding it may be the entry package or a live path dependency, neither of which is pinned.
    let Some(hash) = hash else {
        return Err(Refused::of("missing hash")
            .stating(format_args!(
                "`{name}` states no `hash` in `[foreign.{target}]`"
            ))
            .stating("a file is accepted against one however it arrives")
            .trying(format!("{invocation} --refresh")));
    };

    Ok(Delivery {
        module,
        hash: FileHash::parse(&hash).map_err(|said| Refused::saying(said).value(["hash"]))?,
    })
}

/// The qualified name of the declaration `spelled` names in the library of the package `package`, for the foreign row `row`.
///
/// **A module answers its own package's declarations, and the spelling is what makes that so.** The path is read from the package's root, plain and relative as an executable's is, so every declaration it can name lies under the package's own prefix: a claim on another package's row, or on the standard library's, has no spelling, and nothing later has to hold a claim to its claimant. An absolute path is refused here, where the manifest is read.
fn own_declaration(package: &str, spelled: &str, row: &str) -> Result<String, Refused> {
    if let Some(absolute) = spelled.strip_prefix('/') {
        let own = absolute
            .strip_prefix(package)
            .and_then(|rest| rest.strip_prefix('/'));

        return Err(match own {
            Some(relative) => Refused::of("unrooted name")
                .stating(format_args!(
                    "`{row}` says an export answers `{spelled}`"
                ))
                .stating("a declaration is named from the package's own root")
                .stating(format_args!("write `{relative}`")),
            None => Refused::of("unknown declaration")
                .stating(format_args!(
                    "`{row}` says an export answers `{spelled}`"
                ))
                .stating(format_args!("that is no declaration of `/{package}`"))
                .stating("a module answers its own package's declarations, each named from the package's root")
                .stating(format_args!(
                    "`digest/sha256` names `/{package}/digest/sha256`"
                )),
        });
    }

    for segment in spelled.split('/') {
        canonical(segment)?;
    }

    Ok(format!("/{package}/{spelled}"))
}

/// A path a row names inside its own package, refused where it is not one — the rule an executable's `path` keeps, stated once for both.
fn plain_relative(path: &str, subject: &str) -> Result<PathBuf, Refused> {
    let path = PathBuf::from(path);

    match path
        .components()
        .find(|component| !matches!(component, Component::Normal(_)))
    {
        Some(component) => Err(Refused::of("invalid path")
            .stating(format_args!(
                "`{subject}` names the module `{}`",
                path.display()
            ))
            .stating(format_args!(
                "its `{}` is no plain relative path",
                component.as_os_str().to_string_lossy()
            ))
            .stating("a row names a file inside the package by its path from the manifest, with no `.`, `..` or leading `/`")
            .value(["path"])),
        None => Ok(path),
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
    fn executable(self, declared: &[Executable]) -> Result<Executable, Refused> {
        let Self { name, path } = self;

        if !is_identifier(&name) {
            return Err(Refused::of("invalid name")
                .stating(format_args!(
                    "the executable `{name}` is not named by one identifier"
                ))
                .stating("`curios run` could not tell it from a file argument")
                .value(["name"]));
        }

        // One rule for every row, including the package's own: a declared executable is compiled from the file named after it. `exe.crs` is not a default here — it is what `Package::discovered` finds when *no* row claims the package's name, so writing a row always means what it says and never silently redirects an existing package's file.
        let path = path.map_or_else(
            || PathBuf::from(format!("{name}.{EXTENSION}")),
            PathBuf::from,
        );

        // A row names a file inside its package by the path the package spells, plainly: every check below compares that spelling, and `./lib.crs` or `app/../lib.crs` would walk past the two refusals under it to fail later as a parse error inside the header (law 4). Resolving the spelling instead would resolve `..` lexically through symlinks the package cannot see.
        if let Some(component) = path
            .components()
            .find(|component| !matches!(component, Component::Normal(_)))
        {
            return Err(Refused::of("invalid path")
                .stating(format_args!(
                    "the executable `{name}` is compiled from `{}`",
                    path.display()
                ))
                .stating(format_args!(
                    "its `{}` is no plain relative path",
                    component.as_os_str().to_string_lossy()
                ))
                .stating("a row names a file inside the package by its path from the manifest, with no `.`, `..` or leading `/`")
                .value(["path"]));
        }

        if let Some(previous) = declared.iter().find(|executable| executable.name == name) {
            return Err(Refused::of("duplicate executable")
                .stating(format_args!("`{name}` is declared twice"))
                .stating(format_args!(
                    "from `{}` and from `{}`",
                    previous.path.display(),
                    path.display()
                )));
        }

        // The library header is the library's root, so no program is compiled from it.
        if path == Path::new(LIBRARY) {
            return Err(Refused::of("misplaced executable")
                .stating(format_args!("`{name}` is compiled from `{LIBRARY}`"))
                .stating("that file is the package's library header"));
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

/// Every row of the table `key` names, by the canonical name each is filed under.
fn table(
    rows: BTreeMap<String, DependencyRow>,
    key: &str,
) -> Result<BTreeMap<String, Dependency>, Refused> {
    rows.into_iter()
        .map(|(written, row)| {
            let placed = |refused: Refused| refused.under([key, written.as_str()]);

            Ok((
                canonical(&written).map_err(placed)?,
                row.dependency().map_err(placed)?,
            ))
        })
        .collect()
}

/// The canonical name `written` states, refused when it is not one.
///
/// **A name is one word, and that is the whole rule.** It is the prefix the package mounts at, and a prefix is an atom: `myorg` in a hypothetical `myorg/json` would denote nothing on its own, so admitting segments here would make every package silently declare namespaces nobody wrote. A segment of names is a real thing to want, but it belongs to a package's own modules, where somebody declares each one.
pub fn canonical(written: &str) -> Result<String, Refused> {
    let refuse = |reason: &str| {
        Refused::of("invalid name")
            .stating(format_args!("`{written}` is no name a path could spell"))
            .stating(reason)
            .stating("a name is one identifier: no dashes, no `/`, no keyword")
    };

    match written {
        "" => Err(refuse("it is empty")),
        // `is_identifier` rejects a `/` along with everything else a segment could not be, so the one-word rule needs no separate check for it.
        _ if !is_identifier(written) => Err(refuse("it is no identifier")),
        _ if is_keyword(written) => Err(refuse("it is a keyword")),
        _ => Ok(written.to_string()),
    }
}
