//! Where a unit's module bodies come from.
//!
//! **The layout rule:** `mod x` declared in a namespace's header resolves to `x.crs` in that namespace's directory, and a header's namespace directory is its stem directory. `mod util` in `<dir>/main.crs` reads `<dir>/main/util.crs`, and `util`'s own children read from `<dir>/main/util/`. One rule governs every file in the language, so the file handed to `run` is a header like any other and declaring a file never changes what its `mod`s mean.
//!
//! A stem is never part of a name. `<dir>` and `main` are spelling, and `/util` is the qualifier — which is why [`RootSource::mounted`] takes the header and the directory as two arguments rather than deriving one from the other: a package's library header sits beside its manifest while its namespace *is* the manifest's directory, and that exception is the manifest's to state, not this crate's to guess. See `curios-package`'s `layout` module.

use {
    super::{Error, LoadError, Module},
    curios_utilities::{Mount, Qualifier, RootKind, Source, is_identifier},
    std::{
        cell::RefCell,
        collections::BTreeMap,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// The modules a unit is lowered from: one base per prefix it mounts.
///
/// **A source is a resolver, never an assumed filesystem.** Turning a qualifier into a module is the whole contract, which is why a directory and a tree already in memory are two bases here rather than two kinds of unit. `curios-js` supplies every body inline and compiles with no filesystem at all; a build script hands the fixed prelude over already parsed; a package read from disk is read lazily, one header at a time. Fetching stays separable for the same reason — whatever placed the bytes, what arrives here is a base.
///
/// Lookup is longest-match over the claimed prefixes, exactly as [`Mount::owning`] is everywhere else, and the mounts of one source are pairwise disjoint because a unit claims each of its prefixes once.
pub struct RootSource {
    bases: Vec<(Mount, Base)>,
    /// Text consulted before the disk for every file this source would read. See [`Overlay`].
    overlay: Overlay,
    /// Every file this source has read, by the canonical path it was read from. See [`RootSource::reads`].
    ///
    /// Interior mutability because resolution is a `&self` operation everywhere above this, and recording what was read is not a reason to thread `&mut` through the lowering. Nothing here was ever `Send` — a module holds `Rc<Source>` spans — so this costs no bound that was not already spent.
    reads: RefCell<BTreeMap<PathBuf, Rc<Source>>>,
}

/// Where one mount's modules are.
enum Base {
    /// Read from disk. `header` is the file the mount's own namespace is declared in — `None` for the entry program, whose header is its [`Entrypoint`](crate::Entrypoint) and so is never read twice — and everything below the prefix reads from `directory`: `<prefix>/a/b` is `<directory>/a/b.crs`.
    Disk {
        header: Option<PathBuf>,
        directory: PathBuf,
    },
    /// Supplied already parsed, by canonical qualifier, the mount's own header filed under the prefix itself.
    Supplied(BTreeMap<Qualifier, Module>),
}

impl RootSource {
    /// The entry program with no filesystem: every `mod` it declares must carry an inline body. Used by tests exercising resolution in isolation, and by embedders (`curios-js`) with no filesystem at all.
    pub fn none() -> Self {
        Self::over(vec![(entry_mount(), Base::Supplied(BTreeMap::new()))])
    }

    /// The entry program written in `header`, its own modules under that header's stem directory: `mod util` in `<dir>/main.crs` reads `<dir>/main/util.crs`.
    ///
    /// `header` is never read — the entry's own body is its [`Entrypoint`](crate::Entrypoint), which the caller already has — so the path is here for the directory it names, and an entrypoint parsed from a string can still be given one.
    pub fn entry(header: &Path) -> Self {
        Self::over(vec![(
            entry_mount(),
            Base::Disk {
                header: None,
                directory: header
                    .parent()
                    .unwrap_or(Path::new("."))
                    .join(header.file_stem().unwrap_or_default()),
            },
        )])
    }

    /// A unit mounted at `prefix`, declared in `header` and holding its modules under `directory`.
    ///
    /// The two paths are separate because the layout rule relates them rather than deriving one from the other: a package's library header sits beside its manifest while its namespace *is* the manifest's directory, and every other header's namespace is its stem directory.
    pub fn mounted(
        prefix: &str,
        kind: RootKind,
        header: impl Into<PathBuf>,
        directory: impl Into<PathBuf>,
    ) -> Self {
        Self::over(vec![(
            Mount::new(Qualifier::from([prefix]), kind),
            Base::Disk {
                header: Some(header.into()),
                directory: directory.into(),
            },
        )])
    }

    /// A unit's modules supplied already parsed, claiming nothing until [`insert_root`](Self::insert_root) says so. Nothing is read, which is what lets a build script or an embedder hand a whole unit over.
    pub fn supplied() -> Self {
        Self::over(Vec::new())
    }

    /// A source over `bases`, having read nothing yet.
    fn over(bases: Vec<(Mount, Base)>) -> Self {
        Self {
            bases,
            overlay: Overlay::default(),
            reads: RefCell::new(BTreeMap::new()),
        }
    }

    /// This source with `overlay` consulted before the disk on every read.
    pub fn with_overlay(self, overlay: Overlay) -> Self {
        Self { overlay, ..self }
    }

    /// Claim `prefix` as a supplied root, `module` being the header it is declared in.
    ///
    /// One segment, because a root's name is one word: a mount prefix is an atom, and a name with segments in it would imply namespaces nobody declared. Only the entry's mount is shorter — it claims the empty prefix, which is what makes it the entry.
    pub fn insert_root(&mut self, prefix: &str, kind: RootKind, module: Module) {
        // A caller's broken contract, not a program's: `Qualifier::from(["a/b"])` would build one segment spelling two, and every path derived from it would then be wrong in a way no later check looks for.
        assert!(
            is_identifier(prefix),
            "a root's name is one identifier, and '{prefix}' is not"
        );

        let prefix = Qualifier::from([prefix]);
        assert!(
            !self.bases.iter().any(|(mount, _)| mount.prefix == prefix),
            "root '{}' is already claimed",
            prefix.join()
        );

        let mut modules = BTreeMap::new();
        modules.insert(prefix.clone(), module);
        self.bases
            .push((Mount::new(prefix, kind), Base::Supplied(modules)));
    }

    /// File `module` at `path`, under the supplied root that owns it.
    pub fn insert_module(&mut self, path: Qualifier, module: Module) {
        assert!(!path.is_root(), "a supplied module path cannot be the root");

        let joined = path.join();
        let Some((_, Base::Supplied(modules))) = self.owning_mut(&path) else {
            panic!("no supplied root claims '{joined}'");
        };

        assert!(
            modules.insert(path, module).is_none(),
            "module '{joined}' is already filed"
        );
    }

    /// Every file this source has read so far, by canonical path, with the text that was parsed from it.
    ///
    /// **This is a unit's input set, and it is closed.** A file can only join it by being declared `mod x` in a header, and that header is itself a read — so nothing can enter the set without changing something already in it. Cross-unit references read nothing at all: `Self::owning` claims only qualifiers inside this source's own mounts, so `use /json/Value` resolves against a compiled predecessor rather than a file. Together those are why a cache can verify a stored unit by re-reading exactly this list and needs no record of what was *looked for and not found*.
    ///
    /// Empty until something is resolved, and empty forever for a source supplied already parsed — which is why the fixed prelude has an archive of its own rather than a place in such a store.
    ///
    /// Canonical paths, so the same file reached through a relative invocation and an absolute one is one entry. The consequence is that moving a project invalidates its records, which costs one recompile and then re-records.
    pub fn reads(&self) -> Vec<(PathBuf, Rc<Source>)> {
        self.reads
            .borrow()
            .iter()
            .map(|(path, source)| (path.clone(), Rc::clone(source)))
            .collect()
    }

    /// The directories this source reads its modules from.
    ///
    /// Empty for a source supplied already parsed: there is nothing on disk, which is why the fixed prelude has an archive of its own.
    pub fn directories(&self) -> Vec<&Path> {
        self.bases
            .iter()
            .filter_map(|(_, base)| match base {
                Base::Disk { directory, .. } => Some(directory.as_path()),
                Base::Supplied(_) => None,
            })
            .collect()
    }

    /// The prefixes this source claims.
    pub fn mounts(&self) -> Vec<Mount> {
        self.bases.iter().map(|(mount, _)| mount.clone()).collect()
    }

    /// The module `qualifier` names, from whichever base claims it.
    pub(crate) fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        let missing = || Error::ModuleNotFound {
            path: qualifier.join(),
        };

        let Some((mount, base)) = self.owning(qualifier) else {
            return Err(missing());
        };

        match base {
            Base::Supplied(modules) => modules.get(qualifier).cloned().ok_or_else(missing),
            Base::Disk { header, directory } => {
                let path = match qualifier == &mount.prefix {
                    true => header.clone().ok_or_else(missing)?,
                    false => file(directory, qualifier, mount.prefix.segments().len()),
                };

                let (module, source) = self
                    .overlay
                    .read(&path)
                    .unwrap_or_else(|| Module::read(&path))
                    .map_err(|cause| Error::ModuleLoadFailed {
                        label: qualifier.join(),
                        cause: Box::new(cause),
                    })?;

                // Recorded after the read succeeded and canonicalized against the file that answered it, so the record names something that existed and names it the one way. A path that will not canonicalize was read anyway, so it is recorded as given rather than dropped: a record short one entry would let a cache confirm a unit against less than it was compiled from.
                self.reads
                    .borrow_mut()
                    .insert(path.canonicalize().unwrap_or(path), source);

                Ok(module)
            }
        }
    }

    /// The base whose prefix `qualifier` most specifically lies within.
    fn owning(&self, qualifier: &Qualifier) -> Option<(&Mount, &Base)> {
        self.bases
            .iter()
            .filter(|(mount, _)| qualifier.is_within(&mount.prefix))
            .max_by_key(|(mount, _)| mount.prefix.segments().len())
            .map(|(mount, base)| (mount, base))
    }

    fn owning_mut(&mut self, qualifier: &Qualifier) -> Option<(&Mount, &mut Base)> {
        self.bases
            .iter_mut()
            .filter(|(mount, _)| qualifier.is_within(&mount.prefix))
            .max_by_key(|(mount, _)| mount.prefix.segments().len())
            .map(|(mount, base)| (&*mount, base))
    }
}

impl Default for RootSource {
    fn default() -> Self {
        Self::supplied()
    }
}

/// Text standing in for files: `path → text`, consulted before the disk by every [`RootSource`] it is handed to, and by the entry opened through [`Entrypoint::overlaid`](crate::Entrypoint::overlaid).
///
/// **One door for every source.** An editor holds documents the disk does not have yet, and a program on standard input has no file at all; both reach the compiler as an overlay rather than as a second way to read, so the path a language server takes is the path the one-shot query is tested on. A read that misses the overlay falls through to the disk unchanged, and a read that hits is recorded exactly as a disk read is — under the path, with the text that was parsed — so what a compilation was verified against stays one list.
///
/// Keys are compared by [`identity`]: the canonical path when the file exists, and the canonical parent joined with the file name when it does not yet — which is the case an unsaved new module is.
#[derive(Clone, Default)]
pub struct Overlay {
    texts: Rc<BTreeMap<PathBuf, String>>,
}

impl Overlay {
    /// An overlay holding `texts`.
    pub fn of(texts: impl IntoIterator<Item = (PathBuf, String)>) -> Self {
        Self {
            texts: Rc::new(
                texts
                    .into_iter()
                    .map(|(path, text)| (identity(&path), text))
                    .collect(),
            ),
        }
    }

    /// The text held for `path`, if any.
    pub fn get(&self, path: &Path) -> Option<&str> {
        self.texts.get(&identity(path)).map(String::as_str)
    }

    /// Whether any document held lies under one of `directories` — the question a caller asks before believing something else about what those directories contain.
    ///
    /// Answered here because the comparison is against keys this type spells by [`identity`], and a caller comparing raw paths against them would be comparing two conventions. `directories` is spelled the same way before the containment test, so however a manifest walk wrote a directory it meets the key the same path was stored under.
    pub fn reaches(&self, directories: &[&Path]) -> bool {
        let directories = directories
            .iter()
            .copied()
            .map(identity)
            .collect::<Vec<_>>();

        self.texts.keys().any(|held| {
            directories
                .iter()
                .any(|directory| held.starts_with(directory))
        })
    }

    /// The module held for `path`, parsed, or `None` to say the disk answers.
    fn read(&self, path: &Path) -> Option<Result<(Module, Rc<Source>), LoadError>> {
        let text = self.get(path)?;
        let source = Source::held(path, text);

        Some(
            Module::parse(&source)
                .map(|module| (module, source))
                .map_err(LoadError::Parse),
        )
    }
}

/// The one spelling two paths to a file share: canonical when the file exists, and the canonical parent joined with the file name when it does not — so a document an editor has not saved yet still meets the `mod` declaration that will read it.
pub fn identity(path: &Path) -> PathBuf {
    if let Ok(canonical) = path.canonicalize() {
        return canonical;
    }

    match (path.parent(), path.file_name()) {
        (Some(parent), Some(name)) => parent
            .canonicalize()
            .map(|parent| parent.join(name))
            .unwrap_or_else(|_| path.to_path_buf()),
        _ => path.to_path_buf(),
    }
}

/// The entry program's mount: the empty prefix, which is what makes it the entry.
fn entry_mount() -> Mount {
    Mount::new(Qualifier::empty(), RootKind::Ordinary)
}

/// The file `qualifier` reads from under `directory`, with its first `claimed` segments spelling the mount rather than a path.
fn file(directory: &Path, qualifier: &Qualifier, claimed: usize) -> PathBuf {
    let mut segments = qualifier.iter().skip(claimed).collect::<Vec<_>>();
    let label = segments.pop().expect("a qualifier below its own mount");

    segments
        .into_iter()
        .fold(directory.to_path_buf(), |path, segment| path.join(segment))
        .join(format!("{label}.crs"))
}
