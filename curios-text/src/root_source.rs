//! Where a unit's module bodies come from.
//!
//! **The layout rule:** `mod x` declared in a namespace's header resolves to `x.crs` in that namespace's directory, and a header's namespace directory is its stem directory. `mod util` in `<dir>/main.crs` reads `<dir>/main/util.crs`, and `util`'s own children read from `<dir>/main/util/`. One rule governs every file in the language, so the file handed to `run` is a header like any other and declaring a file never changes what its `mod`s mean.
//!
//! A stem is never part of a name. `<dir>` and `main` are spelling, and `/util` is the qualifier — which is why [`RootSource::mounted`] takes the header and the directory as two arguments rather than deriving one from the other: a package's library header sits beside its manifest while its namespace *is* the manifest's directory, and that exception is the manifest's to state, not this crate's to guess. See `curios-package`'s `layout` module.

use {
    super::{Error, Module},
    curios_base::{Mount, Qualifier, RootKind},
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// The modules a unit is lowered from: one base per prefix it mounts.
///
/// **A source is a resolver, never an assumed filesystem.** Turning a qualifier into a module is the whole contract, which is why a directory and a tree already in memory are two bases here rather than two kinds of unit. `curios-web` supplies every body inline and compiles with no filesystem at all; a build script hands the fixed prelude over already parsed; a package read from disk is read lazily, one header at a time. Fetching stays separable for the same reason — whatever placed the bytes, what arrives here is a base.
///
/// Lookup is longest-match over the claimed prefixes, exactly as [`Mount::owning`] is everywhere else, and the mounts of one source are pairwise disjoint because a unit claims each of its prefixes once.
pub struct RootSource {
    bases: Vec<(Mount, Base)>,
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
    /// The entry program with no filesystem: every `mod` it declares must carry an inline body. Used by tests exercising resolution in isolation, and by embedders (`curios-web`) with no filesystem at all.
    pub fn none() -> Self {
        Self {
            bases: vec![(entry_mount(), Base::Supplied(BTreeMap::new()))],
        }
    }

    /// The entry program written in `header`, its own modules under that header's stem directory: `mod util` in `<dir>/main.crs` reads `<dir>/main/util.crs`.
    ///
    /// `header` is never read — the entry's own body is its [`Entrypoint`](crate::Entrypoint), which the caller already has — so the path is here for the directory it names, and an entrypoint parsed from a string can still be given one.
    pub fn entry(header: &Path) -> Self {
        Self {
            bases: vec![(
                entry_mount(),
                Base::Disk {
                    header: None,
                    directory: header
                        .parent()
                        .unwrap_or(Path::new("."))
                        .join(header.file_stem().unwrap_or_default()),
                },
            )],
        }
    }

    /// A unit mounted at `prefix`, declared in `header` and holding its modules under `directory`.
    ///
    /// The two paths are separate because the layout rule relates them rather than deriving one from the other: a package's library header sits beside its manifest while its namespace *is* the manifest's directory, and every other header's namespace is its stem directory.
    pub fn mounted(
        prefix: Qualifier,
        kind: RootKind,
        header: impl Into<PathBuf>,
        directory: impl Into<PathBuf>,
    ) -> Self {
        Self {
            bases: vec![(
                Mount::new(prefix, kind),
                Base::Disk {
                    header: Some(header.into()),
                    directory: directory.into(),
                },
            )],
        }
    }

    /// A unit's modules supplied already parsed, claiming nothing until [`insert_root`](Self::insert_root) says so. Nothing is read, which is what lets a build script or an embedder hand a whole unit over.
    pub fn supplied() -> Self {
        Self { bases: Vec::new() }
    }

    /// Claim `prefix` as a supplied root, `module` being the header it is declared in.
    ///
    /// A whole qualifier rather than a segment, because a package's canonical name may be multi-segment and the prefix it mounts at *is* that name.
    pub fn insert_root(&mut self, prefix: impl Into<Qualifier>, kind: RootKind, module: Module) {
        let prefix = prefix.into();
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

                Module::from_path(&path).map_err(|cause| Error::ModuleLoadFailed {
                    label: qualifier.join(),
                    cause: Box::new(cause),
                })
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
