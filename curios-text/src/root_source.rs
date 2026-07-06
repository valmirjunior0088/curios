use {
    super::{Error, Module},
    curios_abi::Roster,
    curios_core::Qualifier,
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// Where a compile's modules, beyond the entrypoint's own inline items, are
/// served from. A closed enum, not a trait: every shape a compile actually
/// needs is one of these, so there is no polymorphism to buy by making this
/// an interface — `sys`/`syn`/`std` are always embedded together (no compile
/// ever wants `sys` without `syn`/`std`, so one `Prelude` variant covers all
/// three, rather than a decorator chain of one struct per root), and named
/// path dependencies are likewise one `Dependencies` variant covering all of
/// them at once (each is already its own root, so there is no ordering
/// concern a decorator chain would need to resolve).
pub enum RootSource {
    /// No further modules resolve — every `mod` declaration in the program
    /// must carry an inline body. Used by tests exercising resolution logic
    /// in isolation, and as the innermost source a real compile's `Prelude`
    /// wraps when the entrypoint declares no modules of its own.
    None,
    /// Disk-backed modules rooted at a directory: qualifier `a/b/c` reads
    /// `base/a/b/c.crs`.
    FileSystem(PathBuf),
    /// The three privileged roots (`sys`/`syn`/`std`), embedded in the
    /// binary — `sys` built once from the compilation's foreign store, `syn`
    /// and `std` parsed once per thread from `include_str!`-embedded source
    /// (see `prelude::load_embedded`) — wrapping `base` for anything else.
    Prelude { sys: Module, base: Box<RootSource> },
    /// Named path dependencies: each is its own root, keyed by name, pairing
    /// its already-parsed root module (the file the caller pointed at, like
    /// the entrypoint itself — see `dependency_from_path`) with a source for
    /// its own nested `mod` children. Wraps `base` for anything else.
    Dependencies {
        deps: BTreeMap<String, (Module, RootSource)>,
        base: Box<RootSource>,
    },
}

fn module_file_path(base: &std::path::Path, qualifier: &Qualifier) -> PathBuf {
    let mut segments = qualifier.iter().collect::<Vec<_>>();
    let label = segments.pop().unwrap();

    segments
        .into_iter()
        .fold(base.to_path_buf(), |path, segment| path.join(segment))
        .join(format!("{label}.crs"))
}

impl RootSource {
    pub(crate) fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        match self {
            RootSource::None => Err(Error::ModuleNotFound {
                path: qualifier.join(),
            }),
            RootSource::FileSystem(base) => Module::from_path(module_file_path(base, qualifier))
                .map_err(|cause| Error::ModuleLoadFailed {
                    label: qualifier.join(),
                    cause: Box::new(cause),
                }),
            RootSource::Prelude { sys, base } => {
                match super::prelude::load_embedded(sys, qualifier) {
                    Some(module) => Ok(module),
                    None => base.load(qualifier),
                }
            }
            RootSource::Dependencies { deps, base } => match deps.get(qualifier.root_segment()) {
                // The dependency's own root — already parsed, like `sys` is.
                Some((module, _)) if qualifier.is_single() => Ok(module.clone()),
                // One of its nested `mod` children — strip the dependency's
                // own leading segment (its source has no notion of its own
                // name) and delegate to its own source.
                Some((_, source)) => source.load(&qualifier.without_first()),
                None => base.load(qualifier),
            },
        }
    }

    /// Labels of modules this source always serves at the entrypoint root.
    /// Discovery synthesizes a `pub mod <label>;` declaration for each, so
    /// they are loaded and resolvable without the entrypoint declaring them.
    ///
    /// Privilege order: root order is flat-item lowering order, which is the
    /// topological-sort tiebreak. `sys` (the primitives) comes first; `syn`
    /// (the names the compiler emits — the operator concepts, the
    /// string-literal and `!` desugaring targets) precedes `std` so those
    /// names lower before the library code that elaborates against them. The
    /// operator *witnesses* live at the head of `std`'s own index (nothing
    /// references witness items by name, so only position registers them
    /// before the pervasively-infix std/syn items); a goal that still
    /// elaborates early defers to their later registration. `std` and `syn`
    /// genuinely cross-reference each other — see `to_core::order_flat_items`'s
    /// doc comment — so genuine name dependencies reorder regardless of this
    /// tiebreak.
    pub(crate) fn roots(&self) -> Vec<&str> {
        match self {
            RootSource::Prelude { base, .. } => ["sys", "syn", "std"]
                .into_iter()
                .chain(base.roots())
                .collect(),
            // Dependency names carry no privilege tiebreak of their own (unlike
            // `sys`/`syn`/`std`) — sorted-by-name (`deps` is a `BTreeMap`) is as
            // good an order as any, and keeps it deterministic.
            RootSource::Dependencies { deps, base } => deps
                .keys()
                .map(String::as_str)
                .chain(base.roots())
                .collect(),
            RootSource::None | RootSource::FileSystem(_) => Vec::new(),
        }
    }

    /// This source's root roster: `sys`/`syn`/`std` are always fixed
    /// (`Roster::new` seeds them itself), so only `roots()`'s non-reserved
    /// names — a `Dependencies` layer's own names — are passed through as
    /// named path dependencies.
    pub(crate) fn roster(&self) -> Roster {
        Roster::new(
            self.roots()
                .into_iter()
                .filter(|name| !matches!(*name, "sys" | "syn" | "std"))
                .map(String::from),
        )
    }

    /// Build a `Dependencies` layer naming `deps`, wrapping `base` for
    /// anything else. Rejects a duplicate name, or one colliding with a
    /// reserved root (`sys`/`syn`/`std`) — the point `Roster::new` assumes
    /// is already true of whatever it's handed.
    pub fn dependencies(
        deps: Vec<(String, Module, RootSource)>,
        base: RootSource,
    ) -> Result<RootSource, Error> {
        let mut by_name = BTreeMap::new();

        for (name, module, source) in deps {
            if matches!(name.as_str(), "sys" | "syn" | "std") || by_name.contains_key(&name) {
                return Err(Error::DuplicateRoot { name });
            }

            by_name.insert(name, (module, source));
        }

        Ok(RootSource::Dependencies {
            deps: by_name,
            base: Box::new(base),
        })
    }

    /// Parse `path` as a dependency's own root module, paired with a
    /// filesystem source for its own nested `mod` children rooted at its
    /// parent directory — mirrors the split `curios::compile::load` already
    /// makes for the entrypoint itself.
    pub fn dependency_from_path(path: &Path) -> Result<(Module, RootSource), Error> {
        let module = Module::from_path(path).map_err(|cause| Error::ModuleLoadFailed {
            label: path.display().to_string(),
            cause: Box::new(cause),
        })?;
        let source = RootSource::FileSystem(path.parent().unwrap_or(Path::new(".")).to_path_buf());

        Ok((module, source))
    }
}
