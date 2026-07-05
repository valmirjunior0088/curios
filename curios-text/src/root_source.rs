use {
    super::{Error, Module, Qualifier},
    std::path::PathBuf,
};

/// Where a compile's modules, beyond the entrypoint's own inline items, are
/// served from. A closed enum, not a trait: every shape a compile actually
/// needs is one of these three, so there is no polymorphism to buy by making
/// this an interface — `sys`/`syn`/`std` are always embedded together (no
/// compile ever wants `sys` without `syn`/`std`, so one `Prelude` variant
/// covers all three, rather than a decorator chain of one struct per root),
/// and the one remaining axis of variation (does anything else resolve from
/// disk?) is `FileSystem` vs. `None`.
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
    pub fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
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
        }
    }

    /// Labels of modules this source always serves at the entrypoint root.
    /// Discovery synthesizes a `pub mod <label>;` declaration for each, so
    /// they are loaded and resolvable without the entrypoint declaring them.
    ///
    /// `sys` comes first: root order is flat-item lowering order, which is
    /// the topological-sort tiebreak — and nothing references witness items
    /// by name, so only their position gets the sys operator witnesses
    /// emitted (and registered) before any std item that uses infix
    /// elaborates. `std` before `syn` matches the historical decorator-chain
    /// composition order and is otherwise arbitrary (`std` and `syn`
    /// genuinely cross-reference each other — see `to_core::order_flat_items`'s
    /// doc comment — so nothing but this tiebreak depends on their relative
    /// order).
    pub fn roots(&self) -> Vec<&'static str> {
        match self {
            RootSource::Prelude { base, .. } => ["sys", "std", "syn"]
                .into_iter()
                .chain(base.roots())
                .collect(),
            RootSource::None | RootSource::FileSystem(_) => Vec::new(),
        }
    }
}
