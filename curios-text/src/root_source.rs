use {
    super::{Error, Module, Qualifier},
    curios_abi::Roster,
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
    pub fn roots(&self) -> Vec<&'static str> {
        match self {
            RootSource::Prelude { base, .. } => ["sys", "syn", "std"]
                .into_iter()
                .chain(base.roots())
                .collect(),
            RootSource::None | RootSource::FileSystem(_) => Vec::new(),
        }
    }

    /// This source's root roster: `sys`/`syn`/`std` are always fixed
    /// (`Roster::new` seeds them itself), so only `roots()`'s non-reserved
    /// names — today none, since there is no `Dependencies` variant yet —
    /// are passed through as named path dependencies.
    pub fn roster(&self) -> Roster {
        Roster::new(
            self.roots()
                .into_iter()
                .filter(|name| !matches!(*name, "sys" | "syn" | "std"))
                .map(String::from),
        )
    }
}
