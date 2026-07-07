use {
    super::{Error, Module},
    curios_base::Qualifier,
    std::path::PathBuf,
};

/// Where a compile's modules, beyond the entrypoint's own inline items, are
/// served from. A closed enum, not a trait: every shape a compile actually
/// needs is one of these, so there is no polymorphism to buy by making this
/// an interface — `sys`/`syn`/`std` are always embedded together (no compile
/// ever wants `sys` without `syn`/`std`, so one `Prelude` variant covers all
/// three, rather than a decorator chain of one struct per root).
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
        }
    }

    /// Whether `sys`/`syn`/`std` are attached to this source — gates
    /// `to_core`'s explicit per-root discovery/lowering passes (see
    /// `to_core::FIXED_ROOTS`), since a bare `None`/`FileSystem` loader (a
    /// test exercising resolution logic in isolation) has no prelude at all.
    pub(crate) fn has_embedded_roots(&self) -> bool {
        matches!(self, RootSource::Prelude { .. })
    }
}
