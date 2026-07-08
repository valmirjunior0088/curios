use {
    super::{Error, Module},
    curios_base::Qualifier,
    std::path::PathBuf,
};

/// Where a compile's modules, beyond the entrypoint's own inline items, are
/// served from: the embedded `sys`/`syn`/`std` roots (`sys` built once from
/// the compilation's foreign store, `syn`/`std` parsed once per thread from
/// `include_str!`-embedded source — see `prelude::load_embedded`), if any,
/// and a filesystem base for the entry program's own disk-backed `mod`
/// declarations, if any. A plain struct, not a decorator-chain enum: with
/// named path dependencies gone, every compile is exactly one of the four
/// `(sys?, entry_base?)` combinations, never an arbitrary nesting.
pub struct RootSource {
    sys: Option<Module>,
    entry_base: Option<PathBuf>,
}

impl RootSource {
    /// No further modules resolve — every `mod` declaration in the program
    /// must carry an inline body. Used by tests exercising resolution logic
    /// in isolation, and by embedders (`curios-js`) with no filesystem.
    pub fn none() -> RootSource {
        RootSource {
            sys: None,
            entry_base: None,
        }
    }

    /// Disk-backed modules rooted at a directory: qualifier `a/b/c` reads
    /// `base/a/b/c.crs`.
    pub fn file_system(base: impl Into<PathBuf>) -> RootSource {
        RootSource {
            sys: None,
            entry_base: Some(base.into()),
        }
    }

    /// Attach the embedded `sys`/`syn`/`std` roots, keeping this source's own
    /// entry-filesystem setting — `prelude::prelude`'s sole job.
    pub(crate) fn with_sys(self, sys: Module) -> RootSource {
        RootSource {
            sys: Some(sys),
            entry_base: self.entry_base,
        }
    }

    pub(crate) fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        if let Some(sys) = &self.sys
            && let Some(module) = super::prelude::load_embedded(sys, qualifier)
        {
            return Ok(module);
        }

        match &self.entry_base {
            Some(base) => {
                Module::from_path(Self::module_file_path(base, qualifier)).map_err(|cause| {
                    Error::ModuleLoadFailed {
                        label: qualifier.join(),
                        cause: Box::new(cause),
                    }
                })
            }
            None => Err(Error::ModuleNotFound {
                path: qualifier.join(),
            }),
        }
    }

    fn module_file_path(base: &std::path::Path, qualifier: &Qualifier) -> PathBuf {
        let mut segments = qualifier.iter().collect::<Vec<_>>();
        let label = segments.pop().unwrap();

        segments
            .into_iter()
            .fold(base.to_path_buf(), |path, segment| path.join(segment))
            .join(format!("{label}.crs"))
    }

    /// Whether `sys`/`syn`/`std` are attached to this source — gates
    /// `to_core`'s explicit per-root discovery/lowering passes (see
    /// `to_core::FIXED_ROOTS`), since a bare `none()`/`file_system(..)` loader
    /// (a test exercising resolution logic in isolation) has no prelude at
    /// all.
    pub(crate) fn has_embedded_roots(&self) -> bool {
        self.sys.is_some()
    }
}
