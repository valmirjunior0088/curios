use {
    super::{Error, Module},
    curios_base::Qualifier,
    std::path::PathBuf,
};

/// Optional filesystem source for the entry program's own file-backed modules. Fixed `/sys`, `/syn`, and `/std` content is build-scoped state owned by `curios-prelude`, not a runtime source mounted through this loader.
pub struct RootSource {
    entry_base: Option<PathBuf>,
}

impl RootSource {
    /// No further modules resolve — every `mod` declaration in the program must carry an inline body. Used by tests exercising resolution logic in isolation, and by embedders (`curios-web`) with no filesystem.
    pub fn none() -> RootSource {
        RootSource { entry_base: None }
    }

    /// Disk-backed modules rooted at a directory: qualifier `a/b/c` reads `base/a/b/c.crs`.
    pub fn file_system(base: impl Into<PathBuf>) -> RootSource {
        RootSource {
            entry_base: Some(base.into()),
        }
    }

    pub(crate) fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        match &self.entry_base {
            Some(base) => Module::from_path(module_file_path(base, qualifier)).map_err(|cause| {
                Error::ModuleLoadFailed {
                    label: qualifier.join(),
                    cause: Box::new(cause),
                }
            }),
            None => Err(Error::ModuleNotFound {
                path: qualifier.join(),
            }),
        }
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
