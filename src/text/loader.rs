use {
    super::{Error, Module, Qualifier},
    std::path::PathBuf,
};

pub trait Loader {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error>;
}

pub struct FileLoader {
    base: PathBuf,
}

impl FileLoader {
    pub fn new(base: impl Into<PathBuf>) -> Self {
        Self { base: base.into() }
    }

    fn module_file_path(&self, qualifier: &Qualifier) -> PathBuf {
        let mut segments = qualifier.iter().collect::<Vec<_>>();
        let label = segments.pop().unwrap();

        segments
            .into_iter()
            .fold(self.base.clone(), |path, segment| path.join(segment))
            .join(format!("{label}.crs"))
    }
}

impl Loader for FileLoader {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        Module::from_path(self.module_file_path(qualifier)).map_err(|cause| {
            Error::ModuleLoadFailed {
                label: qualifier.join(),
                cause: Box::new(cause),
            }
        })
    }
}

pub struct NullLoader;

impl Loader for NullLoader {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        Err(Error::ModuleNotFound {
            path: qualifier.join(),
        })
    }
}
