use {
    super::{Module, Path},
    crate::Source,
    std::{fs, path::PathBuf},
};

pub trait Loader {
    fn load(&self, prefix: &Path, label: &str) -> Result<Module, String>;
}

pub struct FileLoader {
    base: PathBuf,
}

impl FileLoader {
    pub fn new(base: impl Into<PathBuf>) -> Self {
        Self { base: base.into() }
    }
}

impl Loader for FileLoader {
    fn load(&self, prefix: &Path, label: &str) -> Result<Module, String> {
        let path = prefix
            .iter()
            .fold(self.base.clone(), |p, seg| p.join(seg))
            .join(format!("{label}.crs"));

        let text = fs::read_to_string(&path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;

        let source = Source::new(path.clone(), text);

        Module::parse(&source).map_err(|e| format!("{}:\n{}", path.display(), e.format()))
    }
}

pub struct PanicLoader;

impl Loader for PanicLoader {
    fn load(&self, _prefix: &Path, label: &str) -> Result<Module, String> {
        panic!("unexpected file-backed module: {label}")
    }
}
