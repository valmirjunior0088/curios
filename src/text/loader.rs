use {
    super::{Module, Name},
    std::{fs, path::PathBuf},
};

pub trait Loader {
    fn load(&self, prefix: &Name, label: &str) -> Result<Module, String>;
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
    fn load(&self, prefix: &Name, label: &str) -> Result<Module, String> {
        let path = prefix
            .iter()
            .fold(self.base.clone(), |p, seg| p.join(seg))
            .join(format!("{label}.crs"));

        let source = fs::read_to_string(&path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;

        source
            .parse::<Module>()
            .map_err(|e| format!("failed to parse {}: {e:?}", path.display()))
    }
}

pub struct PanicLoader;

impl Loader for PanicLoader {
    fn load(&self, _prefix: &Name, label: &str) -> Result<Module, String> {
        panic!("unexpected file-backed module: {label}")
    }
}
