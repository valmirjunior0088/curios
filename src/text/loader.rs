use {
    super::{Entrypoint, Error, Module, Path, TopItem},
    crate::Source,
    std::{
        collections::{HashMap, HashSet},
        fs,
        path::{Path as FsPath, PathBuf},
    },
};

pub trait Store {
    fn get(&self, path: &Path) -> Option<&Module>;
}

pub struct FileStore {
    base: PathBuf,
    modules: HashMap<Path, Module>,
}

impl FileStore {
    pub fn new(base: impl Into<PathBuf>, entrypoint: &Entrypoint) -> Result<Self, Error> {
        let mut store = Self {
            base: base.into(),
            modules: HashMap::new(),
        };
        let mut visiting = HashSet::new();

        store.prepare_items(&Path::empty(), &entrypoint.items, &mut visiting)?;

        Ok(store)
    }

    fn prepare_items(
        &mut self,
        prefix: &Path,
        items: &[TopItem],
        visiting: &mut HashSet<Path>,
    ) -> Result<(), Error> {
        for item in items {
            if let TopItem::Mod(module_item) = item {
                let path = prefix.with(&module_item.label);

                match &module_item.module {
                    Some(module) => self.prepare_items(&path, &module.items, visiting)?,
                    None => {
                        let module =
                            self.prepare_file_module(&path, visiting).map_err(|error| {
                                match &module_item.span {
                                    Some(span) => error.at(span.clone()),
                                    None => error,
                                }
                            })?;
                        self.prepare_items(&path, &module.items, visiting)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn prepare_file_module(
        &mut self,
        path: &Path,
        visiting: &mut HashSet<Path>,
    ) -> Result<Module, Error> {
        if let Some(module) = self.modules.get(path) {
            return Ok(module.clone());
        }

        if !visiting.insert(path.clone()) {
            return Err(Error::ModuleLoadFailed {
                label: path.join(),
                reason: "cycle in file-backed modules".to_string(),
            });
        }

        let file_path = module_file_path(&self.base, path);
        let text = fs::read_to_string(&file_path).map_err(|error| Error::ModuleLoadFailed {
            label: path.join(),
            reason: format!("failed to read {}: {error}", file_path.display()),
        })?;

        let source = Source::new(file_path.clone(), text);
        let module = Module::parse(&source).map_err(|error| Error::ModuleLoadFailed {
            label: path.join(),
            reason: format!("{}:\n{}", file_path.display(), error.format()),
        })?;

        self.modules.insert(path.clone(), module.clone());
        visiting.remove(path);

        Ok(module)
    }
}

impl Store for FileStore {
    fn get(&self, path: &Path) -> Option<&Module> {
        self.modules.get(path)
    }
}

pub struct EmptyStore;

impl Store for EmptyStore {
    fn get(&self, _path: &Path) -> Option<&Module> {
        None
    }
}

fn module_file_path(base: &FsPath, path: &Path) -> PathBuf {
    let mut segments = path.iter().collect::<Vec<_>>();
    let label = segments.pop().unwrap();
    segments
        .into_iter()
        .fold(base.to_path_buf(), |path, segment| path.join(segment))
        .join(format!("{label}.crs"))
}
