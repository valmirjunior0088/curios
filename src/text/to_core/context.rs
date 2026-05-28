use {
    crate::{
        core,
        text::{Name, Path},
    },
    std::collections::HashMap,
};

pub struct FlatLet {
    pub name: Path,
    pub type_: core::Term,
    pub body: core::Term,
}

pub enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

pub struct ModuleInfo {
    children: HashMap<String, bool>,
    bindings: HashMap<String, bool>,
}

impl ModuleInfo {
    pub fn new() -> Self {
        Self {
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn insert_child(&mut self, label: String, is_pub: bool) {
        self.children.insert(label, is_pub);
    }

    pub fn insert_binding(&mut self, label: String, is_pub: bool) {
        self.bindings.insert(label, is_pub);
    }

    pub fn get_child(&self, label: &str) -> Option<bool> {
        self.children.get(label).copied()
    }

    pub fn get_binding(&self, label: &str) -> Option<bool> {
        self.bindings.get(label).copied()
    }

    pub fn public_labels(&self) -> Vec<String> {
        let mut labels = self
            .children
            .iter()
            .chain(self.bindings.iter())
            .filter(|(_, is_pub)| **is_pub)
            .map(|(label, _)| label.clone())
            .collect::<Vec<_>>();

        labels.sort();
        labels.dedup();
        labels
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseResolved {
    pub module: Option<Path>,
    pub binding: Option<Path>,
}

pub struct Context<'a> {
    prefix: Path,
    table: &'a mut HashMap<Path, ModuleInfo>,
    module_aliases: &'a mut HashMap<Path, Path>,
    binding_aliases: &'a mut HashMap<Path, Path>,
    qualifiers: HashMap<String, Path>,
    bindings: HashMap<String, Path>,
}

impl<'a> Context<'a> {
    pub fn new(
        table: &'a mut HashMap<Path, ModuleInfo>,
        module_aliases: &'a mut HashMap<Path, Path>,
        binding_aliases: &'a mut HashMap<Path, Path>,
    ) -> Context<'a> {
        Context {
            prefix: Path::empty(),
            table,
            module_aliases,
            binding_aliases,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn nested(&mut self, label: &str) -> Context<'_> {
        Context {
            prefix: self.prefix.with(label),
            table: &mut *self.table,
            module_aliases: &mut *self.module_aliases,
            binding_aliases: &mut *self.binding_aliases,
            qualifiers: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn prefix(&self) -> &Path {
        &self.prefix
    }

    pub fn prefixed(&self, label: &str) -> Path {
        self.prefix.with(label)
    }

    pub fn qualifiers(&self) -> &HashMap<String, Path> {
        &self.qualifiers
    }

    pub fn bindings(&self) -> &HashMap<String, Path> {
        &self.bindings
    }

    pub fn table(&self) -> &HashMap<Path, ModuleInfo> {
        &*self.table
    }

    pub fn module_aliases(&self) -> &HashMap<Path, Path> {
        &*self.module_aliases
    }

    pub fn binding_aliases(&self) -> &HashMap<Path, Path> {
        &*self.binding_aliases
    }

    pub fn register_alias(&mut self, qualifier: &str) {
        self.module_aliases.insert(
            self.prefix.with(qualifier),
            self.qualifiers[qualifier].clone(),
        );
    }

    pub fn register_binding_alias(&mut self, label: &str) {
        self.binding_aliases
            .insert(self.prefix.with(label), self.bindings[label].clone());
    }

    pub fn finalize(&mut self, info: ModuleInfo) {
        self.table.insert(self.prefix.clone(), info);
    }

    pub fn insert_scope(&mut self, qualifier: String, name: Path) {
        if self.qualifiers.contains_key(&qualifier) {
            panic!("qualifier conflicts with existing scope entry: {qualifier}");
        }

        self.qualifiers.insert(qualifier, name);
    }

    pub fn insert_binding(&mut self, label: String, name: Path) {
        if self.bindings.contains_key(&label) {
            panic!("binding conflicts with existing scope entry: {label}");
        }

        self.bindings.insert(label, name);
    }

    // Import the module child `label` out of the module at `parent_path`, registering
    // it as a qualifier in the current scope.
    fn import_module_label(&mut self, parent_path: &Path, label: &str) -> Path {
        let child = self
            .table
            .get(parent_path)
            .unwrap_or_else(|| panic!("module not found: {}", parent_path.join()))
            .get_child(label);

        match child {
            Some(true) => {
                let mut resolved = parent_path.with(label);

                if let Some(canonical) = self.module_aliases.get(&resolved) {
                    resolved = canonical.clone();
                }

                if !self.table.contains_key(&resolved) {
                    panic!("module not found: {}", resolved.join());
                }

                self.insert_scope(label.to_string(), resolved.clone());
                resolved
            }
            Some(false) => panic!("private child module: {label}"),
            None => panic!("not a module: {label} in {}", parent_path.join()),
        }
    }

    // Import the binding `label` out of the module at `parent_path`, registering it
    // as a binding in the current scope.
    fn import_binding_label(&mut self, parent_path: &Path, label: &str) -> Path {
        let binding = self
            .table
            .get(parent_path)
            .unwrap_or_else(|| panic!("module not found: {}", parent_path.join()))
            .get_binding(label);

        match binding {
            Some(true) => {
                let mut resolved = parent_path.with(label);

                if let Some(canonical) = self.binding_aliases.get(&resolved) {
                    resolved = canonical.clone();
                }

                self.insert_binding(label.to_string(), resolved.clone());
                resolved
            }
            Some(false) => panic!("private binding: {label}"),
            None => panic!("not a binding: {label} in {}", parent_path.join()),
        }
    }

    // Import both module and binding slots — used by glob and by the `Both`
    // group item. Either or both may be absent: callers that require at least
    // one (e.g. an explicit `Both` import) should check `result` afterwards.
    fn import_dual_label(&mut self, parent_path: &Path, label: &str) -> UseResolved {
        let (child, binding) = {
            let parent_info = self
                .table
                .get(parent_path)
                .unwrap_or_else(|| panic!("module not found: {}", parent_path.join()));

            (parent_info.get_child(label), parent_info.get_binding(label))
        };

        let mut result = UseResolved {
            module: None,
            binding: None,
        };

        if let Some(true) = child {
            let mut resolved = parent_path.with(label);

            if let Some(canonical) = self.module_aliases.get(&resolved) {
                resolved = canonical.clone();
            }

            if !self.table.contains_key(&resolved) {
                panic!("module not found: {}", resolved.join());
            }

            self.insert_scope(label.to_string(), resolved.clone());
            result.module = Some(resolved);
        }

        if let Some(true) = binding {
            let mut resolved = parent_path.with(label);

            if let Some(canonical) = self.binding_aliases.get(&resolved) {
                resolved = canonical.clone();
            }

            self.insert_binding(label.to_string(), resolved.clone());
            result.binding = Some(resolved);
        }

        result
    }

    fn resolve_parent_path(&self, name: &Name) -> (Path, String) {
        let is_abs = name.is_abs();
        let label = name.last().to_string();

        let parent_path = if is_abs {
            let mut current = Path::empty();

            if name.is_single() {
                current
            } else {
                let head = name.head();

                let root_info = self
                    .table
                    .get(&Path::empty())
                    .expect("root module info not present");

                let is_pub = root_info
                    .get_child(head)
                    .unwrap_or_else(|| panic!("child module not found: {head}"));

                if !is_pub {
                    panic!("private child module: {head}");
                }

                current = Path::from([head]);

                if !self.table.contains_key(&current) {
                    panic!("module not found: {head}");
                }

                for seg in name.interior() {
                    let info = self
                        .table
                        .get(&current)
                        .unwrap_or_else(|| panic!("module not found: {}", current.join()));

                    let is_pub = info
                        .get_child(seg)
                        .unwrap_or_else(|| panic!("child module not found: {seg}"));

                    if !is_pub {
                        panic!("private child module: {seg}");
                    }

                    current = current.with(seg);

                    if let Some(canonical) = self.module_aliases.get(&current) {
                        current = canonical.clone();
                    }

                    if !self.table.contains_key(&current) {
                        panic!("module not found: {}", current.join());
                    }
                }

                current
            }
        } else {
            let first = name.head();

            let mut current = self
                .qualifiers
                .get(first)
                .unwrap_or_else(|| panic!("undeclared child in relative use: {first}"))
                .clone();

            for seg in name.interior() {
                let info = self
                    .table
                    .get(&current)
                    .unwrap_or_else(|| panic!("module not found: {}", current.join()));

                let is_pub = info
                    .get_child(seg)
                    .unwrap_or_else(|| panic!("child module not found: {seg}"));

                if !is_pub {
                    panic!("private child module: {seg}");
                }

                current = current.with(seg);

                if let Some(canonical) = self.module_aliases.get(&current) {
                    current = canonical.clone();
                }
            }

            if !self.table.contains_key(&current) {
                panic!("module not found: {}", current.join());
            }

            current
        };

        (parent_path, label)
    }

    pub fn resolve_module_use(&mut self, name: &Name) -> Path {
        let (parent_path, label) = self.resolve_parent_path(name);
        self.import_module_label(&parent_path, &label)
    }

    pub fn resolve_binding_use(&mut self, name: &Name) -> Path {
        let (parent_path, label) = self.resolve_parent_path(name);
        self.import_binding_label(&parent_path, &label)
    }

    pub fn resolve_both_use(&mut self, name: &Name) -> UseResolved {
        let (parent_path, label) = self.resolve_parent_path(name);

        let (child, binding) = {
            let parent_info = self
                .table
                .get(&parent_path)
                .unwrap_or_else(|| panic!("module not found: {}", parent_path.join()));

            (parent_info.get_child(&label), parent_info.get_binding(&label))
        };

        if !matches!(child, Some(true)) && !matches!(binding, Some(true)) {
            match (child, binding) {
                (Some(false), _) => panic!("private child module: {label}"),
                (_, Some(false)) => panic!("private binding: {label}"),
                _ => panic!(
                    "no module or binding named {label} in {}",
                    parent_path.join()
                ),
            }
        }

        self.import_dual_label(&parent_path, &label)
    }

    // A glob `use a/b/*` names a module directly and imports every public child
    // and binding it exposes, each under its own label. Walks the full path to
    // the named module, then defers each label to `import_dual_label`.
    pub fn resolve_glob(&mut self, name: &Name) -> Vec<(String, UseResolved)> {
        let mut current = if name.is_abs() {
            Path::empty()
        } else {
            let first = name.head();

            self.qualifiers
                .get(first)
                .unwrap_or_else(|| panic!("undeclared child in relative use: {first}"))
                .clone()
        };

        let walk = if name.is_abs() {
            name.path().segments()
        } else {
            &name.path().segments()[1..]
        };

        for seg in walk {
            let info = self
                .table
                .get(&current)
                .unwrap_or_else(|| panic!("module not found: {}", current.join()));

            let is_pub = info
                .get_child(seg)
                .unwrap_or_else(|| panic!("child module not found: {seg}"));

            if !is_pub {
                panic!("private child module: {seg}");
            }

            current = current.with(seg);

            if let Some(canonical) = self.module_aliases.get(&current) {
                current = canonical.clone();
            }

            if !self.table.contains_key(&current) {
                panic!("module not found: {}", current.join());
            }
        }

        let labels = self
            .table
            .get(&current)
            .unwrap_or_else(|| panic!("module not found: {}", current.join()))
            .public_labels();

        labels
            .into_iter()
            .map(|label| {
                let resolved = self.import_dual_label(&current, &label);
                (label, resolved)
            })
            .collect()
    }

    pub fn export_child(&mut self, label: String) {
        self.table
            .get_mut(&self.prefix)
            .expect("module info not present for current prefix")
            .insert_child(label, true);
    }

    pub fn export_binding(&mut self, label: String) {
        self.table
            .get_mut(&self.prefix)
            .expect("module info not present for current prefix")
            .insert_binding(label, true);
    }
}
