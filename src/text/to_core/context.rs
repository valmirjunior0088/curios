use {
    crate::{
        core,
        text::{Name, TopUse},
    },
    std::collections::HashMap,
};

pub struct DefStack {
    entries: Vec<(String, Name)>,
}

impl DefStack {
    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn push(&self, label: String, name: Name) -> Self {
        Self {
            entries: self
                .entries
                .iter()
                .cloned()
                .chain([(label, name)])
                .collect(),
        }
    }

    pub fn get(&self, label: &str) -> Option<&Name> {
        self.entries
            .iter()
            .rev()
            .find(|(entry_label, _)| entry_label == label)
            .map(|(_, name)| name)
    }
}

pub struct FlatLet {
    pub name: Name,
    pub type_: core::Term,
    pub body: core::Term,
}

pub struct FlatDef {
    pub name: Name,
    pub witness: core::Term,
}

pub enum FlatItem {
    Def(FlatDef),
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
}

pub struct Context<'a> {
    prefix: Name,
    table: &'a mut HashMap<Name, ModuleInfo>,
    module_aliases: &'a mut HashMap<Name, Name>,
    binding_aliases: &'a mut HashMap<Name, Name>,
    qualifiers: HashMap<String, Name>,
    bindings: HashMap<String, Name>,
}

impl<'a> Context<'a> {
    pub fn new(
        table: &'a mut HashMap<Name, ModuleInfo>,
        module_aliases: &'a mut HashMap<Name, Name>,
        binding_aliases: &'a mut HashMap<Name, Name>,
    ) -> Context<'a> {
        Context {
            prefix: Name::empty(),
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

    pub fn prefix(&self) -> &Name {
        &self.prefix
    }

    pub fn prefixed(&self, label: &str) -> Name {
        self.prefix.with(label)
    }

    pub fn qualifiers(&self) -> &HashMap<String, Name> {
        &self.qualifiers
    }

    pub fn bindings(&self) -> &HashMap<String, Name> {
        &self.bindings
    }

    pub fn table(&self) -> &HashMap<Name, ModuleInfo> {
        &*self.table
    }

    pub fn module_aliases(&self) -> &HashMap<Name, Name> {
        &*self.module_aliases
    }

    pub fn binding_aliases(&self) -> &HashMap<Name, Name> {
        &*self.binding_aliases
    }

    pub fn register_alias(&mut self, qualifier: &str) {
        self.module_aliases.insert(
            self.prefix.with(qualifier),
            self.qualifiers[qualifier].clone(),
        );
    }

    pub fn register_binding_alias(&mut self, label: &str) {
        self.binding_aliases.insert(
            self.prefix.with(label),
            self.bindings[label].clone(),
        );
    }

    pub fn finalize(&mut self, info: ModuleInfo) {
        self.table.insert(self.prefix.clone(), info);
    }

    pub fn insert_scope(&mut self, qualifier: String, name: Name) {
        if self.qualifiers.contains_key(&qualifier) {
            panic!("qualifier conflicts with existing scope entry: {qualifier}");
        }

        self.qualifiers.insert(qualifier, name);
    }

    pub fn insert_binding(&mut self, label: String, name: Name) {
        if self.bindings.contains_key(&label) {
            panic!("binding conflicts with existing scope entry: {label}");
        }

        self.bindings.insert(label, name);
    }

    pub fn resolve_use(&mut self, top_use: &TopUse) -> UseResolved {
        if !top_use.is_abs && top_use.name.is_single() {
            panic!(
                "single-segment relative use is forbidden: {}",
                top_use.name.head()
            );
        }

        let label = top_use.name.last().to_string();

        let parent_path = if top_use.is_abs {
            let mut current = Name::empty();

            if top_use.name.is_single() {
                current
            } else {
                let head = top_use.name.head();

                let root_info = self
                    .table
                    .get(&Name::empty())
                    .expect("root module info not present");

                let is_pub = root_info
                    .get_child(head)
                    .unwrap_or_else(|| panic!("child module not found: {head}"));

                if !is_pub {
                    panic!("private child module: {head}");
                }

                current = Name::from([head]);

                if !self.table.contains_key(&current) {
                    panic!("module not found: {head}");
                }

                for seg in top_use.name.interior() {
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
            let first = top_use.name.head();

            let mut current = self
                .qualifiers
                .get(first)
                .unwrap_or_else(|| panic!("undeclared child in relative use: {first}"))
                .clone();

            for seg in top_use.name.interior() {
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

        let parent_info = self
            .table
            .get(&parent_path)
            .unwrap_or_else(|| panic!("module not found: {}", parent_path.join()));

        let child = parent_info.get_child(&label);
        let binding = parent_info.get_binding(&label);

        let mut result = UseResolved {
            module: None,
            binding: None,
        };

        if let Some(true) = child {
            let mut resolved = parent_path.with(&label);

            if let Some(canonical) = self.module_aliases.get(&resolved) {
                resolved = canonical.clone();
            }

            if !self.table.contains_key(&resolved) {
                panic!("module not found: {}", resolved.join());
            }

            self.insert_scope(label.clone(), resolved.clone());
            result.module = Some(resolved);
        }

        if let Some(true) = binding {
            let mut resolved = parent_path.with(&label);

            if let Some(canonical) = self.binding_aliases.get(&resolved) {
                resolved = canonical.clone();
            }

            self.insert_binding(label.clone(), resolved.clone());
            result.binding = Some(resolved);
        }

        if result.module.is_none() && result.binding.is_none() {
            match (child, binding) {
                (None, None) => panic!(
                    "unknown item or submodule: {label} in {}",
                    parent_path.join()
                ),
                (Some(false), None) => panic!("private child module: {label}"),
                (None, Some(false)) => panic!("private binding: {label}"),
                (Some(false), Some(false)) => {
                    panic!("private child module and binding: {label}")
                }
                _ => unreachable!(),
            }
        }

        result
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseResolved {
    pub module: Option<Name>,
    pub binding: Option<Name>,
}
