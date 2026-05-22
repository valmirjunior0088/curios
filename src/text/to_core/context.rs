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
    aliases: &'a mut HashMap<Name, Name>,
    scope: HashMap<String, Name>,
}

impl<'a> Context<'a> {
    pub fn new(
        table: &'a mut HashMap<Name, ModuleInfo>,
        aliases: &'a mut HashMap<Name, Name>,
    ) -> Context<'a> {
        Context {
            prefix: Name::empty(),
            table,
            aliases,
            scope: HashMap::new(),
        }
    }

    pub fn nested(&mut self, label: &str) -> Context<'_> {
        Context {
            prefix: self.prefix.with(label),
            table: &mut *self.table,
            aliases: &mut *self.aliases,
            scope: HashMap::new(),
        }
    }

    pub fn prefixed(&self, label: &str) -> Name {
        self.prefix.with(label)
    }

    pub fn scope(&self) -> &HashMap<String, Name> {
        &self.scope
    }

    pub fn table(&self) -> &HashMap<Name, ModuleInfo> {
        &*self.table
    }

    pub fn aliases(&self) -> &HashMap<Name, Name> {
        &*self.aliases
    }

    pub fn register_alias(&mut self, qualifier: &str) {
        self.aliases
            .insert(self.prefix.with(qualifier), self.scope[qualifier].clone());
    }

    pub fn finalize(&mut self, info: ModuleInfo) {
        self.table.insert(self.prefix.clone(), info);
    }

    pub fn insert_scope(&mut self, qualifier: String, name: Name) {
        if self.scope.contains_key(&qualifier) {
            panic!("qualifier conflicts with existing scope entry: {qualifier}");
        }

        self.scope.insert(qualifier, name);
    }

    pub fn resolve_use(&mut self, top_use: &TopUse) {
        if !top_use.is_abs && top_use.name.is_single() {
            panic!(
                "single-segment relative use is forbidden: {}",
                top_use.name.head()
            );
        }

        let qualifier = top_use.name.last().to_string();

        let resolved_path = if top_use.is_abs {
            let mut current = Name::from([top_use.name.head()]);

            if !self.table.contains_key(&current) {
                panic!("module not found: {}", top_use.name.head());
            }

            for seg in top_use.name.tail() {
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

                if let Some(canonical) = self.aliases.get(&current) {
                    current = canonical.clone();
                }

                if !self.table.contains_key(&current) {
                    panic!("module not found: {}", current.join());
                }
            }

            current
        } else {
            let first = top_use.name.head();

            let mut current = self
                .scope
                .get(first)
                .unwrap_or_else(|| panic!("undeclared child in relative use: {first}"))
                .clone();

            for seg in top_use.name.tail() {
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

                if let Some(canonical) = self.aliases.get(&current) {
                    current = canonical.clone();
                }
            }

            if !self.table.contains_key(&current) {
                panic!("module not found: {}", current.join());
            }

            current
        };

        self.insert_scope(qualifier, resolved_path);
    }
}
