use {crate::{core, text::{Name, TopUse}}, std::collections::HashMap};

pub struct FlatLet {
    pub name: Name,
    pub type_: core::Term,
    pub body: core::Term,
}

pub enum FlatItem {
    Let(FlatLet),
    Rec(Vec<FlatLet>),
}

pub struct ModuleInfo {
    pub children: HashMap<String, bool>,
    pub bindings: HashMap<String, bool>,
}

pub struct Context<'a> {
    pub prefix: Name,
    pub table: &'a mut HashMap<Name, ModuleInfo>,
    pub scope: HashMap<String, Name>,
}

impl<'a> Context<'a> {
    pub fn new(table: &'a mut HashMap<Name, ModuleInfo>) -> Context<'a> {
        Context {
            prefix: Name::new(),
            table,
            scope: HashMap::new(),
        }
    }

    pub fn nested(&mut self, label: &str) -> Context<'_> {
        Context {
            prefix: self.prefix.with(label),
            table: &mut *self.table,
            scope: HashMap::new(),
        }
    }

    pub fn resolve_use(&mut self, top_use: &TopUse) {
        if !top_use.is_abs && top_use.name.path.len() == 1 {
            let seg = &top_use.name.path[0];
            panic!("single-segment relative use is forbidden: {seg}");
        }

        let qualifier = top_use.name.path.last().unwrap().clone();

        let resolved_path = if top_use.is_abs {
            let segments = &top_use.name.path;
            let mut current = Name {
                path: vec![segments[0].clone()],
            };
            if !self.table.contains_key(&current) {
                panic!("module not found: {}", segments[0]);
            }
            for seg in &segments[1..] {
                let info = self
                    .table
                    .get(&current)
                    .unwrap_or_else(|| panic!("module not found: {}", current.path.join("/")));
                let is_pub = info
                    .children
                    .get(seg)
                    .unwrap_or_else(|| panic!("child module not found: {seg}"));
                if !is_pub {
                    panic!("private child module: {seg}");
                }
                current = current.with(seg);
                if !self.table.contains_key(&current) {
                    panic!("module not found: {}", current.path.join("/"));
                }
            }
            current
        } else {
            let first = &top_use.name.path[0];
            let mut current = self
                .scope
                .get(first)
                .unwrap_or_else(|| panic!("undeclared child in relative use: {first}"))
                .clone();
            for seg in &top_use.name.path[1..] {
                let info = self
                    .table
                    .get(&current)
                    .unwrap_or_else(|| panic!("module not found: {}", current.path.join("/")));
                let is_pub = info
                    .children
                    .get(seg)
                    .unwrap_or_else(|| panic!("child module not found: {seg}"));
                if !is_pub {
                    panic!("private child module: {seg}");
                }
                current = current.with(seg);
            }
            if !self.table.contains_key(&current) {
                panic!("module not found: {}", current.path.join("/"));
            }
            current
        };

        if self.scope.contains_key(&qualifier) {
            panic!("use qualifier conflicts with existing scope entry: {qualifier}");
        }
        self.scope.insert(qualifier, resolved_path);
    }
}
