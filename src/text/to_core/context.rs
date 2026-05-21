use {crate::{core, text::Name}, std::collections::HashMap};

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
}
