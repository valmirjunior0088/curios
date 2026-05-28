use {
    super::{Name, Subterm, Term},
    std::iter,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TopMod {
    pub is_pub: bool,
    pub label: String,
    pub module: Option<Module>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GroupItem {
    Mod(String),
    Let(String),
    Both(String),
}

impl GroupItem {
    pub fn label(&self) -> &str {
        match self {
            GroupItem::Mod(s) | GroupItem::Let(s) | GroupItem::Both(s) => s,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseGroup {
    Named(Vec<GroupItem>),
    Glob,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopUse {
    pub is_pub: bool,
    pub name: Name,
    pub group: UseGroup,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopLet {
    pub is_pub: bool,
    pub label: String,
    pub type_: Subterm,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Mod(TopMod),
    Use(TopUse),
    Let(TopLet),
    Rec(Vec<TopLet>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<TopItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub items: Vec<TopItem>,
    pub tail: Term,
}

impl Entrypoint {
    pub fn new(items: Vec<TopItem>, tail: Term) -> Self {
        Self { items, tail }
    }

    pub fn with_prelude(self) -> Self {
        Self {
            items: iter::once(super::prelude()).chain(self.items).collect(),
            tail: self.tail,
        }
    }
}
