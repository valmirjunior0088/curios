use super::{Name, Subterm, Term};

#[derive(Debug, Clone, PartialEq)]
pub struct TopMod {
    pub is_pub: bool,
    pub label: String,
    pub module: Option<Module>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopUse {
    pub is_pub: bool,
    pub is_abs: bool,
    pub name: Name,
    pub group: Option<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopLet {
    pub is_pub: bool,
    pub label: String,
    pub type_: Subterm,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopDef {
    pub is_pub: bool,
    pub label: String,
    pub witness: Subterm,
    pub module: Module,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Mod(TopMod),
    Use(TopUse),
    Let(TopLet),
    Rec(Vec<TopLet>),
    Def(TopDef),
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
