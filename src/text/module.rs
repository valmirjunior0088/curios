use super::{Subterm, Term};

#[derive(Debug, Clone, PartialEq)]
pub struct TopLet {
    pub is_pub: bool,
    pub label: String,
    pub type_: Subterm,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopRecItem {
    pub is_pub: bool,
    pub label: String,
    pub type_: Subterm,
    pub value: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Let(TopLet),
    Rec(Vec<TopRecItem>),
    Mod(String, Module),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub uses: Vec<String>,
    pub items: Vec<TopItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub uses: Vec<String>,
    pub items: Vec<TopItem>,
    pub tail: Term,
}
