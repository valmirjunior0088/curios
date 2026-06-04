use {
    super::{LetSignature, Name, Term},
    crate::Span,
    std::iter,
};

#[derive(Debug, Clone)]
pub struct TopMod {
    pub span: Option<Span>,
    pub is_pub: bool,
    pub label: String,
    pub module: Option<Module>,
}

impl PartialEq for TopMod {
    fn eq(&self, other: &Self) -> bool {
        self.is_pub == other.is_pub && self.label == other.label && self.module == other.module
    }
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
    pub signature: LetSignature,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopCase {
    pub label: String,
    pub payload_types: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopUnion {
    pub is_pub: bool,
    pub label: String,
    pub params: Vec<(String, Term)>,
    pub cases: Vec<TopCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Mod(TopMod),
    Use(TopUse),
    Let(TopLet),
    Rec(Vec<TopLet>),
    Union(Vec<TopUnion>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<TopItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub type_: Option<Term>,
    pub items: Vec<TopItem>,
    pub tail: Term,
}

impl Entrypoint {
    pub fn new(items: Vec<TopItem>, tail: Term) -> Self {
        Self {
            type_: None,
            items,
            tail,
        }
    }

    pub fn with_type(self, type_: Term) -> Self {
        Self {
            type_: Some(type_),
            ..self
        }
    }

    pub fn with_prelude(self) -> Self {
        Self {
            items: iter::once(super::prelude()).chain(self.items).collect(),
            ..self
        }
    }
}
