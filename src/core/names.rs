use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NameType {
    Label(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    type_: NameType,
}

impl Name {
    pub fn label<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            type_: NameType::Label(label.into()),
        }
    }

    pub(super) fn as_label(&self) -> Option<&str> {
        match &self.type_ {
            NameType::Label(label) => Some(label),
            NameType::Index(_) => None,
        }
    }

    pub(super) fn index(index: usize) -> Self {
        Self {
            type_: NameType::Index(index),
        }
    }

    pub(super) fn as_index(&self) -> Option<usize> {
        match &self.type_ {
            NameType::Label(_) => None,
            &NameType::Index(index) => Some(index),
        }
    }

    pub fn unwrap(&self) -> &str {
        self.as_label().unwrap()
    }
}

impl From<&Name> for Name {
    fn from(name: &Name) -> Self {
        name.clone()
    }
}
