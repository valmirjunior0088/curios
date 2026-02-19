use {
    crate::macros::name,
    std::hash::{Hash, Hasher},
};

name!(Atom);

#[derive(Debug, Clone)]
pub struct Name {
    pub index: usize,
    pub label: String,
}

impl Name {
    pub fn new<A>(index: usize, label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            index,
            label: label.into(),
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for Name {}

impl Hash for Name {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.index.hash(state);
    }
}
