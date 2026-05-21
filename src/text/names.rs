use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub path: Vec<String>,
}

impl<I: IntoIterator<Item = String>> From<I> for Name {
    fn from(iter: I) -> Self {
        Self {
            path: iter.into_iter().collect(),
        }
    }
}

impl Name {
    pub fn new() -> Name {
        Name { path: vec![] }
    }

    pub fn with(&self, segment: &str) -> Name {
        Self {
            path: self
                .path
                .iter()
                .cloned()
                .chain([segment.to_string()])
                .collect(),
        }
    }
}
