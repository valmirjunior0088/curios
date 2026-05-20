use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq)]
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
