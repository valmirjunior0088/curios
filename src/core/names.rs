use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    Free(String),
    Bound(usize),
}

impl<A> From<A> for Name
where
    A: Into<String>,
{
    fn from(value: A) -> Self {
        Self::Free(value.into())
    }
}
