use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NameKind {
    Free(String),
    Bound(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    kind: NameKind,
}

impl<A> From<A> for Name
where
    A: Into<String>,
{
    fn from(free: A) -> Self {
        Self {
            kind: NameKind::Free(free.into()),
        }
    }
}

impl Name {
    pub(super) fn as_free(&self) -> Option<&str> {
        match &self.kind {
            NameKind::Free(free) => Some(free),
            NameKind::Bound(_) => None,
        }
    }

    pub(super) fn bound(bound: usize) -> Self {
        Self {
            kind: NameKind::Bound(bound),
        }
    }

    pub(super) fn as_bound(&self) -> Option<usize> {
        match &self.kind {
            NameKind::Free(_) => None,
            &NameKind::Bound(bound) => Some(bound),
        }
    }
}
