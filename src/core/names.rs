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

impl Name {
    pub fn free<A>(free: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            kind: NameKind::Free(free.into()),
        }
    }

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
