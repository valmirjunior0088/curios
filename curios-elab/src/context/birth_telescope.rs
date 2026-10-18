//! A metavariable's birth telescope: the local binders in scope where it was born, each at its type there, in binding order — what its solution is spelled over and judged under.
//!
//! **Persistent, and shared with the scope.** The binders stand at dense ranks in a persistent vector, which the frames extend a binder at a time beside the identity spine over them ([`Frame`](super::Frame)): every metavariable born under one scope shares the scope's telescope, and a birth costs nothing more than the one before it.

use {
    curios_core::{Free, Term},
    curios_utilities::Ordered,
    std::fmt,
};

/// A birth telescope. See the module documentation.
#[derive(Clone, Default)]
pub(crate) struct BirthTelescope(Ordered<(Free, Term)>);

impl BirthTelescope {
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The binders in binding order, each with its type.
    pub(crate) fn iter(&self) -> impl Iterator<Item = &(Free, Term)> + Clone {
        self.0.iter().map(|(_, binder)| binder)
    }

    /// The first `count` binders: what a metavariable born before the rest were bound is re-expressed over.
    pub(crate) fn prefix(&self, count: usize) -> Self {
        self.iter().take(count).cloned().collect::<Vec<_>>().into()
    }

    /// `name` at `type_`, bound after every other.
    pub(super) fn bound(&mut self, name: Free, type_: Term) {
        let rank = self.0.len() as i64;
        self.0.insert(rank, (name, type_));
    }
}

impl From<Vec<(Free, Term)>> for BirthTelescope {
    fn from(binders: Vec<(Free, Term)>) -> Self {
        let mut telescope = Self::default();
        for (name, type_) in binders {
            telescope.bound(name, type_);
        }
        telescope
    }
}

impl fmt::Debug for BirthTelescope {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_list().entries(self.iter()).finish()
    }
}
