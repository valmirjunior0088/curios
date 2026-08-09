//! Where a compilation's names come from: the prefixes units mount, and what each prefix may reach.
//!
//! A compilation is a set of units, and each unit claims one or more *prefixes*. The fixed prelude claims three — `/sys`, `/syn` and `/std`, which cannot be three units because `/syn` and `/std` reference each other — a package claims one, and the entry program claims the empty prefix, which is what makes it the entry. [`Mount`] pairs a prefix with the privilege tier that prefix carries.
//!
//! # Why a prefix and not an identity beside it
//!
//! A declaration used to carry a `RootId` stamp naming its root, cached beside the name whose leading segment already determined it. That stamp was archived, which made it mean something only in the compilation that wrote it — the shape rustc pays a `cnum_map` to translate. Here the name *is* the identity: which mount owns a declaration is [`Mount::owning`] over the name, and the only thing carried is the mount list itself, one per module rather than one per declaration.
//!
//! There is deliberately no answer derivable from a name alone. A leading segment identifies a mount only against the table of what is mounted, because a package's prefix and a module the entry declares are the same shape.

#[cfg(test)]
mod tests;

use crate::Qualifier;

/// One prefix a unit claims, and the privilege tier it carries.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Mount {
    /// The prefix every name this mount owns lies within. The empty qualifier is the entry program's, and at most one mount may claim it.
    pub prefix: Qualifier,
    pub kind: RootKind,
}

impl Mount {
    pub fn new(prefix: Qualifier, kind: RootKind) -> Self {
        Self { prefix, kind }
    }

    /// The mount owning `name`: the most specific prefix `name` lies within, or `None` when nothing mounted claims it.
    ///
    /// *Most specific* is load-bearing rather than a tie-break. The entry program mounts the empty prefix and every qualifier lies within that, so a `/std/Option` answered by the first match would come back ordinary. Mounts are pairwise disjoint, so no two of equal depth can both contain one name and there is no tie left to break.
    ///
    /// `None` is a real answer, not a missing one: while the fixed prelude is prepared, only `/sys`, `/syn` and `/std` are mounted and the empty qualifier is the synthetic compilation root, owned by no unit.
    pub fn owning<'a>(mounts: &'a [Mount], name: &Qualifier) -> Option<&'a Mount> {
        mounts
            .iter()
            .filter(|mount| name.is_within(&mount.prefix))
            .max_by_key(|mount| mount.prefix.segments().len())
    }

    /// Whether `name` is owned by a mount that may reference an internal root. An unowned name is not.
    pub fn privileged(mounts: &[Mount], name: &Qualifier) -> bool {
        Self::owning(mounts, name).is_some_and(|mount| mount.kind.is_privileged())
    }
}

/// A root's privilege tier — replaces the old `INTERNAL_ROOTS`/`PRIVILEGED_ROOTS` string-literal allowlists with a field carried on the mount itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum RootKind {
    /// Reachable only from a privileged root — `sys` today. Discoverable (so the standard library can resolve it by absolute path) but rejected when referenced from an ordinary consumer.
    Internal,
    /// May reference an internal root — `sys`, `syn`, `std` today.
    Privileged,
    /// No special reach — the entry program, and every package.
    Ordinary,
}

impl RootKind {
    /// Whether a root of this kind may reference an [`RootKind::Internal`] root. An internal root is trivially privileged over itself (`sys` referencing `sys` is not a violation).
    pub fn is_privileged(self) -> bool {
        matches!(self, RootKind::Internal | RootKind::Privileged)
    }
}
