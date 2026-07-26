//! Name types for the `core` stage.
//!
//! A name here distinguishes one binding from another and renders for a human.
//! It is not a place to store facts: nothing branches on a name's characters,
//! its prefix, or its collation order. Where a consumer needs structure — which
//! module a definition belongs to, which inductive a constructor came from —
//! that structure is carried as a value by the site that knew it, never
//! recovered by taking a name apart.

#[cfg(test)]
mod tests;

use {
    curios_base::{Qualifier, id, name},
    std::fmt,
};

name!(Atom; archive);

id!(WitnessId, "witness"; archive);
id!(Mint, "local"; archive);

/// A top-level definition's identity: an authored path, or a compiler-generated
/// definition that has no source name at all.
///
/// The two cases are a sum rather than a qualifier with an optional
/// disambiguator, because a witness's declaring module is not its name. Folding
/// both into one field would make [`Qualifier`] mean "module plus the item's own
/// name" for one case and "the declaring module alone" for the other — one field
/// with two readings, which is the defect this vocabulary exists to remove.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(derive(PartialEq, Eq, PartialOrd, Ord, Hash))
)]
pub enum Global {
    /// A name a programmer wrote, at its resolved module path.
    Authored(Qualifier),
    /// A `satisfy` declaration. Witnesses are anonymous by design, so this is an
    /// identity rather than a manufactured name; the declaring module a
    /// diagnostic reports comes from `Definition::island`.
    Witness(WitnessId),
}

/// A free variable's identity: a top-level definition, or a binder some scope
/// opened.
///
/// The distinction is a discriminant rather than a spelling convention. Asking
/// "is this a local?" is a `matches!` — exact, and impossible to get wrong the
/// way a marker character in a string could be.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(derive(PartialEq, Eq, PartialOrd, Ord, Hash))
)]
pub enum Free {
    Global(Global),
    Local(Mint),
    /// **Migration scaffold. Deleted before this work lands.**
    ///
    /// A binder label the elaborator still mints as text (`Context::fresh`'s
    /// `x#7`). Held verbatim so the spelling round-trips exactly, which is what
    /// lets typed and untyped sites coexist while the migration runs: every
    /// construction of a given name-kind goes through one path, so a migrated
    /// site and an unmigrated one never build different values for the same
    /// name. Retired when `Context::fresh` becomes `Context::mint` and the
    /// display hint moves onto the binder.
    Opaque(String),
}

impl Free {
    /// **Migration scaffold. Deleted before this work lands.**
    ///
    /// Wraps a legacy spelling without interpreting it. Decoding is deliberately
    /// deferred to the commit that retypes a given construction site: a `Free`
    /// that owns its spelling can hand out a borrowed `&str`, and the kernel's
    /// hot paths — variable reduction, capture, free-variable collection — read
    /// names often enough that materializing one per read blows the reduction
    /// deadline outright.
    pub(crate) fn from_legacy(label: String) -> Self {
        Free::Opaque(label)
    }

    /// **Migration scaffold. Deleted before this work lands.**
    ///
    /// The legacy spelling of a name that has not been retyped yet. `None` once
    /// a site builds a real identity, which is what forces its consumers to be
    /// migrated in the same commit rather than silently reading a rendering.
    pub(crate) fn as_legacy(&self) -> Option<&str> {
        match self {
            Free::Opaque(label) => Some(label),
            Free::Global(_) | Free::Local(_) => None,
        }
    }
}

impl fmt::Display for Global {
    /// Debug rendering only. A diagnostic names a global through the printer,
    /// which shortens it against the module's other symbols; a witness is named
    /// by its declaring module, which the printer takes from the definition.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Global::Authored(qualifier) => formatter.write_str(&qualifier.join()),
            Global::Witness(id) => write!(formatter, "{id}"),
        }
    }
}

impl fmt::Display for Free {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Free::Global(global) => write!(formatter, "{global}"),
            Free::Local(mint) => write!(formatter, "{mint}"),
            Free::Opaque(label) => formatter.write_str(label),
        }
    }
}
