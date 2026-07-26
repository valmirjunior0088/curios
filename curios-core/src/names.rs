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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(feature = "archive", rkyv(derive(PartialEq, Eq, Hash)))]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(feature = "archive", rkyv(derive(PartialEq, Eq, Hash)))]
pub enum Free {
    Global(Global),
    Local(Mint),
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
        }
    }
}
