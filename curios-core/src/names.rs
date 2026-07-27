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
    std::{cmp::Ordering, fmt, hash},
};

name!(Atom; archive);

id!(WitnessId, "witness"; archive);

/// A compiler-minted binder's identity: a dense index, plus the display hint the
/// minting site chose.
///
/// The index alone is the identity. The hint is display metadata, excluded from
/// equality, ordering, and hashing exactly as a [`Term`](crate::Term)'s span and
/// a [`Scope`](crate::Scope)'s binder names already are — so a hint can neither
/// make two binders collide nor split one binder in two. Carrying it on the
/// identity rather than only at the binding site means a diagnostic can name a
/// variable wherever the occurrence turns up, instead of recovering the written
/// name by cutting a minted spelling apart.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Mint {
    index: u32,
    hint: Option<String>,
}

impl Mint {
    pub(crate) fn new(index: u32, hint: Option<&str>) -> Self {
        Self {
            index,
            hint: hint.map(str::to_string),
        }
    }

    /// The binder's identity.
    pub fn index(&self) -> u32 {
        self.index
    }

    /// What this binder was called where it was written, if anything — a
    /// rendering aid with no bearing on identity.
    pub fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }

    /// The same identity under a different display hint. Used where a rebuild
    /// has to restore the source spelling of binders it re-minted.
    pub(crate) fn with_hint(&self, hint: Option<&str>) -> Self {
        Self::new(self.index, hint)
    }
}

impl PartialEq for Mint {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for Mint {}

impl PartialOrd for Mint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl hash::Hash for Mint {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

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
    ///
    /// Not yet constructed: a witness still reaches `core` as an authored path
    /// whose last segment the lowerer manufactured. Retyping it is the slice
    /// that turns `Definition::name` into a `Global`.
    Witness(WitnessId),
}

impl Global {
    /// This name's canonical flattened spelling — the key the declaration
    /// registries are still keyed by. A boundary, not a rendering choice:
    /// retired when `InductType`/`Struct`/`Concept` carry a [`Global`] too.
    pub fn symbol(&self) -> String {
        self.to_string()
    }

    /// The module path a programmer wrote this name at, if they wrote one.
    pub fn qualifier(&self) -> Option<&Qualifier> {
        match self {
            Global::Authored(qualifier) => Some(qualifier),
            Global::Witness(_) => None,
        }
    }
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
}

impl Free {
    /// A local binder with identity `index`, rendering as `hint`.
    ///
    /// The index space is shared with `Context::fresh`, which seeds its counter
    /// above every index minted here — see `Context::set_local_floor`.
    pub fn local(index: u32, hint: Option<&str>) -> Self {
        Free::Local(Mint::new(index, hint))
    }

    /// A definition at an authored path.
    pub fn global(qualifier: Qualifier) -> Self {
        Free::Global(Global::Authored(qualifier))
    }

    /// The top-level definition this names, if it names one.
    pub fn as_global(&self) -> Option<&Global> {
        match self {
            Free::Global(global) => Some(global),
            Free::Local(_) => None,
        }
    }

    /// The binder this names, if it names one.
    pub(crate) fn as_local(&self) -> Option<&Mint> {
        match self {
            Free::Local(mint) => Some(mint),
            Free::Global(_) => None,
        }
    }

    /// Whether this is a binder some scope opened, as opposed to a top-level
    /// definition. The typed replacement for testing a spelling for a marker
    /// character — see [`Subterm::has_local_free`](crate::Subterm).
    pub fn is_local(&self) -> bool {
        matches!(self, Free::Local(_))
    }

    /// What a diagnostic should call this, if there is anything better than its
    /// rendered form: a local's minting hint, or nothing for a global, whose
    /// rendering the printer shortens against the module it appears in.
    pub(crate) fn hint(&self) -> Option<&str> {
        self.as_local().and_then(Mint::hint)
    }

    /// The same identity rendering as `hint`. A global has no hint to replace —
    /// its rendering is its path — so it is returned unchanged.
    pub(crate) fn relabelled(&self, hint: &str) -> Self {
        match self {
            Free::Local(mint) => Free::Local(mint.with_hint(Some(hint))),
            Free::Global(_) => self.clone(),
        }
    }
}

/// The archived form keeps the live identity law: the hint is not read.
///
/// A key set that could occur has at most one entry per index — two mints with
/// the same index are one value — so this also agrees with the structural order
/// on every map that survives a round trip.
#[cfg(feature = "archive")]
mod archived_mint {
    use {
        super::ArchivedMint,
        std::{cmp::Ordering, hash},
    };

    impl PartialEq for ArchivedMint {
        fn eq(&self, other: &Self) -> bool {
            self.index == other.index
        }
    }

    impl Eq for ArchivedMint {}

    impl PartialOrd for ArchivedMint {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for ArchivedMint {
        fn cmp(&self, other: &Self) -> Ordering {
            self.index.cmp(&other.index)
        }
    }

    impl hash::Hash for ArchivedMint {
        fn hash<H: hash::Hasher>(&self, state: &mut H) {
            self.index.hash(state);
        }
    }
}

/// A binder identity interned by written name: the same name always yields the
/// same binder, and two different names never collide.
///
/// **Test fixtures only, and it does not exist outside them.** Hand-built core
/// terms have no minting counter to draw from, and their whole vocabulary is
/// the spelling. Deriving an identity from a spelling is exactly the coupling
/// this vocabulary exists to remove, so this is `cfg(test)`-gated rather than
/// merely discouraged: no shipped build contains it, and no other crate can
/// reach it. A production binder comes from `Context::fresh` or `into_core`'s
/// counter. Indices are handed out from the top of the space downwards, so a
/// fixture binder cannot alias a minted one.
#[cfg(test)]
pub(crate) fn fixture_binder(name: &str) -> Free {
    use std::{
        collections::HashMap,
        sync::{LazyLock, Mutex},
    };

    static INTERNED: LazyLock<Mutex<HashMap<String, u32>>> =
        LazyLock::new(|| Mutex::new(HashMap::new()));

    let mut interned = INTERNED.lock().expect("the fixture intern table");
    let next = u32::MAX - u32::try_from(interned.len()).expect("fixture binder space");
    let index = *interned.entry(name.to_string()).or_insert(next);

    Free::local(index, (!name.is_empty()).then_some(name))
}

impl From<&Global> for Free {
    fn from(global: &Global) -> Self {
        Free::Global(global.clone())
    }
}

impl fmt::Display for Mint {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.hint {
            Some(hint) => write!(formatter, "{hint}#{}", self.index),
            None => write!(formatter, "#{}", self.index),
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
        }
    }
}
