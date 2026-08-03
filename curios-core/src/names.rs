//! Name types for the `core` stage.
//!
//! A name here distinguishes one binding from another and renders for a human. It is not a place to store facts: nothing branches on a name's characters, its prefix, or its collation order. Where a consumer needs structure — which module a definition belongs to, which inductive a constructor came from — that structure is carried as a value by the site that knew it, never recovered by taking a name apart.
//!
//! The rule holds because the *capability* is absent, not because every site remembers it. A name's spelling was once an undocumented wire format between stages — five structured facts flattened into one `String`, each recovered by a hand-rolled parser whose correctness rested on an invariant enforced in another crate and stated nowhere near the parse. Two of those parsers were provably wrong about their own premise. The types below unmerge the facts: [`Free`] discriminates global from local, [`Global`] discriminates an authored path from an anonymous witness, and [`Mint`] separates a binder's identity from its display hint. No path leads from a `Free` to a `&str` except through the printer, so reintroducing behavior-from-spelling means adding a method to a name type — which cannot happen by accident and appears in review as what it is. That is the property to preserve when extending this vocabulary.

#[cfg(test)]
mod tests;

use {
    curios_base::{Qualifier, id, name},
    std::{cmp::Ordering, fmt, hash},
};

name!(Atom; archive);

id!(WitnessId, "witness"; archive);

impl WitnessId {
    /// A witness identity at `index`. Minted by `into_core` from one program-global counter, seeded above the archived prelude's floor.
    pub fn new(index: u32) -> Self {
        Self(index)
    }
}

/// A compiler-minted binder's identity: a dense index, plus the display hint the minting site chose.
///
/// The index alone is the identity. The hint is display metadata, excluded from equality, ordering, and hashing exactly as a [`Term`](crate::Term)'s span and a [`Scope`](crate::Scope)'s binder names already are — so a hint can neither make two binders collide nor split one binder in two. Carrying it on the identity rather than only at the binding site means a diagnostic can name a variable wherever the occurrence turns up, instead of recovering the written name by cutting a minted spelling apart.
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

    /// What this binder was called where it was written, if anything — a rendering aid with no bearing on identity.
    ///
    /// Crate-private, so no path leads from a `Free` to a spelling outside the stage that renders it. The variant itself stays public — downstream code holds and compares binders — but it cannot look inside one. The index has no accessor at all: nothing ever needed to read it, only to compare it.
    pub(crate) fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }

    /// The same identity under a different display hint. Used where a rebuild has to restore the source spelling of binders it re-minted.
    pub fn with_hint(&self, hint: Option<&str>) -> Self {
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

/// A top-level definition's identity: an authored path, or a compiler-generated definition that has no source name at all.
///
/// The two cases are a sum rather than a qualifier with an optional disambiguator, because a witness's declaring module is not its name. Folding both into one field would make [`Qualifier`] mean "module plus the item's own name" for one case and "the declaring module alone" for the other — one field with two readings, which is the defect this vocabulary exists to remove.
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
    /// A `satisfy` declaration. Witnesses are anonymous by design, so this is an identity rather than a manufactured name; the declaring module a diagnostic reports comes from `Definition::island`.
    ///
    /// The identity is minted from one program-global counter, seeded above the archived prelude's floor — see `PreparedPrelude::witness_floor`. Nothing else distinguishes two witnesses, so a per-module ordinal would not do: aliasing one would silently rebind a coherence-table entry.
    Witness(WitnessId),
}

impl Global {
    /// This name's canonical flattened spelling — the key the declaration registries are still keyed by. A boundary, not a rendering choice: retired when `InductType`/`Struct`/`ConceptDecl` carry a [`Global`] too.
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

/// A free variable's identity: a top-level definition, or a binder some scope opened.
///
/// The distinction is a discriminant rather than a spelling convention. Asking "is this a local?" is a `matches!` — exact, and impossible to get wrong the way a marker character in a string could be.
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
    /// The index space is shared with `Context::fresh`, which seeds its counter above every index minted here — see `Context::set_local_floor`.
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
    /// The raw counter behind a locally minted name, or `None` for a global.
    ///
    /// What a binder floor must exceed. `Entropy::seed` only ever raises its counter, so a checker that mints binders of its own can derive a safe floor from the names a module already contains rather than taking one on trust.
    pub fn local_index(&self) -> Option<u32> {
        self.as_local().map(|mint| mint.index)
    }

    pub(crate) fn as_local(&self) -> Option<&Mint> {
        match self {
            Free::Local(mint) => Some(mint),
            Free::Global(_) => None,
        }
    }

    /// Whether this is a binder some scope opened, as opposed to a top-level definition. The typed replacement for testing a spelling for a marker character — see [`Subterm::has_local_free`](crate::Subterm).
    pub fn is_local(&self) -> bool {
        matches!(self, Free::Local(_))
    }

    /// What a diagnostic should call this, if there is anything better than its rendered form: a local's minting hint, or nothing for a global, whose rendering the printer shortens against the module it appears in.
    pub(crate) fn hint(&self) -> Option<&str> {
        self.as_local().and_then(Mint::hint)
    }

    /// The same identity rendering as `hint`. A global has no hint to replace — its rendering is its path — so it is returned unchanged.
    pub(crate) fn relabelled(&self, hint: &str) -> Self {
        match self {
            Free::Local(mint) => Free::Local(mint.with_hint(Some(hint))),
            Free::Global(_) => self.clone(),
        }
    }
}

/// The archived form keeps the live identity law: the hint is not read.
///
/// A key set that could occur has at most one entry per index — two mints with the same index are one value — so this also agrees with the structural order on every map that survives a round trip.
///
/// Both halves are written by hand for that reason, and **a third comparison surface on [`Mint`](super::Mint) must be written by hand too — a derive there is the bug this note exists to prevent.** A derived comparison reads the hint, so the live and archived orderings would disagree only on maps that round-trip, with nothing to surface the divergence.
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
    /// Debug rendering only. A diagnostic names a global through the printer, which shortens it against the module's other symbols; a witness is named by its declaring module, which the printer takes from the definition.
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
