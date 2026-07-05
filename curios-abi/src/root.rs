//! First-class compilation-root identity.
//!
//! A compilation is a small, ordered set of roots — today always `sys`, `syn`,
//! `std`, and the entry program, in that dependency order; later, a package
//! manager's resolved dependencies join the same list. [`RootId`] is the
//! opaque handle every other stage compares by equality instead of re-deriving
//! "which root does this belong to" from a qualified-name string; [`Root`]
//! is the descriptor a `RootId` resolves to. This lives in `curios-abi`
//! (a pure leaf) because it is needed both by `curios-core` (on `Concept`,
//! `Structure`, `Inductive` registry entries) and by `curios-rt` (on the wasm
//! import-namespace/link-loop side), and those two crates share no other
//! common dependency.

/// An opaque handle identifying one root within a single compilation. Cheap
/// to carry and compare — a dense index, not a string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RootId(u32);

impl RootId {
    /// Fixed ids for the three embedded roots, stable across every
    /// compilation. Needed because [`HeadKey`](../../curios_core/concept/enum.HeadKey.html)'s
    /// primitive variants (`Nat`, `Int`, `Flt`, `Bln`, `Bin`, `Io`, `Lst`,
    /// `Cell`) have no textual declaration of their own to stamp a root onto
    /// — they are simply defined to be `/sys`-owned for orphan-rule purposes.
    pub const SYS: RootId = RootId(0);
    pub const SYN: RootId = RootId(1);
    pub const STD: RootId = RootId(2);

    /// The first id available to a compilation's non-fixed roots (the entry
    /// program, and later a package manager's resolved dependencies).
    const FIRST_DYNAMIC: u32 = 3;

    /// Construct the `n`th dynamic root's id (`n = 0` is the entry program
    /// today; a future package manager's dependencies follow).
    pub const fn dynamic(n: u32) -> RootId {
        RootId(Self::FIRST_DYNAMIC + n)
    }

    /// The entry program's fixed id — today the only dynamic root.
    pub const ENTRY: RootId = RootId::dynamic(0);

    /// The `RootId` a qualified name's leading path segment names — the
    /// single source of truth every consumer classifies a root by, replacing
    /// what used to be independently hardcoded allowlists/constants in
    /// `curios-text` (`INTERNAL_ROOTS`/`PRIVILEGED_ROOTS`/`PRELUDE_ROOTS`) and
    /// in `curios-abi` itself (`NAMESPACE_SYS`/`NAMESPACE_ENV`/`NAMESPACE_SYN`).
    /// Any segment that isn't one of the three embedded roots is the entry
    /// program — today the only other root there is.
    pub fn of_segment(segment: &str) -> RootId {
        match segment {
            "sys" => RootId::SYS,
            "syn" => RootId::SYN,
            "std" => RootId::STD,
            _ => RootId::ENTRY,
        }
    }

    /// This root's privilege tier. `sys` is internal; `sys`/`syn`/`std` are
    /// each privileged (may reference an internal root); everything else —
    /// the entry program, and every future package — is ordinary.
    pub fn kind(self) -> RootKind {
        match self {
            RootId::SYS => RootKind::Internal,
            RootId::SYN | RootId::STD => RootKind::Privileged,
            _ => RootKind::Ordinary,
        }
    }
}

/// A root's privilege tier — replaces the old `INTERNAL_ROOTS`/`PRIVILEGED_ROOTS`
/// string-literal allowlists with a field carried on the root itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RootKind {
    /// Reachable only from a privileged root — `sys` today. Discoverable (so
    /// the standard library can resolve it by absolute path) but rejected
    /// when referenced from an ordinary consumer.
    Internal,
    /// May reference an internal root — `sys`, `syn`, `std` today.
    Privileged,
    /// No special reach — the entry program, and every future package.
    Ordinary,
}

impl RootKind {
    /// Whether a root of this kind may reference an [`RootKind::Internal`]
    /// root. An internal root is trivially privileged over itself (`sys`
    /// referencing `sys` is not a violation).
    pub fn is_privileged(self) -> bool {
        matches!(self, RootKind::Internal | RootKind::Privileged)
    }
}

/// One compilation root's descriptor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Root {
    pub id: RootId,
    /// The root's top-level path segment, e.g. `"sys"`, `"std"`, or the
    /// entry program's (today always empty — the entry program is
    /// unqualified, per `Structure::module`'s "the root module is the empty
    /// string" convention).
    pub name: String,
    pub kind: RootKind,
}

impl Root {
    pub fn new(id: RootId, name: impl Into<String>, kind: RootKind) -> Self {
        Self {
            id,
            name: name.into(),
            kind,
        }
    }
}
