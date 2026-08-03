//! First-class compilation-root identity.
//!
//! A compilation is a small, fixed set of roots: `sys`, `syn`, `std`, and the entry program. [`RootId`] is the handle every other stage compares by equality instead of re-deriving "which root does this belong to" from a qualified-name string. It lives in `curios-base` — the compiler's shared vocabulary — because both `curios-text` (module resolution) and `curios-elab` (on `ConceptDecl`, `Structure`, and `Inductive` registry entries) key on it. Neither the host/guest wire ABI nor the runtime names it.

#[cfg(test)]
mod tests;

/// A compilation's four roots. `Sys`/`Syn`/`Std` are the embedded standard library; `Entry` is the program being compiled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum RootId {
    Sys,
    Syn,
    Std,
    Entry,
}

impl RootId {
    /// The `RootId` a qualified name's leading path segment names. Any segment that isn't one of the three embedded roots is the entry program — today the only other root there is.
    pub fn of_segment(segment: &str) -> RootId {
        match segment {
            "sys" => RootId::Sys,
            "syn" => RootId::Syn,
            "std" => RootId::Std,
            _ => RootId::Entry,
        }
    }

    /// This root's privilege tier. `sys` is internal; `sys`/`syn`/`std` are each privileged (may reference an internal root); the entry program is ordinary.
    pub fn kind(self) -> RootKind {
        match self {
            RootId::Sys => RootKind::Internal,
            RootId::Syn | RootId::Std => RootKind::Privileged,
            RootId::Entry => RootKind::Ordinary,
        }
    }
}

/// A root's privilege tier — replaces the old `INTERNAL_ROOTS`/`PRIVILEGED_ROOTS` string-literal allowlists with a field carried on the root itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum RootKind {
    /// Reachable only from a privileged root — `sys` today. Discoverable (so the standard library can resolve it by absolute path) but rejected when referenced from an ordinary consumer.
    Internal,
    /// May reference an internal root — `sys`, `syn`, `std` today.
    Privileged,
    /// No special reach — the entry program.
    Ordinary,
}

impl RootKind {
    /// Whether a root of this kind may reference an [`RootKind::Internal`] root. An internal root is trivially privileged over itself (`sys` referencing `sys` is not a violation).
    pub fn is_privileged(self) -> bool {
        matches!(self, RootKind::Internal | RootKind::Privileged)
    }
}
