//! A zero of name resolution: a `use` selector no reference resolved through, a written binder nothing referenced, a declaration nothing reaches — what the lowering, the one stage that resolves every written name, records of a unit for the lint owner (`curios-lint`) to decide its lints from. A zero is a fact and says nothing to a reader: which zeros are reported, and how, is the owner's, which reads beside them what elaboration recorded. The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.
//!
//! **A zero stands in a text of its unit, as a lowered term's position does** ([`curios_core::Extent`]): in the declaration it is about, or in the `use` or `mod` line it is on ([`curios_core::Anchored::Namespace`]). So a zero holds no source, and one a recompile keeps is placed in the text its file is read as now.

/// One zero of name resolution, with where it stands among its unit's texts: `None` for one written nowhere, as a name the compiler spelled is.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum Zero {
    /// A `use` selector nothing resolved through, standing at the selector.
    Selector {
        label: String,
        at: Option<curios_core::Extent>,
    },
    /// A glob nothing resolved through, standing over the whole `use` declaration, since the glob itself is one character.
    Glob {
        path: String,
        at: Option<curios_core::Extent>,
    },
    /// A written binder nothing referenced, standing at its word: what a proof the elaborator writes may still read, by `binder`.
    Binder {
        name: String,
        at: Option<curios_core::Extent>,
        binder: LintedBinder,
    },
    /// A declaration nothing reaches from its unit's roots, standing at its name.
    Declaration {
        name: String,
        at: curios_core::Extent,
    },
    /// A private module none of whose declarations anything reaches, standing at its `mod`: its declarations are none of the unit's zeros.
    Module {
        name: String,
        at: Option<curios_core::Extent>,
    },
}

impl Zero {
    /// Where it stands among its unit's texts.
    pub fn at(&self) -> Option<curios_core::Extent> {
        match self {
            Zero::Selector { at, .. }
            | Zero::Glob { at, .. }
            | Zero::Binder { at, .. }
            | Zero::Module { at, .. } => *at,
            Zero::Declaration { at, .. } => Some(*at),
        }
    }
}

/// A written binder, by its declaration and its place among that declaration's written binders: what a local opened from it carries into elaboration, and what a later lowering of the declaration unchanged gives it too — so one key names it to the elaborator's credits and to a recompile over a baseline. Neither half is a counter another compilation shares.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord))]
pub struct LintedBinder {
    /// `None` for an entry's final term, which no baseline holds.
    pub declaration: Option<curios_core::Global>,
    pub ordinal: u32,
}
