//! A zero of name resolution about a written binder: a binder nothing referenced, a written `_`, a `let` that runs nothing and none of whose binders anything referenced — what the lowering, the one stage that resolves every written name, records of a unit for the binder lints, whose last fact is what a proof the elaborator writes reads. A zero is a fact and says nothing to a reader: which zeros are reported, and how, is decided beside what elaboration recorded. The lints resolution alone decides — an import nothing resolved through, a declaration nothing reaches — the lowering words itself ([`PreparedText::lints`](crate::PreparedText::lints)). The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.
//!
//! **A zero stands in a text of its unit, as a lowered term's position does** ([`curios_core::Extent`]): in the declaration it is about. So a zero holds no source, and one a recompile keeps is placed in the text its file is read as now.

/// One zero of name resolution, with where it stands among its unit's texts: `None` for one written nowhere, as a name the compiler spelled is.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum Zero {
    /// A written binder nothing referenced, standing at its word: what a proof the elaborator writes may still read, by `binder`.
    Binder {
        name: String,
        at: Option<curios_core::Extent>,
        binder: LintedBinder,
    },
    /// A written `_`, standing at it: a slot no written name reads, which a proof the elaborator writes may still read, by `binder`, and which binds what its class says.
    Wildcard {
        at: Option<curios_core::Extent>,
        binder: LintedBinder,
    },
    /// A `let` whose value runs no action and none of whose binders a written name referenced, standing at its pattern: one that does nothing where no proof the elaborator writes reads a binder of it either, by `binders`. `name` is what its pattern names where it is one binder with a name.
    Let {
        name: Option<String>,
        at: Option<curios_core::Extent>,
        binders: Vec<LintedBinder>,
    },
}

impl Zero {
    /// Where it stands among its unit's texts.
    pub fn at(&self) -> Option<curios_core::Extent> {
        match self {
            Zero::Binder { at, .. } | Zero::Wildcard { at, .. } | Zero::Let { at, .. } => *at,
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
