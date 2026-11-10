//! A lint: an exact finding read off name resolution, reported as a diagnostic that stops nothing.
//!
//! Every lint is a zero — a `use` selector no reference resolved through, a binder nothing read, a declaration nothing reaches — decided by the lowering, which is the one stage that resolves every written name. The one reference no written name makes is a proof the elaborator writes from the facts in scope, which reads the binders it sums: a binder one reads is *credited* once elaboration has run ([`PreparedText::credit`](crate::PreparedText::credit)), and `unused-binder` does not report it. There are no levels and no suppression: what keeps a name is spelled in the program, and each message says how. The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.
//!
//! **A lint stands in a text of its unit, as a lowered term's position does** ([`curios_core::Extent`]): in the declaration it is about, or in the `use` or `mod` line it is on ([`curios_core::Anchored::Namespace`]). So a stored lint holds no source, a unit storing its lints writes none of its files a second time, and a lint carried past a recompile is placed in the text its file is read as now. Its report is made where it is shown ([`Lint::report`]), from the anchors of the unit that holds it.

use {curios_common::Report, curios_surface::Label, std::collections::BTreeSet};

/// The kinds, each named as `curios lint` documents it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum LintKind {
    UnusedImport,
    UnusedBinder,
    UnusedDeclaration,
}

impl LintKind {
    pub fn name(self) -> &'static str {
        match self {
            LintKind::UnusedImport => "unused-import",
            LintKind::UnusedBinder => "unused-binder",
            LintKind::UnusedDeclaration => "unused-declaration",
        }
    }
}

/// One finding: its kind, the message a reader sees, and where it stands among the unit's texts.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Lint {
    pub kind: LintKind,
    pub message: String,
    /// What it underlines, in a text of its unit: `None` for one written nowhere, as a name the compiler spelled is.
    pub at: Option<curios_core::Extent>,
    /// The binder an `unused-binder` lint names: what a credit is matched against. `None` for every other kind.
    pub binder: Option<LintedBinder>,
}

/// A written binder, by its declaration and its place among that declaration's written binders: what a local opened from it carries into elaboration, and what a later lowering of the declaration unchanged gives it too — so one key names it to the elaborator's credits and to a recompile over a baseline. Neither half is a counter another compilation shares.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord))]
pub struct LintedBinder {
    /// `None` for an entry's final term, which no baseline holds.
    pub declaration: Option<curios_core::Global>,
    pub ordinal: u32,
}

impl Lint {
    /// A `use` selector nothing resolved through, underlined at the selector.
    pub(crate) fn unused_import(label: &Label, at: Option<curios_core::Extent>) -> Self {
        Self::at(
            LintKind::UnusedImport,
            at,
            format!("unused import `{label}`; delete it"),
        )
    }

    /// A glob nothing resolved through, underlined over the whole `use` declaration — the glob itself is one character.
    pub(crate) fn unused_glob(at: Option<curios_core::Extent>, path: &str) -> Self {
        Self::at(
            LintKind::UnusedImport,
            at,
            format!("unused import `{path}`; delete it"),
        )
    }

    /// A binder nothing referenced, underlined at the word. The binder stays where the shape it fills needs a slot, and the name goes: `_` takes its place.
    pub(crate) fn unused_binder(
        name: &str,
        at: Option<curios_core::Extent>,
        binder: LintedBinder,
    ) -> Self {
        Self {
            binder: Some(binder),
            ..Self::at(
                LintKind::UnusedBinder,
                at,
                format!("unused binder `{name}`; write `_` in its name's place"),
            )
        }
    }

    /// Whether a proof the elaborator wrote read the binder this names, which is then used: `credited` holds each such binder by its declaration and its place, as elaboration reports it.
    pub fn is_credited(&self, credited: &BTreeSet<(Option<curios_core::Global>, u32)>) -> bool {
        self.binder
            .is_some_and(|binder| credited.contains(&(binder.declaration, binder.ordinal)))
    }

    /// A declaration nothing reaches, underlined at its name. Deleted, made part of what the unit hands out by `pub`, or kept unused on purpose by a `_` prefix.
    pub(crate) fn unused_declaration(name: &str, at: curios_core::Extent) -> Self {
        Self::at(
            LintKind::UnusedDeclaration,
            Some(at),
            format!(
                "unused declaration `{name}`; delete it, make it `pub`, or name it `_{name}` to keep it unused"
            ),
        )
    }

    /// A private module none of whose declarations anything reaches, underlined at its `mod`: one deletion removes them all.
    pub(crate) fn unused_module(name: &str, at: Option<curios_core::Extent>) -> Self {
        Self::at(
            LintKind::UnusedDeclaration,
            at,
            format!("unused module `{name}`; delete it or make it `pub`"),
        )
    }

    fn at(kind: LintKind, at: Option<curios_core::Extent>, message: String) -> Self {
        Self {
            kind,
            message,
            at,
            binder: None,
        }
    }

    /// The report a reader sees, placed by `anchors` — the unit's own, which hold every text a lint of it stands in — and unlocated where none holds its text.
    pub fn report(&self, anchors: &[&curios_core::Anchors]) -> Report {
        match self.at.and_then(|at| at.resolved(anchors)) {
            Some(span) => Report::at(span, self.message.clone()),
            None => Report::unlocated(self.message.clone()),
        }
    }
}

/// Lints in reading order, as `anchors` place them: by the file they are about, then by position in it, unlocated ones last. Resolution walks a unit's items in dependency order and the modules in discovery order, neither of which is the order a reader opens files in.
pub fn ordered(mut lints: Vec<Lint>, anchors: &curios_core::Anchors) -> Vec<Lint> {
    // An unlocated lint keys as `None`, which sorts first; `Reverse` on the presence alone puts it last without touching the order of the rest.
    lints.sort_by_cached_key(|lint| {
        let located = lint.at.and_then(|at| anchors.resolve(&at)).map(|span| {
            (
                span.source.path().map(|path| path.display().to_string()),
                span.start,
            )
        });
        (std::cmp::Reverse(located.is_some()), located)
    });
    lints
}
