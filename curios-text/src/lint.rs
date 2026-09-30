//! A lint: an exact finding read off name resolution, reported as a diagnostic that stops nothing.
//!
//! Every lint is a zero — a `use` selector no reference resolved through, a binder nothing read, a declaration nothing reaches — decided by the lowering, which is the one stage that resolves every written name. The one reference no written name makes is a proof the elaborator writes from the facts in scope, which reads the binders it sums: a binder one reads is *credited* once elaboration has run ([`PreparedText::credit`](crate::PreparedText::credit)), and `unused-binder` does not report it. There are no levels and no suppression: what keeps a name is spelled in the program, and each message says how. The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.

use {
    curios_utilities::{Report, Span},
    std::collections::BTreeSet,
};

use super::Label;

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

/// One finding: its kind, and the report a reader sees — the message and the span it underlines, exactly as an error's.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Lint {
    pub kind: LintKind,
    pub report: Report,
    /// The binder an `unused-binder` lint names: what a credit is matched against. `None` for every other kind.
    pub binder: Option<LintedBinder>,
}

/// A written binder, by its declaration and its place among that declaration's written binders: what a local opened from it carries into elaboration, and what a later lowering of the declaration unchanged gives it too — so one key names it to the elaborator's credits and to a recompile over a baseline. Neither half is a counter another compilation shares.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived]
pub struct LintedBinder {
    /// `None` for an entry's final term, which no baseline holds.
    pub declaration: Option<curios_core::Global>,
    pub ordinal: u32,
}

impl Lint {
    /// A `use` selector nothing resolved through, underlined at the selector.
    pub(crate) fn unused_import(label: &Label) -> Self {
        Self::at(
            LintKind::UnusedImport,
            label.span(),
            format!("unused import `{label}`; delete it"),
        )
    }

    /// A glob nothing resolved through, underlined over the whole `use` declaration — the glob itself is one character.
    pub(crate) fn unused_glob(span: Option<&Span>, path: &str) -> Self {
        Self::at(
            LintKind::UnusedImport,
            span,
            format!("unused import `{path}`; delete it"),
        )
    }

    /// A binder nothing referenced, underlined at the word. The name is kept by prefixing it with `_`, which is what the message says.
    pub(crate) fn unused_binder(name: &str, span: &Span, binder: LintedBinder) -> Self {
        Self {
            binder: Some(binder),
            ..Self::at(
                LintKind::UnusedBinder,
                Some(span),
                format!("unused binder `{name}`; name it `_{name}` to keep it"),
            )
        }
    }

    /// Whether a proof the elaborator wrote read the binder this names, which is then used: `credited` holds each such binder by its declaration and its place, as elaboration reports it.
    pub fn is_credited(&self, credited: &BTreeSet<(Option<curios_core::Global>, u32)>) -> bool {
        self.binder
            .is_some_and(|binder| credited.contains(&(binder.declaration, binder.ordinal)))
    }

    /// A declaration nothing reaches, underlined at its name. Kept by a `_` prefix, as a binder is, or by `pub`, which makes it part of what the unit hands out.
    pub(crate) fn unused_declaration(name: &str, span: Span) -> Self {
        Self::at(
            LintKind::UnusedDeclaration,
            Some(&span),
            format!("unused declaration `{name}`; name it `_{name}` or make it `pub` to keep it"),
        )
    }

    fn at(kind: LintKind, span: Option<&Span>, message: String) -> Self {
        let report = match span {
            Some(span) => Report::at(span.clone(), message),
            None => Report::unlocated(message),
        };
        Self {
            kind,
            report,
            binder: None,
        }
    }

    /// The report as `curios lint` prints it.
    pub fn render(&self) -> String {
        self.report.render()
    }
}

/// Lints in reading order: by the file they are about, then by position in it, unlocated ones last. Resolution walks a unit's items in dependency order and the modules in discovery order, neither of which is the order a reader opens files in.
pub(crate) fn ordered(mut lints: Vec<Lint>) -> Vec<Lint> {
    // An unlocated lint keys as `None`, which sorts first; `Reverse` on the presence alone puts it last without touching the order of the rest.
    lints.sort_by_cached_key(|lint| {
        let located = lint.report.span.as_ref().map(|span| {
            (
                span.source
                    .path
                    .as_ref()
                    .map(|path| path.display().to_string()),
                span.start,
            )
        });
        (std::cmp::Reverse(located.is_some()), located)
    });
    lints
}
