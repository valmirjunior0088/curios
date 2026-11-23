//! A lint: an exact finding, worded for its reader, standing where the fact it was decided from stands — one value, made by the stage that establishes the fact it is decided from and read by whatever reports a unit's findings, as an [`Unresolved`](crate::Unresolved) is.
//!
//! **A lint stands in a text of its unit, as a lowered term's position does** ([`Extent`]): in the declaration it is about, or in the `use` or `mod` line it is on ([`Anchored::Namespace`](crate::Anchored::Namespace)). So a stored lint holds no source, a unit storing its lints writes none of its files a second time, and its report is made where it is shown ([`Lint::report`]), from the anchors of the unit that holds it.
//!
//! **Each stage words what it decides.** There are no levels and no suppression: what keeps a name is spelled in the program, and each message says how. The decision is `documentation/design/tooling/a-lint-is-an-exact-finding-read-off-the-compilation.md`.

use {
    crate::{Anchors, Extent},
    curios_common::{Report, Said},
    std::fmt,
};

/// The kinds, each named as `curios lint` documents it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum LintKind {
    UnusedImport,
    UnusedBinder,
    UnusedLet,
    UnitWildcard,
    UnusedDeclaration,
    UnusedDependency,
    RenamingBinder,
}

impl LintKind {
    pub fn name(self) -> &'static str {
        match self {
            LintKind::UnusedImport => "unused-import",
            LintKind::UnusedBinder => "unused-binder",
            LintKind::UnusedLet => "unused-let",
            LintKind::UnitWildcard => "unit-wildcard",
            LintKind::UnusedDeclaration => "unused-declaration",
            LintKind::UnusedDependency => "unused-dependency",
            LintKind::RenamingBinder => "renaming-binder",
        }
    }
}

/// One finding: its kind, what a reader is told of it, and where it stands among the unit's texts.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Lint {
    pub kind: LintKind,
    pub said: Said,
    /// What it underlines, in a text of its unit: `None` for one written nowhere, as a name the compiler spelled is, or one about the package rather than any text of it.
    pub at: Option<Extent>,
}

impl Lint {
    /// What a lint says of `name`, a `noun` nothing uses: that it is unused, and what to write in its place — the one phrasing every `unused-*` kind shares.
    pub fn unused(
        kind: LintKind,
        at: Option<Extent>,
        noun: &str,
        name: impl fmt::Display,
        way_out: impl fmt::Display,
    ) -> Self {
        let mut said = Said::line(format!("unused {noun}"));
        said.stating(format_args!("`{name}` is never used"));
        said.stating(way_out);

        Self { kind, said, at }
    }

    /// The report a reader sees, placed by `anchors` — the unit's own, which hold every text a lint of it stands in — and unlocated where none holds its text.
    pub fn report(&self, anchors: &[&Anchors]) -> Report {
        Report::of(
            self.at.and_then(|at| at.resolved(anchors)),
            self.said.clone(),
        )
    }

    /// `lints` in reading order, as `anchors` place them: by the source they are about, its name and then its file, then by position in it, unlocated ones last. The stages decide in orders of their own — resolution walks a unit's items in dependency order and the modules in discovery order, elaboration files declarations as its walk settles them — none of which is the order a reader opens files in.
    pub fn ordered(mut lints: Vec<Lint>, anchors: &Anchors) -> Vec<Lint> {
        // An unlocated lint keys as `None`, which sorts first; `Reverse` on the presence alone puts it last without touching the order of the rest.
        lints.sort_by_cached_key(|lint| {
            let located = lint.at.and_then(|at| anchors.resolve(&at)).map(|span| {
                (
                    span.source.name().cloned(),
                    span.source.file().map(ToOwned::to_owned),
                    span.start,
                )
            });
            (std::cmp::Reverse(located.is_some()), located)
        });
        lints
    }
}
