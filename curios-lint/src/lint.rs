//! A lint: an exact finding, worded for its reader, standing where the fact it was decided from stands.
//!
//! **A lint stands in a text of its unit, as a lowered term's position does** ([`curios_core::Extent`]): in the declaration it is about, or in the `use` or `mod` line it is on ([`curios_core::Anchored::Namespace`]). So a stored lint holds no source, a unit storing its lints writes none of its files a second time, and its report is made where it is shown ([`Lint::report`]), from the anchors of the unit that holds it.

use {
    curios_common::{Report, Said},
    curios_elab::SiteClass,
    curios_module::{LintedBinder, Zero},
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
    pub at: Option<curios_core::Extent>,
}

impl Lint {
    /// What `zero` is reported as, where it stands, its way out worded by what `class` says a binder binds. There are no levels and no suppression: what keeps a name is spelled in the program, and each message says how.
    pub(crate) fn of(zero: &Zero, class: impl Fn(&LintedBinder) -> Option<SiteClass>) -> Self {
        match zero {
            // A `use` selector nothing resolved through, underlined at the selector.
            Zero::Selector { label, at } => Self::at(
                LintKind::UnusedImport,
                *at,
                unused("import", label, "delete it"),
            ),
            // A glob nothing resolved through, underlined over the whole `use` declaration — the glob itself is one character.
            Zero::Glob { path, at } => Self::at(
                LintKind::UnusedImport,
                *at,
                unused("import", path, "delete it"),
            ),
            // A binder nothing referenced, underlined at the word. The binder stays where the shape it fills needs a slot, and the name goes: `()` takes the place of one of `{}`, which refuses a value that stops being one, and `_` the place of any other.
            Zero::Binder { name, at, binder } => {
                let way_out = match class(binder) {
                    Some(SiteClass::Unit) => "write `()` in its name's place",
                    Some(SiteClass::Action) | None => "write `_` in its name's place",
                };
                Self::at(LintKind::UnusedBinder, *at, unused("binder", name, way_out))
            }
            // A `_` that binds `{}`, underlined at it: `()` checks what `_` discards.
            Zero::Wildcard { at, .. } => {
                let mut said = Said::line("unit wildcard");
                said.stating("this `_` binds `{}`");
                said.stating("write `()`, which refuses a value that stops being `{}`");
                Self::at(LintKind::UnitWildcard, *at, said)
            }
            // A `let` that does nothing, underlined at its pattern: its value runs no action, so it is deleted with nothing lost — or, where it holds an action, run as was likely meant.
            Zero::Let { name, at, binders } => {
                let action = matches!(binders.as_slice(), [binder] if class(binder) == Some(SiteClass::Action));
                let mut said = Said::line("unused let");
                match (name, action) {
                    (Some(name), true) => {
                        said.stating(format_args!(
                            "`{name}` is an action this program never runs"
                        ));
                    }
                    (None, true) => {
                        said.stating("this `_` is an action this program never runs");
                    }
                    (Some(name), false) => {
                        said.stating(format_args!(
                            "`{name}` is never used, and its value runs no action"
                        ));
                    }
                    (None, false) => {
                        said.stating("nothing reads this `let`, and its value runs no action");
                    }
                }
                said.stating(match action {
                    true => "run it with `!`, or delete it",
                    false => "delete it",
                });
                Self::at(LintKind::UnusedLet, *at, said)
            }
            // A declaration nothing reaches, underlined at its name. Deleted, made part of what the unit hands out by `pub`, or kept unused on purpose by a `_` prefix.
            Zero::Declaration { name, at } => Self::at(
                LintKind::UnusedDeclaration,
                Some(*at),
                unused(
                    "declaration",
                    name,
                    format_args!(
                        "delete it, make it `pub`, or name it `_{name}` to keep it unused"
                    ),
                ),
            ),
            // A private module none of whose declarations anything reaches, underlined at its `mod`: one deletion removes them all.
            Zero::Module { name, at } => Self::at(
                LintKind::UnusedDeclaration,
                *at,
                unused("module", name, "delete it or make it `pub`"),
            ),
        }
    }

    /// A declared dependency nothing in the package wrote a name under, standing in no text: its row is the package's manifest's.
    pub(crate) fn unused_dependency(name: &str) -> Self {
        Self::at(
            LintKind::UnusedDependency,
            None,
            unused("dependency", name, "delete its row"),
        )
    }

    fn at(kind: LintKind, at: Option<curios_core::Extent>, said: Said) -> Self {
        Self { kind, said, at }
    }

    /// The report a reader sees, placed by `anchors` — the unit's own, which hold every text a lint of it stands in — and unlocated where none holds its text.
    pub fn report(&self, anchors: &[&curios_core::Anchors]) -> Report {
        Report::of(
            self.at.and_then(|at| at.resolved(anchors)),
            self.said.clone(),
        )
    }
}

/// What a lint says of `name`, a `noun` nothing uses: that it is unused, and what to write in its place.
fn unused(noun: &str, name: impl fmt::Display, way_out: impl fmt::Display) -> Said {
    let mut said = Said::line(format!("unused {noun}"));
    said.stating(format_args!("`{name}` is never used"));
    said.stating(way_out);

    said
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
