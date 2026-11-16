//! What a zero of a written binder comes to, worded for its reader, standing where the zero stands.

use {
    curios_common::Said,
    curios_core::{Lint, LintKind},
    curios_elab::SiteClass,
    curios_module::{LintedBinder, Zero},
};

/// What `zero` is reported as, where it stands, its way out worded by what `class` says a binder binds.
pub fn worded(zero: &Zero, class: impl Fn(&LintedBinder) -> Option<SiteClass>) -> Lint {
    match zero {
        // A binder nothing referenced, underlined at the word. The binder stays where the shape it fills needs a slot, and the name goes: `()` takes the place of one of `{}`, which refuses a value that stops being one, and `_` the place of any other.
        Zero::Binder { name, at, binder } => {
            let way_out = match class(binder) {
                Some(SiteClass::Unit) => "write `()` in its name's place",
                Some(SiteClass::Action) | None => "write `_` in its name's place",
            };
            Lint::unused(LintKind::UnusedBinder, *at, "binder", name, way_out)
        }
        // A `_` that binds `{}`, underlined at it: `()` checks what `_` discards.
        Zero::Wildcard { at, .. } => {
            let mut said = Said::line("unit wildcard");
            said.stating("this `_` binds `{}`");
            said.stating("write `()`, which refuses a value that stops being `{}`");
            Lint {
                kind: LintKind::UnitWildcard,
                said,
                at: *at,
            }
        }
        // A `let` that does nothing, underlined at its pattern: its value runs no action, so it is deleted with nothing lost — or, where it holds an action, run as was likely meant.
        Zero::Let { name, at, binders } => {
            let action =
                matches!(binders.as_slice(), [binder] if class(binder) == Some(SiteClass::Action));
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
            Lint {
                kind: LintKind::UnusedLet,
                said,
                at: *at,
            }
        }
    }
}

/// A declared dependency nothing in the package wrote a name under, standing in no text: its row is the package's manifest's.
pub fn unused_dependency(name: &str) -> Lint {
    Lint::unused(
        LintKind::UnusedDependency,
        None,
        "dependency",
        name,
        "delete its row",
    )
}
