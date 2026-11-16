//! The binder lints, decided where elaboration files a declaration's record: from the sites the lowering found no written name reads ([`curios_core::Unread`]), less every binder a proof elaboration wrote read, worded by what each site binds ([`SiteClass`]). Each lint is decided by the stage that establishes its last fact, and what a proof reads is elaboration's: the decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.

use {
    crate::SiteClass,
    curios_common::Said,
    curios_core::{Lint, LintKind, Unread, WrittenBinder},
    std::collections::BTreeSet,
};

/// The lints `sites` come to: each reported, a `_` only where it binds `{}`, less every binder `read` holds of — one a proof the elaborator wrote reads, which is used though no written name reaches it. A `let` none of whose binders is read either way does nothing, whatever its value: it is reported whole, once, and its binders are none of `unused-binder`'s. `class` says what a binder binds where elaboration decided it, which words the way out.
pub fn binder_lints<'s>(
    sites: impl IntoIterator<Item = &'s Unread>,
    read: impl Fn(&WrittenBinder) -> bool,
    class: impl Fn(&WrittenBinder) -> Option<SiteClass>,
) -> Vec<Lint> {
    let sites = sites.into_iter().collect::<Vec<_>>();
    let dead = |binders: &[WrittenBinder]| !binders.iter().any(&read);
    let in_dead_let = sites
        .iter()
        .filter_map(|site| match site {
            Unread::Let { binders, .. } if dead(binders) => Some(binders),
            _ => None,
        })
        .flatten()
        .copied()
        .collect::<BTreeSet<_>>();

    sites
        .into_iter()
        .filter(|site| match site {
            Unread::Binder { binder, .. } => !read(binder) && !in_dead_let.contains(binder),
            // A `_` of `{}` is asked to be `()`, which refuses a value that stops being one; a `_` of anything else is the slot it should be.
            Unread::Wildcard { binder, .. } => {
                !read(binder)
                    && !in_dead_let.contains(binder)
                    && class(binder) == Some(SiteClass::Unit)
            }
            Unread::Let { binders, .. } => dead(binders),
        })
        .map(|site| worded(site, &class))
        .collect()
}

/// What `site` is reported as, where it stands, its way out worded by what `class` says a binder binds.
fn worded(site: &Unread, class: impl Fn(&WrittenBinder) -> Option<SiteClass>) -> Lint {
    match site {
        // A binder nothing referenced, underlined at the word. The binder stays where the shape it fills needs a slot, and the name goes: `()` takes the place of one of `{}`, which refuses a value that stops being one, and `_` the place of any other.
        Unread::Binder { name, at, binder } => {
            let way_out = match class(binder) {
                Some(SiteClass::Unit) => "write `()` in its name's place",
                Some(SiteClass::Action) | None => "write `_` in its name's place",
            };
            Lint::unused(LintKind::UnusedBinder, *at, "binder", name, way_out)
        }
        // A `_` that binds `{}`, underlined at it: `()` checks what `_` discards.
        Unread::Wildcard { at, .. } => {
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
        Unread::Let { name, at, binders } => {
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
