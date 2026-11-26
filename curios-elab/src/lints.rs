//! The binder lints and `unused-case`, decided where elaboration files a declaration's record: from the sites the lowering found no written name reads ([`curios_core::Unread`]), less every binder a proof elaboration wrote read, worded by what each site binds ([`SiteClass`]), and from the rows of matches of one arm, by whether their bodies read what their cases state ([`LoneArm`]). Each lint is decided by the stage that establishes its last fact, and what a proof reads is elaboration's: the decision is `documentation/design/tooling/a-lint-is-an-exact-finding-read-off-the-compilation.md`.

use {
    crate::{LoneArm, SiteClass},
    curios_common::Said,
    curios_core::{Extent, Lint, LintKind, Unread, WrittenBinder},
    std::collections::BTreeSet,
};

/// The lints `sites` come to: each reported, a `_` only where it binds `{}`, less every binder `read` holds of — one a proof the elaborator wrote reads, which is used though no written name reaches it. A `let` none of whose binders is read either way does nothing, whatever its value: it is reported whole, once, and its binders are none of `unused-binder`'s. `class` says what a binder binds where elaboration decided it, which words the way out.
///
/// A row of `lone` decides nothing where `case_read` says no lookup answered from the frames of its cases and no binder it writes is read: its match is reported, once wherever it was elaborated more than once and every elaboration read nothing, and its binders are none of the binder lints'.
pub fn binder_lints<'s>(
    sites: impl IntoIterator<Item = &'s Unread>,
    lone: &[LoneArm],
    case_read: impl Fn(&LoneArm) -> bool,
    read: impl Fn(&WrittenBinder) -> bool,
    class: impl Fn(&WrittenBinder) -> Option<SiteClass>,
) -> Vec<Lint> {
    let sites = sites.into_iter().collect::<Vec<_>>();
    let dead = |binders: &[WrittenBinder]| !binders.iter().any(&read);
    // A binder no written name references is among the sites; one a proof read is read all the same.
    let unread = sites
        .iter()
        .filter(|site| !matches!(site, Unread::Let { .. }))
        .flat_map(|site| site.binders())
        .filter(|binder| !read(binder))
        .copied()
        .collect::<BTreeSet<_>>();
    let decides_nothing =
        |arm: &LoneArm| !case_read(arm) && arm.binders.iter().all(|binder| unread.contains(binder));
    let mut unused_cases = Vec::<&LoneArm>::new();
    for arm in lone {
        if lone
            .iter()
            .filter(|other| other.at == arm.at)
            .all(&decides_nothing)
            && unused_cases.iter().all(|reported| reported.at != arm.at)
        {
            unused_cases.push(arm);
        }
    }
    let in_unused_case = unused_cases
        .iter()
        .flat_map(|arm| arm.binders.iter().copied())
        .collect::<BTreeSet<_>>();
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
            Unread::Binder { binder, .. } => {
                !read(binder) && !in_dead_let.contains(binder) && !in_unused_case.contains(binder)
            }
            // A `_` of `{}` is asked to be `()`, which refuses a value that stops being one; a `_` of anything else is the slot it should be.
            Unread::Wildcard { binder, .. } => {
                !read(binder)
                    && !in_dead_let.contains(binder)
                    && !in_unused_case.contains(binder)
                    && class(binder) == Some(SiteClass::Unit)
            }
            Unread::Let { binders, .. } => dead(binders),
        })
        .map(|site| worded(site, &class))
        .chain(unused_cases.iter().map(|arm| unused_case(arm.at)))
        .collect()
}

/// A match of one arm whose body reads nothing its case states, underlined at its head: the match decides nothing the body reads, so the body stands without it.
fn unused_case(at: Extent) -> Lint {
    let mut said = Said::line("unused case");
    said.stating("this match decides nothing its body reads");
    said.stating("write the body without the match");

    Lint {
        kind: LintKind::UnusedCase,
        said,
        at: Some(at),
    }
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
            let mut lint = Lint::unused(LintKind::UnusedBinder, *at, "binder", name, way_out);
            // A `_` before a name keeps a declaration unused and no binder, which `_` alone does: what another language's habit expects is said where it fails.
            if name.to_string().starts_with('_') {
                lint.said
                    .stating("a `_` before a name keeps a declaration unused, and no binder");
            }
            lint
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
