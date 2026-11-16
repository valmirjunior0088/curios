//! The lint owner: every lint decided from the facts the stages recorded of a unit, worded, placed and ordered for its reader.
//!
//! **A stage records, this crate decides.** The lowering records each zero of name resolution — a `use` selector nothing resolved through, a written binder nothing referenced, a declaration nothing reaches ([`curios_module::Zero`]) — and elaboration records each written binder a proof it wrote read, which no written name reaches. Neither words a lint: which facts are reported, as what, where and in what order is decided here, from both. The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.
//!
//! **Decided once, where a unit is made.** A unit's lints are decided where `curios-unit` makes it and stored with it, so a unit compiled now, restored from a store or compiled over a baseline reports the same lints, and asking again decides nothing. A package's `unused-dependency` is decided where its units' reached mounts are gathered ([`unused_dependencies`]).

#[cfg(test)]
mod tests;

mod lint;
use lint::*;
pub use lint::{ArchivedLint, ArchivedLintKind, Lint, LintKind};

use {
    curios_common::Qualifier,
    curios_module::{LintedBinder, Zero},
    std::collections::BTreeSet,
};

/// The lints `zeros` come to, in reading order as `anchors` place them: each zero reported, less every binder zero whose binder `read` holds of — one a proof the elaborator wrote reads, which is used though no written name reaches it.
pub fn lints<'z>(
    zeros: impl IntoIterator<Item = &'z Zero>,
    read: impl Fn(&LintedBinder) -> bool,
    anchors: &curios_core::Anchors,
) -> Vec<Lint> {
    let lints = zeros
        .into_iter()
        .filter(|zero| !matches!(zero, Zero::Binder { binder, .. } if read(binder)))
        .map(Lint::of)
        .collect();

    ordered(lints, anchors)
}

/// The `unused-dependency` lints of a package: each dependency `declared` names whose prefix nothing in the package *wrote* (`reached`). Exact, where a language with externals could only guess: a dependency mounts at its name and is reached only by a reference that names it.
///
/// Spelling rather than resolution, which is the same basis the refusal uses — one rule enforced one way. Resolution would report almost nothing unused: a re-export means the path a reference resolves to need not be the path it was written under, so a program naming `/std/Nat` resolves into `/sys` without having spelled it.
pub fn unused_dependencies<'a>(
    declared: impl IntoIterator<Item = &'a String>,
    reached: &BTreeSet<Qualifier>,
) -> Vec<Lint> {
    declared
        .into_iter()
        .filter(|name| !reached.contains(&Qualifier::from([name.as_str()])))
        .map(|name| Lint::unused_dependency(name))
        .collect()
}
