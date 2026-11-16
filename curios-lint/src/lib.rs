//! `unused-dependency`: the one lint no unit can decide alone, decided where a package's units' reached mounts are gathered. Every other lint is decided by the stage that establishes its last fact and stored with the unit it is about: the lowering words `unused-import` and `unused-declaration`, and elaboration the binder lints (`curios-elab`'s `binder_lints`). The decision is `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`.

#[cfg(test)]
mod tests;

use {
    curios_common::Qualifier,
    curios_core::{Lint, LintKind},
    std::collections::BTreeSet,
};

/// The `unused-dependency` lints of a package: each dependency `declared` names whose prefix nothing in the package *wrote* (`reached`), standing in no text, since its row is the package's manifest's. Exact, where a language with externals could only guess: a dependency mounts at its name and is reached only by a reference that names it.
///
/// Spelling rather than resolution, which is the same basis the refusal uses — one rule enforced one way. Resolution would report almost nothing unused: a re-export means the path a reference resolves to need not be the path it was written under, so a program naming `/std/Nat` resolves into `/sys` without having spelled it.
pub fn unused_dependencies<'a>(
    declared: impl IntoIterator<Item = &'a String>,
    reached: &BTreeSet<Qualifier>,
) -> Vec<Lint> {
    declared
        .into_iter()
        .filter(|name| !reached.contains(&Qualifier::from([name.as_str()])))
        .map(|name| {
            Lint::unused(
                LintKind::UnusedDependency,
                None,
                "dependency",
                name,
                "delete its row",
            )
        })
        .collect()
}
