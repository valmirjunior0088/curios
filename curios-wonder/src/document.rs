//! The `document` engine: a unit's interface as a [`Documentation`] record, read off the unit the compilation builds — what a `wonder document` transport would print. Nothing executes, and the store is read as every query reads it and never written; `curios document` is a build, and reads the same record off a compilation that files what it compiled, in `curios`'s pipeline. [`std_documentation`] is the same record read off the standard library this compiler was built with, which is how `curios document --std` documents it: it has no package a build would compile it from, and the prelude every compilation starts from already carries its record.

use {
    crate::{ReadOnly, overlaid},
    curios_document::Documentation,
    curios_pipeline::{Cache, CompileError, DEFAULT_STEP_BUDGET, Fold},
    curios_text::{Overlay, RootSource},
    curios_utilities::Qualifier,
    curios_verdicts::Verdicts,
};

/// The standard library's record, read off the prelude this compiler was built with — no sources, no store, and nothing compiled.
///
/// **The record of `/std`, named by its prefix rather than taken as the first one found.** Both prelude roots carry one — `/sys` documents itself so that `/std` has something to adopt its intrinsic declarations out of — and the fold puts `/sys` first, so a search for "the" record finds the wrong half.
pub fn std_documentation() -> Result<Documentation, CompileError> {
    let std = Qualifier::from(["std"]);

    // No units, so nothing is compiled and the budget is never spent: the fold is how the prelude is lent above the pipeline.
    Fold::new(DEFAULT_STEP_BUDGET, &[], None).units(
        |_| {},
        |prelude, _| {
            prelude
                .iter()
                .filter_map(|root| root.text().documentation())
                .find(|record| record.prefix == std)
                .cloned()
                .ok_or_else(|| {
                    CompileError::failure("the prelude carries no /std record".to_string())
                })
        },
    )
}

/// The interface of the last of `units` — a package's library, compiled against everything before it — for its consumers. `overlay` and `cache` behave exactly as they do for `diagnostics`: unsaved text wins over the disk, and the store is read but never written.
///
/// The compilation runs to completion first, the kernel included, so a library that does not check is not documented and reports what stopped it exactly as `run` would. The record itself is the one the lowering built and left on the unit, whether the unit was compiled now or reused from the store.
pub fn documentation(
    budget: u64,
    units: Vec<RootSource>,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Documentation, CompileError> {
    let read_only = cache.map(|cache| ReadOnly { cache, overlay });
    let cache = read_only.as_ref().map(|cache| cache as &dyn Cache);
    let units = overlaid(units, overlay);

    Fold::new(budget, &units, cache).units(
        |_| {},
        |_, produced| {
            produced
                .last()
                .and_then(|unit| unit.text().documentation().cloned())
                .ok_or_else(|| {
                    CompileError::failure(
                        "nothing to document: the scope's last unit carries no interface"
                            .to_string(),
                    )
                })
        },
    )
}
