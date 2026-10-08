//! The `document` engine: a unit's interface as a [`Documentation`] record, made from the unit the compilation builds — what a `wonder document` transport would print. Nothing executes, and the store is reached as every query reaches it; `curios document` is a build, which makes the same record from its own compilation, in `curios`'s pipeline, and writes the pages. [`std_documentation`] is the record of the standard library this compiler was built with, which is how `curios document --std` documents it: it has no package a build would compile it from, and the build that lowered the prelude filed its records beside it.

use {
    crate::{Overlaid, overlaid},
    curios_document::Documentation,
    curios_pipeline::{Cache, CompileError, Fold, prelude_documentation},
    curios_text::{Overlay, RootSource},
    curios_utilities::Qualifier,
    curios_verdicts::Verdicts,
};

/// The standard library's record, read off the image this compiler was built with — no sources, no store, nothing compiled and no unit restored.
///
/// **The record of `/std`, named by its prefix rather than taken as the first one found.** Both prelude roots have one — `/sys` documents itself so that `/std` has something to adopt its intrinsic declarations out of — and the fold puts `/sys` first, so a search for "the" record finds the wrong half.
pub fn std_documentation() -> Result<Documentation, CompileError> {
    let std = Qualifier::from(["std"]);

    prelude_documentation()
        .into_iter()
        .find(|record| record.prefix == std)
        .ok_or_else(|| CompileError::failure("the prelude carries no /std record".to_string()))
}

/// The interface of the last of `units` — a package's library, compiled against everything before it — for its consumers. `overlay` and `cache` behave exactly as they do for `diagnostics`: unsaved text wins over the disk, and the store is read, and filed into where a question may.
///
/// The compilation runs to completion first, the kernel included, so a library that does not check is not documented and reports what stopped it exactly as `run` would. The record itself is made after it, from the unit's stored tables and its sources, whether the unit was compiled now or reused from the store.
pub fn documentation(
    budget: u64,
    units: Vec<RootSource>,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Documentation, CompileError> {
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);
    let units = overlaid(units, overlay);

    Fold::new(budget, &units, cache).documentation(|_| {})
}
