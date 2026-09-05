//! The `document` engine: a unit's interface as a [`Documentation`] record, read off the unit the compilation builds — what `curios document` renders into pages, and what a `wonder document` transport would print. Nothing executes, and the store is read as every query reads it and never written. [`archived_documentation`] is the same record read off a unit already archived, which is how a library is documented without compiling it again: a store slot's unit, or the prelude image, which has no package to be compiled from.

use {
    crate::ReadOnly,
    curios_document::Documentation,
    curios_pipeline::{Cache, CompileError, with_units},
    curios_text::{Overlay, RootSource},
    curios_verdicts::{Verdicts, archived_unit},
    std::path::Path,
};

/// The record carried by the unit archived at `path`: a verdict slot under a store, or the prelude image, read through [`archived_unit`], which is what knows a slot's framing. A unit that carries no record — an executable's — is refused by name.
pub fn archived_documentation(path: &Path) -> Result<Documentation, String> {
    archived_unit(path)?
        .text()
        .documentation()
        .cloned()
        .ok_or_else(|| {
            format!(
                "{}: the archived unit carries no interface to document",
                path.display()
            )
        })
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
    let units = crate::overlaid(units, overlay);

    with_units(
        budget,
        &units,
        cache,
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
