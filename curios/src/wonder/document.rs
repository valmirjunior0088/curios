//! The `document` engine: a unit's interface as a [`Documentation`] record, read off the unit the compilation builds — what `curios document` renders into pages, and what a `wonder document` transport would print. Nothing executes, and the store is read as every query reads it and never written.

use {
    super::ReadOnly,
    crate::Verdicts,
    curios_pipeline::{Cache, CompileError, with_units},
    curios_text::{Documentation, Overlay, RootSource},
};

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
    let units = super::overlaid(units, overlay);

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
