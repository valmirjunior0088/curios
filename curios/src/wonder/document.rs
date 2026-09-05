//! The `document` engine: a unit's interface as a [`Documentation`] record, read off the compilation that builds it — what `curios document` renders into pages, and what a `wonder document` transport would print. Nothing executes, and the store is read as every query reads it and never written.

use {
    super::ReadOnly,
    crate::Verdicts,
    curios_pipeline::{Cache, CompileError, with_units},
    curios_text::{Documentation, Overlay, RootSource, document_unit},
};

/// The interface of the last of `units` — a package's library, compiled against everything before it — for its consumers. `overlay` and `cache` behave exactly as they do for `diagnostics`: unsaved text wins over the disk, and the store is read but never written.
///
/// The compilation runs to completion first, the kernel included, so a library that does not check is not documented and reports what stopped it exactly as `run` would.
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
        |prelude, produced| {
            let (Some(source), Some(unit)) = (units.last(), produced.last()) else {
                return Err(CompileError::failure(
                    "nothing to document: the scope holds no unit".to_string(),
                ));
            };
            // The prelude, then every unit before the last: the scope the last unit's names resolve against.
            let scope = std::iter::once(prelude)
                .chain(&produced[..produced.len() - 1])
                .map(|unit| unit.text())
                .collect::<Vec<_>>();
            let prefix = source
                .mounts()
                .first()
                .map(|mount| mount.prefix.clone())
                .unwrap_or_default();

            document_unit(source, &prefix, &scope, unit.text())
                .map_err(|error| CompileError::Failure(vec![error.report()]))
        },
    )
}
