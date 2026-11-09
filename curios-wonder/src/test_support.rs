//! What `curios`'s suites read of the engine that no transport reads: a library's documentation record made the way a question compiles.

use {
    crate::*,
    curios_document::Documentation,
    curios_module::{Overlay, RootSource},
    curios_pipeline::{Cache, CompileError, Fold},
    curios_verdict::Verdicts,
};

/// The interface of the last of `units` — a package's library, compiled against everything before it — for its consumers. `overlay` and `cache` behave exactly as they do for [`diagnostics`](diagnostics()): unsaved text wins over the disk, and the store is read, and filed into where a question may.
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
