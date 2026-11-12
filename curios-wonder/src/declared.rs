//! The `tests` query: every test a subject declares, as `{ path }` records — read off its lowered module's tests (`curios_core::Definitions::tests`) by the compilation that would build the subject, executing nothing. A rung is a constructor the body builds at run time, so a record deliberately does not name one.

use {
    crate::{DeclaredTest, Overlaid, Subject, open, overlaid, tested_last},
    curios_module::{Overlay, PreparedText},
    curios_pipeline::{Cache, CompileError, EntryTail, Fold, Progress, declared_test_paths},
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::cell::RefCell,
};

/// Every test `subject` declares, in the order its test program schedules them — a library's own for a unit subject, the entry's own for a program. `overlay` and `cache` behave exactly as they do for `diagnostics`: unsaved text wins over the disk, and the store is read, and filed into where a question may.
pub fn declared_tests(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Vec<DeclaredTest>, CompileError> {
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);

    let paths = match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = tested_last(overlaid(units, overlay));
            Fold::new(budget, &units, cache).test_paths(&mut *progress)?
        }
        Subject::Entry {
            units,
            origin,
            declares,
            ..
        } => {
            let (entrypoint, loader) = open(origin, declares, overlay).map_err(|refusal| {
                CompileError::Failure(
                    refusal
                        .into_iter()
                        .map(|diagnostic| diagnostic.report)
                        .collect(),
                )
            })?;
            let units = overlaid(units, overlay);
            // Read off the program's lowering, as its test program schedules them: its elaborated module stores its items in dependency order.
            let lowered = RefCell::new(Vec::new());
            let read = |_: &[&Unit], text: &PreparedText, _: Option<&curios_core::Entrypoint>| {
                *lowered.borrow_mut() = declared_test_paths(text.core());
            };
            Fold::new(budget, &units, cache)
                .reading(&read)
                .check(
                    &entrypoint,
                    &loader.tested(),
                    EntryTail::Program,
                    &mut *progress,
                )?
                .verdict?;

            lowered.into_inner()
        }
    };

    Ok(paths
        .into_iter()
        .map(|path| DeclaredTest { path })
        .collect())
}
