//! The `tests` query: every test a subject declares, as `{ path }` records — read off `Module::tests` by the compilation that would build the subject, executing nothing. A rung is a constructor the body builds at run time, so a record deliberately does not name one.

use {
    crate::{DeclaredTest, Overlaid, Subject, open, overlaid},
    curios_pipeline::{Cache, CompileError, EntryTail, Fold, declared_test_paths},
    curios_text::Overlay,
    curios_verdicts::Verdicts,
};

/// Every test `subject` declares, in declaration order — a library's own for a unit subject, the entry's own for a program. `overlay` and `cache` behave exactly as they do for `diagnostics`: unsaved text wins over the disk, and the store is read, and filed into where a question may.
pub fn declared_tests(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Vec<DeclaredTest>, CompileError> {
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);

    let paths = match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = overlaid(units, overlay);
            Fold::new(budget, &units, cache).test_paths(|_| {})?
        }
        Subject::Entry {
            units,
            origin,
            declares,
            ..
        } => {
            let (entrypoint, loader) = open(origin, declares, overlay).map_err(|refusal| {
                CompileError::failure(
                    refusal
                        .iter()
                        .map(|diagnostic| diagnostic.render())
                        .collect::<Vec<_>>()
                        .join("\n\n"),
                )
            })?;
            let units = overlaid(units, overlay);
            let program = Fold::new(budget, &units, cache)
                .check(&entrypoint, &loader, EntryTail::Authored, |_| {})?
                .verdict?;

            declared_test_paths(&program.module)
        }
    };

    Ok(paths
        .into_iter()
        .map(|path| DeclaredTest { path })
        .collect())
}
