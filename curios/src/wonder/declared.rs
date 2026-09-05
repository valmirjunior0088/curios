//! The `tests` query: every test a subject declares, as `{ path }` records — read off `Module::tests` by the compilation that would build the subject, executing nothing. A rung is a constructor the body builds at run time, so a record deliberately does not name one.

use {
    super::{DeclaredTest, ReadOnly, Subject, open},
    crate::Verdicts,
    curios_pipeline::{
        Cache, CompileError, EntryTail, check_with_units, declared_test_paths, unit_test_paths,
    },
    curios_text::Overlay,
};

/// Every test `subject` declares, in declaration order — a library's own for a unit subject, the entry's own for a program. `overlay` and `cache` behave exactly as they do for `diagnostics`: unsaved text wins over the disk, and the store is read but never written.
pub fn declared_tests(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Vec<DeclaredTest>, CompileError> {
    let read_only = cache.map(|cache| ReadOnly { cache, overlay });
    let cache = read_only.as_ref().map(|cache| cache as &dyn Cache);

    let paths = match subject {
        Subject::Unit { units } => {
            let units = super::overlaid(units, overlay);
            unit_test_paths(budget, &units, cache, |_| {})?
        }
        Subject::Entry { units, origin } => {
            let (entrypoint, loader) = open(origin, overlay).map_err(|refusal| {
                CompileError::failure(
                    refusal
                        .iter()
                        .map(|diagnostic| diagnostic.render())
                        .collect::<Vec<_>>()
                        .join("\n\n"),
                )
            })?;
            let units = super::overlaid(units, overlay);
            let module = check_with_units(
                budget,
                &units,
                &entrypoint,
                &loader,
                cache,
                EntryTail::Authored,
                |_| {},
            )?
            .verdict?;

            declared_test_paths(&module)
        }
    };

    Ok(paths
        .into_iter()
        .map(|path| DeclaredTest { path })
        .collect())
}
