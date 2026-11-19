//! The `--false` query: whether the checkers admit a subject's closing term at `False`, asked through the pipeline's put ([`put_program`], [`put_core_text`]), the one the soundness board asks of every witness, over the units the subject is compiled against.
//!
//! **What an answer rests on**: the checkers, and the units in scope, believed as every build believes them — compiled against the store a build reads, or restored from it. A program is put whole, its declarations and its closing term elaborated as one module, as the board puts a witness; Core text over the prelude alone, as a `.crsc` is compiled.

use {
    crate::{Diagnosis, Overlaid, Subject, of_error, open, overlaid},
    curios_module::{Overlay, SYNTAX},
    curios_pipeline::{Cache, Fold, Progress, Put, put_core_text, put_program},
    curios_unit::Predecessors,
    curios_verdict::Verdicts,
};

/// Why a subject's closing term could not be put at `False`.
#[derive(Debug)]
pub enum Unput {
    /// The subject is a unit rather than a program: it is written as a module, with no closing term to put.
    Unit,
    /// The subject does not lower, or its text does not read, or a unit it stands on is refused: put to no checker, what stopped it.
    Diagnostics(Vec<Diagnosis>),
}

/// What the checkers say of `subject`'s closing term put at `False`, over its units and the prelude compiled through `cache`. `overlay` and `cache` behave as they do for `diagnostics`.
pub fn falsified(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Put, Unput> {
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = reached.as_ref().map(|cache| cache as &dyn Cache);

    match subject.formed(overlay) {
        Subject::Unit { .. } => Err(Unput::Unit),
        Subject::Entry {
            units,
            origin,
            declares,
            package,
        } => {
            let (entrypoint, loader) =
                open(origin, declares, package.as_ref(), overlay).map_err(Unput::Diagnostics)?;
            let units = overlaid(units, overlay);

            Fold::new(budget, &units, through)
                .units(&mut *progress, |prelude, produced| {
                    let scope = prelude.iter().copied().chain(produced).collect::<Vec<_>>();

                    put_program(
                        budget,
                        Predecessors::over(&scope),
                        &SYNTAX,
                        &entrypoint,
                        &loader,
                    )
                })
                .map_err(|error| Unput::Diagnostics(of_error(error)))
        }
        Subject::CoreText { origin } => {
            let (source, _) = origin
                .read(overlay)
                .map_err(|refusal| Unput::Diagnostics(vec![*refusal]))?;

            Fold::new(budget, &[], through)
                .units(&mut *progress, |prelude, _| {
                    put_core_text(budget, Predecessors::over(prelude), &SYNTAX, &source)
                })
                .map_err(|error| Unput::Diagnostics(of_error(error)))?
                .ok_or(Unput::Unit)
        }
    }
}
