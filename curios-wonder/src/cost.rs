//! The `cost` query: what the optimizer did to each declaration the program declares.
//!
//! It answers *which cliff am I on* — whether an optimization fired — and needs no execution. It is read off the compilation the way a lint is read off name resolution: the driver already observes the Cont graph before and after optimization, so counting the declarations each side names is the whole measurement, and no pass is instrumented to produce it.
//!
//! **Nothing here judges.** A row states what became of a declaration and stops; whether an absorbed helper or a threefold specialization is good news is the author's to decide, and a rule that decided it for them would be the heuristic the lint decision spent a paragraph refusing.

use {
    crate::{Diagnosis, Overlaid, Subject, of_error, open, overlaid},
    curios_cont::{Fate, descendants, fates},
    curios_pipeline::{Cache, Fold, Stage},
    curios_text::Overlay,
    curios_verdicts::Verdicts,
    std::collections::BTreeMap,
};

/// Why a subject has no fates to report.
#[derive(Debug)]
pub enum Uncosted {
    /// The subject is a unit rather than a program: it is written as a module, with no final term to compile a program from, and the optimizer's fates are a program's.
    Unit,
    /// The program did not compile as far as Cont: what stopped it.
    Diagnostics(Vec<Diagnosis>),
}

/// What each of `subject`'s declarations cost it, compiled against its units and the prelude.
///
/// The two observations are the same ones `wonder stage cont` and `wonder stage cont-optm` render, taken together rather than one at a time: the difference between them is the answer, and neither alone is.
///
/// A program that does not compile as far as Cont has no fates to report, so this refuses with what stopped it — unlike a rung, which is an answer as soon as the driver emits it. A failure *after* `cont-optm` cannot happen without the graph already being final, so there is no partial case to admit.
pub fn cost(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Vec<Fate>, Uncosted> {
    let Subject::Entry {
        units,
        origin,
        declares,
        ..
    } = subject.formed(overlay)
    else {
        return Err(Uncosted::Unit);
    };
    let (entrypoint, loader) = open(origin, declares, overlay).map_err(Uncosted::Diagnostics)?;
    let units = overlaid(units, overlay);
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);

    let mut before = BTreeMap::new();
    let mut after = BTreeMap::new();
    let compiled = Fold::new(budget, &units, cache).compile(
        &entrypoint,
        &loader,
        |stage| match stage {
            Stage::Cont(module) => before = descendants(module),
            Stage::ContOptm(module) => after = descendants(module),
            _ => {}
        },
        |_| {},
    );

    match compiled {
        Ok(_) => Ok(fates(&before, &after)),
        Err(error) => Err(Uncosted::Diagnostics(of_error(error))),
    }
}

#[cfg(test)]
mod tests;
