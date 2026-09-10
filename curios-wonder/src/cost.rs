//! The `cost` query: what the optimizer did to each declaration the program declares.
//!
//! The first of the three questions [a profile is a fact about the program, not about the machine](../../documentation/roadmap/profiling-spec.md) separates — *which cliff am I on* — and the one that needs no execution. It is read off the compilation the way a lint is read off name resolution: the driver already observes the Cont graph before and after optimization, so counting the declarations each side names is the whole measurement, and no pass is instrumented to produce it.
//!
//! **Nothing here judges.** A row states what became of a declaration and stops; whether an absorbed helper or a threefold specialization is good news is the author's to decide, and a rule that decided it for them would be the heuristic the lint decision spent a paragraph refusing.

use {
    crate::{Diagnostic, Origin, ReadOnly, of_error, open, overlaid},
    curios_cont::{Fate, descendants, fates},
    curios_pipeline::{Cache, Stage, compile_with_units},
    curios_text::{Overlay, RootSource},
    curios_utilities::Qualifier,
    curios_verdicts::Verdicts,
    std::collections::BTreeMap,
};

/// What each of `program`'s declarations cost it, compiled against `units` and the prelude.
///
/// The two observations are the same ones `wonder stage cont` and `wonder stage cont-optm` render, taken together rather than one at a time: the difference between them is the answer, and neither alone is.
///
/// A program that does not compile as far as Cont has no fates to report, so this refuses with what stopped it — unlike a rung, which is an answer as soon as the driver emits it. A failure *after* `cont-optm` cannot happen without the graph already being final, so there is no partial case to admit.
pub fn cost(
    budget: u64,
    units: Vec<RootSource>,
    origin: Origin,
    declares: Option<Vec<Qualifier>>,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Vec<Fate>, Vec<Diagnostic>> {
    let (entrypoint, loader) = open(origin, declares, overlay)?;
    let units = overlaid(units, overlay);
    let read_only = cache.map(|cache| ReadOnly { cache, overlay });
    let cache = read_only.as_ref().map(|cache| cache as &dyn Cache);

    let mut before = BTreeMap::new();
    let mut after = BTreeMap::new();
    let compiled = compile_with_units(
        budget,
        &units,
        &entrypoint,
        &loader,
        cache,
        |stage| match stage {
            Stage::Cont(module) => before = descendants(module),
            Stage::ContOptm(module) => after = descendants(module),
            _ => {}
        },
        |_| {},
    );

    match compiled {
        Ok(_) => Ok(fates(&before, &after)),
        Err(error) => Err(of_error(error)),
    }
}

#[cfg(test)]
mod tests;
