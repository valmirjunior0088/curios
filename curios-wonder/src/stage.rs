//! The `stage` query: the program's representation at one rung of the pipeline, reprinted, and a library's at the rungs a unit has.
//!
//! The rungs are [`Stage::NAMES`], each observed exactly where the driver emits it.

use {
    crate::{Diagnosis, Overlaid, Severity, Subject, of_error, open, overlaid},
    curios_core::Definitions,
    curios_module::{Overlay, RootSource, SYNTAX, read_module},
    curios_pipeline::{Cache, Fold, Progress, Stage},
    curios_surface::{FormScope, Module, Spellable, Spelling},
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::{path::Path, rc::Rc},
};

/// One rung, reprinted and handed on where the driver emitted it.
#[derive(Debug)]
pub struct Rendering {
    /// What stopped the compilation *after* this rung was reached, if anything. A rung the driver already emitted is an answer, and a later failure does not unmake it — the transport prints these beside the rendering rather than in place of it.
    pub diagnostics: Vec<Diagnosis>,
}

/// Why a rung could not be shown.
#[derive(Debug)]
pub enum Refusal {
    /// The subject is a unit rather than a program: it is written as a module, with no final term to compile a program from, and only a program has rungs.
    Unit,
    /// No rung of that name — the question could not be asked.
    NoSuchStage { asked: String },
    /// The program did not compile *as far as* the rung: what stopped it. A failure past the rung is not a refusal — see [`Rendering::diagnostics`].
    Diagnostics(Vec<Diagnosis>),
}

/// `subject`'s representation at the rung `name`, compiled against its units and the prelude, handed to `shown` where the driver emits it: nothing a later rung does unmakes a rung already handed on, a panic on an invariant of its own among it.
pub fn stage(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    name: &str,
    mut shown: impl FnMut(&str),
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Rendering, Refusal> {
    let Subject::Entry {
        units,
        origin,
        declares,
        package,
    } = subject.formed(overlay)
    else {
        return Err(Refusal::Unit);
    };
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };

    let (entrypoint, loader) =
        open(origin, declares, package.as_ref(), overlay).map_err(Refusal::Diagnostics)?;
    let units = overlaid(units, overlay);
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);

    let mut reached = false;
    let compiled = Fold::new(budget, &units, cache).compile(
        &entrypoint,
        &loader,
        |stage| {
            if stage.name() == name {
                shown(&stage.to_string());
                reached = true;
            }
        },
        &mut *progress,
    );

    // A rung the driver emitted has been answered, whatever happens downstream of it. Asking for `core` on a program that fails to elaborate is asking what the lowering produced — which is most of why one asks — and discarding it because a later stage refused answers a question nobody put. What the transport's contract refuses is a program that stopped *before* the rung, which is exactly the case where nothing was observed.
    match (reached, compiled) {
        (true, Ok(_)) => Ok(Rendering {
            diagnostics: Vec::new(),
        }),
        (true, Err(error)) => Ok(Rendering {
            diagnostics: of_error(error),
        }),
        (false, Ok(_)) => unreachable!("a compile that ends emits every rung"),
        (false, Err(error)) => Err(Refusal::Diagnostics(of_error(error))),
    }
}

/// The rung `name` of a library — the last of `units`, compiled against the ones before it and the prelude — asked through `file`, as its rendering. A unit has the rungs [`Stage::of_a_unit`] names and no other: every rung from `anf` on is rooted at a program's entry, and is [`Refusal::Unit`] here.
///
/// **Each rung reprints what the unit holds of it, and none prints an entry.** `text` is the parse of the file asked through, since a unit is several files and the rung's printer reprints one. `core` is the unit's lowering, made over the units before it as a question about a place makes one, and the unit checked nowhere (`Fold::lowered_unit`). `core-elab` is its elaborated module as it stands, compiled over `cache` as a build compiles it.
pub fn unit_stage(
    budget: u64,
    units: Vec<RootSource>,
    file: &Path,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    name: &str,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<String, Refusal> {
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };
    if !Stage::of_a_unit(name) {
        return Err(Refusal::Unit);
    }

    let units = overlaid(units, overlay);
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = reached.as_ref().map(|cache| cache as &dyn Cache);
    let rendered = match name {
        "text" => {
            let parsed = match overlay.get(file) {
                Some(text) => Module::overlaid(file, text).map_err(|error| error.report()),
                None => read_module(file).map_err(|error| error.report()),
            };

            parsed.map(|module| module.to_string()).map_err(|report| {
                vec![Diagnosis {
                    severity: Severity::Error,
                    report,
                }]
            })
        }
        "core" => Fold::new(budget, &units, through)
            .lowered_unit(&mut *progress, |scope, lowered| {
                lowered
                    .core()
                    .spelled(&dumped_over(scope.iter().map(|unit| unit.core())))
                    .to_string()
            })
            .map_err(of_error),
        _ => last_unit(budget, &units, through, progress, |unit, spelling| {
            unit.core().spelled(spelling).to_string()
        }),
    };

    rendered.map_err(Refusal::Diagnostics)
}

/// What `read` makes of the last of `units` once the fold has compiled it over `cache`, under the spelling a dump of its Core is written in — a test's and a foreign row's forms made against the units before it, the prelude's roots first — or what stopped the fold.
fn last_unit(
    budget: u64,
    units: &[RootSource],
    cache: Option<&dyn Cache>,
    progress: &mut dyn FnMut(Progress<'_>),
    read: impl FnOnce(&Unit, &Rc<Spelling>) -> String,
) -> Result<String, Vec<Diagnosis>> {
    Fold::new(budget, units, cache)
        .units(&mut *progress, |prelude, produced| {
            Ok(produced
                .split_last()
                .map(|(last, before)| {
                    let before = prelude
                        .iter()
                        .map(|unit| unit.core())
                        .chain(before.iter().map(Unit::core));

                    read(last, &dumped_over(before))
                })
                .unwrap_or_default())
        })
        .map_err(of_error)
}

/// The spelling a dump of a unit's Core is written in: Core text proper, a test's and a foreign row's forms made against `before`, the elaborated modules of the units before it, the prelude's roots first.
fn dumped_over<'a>(before: impl IntoIterator<Item = &'a Definitions>) -> Rc<Spelling> {
    let scope = FormScope::over(SYNTAX, before);

    Rc::new(
        Spelling::core_text()
            .with_form_scope(Rc::new(scope))
            .written_as_text(),
    )
}
