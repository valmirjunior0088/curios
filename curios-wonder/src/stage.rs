//! The `stage` query: the program's representation at one rung of the pipeline, reprinted, and a library's at the rungs a unit has.
//!
//! The rungs are [`Stage::NAMES`], observed exactly where the driver emits them; the last, `wasm-optm`, is emitted by the native product where Binaryen runs rather than by the driver, so the engine cannot render it and hands the module back for the transport to finish — the one rung `curios` can reach and `curios-js` cannot.

use {
    crate::{Diagnosis, Overlaid, Severity, Subject, of_error, open, overlaid},
    curios_pipeline::{Cache, Fold, Stage},
    curios_text::{Module, Overlay, RootSource},
    curios_unit::Unit,
    curios_verdicts::Verdicts,
    std::path::Path,
};

/// One rung, reprinted and handed on where the driver emitted it.
#[derive(Debug)]
pub struct Rendering {
    pub name: &'static str,
    /// What stopped the compilation *after* this rung was reached, if anything. A rung the driver already emitted is an answer, and a later failure does not unmake it — the transport prints these beside the rendering rather than in place of it.
    pub diagnostics: Vec<Diagnosis>,
}

/// What asking for a rung reached.
pub enum Reached {
    /// The rung, rendered by the driver's own `Display` for it and handed to the asker's `shown`.
    Rendered(Rendering),
    /// The rung past the driver: the emitted module, for the transport that owns Binaryen to optimize and render.
    Wasm(Box<curios_wasm::Module>),
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
) -> Result<Reached, Refusal> {
    let Subject::Entry {
        units,
        origin,
        declares,
        ..
    } = subject.formed(overlay)
    else {
        return Err(Refusal::Unit);
    };
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };

    let (entrypoint, loader) = open(origin, declares, overlay).map_err(Refusal::Diagnostics)?;
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
        |_| {},
    );

    // A rung the driver emitted has been answered, whatever happens downstream of it. Asking for `core` on a program that fails to elaborate is asking what the lowering produced — which is most of why one asks — and discarding it because a later stage refused answers a question nobody put. What the transport's contract refuses is a program that stopped *before* the rung, which is exactly the case where nothing was observed.
    match (reached, compiled) {
        (true, Ok(_)) => Ok(Reached::Rendered(Rendering {
            name,
            diagnostics: Vec::new(),
        })),
        (true, Err(error)) => Ok(Reached::Rendered(Rendering {
            name,
            diagnostics: of_error(error),
        })),
        (false, Ok((module, _foreigns))) => Ok(Reached::Wasm(Box::new(module))),
        (false, Err(error)) => Err(Refusal::Diagnostics(of_error(error))),
    }
}

/// The rung `name` of a library — the last of `units`, compiled against the ones before it and the prelude — asked through `file`, handed to `shown`. A unit has the rungs [`Stage::of_a_unit`] names and no other: every rung from `ersd` on is rooted at a program's entry, and is [`Refusal::Unit`] here.
///
/// **Each rung reprints what the unit holds of it, and none prints an entry.** `text` is the parse of the file asked through, since a unit is several files and the rung's printer reprints one. `core` is the unit's lowered module, which a unit restored from a store holds without its items, so the unit is lowered again in this process with no store. `core-elab` is its elaborated module as it stands, compiled over `cache` as a build compiles it.
pub fn unit_stage(
    budget: u64,
    units: Vec<RootSource>,
    file: &Path,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    name: &str,
    mut shown: impl FnMut(&str),
) -> Result<Rendering, Refusal> {
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
    let rendered = match name {
        "text" => {
            let parsed = match overlay.get(file) {
                Some(text) => Module::overlaid(file, text).map_err(|error| error.report()),
                None => Module::from_path(file).map_err(|error| error.report()),
            };

            parsed.map(|module| module.to_string()).map_err(|report| {
                vec![Diagnosis {
                    severity: Severity::Error,
                    report,
                }]
            })
        }
        "core" => last_unit(budget, &units, None, |unit| unit.text().core().to_string()),
        _ => last_unit(
            budget,
            &units,
            reached.as_ref().map(|cache| cache as &dyn Cache),
            |unit| unit.core().to_string(),
        ),
    };

    match rendered {
        Ok(rendering) => {
            shown(&rendering);

            Ok(Rendering {
                name,
                diagnostics: Vec::new(),
            })
        }
        Err(diagnostics) => Err(Refusal::Diagnostics(diagnostics)),
    }
}

/// What `read` makes of the last of `units` once the fold has compiled it over `cache`, or what stopped the fold.
fn last_unit(
    budget: u64,
    units: &[RootSource],
    cache: Option<&dyn Cache>,
    read: impl FnOnce(&Unit) -> String,
) -> Result<String, Vec<Diagnosis>> {
    Fold::new(budget, units, cache)
        .units(
            |_| {},
            |_, produced| Ok(produced.last().map(read).unwrap_or_default()),
        )
        .map_err(of_error)
}
