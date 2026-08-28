//! The `stage` query: the program's representation at one rung of the pipeline, reprinted.
//!
//! What `--print` was, as a question rather than a flag. The rungs are [`Stage::NAMES`], observed exactly where the driver emits them; the last, `wasm-optm`, is constructed by the native product after Binaryen rather than by the driver, so the engine cannot render it and hands the module back for the transport to finish — the one rung `curios` can reach and `curios-js` cannot.

use {
    super::{Diagnostic, Origin, ReadOnly, of_error, open, overlaid},
    crate::Verdicts,
    curios_pipeline::{Cache, Stage, compile_with_units},
    curios_text::{Overlay, RootSource},
};

/// One rung, reprinted.
#[derive(Debug)]
pub struct Rendering {
    pub name: &'static str,
    pub text: String,
    /// What stopped the compilation *after* this rung was reached, if anything. A rung the driver already emitted is an answer, and a later failure does not unmake it — the transport prints these beside the rendering rather than in place of it.
    pub diagnostics: Vec<Diagnostic>,
}

/// What asking for a rung reached.
pub enum Reached {
    /// The rung, rendered by the driver's own `Display` for it.
    Rendered(Rendering),
    /// The rung past the driver: the emitted module, for the transport that owns Binaryen to optimize and render.
    Wasm(Box<curios_wasm::Module>),
}

/// Why a rung could not be shown.
#[derive(Debug)]
pub enum Refusal {
    /// No rung of that name — the question could not be asked.
    NoSuchStage { asked: String },
    /// The program did not compile *as far as* the rung: what stopped it. A failure past the rung is not a refusal — see [`Rendering::diagnostics`].
    Diagnostics(Vec<Diagnostic>),
}

/// `program`'s representation at the rung `name`, compiled against `units` and the prelude.
pub fn stage(
    budget: u64,
    units: Vec<RootSource>,
    origin: Origin,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    name: &str,
) -> Result<Reached, Refusal> {
    let Some(name) = Stage::NAMES.into_iter().find(|known| *known == name) else {
        return Err(Refusal::NoSuchStage {
            asked: name.to_string(),
        });
    };

    let (entrypoint, loader) = open(origin, overlay).map_err(Refusal::Diagnostics)?;
    let units = overlaid(units, overlay);
    let read_only = cache.map(|cache| ReadOnly { cache, overlay });
    let cache = read_only.as_ref().map(|cache| cache as &dyn Cache);

    let mut text = None;
    let compiled = compile_with_units(
        budget,
        &units,
        &entrypoint,
        &loader,
        cache,
        |stage| {
            if stage.name() == name {
                text = Some(stage.to_string());
            }
        },
        |_| {},
    );

    // A rung the driver emitted has been answered, whatever happens downstream of it. Asking for `core` on a program that fails to elaborate is asking what the lowering produced — which is most of why one asks — and discarding it because a later stage refused answers a question nobody put. What the transport's contract refuses is a program that stopped *before* the rung, which is exactly the case where nothing was observed.
    match (text, compiled) {
        (Some(text), Ok(_)) => Ok(Reached::Rendered(Rendering {
            name,
            text,
            diagnostics: Vec::new(),
        })),
        (Some(text), Err(error)) => Ok(Reached::Rendered(Rendering {
            name,
            text,
            diagnostics: of_error(error),
        })),
        (None, Ok((module, _foreigns))) => Ok(Reached::Wasm(Box::new(module))),
        (None, Err(error)) => Err(Refusal::Diagnostics(of_error(error))),
    }
}
