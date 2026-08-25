//! The `stage` query: the program's representation at one rung of the pipeline, reprinted.
//!
//! What `--print` was, as a question rather than a flag. The rungs are [`Stage::NAMES`], observed exactly where the driver emits them; the last, `wasm-optm`, is constructed by the native product after Binaryen rather than by the driver, so the engine cannot render it and hands the module back for the transport to finish — the one rung `curios` can reach and `curios-js` cannot.

use {
    super::{Diagnostic, Origin, ReadOnly, of_error, open, overlaid},
    curios_pipeline::{Cache, Stage, compile_with_units},
    curios_text::{Overlay, RootSource},
};

/// One rung, reprinted.
#[derive(Debug)]
pub struct Rendering {
    pub name: &'static str,
    pub text: String,
}

/// What asking for a rung reached.
pub enum Reached {
    /// The rung, rendered by the driver's own `Display` for it.
    Rendered(Rendering),
    /// The rung past the driver: the emitted module, for the transport that owns Binaryen to optimize and render.
    Wasm(Box<curios_wasm::Module>),
}

/// Why a rung could not be shown.
pub enum Refusal {
    /// No rung of that name — the question could not be asked.
    NoSuchStage { asked: String },
    /// The program did not compile as far as the rung: what stopped it.
    Diagnostics(Vec<Diagnostic>),
}

/// `program`'s representation at the rung `name`, compiled against `units` and the prelude.
pub fn stage(
    budget: u64,
    units: Vec<RootSource>,
    origin: Origin,
    overlay: &Overlay,
    cache: Option<&dyn Cache>,
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
    let (module, _foreigns) = compile_with_units(
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
    )
    .map_err(|error| Refusal::Diagnostics(of_error(error)))?;

    match text {
        Some(text) => Ok(Reached::Rendered(Rendering { name, text })),
        None => Ok(Reached::Wasm(Box::new(module))),
    }
}
