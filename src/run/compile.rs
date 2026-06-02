use {
    crate::{Source, cont, core, ersd, text, wasm},
    std::time::Duration,
};

pub enum Stage<'a> {
    Text(&'a text::Entrypoint),
    Core(&'a core::Term),
    Ersd(&'a ersd::Term),
    Cont(&'a cont::Module),
    Wasm(&'a wasm::Module),
}

pub fn compile<L, O>(
    timeout: Duration,
    loader: &L,
    type_: Option<&str>,
    term: &str,
    mut observe: O,
) -> Result<wasm::Module, String>
where
    L: text::Loader,
    O: FnMut(Stage<'_>),
{
    let term_source = Source::inline(term);

    let text_entrypoint = text::Entrypoint::parse(&term_source)
        .map_err(|error| error.format())?
        .with_prelude();

    observe(Stage::Text(&text_entrypoint));

    let core_term = text::to_core(&text_entrypoint, loader).map_err(|error| error.format())?;

    observe(Stage::Core(&core_term));

    let core_type = match type_ {
        Some(type_source) => {
            let type_source = Source::inline(type_source);

            let type_entrypoint = text::Entrypoint::parse(&type_source)
                .map_err(|error| error.format())?
                .with_prelude();

            text::to_core(&type_entrypoint, loader).map_err(|error| error.format())?
        }
        None => core::infer(&mut core::Context::new(timeout), &core_term)
            .map_err(|error| error.format())?,
    };

    let ersd_term = core::erase(&mut core::Context::new(timeout), &core_term, &core_type)
        .map_err(|error| error.format())?;

    observe(Stage::Ersd(&ersd_term));

    let cont_module = ersd::to_cont(&ersd_term);

    observe(Stage::Cont(&cont_module));

    let wasm_module = cont::to_wasm(&cont_module);

    observe(Stage::Wasm(&wasm_module));

    Ok(wasm_module)
}
