use {
    crate::{cont, core, ersd, text, wasm},
    std::time::Duration,
};

pub enum Stage<'a> {
    Text(&'a text::Entrypoint),
    Core(&'a core::Term),
    Ersd(&'a ersd::Term),
    Cont(&'a cont::Module),
    Wasm(&'a wasm::Module),
}

pub fn compile_entrypoint<O>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    store: &dyn text::Store,
    mut observe: O,
) -> Result<wasm::Module, String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    let core_term = text::to_core(entrypoint, store).map_err(|error| error.format())?;

    observe(Stage::Core(&core_term));

    let core_type = match text::type_to_core(entrypoint, store).map_err(|error| error.format())? {
        Some(type_) => type_,
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

#[cfg(test)]
mod tests {
    use {super::*, std::time::Duration};

    #[test]
    fn entrypoint_type_is_used_as_expected_type() {
        let entrypoint = "0"
            .parse::<text::Entrypoint>()
            .unwrap()
            .with_type("/sys/Bln".parse().unwrap())
            .with_prelude();

        let error = compile_entrypoint(
            Duration::from_secs(1),
            &entrypoint,
            &text::EmptyStore,
            |_| {},
        )
        .unwrap_err();

        assert!(error.contains("type mismatch"));
    }
}
