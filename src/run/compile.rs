use {
    crate::{cont, core, ersd, optm, text, wasm},
    std::time::Duration,
};

pub enum Stage<'a> {
    Text(&'a text::Entrypoint),
    Core(&'a core::Term),
    Ersd(&'a ersd::Term),
    Cont(&'a cont::Module),
    Optm(&'a cont::Module),
    Wasm(&'a wasm::Module),
}

pub fn compile_entrypoint<O>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
    mut observe: O,
) -> Result<wasm::Module, String>
where
    O: FnMut(Stage<'_>),
{
    observe(Stage::Text(entrypoint));

    let text::Lowered { term, type_ } =
        text::to_core(entrypoint, &text::prelude(loader)).map_err(|error| error.format())?;

    observe(Stage::Core(&term));

    // Elaborate (checking against the entrypoint's type when it carries one, else
    // synthesizing), then zonk metavariable solutions in so the term is meta-free,
    // then erase the meta-free term to `ersd` — the `elaborate → zonk → erase`
    // data flow (§9). Elaboration and zonking share one context (the solutions
    // live in its `MetaStore`); erase runs over a fresh one.
    let mut context = core::Context::new(timeout);

    let core_mode = match &type_ {
        Some(type_) => core::Mode::Check(type_.clone()),
        None => core::Mode::Infer,
    };

    let (core_term, core_type) =
        core::elaborate(&mut context, &term, core_mode).map_err(|error| error.format())?;

    let elaborated = core::zonk(&context, &core_term).map_err(|error| error.format())?;
    let core_type = core::zonk(&context, &core_type).map_err(|error| error.format())?;

    let ersd_term = core::erase(&mut core::Context::new(timeout), &elaborated, &core_type)
        .map_err(|error| error.format())?;

    observe(Stage::Ersd(&ersd_term));

    let cont_module = ersd::to_cont(&ersd_term);

    observe(Stage::Cont(&cont_module));

    let optm_module = optm::optimize(cont_module);

    observe(Stage::Optm(&optm_module));

    let wasm_module = cont::to_wasm(&optm_module);

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
            .with_type("/sys/Bln".parse().unwrap());

        let error = compile_entrypoint(
            Duration::from_secs(1),
            &entrypoint,
            &text::NullLoader,
            |_| {},
        )
        .unwrap_err();

        assert!(error.contains("type mismatch"));
    }
}
