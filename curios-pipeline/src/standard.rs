//! The fold with the fixed prelude in front of it.
//!
//! **The scope-agnostic half is [`compile_entrypoint`](crate::compile_entrypoint), and it stays that way.** It takes a [`Prefix`](curios_unit::Prefix) and cannot tell which unit is `/std`; nothing here changes that, and nothing there calls anything here. What this module adds is the *standard* prefix — the one every product puts in scope — so that the answer to "what does a Curios program get for free" is written once.
//!
//! It used to be written three times. The native product, the browser product and this crate's own test suite each spelled `with_prelude(|prelude| … Prefix::over(from_ref(&prelude)), &SYNTAX, …)` by hand, under the reading that naming the standard library is a product's decision. That reading survives — a product may still hand the fold any prefix it likes — but three callers deciding it identically is a missing function rather than a policy, and the third of them was not a product at all.

use {
    crate::{Cache, CompileError, Stage, compile_entrypoint, compile_units, recheck},
    curios_prelude::{SYNTAX, with_prelude},
    curios_unit::Prefix,
    std::slice::from_ref,
};

/// Compile `entrypoint` against the fixed prelude — the one unit every product path puts in scope.
pub fn compile_with_prelude<O>(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    compile_with_units(budget, &[], entrypoint, loader, None, observe)
}

/// Compile `units` in the order given, then `entrypoint` against all of them and the prelude.
///
/// The order *is* the dependency order — nothing here resolves or sorts one, because deciding a scope is still the caller's job and only the shape of the standard prefix is settled here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what a manifest's declared dependencies replace.
pub fn compile_with_units<O>(
    budget: u64,
    units: &[curios_text::RootSource],
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    cache: Option<&dyn Cache>,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    with_prelude(|prelude| {
        let sources = units
            .iter()
            .map(curios_text::UnitSource::mounted)
            .collect::<Vec<_>>();
        let produced = compile_units(
            budget,
            Prefix::over(from_ref(&prelude)),
            &SYNTAX,
            &sources,
            cache,
        )?;
        let scope = std::iter::once(prelude)
            .chain(produced.iter())
            .collect::<Vec<_>>();

        compile_entrypoint(
            budget,
            Prefix::over(&scope),
            &SYNTAX,
            entrypoint,
            loader,
            observe,
        )
    })
}

/// Lower and type-check `entrypoint` against the fixed prelude, reporting the erasure obligations rather than raising them. See [`typecheck_reporting`](crate::typecheck_reporting).
pub fn typecheck_with_prelude(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
) -> Result<(curios_core::Module, Vec<String>), CompileError> {
    with_prelude(|prelude| {
        crate::typecheck_reporting(
            budget,
            Prefix::over(from_ref(&prelude)),
            &SYNTAX,
            entrypoint,
            loader,
        )
    })
}

/// Put `module` to the independent kernel with the fixed prelude in scope. See [`recheck`].
pub fn recheck_with_prelude(
    module: &curios_core::Module,
    budget: u64,
) -> Vec<curios_cert::Verdict> {
    with_prelude(|prelude| recheck(module, budget, Prefix::over(from_ref(&prelude))))
}
