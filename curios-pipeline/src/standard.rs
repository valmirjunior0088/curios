//! The fold with the fixed prelude in front of it.
//!
//! **The scope-agnostic half is [`compile_entrypoint`](crate::compile_entrypoint), and it stays that way.** It takes a [`Prefix`](curios_unit::Prefix) and cannot tell which unit is `/std`; nothing here changes that, and nothing there calls anything here. What this module adds is the *standard* prefix — the one every product puts in scope — so that the answer to "what does a Curios program get for free" is written once.
//!
//! It used to be written three times. The native product, the browser product and this crate's own test suite each spelled `with_prelude(|prelude| … Prefix::over(from_ref(&prelude)), &SYNTAX, …)` by hand, under the reading that naming the standard library is a product's decision. That reading survives — a product may still hand the fold any prefix it likes — but three callers deciding it identically is a missing function rather than a policy, and the third of them was not a product at all.

use {
    crate::{
        Cache, CompileError, Progress, Stage, check_entrypoint, compile_entrypoint, compile_units,
        recheck,
    },
    curios_prelude::{SYNTAX, with_prelude},
    curios_unit::Prefix,
    std::slice::from_ref,
};

/// Compile `entrypoint` against the fixed prelude — the one unit every product path puts in scope.
///
/// Reports no progress. Every caller of this one is a test, an embedder or the browser, none of which has a terminal to narrate to; the CLI takes [`compile_with_units`] instead.
pub fn compile_with_prelude<O>(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    compile_with_units(budget, &[], entrypoint, loader, None, observe, |_| {})
}

/// Compile `units` in the order given, then `entrypoint` against all of them and the prelude.
///
/// The order *is* the dependency order — nothing here resolves or sorts one, because deciding a scope is still the caller's job and only the shape of the standard prefix is settled here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what a manifest's declared dependencies replace.
pub fn compile_with_units<O, P>(
    budget: u64,
    units: &[curios_text::RootSource],
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
    cache: Option<&dyn Cache>,
    observe: O,
    progress: P,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
    P: FnMut(Progress<'_>),
{
    fold_with_units(budget, units, cache, progress, |scope| {
        compile_entrypoint(budget, scope, &SYNTAX, entrypoint, loader, observe)
    })
}

/// [`compile_with_units`] stopped where the verdicts stop: `units` in the order given, then `entrypoint` lowered, elaborated and judged against all of them and the prelude — see [`check_entrypoint`]. The same fold, so a unit the store already holds is reused here exactly as a compile reuses it, and one that is not is compiled and judged in full: a question about the entry is answered against the dependencies it would actually be built on.
pub fn check_with_units<P>(
    budget: u64,
    units: &[curios_text::RootSource],
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
    cache: Option<&dyn Cache>,
    progress: P,
) -> Result<curios_core::Module, CompileError>
where
    P: FnMut(Progress<'_>),
{
    fold_with_units(budget, units, cache, progress, |scope| {
        check_entrypoint(budget, scope, &SYNTAX, entrypoint, loader)
    })
}

/// The fold with no entry: `units` in the order given, each lowered, elaborated, judged and erased against everything before it and the prelude, and nothing compiled on top. What a question about a *unit* — a library, which has no entrypoint to check through — is answered by: the last unit's verdicts are the answer, and the ones before it are its scope.
pub fn check_units_with_prelude<P>(
    budget: u64,
    units: &[curios_text::RootSource],
    cache: Option<&dyn Cache>,
    mut progress: P,
) -> Result<(), CompileError>
where
    P: FnMut(Progress<'_>),
{
    with_prelude(|prelude| {
        let sources = units
            .iter()
            .map(curios_text::UnitSource::mounted)
            .collect::<Vec<_>>();
        compile_units(
            budget,
            Prefix::over(from_ref(&prelude)),
            &SYNTAX,
            &sources,
            cache,
            &mut progress,
        )
        .map(|_produced| ())
    })
}

/// The fold both entry points share: the prelude, then `units` in order, then `entry` over the whole scope, bracketed by the progress events the entry step cannot announce for itself.
fn fold_with_units<P, E, T>(
    budget: u64,
    units: &[curios_text::RootSource],
    cache: Option<&dyn Cache>,
    mut progress: P,
    entry: E,
) -> Result<T, CompileError>
where
    P: FnMut(Progress<'_>),
    E: FnOnce(Prefix<'_>) -> Result<T, CompileError>,
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
            &mut progress,
        )?;
        let scope = std::iter::once(prelude)
            .chain(produced.iter())
            .collect::<Vec<_>>();

        // The entry is announced here rather than inside `compile_entrypoint`, which stays free of the concern: it is the last step of this fold, and bracketing it costs one event where threading a second callback down would cost a signature.
        progress(Progress::Entry);
        let compiled = entry(Prefix::over(&scope))?;
        progress(Progress::Compiled);

        Ok(compiled)
    })
}

/// Lower and type-check `entrypoint` against the fixed prelude, reporting the erasure obligations rather than raising them. See [`typecheck_reporting`](crate::typecheck_reporting).
pub fn typecheck_with_prelude(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
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

/// [`typecheck_with_prelude`], reporting what elaboration consumed as well. See [`typecheck_measured`](crate::typecheck_measured).
pub fn typecheck_with_prelude_measured(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
) -> Result<
    (
        curios_core::Module,
        Vec<String>,
        curios_core::Consumption,
        u64,
    ),
    CompileError,
> {
    with_prelude(|prelude| {
        crate::typecheck_measured(
            budget,
            Prefix::over(from_ref(&prelude)),
            &SYNTAX,
            entrypoint,
            loader,
        )
    })
}

/// Put `module` to the independent kernel with the fixed prelude in scope, handing back the walk's own kernel for a measurement to read. See `curios_cert::recheck_module_measured`.
pub fn recheck_with_prelude_measured(
    module: &curios_core::Module,
    budget: u64,
) -> (Vec<curios_cert::Verdict>, curios_cert::Kernel) {
    with_prelude(|prelude| {
        crate::recheck_measured(module, budget, Prefix::over(from_ref(&prelude)), &SYNTAX)
    })
}

/// Put `module` to the independent kernel with the fixed prelude in scope. See [`recheck`].
pub fn recheck_with_prelude(
    module: &curios_core::Module,
    budget: u64,
) -> Vec<curios_cert::Verdict> {
    with_prelude(|prelude| recheck(module, budget, Prefix::over(from_ref(&prelude)), &SYNTAX))
}
