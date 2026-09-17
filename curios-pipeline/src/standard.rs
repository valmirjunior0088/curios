//! The fold with the fixed prelude in front of it.
//!
//! **The scope-agnostic half is [`compile_entrypoint`], and it stays that way.** It takes a [`Prefix`] and cannot tell which unit is `/std`; nothing here changes that, and nothing there calls anything here. What this module adds is the *standard* prefix — the one every product puts in scope — so that the answer to "what does a Curios program get for free" is written once.
//!
//! It used to be written three times. The native product, the browser product and this crate's own test suite each spelled `with_prelude(|prelude| … Prefix::over(prelude), &SYNTAX, …)` by hand, under the reading that naming the standard library is a product's decision. That reading survives — a product may still hand the fold any prefix it likes — but three callers deciding it identically is a missing function rather than a policy, and the third of them was not a product at all.

#[cfg(test)]
mod tests;

use {
    crate::{
        Cache, Checked, CompileError, EntryTail, Progress, Stage, TestRecord, check_entrypoint,
        compile_entrypoint, compile_unit_as_tests, compile_units, declared_test_paths, recheck,
    },
    curios_prelude::{SYNTAX, with_prelude, with_stored},
    curios_text::{RootSource, UnitSource},
    curios_unit::{Prefix, Stored, Unit},
    curios_utilities::Qualifier,
};

/// Compile `entrypoint` against the fixed prelude — the two units every product path puts in scope, `/sys` then `/std`.
///
/// Reports no progress. Every caller of this one is a test, an embedder or the browser, none of which has a terminal to narrate to; the CLI folds its units through a [`Fold`] instead.
pub fn compile_with_prelude<O>(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    Fold::new(budget, &[], None).compile(entrypoint, loader, observe, |_| {})
}

/// The standard fold: the fixed prelude's roots, then `units` compiled in the order given, each against everything before it, reusing what `cache` holds — what every entry point here puts in front of its subject, and so what each of them is a method of.
///
/// **One value rather than three arguments, because the three are one decision.** Every entry point took the budget, the units and the cache and meant the same thing by them; what varied was the subject compiled on top. Spelled apart, that decision was restated at every call, and it pushed the entry points past the argument count a signature carries — the test build to eight, behind an allowance.
///
/// **The order *is* the dependency order.** Nothing here resolves or sorts one, because deciding a scope is still the caller's job and only the shape of the standard prefix is settled here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what a manifest's declared dependencies replace.
#[derive(Clone, Copy)]
pub struct Fold<'a> {
    budget: u64,
    units: &'a [RootSource],
    cache: Option<&'a dyn Cache>,
}

impl<'a> Fold<'a> {
    /// `units`, each compiled under `budget`, reusing what `cache` holds — `None` compiles every one.
    pub fn new(budget: u64, units: &'a [RootSource], cache: Option<&'a dyn Cache>) -> Self {
        Self {
            budget,
            units,
            cache,
        }
    }

    /// Compile `entrypoint` on top of the fold, through to the emitted module.
    pub fn compile<O, P>(
        self,
        entrypoint: &curios_text::Entrypoint,
        loader: &RootSource,
        observe: O,
        progress: P,
    ) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), CompileError>
    where
        O: FnMut(Stage<'_>),
        P: FnMut(Progress<'_>),
    {
        self.under_entry(progress, |scope| {
            compile_entrypoint(self.budget, scope, &SYNTAX, entrypoint, loader, observe)
        })
    }

    /// [`Fold::compile`] with the entry compiled as a test program — the synthesized `Test/main([...])` tail over the registered tests `tail` selects, in place of the authored one; see [`compile_unit_as_tests`].
    pub fn tests<O, P>(
        self,
        entrypoint: &curios_text::Entrypoint,
        loader: &RootSource,
        tail: EntryTail,
        observe: O,
        progress: P,
    ) -> Result<
        (
            curios_wasm::Module,
            curios_abi::ForeignStore,
            Vec<TestRecord>,
        ),
        CompileError,
    >
    where
        O: FnMut(Stage<'_>),
        P: FnMut(Progress<'_>),
    {
        self.under_entry(progress, |scope| {
            compile_unit_as_tests(
                self.budget,
                scope,
                &SYNTAX,
                entrypoint,
                loader,
                tail,
                observe,
            )
        })
    }

    /// [`Fold::compile`] stopped where the verdicts stop: `entrypoint` lowered, elaborated and judged on top of the fold — see [`check_entrypoint`]. The same fold, so a unit the store already holds is reused here exactly as a compile reuses it, and one that is not is compiled and judged in full: a question about the entry is answered against the dependencies it would actually be built on.
    pub fn check<P>(
        self,
        entrypoint: &curios_text::Entrypoint,
        loader: &RootSource,
        tail: EntryTail,
        progress: P,
    ) -> Result<Checked, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.under_entry(progress, |scope| {
            check_entrypoint(self.budget, scope, &SYNTAX, entrypoint, loader, tail)
        })
    }

    /// The fold with nothing on top: each unit lowered, elaborated, judged and erased against everything before it and the prelude. What a build of a library runs, whose last unit's verdicts are the answer and whose units before it are its scope.
    pub fn check_units<P>(self, progress: P) -> Result<(), CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |_, _, _| Ok(()))
    }

    /// The declaration-ordered test paths of the last unit — what `wonder tests` answers for a library. The same fold [`Fold::check_units`] runs, read for its `Module::tests` instead of its verdicts; nothing executes.
    pub fn test_paths<P>(self, progress: P) -> Result<Vec<String>, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |_, produced, _| {
            Ok(produced
                .last()
                .map(|unit| declared_test_paths(unit.core()))
                .unwrap_or_default())
        })
    }

    /// The fold with nothing on top, handed to `then` as the prelude's roots in dependency order and the units produced in order — for a reader of what the fold established, such as a question answering from the last unit's own tables. The same fold [`Fold::check_units`] runs; nothing executes.
    pub fn units<P, T>(
        self,
        progress: P,
        then: impl FnOnce(&[&Unit], &[Unit]) -> Result<T, CompileError>,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |prelude, produced, _| then(prelude, &produced))
    }

    /// The fold with an entry on top: the prelude, the units, then `entry` over the whole scope, bracketed by the progress events the entry step cannot announce for itself.
    fn under_entry<P, E, T>(self, progress: P, entry: E) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
        E: FnOnce(Prefix<'_>) -> Result<T, CompileError>,
    {
        self.scoped(progress, |prelude, produced, progress| {
            let scope = prelude
                .iter()
                .copied()
                .chain(produced.iter())
                .collect::<Vec<_>>();

            // The entry is announced here rather than inside `compile_entrypoint`, which stays free of the concern: it is the last step of this fold, and bracketing it costs one event where threading a second callback down would cost a signature.
            progress(Progress::Entry);
            let compiled = entry(Prefix::over(&scope))?;
            progress(Progress::Compiled);

            Ok(compiled)
        })
    }

    /// The standard scope, assembled once: the fixed prelude's roots, then the units compiled in order against them — what every method here compiles against — handed to `then` as the prelude, the units produced, and the progress reporter for whatever follows. The one spelling of the scope this type exists to write once; the methods differ only in what they do with it.
    fn scoped<P, T>(
        self,
        mut progress: P,
        then: impl FnOnce(&[&Unit], Vec<Unit>, &mut P) -> Result<T, CompileError>,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        let Self {
            budget,
            units,
            cache,
        } = self;

        with_stored(|stored| {
            // The prelude's roots are in scope unconditionally, with one exception: a package named `std` is the standard library, by the meaning of the name. It takes the archived root's place — the root is withheld, the package is compiled over the archived unit as its baseline and sees what the root could see — and every unit after it is compiled against it, addressed in the store after it. Any other unit claiming a prefix a root mounts collides with it and is refused, exactly as two source units claiming one prefix are. What this is not is a way to swap standard libraries under a dependency: a dependency means what it means against the `/std` it was compiled after, which is why the name is reserved rather than the scope made a parameter.
            let withheld = withheld(stored, units);
            let roots = stored
                .iter()
                .enumerate()
                .filter(|(index, _)| withheld.is_none_or(|(root, _)| *index != root))
                .map(|(_, stored)| &stored.unit)
                .collect::<Vec<_>>();

            let sources = units
                .iter()
                .enumerate()
                .map(|(index, unit)| match withheld {
                    Some((root, _)) if index == 0 => {
                        UnitSource::mounted(unit).seeing(granted(stored, root, unit))
                    }
                    _ => UnitSource::mounted(unit),
                })
                .collect::<Vec<_>>();
            let baselined = Baselined {
                cache,
                withheld: withheld.and_then(|(_, root)| {
                    root.unit
                        .mounts()
                        .first()
                        .map(|mount| (mount.prefix.clone(), &root.unit))
                }),
            };
            let produced = compile_units(
                budget,
                Prefix::over(&roots),
                &SYNTAX,
                &sources,
                Some(&baselined),
                &mut progress,
            )?;

            then(&roots, produced, &mut progress)
        })
    }
}

/// The last archived root, when the first of `units` claims a prefix it mounts: the root that unit takes the place of.
///
/// The first unit alone, because its scope is then exactly the roots before the withheld one — the scope the archived unit was compiled in — and a later unit's would not be; a package named `std` placed later in a fold collides as any other claim does. And the last root alone, because the roots after a withheld one would have been compiled against it: a claim on an earlier root — a package named `sys`, which nothing could name anyway — is left to collide with it as any claim does.
fn withheld<'a>(stored: &[&'a Stored], units: &[RootSource]) -> Option<(usize, &'a Stored)> {
    let claims = units.first()?.mounts();
    let (index, root) = stored.iter().enumerate().next_back()?;

    root.unit
        .mounts()
        .iter()
        .any(|mount| claims.iter().any(|claim| claim.prefix == mount.prefix))
        .then_some((index, root))
}

/// What the unit standing in for the archived root at `withheld` may name: every root before it, which the archived unit could see, beside whatever the unit declared itself.
///
/// The roots before it are granted whether or not the unit declared them, because the one that matters is closed: `/sys` is in no unit's default set and the standard library reaches it by declaring it, and a package's manifest cannot declare it.
fn granted(stored: &[&Stored], withheld: usize, unit: &RootSource) -> Vec<Qualifier> {
    let source = UnitSource::mounted(unit);
    let mut prefixes = stored[..withheld]
        .iter()
        .flat_map(|root| root.unit.mounts().iter().map(|mount| mount.prefix.clone()))
        .collect::<Vec<_>>();
    prefixes.extend(source.declared().unwrap_or(&[]).iter().cloned());

    prefixes
}

/// The caller's cache with the withheld root offered as a baseline for the unit claiming its prefix.
///
/// Offered rather than imposed: the cache decides whether the unit is compiled over it, so a question takes the archived unit as a baseline while a build compiles the package whole and files it as any unit — and the fold learns nothing about a prelude either way. Whatever tree the package is, the baseline is correct: an item is reused only where its lowered form matches the archived one and nothing it reaches changed, so a tree far from the archive's is simply a larger closure.
struct Baselined<'a> {
    cache: Option<&'a dyn Cache>,
    withheld: Option<(Qualifier, &'a Unit)>,
}

impl Cache for Baselined<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.cache?.get(source)
    }

    fn baseline(&self, source: &UnitSource<'_>, offered: Option<Unit>) -> Option<Unit> {
        let offered = self
            .withheld
            .as_ref()
            .filter(|(prefix, _)| *prefix == source.prefix())
            .map(|(_, unit)| (*unit).clone())
            .or(offered);

        self.cache?.baseline(source, offered)
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit, followed: bool) {
        if let Some(cache) = self.cache {
            cache.put(source, unit, followed);
        }
    }
}

/// Lower and type-check `entrypoint` against the fixed prelude, reporting the erasure obligations rather than raising them. See [`typecheck_reporting`](crate::typecheck_reporting).
pub fn typecheck_with_prelude(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: &curios_text::RootSource,
) -> Result<(curios_core::Module, Vec<String>), CompileError> {
    with_prelude(|prelude| {
        crate::typecheck_reporting(budget, Prefix::over(prelude), &SYNTAX, entrypoint, loader)
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
        crate::typecheck_measured(budget, Prefix::over(prelude), &SYNTAX, entrypoint, loader)
    })
}

/// Put `module` to the independent kernel with the fixed prelude in scope, handing back the walk's own kernel for a measurement to read. See `curios_cert::recheck_module_measured`.
pub fn recheck_with_prelude_measured(
    module: &curios_core::Zonked<curios_core::Module>,
    budget: u64,
) -> (Vec<curios_cert::Verdict>, curios_cert::Kernel) {
    with_prelude(|prelude| crate::recheck_measured(module, budget, Prefix::over(prelude), &SYNTAX))
}

/// Put `module` to the independent kernel with the fixed prelude in scope. See [`recheck`].
pub fn recheck_with_prelude(
    module: &curios_core::Zonked<curios_core::Module>,
    budget: u64,
) -> Vec<curios_cert::Verdict> {
    with_prelude(|prelude| recheck(module, budget, Prefix::over(prelude), &SYNTAX))
}
