//! The fold with the fixed prelude in front of it.
//!
//! **The half that knows no particular predecessors is [`compile_entrypoint`], and it stays that way.** It takes a [`Predecessors`] and cannot tell which unit is `/std`; nothing here changes that, and nothing there calls anything here. What this module adds is the *standard* predecessors — the ones every product puts in scope — so that the answer to "what does a Curios program get for free" is written once rather than by the native product, the browser product and this crate's own tests each. A product may still hand the fold any predecessors it likes.

#[cfg(test)]
mod tests;

use {
    crate::{
        Cache, Checked, CompileError, EntryTail, Listeners, Progress, ReadElaborated, ReadLowering,
        ReadRefusal, Stage, TestProgram, Told, check_entrypoint, compile_entrypoint, compile_units,
        declared_test_paths, enter_core_text, lower_from_anf, lower_subject,
    },
    curios_abi::ForeignStore,
    curios_common::{Qualifier, Source},
    curios_document::Documentation,
    curios_module::{PreparedText, RootSource, SYNTAX, UnitSource, document_unit},
    curios_prelude::{root_prefixes, with_prelude},
    curios_unit::{Predecessors, Unit},
    std::sync::Arc,
};

/// Compile `entrypoint` against the fixed prelude — the two units every product path puts in scope, `/sys` then `/std`.
///
/// Reports no progress. Every caller of this one is a test, an embedder or the browser, none of which has a terminal to narrate to; the CLI folds its units through a [`Fold`] instead.
pub fn compile_with_prelude<O>(
    budget: u64,
    entrypoint: &curios_surface::Entrypoint,
    loader: &curios_module::RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    Fold::new(budget, &[], None).compile(entrypoint, loader, observe, |_| {})
}

/// The standard fold: the fixed prelude's roots, then `units` compiled in the order given, each against everything before it, reusing what `cache` holds — what every entry point here puts in front of its subject, and so what each of them is a method of.
///
/// **One value rather than three arguments, because the three are one decision.** Every entry point takes the budget, the units and the cache and means the same thing by them; what varies is the subject compiled on top. Spelled apart, that decision would be restated at every call and push the entry points past the argument count a signature carries.
///
/// **The order *is* the dependency order.** Nothing here resolves or sorts one, because deciding the predecessors is the caller's job and only the shape of the standard ones is settled here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what a manifest's declared dependencies replace.
#[derive(Clone, Copy)]
pub struct Fold<'a> {
    budget: u64,
    units: &'a [RootSource],
    cache: Option<&'a dyn Cache>,
    /// Told the subject's lowering — the last unit's, or the program's on top — as soon as it is made or restored. See [`Fold::reading`].
    read: Option<ReadLowering<'a>>,
    /// Told the subject's declarations as elaboration established them. See [`Fold::elaborating`].
    elaborated: Option<ReadElaborated<'a>>,
    /// Told the subject's findings where it is a unit and its compile is refused. See [`Fold::refusing`].
    refused: Option<ReadRefusal<'a>>,
}

impl<'a> Fold<'a> {
    /// `units`, each compiled under `budget`, reusing what `cache` holds — `None` compiles every one.
    pub fn new(budget: u64, units: &'a [RootSource], cache: Option<&'a dyn Cache>) -> Self {
        Self {
            budget,
            units,
            cache,
            read: None,
            elaborated: None,
            refused: None,
        }
    }

    /// This fold, telling `read` its subject's lowering as soon as it is made or restored, whatever the subject's compile goes on to conclude (`ReadLowering`): the last unit's where nothing is compiled on top of the units, and the program's where one is.
    ///
    /// **One fold answers both questions.** A question about what a file writes is answered from the lowering of the check that compiled it, so a check is one fold, and a subject that is refused answers as one that compiles.
    pub fn reading(self, read: ReadLowering<'a>) -> Self {
        Self {
            read: Some(read),
            ..self
        }
    }

    /// This fold, telling `elaborated` its subject's declarations as elaboration established them, whatever its compile concluded (`ReadElaborated`): the unit's module made or restored, with its certifier's record, or where it is refused the declarations that elaborated.
    ///
    /// **The check that answers a file answers what its elaboration established.** A question about a place reads a declaration of the subject's own off it, its type and, where the subject was judged, its totality, as a unit in scope is read off its own.
    pub fn elaborating(self, elaborated: ReadElaborated<'a>) -> Self {
        Self {
            elaborated: Some(elaborated),
            ..self
        }
    }

    /// This fold, telling `refused` the findings of its subject where the subject is its last unit and that unit's compile is refused (`ReadRefusal`): what a question about a library reports beside its refusal, as a program's findings are reported beside its verdict ([`Checked`]).
    pub fn refusing(self, refused: ReadRefusal<'a>) -> Self {
        Self {
            refused: Some(refused),
            ..self
        }
    }

    /// Compile `entrypoint` on top of the fold, through to the emitted module.
    pub fn compile<O, P>(
        self,
        entrypoint: &curios_surface::Entrypoint,
        loader: &RootSource,
        observe: O,
        progress: P,
    ) -> Result<(curios_wasm::Module, ForeignStore), CompileError>
    where
        O: FnMut(Stage<'_>),
        P: FnMut(Progress<'_>),
    {
        let mut observe = observe;

        self.under_entry(progress, |predecessors, stand_in, progress| {
            compile_entrypoint(
                self.budget,
                predecessors,
                &SYNTAX,
                &entry_source(entrypoint, loader, stand_in),
                self.cache,
                EntryTail::Program,
                Told {
                    observe: &mut observe,
                    read: self.read,
                    elaborated: self.elaborated,
                    progress,
                },
            )
            .map(TestProgram::built)
        })
    }

    /// [`Fold::compile`] with the entry compiled as a test program: the authored tail, or a library's absence of one, replaced by the synthesized `Test/main([...])` over the tests in scope at or under any of `under` ([`EntryTail::Test`]) — which are tests of the one unit marked tested, the entry's own where `loader` is ([`RootSource::tested`]). Everything else is the ordinary pipeline, certifier judgment included, and a selection meeting no test builds no program ([`TestProgram`]).
    pub fn tests<O, P>(
        self,
        entrypoint: &curios_surface::Entrypoint,
        loader: &RootSource,
        under: Vec<Qualifier>,
        observe: O,
        progress: P,
    ) -> Result<TestProgram, CompileError>
    where
        O: FnMut(Stage<'_>),
        P: FnMut(Progress<'_>),
    {
        let mut observe = observe;

        self.under_entry(progress, |predecessors, stand_in, progress| {
            compile_entrypoint(
                self.budget,
                predecessors,
                &SYNTAX,
                &entry_source(entrypoint, loader, stand_in),
                self.cache,
                EntryTail::Test { under },
                Told {
                    observe: &mut observe,
                    read: self.read,
                    elaborated: self.elaborated,
                    progress,
                },
            )
        })
    }

    /// [`Fold::compile`] stopped where the verdicts stop: `entrypoint` lowered, elaborated and judged on top of the fold — see `check_entrypoint`. The same fold, so a unit the cabinet already holds is reused here exactly as a compile reuses it, and one that is not is compiled and judged in full: a question about the entry is answered against the dependencies it would actually be built on.
    pub fn check<P>(
        self,
        entrypoint: &curios_surface::Entrypoint,
        loader: &RootSource,
        tail: EntryTail,
        progress: P,
    ) -> Result<Checked, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.under_entry(progress, |predecessors, stand_in, progress| {
            check_entrypoint(
                self.budget,
                predecessors,
                &SYNTAX,
                &entry_source(entrypoint, loader, stand_in),
                self.cache,
                tail,
                // A check hands no rung on.
                Told {
                    observe: &mut |_: Stage<'_>| {},
                    read: self.read,
                    elaborated: self.elaborated,
                    progress,
                },
            )
        })
    }

    /// The Core text `source` holds checked on top of the fold, as [`Fold::check`] checks a program: read over the units in scope, compiled as a unit — claiming `module` where the text closes with no term — and the term it closes with judged at the program contract and erased (`enter_core_text`). Nothing is taken from the cache or handed to it: a text of Core text is loose, compiled against the prelude alone and filed nowhere.
    pub fn check_core_text<P>(
        self,
        source: &Arc<Source>,
        module: Qualifier,
        progress: P,
    ) -> Result<(), CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.under_entry(progress, |predecessors, _, progress| {
            enter_core_text(
                self.budget,
                predecessors,
                &SYNTAX,
                source,
                module,
                &mut |_| {},
                progress,
            )
            .map(|_| ())
        })
    }

    /// [`Fold::check_core_text`] carried through the back half as [`Fold::compile`] carries a program, every rung from `core-elab` on handed to `observe`: the module the program read from Core text compiles to, or `None` for a text closing with no term, which is a module and no program.
    pub fn compile_core_text<O, P>(
        self,
        source: &Arc<Source>,
        module: Qualifier,
        observe: O,
        progress: P,
    ) -> Result<Option<(curios_wasm::Module, ForeignStore)>, CompileError>
    where
        O: FnMut(Stage<'_>),
        P: FnMut(Progress<'_>),
    {
        let mut observe = observe;

        self.under_entry(progress, |predecessors, _, progress| {
            let entered = enter_core_text(
                self.budget,
                predecessors,
                &SYNTAX,
                source,
                module,
                &mut observe,
                progress,
            )?;

            Ok(entered
                .linked
                .map(|linked| lower_from_anf(linked, None, &mut observe)))
        })
    }

    /// The unit the Core text `source` holds reads to, compiled on top of the fold as [`Fold::check_core_text`] compiles it, handed to `then` with the prelude's roots before it: what a question about what a text declares reads.
    pub fn core_text_unit<P, T>(
        self,
        source: &Arc<Source>,
        module: Qualifier,
        progress: P,
        then: impl FnOnce(&[&Unit], &Unit) -> T,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.under_entry(progress, |predecessors, _, progress| {
            let entered = enter_core_text(
                self.budget,
                predecessors,
                &SYNTAX,
                source,
                module,
                &mut |_| {},
                progress,
            )?;

            Ok(then(predecessors.units(), &entered.unit))
        })
    }

    /// The fold with nothing on top: each unit lowered, elaborated, judged and erased against everything before it and the prelude. What a build of a library runs, whose last unit's verdicts are the answer and whose units before it are its predecessors.
    pub fn check_units<P>(self, progress: P) -> Result<(), CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |_, _, _, _| Ok(()))
    }

    /// The test paths of the last unit, in the order its test program schedules them — what `wonder --tests` answers for a library, whose last unit it marks tested, since a built one holds none. The same fold [`Fold::check_units`] runs, read for its lowered module's tests instead of its verdicts, since its elaborated module stores its items in dependency order; nothing executes.
    pub fn test_paths<P>(self, progress: P) -> Result<Vec<String>, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |_, _, produced, _| {
            Ok(produced
                .last()
                .and_then(Unit::text)
                .map(|text| declared_test_paths(text.core()))
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
        self.scoped(progress, |prelude, _, produced, _| then(prelude, &produced))
    }

    /// The program `entrypoint` is, lowered on top of the fold and no further (`lower_subject`), handed to `then` with the term it closes with and the units in scope, the prelude's roots first: what a question about what a program writes reads, asked of no checker, so a program that would be refused answers.
    pub fn lowered<P, T>(
        self,
        entrypoint: &curios_surface::Entrypoint,
        loader: &RootSource,
        progress: P,
        then: impl FnOnce(&[&Unit], &PreparedText, &curios_core::Entrypoint) -> T,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |prelude, sources, produced, _| {
            let scope = prelude
                .iter()
                .copied()
                .chain(produced.iter())
                .collect::<Vec<_>>();

            lower_subject(
                Predecessors::over(&scope),
                &SYNTAX,
                &entry_source(entrypoint, loader, stand_in_of(sources)),
                None,
                self.cache,
                |text, entry| {
                    let entry = entry.expect("a program's lowering holds the term it closes with");

                    then(&scope, text, entry)
                },
            )
        })
    }

    /// The last unit lowered on top of the ones before it and no further (`lower_subject`), handed to `then` with the units in scope, the prelude's roots first: [`Fold::lowered`] for a library, whose units before the last are compiled as any fold compiles them and whose last is asked of no checker.
    pub fn lowered_unit<P, T>(
        self,
        progress: P,
        then: impl FnOnce(&[&Unit], &PreparedText) -> T,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped_before(
            1,
            Listeners::default(),
            progress,
            |prelude, sources, produced, _| {
                let (source, offered) = sources.last().ok_or_else(|| {
                    CompileError::fault("there is nothing to read: the scope holds no unit")
                })?;
                let scope = prelude
                    .iter()
                    .copied()
                    .chain(produced.iter())
                    .collect::<Vec<_>>();

                lower_subject(
                    Predecessors::over(&scope),
                    &SYNTAX,
                    source,
                    *offered,
                    self.cache,
                    |text, _| then(&scope, text),
                )
            },
        )
    }

    /// The interface of the last unit — a package's library, compiled against everything before it — for its consumers. The same fold [`Fold::check_units`] runs, the certifier included, so a library that does not check is not documented and reports what stopped it as `run` would. The record is then made from the unit's stored tables and its sources ([`document_unit`]), whether the unit was compiled now or reused from the cabinet: no unit carries one.
    ///
    /// **The prelude's records are the ones handed over.** A unit adopts a root no consumer can name, and the prelude holds the only one: `/sys`, which a package named `std` adopts as the archived `/std` does.
    pub fn documentation<P>(self, progress: P) -> Result<Documentation, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped(progress, |roots, sources, produced, _| {
            let undocumented = || {
                CompileError::fault(
                    "there is nothing to document: the scope's last unit carries no interface",
                )
            };
            let ((source, _), (unit, earlier)) = sources
                .last()
                .zip(produced.split_last())
                .ok_or_else(undocumented)?;
            let predecessors = roots
                .iter()
                .copied()
                .chain(earlier)
                .filter_map(Unit::text)
                .collect::<Vec<_>>();
            // Of the roots in scope alone: a root this fold withheld is the unit being documented.
            let records = prelude_documentation();
            let records = records
                .iter()
                .filter(|record| {
                    roots
                        .iter()
                        .flat_map(|root| root.mounts())
                        .any(|mount| mount.prefix == record.prefix)
                })
                .collect::<Vec<_>>();

            // A unit read from Core text holds no lowering, which is what a unit is documented from.
            let Some(written) = unit.text() else {
                return Err(undocumented());
            };

            // The unit's checked module and the certifier's record of it say which of its declarations are proofs, and which are proven total.
            document_unit(
                source,
                written,
                &predecessors,
                &records,
                unit.core(),
                unit.certification(),
            )
            .map_err(|error| CompileError::Failure(vec![error.report()]))?
            .ok_or_else(undocumented)
        })
    }

    /// The fold with an entry on top: the prelude, the units, then `entry` over all of them, handed the fold's progress to announce itself with and followed by the event that says it finished. The entry's declarations are a unit as the ones before it are, taken from the cache or handed to it by the step itself, which knows the term that closes the program beside them.
    fn under_entry<P, E, T>(self, progress: P, entry: E) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
        E: FnOnce(
            Predecessors<'_>,
            Option<&[Qualifier]>,
            &mut dyn FnMut(Progress<'_>),
        ) -> Result<T, CompileError>,
    {
        // The subject is the program on top, whose step tells the reader itself: no unit beneath it is read.
        self.scoped_before(
            0,
            Listeners::default(),
            progress,
            |prelude, sources, produced, progress| {
                let predecessors = prelude
                    .iter()
                    .copied()
                    .chain(produced.iter())
                    .collect::<Vec<_>>();

                // The entry step announces itself, since what its announcement says, whether its declarations are held, is known first there; that it finished is said here, where the fold's last step ends.
                let compiled = entry(
                    Predecessors::over(&predecessors),
                    stand_in_of(sources),
                    progress,
                )?;
                progress(Progress::Compiled);

                Ok(compiled)
            },
        )
    }

    /// The standard predecessors, every unit compiled — see [`Fold::scoped_before`].
    fn scoped<P, T>(
        self,
        progress: P,
        then: impl FnOnce(
            &[&Unit],
            &[(UnitSource<'_>, Option<&Unit>)],
            Vec<Unit>,
            &mut P,
        ) -> Result<T, CompileError>,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        self.scoped_before(
            0,
            Listeners {
                read: self.read,
                elaborated: self.elaborated,
                refused: self.refused,
            },
            progress,
            then,
        )
    }

    /// The standard predecessors, assembled once: the fixed prelude's roots, then the units compiled in order against them — what every method here compiles against — handed to `then` as the prelude, each unit's source as it is compiled, the units produced, and the progress reporter for whatever follows. The one spelling of them this type exists to write once; the methods differ only in what they do with it. The last `left` units are given their sources and not compiled, for a method that does something else with its subject, and `listeners` are told of the last unit compiled: its lowering, and its findings where it is refused.
    fn scoped_before<P, T>(
        self,
        left: usize,
        listeners: Listeners<'a>,
        mut progress: P,
        then: impl FnOnce(
            &[&Unit],
            &[(UnitSource<'_>, Option<&Unit>)],
            Vec<Unit>,
            &mut P,
        ) -> Result<T, CompileError>,
    ) -> Result<T, CompileError>
    where
        P: FnMut(Progress<'_>),
    {
        let Self {
            budget,
            units,
            cache,
            read: _,
            elaborated: _,
            refused: _,
        } = self;

        with_prelude(|prelude| {
            // The prelude's roots are in scope unconditionally, with one exception: a package named `std` is the standard library, by the meaning of the name. It takes the archived root's place — the root is withheld, the package is compiled over the archived unit as its baseline and sees what the root could see — and every unit after it is compiled against it, addressed in the cabinet after it. Any other unit claiming a prefix a root mounts collides with it and is refused, exactly as two source units claiming one prefix are. What this is not is a way to swap standard libraries under a dependency: a dependency means what it means against the `/std` it was compiled after, which is why the name is reserved rather than the scope made a parameter.
            let withheld =
                stood_in_for(&root_prefixes(), units).map(|index| (index, prelude[index]));
            let roots = prelude
                .iter()
                .enumerate()
                .filter(|(index, _)| withheld.is_none_or(|(root, _)| *index != root))
                .map(|(_, root)| *root)
                .collect::<Vec<_>>();

            // The unit taking a root's place sees what the root could see, and is offered the root's archived unit as its baseline — which the cache takes or declines — read for that unit's packages, so the texts a lowering over it keeps lie in the package's own files.
            let offered = withheld.map(|(_, root)| root.clone().read_for(units[0].packages()));
            let sources = fold_sources(units)
                .into_iter()
                .enumerate()
                .map(|(index, source)| (source, offered.as_ref().filter(|_| index == 0)))
                .collect::<Vec<_>>();
            let produced = compile_units(
                budget,
                Predecessors::over(&roots),
                &SYNTAX,
                &sources[..sources.len().saturating_sub(left)],
                cache,
                &mut progress,
                listeners,
            )?;

            then(&roots, &sources, produced, &mut progress)
        })
    }
}

/// The prefixes of the archived root the first of a fold's `sources` stands in for, where it does: what every unit of the fold, its entry among them, is compiled over.
fn stand_in_of<'s>(sources: &'s [(UnitSource<'_>, Option<&Unit>)]) -> Option<&'s [Qualifier]> {
    sources.first().and_then(|(source, _)| source.stand_in())
}

/// The entry's source, over the stand-in the fold's first unit is where there is one.
fn entry_source<'a>(
    entrypoint: &'a curios_surface::Entrypoint,
    loader: &'a RootSource,
    stand_in: Option<&[Qualifier]>,
) -> UnitSource<'a> {
    let source = UnitSource::entry(entrypoint, loader);
    match stand_in {
        Some(root) => source.over_stand_in(root.to_vec()),
        None => source,
    }
}

/// The source each of `units` is compiled from in a fold: over the archived root the first takes the place of, where it takes one ([`stood_in_for`]), every unit of the fold, and that first unit seeing what the root could see ([`granted_in_place`]).
///
/// **Stated once, so a unit has one address.** A unit's slot is a function of its source, so the fold compiling and filing a unit, a payload's probe naming the units it was made from and the payload's filing all read their sources here, and each names a unit by the slot the others do. It reads the roots' prefixes alone and restores none of the prelude, so a probe that hits pays nothing for it.
pub fn fold_sources(units: &[RootSource]) -> Vec<UnitSource<'_>> {
    let roots = root_prefixes();
    let withheld = stood_in_for(&roots, units);

    units
        .iter()
        .enumerate()
        .map(|(index, unit)| {
            let source = UnitSource::mounted(unit);
            match withheld {
                Some(root) if index == 0 => source
                    .over_stand_in(vec![roots[root]])
                    .seeing(granted_in_place(&roots, root, unit)),
                Some(root) => source.over_stand_in(vec![roots[root]]),
                None => source,
            }
        })
        .collect()
}

/// Which of the archived roots, claiming `roots` in the fold's order, the first of `units` takes the place of: the last, where the first unit claims its prefix.
///
/// The first unit alone, because its predecessors are then exactly the roots before the withheld one — the ones the archived unit was compiled against — and a later unit's would not be; a package named `std` placed later in a fold collides as any other claim does. And the last root alone, because the roots after a withheld one would have been compiled against it: a claim on an earlier root — a package named `sys`, which nothing could name anyway — is left to collide with it as any claim does.
pub fn stood_in_for(roots: &[Qualifier], units: &[RootSource]) -> Option<usize> {
    let claims = units.first()?.mounts();
    let (index, root) = roots.iter().enumerate().next_back()?;

    claims
        .iter()
        .any(|claim| claim.prefix == *root)
        .then_some(index)
}

/// What the unit standing in for the archived root at `withheld`, of the roots claiming `roots`, may name: every root before it, which the archived unit could see, beside whatever the unit declared itself.
///
/// The roots before it are granted whether or not the unit declared them, because the one that matters is closed: `/sys` is in no unit's default set and the standard library reaches it by declaring it, and a package's manifest cannot declare it.
pub fn granted_in_place(roots: &[Qualifier], withheld: usize, unit: &RootSource) -> Vec<Qualifier> {
    let source = UnitSource::mounted(unit);
    let mut prefixes = roots[..withheld].to_vec();
    prefixes.extend(source.declared().unwrap_or(&[]).iter().cloned());

    prefixes
}

/// The documentation record of each of the prelude's roots, in the fold's order — off the image this compiler was built with: no sources, no cabinet, nothing compiled and no unit restored.
pub fn prelude_documentation() -> Vec<Documentation> {
    curios_prelude::documentation()
}
