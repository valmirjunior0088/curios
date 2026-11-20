//! A history: units walked one step at a time over a cabinet of their own, each step a build of every unit as the latest step of each states it, for the soundness board to put a witness that passes through what one walk filed and a later walk read.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

#[cfg(test)]
mod believed_tests;
#[cfg(test)]
mod tests;

use {
    crate::Verdicts,
    curios_common::{Source, test_support::Temporary},
    curios_core::{Intrinsic, Term},
    curios_elab::DEFAULT_BUDGET,
    curios_module::{RootSource, SYNTAX, UnitSource},
    curios_package::Cabinet,
    curios_pipeline::{
        Cache, CompileError, Listeners, Unmade, Unread, compile_read, compile_units, fold_sources,
        read_core_text, stood_in_for,
    },
    curios_prelude::{root_prefixes, with_prelude},
    curios_surface::{FormScope, ReadError, WrittenIn},
    curios_unit::{Predecessors, Unit},
    std::{fs, path::PathBuf},
};

/// Why a build stopped: at the unit claiming `mount`, for `why`.
#[derive(Debug)]
pub struct Stopped {
    pub mount: String,
    pub why: Unbuilt,
}

/// Why a unit of a build was not made.
#[derive(Debug)]
pub enum Unbuilt {
    /// A program, refused as a build reports it.
    Compiled(CompileError),
    /// Core text that does not parse, or that closes with a term, which a unit's text does not.
    Unparsed(String),
    /// Core text the reader refused.
    Read(Box<ReadError>),
    /// Core text read to a module the certifier or erasure refused.
    Unmade(Unmade),
    /// What the history's cabinet held or did not, and what its directory would not take.
    Stored(String),
}

/// Units walked over a cabinet of their own, in a directory removed with the history.
pub struct History {
    directory: Temporary,
    /// Each unit by the mount it claims, in the order its first step came, with what its latest step is written in and the file holding that step's text.
    units: Vec<(String, WrittenIn, PathBuf)>,
}

impl Default for History {
    fn default() -> Self {
        Self::new()
    }
}

impl History {
    /// A history with no step yet.
    pub fn new() -> Self {
        Self {
            directory: Temporary::new("history", "cabinet"),
            units: Vec::new(),
        }
    }

    /// The cache over the history's own cabinet, which no build reads: a new one for every build, as every command a build is has its own.
    fn verdicts(&self) -> Verdicts {
        Verdicts::at(Cabinet::of(&self.directory.join("project"), None))
    }

    /// Step the unit claiming `mount` to `text`, written as `written`, then build: every unit compiled over the ones before it as the cabinet holds them, each through the cabinet, which files what it compiles and offers each unit's earlier step as its baseline.
    pub fn step(&mut self, mount: &str, written: WrittenIn, text: &str) -> Result<(), Stopped> {
        let stored = |why: String| Stopped {
            mount: mount.to_string(),
            why: Unbuilt::Stored(why),
        };
        let file = self
            .directory
            .join(format!("{mount}.{}", written.extension()));
        fs::create_dir_all(&self.directory)
            .and_then(|()| fs::write(&file, text))
            .map_err(|error| stored(error.to_string()))?;
        match self.units.iter_mut().find(|(at, ..)| at == mount) {
            Some(unit) => *unit = (mount.to_string(), written, file),
            None => self.units.push((mount.to_string(), written, file)),
        }

        self.built(|_| ())
    }

    /// Mount the unit claiming `mount` in no later build, as a dependency taken out of a manifest is, then build: what each unit after it filed stays in the cabinet, and a unit standing in for an archived root unmounted gives the root its place back.
    pub fn unmounted(&mut self, mount: &str) -> Result<(), Stopped> {
        self.units.retain(|(at, ..)| at != mount);

        self.built(|_| ())
    }

    /// Build every unit, and hand `then` the prelude and each unit as the build made it or the cabinet held it: what the term a history closes with is put over.
    pub fn built<R>(&self, then: impl FnOnce(Predecessors<'_>) -> R) -> Result<R, Stopped> {
        let verdicts = self.verdicts();
        let (units, stand_in) = self.walked(&verdicts)?;

        Ok(with_prelude(|prelude| {
            let scope = roots(prelude, stand_in).chain(&units).collect::<Vec<_>>();
            then(Predecessors::over(&scope))
        }))
    }

    /// Every unit in the order its first step came, each compiled through `verdicts` over the prelude and the units before it, from the source the standard fold compiles it from (`fold_sources`); with the index of the archived root the first unit stands in for, where it takes one, as a package named `std` first in a fold does.
    fn walked(&self, verdicts: &Verdicts) -> Result<(Vec<Unit>, Option<usize>), Stopped> {
        let texts = self
            .units
            .iter()
            .map(|(mount, _, file)| {
                fs::read_to_string(file).map_err(|error| Stopped {
                    mount: mount.clone(),
                    why: Unbuilt::Stored(error.to_string()),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mounted = self
            .units
            .iter()
            .zip(&texts)
            .map(|((mount, _, file), text)| RootSource::one_file(mount, file, text))
            .collect::<Vec<_>>();
        let stand_in = stood_in_for(&root_prefixes(), &mounted);

        let mut units = Vec::new();
        for (position, (((mount, written, _), text), (root, seeing))) in self
            .units
            .iter()
            .zip(&texts)
            .zip(mounted.iter().zip(fold_sources(&mounted)))
            .enumerate()
        {
            let unit = with_prelude(|prelude| {
                let scope = roots(prelude, stand_in).chain(&units).collect::<Vec<_>>();
                let predecessors = Predecessors::over(&scope);
                // The unit taking a root's place is offered its archived unit as a baseline, as the standard fold offers it.
                let offered = stand_in
                    .filter(|_| position == 0)
                    .map(|index| prelude[index].clone().read_for(root.packages()));
                match written {
                    WrittenIn::Program => compile_units(
                        DEFAULT_BUDGET,
                        predecessors,
                        &SYNTAX,
                        &[(seeing, offered.as_ref())],
                        Some(verdicts),
                        |_| {},
                        Listeners::default(),
                    )
                    .map(|mut compiled| compiled.pop().expect("a unit was compiled"))
                    .map_err(Unbuilt::Compiled),
                    WrittenIn::CoreText => read(verdicts, predecessors, &seeing, text),
                }
            })
            .map_err(|why| Stopped {
                mount: mount.clone(),
                why,
            })?;
            units.push(unit);
        }

        Ok((units, stand_in))
    }
}

/// The prelude's roots a history's units are compiled over: every one but the root at `stood_in`, where a unit stands in for it.
fn roots<'u>(prelude: &[&'u Unit], stood_in: Option<usize>) -> impl Iterator<Item = &'u Unit> {
    prelude
        .iter()
        .enumerate()
        .filter(move |(index, _)| Some(*index) != stood_in)
        .map(|(_, root)| *root)
        .collect::<Vec<_>>()
        .into_iter()
}

/// The unit `source` is, `text` being the Core text it is written in: the one the cabinet holds for it where its record agrees, and otherwise read over `predecessors`, compiled over the unit the cabinet offers for its earlier text, and filed.
fn read(
    verdicts: &Verdicts,
    predecessors: Predecessors<'_>,
    source: &UnitSource<'_>,
    text: &str,
) -> Result<Unit, Unbuilt> {
    if let Some(unit) = verdicts.get(source) {
        return Ok(unit);
    }

    // Read through the product's door, a closing term put where a program's is: a unit's text closes with none, which is refused below.
    let scope = FormScope::over(SYNTAX, predecessors.cores());
    let contract = Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit()));
    let read =
        read_core_text(&Source::inline(text), &scope, &contract).map_err(
            |unread| match unread {
                Unread::Parse(report) => Unbuilt::Unparsed(report.render()),
                Unread::Read(error) => Unbuilt::Read(Box::new(error)),
            },
        )?;
    if read.entry.is_some() {
        return Err(Unbuilt::Unparsed(
            "a unit's Core text closes with no term".to_string(),
        ));
    }
    let baseline = verdicts.baseline(source, None);
    let unit = compile_read(
        DEFAULT_BUDGET,
        predecessors,
        &SYNTAX,
        source.claims(),
        &read,
        baseline.as_ref(),
    )
    .map_err(Unbuilt::Unmade)?;
    verdicts.put(source, &unit);

    Ok(unit)
}
