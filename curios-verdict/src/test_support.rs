//! A history: units walked one step at a time over a store of their own, each step a build of every unit as the latest step of each states it, for the soundness board to put a witness that passes through what one walk filed and a later walk read.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

#[cfg(test)]
mod tests;

use {
    crate::Verdicts,
    curios_abi::ForeignStore,
    curios_common::{RootKind, Source, test_support::Temporary},
    curios_core::{Certification, Definitions, Global},
    curios_module::{RootSource, SYNTAX, UnitSource},
    curios_pipeline::{Cache, CompileError, DEFAULT_BUDGET, compile_units, test_support::*},
    curios_prelude::with_prelude,
    curios_surface::{CoreFile, FormScope, ReadError, to_core},
    curios_unit::{Predecessors, Unit},
    std::{
        fs,
        path::{Path, PathBuf},
        sync::Arc,
    },
};

/// What a unit's step is written in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Written {
    /// A program, compiled as a build compiles one.
    Program,
    /// Core text, read where a program is lowered and elaborated ([`compile_read`]).
    CoreText,
}

impl Written {
    /// The file a step written so is filed in.
    fn extension(self) -> &'static str {
        match self {
            Written::Program => "crs",
            Written::CoreText => "crsc",
        }
    }
}

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
    /// Core text read to a module the kernel or erasure refused.
    Unmade(Unmade),
    /// What the history's store held or did not, and what its directory would not take.
    Stored(String),
}

/// Units walked over a store of their own, in a directory removed with the history.
pub struct History {
    directory: Temporary,
    /// Each unit by the mount it claims, in the order its first step came, with what its latest step is written in and the file holding that step's text.
    units: Vec<(String, Written, PathBuf)>,
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
            directory: Temporary::new("history", "store"),
            units: Vec::new(),
        }
    }

    /// The cache over the history's store, which shares nothing this machine offers: a new one for every build, as every command a build is has its own.
    fn verdicts(&self) -> Verdicts {
        Verdicts::alone(self.directory.join("project"))
    }

    /// Step the unit claiming `mount` to `text`, written as `written`, then build: every unit compiled over the ones before it as the store holds them, each through the store, which files what it compiles and offers each unit's earlier step as its baseline.
    pub fn step(&mut self, mount: &str, written: Written, text: &str) -> Result<(), Stopped> {
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

    /// Withhold what the record of the unit claiming `mount` states of `names`, or all of it where they are none, written into the store as the slot's record: every later build mounts the unit as the store now holds it, and every verdict the record still states is one a walk concluded.
    pub fn withhold(&mut self, mount: &str, names: Option<&[Global]>) -> Result<(), Stopped> {
        let stored = |why: String| Stopped {
            mount: mount.to_string(),
            why: Unbuilt::Stored(why),
        };
        let mut held = None;
        self.walked(&self.verdicts(), None, |at, _, unit| {
            if at == mount {
                held = Some(unit.clone());
            }
        })?;
        let held = held.ok_or_else(|| stored(format!("no step filed a unit at `/{mount}`")))?;
        let certification = match names {
            None => Certification::default(),
            Some(names) => {
                let mut revision = held.certification().revised();
                for name in names {
                    revision.dropped(name);
                }
                revision.filed()
            }
        };

        // Filed through a cache that has taken the units before it and not the unit itself, so its record names what it was compiled after, as the filing that made it did.
        let filing = self.verdicts();
        self.walked(&filing, Some(mount), |_, _, _| ())?;
        let (_, written, file) = self
            .units
            .iter()
            .find(|(at, ..)| at == mount)
            .expect("the unit was walked");
        let text = Source::read(file).map_err(|error| stored(error.to_string()))?;
        let root = rooted(mount, *written, file, &text);
        filing.put(
            &UnitSource::mounted(&root),
            &held.with_certification(certification),
        );

        match filing.refused() {
            Some(refused) => Err(stored(refused)),
            None => Ok(()),
        }
    }

    /// Build every unit, and hand `then` the prelude and each unit as the build made it or the store held it: what the term a history closes with is put over.
    pub fn built<R>(&self, then: impl FnOnce(Predecessors<'_>) -> R) -> Result<R, Stopped> {
        let verdicts = self.verdicts();
        let units = self.walked(&verdicts, None, |_, _, _| ())?;

        Ok(with_prelude(|prelude| {
            let scope = prelude.iter().copied().chain(&units).collect::<Vec<_>>();
            then(Predecessors::over(&scope))
        }))
    }

    /// Every unit in the order its first step came, up to the one claiming `until` where it is given, each compiled through `verdicts` over the prelude and the units before it, and handed to `each` with the source it was compiled from as it is made.
    fn walked(
        &self,
        verdicts: &Verdicts,
        until: Option<&str>,
        mut each: impl FnMut(&str, &UnitSource<'_>, &Unit),
    ) -> Result<Vec<Unit>, Stopped> {
        let mut units = Vec::new();
        for (mount, written, file) in &self.units {
            if until == Some(mount.as_str()) {
                break;
            }
            let stopped = |why: Unbuilt| Stopped {
                mount: mount.clone(),
                why,
            };
            let text =
                Source::read(file).map_err(|error| stopped(Unbuilt::Stored(error.to_string())))?;
            let root = rooted(mount, *written, file, &text);
            let source = UnitSource::mounted(&root);
            let unit = with_prelude(|prelude| {
                let scope = prelude.iter().copied().chain(&units).collect::<Vec<_>>();
                let predecessors = Predecessors::over(&scope);
                match written {
                    Written::Program => compile_units(
                        DEFAULT_BUDGET,
                        predecessors,
                        &SYNTAX,
                        &[(UnitSource::mounted(&root), None)],
                        Some(verdicts),
                        |_| {},
                    )
                    .map(|mut compiled| compiled.pop().expect("a unit was compiled"))
                    .map_err(Unbuilt::Compiled),
                    Written::CoreText => read(verdicts, predecessors, &source, &text.text),
                }
            })
            .map_err(stopped)?;
            each(mount, &source, &unit);
            units.push(unit);
        }

        Ok(units)
    }
}

/// The source of the unit claiming `mount`, written as `written` in `file`, whose text is `text`.
fn rooted(mount: &str, written: Written, file: &Path, text: &Arc<Source>) -> RootSource {
    match written {
        Written::Program => RootSource::mounted(
            mount,
            RootKind::Ordinary,
            file,
            RootSource::namespace_of(file),
        ),
        Written::CoreText => RootSource::core_text(mount, file, text),
    }
}

/// The unit `source` is, `text` being the Core text it is written in: the one the store holds for it where its record agrees, and otherwise read over `predecessors`, compiled over the unit the store offers for its earlier text, and filed.
fn read(
    verdicts: &Verdicts,
    predecessors: Predecessors<'_>,
    source: &UnitSource<'_>,
    text: &str,
) -> Result<Unit, Unbuilt> {
    if let Some(unit) = verdicts.get(source) {
        return Ok(unit);
    }

    let file = text
        .parse::<CoreFile>()
        .map_err(|error| Unbuilt::Unparsed(error.format()))?;
    let scope = FormScope::over(SYNTAX, predecessors.cores());
    let (definitions, entry) =
        to_core(&file, Some(&scope)).map_err(|error| Unbuilt::Read(Box::new(error)))?;
    if entry.is_some() {
        return Err(Unbuilt::Unparsed(
            "a unit's Core text closes with no term".to_string(),
        ));
    }
    let foreigns = rows(&definitions);
    let baseline = verdicts.baseline(source, None);
    let unit = compile_read(
        DEFAULT_BUDGET,
        predecessors,
        source.claims(),
        (definitions, foreigns),
        baseline.as_ref(),
    )
    .map_err(Unbuilt::Unmade)?;
    verdicts.put(source, &unit);

    Ok(unit)
}

/// The foreign rows `definitions` declares, in the order it declares them.
fn rows(definitions: &Definitions) -> ForeignStore {
    let scope = FormScope::over(SYNTAX, [definitions]);
    let mut rows = ForeignStore::new();
    for item in definitions.items.iter() {
        for name in item.declared_names() {
            if let Some(row) = scope.rows.get(name) {
                rows.register((**row).clone());
            }
        }
    }

    rows
}
