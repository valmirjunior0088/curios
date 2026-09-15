//! What an invocation is about: every command's argument, resolved once.
//!
//! **A spelling means one thing.** `-` is standard input, an argument ending in `.crs` or holding a path separator is a file, any other argument is an executable's name, and no argument at all is the governing package entire. The rule is lexical and probes no disk — an executable's name is a single identifier, so it can hold neither `.crs` nor a separator nor be `-`, and the spaces cannot overlap — and it is spelled here once, so no two commands can come to read one argument two ways.
//!
//! That is also why `-` is spelled at all, rather than a bare invocation reading standard input when one is piped in. No argument already means the governing package, and deciding between the two would mean asking whether a terminal is attached — which makes one command line mean different things in a shell and in a pipeline.
//!
//! **What is selected is one of three things.** The package entire — its library and every program it declares, which a command that needs one program narrows to the sole or `default` one; a library; or a program. A program is *declared*, with a home in its package's store and the units it is compiled against, or *loose*, with neither: a loose program is compiled against the prelude and nothing else, and the type has no field that could carry more.
//!
//! **A file is still placed two ways.** `run` and `compile` compile one standalone wherever it sits, and a question places it in the unit that declares it (`Membership`). [`Placement`] is that difference, stated by the caller rather than decided here.

#[cfg(test)]
mod tests;

use {
    crate::{EXECUTABLE, Executable, Governing, LIBRARY, Membership, Package, order, reachable},
    curios_text::{RootSource, identity},
    curios_utilities::Qualifier,
    std::path::{Path, PathBuf},
};

/// How an argument is spelled, decided before anything looks at a manifest or the disk.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Spelling {
    /// `-`: the program on standard input.
    Stdin,
    /// A path, by its `.crs` suffix or a separator.
    File(PathBuf),
    /// An executable's name.
    Name(String),
    /// No argument: the governing package entire.
    Nothing,
}

impl Spelling {
    /// How standard input is asked for. Not an identifier, so it cannot collide with an executable's name, and not path-shaped, so it cannot collide with a file's.
    pub const STDIN: &'static str = "-";

    /// The spelling `argument` takes, by its text alone.
    pub fn of(argument: Option<&str>) -> Self {
        match argument {
            None => Self::Nothing,
            Some(Self::STDIN) => Self::Stdin,
            Some(argument) if names_a_file(argument) => Self::File(PathBuf::from(argument)),
            Some(argument) => Self::Name(argument.to_string()),
        }
    }
}

/// Whether `argument` names a file rather than an executable.
///
/// Both separators, not the platform's: an executable's name is an identifier, so neither can appear in one, and a path written with the other spelling is still plainly a path. Named for what it decides rather than `is_file`, which reads as a question about the disk — and this asks nothing of it.
fn names_a_file(argument: &str) -> bool {
    argument.ends_with(".crs") || argument.contains('/') || argument.contains('\\')
}

/// Where a file argument is compiled.
pub enum Placement {
    /// Alone, against the prelude and nothing else, wherever it sits: how `run` and `compile` take a file.
    Standalone,
    /// In the unit that declares it, and standalone only when none does: how a question takes one.
    Contained,
}

/// What an argument selects.
pub enum Selection {
    /// The governing package entire.
    Entire(Entire),
    /// A package's library.
    Library(Library),
    /// One program.
    Program(Program),
}

impl Selection {
    /// What `spelling` selects, under the manifest `manifest` names or the one in `directory`.
    pub fn of(
        spelling: Spelling,
        manifest: Option<&Path>,
        directory: &Path,
        placement: Placement,
    ) -> Result<Self, String> {
        Ok(match spelling {
            // Both standalone forms answer here, before anything looks for a manifest.
            Spelling::Stdin => Self::Program(Program::loose(Entry::Stdin)),
            Spelling::File(path) => match placement {
                Placement::Standalone => Self::Program(Program::loose(Entry::File(path))),
                Placement::Contained => Self::contained(path, manifest)?,
            },
            Spelling::Name(name) => {
                let governing = Governing::found(manifest, directory)?;
                let executable = named(&governing.package, &name)?;
                Self::Program(program(&governing, executable)?)
            }
            Spelling::Nothing => Self::Entire(Entire {
                governing: Governing::found(manifest, directory)?,
            }),
        })
    }

    /// What `spelling` selects, standing where the process is.
    pub fn here(
        spelling: Spelling,
        manifest: Option<&Path>,
        placement: Placement,
    ) -> Result<Self, String> {
        let directory = std::env::current_dir().map_err(|error| error.to_string())?;

        Self::of(spelling, manifest, &directory, placement)
    }

    /// `file`, placed in the unit that declares it.
    fn contained(file: PathBuf, manifest: Option<&Path>) -> Result<Self, String> {
        Ok(match Membership::of(&file, manifest)? {
            Membership::Standalone => Self::Program(Program::loose(Entry::File(file))),
            Membership::Library {
                root,
                units,
                module,
            } => Self::Library(Library {
                root,
                units,
                through: Some(file),
                module,
            }),
            Membership::Executable {
                name,
                package,
                entry,
                output,
                root,
                units,
                declares,
            } => {
                let through = (identity(&file) != entry).then_some(file);
                Self::Program(Program {
                    entry: Entry::File(entry),
                    scope: Scope::Declared {
                        home: Home {
                            root,
                            package,
                            executable: name,
                            output,
                        },
                        units,
                        declares,
                    },
                    through,
                })
            }
        })
    }
}

/// The governing package entire: its library, when it has one, and every program it declares.
pub struct Entire {
    pub governing: Governing,
}

impl Entire {
    /// Its library, compiled against everything it depends on — `None` when no `lib.crs` sits beside the manifest.
    pub fn library(&self) -> Result<Option<Library>, String> {
        if !self.governing.directory.join(LIBRARY).is_file() {
            return Ok(None);
        }

        Ok(Some(Library {
            root: self.governing.root.clone(),
            units: order(&self.governing)?,
            through: None,
            module: None,
        }))
    }

    /// Every program it declares, in declaration order.
    pub fn programs(&self) -> Result<Vec<Program>, String> {
        self.governing
            .package
            .executables
            .iter()
            .map(|executable| program(&self.governing, executable))
            .collect()
    }

    /// The program no argument means to a command that needs one: the sole one, or the one `default` names.
    pub fn default_program(&self) -> Result<Program, String> {
        program(&self.governing, sole(&self.governing.package)?)
    }
}

/// A package's library, compiled against everything it depends on.
pub struct Library {
    /// The governing root, which is where the store sits.
    pub root: PathBuf,
    /// The whole scope in dependency order, the library last.
    pub units: Vec<RootSource>,
    /// The file the library was selected through, when it was selected through one.
    pub through: Option<PathBuf>,
    /// The module `through` would be by the layout rule — what to ask the library whether a `mod` declares, since a file under its directory that none does is in no unit at all. `None` when its spelling is no module's, or when nothing was selected through a file.
    pub module: Option<Qualifier>,
}

/// Where a program's text comes from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Entry {
    /// Standard input, drained to end — anonymous, so it has no entry path.
    Stdin,
    /// A file on disk.
    File(PathBuf),
}

/// One program: its entry, and what it is compiled against.
pub struct Program {
    entry: Entry,
    scope: Scope,
    through: Option<PathBuf>,
}

/// What a program is compiled against. Private, so a program is loose or declared by construction and never a loose one carrying a declared one's scope.
enum Scope {
    /// The prelude and nothing else, with nowhere to file what was built.
    Loose,
    /// A declared executable of its package.
    Declared {
        home: Home,
        /// The units it is compiled against — its own library last, everything that depends on before that.
        units: Vec<RootSource>,
        /// The prefixes the entry may name: its package's declared dependencies, its own library, and `/std`. Carried rather than derived by the caller, because it is the manifest's statement and nothing downstream reads a manifest.
        declares: Vec<Qualifier>,
    },
}

/// Where a declared program belongs.
pub struct Home {
    /// The governing root, which is where the store sits. Carried because what a compilation may reuse is a fact about the project it is in, and only the walk knows which project that is.
    pub root: PathBuf,
    /// The package that declares it. Carried beside `executable` because the two together are the one identity in a compilation that cannot collide, which is what the store addresses a built artifact by; neither alone will do, since an umbrella's members may each declare a `serve`.
    pub package: String,
    /// The executable's declared name.
    pub executable: String,
    /// Where a native build of it is written, under the governing root's store.
    pub output: PathBuf,
}

impl Program {
    /// A program compiled against the prelude alone.
    fn loose(entry: Entry) -> Self {
        Self {
            entry,
            scope: Scope::Loose,
            through: None,
        }
    }

    /// Where its text comes from.
    pub fn entry(&self) -> &Entry {
        &self.entry
    }

    /// Where it belongs — `None` for a loose program, which has no project and so no store.
    pub fn home(&self) -> Option<&Home> {
        match &self.scope {
            Scope::Loose => None,
            Scope::Declared { home, .. } => Some(home),
        }
    }

    /// The prefixes its entry may name, as its manifest declares them — `None` for a loose program, which declared nothing and so sees every open prefix in scope, which is the prelude's.
    pub fn declares(&self) -> Option<Vec<Qualifier>> {
        match &self.scope {
            Scope::Loose => None,
            Scope::Declared { declares, .. } => Some(declares.clone()),
        }
    }

    /// The file it was selected through, when that file is one of its modules rather than its entry.
    pub fn through(&self) -> Option<&Path> {
        self.through.as_deref()
    }

    /// The units it is compiled against, predecessors first — none for a loose program.
    pub fn into_units(self) -> Vec<RootSource> {
        match self.scope {
            Scope::Loose => Vec::new(),
            Scope::Declared { units, .. } => units,
        }
    }
}

/// `executable`, declared by the package `governing` governs, and everything needed to compile it.
fn program(governing: &Governing, executable: &Executable) -> Result<Program, String> {
    Ok(Program {
        entry: Entry::File(governing.directory.join(&executable.path)),
        scope: Scope::Declared {
            home: Home {
                root: governing.root.clone(),
                package: governing.package.name.clone(),
                executable: executable.name.clone(),
                output: governing
                    .store()
                    .executable(&governing.package.name, &executable.name),
            },
            declares: reachable(&governing.package),
            units: order(governing)?,
        },
        through: None,
    })
}

/// The executable `name` names.
fn named<'a>(package: &'a Package, name: &str) -> Result<&'a Executable, String> {
    if let Some(executable) = package
        .executables
        .iter()
        .find(|executable| executable.name == name)
    {
        return Ok(executable);
    }

    // The package's own name is declared by a file's presence rather than by a row, so the refusal names the file: this is the one name `Document::package` lets a `default` state without a row behind it, and the reader who wrote that `default` has to be told what declares it.
    match name == package.name {
        true => Err(format!(
            "{name:?} has no executable of its own: no `{EXECUTABLE}` sits beside the manifest, and no `[[executables]]` row declares one by that name{}",
            candidates(package)
        )),
        false => Err(format!(
            "{:?} declares no executable named {name:?}{}",
            package.name,
            candidates(package)
        )),
    }
}

/// The executable no argument means: the sole one, or the one `default` names.
fn sole(package: &Package) -> Result<&Executable, String> {
    if let Some(default) = &package.default {
        return named(package, default);
    }

    match package.executables.as_slice() {
        [executable] => Ok(executable),
        [] => Err(format!(
            "{:?} declares no executable: add `exe.crs`, or declare one with `[[executables]]`",
            package.name
        )),
        _ => Err(format!(
            "{:?} declares more than one executable and no `default`, so a bare target means nothing in particular{}",
            package.name,
            candidates(package)
        )),
    }
}

/// The executables a package does declare, for a refusal that leaves the reader somewhere to go.
fn candidates(package: &Package) -> String {
    match package.executables.as_slice() {
        [] => String::new(),
        executables => format!(
            "; it declares {}",
            executables
                .iter()
                .map(|executable| package.describe(executable))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}
