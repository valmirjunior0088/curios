//! What each command accepts, stated once, and the one place a command's argument is admitted as its subject.
//!
//! **A contract is data, and dispatch reads it.** Every command's [`Contract`] says what its argument may select, whether a program must be named by its own file, how the command reaches the store and what it leaves on disk, and [`Mode::contract`] is an exhaustive match, so no command exists without one. What a command refuses is decided here too — a library where a program is needed, a module where a program's own file is, a build with nowhere to be filed, a flag written before the command that reads it — so a refusal is one sentence per kind naming the command, never a sentence each command words for itself. A TARGET's help is read off the same contract, so the help cannot describe an argument its command admits another way.
//!
//! **A question's access is stated here and enforced below.** A command that asks the `wonder` engine reads the store and files nothing because the engine wraps whatever store it is handed so that nothing can be filed, and no contract can relax that. What dispatch chooses from a contract is a build's store: whether one is opened for it to file into.

#[cfg(test)]
mod tests;

use {
    crate::{Cli, Elaboration, ManifestFlag, Mode, Pinned, Query},
    curios_package::{Entire, Entry, Library, Program, Selection, Spelling},
    curios_text::Overlay,
    curios_verdicts::Verdicts,
    curios_wonder::file_target,
    std::{
        env,
        path::{Path, PathBuf},
    },
};

/// What a command takes its argument to select.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Accepts {
    /// One program: one selected, or the governing package entire narrowed to its sole or `default` executable.
    Program,
    /// A library: the governing package entire narrowed to its own.
    Library,
    /// Whatever the argument selects.
    Any,
    /// The governing package entire and nothing else: the command's argument, when it has one, is not a target.
    Entire,
    /// Files to rewrite: every file what the argument selects declares, a file itself, or standard input.
    Files,
    /// No subject: nothing the command is given is resolved against a package.
    Nothing,
}

/// How a command reaches the store of verdicts and payloads beside the package it is about.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Access {
    /// It opens none.
    None,
    /// It reads one and files nothing: a question, which the `wonder` engine holds to that whatever store it is handed.
    Read,
    /// It reads one and files what it compiled: a build.
    Write,
}

impl Access {
    /// The store beside `root` for a build to file into — `None` for an access that files nothing. A question never opens one here: the `wonder` engine opens its own and wraps it so that nothing is filed.
    pub(crate) fn filed(self, root: &Path) -> Option<Verdicts> {
        debug_assert_ne!(
            self,
            Self::Read,
            "a build files what it compiled or opens no store at all"
        );

        match self {
            Self::Write => Some(Verdicts::at(root.to_path_buf())),
            Self::None | Self::Read => None,
        }
    }
}

/// What a command leaves on disk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Product {
    /// Nothing beyond what the store keeps.
    Nothing,
    /// A native executable, under the store beside the package that declares it, or wherever `--output` says — the one place a program no package declares can go.
    Executable,
    /// A library's pages, under the store beside its package unless `--output` says elsewhere.
    Pages,
    /// Dependency sources, materialized into the store.
    Sources,
    /// A new package's files, in the directory named.
    Package,
    /// The files named, rewritten in place.
    Rewritten,
    /// The governing package's manifest, with one row written into it.
    Manifest,
}

impl Product {
    /// Whether what is left is filed under the package that declared its subject unless the command is told where else, so a subject no package declares has to be told.
    fn filed_under_a_package(self) -> bool {
        matches!(self, Self::Executable | Self::Pages | Self::Sources)
    }
}

/// What a command accepts, how it reaches the store, and what it leaves behind.
pub(crate) struct Contract {
    /// The command as it is typed, for a refusal to name.
    pub(crate) command: &'static str,
    pub(crate) accepts: Accepts,
    /// Whether a program must be named by its own file rather than through one of its modules: a command that runs or builds a program performs it, and a module is not what performs.
    pub(crate) own_file_only: bool,
    pub(crate) access: Access,
    pub(crate) product: Product,
}

pub(crate) const RUN: Contract = Contract {
    command: "run",
    accepts: Accepts::Program,
    own_file_only: true,
    access: Access::Write,
    product: Product::Nothing,
};

pub(crate) const COMPILE: Contract = Contract {
    command: "compile",
    accepts: Accepts::Program,
    own_file_only: true,
    access: Access::Write,
    product: Product::Executable,
};

pub(crate) const DOCUMENT: Contract = Contract {
    command: "document",
    accepts: Accepts::Library,
    own_file_only: false,
    access: Access::Write,
    product: Product::Pages,
};

pub(crate) const DOCUMENT_ARCHIVE: Contract = Contract {
    command: "document",
    accepts: Accepts::Nothing,
    own_file_only: false,
    access: Access::None,
    product: Product::Pages,
};

pub(crate) const TEST: Contract = Contract {
    command: "test",
    accepts: Accepts::Any,
    own_file_only: false,
    access: Access::Write,
    product: Product::Nothing,
};

pub(crate) const CURATE: Contract = Contract {
    command: "curate",
    accepts: Accepts::Entire,
    own_file_only: false,
    access: Access::None,
    product: Product::Sources,
};

/// Its subject is the governing package's own manifest, which it resolves nothing against and writes back: no target, and the store it reaches is the one a delivery is filed in rather than one it compiles into.
pub(crate) const PIN: Contract = Contract {
    command: "pin",
    accepts: Accepts::Nothing,
    own_file_only: false,
    access: Access::None,
    product: Product::Manifest,
};

pub(crate) const NEW: Contract = Contract {
    command: "new",
    accepts: Accepts::Nothing,
    own_file_only: false,
    access: Access::None,
    product: Product::Package,
};

pub(crate) const LINT: Contract = Contract {
    command: "lint",
    accepts: Accepts::Any,
    own_file_only: false,
    access: Access::Read,
    product: Product::Nothing,
};

pub(crate) const FORMAT: Contract = Contract {
    command: "format",
    accepts: Accepts::Files,
    own_file_only: false,
    access: Access::None,
    product: Product::Rewritten,
};

/// Its argument is a filed stream rather than anything a package declares, so nothing is resolved against a manifest and no store is opened: the file named is the whole of what it reads.
#[cfg(feature = "profile")]
pub(crate) const PROFILE: Contract = Contract {
    command: "profile",
    accepts: Accepts::Nothing,
    own_file_only: false,
    access: Access::None,
    product: Product::Nothing,
};

pub(crate) const DIAGNOSTICS: Contract = Contract {
    command: "wonder diagnostics",
    accepts: Accepts::Any,
    own_file_only: false,
    access: Access::Read,
    product: Product::Nothing,
};

pub(crate) const TESTS: Contract = Contract {
    command: "wonder tests",
    accepts: Accepts::Any,
    own_file_only: false,
    access: Access::Read,
    product: Product::Nothing,
};

pub(crate) const COST: Contract = Contract {
    command: "wonder cost",
    accepts: Accepts::Program,
    own_file_only: false,
    access: Access::Read,
    product: Product::Nothing,
};

pub(crate) const STAGE: Contract = Contract {
    command: "wonder stage",
    accepts: Accepts::Program,
    own_file_only: false,
    access: Access::Read,
    product: Product::Nothing,
};

pub(crate) const SERVER: Contract = Contract {
    command: "wonder server",
    accepts: Accepts::Nothing,
    own_file_only: false,
    access: Access::Read,
    product: Product::Nothing,
};

impl Mode {
    /// The contract this command is dispatched under.
    pub(crate) fn contract(&self) -> &'static Contract {
        match self {
            Mode::Run { .. } => &RUN,
            Mode::Compile { .. } => &COMPILE,
            // An archived unit is read rather than resolved against a package.
            Mode::Document {
                archive: Some(_), ..
            } => &DOCUMENT_ARCHIVE,
            Mode::Document { archive: None, .. } => &DOCUMENT,
            Mode::Test { .. } => &TEST,
            Mode::Curate { .. } => &CURATE,
            Mode::Pin { .. } => &PIN,
            Mode::New { .. } => &NEW,
            Mode::Lint { .. } => &LINT,
            Mode::Format { .. } => &FORMAT,
            Mode::Wonder { query } => match query {
                Query::Diagnostics { .. } => &DIAGNOSTICS,
                Query::Tests { .. } => &TESTS,
                Query::Cost { .. } => &COST,
                Query::Stage { .. } => &STAGE,
                Query::Server { .. } => &SERVER,
            },
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => &PROFILE,
        }
    }

    /// The argument the contract resolves: the target, for a command that takes one — for `format`, the first of its targets, each of which is admitted alone the same way.
    pub(crate) fn target(&self) -> Option<&str> {
        match self {
            Mode::Run { target, .. }
            | Mode::Compile { target, .. }
            | Mode::Document { target, .. }
            | Mode::Test { target, .. }
            | Mode::Lint { target, .. } => target.as_deref(),
            Mode::Format { targets, .. } => targets.first().map(String::as_str),
            Mode::Wonder { query } => match query {
                Query::Diagnostics { target, .. }
                | Query::Tests { target, .. }
                | Query::Cost { target, .. }
                | Query::Stage { target, .. } => target.as_deref(),
                Query::Server { .. } => None,
            },
            // A directory to create is an argument, and no target. So is a filed stream to read.
            Mode::Curate { .. } | Mode::New { .. } | Mode::Pin { .. } => None,
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => None,
        }
    }

    /// The manifest the command was told governs — `None` when it was told none, or reads no manifest.
    pub(crate) fn manifest(&self) -> Option<&Path> {
        self.manifest_flag()?.manifest.as_deref()
    }

    /// Where the command was told to write what it builds — `None` when it was told nowhere, or builds nothing it could be told about.
    pub(crate) fn output(&self) -> Option<&Path> {
        match self {
            Mode::Compile { output_path, .. } | Mode::Document { output_path, .. } => {
                output_path.as_deref()
            }
            Mode::Run { .. }
            | Mode::Test { .. }
            | Mode::Curate { .. }
            | Mode::New { .. }
            | Mode::Pin { .. }
            | Mode::Lint { .. }
            | Mode::Format { .. }
            | Mode::Wonder { .. } => None,
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => None,
        }
    }

    /// The `--manifest` flag, for a command that resolves against a package.
    fn manifest_flag(&self) -> Option<&ManifestFlag> {
        match self {
            Mode::Curate { manifest } | Mode::Format { manifest, .. } => Some(manifest),
            Mode::Pin { row } => match row {
                Pinned::Foreign { manifest, .. } | Pinned::Dependency { manifest, .. } => {
                    Some(manifest)
                }
            },
            _ => self.elaboration().map(|elaboration| &elaboration.manifest),
        }
    }

    /// The flags of a command that elaborates.
    fn elaboration(&self) -> Option<&Elaboration> {
        match self {
            Mode::Run { elaboration, .. }
            | Mode::Compile { elaboration, .. }
            | Mode::Document { elaboration, .. }
            | Mode::Test { elaboration, .. }
            | Mode::Lint { elaboration, .. } => Some(elaboration),
            Mode::Wonder { query } => match query {
                Query::Diagnostics { elaboration, .. }
                | Query::Tests { elaboration, .. }
                | Query::Cost { elaboration, .. }
                | Query::Stage { elaboration, .. }
                | Query::Server { elaboration } => Some(elaboration),
            },
            Mode::Curate { .. } | Mode::New { .. } | Mode::Pin { .. } | Mode::Format { .. } => None,
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => None,
        }
    }
}

impl Cli {
    /// The refusal a flag earns written before the command, where `--budget` and `--manifest` stood before they belonged to the commands that read them: the spelling that works, or why the command takes no such flag.
    pub(crate) fn misplaced(&self) -> Option<String> {
        let command = self.mode.contract().command;

        if let Some(budget) = &self.misplaced_budget {
            return Some(match self.mode.elaboration() {
                Some(_) => format!(
                    "`--budget` belongs to the command, so it follows it: `curios {command} --budget {}`",
                    budget.to_string_lossy()
                ),
                None => format!("`{command}` elaborates nothing, so it takes no `--budget`"),
            });
        }

        let manifest = self.misplaced_manifest.as_ref()?;

        Some(match self.mode.manifest_flag() {
            Some(_) => format!(
                "`--manifest` belongs to the command, so it follows it: `curios {command} --manifest {}`",
                manifest.to_string_lossy()
            ),
            None => format!("`{command}` reads no manifest, so it takes no `--manifest`"),
        })
    }
}

/// What `format` rewrites for one target.
pub(crate) enum Rewritten {
    /// Files on disk, rewritten in place.
    Files(Vec<PathBuf>),
    /// Standard input, written back to standard output.
    Stdin,
}

impl Contract {
    /// What a TARGET's help says for this command: the forms its argument takes, and what none means.
    pub(crate) fn target_help(&self) -> String {
        let forms = match (self.accepts, self.own_file_only) {
            (Accepts::Library, _) => "A .crs file a library declares",
            (_, true) => {
                "A declared executable's name, a program's own .crs file, or `-` for standard input"
            }
            (_, false) => {
                "A declared executable's name, a path to a .crs file, or `-` for standard input"
            }
        };
        let none = match self.accepts {
            Accepts::Program => "the governing package's sole or `default` executable",
            Accepts::Library => "the governing package's library",
            Accepts::Any | Accepts::Entire => "the governing package entire",
            Accepts::Files => "every file the governing package declares",
            Accepts::Nothing => "nothing",
        };

        format!("{forms} (default: {none})")
    }

    /// What `target` selects standing in `directory`, before any kind of subject is refused — the same for every contract, since a file is placed one way whichever command asks.
    fn selection(
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Selection, String> {
        let spelling = match Spelling::of(target) {
            // A file is placed by what declares it, which starts from a file the disk holds: a path it does not hold is refused here, in the words the read would have failed with.
            Spelling::File(path) => Spelling::File(file_target(path)?),
            spelling => spelling,
        };

        Selection::of(spelling, manifest, directory, &Overlay::default())
    }

    /// The program `target` selects: a program, or the package entire's sole or `default` one — named by its own file where the contract says so, and with somewhere to put what is built, `output` included.
    pub(crate) fn admit_program(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        output: Option<&Path>,
        directory: &Path,
    ) -> Result<Program, String> {
        debug_assert_eq!(self.accepts, Accepts::Program, "{}", self.command);

        let program = match Self::selection(target, manifest, directory)? {
            Selection::Program(program) => program,
            Selection::Entire(entire) => entire.default_program()?,
            Selection::Library(_) => {
                return Err(format!(
                    "`{}` takes a program, and a library is not one: name an executable or a program file",
                    self.command
                ));
            }
        };

        if self.own_file_only
            && let (Some(module), Some(home)) = (program.through(), program.home())
        {
            return Err(format!(
                "`{}` takes a program's own file, and {} is a module of `{}`: name `{}` instead",
                self.command,
                module.display(),
                home.executable,
                home.executable
            ));
        }

        if self.product.filed_under_a_package() && program.home().is_none() && output.is_none() {
            return Err(homeless(self.command, program.entry()));
        }

        Ok(program)
    }

    /// The library `target` selects: a library, or the package entire's own.
    pub(crate) fn admit_library(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Library, String> {
        debug_assert_eq!(self.accepts, Accepts::Library, "{}", self.command);

        match Self::selection(target, manifest, directory)? {
            Selection::Library(library) => Ok(library),
            Selection::Entire(entire) => entire.library()?.ok_or_else(|| {
                format!(
                    "`{}` takes a library, and {:?} declares none",
                    self.command, entire.governing.package.name
                )
            }),
            Selection::Program(_) => Err(format!(
                "`{}` takes a library, and a program is not one",
                self.command
            )),
        }
    }

    /// Whatever `target` selects.
    pub(crate) fn admit_any(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Selection, String> {
        debug_assert_eq!(self.accepts, Accepts::Any, "{}", self.command);

        Self::selection(target, manifest, directory)
    }

    /// The governing package entire, which is all a command taking no target selects.
    pub(crate) fn admit_entire(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Entire, String> {
        debug_assert_eq!(self.accepts, Accepts::Entire, "{}", self.command);

        match Self::selection(target, manifest, directory)? {
            Selection::Entire(entire) => Ok(entire),
            Selection::Library(_) | Selection::Program(_) => Err(format!(
                "`{}` takes the governing package entire, and no target",
                self.command
            )),
        }
    }

    /// What `target` rewrites: every file a package or program it names declares, a file itself whatever declares it — formatting reads no unit, so placement is not its question — or standard input.
    pub(crate) fn admit_files(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Rewritten, String> {
        debug_assert_eq!(self.accepts, Accepts::Files, "{}", self.command);

        Ok(match Spelling::of(target) {
            Spelling::File(path) => Rewritten::Files(vec![file_target(path)?]),
            spelling => match Self::selection(target, manifest, directory)? {
                Selection::Entire(entire) => Rewritten::Files(entire.declared_files()?),
                Selection::Library(library) => Rewritten::Files(library.declared_files()?),
                Selection::Program(_) if spelling == Spelling::Stdin => Rewritten::Stdin,
                Selection::Program(program) => Rewritten::Files(program.declared_files()?),
            },
        })
    }
}

/// What a command that builds says about a program no package declares and nothing said where to put: that it has nowhere, and the command line that gives it somewhere.
fn homeless(command: &str, entry: &Entry) -> String {
    match entry {
        Entry::Stdin => format!(
            "`{command}` files what it builds under the package that declares it, and standard input has none: say where, as in `curios {command} - --output program`"
        ),
        Entry::File(path) => {
            let named = path
                .file_stem()
                .map_or_else(|| "program".into(), |stem| stem.to_string_lossy());
            format!(
                "`{command}` files what it builds under the package that declares it, and {} has none: say where, as in `curios {command} {} --output {named}`",
                path.display(),
                path.display()
            )
        }
    }
}

/// Where the process stands, which is where an invocation's argument is resolved from.
pub(crate) fn here() -> Result<PathBuf, String> {
    env::current_dir().map_err(|error| error.to_string())
}
