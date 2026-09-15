//! What each command accepts, stated once, and the one place a command's argument is admitted as its subject.
//!
//! **A contract is data, and dispatch reads it.** Every command's [`Contract`] says what its argument may select, where a file argument is compiled, how the command reaches the store and what it leaves on disk, and [`Mode::contract`] is an exhaustive match, so no command exists without one. What a command refuses is decided here too — a library where a program is needed, a file where a product must be filed under a package — so a refusal is one sentence per kind naming the command, never a sentence each command words for itself.
//!
//! **A question's access is stated here and enforced below.** A command that asks the `wonder` engine reads the store and files nothing because the engine wraps whatever store it is handed so that nothing can be filed, and no contract can relax that. What dispatch chooses from a contract is a build's store: whether one is opened for it to file into.

#[cfg(test)]
mod tests;

use {
    crate::{Mode, Query},
    curios_package::{Entire, Library, Placement, Program, Selection, Spelling},
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
    /// The store beside `root` for a build to file into — `None` for an access that files nothing. A question reads through [`Access::consulted`] instead, and it is the engine it hands the store to that keeps it from filing.
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

    /// The store beside `root` for a question to read, to be handed to the `wonder` engine, which files nothing into it — `None` for an access that does not read.
    pub(crate) fn consulted(self, root: &Path) -> Option<Verdicts> {
        debug_assert_ne!(
            self,
            Self::Write,
            "a question files nothing, so it never asks for a store to file into"
        );

        match self {
            Self::Read => Some(Verdicts::at(root.to_path_buf())),
            Self::None | Self::Write => None,
        }
    }
}

/// What a command leaves on disk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Product {
    /// Nothing beyond what the store keeps.
    Nothing,
    /// A native executable, under the store beside the package that declares it unless `--output` says elsewhere.
    Executable,
    /// A library's pages, under the store beside its package unless `--output` says elsewhere.
    Pages,
    /// Dependency sources, materialized into the store.
    Sources,
    /// A new package's files, in the directory named.
    Package,
    /// The files named, rewritten in place.
    Rewritten,
}

impl Product {
    /// Whether what is left is filed under the package that declared its subject, so a subject no package declares has nowhere to put it.
    fn filed_under_a_package(self) -> bool {
        matches!(self, Self::Executable | Self::Pages | Self::Sources)
    }
}

/// What a command accepts, how it reaches the store, and what it leaves behind.
pub(crate) struct Contract {
    /// The command as it is typed, for a refusal to name.
    pub(crate) command: &'static str,
    pub(crate) accepts: Accepts,
    /// Where a file argument is compiled.
    pub(crate) placement: Placement,
    pub(crate) access: Access,
    pub(crate) product: Product,
}

const RUN: Contract = Contract {
    command: "run",
    accepts: Accepts::Program,
    placement: Placement::Standalone,
    access: Access::Write,
    product: Product::Nothing,
};

const COMPILE: Contract = Contract {
    command: "compile",
    accepts: Accepts::Program,
    placement: Placement::Standalone,
    access: Access::Write,
    product: Product::Executable,
};

const DOCUMENT: Contract = Contract {
    command: "document",
    accepts: Accepts::Library,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Pages,
};

const DOCUMENT_ARCHIVE: Contract = Contract {
    command: "document",
    accepts: Accepts::Nothing,
    placement: Placement::Contained,
    access: Access::None,
    product: Product::Pages,
};

const TEST: Contract = Contract {
    command: "test",
    accepts: Accepts::Entire,
    placement: Placement::Contained,
    access: Access::Write,
    product: Product::Nothing,
};

const CURATE: Contract = Contract {
    command: "curate",
    accepts: Accepts::Entire,
    placement: Placement::Contained,
    access: Access::None,
    product: Product::Sources,
};

const NEW: Contract = Contract {
    command: "new",
    accepts: Accepts::Nothing,
    placement: Placement::Contained,
    access: Access::None,
    product: Product::Package,
};

const LINT: Contract = Contract {
    command: "lint",
    accepts: Accepts::Any,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Nothing,
};

const FORMAT: Contract = Contract {
    command: "format",
    accepts: Accepts::Nothing,
    placement: Placement::Contained,
    access: Access::None,
    product: Product::Rewritten,
};

const DIAGNOSTICS: Contract = Contract {
    command: "wonder diagnostics",
    accepts: Accepts::Any,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Nothing,
};

const TESTS: Contract = Contract {
    command: "wonder tests",
    accepts: Accepts::Any,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Nothing,
};

const COST: Contract = Contract {
    command: "wonder cost",
    accepts: Accepts::Program,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Nothing,
};

const STAGE: Contract = Contract {
    command: "wonder stage",
    accepts: Accepts::Program,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Nothing,
};

const SERVER: Contract = Contract {
    command: "wonder server",
    accepts: Accepts::Nothing,
    placement: Placement::Contained,
    access: Access::Read,
    product: Product::Nothing,
};

impl Mode {
    /// The contract this command is dispatched under.
    pub(crate) fn contract(&self) -> &'static Contract {
        match self {
            Mode::Run { .. } => &RUN,
            Mode::Compile { .. } => &COMPILE,
            // A file here holds an archived unit, which is read rather than resolved against a package.
            Mode::Document {
                target: Some(_), ..
            } => &DOCUMENT_ARCHIVE,
            Mode::Document { target: None, .. } => &DOCUMENT,
            Mode::Test { .. } => &TEST,
            Mode::Curate => &CURATE,
            Mode::New { .. } => &NEW,
            Mode::Lint { .. } => &LINT,
            Mode::Format { .. } => &FORMAT,
            Mode::Wonder { query } => match query {
                Query::Diagnostics { .. } => &DIAGNOSTICS,
                Query::Tests { .. } => &TESTS,
                Query::Cost { .. } => &COST,
                Query::Stage { .. } => &STAGE,
                Query::Server => &SERVER,
            },
        }
    }

    /// The argument the contract resolves: the target, for a command that takes one.
    pub(crate) fn target(&self) -> Option<&str> {
        match self {
            Mode::Run { target, .. } | Mode::Compile { target, .. } | Mode::Lint { target } => {
                target.as_deref()
            }
            Mode::Wonder { query } => match query {
                Query::Diagnostics { target }
                | Query::Tests { target }
                | Query::Cost { target }
                | Query::Stage { target, .. } => target.as_deref(),
                Query::Server => None,
            },
            // A filter, an archive, a directory to create and files to rewrite: arguments, and none of them a target.
            Mode::Document { .. }
            | Mode::Test { .. }
            | Mode::Curate
            | Mode::New { .. }
            | Mode::Format { .. } => None,
        }
    }
}

impl Contract {
    /// What `target` selects under this contract, standing in `directory`, before any kind of subject is refused.
    fn selection(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Selection, String> {
        let spelling = match (self.placement, Spelling::of(target)) {
            // A file placed in its unit is first a file the disk holds, where a standalone compile reads the file and fails on the read.
            (Placement::Contained, Spelling::File(path)) => Spelling::File(file_target(path)?),
            (_, spelling) => spelling,
        };

        Selection::of(spelling, manifest, directory, self.placement)
    }

    /// The program `target` selects: a program, or the package entire's sole or `default` one.
    pub(crate) fn admit_program(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Program, String> {
        debug_assert_eq!(self.accepts, Accepts::Program, "{}", self.command);

        let program = match self.selection(target, manifest, directory)? {
            Selection::Program(program) => program,
            Selection::Entire(entire) => entire.default_program()?,
            Selection::Library(_) => {
                return Err(format!(
                    "`{}` takes a program, and a library is not one: name an executable or a program file",
                    self.command
                ));
            }
        };

        if self.product.filed_under_a_package() && program.home().is_none() {
            return Err(format!(
                "`{}` files what it builds under the package that declares it, and a file or standard input has none: `run` is what takes one",
                self.command
            ));
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

        match self.selection(target, manifest, directory)? {
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

        self.selection(target, manifest, directory)
    }

    /// The governing package entire, which is all a command taking no target selects.
    pub(crate) fn admit_entire(
        &self,
        target: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Entire, String> {
        debug_assert_eq!(self.accepts, Accepts::Entire, "{}", self.command);

        match self.selection(target, manifest, directory)? {
            Selection::Entire(entire) => Ok(entire),
            Selection::Library(_) | Selection::Program(_) => Err(format!(
                "`{}` takes the governing package entire, and no target",
                self.command
            )),
        }
    }
}

/// Where the process stands, which is where an invocation's argument is resolved from.
pub(crate) fn here() -> Result<PathBuf, String> {
    env::current_dir().map_err(|error| error.to_string())
}
