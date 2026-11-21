//! What each command accepts, stated once, and the one place a command's argument is admitted as its subject.
//!
//! **A contract is data, and dispatch reads it.** Every command's [`Contract`] says what its argument may select, whether a program must be named by its own file, how the command reaches the cabinet and what it leaves on disk, and [`Mode::contract`] is an exhaustive match, so no command exists without one. What a command refuses is decided here too — a library where a program is needed, a module where a program's own file is, a build with nowhere to be filed, a flag written before the command that reads it — so a refusal is one sentence per kind naming the command, never a sentence each command words for itself. A SUBJECT's help is read off the same contract, so the help cannot describe an argument its command admits another way.
//!
//! **A question's access is a build's, and what it files is decided below.** A command that asks the `wonder` engine reads the cabinet and files what it compiled as a build would have: the engine opens the cabinet beside what it is asked about and files a unit where the disk holds every text its fold has read. What dispatch chooses from a contract is a build's cabinet: whether one is opened for it to file into, and whether the project's own is collected once the command has filed ([`Access::collect`]).

#[cfg(test)]
mod tests;

use {
    crate::{Elaboration, ManifestFlag, Mode, Pinned, Query, View},
    curios_common::{OsError, Report, STDIN_NAME},
    curios_module::Overlay,
    curios_package::{
        Cabinet, Entire, Entry, FormatSettings, Governing, Library, Program, Refused, Selection,
        Spelling, Supplied, nearest_manifest,
    },
    curios_pipeline::Stage,
    curios_surface::{Format, WrittenIn},
    curios_verdict::{Verdicts, collect_project},
    curios_wonder::{Scope, Staged, file_subject, not_a_program},
    std::{
        env,
        path::{Path, PathBuf},
        time::SystemTime,
    },
};

/// What a command takes its argument to select.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Accepts {
    /// One program: one selected, or the governing package entire narrowed to its sole or `default` executable.
    Program,
    /// One program, as [`Accepts::Program`] takes one, or a library selected through one of its files.
    ProgramOrLibrary,
    /// A scope to ask a name in: what [`Accepts::ProgramOrLibrary`] takes, and with no subject the widest scope where the command stands — the governing package's sole or `default` executable's, else its library's, and outside every package the prelude's alone.
    Scope,
    /// A library: the governing package entire narrowed to its own.
    Library,
    /// Whatever the argument selects.
    Any,
    /// The governing package entire and nothing else: the command's argument, when it has one, is not a subject.
    Entire,
    /// Files to rewrite: every file what the argument selects declares, a file itself, or standard input.
    Files,
    /// No subject: nothing the command is given is resolved against a package.
    Nothing,
}

/// How a command reaches the cabinet of verdicts and payloads its package files into.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    /// It opens none.
    None,
    /// It reads one and files what it compiled: a build, which files every unit and its product, and a question, which files the units a build would have.
    Write,
}

impl Access {
    /// The verdicts of `cabinet` for a build to file into — `None` for an access that opens none. A question does not open one here: the `wonder` engine opens the cabinet of what it is asked about.
    pub fn filed(self, cabinet: &Cabinet) -> Option<Verdicts> {
        match self {
            Self::Write => Some(Verdicts::at(cabinet.clone())),
            Self::None => None,
        }
    }

    /// Collect the project at `root` once a command of this access has filed into its own cabinet, whatever the filing came to ([`collect_project`]): nothing for a command that files nothing, for a subject no package declares, which has no `root`, and for a cabinet the command line `named` for every project a run sees, which is collected within each address alone.
    pub fn collect(self, root: Option<&Path>, named: Option<&Path>) {
        if let (Self::Write, Some(root), None) = (self, root, named) {
            collect_project(root, SystemTime::now());
        }
    }
}

/// What a command leaves on disk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Product {
    /// Nothing beyond what the cabinet keeps.
    Nothing,
    /// A native executable, or for a browser a directory holding its page, under `.curios/` beside the package that declares it, under the target it is built for, or wherever `--output` says — the one place a program no package declares can go.
    Executable,
    /// A library's pages, under `.curios/` beside its package unless `--output` says elsewhere.
    Pages,
    /// Dependency sources, materialized into the cabinet.
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

/// What an invocation states of standard input: whether `--core` says it is Core text, and the read that drains it, which admission draws on only where `-` asks for it.
pub struct StandardInput<'a> {
    pub core: bool,
    pub read: &'a dyn Fn() -> Result<String, Box<Report>>,
}

/// What a command accepts, how it reaches the cabinet, and what it leaves behind.
pub struct Contract {
    /// The command as it is typed, for a refusal to name.
    pub command: &'static str,
    pub accepts: Accepts,
    /// Whether a program must be named by its own file rather than through one of its modules: a command that runs or builds a program performs it, and a module is not what performs.
    pub own_file_only: bool,
    /// Whether the command reads Core text: a file whose name says it is Core text ([`WrittenIn::of`]) is refused, naming the cause, by a command that does not, before anything reads it.
    pub core_text: bool,
    pub access: Access,
    pub product: Product,
}

pub const RUN: Contract = Contract {
    command: "run",
    accepts: Accepts::Program,
    own_file_only: true,
    core_text: false,
    access: Access::Write,
    product: Product::Nothing,
};

pub const COMPILE: Contract = Contract {
    command: "compile",
    accepts: Accepts::Program,
    own_file_only: true,
    core_text: false,
    access: Access::Write,
    product: Product::Executable,
};

pub const DOCUMENT: Contract = Contract {
    command: "document",
    accepts: Accepts::Library,
    own_file_only: false,
    core_text: false,
    access: Access::Write,
    product: Product::Pages,
};

pub const DOCUMENT_STD: Contract = Contract {
    command: "document",
    accepts: Accepts::Nothing,
    own_file_only: false,
    core_text: false,
    access: Access::None,
    product: Product::Pages,
};

pub const TEST: Contract = Contract {
    command: "test",
    accepts: Accepts::Any,
    own_file_only: false,
    core_text: false,
    access: Access::Write,
    product: Product::Nothing,
};

pub const CURATE: Contract = Contract {
    command: "curate",
    accepts: Accepts::Entire,
    own_file_only: false,
    core_text: false,
    access: Access::None,
    product: Product::Sources,
};

/// Its subject is the governing package's own manifest, which it resolves nothing against and writes back: no argument names it, and the cabinet it reaches is the one a delivery is filed in rather than one it compiles into.
pub const PIN: Contract = Contract {
    command: "pin",
    accepts: Accepts::Nothing,
    own_file_only: false,
    core_text: false,
    access: Access::None,
    product: Product::Manifest,
};

pub const NEW: Contract = Contract {
    command: "new",
    accepts: Accepts::Nothing,
    own_file_only: false,
    core_text: false,
    access: Access::None,
    product: Product::Package,
};

pub const LINT: Contract = Contract {
    command: "lint",
    accepts: Accepts::Any,
    own_file_only: false,
    core_text: false,
    access: Access::Write,
    product: Product::Nothing,
};

pub const FORMAT: Contract = Contract {
    command: "format",
    accepts: Accepts::Files,
    own_file_only: false,
    core_text: true,
    access: Access::None,
    product: Product::Rewritten,
};

/// Its argument is a filed stream rather than anything a package declares, so nothing is resolved against a manifest and no cabinet is opened: the file named is the whole of what it reads.
#[cfg(feature = "profile")]
pub const PROFILE: Contract = Contract {
    command: "profile",
    accepts: Accepts::Nothing,
    own_file_only: false,
    core_text: false,
    access: Access::None,
    product: Product::Nothing,
};

pub const DIAGNOSTICS: Contract = Contract {
    command: "wonder",
    accepts: Accepts::Any,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

pub const TESTS: Contract = Contract {
    command: "wonder --tests",
    accepts: Accepts::Any,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

pub const STAGE: Contract = Contract {
    command: "wonder --stage",
    accepts: Accepts::Program,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// `wonder --stage` at a rung a unit has ([`Stage::of_a_unit`]): a library's file is reprinted there as a program is, where every rung past them is a program's alone.
pub const STAGE_OF_A_UNIT: Contract = Contract {
    command: "wonder --stage",
    accepts: Accepts::ProgramOrLibrary,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// A question about one file: a program's, named by its own file or through one of its modules, or a library's.
pub const AT: Contract = Contract {
    command: "wonder --at",
    accepts: Accepts::ProgramOrLibrary,
    own_file_only: false,
    core_text: false,
    access: Access::Write,
    product: Product::Nothing,
};

/// A question about one file, as [`AT`] is.
pub const CLASSES: Contract = Contract {
    command: "wonder --classes",
    accepts: Accepts::ProgramOrLibrary,
    own_file_only: false,
    core_text: false,
    access: Access::Write,
    product: Product::Nothing,
};

/// A question about a declaration by its path, asked in the scope of a program or a library.
pub const NAME: Contract = Contract {
    command: "wonder --name",
    accepts: Accepts::Scope,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// A question about the witnesses a declaration takes part in, by its path, as [`NAME`] is about the declaration.
pub const WITNESSES: Contract = Contract {
    command: "wonder --name --witnesses",
    accepts: Accepts::Scope,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// A declaration's part of a rung a unit has is read off the unit that declares it, so the view takes any scope `--name` takes.
pub const NAMED_STAGE_OF_A_UNIT: Contract = Contract {
    command: "wonder --name --stage",
    accepts: Accepts::Scope,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// Past a unit's rungs, a declaration's part is a compiled program's.
pub const NAMED_STAGE: Contract = Contract {
    command: "wonder --name --stage",
    accepts: Accepts::Program,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// A closing term is a program's, so the question takes a program, written as one or as Core text.
pub const FALSE: Contract = Contract {
    command: "wonder --false",
    accepts: Accepts::Program,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

/// What a program holds of a declaration is a compiled program's, so the view takes a program where `--name` alone takes any scope.
pub const FOOTPRINT: Contract = Contract {
    command: "wonder --name --footprint",
    accepts: Accepts::Program,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

pub const SERVER: Contract = Contract {
    command: "wonder --server",
    accepts: Accepts::Nothing,
    own_file_only: false,
    core_text: true,
    access: Access::Write,
    product: Product::Nothing,
};

impl Mode {
    /// The contract this command is dispatched under.
    pub fn contract(&self) -> &'static Contract {
        match self {
            Mode::Run { .. } => &RUN,
            Mode::Compile { .. } => &COMPILE,
            // The standard library is read off the prelude rather than resolved against a package.
            Mode::Document { std: Some(_), .. } => &DOCUMENT_STD,
            Mode::Document { std: None, .. } => &DOCUMENT,
            Mode::Test { .. } => &TEST,
            Mode::Curate { .. } => &CURATE,
            Mode::Pin { .. } => &PIN,
            Mode::New { .. } => &NEW,
            Mode::Lint { .. } => &LINT,
            Mode::Format { .. } => &FORMAT,
            Mode::Wonder { query, .. } => match query {
                Query::Check => &DIAGNOSTICS,
                Query::Tests => &TESTS,
                Query::Stage(name) if Stage::of_a_unit(name) => &STAGE_OF_A_UNIT,
                Query::Stage(_) => &STAGE,
                Query::At(_) => &AT,
                Query::Classes => &CLASSES,
                Query::Name(_, None) => &NAME,
                Query::Name(_, Some(View::Witnesses)) => &WITNESSES,
                Query::Name(_, Some(View::Footprint)) => &FOOTPRINT,
                Query::Name(_, Some(View::Stage(name))) if Stage::of_a_unit(name) => {
                    &NAMED_STAGE_OF_A_UNIT
                }
                Query::Name(_, Some(View::Stage(_))) => &NAMED_STAGE,
                Query::False => &FALSE,
                Query::Server => &SERVER,
            },
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => &PROFILE,
        }
    }

    /// The argument the contract resolves: the subject, for a command that takes one — for `format`, the first of its subjects, each of which is admitted alone the same way.
    pub fn subject(&self) -> Option<&str> {
        match self {
            Mode::Run { subject, .. }
            | Mode::Compile { subject, .. }
            | Mode::Document { subject, .. }
            | Mode::Test { subject, .. }
            | Mode::Lint { subject, .. } => subject.as_deref(),
            // A place names its file, which is the question's subject.
            Mode::Wonder {
                query: Query::At(place),
                ..
            } => Some(place.subject()),
            Mode::Wonder { subject, .. } => subject.as_deref(),
            Mode::Format { subjects, .. } => subjects.first().map(String::as_str),
            // A directory to create is an argument, and no subject. So is a filed stream to read.
            Mode::Curate { .. } | Mode::New { .. } | Mode::Pin { .. } => None,
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => None,
        }
    }

    /// Whether the command was told standard input is Core text — `false` for a command that reads none.
    pub fn core(&self) -> bool {
        match self {
            Mode::Wonder { core, .. } | Mode::Format { core, .. } => core.core,
            Mode::Run { .. }
            | Mode::Compile { .. }
            | Mode::Document { .. }
            | Mode::Test { .. }
            | Mode::Curate { .. }
            | Mode::Pin { .. }
            | Mode::New { .. }
            | Mode::Lint { .. } => false,
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => false,
        }
    }

    /// The manifest the command was told governs — `None` when it was told none, or reads no manifest.
    pub fn manifest(&self) -> Option<&Path> {
        self.manifest_flag()?.manifest.as_deref()
    }

    /// Where the command was told to write what it builds — `None` when it was told nowhere, or builds nothing it could be told about.
    pub fn output(&self) -> Option<&Path> {
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
            | Mode::Lint { elaboration, .. }
            | Mode::Wonder { elaboration, .. } => Some(elaboration),
            Mode::Curate { .. } | Mode::New { .. } | Mode::Pin { .. } | Mode::Format { .. } => None,
            #[cfg(feature = "profile")]
            Mode::Profile { .. } => None,
        }
    }
}

/// What `format` rewrites for one subject, and the format it is formatted within.
pub enum Rewritten {
    /// Files on disk, rewritten in place: the files of one subject, which one package governs or none does, so one format serves them all.
    Files(Vec<PathBuf>, Format),
    /// What standard input held, written back to standard output, in the format the command line states, since no manifest governs it.
    Stdin(Format, Supplied),
}

impl Contract {
    /// A `wonder` query's help: what it answers, and what no subject is for it where that is not what it is for the check, whose contract ([`DIAGNOSTICS`]) the one SUBJECT's help is read off.
    pub fn query_help(&self, answers: &str) -> String {
        match self.no_subject() == DIAGNOSTICS.no_subject() {
            true => answers.to_string(),
            false => format!("{answers}; with no subject, {}", self.no_subject()),
        }
    }

    /// What a SUBJECT's help says for this command: the forms its argument takes, and what none means.
    pub fn subject_help(&self) -> String {
        let file = match self.core_text {
            true => "a `.crs` or `.crsc` file",
            false => "a `.crs` file",
        };
        let forms = match (self.accepts, self.own_file_only) {
            (Accepts::Library, _) => "A `.crs` file a library declares".to_string(),
            (_, true) => {
                "A declared executable's name, a program's own `.crs` file, or `-` for standard input".to_string()
            }
            (_, false) => format!(
                "A declared executable's name, a path to {file}, or `-` for standard input"
            ),
        };

        format!("{forms} (default: {})", self.no_subject())
    }

    /// What no argument selects for this command.
    fn no_subject(&self) -> &'static str {
        match self.accepts {
            Accepts::Program | Accepts::ProgramOrLibrary => {
                "the governing package's sole or `default` executable"
            }
            Accepts::Scope => {
                "the scope of the governing package's sole or `default` executable, else of its library, and outside every package the prelude's"
            }
            Accepts::Library => "the governing package's library",
            Accepts::Any | Accepts::Entire => "the governing package entire",
            Accepts::Files => "every file the governing package declares",
            Accepts::Nothing => "nothing",
        }
    }

    /// What `subject` selects standing in `directory`, before any kind of subject is refused — the same for every contract, since a file is placed one way whichever command asks — save Core text, which a command that reads none refuses first, and `--core` beside any subject but `-`. Standard input is drained here, by `stdin`'s read, where `-` is the subject, and nowhere else.
    fn selection(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Selection, Box<Report>> {
        let written = Self::stdin_written_in(stdin.core, subject)?;
        let spelling = match Spelling::of(subject) {
            // A file is placed by what declares it, which starts from a file the disk holds: a path it does not hold is refused here, in the words the read would have failed with.
            Spelling::File(path) => Spelling::File(self.read_here(file_subject(path)?)?),
            spelling => spelling,
        };

        Selection::of(
            spelling,
            manifest,
            directory,
            &Overlay::default(),
            cabinet,
            || {
                Ok(Supplied {
                    text: (stdin.read)()?,
                    written,
                })
            },
        )
    }

    /// `path`, where the command reads what its name says it is written in: a command that reads no Core text refuses a `.crsc` before anything reads it, rather than reading it as the program it is not.
    fn read_here(&self, path: PathBuf) -> Result<PathBuf, Box<Report>> {
        match (self.core_text, WrittenIn::of(&path)) {
            (false, WrittenIn::CoreText) => Err(Refused::of("core text")
                .stating(format_args!("`{}` reads no Core text", self.command))
                .stating(format_args!(
                    "a `.{}` file is Core text, which `wonder` checks and `format` formats",
                    WrittenIn::CoreText.extension()
                ))
                .trying(format!("curios wonder {}", path.display()))
                .about_path(&path)),
            _ => Ok(path),
        }
    }

    /// What standard input is written in where `core` says it is Core text, refused beside any subject but standard input, whose text alone the flag speaks of: a file says what it is written in by its name.
    fn stdin_written_in(core: bool, subject: Option<&str>) -> Result<WrittenIn, Box<Report>> {
        match (core, Spelling::of(subject)) {
            (false, _) => Ok(WrittenIn::Program),
            (true, Spelling::Stdin) => Ok(WrittenIn::CoreText),
            (true, spelling) => Err(Refused::of("surplus flag")
                .stating("`--core` says the text on standard input is Core text")
                .stating(match spelling {
                    Spelling::File(_) => format!(
                        "a file says what it is written in by its name: a `.{}` is Core text",
                        WrittenIn::CoreText.extension()
                    ),
                    Spelling::Name(name) => format!("`{name}` names an executable, a program"),
                    Spelling::Nothing | Spelling::Stdin => {
                        "with no subject nothing reads standard input; `-` does".to_string()
                    }
                })
                .about_named("--core")),
        }
    }

    /// The program `subject` selects: a program, or the package entire's sole or `default` one — named by its own file where the contract says so, and with somewhere to put what is built, `output` included.
    pub fn admit_program(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        output: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Program, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::Program, "{}", self.command);

        let program = match self.selection(subject, manifest, directory, cabinet, stdin)? {
            Selection::Program(program) => program,
            Selection::Entire(entire) => entire.default_program()?,
            Selection::Library(library) => {
                let refused = Refused::of("not a program")
                    .stating(format_args!("`{}` takes a program", self.command))
                    .stating(format_args!(
                        "this file belongs to the library of `/{}`",
                        library.package
                    ))
                    .stating("name an executable or a program file");

                return Err(match &library.through {
                    Some(file) => refused.about_path(file),
                    None => refused.about_path(&library.manifest),
                });
            }
            Selection::Module(module) => return Err(not_a_program(module.entry())),
        };

        if self.own_file_only
            && let (Some(module), Some(home)) = (program.through(), program.home())
        {
            return Err(Refused::of("not a program")
                .stating(format_args!(
                    "`{}` takes a program's own file",
                    self.command
                ))
                .stating(format_args!(
                    "this file is a module of `{}`",
                    home.executable
                ))
                .trying(format!("curios {} {}", self.command, home.executable))
                .about_path(module));
        }

        if self.product.filed_under_a_package() && program.home().is_none() && output.is_none() {
            return Err(homeless(self.command, program.entry()));
        }

        Ok(program)
    }

    /// What `subject` selects for a rung a unit has: a program as [`Contract::admit_program`] admits one, or the library a file of one selects.
    pub fn admit_staged(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Staged, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::ProgramOrLibrary, "{}", self.command);

        Ok(
            match self.selection(subject, manifest, directory, cabinet, stdin)? {
                Selection::Program(program) => Staged::Program(program),
                Selection::Entire(entire) => Staged::Program(entire.default_program()?),
                Selection::Library(library) => Staged::Library(library),
                Selection::Module(module) => Staged::Module(module),
            },
        )
    }

    /// The scope `subject` asks in: what [`Contract::admit_staged`] admits, but for no subject, which asks in the governing package's sole or `default` executable's scope, else its library's, and outside every package the prelude's alone.
    pub fn admit_scope(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Scope, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::Scope, "{}", self.command);

        if subject.is_none() && manifest.is_none() && nearest_manifest(directory).is_none() {
            Self::stdin_written_in(stdin.core, subject)?;
            return Ok(Scope::Prelude);
        }

        Ok(
            match self.selection(subject, manifest, directory, cabinet, stdin)? {
                Selection::Program(program) => Scope::Program(program),
                Selection::Library(library) => Scope::Library(library),
                Selection::Module(module) => Scope::Module(module),
                Selection::Entire(entire) => match (entire.default_program(), entire.library()?) {
                    (Ok(program), _) => Scope::Program(program),
                    (Err(_), Some(library)) => Scope::Library(library),
                    (Err(refused), None) => return Err(refused),
                },
            },
        )
    }

    /// The library `subject` selects: a library, or the package entire's own.
    pub fn admit_library(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Library, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::Library, "{}", self.command);

        let refused = Refused::of("not a library")
            .stating(format_args!("`{}` takes a library", self.command));

        match self.selection(subject, manifest, directory, cabinet, stdin)? {
            Selection::Library(library) => Ok(library),
            Selection::Entire(entire) => entire.library()?.ok_or_else(|| {
                Refused::of("no library")
                    .stating(format_args!("`{}` takes a library", self.command))
                    .stating(format_args!(
                        "`{}` declares none",
                        entire.governing.package.name
                    ))
                    .about_path(&entire.governing.manifest)
            }),
            Selection::Program(program) => {
                let refused = refused.stating("a program is not one");

                Err(match program.entry() {
                    Entry::File(path) => refused.about_path(path),
                    Entry::Stdin(_) => refused.about_named(STDIN_NAME),
                })
            }
            // A module no package declares is a unit with no package to document it under: a `mod` line in a library's header makes it one of that library's modules, which standard input's text can never be.
            Selection::Module(module) => Err(match module.entry() {
                Entry::File(path) => refused
                    .stating("this file is a module no package declares")
                    .stating("a `mod` line in a package's `lib.crs` puts it in that library")
                    .about_path(path),
                Entry::Stdin(_) => refused
                    .stating("this text is a module no package declares")
                    .about_named(STDIN_NAME),
            }),
        }
    }

    /// Whatever `subject` selects.
    pub fn admit_any(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Selection, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::Any, "{}", self.command);

        self.selection(subject, manifest, directory, cabinet, stdin)
    }

    /// The governing package entire, which is all a command taking no subject selects.
    pub fn admit_entire(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Entire, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::Entire, "{}", self.command);

        match self.selection(subject, manifest, directory, cabinet, stdin)? {
            Selection::Entire(entire) => Ok(*entire),
            Selection::Library(_) | Selection::Program(_) | Selection::Module(_) => {
                Err(Refused::of("surplus subject")
                    .stating(format_args!(
                        "`{}` takes the governing package entire, and no subject",
                        self.command
                    ))
                    .about_named(subject.unwrap_or_default()))
            }
        }
    }

    /// What `subject` rewrites, and within which format: every file a package or program it names declares, a file itself whatever declares it — formatting reads no unit, so placement is not its question — or what standard input holds. A file takes the format of the package that governs it, and `settings`, the command line's, reach only a file no package governs and standard input ([`FormatSettings::format_under`]).
    /// Standard input is read as Core text where `stdin` says so, which is refused beside any other subject.
    pub fn admit_files(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        settings: &FormatSettings,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Rewritten, Box<Report>> {
        debug_assert_eq!(self.accepts, Accepts::Files, "{}", self.command);

        self.rewritten(subject, manifest, directory, settings, cabinet, stdin)
    }

    fn rewritten(
        &self,
        subject: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
        settings: &FormatSettings,
        cabinet: Option<&Path>,
        stdin: &StandardInput,
    ) -> Result<Rewritten, Box<Report>> {
        Ok(match Spelling::of(subject) {
            Spelling::File(path) => {
                Self::stdin_written_in(stdin.core, subject)?;
                let path = file_subject(path)?;
                let format = settings.format_of_file(&path, manifest, cabinet)?;
                Rewritten::Files(vec![path], format)
            }
            _ => match self.selection(subject, manifest, directory, cabinet, stdin)? {
                Selection::Program(ref program) if let Entry::Stdin(supplied) = program.entry() => {
                    Rewritten::Stdin(settings.format_under(None)?, supplied.clone())
                }
                Selection::Entire(entire) => {
                    let format = settings.format_under(Some(&entire.governing))?;
                    Rewritten::Files(entire.declared_files()?, format)
                }
                // Selected through the package `selection` found, which a program and a library do not keep.
                Selection::Library(library) => {
                    let governing = Governing::found(manifest, directory, cabinet)?;
                    let format = settings.format_under(Some(&governing))?;
                    Rewritten::Files(library.declared_files()?, format)
                }
                Selection::Program(program) => {
                    let governing = Governing::found(manifest, directory, cabinet)?;
                    let format = settings.format_under(Some(&governing))?;
                    Rewritten::Files(program.declared_files()?, format)
                }
                // A loose module declares no file but its own, formatted as the file it is.
                Selection::Module(module) => match module.entry() {
                    Entry::Stdin(supplied) => {
                        Rewritten::Stdin(settings.format_under(None)?, supplied.clone())
                    }
                    Entry::File(path) => Rewritten::Files(
                        vec![path.clone()],
                        settings.format_of_file(path, manifest, cabinet)?,
                    ),
                },
            },
        })
    }
}

/// What a command that builds says about a program no package declares and nothing said where to put: that it has nowhere, and the command line that gives it somewhere.
fn homeless(command: &str, entry: &Entry) -> Box<Report> {
    let refused = |what: &str| {
        Refused::of("missing output")
            .stating(format_args!(
                "`{command}` files what it builds under the package that declares it"
            ))
            .stating(format_args!("{what} has none"))
    };

    match entry {
        Entry::Stdin(_) => refused("standard input")
            .trying(format!("curios {command} - --output program"))
            .about_named(STDIN_NAME),
        Entry::File(path) => {
            let named = path
                .file_stem()
                .map_or_else(|| "program".into(), |stem| stem.to_string_lossy());

            refused("this file")
                .trying(format!(
                    "curios {command} {} --output {named}",
                    path.display()
                ))
                .about_path(path)
        }
    }
}

/// Where the process stands, which is where an invocation's argument is resolved from.
pub fn here() -> Result<PathBuf, Box<Report>> {
    env::current_dir().map_err(|error| {
        Refused::of("unreadable directory")
            .stating("the working directory could not be read")
            .stating(OsError(&error))
            .about_named("the working directory")
    })
}
