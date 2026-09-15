//! What an invocation is about: every command's argument, resolved once.
//!
//! **A spelling means one thing.** `-` is standard input, an argument ending in `.crs` or holding a path separator is a file, any other argument is an executable's name, and no argument at all is the governing package entire. The rule is lexical and probes no disk — an executable's name is a single identifier, so it can hold neither `.crs` nor a separator nor be `-`, and the spaces cannot overlap — and it is spelled here once, so no two commands can come to read one argument two ways.
//!
//! That is also why `-` is spelled at all, rather than a bare invocation reading standard input when one is piped in. No argument already means the governing package, and deciding between the two would mean asking whether a terminal is attached — which makes one command line mean different things in a shell and in a pipeline.
//!
//! **What is selected is one of three things.** The package entire — its library and every program it declares, which a command that needs one program narrows to the sole or `default` one; a library; or a program. A program is *declared*, with a home in its package's store and the units it is compiled against, or *loose*, with neither: a loose program is compiled against the prelude and nothing else, and the type has no field that could carry more.
//!
//! **A file is placed by what declares it, never by where it sits** (law 1). It is an executable's when it is that executable's entry or a module a `mod` chain from the entry reaches, the library's when a chain from `lib.crs` reaches it, and loose otherwise — carrying, when a package's directory holds it, why it is in none of that package's units and what would put it in one, so a question can say so before it answers. Every command places a file this one way; what a command then does with a program or a library is its contract's to decide.

#[cfg(test)]
mod tests;

use {
    crate::{
        EXECUTABLE, EXTENSION, Executable, Governing, LIBRARY, MANIFEST, Manifest, Package,
        module_of, nearest_manifest, order, reachable, stem_module_of,
    },
    curios_text::{Entrypoint, Overlay, RootSource, identity},
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
    /// What `spelling` selects, under the manifest `manifest` names or the one nearest `directory`, reading `overlay`'s text before the disk for every header a file's placement walks.
    pub fn of(
        spelling: Spelling,
        manifest: Option<&Path>,
        directory: &Path,
        overlay: &Overlay,
    ) -> Result<Self, String> {
        Ok(match spelling {
            // Answered before anything looks for a manifest, which is also why no manifest can govern it.
            Spelling::Stdin => match manifest {
                Some(manifest) => {
                    return Err(format!(
                        "`-` is standard input, which no manifest governs: drop `--manifest {}`",
                        manifest.display()
                    ));
                }
                None => Self::Program(Program::loose(Entry::Stdin, None)),
            },
            Spelling::File(path) => placed(path, manifest, overlay)?,
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
            package: self.governing.package.name.clone(),
            root: self.governing.root.clone(),
            manifest: self.governing.manifest.clone(),
            units: order(&self.governing)?,
            through: None,
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

    /// Every file the package is written in: its library's, then each program's, in declaration order.
    pub fn declared_files(&self) -> Result<Vec<PathBuf>, String> {
        let mut files = match self.library()? {
            Some(library) => library.declared_files()?,
            None => Vec::new(),
        };

        for program in self.programs()? {
            files.extend(program.declared_files()?);
        }

        Ok(files)
    }
}

/// A package's library, compiled against everything it depends on.
pub struct Library {
    /// The package whose library it is, which is what its pages are filed under.
    pub package: String,
    /// The governing root, which is where the store sits.
    pub root: PathBuf,
    /// The manifest that declares it, beside which its header sits — and what a report names when the invocation stands somewhere else.
    pub manifest: PathBuf,
    /// The whole scope in dependency order, the library last.
    pub units: Vec<RootSource>,
    /// The file the library was selected through, when it was selected through one.
    pub through: Option<PathBuf>,
}

impl Library {
    /// Every file the library is written in: its header, and every file module its `mod` lines reach.
    pub fn declared_files(&self) -> Result<Vec<PathBuf>, String> {
        self.units.last().map_or(Ok(Vec::new()), |unit| {
            unit.declared_files().map_err(|error| error.to_string())
        })
    }
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
    unlinked: Option<Unlinked>,
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
    /// The manifest that declares it, for a report to name when the invocation stands somewhere else.
    pub manifest: PathBuf,
    /// The package that declares it. Carried beside `executable` because the two together are the one identity in a compilation that cannot collide, which is what the store addresses a built artifact by; neither alone will do, since an umbrella's members may each declare a `serve`.
    pub package: String,
    /// The executable's declared name.
    pub executable: String,
    /// Where a native build of it is written, under the governing root's store.
    pub output: PathBuf,
}

/// Why a file a package's directory holds is in none of its units: what a question says before it answers about the file on its own.
#[derive(Debug, Clone)]
pub struct Unlinked {
    /// The file, as it was asked about.
    pub file: PathBuf,
    /// What to say about it, and what would put it in a unit.
    pub message: String,
}

impl Program {
    /// A program compiled against the prelude alone, carrying why a package that holds its file declares it nowhere.
    fn loose(entry: Entry, unlinked: Option<Unlinked>) -> Self {
        Self {
            entry,
            scope: Scope::Loose,
            through: None,
            unlinked,
        }
    }

    /// This program, selected through `file`, one of its modules.
    fn selected_through(self, file: PathBuf) -> Self {
        Self {
            through: Some(file),
            ..self
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

    /// Why its file is in none of the units of the package that holds it — `None` for a declared program, and for a loose one no package holds.
    pub fn unlinked(&self) -> Option<&Unlinked> {
        self.unlinked.as_ref()
    }

    /// The units it is compiled against, predecessors first — none for a loose program.
    pub fn into_units(self) -> Vec<RootSource> {
        match self.scope {
            Scope::Loose => Vec::new(),
            Scope::Declared { units, .. } => units,
        }
    }

    /// Every file the program is written in: its entry, and every file module its entry's `mod` lines reach — none for a program on standard input.
    pub fn declared_files(&self) -> Result<Vec<PathBuf>, String> {
        let Entry::File(entry) = &self.entry else {
            return Ok(Vec::new());
        };
        let (entrypoint, loader, _) = Entrypoint::opened(entry).map_err(|error| error.format())?;

        let mut files = vec![entry.clone()];
        files.extend(
            loader
                .entry_declared_files(&entrypoint.module.items)
                .map_err(|error| error.to_string())?,
        );

        Ok(files)
    }
}

/// `executable`, declared by the package `governing` governs, and everything needed to compile it.
fn program(governing: &Governing, executable: &Executable) -> Result<Program, String> {
    Ok(Program {
        entry: Entry::File(governing.directory.join(&executable.path)),
        scope: Scope::Declared {
            home: Home {
                root: governing.root.clone(),
                manifest: governing.manifest.clone(),
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
        unlinked: None,
    })
}

/// What a file whose spelling names no module is told.
const UNSPELLED: &str = "its spelling names no module a `mod` could declare";

/// `file`, placed in the unit whose `mod` chain declares it, and loose when none does.
fn placed(file: PathBuf, manifest: Option<&Path>, overlay: &Overlay) -> Result<Selection, String> {
    let spelled = identity(&file);

    let governing = match manifest {
        Some(named) => {
            let governing = Governing::at(named)?;
            if !spelled.starts_with(identity(&governing.directory)) {
                return Err(format!(
                    "{} is outside the package {} declares, so `--manifest` cannot place it",
                    file.display(),
                    named.display()
                ));
            }
            governing
        }
        // An umbrella found first declares no unit, so nothing declares the file — exactly as when no manifest is above it at all.
        None => match spelled.parent().and_then(nearest_manifest) {
            Some(at) => match Manifest::from_path(&at.join(MANIFEST))? {
                Manifest::Package(_) => Governing::of(&at)?,
                Manifest::Umbrella(_) => return Ok(loose(file)),
            },
            None => return Ok(loose(file)),
        },
    };
    let directory = identity(&governing.directory);

    // An executable's own entry, or a file under its stem directory, which only a `mod` chain from that entry can reach: a row's stem is its own, as the layout rule gives a header's stem directory to that header.
    for executable in &governing.package.executables {
        let entry = identity(&governing.directory.join(&executable.path));
        if spelled == entry {
            return Ok(Selection::Program(program(&governing, executable)?));
        }

        let stem = entry.with_extension("");
        if !spelled.starts_with(&stem) {
            continue;
        }

        let Some(module) = stem_module_of(&stem, &spelled) else {
            return Ok(unlinked(file, &governing, UNSPELLED.to_string()));
        };
        if !entry_declares(&entry, &module, overlay) {
            let remedy = declaration(module.segments(), &entry, &stem, &directory);
            return Ok(unlinked(file, &governing, remedy));
        }

        return Ok(Selection::Program(
            program(&governing, executable)?.selected_through(file),
        ));
    }

    // Everything else the package's directory holds is its library's to declare.
    if !governing.directory.join(LIBRARY).is_file() {
        let remedy = format!(
            "`/{}` has no library to declare it in",
            governing.package.name
        );
        return Ok(unlinked(file, &governing, remedy));
    }
    let Some(module) = module_of(&governing.package, &directory, &spelled) else {
        return Ok(unlinked(file, &governing, UNSPELLED.to_string()));
    };

    let mut units = order(&governing)?;
    let library = units
        .pop()
        .expect("a package with a library compiles it last")
        .with_overlay(overlay.clone());
    // A header on the chain that cannot be read places the file in the library anyway: the compilation reports that fault on its own account.
    if !library.declares_module(&module).unwrap_or(true) {
        let remedy = declaration(
            &module.segments()[1..],
            &directory.join(LIBRARY),
            &directory,
            &directory,
        );
        return Ok(unlinked(file, &governing, remedy));
    }
    units.push(library);

    Ok(Selection::Library(Library {
        package: governing.package.name.clone(),
        root: governing.root.clone(),
        manifest: governing.manifest.clone(),
        units,
        through: Some(file),
    }))
}

/// `file`, loose, with no package to say anything about it.
fn loose(file: PathBuf) -> Selection {
    Selection::Program(Program::loose(Entry::File(file), None))
}

/// `file`, loose although `governing`'s package holds it, carrying `remedy` — what would put it in a unit.
fn unlinked(file: PathBuf, governing: &Governing, remedy: String) -> Selection {
    let unlinked = Unlinked {
        message: format!(
            "{} is in no unit of `/{}`, so it was checked on its own against `/std`: {remedy}",
            file.display(),
            governing.package.name
        ),
        file: file.clone(),
    };

    Selection::Program(Program::loose(Entry::File(file), Some(unlinked)))
}

/// The `mod` line that declares a module `below` a header's root, and the file it goes in: `header` itself for a child of the root, and otherwise the file its parent module is read from under `namespace` — named from `package`, the directory its reader works in.
fn declaration(below: &[String], header: &Path, namespace: &Path, package: &Path) -> String {
    let (label, parents) = below
        .split_last()
        .expect("a module below the root it is declared from");
    let file = match parents.split_last() {
        None => header.to_path_buf(),
        Some((parent, above)) => above
            .iter()
            .fold(namespace.to_path_buf(), |path, segment| path.join(segment))
            .join(format!("{parent}.{EXTENSION}")),
    };

    format!(
        "declare it with `mod {label};` in {}",
        file.strip_prefix(package).unwrap_or(&file).display()
    )
}

/// Whether a `mod` chain from `entry` reaches `module`, reading `overlay`'s text before the disk. An entry that cannot be read or parsed places the file in its program anyway: the compilation reports that fault on its own account, and a file that only looks unlinked because its entry is broken is no finding.
fn entry_declares(entry: &Path, module: &Qualifier, overlay: &Overlay) -> bool {
    let opened = match overlay.get(entry) {
        Some(text) => Entrypoint::overlaid(entry, text).ok(),
        None => Entrypoint::opened(entry).ok(),
    };
    let Some((entrypoint, loader, _)) = opened else {
        return true;
    };

    loader
        .with_overlay(overlay.clone())
        .entry_declares(&entrypoint.module.items, module)
        .unwrap_or(true)
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
