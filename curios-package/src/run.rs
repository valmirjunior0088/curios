//! What `curios run` was asked to run.
//!
//! **Dispatch is lexical, never probed.** `-` is standard input; an argument ending in `.crs` or holding a path separator is a file; anything else is an executable name. The three spaces cannot overlap, because an executable's name is a single identifier and `-` is not one — so no reserved names, no magic filenames, and no case where the answer depends on what happens to exist on disk, or on what happens to be attached to a descriptor.
//!
//! That last clause is why `-` is spelled at all, rather than a bare invocation reading standard input when one is piped in. A bare invocation already means the governing package's default executable, and deciding between the two would mean asking whether a terminal is attached — which makes one command line mean different things in a shell and in a pipeline, and leaves `curios run < input.txt` compiling the input it was meant to be fed.
//!
//! **Neither a file argument nor `-` triggers a walk at all.** Both compile standalone *everywhere*, with no manifest, no dependencies and no project, which is what makes project scope reachable only through a declared artifact. The scratch-file hazard is not mitigated but unconstructible, at the price of one `[[executables]]` line when a scratch program wants the library.

#[cfg(test)]
mod tests;

use {
    crate::{Executable, Governing, Package, order},
    curios_text::RootSource,
    std::path::{Path, PathBuf},
};

/// What an invocation resolved to.
pub enum Target {
    /// The program on standard input, standalone for the same reason a file argument is — and anonymous besides, so it has neither an entry path nor a name to write an executable to.
    Stdin,
    /// A bare file, standalone: no manifest captures a file argument.
    File(PathBuf),
    /// A declared executable of the governing package, and the units it is compiled against — its own library last, everything it depends on before that.
    Executable {
        name: String,
        /// The package that declares it. Carried beside `name` because the two together are the one identity in a compilation that cannot collide, which is what the store addresses a built artifact by; neither alone will do, since an umbrella's members may each declare a `serve`.
        package: String,
        entry: PathBuf,
        /// Where a native build of it is written, under the governing root's store.
        output: PathBuf,
        /// The governing root, which is where the store sits. Carried because what a compilation may reuse is a fact about the project it is in, and only the walk knows which project that is.
        root: PathBuf,
        units: Vec<RootSource>,
    },
}

/// What a TARGET argument lexically names, decided before anything looks at a manifest or the disk — the one spelling of the rule the module documentation states, which `run` and every `wonder` query dispatch on. `compile` dispatches on it too and then admits the named form alone: a product written to disk needs a package to be filed under, and only a declared executable has one.
pub enum Form {
    /// `-`: the program on standard input.
    Stdin,
    /// A path, by its `.crs` suffix or a separator.
    File(PathBuf),
    /// An executable's name, or nothing. What nothing means is the caller's to decide — the governing package's sole or `default` executable for `run`, the package entire for a question — and it is the one place the callers legitimately differ.
    Named(Option<String>),
}

impl Form {
    /// The form `argument` takes, by spelling alone.
    pub fn of(argument: Option<&str>) -> Self {
        match argument {
            Some(Target::STDIN) => Self::Stdin,
            Some(argument) if names_a_file(argument) => Self::File(PathBuf::from(argument)),
            argument => Self::Named(argument.map(str::to_string)),
        }
    }
}

impl Target {
    /// How standard input is asked for. Not an identifier, so it cannot collide with an executable's name, and not path-shaped, so it cannot collide with a file's.
    pub const STDIN: &'static str = "-";

    /// The `.crs` file this target is compiled from, and `None` for a program that is not in a file.
    pub fn entry(&self) -> Option<&Path> {
        match self {
            Self::Stdin => None,
            Self::File(path) => Some(path),
            Self::Executable { entry, .. } => Some(entry),
        }
    }

    /// What `argument` names, and everything needed to compile it.
    ///
    /// `manifest` is the explicit override for scripting; without one the governing package is the one `directory`'s own manifest declares.
    pub fn of(
        argument: Option<&str>,
        manifest: Option<&Path>,
        directory: &Path,
    ) -> Result<Self, String> {
        // Both standalone forms answer here, before anything looks for a manifest — which is what "triggers no walk at all" means operationally.
        let name = match Form::of(argument) {
            Form::Stdin => return Ok(Self::Stdin),
            Form::File(path) => return Ok(Self::File(path)),
            Form::Named(name) => name,
        };

        let governing = Governing::found(manifest, directory)?;

        let executable = match &name {
            Some(name) => named(&governing.package, name)?,
            None => sole(&governing.package)?,
        };

        Ok(Self::Executable {
            output: governing
                .store()
                .executable(&governing.package.name, &executable.name),
            root: governing.root.clone(),
            name: executable.name.clone(),
            package: governing.package.name.clone(),
            entry: governing.directory.join(&executable.path),
            units: order(&governing)?,
        })
    }

    /// What `argument` names, resolved against whatever governs the directory the process is standing in.
    pub fn here(argument: Option<&str>, manifest: Option<&Path>) -> Result<Self, String> {
        let directory = std::env::current_dir().map_err(|error| error.to_string())?;

        Self::of(argument, manifest, &directory)
    }
}

/// Whether `argument` names a file rather than an executable.
///
/// Both separators, not the platform's: an executable's name is an identifier, so neither can appear in one, and a path written with the other spelling is still plainly a path.
///
/// Reached through [`Form::of`], which is how `curios wonder` resolves the same argument the same way — a second spelling of one lexical rule is one the subcommands can come to disagree about. Named for what it decides rather than `is_file`, which at this altitude reads as a question about the disk — and this asks nothing of it, which is the module's first sentence.
fn names_a_file(argument: &str) -> bool {
    argument.ends_with(".crs") || argument.contains('/') || argument.contains('\\')
}

/// The executable `name` names.
fn named<'a>(package: &'a Package, name: &str) -> Result<&'a Executable, String> {
    package
        .executables
        .iter()
        .find(|executable| executable.name == name)
        .ok_or_else(|| {
            format!(
                "{:?} declares no executable named {name:?}{}",
                package.name,
                candidates(package)
            )
        })
}

/// The executable a bare `curios run` means: the sole one, or the one `default` names.
fn sole(package: &Package) -> Result<&Executable, String> {
    if let Some(default) = &package.default {
        return named(package, default);
    }

    match package.executables.as_slice() {
        [executable] => Ok(executable),
        [] => Err(format!(
            "{:?} declares no executable; `run` a `.crs` file by name, or declare one with `[[executables]]`",
            package.name
        )),
        _ => Err(format!(
            "{:?} declares more than one executable and no `default`, so a bare `run` means nothing in particular{}",
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
                .map(|executable| format!("{:?}", executable.name))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}
