//! What `curios run` was asked to run.
//!
//! **Dispatch is lexical, never probed.** An argument ending in `.crs` or holding a path separator is a file; anything else is an executable name. The two spaces cannot overlap, because an executable's name is a single identifier — so no reserved names, no magic filenames, and no case where the answer depends on what happens to exist on disk.
//!
//! **A file argument triggers no walk at all.** A bare `.crs` file compiles standalone *everywhere*, with no manifest, no dependencies and no project, which is what makes project scope reachable only through a declared artifact. The scratch-file hazard is not mitigated but unconstructible, at the price of one `[[executables]]` line when a scratch program wants the library.

#[cfg(test)]
mod tests;

use {
    crate::{Executable, Governing, Package, order},
    curios_text::RootSource,
    std::path::{Path, PathBuf},
};

/// What an invocation resolved to.
pub enum Target {
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

impl Target {
    /// The `.crs` file this target is compiled from.
    pub fn entry(&self) -> &Path {
        match self {
            Self::File(path) => path,
            Self::Executable { entry, .. } => entry,
        }
    }

    /// Where a native build of this target is written by default.
    ///
    /// A declared executable goes into the governing root's store, nested under the package that declares it. A bare file has no project, hence no governing root and no store, so it lands beside the working directory under its own stem — the same declared-artifact-versus-bare-file split as everywhere else.
    pub fn output(&self) -> PathBuf {
        match self {
            Self::File(path) => PathBuf::from(path.file_stem().unwrap_or(path.as_os_str())),
            Self::Executable { output, .. } => output.clone(),
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
        if let Some(argument) = argument
            && is_file(argument)
        {
            return Ok(Self::File(PathBuf::from(argument)));
        }

        let governing = Governing::found(manifest, directory)?;

        let executable = match argument {
            Some(name) => named(&governing.package, name)?,
            None => sole(&governing.package)?,
        };

        Ok(Self::Executable {
            output: governing
                .store()
                .bin(&governing.package.name, &executable.name),
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
fn is_file(argument: &str) -> bool {
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
