//! The minimum compiler a package declares, or inherits from the umbrella that enumerates it.

#[cfg(test)]
mod tests;

use {
    crate::{MANIFEST, Package, Umbrella},
    semver::Version,
    std::{
        fmt,
        path::{Path, PathBuf},
        str::FromStr,
    },
};

/// A minimum Curios compiler version, compared by SemVer precedence rather than by build metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerVersion(Version);

impl CompilerVersion {
    /// The version this compiler's package declares at build time.
    pub fn current() -> Self {
        Self(
            env!("CARGO_PKG_VERSION")
                .parse()
                .expect("Cargo's package version is SemVer"),
        )
    }

    /// Refuse an older compiler before a package is compiled or its cached executable is selected.
    fn require_current(&self, manifest: &Path) -> Result<(), String> {
        let current = Self::current();
        if current.0.cmp_precedence(&self.0).is_lt() {
            return Err(format!(
                "{} requires Curios {self} or newer; this compiler is Curios {current}",
                manifest.display()
            ));
        }
        Ok(())
    }
}

impl FromStr for CompilerVersion {
    type Err = String;

    fn from_str(spelling: &str) -> Result<Self, String> {
        spelling.parse().map(Self).map_err(|error| {
            format!("`compiler` must be a bare SemVer version such as \"{}\", not a range: {spelling:?}: {error}", Self::current())
        })
    }
}

impl fmt::Display for CompilerVersion {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

/// Membership chooses the one owner of the requirement: an enumerating umbrella, or the standalone package itself.
pub(crate) fn require_compiler(
    package: &Package,
    directory: &Path,
    umbrella: Option<(&Umbrella, &Path)>,
) -> Result<(), String> {
    let (minimum, owner) = compiler_requirement(package, directory, umbrella)?;
    minimum.require_current(&owner)
}

/// Resolve the declaration before comparing versions: file placement may discover that a nearby package does not own the file at all.
pub(crate) fn compiler_requirement<'a>(
    package: &'a Package,
    directory: &Path,
    umbrella: Option<(&'a Umbrella, &Path)>,
) -> Result<(&'a CompilerVersion, PathBuf), String> {
    let manifest = directory.join(MANIFEST);
    match umbrella {
        Some((umbrella, root)) => {
            let owner = root.join(MANIFEST);
            if package.compiler.is_some() {
                return Err(format!(
                    "{} inherits `compiler` from the umbrella at {}; remove its local `compiler` field",
                    manifest.display(), owner.display()
                ));
            }
            Ok((&umbrella.compiler, owner))
        }
        None => Ok((package.compiler.as_ref().ok_or_else(|| {
            format!(
                "{} declares no minimum compiler version; a standalone package must state `compiler = \"{}\"`",
                manifest.display(), CompilerVersion::current()
            )
        })?, manifest)),
    }
}
