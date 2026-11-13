//! The compiler a package is written for: the version it declares, or inherits from the umbrella that enumerates it.

#[cfg(test)]
mod tests;

use {
    crate::{MANIFEST, Package, Refused, Umbrella},
    curios_common::Report,
    semver::Version,
    std::{
        fmt,
        path::{Path, PathBuf},
        str::FromStr,
    },
};

/// A Curios compiler version, compared by SemVer precedence, so build metadata is no part of it and a pre-release is a version of its own.
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

    /// Refuse every compiler but this one before a package is compiled or its cached executable is selected, at the `compiler` of `manifest`, which states this version.
    fn require_current(&self, manifest: &Path) -> Result<(), Box<Report>> {
        let current = Self::current();
        if current.0.cmp_precedence(&self.0).is_ne() {
            return Err(Refused::of("compiler mismatch")
                .stating(format_args!("this package requires Curios {self}"))
                .stating(format_args!("this compiler is Curios {current}"))
                .value(["compiler"])
                .located_at(manifest));
        }
        Ok(())
    }
}

impl FromStr for CompilerVersion {
    type Err = Refused;

    fn from_str(spelling: &str) -> Result<Self, Refused> {
        spelling.parse().map(Self).map_err(|error| {
            Refused::of("invalid version")
                .stating(format_args!("written: {spelling:?}"))
                .stating(format_args!(
                    "`compiler` is a bare SemVer version such as \"{}\", not a range",
                    Self::current()
                ))
                .stating(error)
                .value(["compiler"])
        })
    }
}

impl fmt::Display for CompilerVersion {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

/// Membership chooses the one owner of the requirement: an enumerating umbrella, or the standalone package itself.
pub fn require_compiler(
    package: &Package,
    directory: &Path,
    umbrella: Option<(&Umbrella, &Path)>,
) -> Result<(), Box<Report>> {
    let (stated, owner) = compiler_requirement(package, directory, umbrella)?;
    stated.require_current(&owner)
}

/// Resolve the declaration before comparing versions: file placement may discover that a nearby package does not own the file at all.
pub fn compiler_requirement<'a>(
    package: &'a Package,
    directory: &Path,
    umbrella: Option<(&'a Umbrella, &Path)>,
) -> Result<(&'a CompilerVersion, PathBuf), Box<Report>> {
    let manifest = directory.join(MANIFEST);
    match umbrella {
        Some((umbrella, root)) => {
            let owner = root.join(MANIFEST);
            if package.compiler.is_some() {
                return Err(Refused::of("duplicate setting")
                    .stating(format_args!(
                        "`compiler` is inherited from the umbrella at {}",
                        owner.display()
                    ))
                    .stating("remove the local field")
                    .entry(["compiler"])
                    .located_at(&manifest));
            }
            Ok((&umbrella.compiler, owner))
        }
        None => Ok((
            package.compiler.as_ref().ok_or_else(|| {
                Refused::of("missing compiler version")
                    .stating("this package states no `compiler`")
                    .stating(format_args!(
                        "a standalone package states one, as `compiler = \"{}\"`",
                        CompilerVersion::current()
                    ))
                    .about_path(&manifest)
            })?,
            manifest,
        )),
    }
}
