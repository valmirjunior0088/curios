//! The compiler a package is written for: the version it declares, exactly or as the oldest it accepts, or inherits from the umbrella that enumerates it.

#[cfg(test)]
mod tests;

use {
    crate::{MANIFEST, Package, Refused, Umbrella},
    curios_common::{Code, Report},
    semver::Version,
    std::{
        fmt,
        path::{Path, PathBuf},
        str::FromStr,
    },
};

/// The Curios compiler a package states: one version, or that version and every later one where it is written after `>=`. Versions are compared by SemVer precedence, so build metadata is no part of one and a pre-release is a version of its own.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerVersion {
    version: Version,
    /// Whether every later version is accepted too, which `>=` says.
    minimum: bool,
}

/// What a minimum is written after.
const MINIMUM: &str = ">=";

impl CompilerVersion {
    /// The version this compiler's package declares at build time, stated exactly.
    pub fn current() -> Self {
        Self {
            version: env!("CARGO_PKG_VERSION")
                .parse()
                .expect("Cargo's package version is SemVer"),
            minimum: false,
        }
    }

    /// Refuse every compiler this statement does not accept before a package is compiled or its cached executable is selected, at the `compiler` of `manifest`, which states it: every compiler but the one version, or every one older than it where it is a minimum.
    fn require_current(&self, manifest: &Path) -> Result<(), Box<Report>> {
        let current = Self::current();
        let running = current.version.cmp_precedence(&self.version);
        let accepted = match self.minimum {
            true => running.is_ge(),
            false => running.is_eq(),
        };
        if !accepted {
            let required = match self.minimum {
                true => format!(
                    "this package requires Curios {} or later",
                    Code(&self.version)
                ),
                false => format!("this package requires Curios {}", Code(&self.version)),
            };
            return Err(Refused::of("compiler mismatch")
                .stating(required)
                .stating(format_args!("this compiler is Curios `{current}`"))
                .value(["compiler"])
                .located_at(manifest));
        }
        Ok(())
    }
}

impl FromStr for CompilerVersion {
    type Err = Refused;

    fn from_str(spelling: &str) -> Result<Self, Refused> {
        let (minimum, version) = match spelling.strip_prefix(MINIMUM) {
            Some(version) => (true, version.trim_start()),
            None => (false, spelling),
        };
        version
            .parse()
            .map(|version| Self { version, minimum })
            .map_err(|error| {
                Refused::of("invalid version")
                    .stating(format_args!("written: {}", Code(format!("{spelling:?}"))))
                    .stating(format_args!(
                        "`compiler` is a SemVer version such as `\"{current}\"`, or a minimum, `\"{MINIMUM} {current}\"`",
                        current = Self::current()
                    ))
                    .stating(error)
                    .value(["compiler"])
            })
    }
}

/// As a manifest writes it: the version, after `>=` where it is a minimum.
impl fmt::Display for CompilerVersion {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.minimum {
            true => write!(formatter, "{MINIMUM} {}", self.version),
            false => self.version.fmt(formatter),
        }
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
