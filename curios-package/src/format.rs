//! What `[format]` and `curios format`'s flags state, and the one place a file's format is read from.
//!
//! **Each setting is declared once, as a field**: its key under `[format]` is the field's name as serde reads it, and its flag the same name as clap reads it, so no setting has a key without a flag or a flag without a key, and `format/tests.rs` holds the two spellings equal.
//!
//! **A file's format has one source.** Where a package governs the file, it is that package's `[format]`, or its enumerating umbrella's, which a member does not restate, as it does not restate `compiler`; where none governs it — standard input, a file with no package's manifest above it — it is the command line's. A flag where a package governs is refused rather than ranked against the manifest, so the command line, a check and an editor's server format a file one way. The decision is `documentation/design/tools/a-file-is-formatted-at-the-width-one-place-states.md`'s.

#[cfg(test)]
mod tests;

use {
    crate::{Governing, MANIFEST, Package, Umbrella},
    clap::Args,
    curios_common::LINE_WIDTH,
    curios_surface::{NARROWEST, Width},
    serde::{Deserialize, Deserializer, de::Error},
    std::path::{Path, PathBuf},
};

/// What a `[format]` table states, and what `curios format`'s flags state of a file no package governs: every setting optional, and absent where it is its default.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Args)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct FormatSettings {
    /// The columns a formatted line is written within.
    #[arg(long, value_name = "COLUMNS", help = width_help())]
    #[serde(default, deserialize_with = "width")]
    pub width: Option<Width>,
}

impl FormatSettings {
    /// The width a file is formatted within, from the one place that states it: `governing`'s `[format]` where a package governs the file, and these settings, the command line's, where none does. A setting stated here where a package governs is refused, naming the manifest that owns the package's format.
    pub fn width_under(&self, governing: Option<&Governing>) -> Result<Width, String> {
        let settings = match governing {
            None => self.clone(),
            Some(governing) => {
                let (stated, owner) = governing.format()?;
                if *self != Self::default() {
                    return Err(format!(
                        "the package `{}` takes its format from {}, which a flag does not override: write the setting under its `[format]` table, or format the text through standard input, which no manifest governs",
                        governing.package.name,
                        owner.display()
                    ));
                }
                stated.cloned().unwrap_or_default()
            }
        };

        // Every setting is named here, so one added to the struct does not compile until it is read.
        let Self { width } = settings;
        Ok(width.unwrap_or_default())
    }

    /// [`FormatSettings::width_under`] for `file`: under the package `manifest` names or the one whose manifest is nearest above it, whose compiler minimum the running compiler must meet, and under nothing where no package governs it.
    pub fn width_of_file(&self, file: &Path, manifest: Option<&Path>) -> Result<Width, String> {
        let governing = Governing::of_file(file, manifest)?;
        if let Some(governing) = &governing {
            governing.check_compiler()?;
        }

        self.width_under(governing.as_ref())
    }
}

/// `--width`'s help, its bounds read off the constants that set them.
fn width_help() -> String {
    format!(
        "The columns a formatted line is written within, {NARROWEST} through {LINE_WIDTH}, for standard input or a file no package governs; a package states its own under `[format]` (default: {LINE_WIDTH})"
    )
}

/// `width` as a manifest spells it: a whole number of columns, refused where the formatter admits no such width.
fn width<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Option<Width>, D::Error> {
    let columns = u64::deserialize(deserializer)?;
    let columns = usize::try_from(columns).map_err(D::Error::custom)?;

    Width::new(columns).map(Some).map_err(D::Error::custom)
}

/// Membership chooses the one owner of a package's format, as it chooses the owner of its compiler minimum: an enumerating umbrella, whose `[format]` a member does not restate, or the standalone package itself. The settings are `None` where the owner states no `[format]`, which is every setting at its default.
pub fn format_owner<'a>(
    package: &'a Package,
    directory: &Path,
    umbrella: Option<(&'a Umbrella, &Path)>,
) -> Result<(Option<&'a FormatSettings>, PathBuf), String> {
    let manifest = directory.join(MANIFEST);
    match umbrella {
        Some((umbrella, root)) => {
            let owner = root.join(MANIFEST);
            if package.format.is_some() {
                return Err(format!(
                    "{} inherits `[format]` from the umbrella at {}; remove its local `[format]` table",
                    manifest.display(),
                    owner.display()
                ));
            }
            Ok((umbrella.format.as_ref(), owner))
        }
        None => Ok((package.format.as_ref(), manifest)),
    }
}
