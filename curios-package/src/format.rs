//! What `[format]` and `curios format`'s flags state, and the one place a file's format is read from.
//!
//! **Each setting is declared once, as a field**: its key under `[format]` is the field's name as serde reads it, and its flag the same name as clap reads it, so no setting has a key without a flag or a flag without a key, and `format/tests.rs` holds the two spellings equal.
//!
//! **A file's format has one source.** Where a package governs the file, it is that package's `[format]`, or its enumerating umbrella's, which a member does not restate, as it does not restate `compiler`; where none governs it — standard input, a file with no package's manifest above it — it is the command line's. A flag where a package governs is refused rather than ranked against the manifest, so the command line, a check and an editor's server format a file one way. The decision is `documentation/design/tools/a-file-is-formatted-as-one-place-states.md`'s.

#[cfg(test)]
mod tests;

use {
    crate::{Governing, MANIFEST, Package, Refused, Umbrella},
    clap::Args,
    curios_common::{MAX_LINE_WIDTH, Report},
    curios_surface::{
        DEFAULT_FLAT_MATCH_ARMS, DEFAULT_FLAT_MATCH_WIDTH, FlatMatch, FlatMatchSetting, Format,
        MIN_FLAT_MATCH_ARMS, MIN_LINE_WIDTH, Width,
    },
    serde::{Deserialize, Deserializer, de::Error},
    std::path::{Path, PathBuf},
};

/// What a `[format]` table states, and what `curios format`'s flags state of a file no package governs: every setting optional, and absent where it is its default.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Args)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct FormatSettings {
    /// The columns a formatted line is written within.
    #[arg(long, value_name = "COLUMNS", help = line_width_help())]
    #[serde(default, deserialize_with = "line_width")]
    pub line_width: Option<Width>,
    /// The most columns a match keeps on one line, from `match` to `end`.
    #[arg(long, value_name = "COLUMNS", help = flat_match_width_help())]
    #[serde(default)]
    pub flat_match_width: Option<usize>,
    /// The most arms a match keeps on one line.
    #[arg(long, value_name = "ARMS", help = flat_match_arms_help())]
    #[serde(default)]
    pub flat_match_arms: Option<usize>,
}

impl FormatSettings {
    /// The format a file is formatted within, from the one place that states it: `governing`'s `[format]` where a package governs the file, and these settings, the command line's, where none does. A setting stated here where a package governs is refused, naming the manifest that owns the package's format, and a flat match's width stated past the line width is refused wherever it is stated.
    pub fn format_under(&self, governing: Option<&Governing>) -> Result<Format, Box<Report>> {
        let settings = match governing {
            None => {
                return self
                    .format()
                    .map_err(|(setting, refused)| refused.about_named(spelled(setting).1));
            }
            Some(governing) => {
                let (stated, owner) = governing.format()?;
                if let Some(&flag) = self.stated().first() {
                    return Err(Refused::of("surplus flag")
                        .stating(format_args!(
                            "the package `{}` takes its format from {}",
                            governing.package.name,
                            owner.display()
                        ))
                        .stating("a flag does not override it")
                        .stating("write the setting under its `[format]` table, or format the text through standard input, which no manifest governs")
                        .about_named(flag));
                }
                stated.cloned().unwrap_or_default()
            }
        };

        // A manifest's settings were held to a format where it was read, so they make one.
        settings
            .format()
            .map_err(|(setting, refused)| refused.about_named(spelled(setting).1))
    }

    /// [`FormatSettings::format_under`] for `file`: under the package `manifest` names or the one whose manifest is nearest above it, whose stated compiler the running one must be, and under nothing where no package governs it. `cabinet` is the one the invocation was handed, which a governing package is opened with and formatting never reads.
    pub fn format_of_file(
        &self,
        file: &Path,
        manifest: Option<&Path>,
        cabinet: Option<&Path>,
    ) -> Result<Format, Box<Report>> {
        let governing = Governing::of_file(file, manifest, cabinet)?;
        if let Some(governing) = &governing {
            governing.check_compiler()?;
        }

        self.format_under(governing.as_ref())
    }

    /// These settings as a manifest's `[format]` table states them, refused where they make no format: what a manifest is read with, so a table that states a flat match wider than its lines is refused where it is written.
    pub(crate) fn checked(self) -> Result<Self, Refused> {
        self.format()
            .map_err(|(setting, refused)| refused.value(["format", spelled(setting).0]))?;

        Ok(self)
    }

    /// The format these settings state, each setting its default where it is not stated, or the setting refused and why.
    fn format(&self) -> Result<Format, (FlatMatchSetting, Refused)> {
        // Every setting is named here, so one added to the struct does not compile until it is read.
        let Self {
            line_width,
            flat_match_width,
            flat_match_arms,
        } = self;
        let line_width = line_width.unwrap_or_default();
        let flat_match = FlatMatch::within(line_width, *flat_match_width, *flat_match_arms)
            .map_err(|(setting, said)| (setting, Refused::of("invalid value").stating(said)))?;

        Ok(Format {
            line_width,
            flat_match,
        })
    }

    /// The flags these settings state, as the command line spells them.
    fn stated(&self) -> Vec<&'static str> {
        let Self {
            line_width,
            flat_match_width,
            flat_match_arms,
        } = self;
        [
            (line_width.is_some(), "--line-width"),
            (flat_match_width.is_some(), "--flat-match-width"),
            (flat_match_arms.is_some(), "--flat-match-arms"),
        ]
        .into_iter()
        .filter_map(|(stated, flag)| stated.then_some(flag))
        .collect()
    }
}

/// `--line-width`'s help, its bounds read off the constants that set them.
fn line_width_help() -> String {
    format!(
        "The columns a formatted line is written within, `{MIN_LINE_WIDTH}` through `{MAX_LINE_WIDTH}`, for standard input or a file no package governs; a package states its own under `[format]` (default: `{MAX_LINE_WIDTH}`)"
    )
}

/// A flat match's setting as a `[format]` key and as a flag spell it.
fn spelled(setting: FlatMatchSetting) -> (&'static str, &'static str) {
    match setting {
        FlatMatchSetting::Width => ("flat-match-width", "--flat-match-width"),
        FlatMatchSetting::Arms => ("flat-match-arms", "--flat-match-arms"),
    }
}

/// `--flat-match-width`'s help, its default read off the constant that sets it.
fn flat_match_width_help() -> String {
    format!(
        "The most columns a match keeps on one line, from `match` to `end`, `0` through the line width, for standard input or a file no package governs; a package states its own under `[format]` (default: `{DEFAULT_FLAT_MATCH_WIDTH}`, or the line width where that is narrower)"
    )
}

/// `--flat-match-arms`' help, its default read off the constant that sets it.
fn flat_match_arms_help() -> String {
    format!(
        "The most arms a match keeps on one line, from `{MIN_FLAT_MATCH_ARMS}`, for standard input or a file no package governs; a package states its own under `[format]` (default: `{DEFAULT_FLAT_MATCH_ARMS}`)"
    )
}

/// `line-width` as a manifest spells it: a whole number of columns, refused where the formatter admits no such width.
fn line_width<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Option<Width>, D::Error> {
    let columns = u64::deserialize(deserializer)?;
    let columns = usize::try_from(columns).map_err(D::Error::custom)?;

    Width::new(columns).map(Some).map_err(D::Error::custom)
}

/// Membership chooses the one owner of a package's format, as it chooses the owner of its compiler version: an enumerating umbrella, whose `[format]` a member does not restate, or the standalone package itself. The settings are `None` where the owner states no `[format]`, which is every setting at its default.
pub fn format_owner<'a>(
    package: &'a Package,
    directory: &Path,
    umbrella: Option<(&'a Umbrella, &Path)>,
) -> Result<(Option<&'a FormatSettings>, PathBuf), Box<Report>> {
    let manifest = directory.join(MANIFEST);
    match umbrella {
        Some((umbrella, root)) => {
            let owner = root.join(MANIFEST);
            if package.format.is_some() {
                return Err(Refused::of("duplicate setting")
                    .stating(format_args!(
                        "`[format]` is inherited from the umbrella at {}",
                        owner.display()
                    ))
                    .stating("remove the local table")
                    .entry(["format"])
                    .located_at(&manifest));
            }
            Ok((umbrella.format.as_ref(), owner))
        }
        None => Ok((package.format.as_ref(), manifest)),
    }
}
