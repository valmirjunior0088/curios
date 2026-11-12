//! How a report is written for whoever reads it: where they stand, which a path is spelled from, and whether they see colour.

use std::{
    fmt,
    path::{Path, PathBuf},
};

/// What a report is, which is the word it closes on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// A rule refused the program.
    Error,
    /// A goal the program writes, with what the compiler knows of it.
    Goal,
    /// An exact finding nothing stops on.
    Lint,
    /// How a command took what it was asked.
    Note,
}

impl Kind {
    /// The word a report of this kind closes on.
    pub fn word(self) -> &'static str {
        match self {
            Kind::Error => "error",
            Kind::Goal => "goal",
            Kind::Lint => "lint",
            Kind::Note => "note",
        }
    }

    /// Whether the kind says something went wrong, which is what is painted as trouble.
    pub fn troubled(self) -> bool {
        self == Kind::Error
    }
}

/// Whoever a report is written for: where they stand, and whether they see colour. Decided by whoever prints, once, and handed to what renders, so a report is data until the edge that writes it.
///
/// **Colour is a layer.** It changes no byte beneath it: what is painted, stripped of its codes, is what the plain style writes.
#[derive(Debug, Clone, Default)]
pub struct Style {
    /// The directory the reader stands in, which a path is spelled from. `None` spells every path as it is held.
    pub here: Option<PathBuf>,
    /// Whether the reader sees colour.
    pub painted: bool,
}

impl Style {
    /// The style of a reader standing nowhere who sees no colour: every path as it is held, and no code written.
    pub fn plain() -> Self {
        Self::default()
    }

    /// `text` under the attribute `code`, where the reader sees colour.
    fn painted(&self, code: &str, text: impl fmt::Display) -> String {
        match self.painted {
            true => format!("\x1b[{code}m{text}\x1b[0m"),
            false => text.to_string(),
        }
    }

    /// `text` as what a block is about is painted: a subject, a headline.
    pub fn bold(&self, text: impl fmt::Display) -> String {
        self.painted("1", text)
    }

    /// `text` as what frames is painted: a tree, a gutter, a time.
    pub fn dim(&self, text: impl fmt::Display) -> String {
        self.painted("2", text)
    }

    /// `text` as what went wrong is painted.
    pub fn troubled(&self, text: impl fmt::Display) -> String {
        self.painted("31", text)
    }

    /// `path` as the reader would spell it: as it is written where it is relative, by the climb from where they stand and the descent to it where the two share more than the root, and whole anywhere else.
    pub fn spelled(&self, path: &Path) -> String {
        match (path.is_absolute(), &self.here) {
            // What stands on disk is read by where it is, so a link in the way to it does not hide what the two share.
            (true, Some(here)) => between(
                &path.canonicalize().unwrap_or_else(|_| path.to_path_buf()),
                here,
            ),
            _ => path.display().to_string(),
        }
    }
}

/// The absolute `path` as spelled from the directory `here`.
fn between(path: &Path, here: &Path) -> String {
    let shared = path
        .components()
        .zip(here.components())
        .take_while(|(there, stood)| there == stood)
        .count();

    // The root alone is shared: the two are in different places, and the path is told whole.
    if shared <= 1 {
        return path.display().to_string();
    }

    let climb = "../".repeat(here.components().count() - shared);
    let descent = path.components().skip(shared).collect::<PathBuf>();

    match descent.as_os_str().is_empty() {
        true => format!("{climb}."),
        false => format!("{climb}{}", descent.display()),
    }
}

#[cfg(test)]
mod tests;
