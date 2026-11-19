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
    /// A text the formatter would write otherwise.
    Format,
    /// A declaration asked about by its path, its whole text following the block.
    Declaration,
    /// A module asked about by its path, the names it exposes following the block.
    Module,
    /// A term asked about by its place, where no name stands.
    Term,
    /// A local asked about by its place: where it is bound, and what it was bound at.
    Local,
}

impl Kind {
    /// The word a report of this kind closes on.
    pub fn word(self) -> &'static str {
        match self {
            Kind::Error => "error",
            Kind::Goal => "goal",
            Kind::Lint => "lint",
            Kind::Format => "format",
            Kind::Declaration => "declaration",
            Kind::Module => "module",
            Kind::Term => "term",
            Kind::Local => "local",
        }
    }

    /// Whether a report of this kind shows the code at its own place: every kind but a declaration's, whose whole text follows the block, and a module's, whose names do. A term's place is the term.
    pub fn quotes_its_place(self) -> bool {
        !matches!(self, Kind::Declaration | Kind::Module)
    }

    /// Whether the kind says something went wrong, which is what is painted as trouble.
    pub fn troubled(self) -> bool {
        self == Kind::Error
    }
}

/// What a painted run is painted as: the attribute a terminal is written for it, and the name an edge that paints otherwise reads it by ([`runs`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    /// What a block is about: a subject, a headline.
    Bold,
    /// What frames: a tree, a gutter, a time.
    Dim,
    /// What went wrong.
    Troubled,
    /// What is said aside: the mark of a line a program wrote on its standard error.
    Aside,
}

/// What opens a terminal attribute, closed by `m`, and the attribute that ends every other.
const OPEN: &str = "\x1b[";
const RESET: &str = "0";

impl Role {
    const ALL: [Role; 4] = [Role::Bold, Role::Dim, Role::Troubled, Role::Aside];

    /// The attribute a terminal is written for the role.
    fn code(self) -> &'static str {
        match self {
            Role::Bold => "1",
            Role::Dim => "2",
            Role::Troubled => "31",
            Role::Aside => "33",
        }
    }

    /// The role as an edge that paints otherwise names it.
    pub fn name(self) -> &'static str {
        match self {
            Role::Bold => "bold",
            Role::Dim => "dim",
            Role::Troubled => "troubled",
            Role::Aside => "aside",
        }
    }
}

/// `painted`, as a [`Style`] that sees colour wrote it, read back into its runs, in order: each text with the role it is painted in, `None` where it is plain. What an edge that paints a report otherwise than a terminal — a page — takes, so the roles reach it as data; joined, the texts are what the plain style writes.
pub fn runs(painted: &str) -> Vec<(Option<Role>, String)> {
    let mut runs = Vec::new();
    let mut role = None;
    let mut rest = painted;
    while let Some(start) = rest.find(OPEN) {
        let (text, attribute) = rest.split_at(start);
        if !text.is_empty() {
            runs.push((role, text.to_string()));
        }
        let end = attribute
            .find('m')
            .expect("a painted text closes every attribute it opens");
        let code = &attribute[OPEN.len()..end];
        role = match code {
            RESET => None,
            code => Role::ALL.into_iter().find(|role| role.code() == code),
        };
        rest = &attribute[end + 1..];
    }
    if !rest.is_empty() {
        runs.push((role, rest.to_string()));
    }
    runs
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

    /// `text` in `role`, where the reader sees colour.
    fn painted(&self, role: Role, text: impl fmt::Display) -> String {
        match self.painted {
            true => format!("{OPEN}{}m{text}{OPEN}{RESET}m", role.code()),
            false => text.to_string(),
        }
    }

    /// `text` as what a block is about is painted: a subject, a headline.
    pub fn bold(&self, text: impl fmt::Display) -> String {
        self.painted(Role::Bold, text)
    }

    /// `text` as what frames is painted: a tree, a gutter, a time.
    pub fn dim(&self, text: impl fmt::Display) -> String {
        self.painted(Role::Dim, text)
    }

    /// `text` as what went wrong is painted.
    pub fn troubled(&self, text: impl fmt::Display) -> String {
        self.painted(Role::Troubled, text)
    }

    /// `text` as what is said aside is painted: the mark of a line a program wrote on its standard error.
    pub fn aside(&self, text: impl fmt::Display) -> String {
        self.painted(Role::Aside, text)
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
