//! What the CLI says while it works.
//!
//! One transcript. A subject stands alone on a line, and beneath it a node for each thing done for it, written `label; outcome` ([`Tree`]):
//!
//! ```text
//! /geometry
//! ├─ check; 1.6s
//! ╰─ store; 0.1s
//! hello
//! ├─ check; 0.1s
//! ├─ build; 0.2s
//! ╰─ emit; 0.4s
//! ```
//!
//! **Everything here goes to stderr.** `curios run` hands stdout to the program it executes, so a status line written there would corrupt `curios run report.crs > report.json`.
//!
//! **Nothing written is taken back.** There is no cursor control, no repainting and no terminal detection: the bytes are the same in a pipe, a log file and a CI transcript as they are on a terminal. Rust's stderr is unbuffered, so a label reaches the reader before the work it names with no flushing of its own.
//!
//! **A heading column is what a command not drawn as a tree prints**: one word of [`Heading`], padded to a fixed width, then what it is about ([`fact`]).

#[cfg(test)]
mod tests;

use {
    curios_common::Qualifier,
    curios_wonder::STDIN_LABEL,
    std::{
        env, fmt,
        path::{Path, PathBuf},
        time::Instant,
    },
};

/// What a status line is about.
///
/// Four namespaces, and only the first is the program's — which is where a leading `/` comes from, and the only place one is ever written. A mount prefix is a name a program can spell, an executable's and a foreign module's are identifiers their manifest rows chose, and a file's is whatever was typed or wherever it landed. Rendering all of them through one type is what keeps a print site from spelling a slash it did not derive.
#[derive(PartialEq, Eq)]
pub enum Subject {
    /// A unit, by the prefix it mounts, or a module, by its path: `/` for the root, which joins to nothing.
    Mounted(Qualifier),
    /// A declared executable, by the name its manifest row gives it.
    Executable(String),
    /// A declared foreign module, by the name its manifest row gives it. Never a prefix: nothing in Curios source refers to it, since a `foreign` declaration is reached by its own qualified name rather than through whatever implements it.
    Module(String),
    /// A file, as it was written on the command line or as it landed on disk.
    File(PathBuf),
    /// The program on standard input, which was asked for as `-` and is reported as what that means: a `-` alone on a line reads as one the compiler failed to finish writing.
    Stdin,
}

impl Subject {
    /// The subject a package is reported as. Its declared name *is* its mount prefix, so it is reported as one — which keeps the slash derived here as it is everywhere else instead of written by the caller.
    pub fn package(name: &str) -> Self {
        Self::Mounted(Qualifier::empty().with(name))
    }
}

impl fmt::Display for Subject {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mounted(prefix) if prefix.is_root() => formatter.write_str("/"),
            Self::Mounted(prefix) => formatter.write_str(&prefix.join()),
            Self::Executable(name) | Self::Module(name) => formatter.write_str(name),
            Self::File(path) => write!(formatter, "{}", path.display()),
            Self::Stdin => formatter.write_str(STDIN_LABEL),
        }
    }
}

/// Declare every heading once — the variant, its documentation, and the word it prints — so the enum and the list the column is measured from cannot drift apart: a list kept by hand beside the enum would go unchecked, and a missing entry would only make the column too narrow.
macro_rules! headings {
    ($($(#[$note:meta])* $variant:ident => $word:literal,)+) => {
        /// Every word that can head a status line. The tense is the contract: a present participle opens a line before its work and may accrue what it came to, a past participle states a settled fact after it, and the imperative addresses the reader.
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub enum Heading {
            $($(#[$note])* $variant,)+
        }

        impl Heading {
            /// The word itself.
            const fn text(self) -> &'static str {
                match self {
                    $(Self::$variant => $word,)+
                }
            }
        }

        /// Every heading, so the column below is measured rather than kept by hand.
        const HEADINGS: &[Heading] = &[$(Heading::$variant,)+];
    };
}

headings! {
    /// A subject being taken on: the header of the steps that deal with it. Deliberately neutral about which steps those are, since what follows may be a compile or a reuse, then a run or a test.
    Processing => "Processing",
    /// A unit being obtained — closed by the time its compile took, or by `reused` when the store already held it.
    Compiling => "Compiling",
    /// A subject's program being made of its compiled units, through to the machine code a run executes — closed by the time that took, or by `reused` when the store already held the payload. A step of its own, and not a second `Compiling`, because a library's tests are a program named as its unit is.
    Linking => "Linking",
    /// A module's tests begun, the report of each that runs following on stdout.
    Testing => "Testing",
    /// A module's tests finished, with their tally.
    Tested => "Tested",
    /// The handover to the program itself.
    Running => "Running",
    /// A program's page being served, at the address a browser opens it at.
    Serving => "Serving",
    /// Work that could not be done and was not required — never a failure, always a reason.
    Skipped => "Skipped",
    /// The whole invocation, summarized by what it produced.
    Finished => "Finished",
    /// A dependency brought into the store.
    Fetched => "Fetched",
    /// A manifest row written, or — under `--check` — one that would be.
    Pinned => "Pinned",
    /// What a delivery turned out to be, said before the pin derived from it is believed.
    Delivered => "Delivered",
    /// One file or directory scaffolding produced.
    Created => "Created",
    /// The command to run next.
    Try => "Try",
}

/// The marker a nested line wears, in the columns a top-level line leaves empty in front of its heading.
const NESTED: &str = "↳ ";

/// How many columns [`NESTED`] occupies. Written out rather than measured: `str::len` counts the arrow's three UTF-8 bytes, and it prints in one.
const NESTED_WIDTH: usize = 2;

/// The widest heading in `headings`.
const fn widest(headings: &[Heading]) -> usize {
    let (mut max, mut index) = (0, 0);

    while index < headings.len() {
        let width = headings[index].text().len();

        if width > max {
            max = width;
        }

        index += 1;
    }

    max
}

/// The heading column: the widest heading, the columns a nested line's marker takes in front of it, and the three spaces every line keeps to its right.
pub const HEADING_WIDTH: usize = widest(HEADINGS) + NESTED_WIDTH + 3;

/// A line's heading column, padded out to where its subject begins. `marker` is [`NESTED`] for a step of the header above and empty for a line of its own.
///
/// Padded once here rather than at each write site, and after the marker is joined on: `{:<}` measures characters, so the arrow counts for the one column it prints in whatever its byte length.
fn head(marker: &str, heading: Heading) -> String {
    format!("{:<HEADING_WIDTH$}", format!("{marker}{}", heading.text()))
}

/// One complete fact, on a line of its own.
pub fn fact(heading: Heading, detail: impl fmt::Display) {
    eprintln!("{}", unnested(heading, detail));
}

/// One complete fact, as text — `Finished 3 passed, 1 failed` — for a fact written to the terminal and carried elsewhere as the same bytes.
pub fn unnested(heading: Heading, detail: impl fmt::Display) -> String {
    format!("{}{detail}", head("", heading))
}

/// How a build is taken rather than anything it did: a line of its own ahead of the header, spelled as a question spells its note.
pub fn note(text: impl fmt::Display) {
    eprintln!("note: {text}");
}

/// What a question says where the store took nothing of what it compiled: the fact a build states for the same reason, in a question's words. A store nobody can write otherwise reads as a compiler that is slow.
pub fn unfiled(refusal: String) {
    fact(
        Heading::Skipped,
        format!("storing what this compiled; {refusal}"),
    );
}

/// One detail of the line above, in the columns a heading would occupy — the keys a `pin` wrote, under the row it wrote them into.
///
/// Headingless because it is not a thing that happened, which is what every heading names; it is part of what the line above already reported.
pub fn detail(text: impl fmt::Display) {
    eprintln!("{:<HEADING_WIDTH$}  {text}", "");
}

/// One complete step of the header above, as text — `↳ Running hello`, or `↳ Tested /app; 3 passed, 1 failed` with `outcome` — for a step written to the terminal and carried elsewhere as the same bytes.
pub fn nested(heading: Heading, subject: &Subject, outcome: Option<&str>) -> String {
    match outcome {
        Some(outcome) => format!("{}{subject}; {outcome}", head(NESTED, heading)),
        None => format!("{}{subject}", head(NESTED, heading)),
    }
}

/// The step a page is served with, naming where a browser opens it — the one fact a reader needs from what follows, which is nothing until the server is stopped.
pub fn serving(url: &str) {
    eprintln!("{}{url}", head(NESTED, Heading::Serving));
}

/// `manifest` as a reader standing in `here` would spell it: nothing when it sits right there, a climb of `../` when it sits above, and the path itself anywhere else — `--manifest` can name a file off to the side.
fn spelled_from(manifest: &Path, here: &Path) -> Option<String> {
    let directory = manifest.parent()?;

    if directory == here {
        return None;
    }

    Some(match here.strip_prefix(directory) {
        Ok(below) => format!(
            "{}{}",
            "../".repeat(below.components().count()),
            manifest.file_name()?.to_string_lossy()
        ),
        Err(_) => manifest.display().to_string(),
    })
}

/// `path` as a reader standing where the invocation stands would spell it: as it is written where it is relative, by the climb from here and the descent to it where the two share more than the root, and whole anywhere else.
pub fn from_here(path: &Path) -> String {
    let here = env::current_dir().and_then(|here| here.canonicalize()).ok();

    match (path.is_absolute(), here) {
        // What stands on disk is read by where it is, so a link in the way to it does not hide what the two share.
        (true, Some(here)) => between(
            &path.canonicalize().unwrap_or_else(|_| path.to_path_buf()),
            &here,
        ),
        _ => path.display().to_string(),
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

/// The connector of a node more are planned after.
const BRANCH: &str = "├─ ";

/// The connector of the last node a tree plans.
const CORNER: &str = "╰─ ";

/// One subject's tree: the subject alone on a line, and beneath it a node for each thing done for it, written `label; outcome`.
///
/// ```text
/// /geometry
/// ├─ check; 1.6s
/// ╰─ store; 0.1s
/// ```
///
/// **The subject's line is written with its first node**, so a subject nothing had to be done for says nothing: what the store already held is not narrated.
///
/// **A node's line is built by unterminated writes.** The label reaches the reader before the work it names, and what closes the line is what the work came to. A compiler that dies mid-step leaves the line open, so the last thing written is how far it got.
///
/// **The connector is the plan's.** `├─` is written where more nodes are planned and `╰─` at the last, before either's work is done, so a tree a refusal cuts short ends on `├─`: the steps it planned and never reached are the ones missing.
pub struct Tree {
    /// The subject's line, until the first node writes it.
    line: Option<String>,
    /// Whether the line names the manifest that governs the subject.
    governed: bool,
    /// When the open node's work began, where one is open.
    opened: Option<Instant>,
}

impl Tree {
    /// A tree for `subject`, naming `manifest` beside it when the invocation does not stand in the manifest's directory, which is the one case where what governs could otherwise be a guess.
    pub fn of(subject: &Subject, manifest: Option<&Path>) -> Self {
        let here = env::current_dir().and_then(|here| here.canonicalize());
        let spelled = manifest
            .zip(here.ok())
            .and_then(|(manifest, here)| spelled_from(manifest, &here));

        Self {
            governed: spelled.is_some(),
            line: Some(match spelled {
                Some(manifest) => format!("{subject} ({manifest})"),
                None => subject.to_string(),
            }),
            opened: None,
        }
    }

    /// Leave the tree as it stands. A subject no node was written for says nothing, unless its line names the manifest that governs it: that is said whatever was done, since nothing else tells which package the invocation reached.
    pub fn close(self) {
        if let Some(line) = self.line.filter(|_| self.governed) {
            eprintln!("{line}");
        }
    }

    /// Open a node for `label`, the last its tree plans or not, leaving its line unterminated.
    pub fn open(&mut self, label: &str, last: bool) {
        if let Some(line) = self.line.take() {
            eprintln!("{line}");
        }
        eprint!(
            "{}{label}",
            match last {
                true => CORNER,
                false => BRANCH,
            }
        );
        self.opened = Some(Instant::now());
    }

    /// Whether a node is open: its label written and its outcome not yet.
    pub fn is_open(&self) -> bool {
        self.opened.is_some()
    }

    /// Close the open node with what its work took: `; 1.4s`.
    pub fn done(&mut self) {
        eprintln!("; {}", self.taken());
    }

    /// Close the open node with how its work ended where it did not simply finish, and what it took: `; refused in 0.3s`.
    pub fn verdict(&mut self, verdict: &str) {
        eprintln!("; {verdict} in {}", self.taken());
    }

    /// Close the open node with what its work came to stand as, where that is the one fact it is read for: `; .curios/native/hello/hello`.
    pub fn outcome(&mut self, outcome: impl fmt::Display) {
        self.opened = None;
        eprintln!("; {outcome}");
    }

    /// Close the open node with where its work came to stand, or as `failed` where it could not be done: why is `done`'s to say, handed back for whoever reports it beneath.
    pub fn stands<E>(&mut self, done: Result<(), E>, place: impl fmt::Display) -> Result<(), E> {
        match &done {
            Ok(()) => self.outcome(place),
            Err(_) => {
                self.verdict("failed");
                eprintln!();
            }
        }

        done
    }

    /// What the open node's work took, which closes it.
    fn taken(&mut self) -> String {
        let elapsed = self
            .opened
            .take()
            .map_or(0.0, |at| at.elapsed().as_secs_f64());

        format!("{elapsed:.1}s")
    }
}
