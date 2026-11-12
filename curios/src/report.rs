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
//! **Nothing written is taken back.** There is no cursor control and no repainting: the bytes are the same in a pipe, a log file and a CI transcript as they are on a terminal, colour apart. Rust's stderr is unbuffered, so a label reaches the reader before the work it names with no flushing of its own.
//!
//! **Colour is a layer, decided once** ([`paint`]): a subject bold, the tree and the times dim, and what went wrong red. It changes no byte beneath it, so a painted transcript stripped of its codes is the plain one, which is what is kept of one for a page.
//!
//! **A run that sends pages its report keeps the transcript** ([`keep`]): every byte written here, from then on, is what a page is sent, so the page shows what the terminal showed.
//!
//! **What is said of no one subject stands under none**: a `note:` of how the command took what it was asked ([`note`]), a step taken for every subject ([`alone`]), and what the whole invocation came to, as its last line ([`total`]).

#[cfg(test)]
mod tests;

use {
    curios_common::{Qualifier, Style},
    curios_package::Named,
    curios_wonder::STDIN_LABEL,
    std::{
        env, fmt,
        io::Write,
        path::{Path, PathBuf},
        sync::{Mutex, MutexGuard, OnceLock, PoisonError},
        time::{Duration, Instant},
    },
};

/// What the transcript wrote since a run asked for it to be kept, where one did.
static KEPT: Mutex<Option<Vec<u8>>> = Mutex::new(None);

/// What is kept of the transcript, held. A writer that panicked left every byte it wrote, so its poison is passed over.
fn held() -> MutexGuard<'static, Option<Vec<u8>>> {
    KEPT.lock().unwrap_or_else(PoisonError::into_inner)
}

/// The reader no style was decided for: standing nowhere, seeing no colour.
static PLAIN: Style = Style {
    here: None,
    painted: false,
};

/// Whoever reads standard error, decided once as the command starts.
static READER: OnceLock<Style> = OnceLock::new();

/// Write for `reader` from here on: where they stand, which a path is spelled from, and whether they see colour. Decided once, as the command starts, from what was asked and where standard error goes.
pub fn paint(reader: Style) {
    let _ = READER.set(reader);
}

/// Whoever reads standard error: what the transcript, and a report beside it, is written for.
pub fn reader() -> &'static Style {
    READER.get().unwrap_or(&PLAIN)
}

/// `text` as a subject is painted.
fn bold(text: impl fmt::Display) -> String {
    reader().bold(text)
}

/// `text` as the tree and a time are painted.
fn dim(text: impl fmt::Display) -> String {
    reader().dim(text)
}

/// `text` as what went wrong is painted: a refusal, a failure, a count of either that is not zero.
pub fn troubled(text: impl fmt::Display) -> String {
    reader().troubled(text)
}

/// Whether `verdict` says something went wrong: a rule refused, the machine failed, a test trapped or exited.
fn trouble(verdict: &str) -> bool {
    matches!(verdict, "refused" | "failed" | "trapped") || verdict.starts_with("exited ")
}

/// `verdict` as it is painted: as trouble where it says something went wrong.
fn worded(verdict: &str) -> String {
    match trouble(verdict) {
        true => troubled(verdict),
        false => verdict.to_string(),
    }
}

/// `text` with every attribute code [`painted`] wrote taken out.
fn plain(text: &str) -> String {
    let mut plain = String::with_capacity(text.len());
    let mut rest = text;

    while let Some((before, coded)) = rest.split_once("\x1b[") {
        plain.push_str(before);
        rest = coded.split_once('m').map_or("", |(_, after)| after);
    }
    plain.push_str(rest);

    plain
}

/// The one path every byte of the transcript takes: to stderr, and, plain, onto what is kept of it where a run keeps it.
fn say(text: impl fmt::Display) {
    let text = text.to_string();

    eprint!("{text}");
    if let Some(kept) = held().as_mut() {
        kept.extend_from_slice(plain(&text).as_bytes());
    }
}

/// Keep what the transcript writes from here on. A run that sends pages its report asks before its first line, so what a page is sent is what the terminal showed and the two cannot drift.
pub fn keep() {
    held().get_or_insert_default();
}

/// What the transcript wrote since it was asked kept, in the order written, and nothing where it was not.
pub fn kept() -> Vec<u8> {
    held().clone().unwrap_or_default()
}

/// Relay `bytes`, which a program run elsewhere wrote on one of its standard streams, to `stream`, this process's of the same name, and onto what is kept of the transcript, where they fell.
pub fn relayed(mut stream: impl Write, bytes: &[u8]) {
    stream.write_all(bytes).expect("failed relaying");
    if let Some(kept) = held().as_mut() {
        kept.extend_from_slice(bytes);
    }
}

/// What a status line is about.
///
/// Four namespaces, and only the first is the program's — which is where a leading `/` comes from, and the only place one is ever written. A mount prefix is a name a program can spell, an executable's and a foreign module's are identifiers their manifest rows chose, and a file's is whatever was typed or wherever it landed. Rendering all of them through one type is what keeps a print site from spelling a slash it did not derive.
#[derive(Clone, PartialEq, Eq)]
pub enum Subject {
    /// A unit, by the prefix it mounts, or a module, by its path: `/` for the root, which joins to nothing.
    Mounted(Qualifier),
    /// A declared executable, by the name its manifest row gives it.
    Executable(String),
    /// A declared foreign module, by the name its manifest row gives it. Never a prefix: nothing in Curios source refers to it, since a `foreign` declaration is reached by its own qualified name rather than through whatever implements it.
    Module(String),
    /// A file, as it was written on the command line or as it landed on disk.
    File(PathBuf),
    /// A directory, as it was written: the separator it is shown closed by is what tells a reader the nodes beneath are what landed in it.
    Directory(PathBuf),
    /// The program on standard input, which was asked for as `-` and is reported as what that means: a `-` alone on a line reads as one the compiler failed to finish writing.
    Stdin,
}

impl Subject {
    /// The subject a package is reported as. Its declared name *is* its mount prefix, so it is reported as one — which keeps the slash derived here as it is everywhere else instead of written by the caller.
    pub fn package(name: &str) -> Self {
        Self::Mounted(Qualifier::empty().with(name))
    }

    /// What a program or a library is reported as: the name that was asked for, never the file it resolved to. A declared executable resolves to an absolute path somewhere under the governing root, and echoing that back fills a line with what the reader already knew; a loose file is what was asked for, so it is reported as written.
    pub fn of(named: &Named) -> Self {
        match named {
            Named::Library(package) => Self::package(package),
            Named::Executable(name) => Self::Executable(name.clone()),
            Named::File(path) => Self::File(path.clone()),
            Named::Stdin => Self::Stdin,
        }
    }
}

impl fmt::Display for Subject {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mounted(prefix) if prefix.is_root() => formatter.write_str("/"),
            Self::Mounted(prefix) => formatter.write_str(&prefix.join()),
            Self::Executable(name) | Self::Module(name) => formatter.write_str(name),
            Self::File(path) => write!(formatter, "{}", path.display()),
            Self::Directory(path) => write!(
                formatter,
                "{}/",
                path.display().to_string().trim_end_matches('/')
            ),
            Self::Stdin => formatter.write_str(STDIN_LABEL),
        }
    }
}

/// A step taken for no one subject, on a line of its own under none: `serve; http://127.0.0.1:8000/`, where what is served is every subject's.
pub fn alone(label: &str, outcome: impl fmt::Display) {
    say(format_args!("{label}; {outcome}\n"));
}

/// What a whole invocation came to, as its last line, with the time `taken` by all it sums where that was measured: `3 passed, 1 failed; 0.2s`.
pub fn total(text: impl fmt::Display, taken: Option<Duration>) {
    match taken {
        Some(taken) => say(format_args!("{text}; {}\n", dim(seconds(taken)))),
        None => say(format_args!("{text}\n")),
    }
}

/// A blank line, between a tree and the report of what stopped it.
pub fn apart() {
    say("\n");
}

/// End the line of the node standing open, where what it names is about to write of its own: its outcome then closes it beneath ([`Tree::ended`]). Free of any tree because whoever sees the write holds none: a test's host, which runs where the test does.
pub fn cut() {
    say("\n");
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

/// `path` as whoever reads standard error would spell it from where they stand ([`Style::spelled`]).
pub fn from_here(path: &Path) -> String {
    reader().spelled(path)
}

/// The connector of a node more are planned after.
const BRANCH: &str = "├─";

/// The connector of the last node a tree plans.
const CORNER: &str = "╰─";

/// The trunk, beside what stands beneath a node more are planned after.
const TRUNK: &str = "│";

/// What a node's work wrote of its own on the standard streams while the node's line stood open, as whoever watched them saw it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Wrote {
    /// Nothing: the node's line is open still, and its outcome closes it.
    Nothing,
    /// Something, the last of it a line's end.
    Lines,
    /// Something that left a line open.
    Open,
}

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
///
/// **What a node's work writes of its own is never held back.** Its first byte ends the node's line ([`cut`]), it flows as written, and the outcome closes the node beneath it, on a curve off the trunk:
///
/// ```text
/// ├─ /geometry/draws_a_square
/// ■■■■
/// │  ╰─ passed in 0.1s
/// ```
pub struct Tree {
    /// The subject's line, until the first node writes it.
    line: Option<String>,
    /// Whether the line names the manifest that governs the subject.
    governed: bool,
    /// When the open node's work began, where one is open.
    opened: Option<Instant>,
    /// Whether the node last opened is the last its tree plans, which is whether the trunk goes on beside what stands beneath it.
    last: bool,
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
                Some(manifest) => format!("{} {}", bold(subject), dim(format!("({manifest})"))),
                None => bold(subject),
            }),
            opened: None,
            last: false,
        }
    }

    /// Leave the tree as it stands. A subject no node was written for says nothing, unless its line names the manifest that governs it: that is said whatever was done, since nothing else tells which package the invocation reached.
    pub fn close(mut self) {
        if self.governed {
            self.headed();
        }
    }

    /// The subject's line, written ahead of its first node.
    fn headed(&mut self) {
        if let Some(line) = self.line.take() {
            say(format_args!("{line}\n"));
        }
    }

    /// Open a node for `label`, the last its tree plans or not, leaving its line unterminated.
    pub fn open(&mut self, label: &str, last: bool) {
        self.headed();
        say(format_args!(
            "{} {label}",
            dim(match last {
                true => CORNER,
                false => BRANCH,
            })
        ));
        self.opened = Some(Instant::now());
        self.last = last;
    }

    /// One line of what the node last closed came to, beneath it and beside the trunk where the trunk goes on: the keys a `pin` wrote, under the node that wrote them.
    pub fn detail(&mut self, text: impl fmt::Display) {
        match self.last {
            true => say(format_args!("     {text}\n")),
            false => say(format_args!("{}    {text}\n", dim(TRUNK))),
        }
    }

    /// Whether a node is open: its label written and its outcome not yet.
    pub fn is_open(&self) -> bool {
        self.opened.is_some()
    }

    /// Close the open node with what its work took: `; 1.4s`.
    pub fn done(&mut self) {
        say(format_args!("; {}\n", dim(self.taken())));
    }

    /// Close the open node with how its work ended where it did not simply finish, and what it took: `; refused in 0.3s`.
    pub fn verdict(&mut self, verdict: &str) {
        let taken = self.taken();

        say(format_args!(
            "; {} {}\n",
            worded(verdict),
            dim(format_args!("in {taken}"))
        ));
    }

    /// Close the open node with what its work came to stand as, where that is the one fact it is read for: `; .curios/native/hello/hello`.
    pub fn outcome(&mut self, outcome: impl fmt::Display) {
        self.opened = None;
        say(format_args!("; {outcome}\n"));
    }

    /// Close the open node with where its work came to stand, or as `failed` where it could not be done: why is `done`'s to say, handed back for whoever reports it beneath.
    pub fn stands<E>(&mut self, done: Result<(), E>, place: impl fmt::Display) -> Result<(), E> {
        match &done {
            Ok(()) => self.outcome(place),
            Err(_) => {
                self.verdict("failed");
                apart();
            }
        }

        done
    }

    /// Close the open node with how its work ended and the time `taken` whoever ran it measured, then what only they know of it, a line of `beneath` each: `; passed in 0.1s` on the node's own line, or, where the work `wrote` of its own and so ended that line, on a curve beneath what it wrote, `│  ╰─ passed in 0.1s`, after the end of a line it left open.
    pub fn ended(&mut self, verdict: &str, taken: Duration, wrote: Wrote, beneath: &[String]) {
        let closing = format!(
            "{} {}",
            worded(verdict),
            dim(format_args!("in {}", seconds(taken)))
        );

        self.opened = None;
        let inset = match wrote {
            Wrote::Nothing => {
                say(format_args!("; {closing}\n"));

                "    "
            }
            Wrote::Lines | Wrote::Open => {
                if wrote == Wrote::Open {
                    say("\n");
                }
                say(format_args!("{}  {} {closing}\n", dim(TRUNK), dim(CORNER)));

                "       "
            }
        };
        for line in beneath {
            say(format_args!("{}{inset}{line}\n", dim(TRUNK)));
        }
    }

    /// Close the tree with the node that sums it up, and the time `taken` by all it sums where any of it ran: `╰─ 2 passed, 1 failed; 0.1s`.
    pub fn summed(mut self, sum: &str, taken: Option<Duration>) {
        self.headed();
        match taken {
            Some(taken) => say(format_args!(
                "{} {sum}; {}\n",
                dim(CORNER),
                dim(seconds(taken))
            )),
            None => say(format_args!("{} {sum}\n", dim(CORNER))),
        }
    }

    /// What the open node's work took, which closes it.
    fn taken(&mut self) -> String {
        seconds(self.opened.take().map_or(Duration::ZERO, |at| at.elapsed()))
    }
}

/// A count as the transcript spells one, its thousands apart: `4,312`.
pub fn grouped(count: u64) -> String {
    let digits = count.to_string();

    digits
        .char_indices()
        .flat_map(|(at, digit)| {
            let apart = at > 0 && (digits.len() - at).is_multiple_of(3);

            apart.then_some(',').into_iter().chain([digit])
        })
        .collect()
}

/// A time as the transcript spells one: seconds, to a tenth.
fn seconds(taken: Duration) -> String {
    format!("{:.1}s", taken.as_secs_f64())
}
