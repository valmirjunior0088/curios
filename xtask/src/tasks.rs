//! What a step's plan is made of: the tasks the runner performs, as data.
//!
//! **A plan describes, and the runner performs.** A step's recipe returns the tasks it needs done and does none of them, as a Curios program's `Io` describes a computation only its entry point performs ([Effects are descriptions, and the carrier has no eliminator](../../documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)). So what a step runs is something read before it runs: every command line the runner echoes is one of these tasks spelled out, and nothing a step does happens outside one.
//!
//! **A task's fields are its whole contract.** There is a kind of task for each thing a step does that no other kind says: a tool run in a tree, a binary filed where a build script reads it, the browser's bindings generated, a file written whole, and a file a task must leave as it found it. The set is closed, so no task's reads and writes hide in a function nothing can inspect.

use {
    crate::{Tool, Tree},
    std::path::PathBuf,
};

#[cfg(test)]
mod tests;

/// The workspace's own tools, which ship nothing: the compiler's checks leave them out, since cargo unifies a build's features across every package it selects and theirs would give the compiler's dependencies features no shipped compiler has.
pub(crate) const TOOLS: [&str; 3] = ["xtask", "xbench", "xboard"];

/// One thing a step asks the runner to do.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Task {
    /// A tool run in a tree.
    Run(Run),
    /// A built binary filed at `filed` as [`file_with_inputs`](crate::file_with_inputs) files one, beside the list of its inputs at `listed`.
    File {
        built: PathBuf,
        filed: PathBuf,
        listed: PathBuf,
    },
    /// The bindings `wasm-bindgen` generates for `module`, filed in `bundle`.
    Bindgen { module: PathBuf, bundle: PathBuf },
    /// `text` written to `path`, whole.
    Write { path: PathBuf, text: &'static str },
    /// `during`, which must leave `path` as it found it: where it writes the file again, the task fails, saying `rewritten`.
    Keeps {
        path: PathBuf,
        during: Box<Task>,
        rewritten: &'static str,
    },
}

/// A tool run from a tree's root, with its arguments and what is set in its environment beside what it inherits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Run {
    pub(crate) tool: Tool,
    pub(crate) tree: Tree,
    pub(crate) arguments: Vec<Arg>,
    pub(crate) environment: Vec<(&'static str, Arg)>,
}

/// One argument of a run, or one value set in its environment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Arg {
    Text(String),
    /// The packages a cargo or nextest run builds, spelled as the flags that select them.
    Selection(Selection),
    /// A path the run reads.
    Path(PathBuf),
}

/// The packages of the workspace a build selects.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Selection {
    /// Every member but [`TOOLS`]: what the compiler's checks build.
    Shipped,
    /// Every member, the tools among them.
    Every,
    /// These packages alone.
    Packages(Vec<String>),
}

impl Selection {
    /// One package alone.
    pub(crate) fn package(name: &str) -> Self {
        Selection::Packages(vec![name.to_string()])
    }

    /// The workspace's own tools alone.
    pub(crate) fn tools() -> Self {
        Selection::Packages(TOOLS.iter().map(|tool| tool.to_string()).collect())
    }

    /// The flags that select the packages.
    fn flags(&self) -> Vec<String> {
        match self {
            Selection::Shipped => ["--workspace"]
                .into_iter()
                .chain(TOOLS.iter().flat_map(|tool| ["--exclude", tool]))
                .map(str::to_string)
                .collect(),
            Selection::Every => vec!["--workspace".to_string()],
            Selection::Packages(packages) => packages
                .iter()
                .flat_map(|package| ["--package".to_string(), package.clone()])
                .collect(),
        }
    }
}

impl Arg {
    /// The words the argument stands for on a command line.
    pub(crate) fn spelled(&self) -> Vec<String> {
        match self {
            Arg::Text(text) => vec![text.clone()],
            Arg::Selection(selection) => selection.flags(),
            Arg::Path(path) => vec![path.to_string_lossy().into_owned()],
        }
    }
}

/// `words` as text arguments.
pub(crate) fn texts<'a>(words: impl IntoIterator<Item = &'a str>) -> Vec<Arg> {
    words
        .into_iter()
        .map(|word| Arg::Text(word.to_string()))
        .collect()
}

impl Run {
    /// `tool` run from `tree`'s root with `arguments`, nothing set in its environment.
    pub(crate) fn new(tool: Tool, tree: Tree, arguments: Vec<Arg>) -> Self {
        Run {
            tool,
            tree,
            arguments,
            environment: Vec::new(),
        }
    }

    /// The run with `name` set to `value` in its environment.
    pub(crate) fn with(mut self, name: &'static str, value: Arg) -> Self {
        self.environment.push((name, value));

        self
    }

    /// The run's command line after its program.
    pub(crate) fn spelled(&self) -> Vec<String> {
        self.arguments.iter().flat_map(Arg::spelled).collect()
    }
}

impl Task {
    /// cargo run in `tree` as `command`, over `selection` where there is one, then `options`.
    pub(crate) fn cargo(
        tree: Tree,
        command: &[&str],
        selection: Option<Selection>,
        options: &[&str],
    ) -> Self {
        let mut arguments = texts(command.iter().copied());
        arguments.extend(selection.map(Arg::Selection));
        arguments.extend(texts(options.iter().copied()));

        Task::Run(Run::new(Tool::Cargo, tree, arguments))
    }

    /// A `run` of nextest over `selection`, then `options`.
    pub(crate) fn nextest(selection: Selection, options: &[&str]) -> Self {
        let mut arguments = texts(["run"]);
        arguments.push(Arg::Selection(selection));
        arguments.extend(texts(options.iter().copied()));

        Task::Run(Run::new(Tool::Nextest, Tree::Workspace, arguments))
    }

    /// npm run in `tree`, one of the two npm packages under `editors/`.
    pub(crate) fn npm(tree: Tree, arguments: &[&str]) -> Self {
        Task::Run(Run::new(Tool::Npm, tree, texts(arguments.iter().copied())))
    }
}
