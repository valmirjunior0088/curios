//! What a step's plan is made of: the tasks the runner performs, as data, and the products they leave and read.
//!
//! **A plan describes, and the runner performs.** A step's recipe returns the tasks it needs done and does none of them, as a Curios program's `Io` describes a computation only its entry point performs ([Effects are descriptions, and the carrier has no eliminator](../../documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)). So what a step runs is something read before it runs: every command line the runner echoes is one of these tasks spelled out, and nothing a step does happens outside one.
//!
//! **A task's fields are its whole contract.** There is a kind of task for each thing a step does that no other kind says: a tool run in a tree, a binary filed where a build script reads it, the browser's bindings generated, a file written whole, and a file a task must leave as it found it. The set is closed, so no task's reads and writes hide in a function nothing can inspect.
//!
//! **What a step reads is read off its tasks.** A [`Product`] is what one step leaves and another reads, and [`Task::reads`] names the products a task reads from its structure: a cargo or nextest run whose selection builds a package whose build script reads a product, as cargo's own graph answers; an npm script, which reads its tree's install; a product handed to a run as a value; and, for a run that finds one by a path it was not handed, the product it declares. The runner runs a step after the step that leaves each product its plan reads, so no table states an edge.

use {
    crate::{BROWSER_TRIPLE, Checkout, HOST_TRIPLE, Tool, Tree, artifact, released},
    std::path::PathBuf,
};

#[cfg(test)]
mod tests;

/// The workspace's own tools, which ship nothing: the compiler's checks leave them out, since cargo unifies a build's features across every package it selects and theirs would give the compiler's dependencies features no shipped compiler has.
pub(crate) const TOOLS: [&str; 3] = ["xtask", "xbench", "xboard"];

/// What one step leaves for others to read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Product {
    /// The slim launcher, filed where `curios/build.rs` embeds it.
    Launcher,
    /// The compiler as it is released.
    Compiler,
    /// The browser bundle: `curios-js` built for the bare Wasm target, and its bindings.
    Bundle,
    /// The grammar's parser, generated from its definition and committed beside it.
    Parser,
    /// An npm tree's installed tools.
    Installed(Tree),
}

/// The products a workspace package's build script reads, beside the package: `curios/build.rs` embeds the launcher, so a build of `curios` reads it.
const READ_BY_BUILDS: [(Product, &str); 1] = [(Product::Launcher, "curios")];

impl Product {
    /// Where the product lives.
    pub(crate) fn path(self) -> PathBuf {
        match self {
            Product::Launcher => artifact("curios", HOST_TRIPLE),
            Product::Compiler => released("curios"),
            Product::Bundle => artifact("curios-js", BROWSER_TRIPLE),
            Product::Parser => Tree::Grammar.directory().join("src"),
            Product::Installed(tree) => tree.directory().join("node_modules"),
        }
    }
}

/// One thing a step asks the runner to do.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Task {
    /// A tool run in a tree.
    Run(Run),
    /// A built binary filed as `product` the way [`file_with_inputs`](crate::file_with_inputs) files one, beside the list of its inputs at `listed`.
    File {
        built: PathBuf,
        product: Product,
        listed: PathBuf,
    },
    /// The bindings `wasm-bindgen` generates for `module`, filed as `product`.
    Bindgen { module: PathBuf, product: Product },
    /// `text` written to `path`, whole.
    Write { path: PathBuf, text: &'static str },
    /// `during`, which must leave `path` as it found it: where it writes the file again, the task fails, saying `rewritten`.
    Keeps {
        path: PathBuf,
        during: Box<Task>,
        rewritten: &'static str,
    },
}

/// A tool run from a tree's root, with its arguments, what is set in its environment beside what it inherits, the product it leaves where it leaves one, and the products it finds by a path it was not handed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Run {
    pub(crate) tool: Tool,
    pub(crate) tree: Tree,
    pub(crate) arguments: Vec<Arg>,
    pub(crate) environment: Vec<(&'static str, Arg)>,
    pub(crate) leaves: Option<Product>,
    pub(crate) finds: Vec<Product>,
}

/// One argument of a run, or one value set in its environment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Arg {
    Text(String),
    /// The packages a cargo or nextest run builds, spelled as the flags that select them.
    Selection(Selection),
    /// A product, spelled as its path, which the run then reads.
    Product(Product),
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
            Arg::Product(product) => vec![product.path().to_string_lossy().into_owned()],
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
    /// `tool` run from `tree`'s root with `arguments`, nothing set in its environment, leaving nothing and finding nothing by a path of its own.
    pub(crate) fn new(tool: Tool, tree: Tree, arguments: Vec<Arg>) -> Self {
        Run {
            tool,
            tree,
            arguments,
            environment: Vec::new(),
            leaves: None,
            finds: Vec::new(),
        }
    }

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

        Run::new(Tool::Cargo, tree, arguments)
    }

    /// A `run` of nextest over `selection`, then `options`.
    pub(crate) fn nextest(selection: Selection, options: &[&str]) -> Self {
        let mut arguments = texts(["run"]);
        arguments.push(Arg::Selection(selection));
        arguments.extend(texts(options.iter().copied()));

        Run::new(Tool::Nextest, Tree::Workspace, arguments)
    }

    /// npm run in `tree`, one of the two npm packages under `editors/`.
    pub(crate) fn npm(tree: Tree, arguments: &[&str]) -> Self {
        Run::new(Tool::Npm, tree, texts(arguments.iter().copied()))
    }

    /// The run with `name` set to `value` in its environment.
    pub(crate) fn with(mut self, name: &'static str, value: Arg) -> Self {
        self.environment.push((name, value));

        self
    }

    /// The run, leaving `product`.
    pub(crate) fn leaving(mut self, product: Product) -> Self {
        self.leaves = Some(product);

        self
    }

    /// The run, which finds `product` by a path of its own rather than one it is handed.
    pub(crate) fn finding(mut self, product: Product) -> Self {
        self.finds.push(product);

        self
    }

    /// The run's command line after its program.
    pub(crate) fn spelled(&self) -> Vec<String> {
        self.arguments.iter().flat_map(Arg::spelled).collect()
    }

    /// The products the run reads: those handed to it, those it finds, its tree's install where it is an npm script, and what a build of its selection reads.
    fn reads(&self, checkout: &Checkout) -> Result<Vec<Product>, String> {
        let mut reads = self
            .arguments
            .iter()
            .chain(self.environment.iter().map(|(_, value)| value))
            .filter_map(|argument| match argument {
                Arg::Product(product) => Some(*product),
                Arg::Text(_) | Arg::Selection(_) => None,
            })
            .chain(self.finds.iter().copied())
            .collect::<Vec<_>>();

        let installed = Product::Installed(self.tree);
        if self.tool == Tool::Npm && self.leaves != Some(installed) {
            reads.push(installed);
        }

        if matches!(self.tool, Tool::Cargo | Tool::Nextest) && self.tree == Tree::Workspace {
            let selections = self.arguments.iter().filter_map(|argument| match argument {
                Arg::Selection(selection) => Some(selection),
                Arg::Text(_) | Arg::Product(_) => None,
            });
            for selection in selections {
                for (product, package) in READ_BY_BUILDS {
                    if checkout.builds(selection, package)? {
                        reads.push(product);
                    }
                }
            }
        }

        Ok(reads)
    }
}

impl From<Run> for Task {
    fn from(run: Run) -> Self {
        Task::Run(run)
    }
}

impl Task {
    /// The product the task leaves, where it leaves one.
    pub(crate) fn leaves(&self) -> Option<Product> {
        match self {
            Task::Run(run) => run.leaves,
            Task::File { product, .. } | Task::Bindgen { product, .. } => Some(*product),
            Task::Write { .. } => None,
            Task::Keeps { during, .. } => during.leaves(),
        }
    }

    /// The products the task reads, what a build reads answered by cargo's graph through `checkout`.
    pub(crate) fn reads(&self, checkout: &Checkout) -> Result<Vec<Product>, String> {
        match self {
            Task::Run(run) => run.reads(checkout),
            Task::File { .. } | Task::Bindgen { .. } | Task::Write { .. } => Ok(Vec::new()),
            Task::Keeps { during, .. } => during.reads(checkout),
        }
    }
}
