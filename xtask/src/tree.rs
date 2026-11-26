//! The task files read into one tree, and every refusal reading them can make.
//!
//! **A file is a group, and a key's meaning is where it sits.** `xtask/tasks.toml` is the group the files in `xtask/tasks/` belong to, each a group named after its file. A file's top level holds `what`, its values and its steps and groups, and the root's holds `tools` and `data` beside them: a table that states its `what` is a task, and every other key a value — words, a command's output (`run`), a key of another file (`read` and `key`), or what the command line gives (`flag`, or `argument` and its `else`) — or a table of values, each named by its path, `.zed.scripts.pins`. A group holds its `what` and its steps and groups; a step holds `what`, `requires`, `consumes`, `produces` and `run`, and no table of its own. So whether a key is an attribute, a value or a child is read off where it sits, and one out of place is refused rather than guessed at.
//!
//! **A name is the path to where it is declared, written from the top with a leading `.`, and it names one thing.** Each segment is a key, read from the top one projection at a time: a step or a group is its tables, `.compiler.lint.clippy`; a value sits at its file's top, `.compiler.edition`; a tool under the root's `tools`, `.tools.node`, and its version beneath it, `.tools.node.version`; data under the root's `data`, `.data.json`; and a produced file under its step's `produces`, `.compiler.launcher.produces.binary`. So a name reads the same wherever it is written, and says where to find what it names. `requires` names steps and groups, `consumes` produced files, and the braces in a command a variable, a tool, data or a produced file, or, with a `!` where the `.` stands, a value the runner fills ([`BUILT_INS`]). Each resolves as the tree is read, so a name that names nothing, one written without its `.`, or one naming the wrong kind of thing, is refused before anything runs, and so is a command that reads a file its step does not consume, which would run without the edge that orders it.

use {
    crate::{Refusal, root},
    std::{
        collections::{BTreeMap, BTreeSet},
        fs,
        ops::Range,
    },
    toml_edit::{Array, Document, Item, TableLike},
};

#[cfg(test)]
pub mod test_support;

#[cfg(test)]
mod data_tests;
#[cfg(test)]
mod instruction_tests;
#[cfg(test)]
mod layout_tests;
#[cfg(test)]
mod name_tests;
#[cfg(test)]
mod tool_tests;

/// The file whose group holds the files in [`DIRECTORY`], from the repository's root.
const ROOT_FILE: &str = "xtask/tasks.toml";

/// The directory whose files are the top's groups, each named after its file.
const DIRECTORY: &str = "xtask/tasks";

/// The values the runner fills, which no file declares: each written after a `!` in a brace, `{!host_triple}`, named as Cargo and `rustc` name what it holds.
const BUILT_INS: [&str; 4] = [
    "host_triple",
    "cargo_target_dir",
    "cargo_home",
    "rust_sysroot",
];

/// The keys a step may hold.
const STEP_KEYS: [&str; 5] = ["what", "requires", "consumes", "produces", "run"];

/// What names a task, a variable or a produced file: its words from the top, as the command line spells a task.
pub type Name = Vec<String>;

/// A name's words joined by dots.
pub fn dotted(name: &[String]) -> String {
    name.join(".")
}

/// A name as a file writes it: the path from the top, with its leading `.`.
pub fn from_top(name: &[String]) -> String {
    format!(".{}", dotted(name))
}

/// A name as the command line spells it, with spaces.
pub fn spelled(name: &[String]) -> String {
    name.join(" ")
}

/// The task files, read and resolved.
#[derive(Debug)]
pub struct Tree {
    /// Every step, in the order the tree writes them: the root file's own, then each file of [`DIRECTORY`] by name, each in its own order.
    pub steps: Vec<Step>,
    /// Every group by its name, the top's included under the empty name: the steps it holds, at any depth, in order.
    pub groups: BTreeMap<Name, Group>,
    pub variables: BTreeMap<Name, Variable>,
    /// What the command line may give a run, by name.
    pub given: BTreeMap<Name, Given>,
    pub produced: BTreeMap<Name, Produced>,
    pub tools: BTreeMap<Name, Tool>,
    pub data: BTreeMap<Name, Data>,
}

#[derive(Debug)]
pub struct Group {
    /// What the group is for, as it states it: a file's at its top, a group's inside a file under its own `what`.
    pub description: String,
    /// The groups and steps directly inside it, in order.
    pub children: Vec<Name>,
    /// The steps it holds, at any depth, in order, by their place in [`Tree::steps`].
    pub steps: Vec<usize>,
}

#[derive(Debug)]
pub struct Step {
    pub name: Name,
    pub description: String,
    /// The steps and groups that always run first, resolved.
    pub requires: Vec<Name>,
    /// The files other steps produce that this one reads, resolved.
    pub consumes: Vec<Name>,
    /// The files this step makes, by their names.
    pub produces: Vec<Name>,
    /// What it runs, in order.
    pub run: Vec<Instruction>,
}

/// One thing a step runs.
#[derive(Debug)]
pub enum Instruction {
    /// A command: the words a person would type, the program first, and the directory it runs in, from the repository's root, where it states one, `{ in = directory, do = words }`.
    Command {
        words: Vec<Word>,
        directory: Option<Word>,
    },
    /// A file written whole: where, under its directory where it states one, `{ in = directory, write = path, contents = text }`, and from the repository's root else_, and what it holds.
    Write {
        path: Word,
        contents: Word,
        directory: Option<Word>,
    },
}

impl Instruction {
    /// Every word the instruction holds.
    pub fn words(&self) -> impl Iterator<Item = &Word> {
        match self {
            Instruction::Command { words, directory } => {
                words.iter().chain(directory).collect::<Vec<_>>()
            }
            Instruction::Write {
                path,
                contents,
                directory,
            } => [path, contents].into_iter().chain(directory).collect(),
        }
        .into_iter()
    }
}

/// The keys a write holds, besides which it holds none.
const WRITE_KEYS: [&str; 4] = ["in", "write", "contents", "template"];

/// The keys a command written as a table holds, besides which it holds none.
const COMMAND_KEYS: [&str; 2] = ["in", "do"];

/// A file a step makes: where it is, and which step makes it.
#[derive(Debug)]
pub struct Produced {
    pub path: Word,
    pub producer: usize,
}

/// An executable a step names, `{.tools.node}`, kept under `xtask/.artifacts` before a run's first step.
#[derive(Debug)]
pub enum Tool {
    /// One fetched itself, from a download of its own on each host.
    Fetched(Fetched),
    /// One another tool's download holds, at `path` inside it: npm in Node's.
    Shipped { archive: Name, path: String },
}

/// A tool fetched from a download of its own.
#[derive(Debug)]
pub struct Fetched {
    pub version: String,
    /// Each host's download, by its triple.
    pub downloads: BTreeMap<String, Published>,
    /// Where the executable sits in a download kept as a directory: the tool's own name where the files write none.
    pub path: String,
}

/// Where a host fetches a tool from, the digest its download has, and how it is kept.
#[derive(Debug)]
pub struct Published {
    pub url: Word,
    pub sha256: String,
    pub shape: Shape,
}

/// A directory of data a step or a build reads, `{.data.json}`, kept beside what reads it, checked by the digest of its files, the same on every host: fetched before a run's first step where a step of the run names it, and before any run's where a crate's build reads it.
#[derive(Debug)]
pub struct Data {
    /// The version the data is pinned at: a commit, where a forge generates its archive.
    pub version: String,
    /// Where it is published: a gzipped tarball, or the directory its files stand under.
    pub url: Word,
    /// How it is published and what of it is kept.
    pub form: Form,
    /// The digest of the files the kept directory holds, as [`tree_sha256_hex`](crate::tree_sha256_hex) writes it.
    pub sha256: String,
    /// The crate whose build reads it, by its directory at the repository's root: the data is kept beside it, `<crate>/.artifacts/<name>-<version>`, where its build finds it through its own manifest. `None` where a step's command reads it, and it is kept with the runner's own, `xtask/.artifacts/data`.
    pub beside: Option<String>,
}

/// How data is published and what of it is kept, as its table states it: an archive, whole or the `path` of one directory of it, or `files`.
#[derive(Debug, PartialEq, Eq)]
pub enum Form {
    /// One directory of a gzipped tarball, from the archive's top, its sole top directory taken as the contents: the whole of it where the path is empty, which is what an archive stating no `path` keeps.
    Directory(String),
    /// Files published one by one, each at its path under the URL, kept at that path.
    Files(Vec<String>),
}

/// The keys data holds, of which an archive may state the `path` it keeps and data that is no archive states its `files`, and `beside` where a crate's build reads it.
const DATA_KEYS: [&str; 6] = ["version", "url", "path", "files", "sha256", "beside"];

/// How a download is kept, read off the end of its URL: a gzipped tarball, `.tar.gz`, is unpacked into a directory; a gzipped file, `.gz`, is decoded into the executable; anything else is the executable as it was downloaded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Shape {
    Tarball,
    Gzipped,
    Bare,
}

/// Where a tool is declared: once, at the root's top, where every tool the steps run stands together.
const TOOLS_AT_THE_ROOT: &str =
    "`tools` belongs at the top of `xtask/tasks.toml`, where every tool is declared";

/// Where data is declared: at the root's top, beside the tools, whose fetch it shares.
const DATA_AT_THE_ROOT: &str =
    "`data` belongs at the top of `xtask/tasks.toml`, where all data is declared";

/// The keys a tool fetched itself holds, and those of one shipped in another's download.
const FETCHED_KEYS: [&str; 4] = ["version", "url", "sha256", "path"];
const SHIPPED_KEYS: [&str; 2] = ["archive", "path"];

/// A named value of a file, the same for the whole run.
#[derive(Debug, PartialEq, Eq)]
pub enum Variable {
    /// Words as written, each of which may hold a value the runner fills ([`runner_word`]): a string is one, a list each of its items.
    Words(Vec<Word>),
    /// What a command prints, a word to a line, asked once a run and only where a step that uses it runs.
    Output(Vec<String>),
    /// The string a key of a TOML file holds, `key` its keys from the file's top, read once a run and only where a step that uses it runs: a value another file owns, stated there alone.
    Read { path: String, key: Vec<String> },
}

impl Variable {
    /// Whether the variable always stands for one word, so it may sit inside a word beside other text.
    fn is_one_word(&self) -> bool {
        match self {
            Variable::Words(words) => words.len() == 1,
            Variable::Read { .. } => true,
            Variable::Output(_) => false,
        }
    }
}

/// What the command line may give a run, which a file's `given` declares: a value the person running chose, which also narrows the run to the steps that read it, where a variable is a constant of the run.
#[derive(Debug, PartialEq, Eq)]
pub enum Given {
    /// Given with a value by any of `flags`, a short spelling, a long one or both: the flag and the value for each time it is given, nothing where it is not. Whether the command takes it more than once is the command's to say.
    Flag { flags: Vec<String>, help: String },
    /// Given as a positional argument: its `else` where it is not given and states one, and required where it states none, the command line refusing it missing before anything runs.
    Argument {
        placeholder: String,
        else_: Option<String>,
        help: String,
    },
}

impl Given {
    /// Whether the value always stands for one word, so it may sit inside a word beside other text.
    fn is_one_word(&self) -> bool {
        match self {
            Given::Argument { .. } => true,
            Given::Flag { .. } => false,
        }
    }
}

/// One word of a command: text, and the values its braces stand for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Word(pub Vec<Piece>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Piece {
    Text(String),
    Value(Fill),
}

/// What a pair of braces stands for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fill {
    /// The triple `xtask` was built for, `!host_triple`.
    HostTriple,
    /// Cargo's target directory, `!cargo_target_dir`.
    CargoTargetDir,
    /// Cargo's home, `!cargo_home`.
    CargoHome,
    /// The toolchain's root, as the pinned `rustc` prints it, `!rust_sysroot`.
    RustSysroot,
    Variable(Name),
    /// A value the command line gives a run.
    Given(Name),
    /// A produced file's path.
    Produced(Name),
    /// A tool's executable, kept for the host the run is on.
    Tool(Name),
    /// The directory data is kept as.
    Data(Name),
}

impl Fill {
    /// What a task file writes between the braces: a built-in by its `!` name, and any other value by its path from the top.
    pub fn written(&self) -> String {
        match self {
            Fill::HostTriple => "!host_triple".to_string(),
            Fill::CargoTargetDir => "!cargo_target_dir".to_string(),
            Fill::CargoHome => "!cargo_home".to_string(),
            Fill::RustSysroot => "!rust_sysroot".to_string(),
            Fill::Variable(name)
            | Fill::Given(name)
            | Fill::Produced(name)
            | Fill::Tool(name)
            | Fill::Data(name) => from_top(name),
        }
    }
}

impl Word {
    /// The word that is `text` and holds no value.
    pub fn text(text: String) -> Self {
        Self(vec![Piece::Text(text)])
    }

    /// The word as a task file writes it, each value by its braces: what a reader looks the value up by.
    pub fn written(&self) -> String {
        self.0
            .iter()
            .map(|piece| match piece {
                Piece::Text(text) => text.clone(),
                Piece::Value(fill) => format!("{{{}}}", fill.written()),
            })
            .collect()
    }

    /// The values the word's braces stand for.
    pub fn fills(&self) -> impl Iterator<Item = &Fill> {
        self.0.iter().filter_map(|piece| match piece {
            Piece::Value(fill) => Some(fill),
            Piece::Text(_) => None,
        })
    }

    /// The one value the word is, where it is nothing but a pair of braces.
    pub fn alone(&self) -> Option<&Fill> {
        match self.0.as_slice() {
            [Piece::Value(fill)] => Some(fill),
            _ => None,
        }
    }
}

/// One file as read: its text, as a refusal quotes it, and the name its group takes.
struct File<'a> {
    path: &'a str,
    text: &'a str,
    scope: Name,
}

impl File<'_> {
    /// A refusal about `span` of this file, or the whole file where it has none.
    fn refuse(&self, span: Option<Range<usize>>, message: String) -> Refusal {
        match span {
            Some(span) => Refusal::at(self.path, self.text, span, message),
            None => Refusal::about(self.path, message),
        }
    }
}

/// A step as written, before its names resolve.
struct Written<'a> {
    file: usize,
    name: Name,
    table: &'a dyn TableLike,
    span: Option<Range<usize>>,
}

/// What a name declares.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Task,
    Variable,
    Given,
    Produced,
    Tool,
    Data,
}

impl Tree {
    /// The task files of the checkout, read and resolved.
    pub fn read() -> Result<Self, Refusal> {
        let read = |path: &str| {
            fs::read_to_string(root().join(path))
                .map_err(|error| Refusal::about(path, format!("cannot be read: {error}")))
        };

        let mut files = vec![(ROOT_FILE.to_string(), read(ROOT_FILE)?)];
        let directory = root().join(DIRECTORY);
        let mut names = fs::read_dir(&directory)
            .map_err(|error| Refusal::about(DIRECTORY, format!("cannot be read: {error}")))?
            .map(|entry| {
                entry
                    .map(|entry| entry.file_name().to_string_lossy().into_owned())
                    .map_err(|error| Refusal::about(DIRECTORY, format!("cannot be read: {error}")))
            })
            .collect::<Result<Vec<_>, _>>()?;
        // A task file is a `.toml`, and what else stands beside them, as the programs a subject's steps run do, is none of the runner's: a `.toml` that does not parse is still refused where it is read.
        names.retain(|name| name.ends_with(".toml"));
        names.sort();
        for name in names {
            let path = format!("{DIRECTORY}/{name}");
            let text = read(&path)?;
            files.push((path, text));
        }

        Tree::parse(
            &files
                .iter()
                .map(|(path, text)| (path.as_str(), text.as_str()))
                .collect::<Vec<_>>(),
        )
    }

    /// The tree the files `(path, text)` make: the root file, then the files of [`DIRECTORY`], each a group named after it.
    pub fn parse(files: &[(&str, &str)]) -> Result<Self, Refusal> {
        let files = files
            .iter()
            .map(|(path, text)| File {
                path,
                text,
                scope: match path.strip_prefix(&format!("{DIRECTORY}/")) {
                    Some(name) => vec![name.trim_end_matches(".toml").to_string()],
                    None => Vec::new(),
                },
            })
            .collect::<Vec<_>>();
        let documents = files
            .iter()
            .map(|file| {
                Document::parse(file.text).map_err(|error| {
                    file.refuse(error.span(), error.message().trim_end().to_string())
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut tree = Tree {
            steps: Vec::new(),
            groups: BTreeMap::new(),
            variables: BTreeMap::new(),
            given: BTreeMap::new(),
            produced: BTreeMap::new(),
            tools: BTreeMap::new(),
            data: BTreeMap::new(),
        };
        let mut kinds = BTreeMap::<Name, Kind>::new();
        let mut written = Vec::new();
        let mut tools = Vec::new();
        let mut data = Vec::new();

        // The first pass declares every name, so the second resolves against the whole tree whatever order its files come in.
        for (index, (file, document)) in files.iter().zip(&documents).enumerate() {
            let top = document.as_table();
            let mut description = None;
            let mut children = Vec::new();
            for (key, item) in top.iter() {
                let span = top.key(key).and_then(|key| key.span());
                match key {
                    "what" => description = Some(text_of(file, item, "what")?),
                    "tools" => {
                        if !file.scope.is_empty() {
                            return Err(file.refuse(span, TOOLS_AT_THE_ROOT.to_string()));
                        }
                        let table = item.as_table_like().ok_or_else(|| {
                            file.refuse(
                                item.span(),
                                "`tools` is a table of tools by name".to_string(),
                            )
                        })?;
                        for (key, value) in table.iter() {
                            let name = scoped(&scoped(&file.scope, "tools"), key);
                            let span = table.key(key).and_then(|key| key.span());
                            declare(&mut kinds, file, &name, Kind::Tool, span)?;
                            let entry = value.as_table_like().ok_or_else(|| {
                                file.refuse(
                                    value.span(),
                                    format!("tool `{key}` is a table: its `version`, `url` and `sha256`, or the `archive` it ships in"),
                                )
                            })?;
                            // A tool's version is a value its downloads' URLs write, `{.tools.node.version}`.
                            if let Some(version) = entry.get("version") {
                                let version_name = scoped(&name, "version");
                                declare(
                                    &mut kinds,
                                    file,
                                    &version_name,
                                    Kind::Variable,
                                    version.span(),
                                )?;
                                let version = text_of(file, version, "version")?;
                                tree.variables.insert(
                                    version_name,
                                    Variable::Words(vec![Word::text(version)]),
                                );
                            }
                            tools.push(Written {
                                file: index,
                                name,
                                table: entry,
                                span: value.span(),
                            });
                        }
                    }
                    "data" => {
                        if !file.scope.is_empty() {
                            return Err(file.refuse(span, DATA_AT_THE_ROOT.to_string()));
                        }
                        let table = item.as_table_like().ok_or_else(|| {
                            file.refuse(
                                item.span(),
                                "`data` is a table of data by name".to_string(),
                            )
                        })?;
                        for (key, value) in table.iter() {
                            let name = scoped(&scoped(&file.scope, "data"), key);
                            let span = table.key(key).and_then(|key| key.span());
                            declare(&mut kinds, file, &name, Kind::Data, span)?;
                            let entry = value.as_table_like().ok_or_else(|| {
                                file.refuse(
                                    value.span(),
                                    format!("data `{key}` is a table: its `version`, `url`, `sha256`, and its `path` or `files`"),
                                )
                            })?;
                            // Its version is a value its URL writes, `{.data.json.version}`.
                            if let Some(version) = entry.get("version") {
                                let version_name = scoped(&name, "version");
                                declare(
                                    &mut kinds,
                                    file,
                                    &version_name,
                                    Kind::Variable,
                                    version.span(),
                                )?;
                                let version = text_of(file, version, "version")?;
                                tree.variables.insert(
                                    version_name,
                                    Variable::Words(vec![Word::text(version)]),
                                );
                            }
                            data.push(Written {
                                file: index,
                                name,
                                table: entry,
                                span: value.span(),
                            });
                        }
                    }
                    // Every other key is a value of the file's or a task: a table that states its `what` is a task, and so is any table no value's keys make.
                    _ if valued(item) || of_values(item) => {
                        values_of(
                            file,
                            scoped(&file.scope, key),
                            item,
                            span,
                            &mut kinds,
                            &mut tree,
                        )?;
                    }
                    _ => {
                        let name = scoped(&file.scope, key);
                        declare(&mut kinds, file, &name, Kind::Task, span.clone())?;
                        let table = item.as_table_like().ok_or_else(|| {
                            file.refuse(
                                span.clone(),
                                format!("`{key}` is out of place: a file's top holds its `what`, its values, and its steps and groups as tables"),
                            )
                        })?;
                        children.push(name.clone());
                        walk(
                            file,
                            index,
                            name,
                            table,
                            item.span(),
                            &mut tree,
                            &mut written,
                        )?;
                    }
                }
            }

            let description = description.ok_or_else(|| {
                file.refuse(None, "has no `what`, which a file's top states".to_string())
            })?;
            tree.groups.insert(
                file.scope.clone(),
                Group {
                    description,
                    children,
                    steps: Vec::new(),
                },
            );
            if !file.scope.is_empty() {
                tree.groups
                    .get_mut(&Vec::new())
                    .expect("the root file comes first")
                    .children
                    .push(file.scope.clone());
                declare(&mut kinds, &files[0], &file.scope, Kind::Task, None)?;
            }
        }

        for step in &written {
            let file = &files[step.file];
            if let Some(produces) = step.table.get("produces") {
                let table = produces.as_table_like().ok_or_else(|| {
                    file.refuse(
                        produces.span(),
                        "`produces` is a table of names and paths".to_string(),
                    )
                })?;
                for (name, value) in table.iter() {
                    let name = scoped(&scoped(&step.name, "produces"), name);
                    declare(&mut kinds, file, &name, Kind::Produced, value.span())?;
                }
            }
        }

        for tool in &tools {
            let read = tool_of(
                &files[tool.file],
                tool,
                &kinds,
                &tree.variables,
                &tree.given,
            )?;
            tree.tools.insert(tool.name.clone(), read);
        }
        tree.refuse_tools(&files, &tools)?;
        for written in &data {
            let read = data_of(
                &files[written.file],
                written,
                &kinds,
                &tree.variables,
                &tree.given,
            )?;
            tree.data.insert(written.name.clone(), read);
        }

        // The second pass resolves each step's names against every declaration.
        let mut steps = Vec::new();
        for (index, step) in written.iter().enumerate() {
            let file = &files[step.file];
            let resolve = |text: &str, span: Option<Range<usize>>| {
                let name =
                    resolved(text).ok_or_else(|| file.refuse(span.clone(), unrooted(text)))?;
                match kinds.get(&name) {
                    Some(kind) => Ok((name, *kind)),
                    None => Err(file.refuse(span, format!("`{text}` names nothing"))),
                }
            };

            let description = match step.table.get("what") {
                Some(item) => text_of(file, item, "what")?,
                None => {
                    return Err(file.refuse(
                        step.span.clone(),
                        format!("step `{}` has no `what`", from_top(&step.name)),
                    ));
                }
            };

            let mut requires = Vec::new();
            for (text, span) in strings_of(file, step.table.get("requires"), "requires")? {
                match resolve(&text, span.clone())? {
                    (name, Kind::Task) => requires.push(name),
                    _ => {
                        return Err(file.refuse(
                            span,
                            format!("`{text}` is no step or group: a file a step reads belongs in `consumes`"),
                        ));
                    }
                }
            }

            let mut consumes = Vec::new();
            for (text, span) in strings_of(file, step.table.get("consumes"), "consumes")? {
                match resolve(&text, span.clone())? {
                    (name, Kind::Produced) => consumes.push(name),
                    _ => {
                        return Err(file.refuse(
                            span,
                            format!("`{text}` is no produced file: a step another must run after belongs in `requires`"),
                        ));
                    }
                }
            }

            let mut produces = Vec::new();
            if let Some(table) = step.table.get("produces").and_then(Item::as_table_like) {
                for (name, value) in table.iter() {
                    let path = value.as_str().ok_or_else(|| {
                        file.refuse(
                            value.span(),
                            format!("`{name}` is produced at a path, a string"),
                        )
                    })?;
                    let path = word_of(
                        file,
                        path,
                        value.span(),
                        &kinds,
                        &tree.variables,
                        &tree.given,
                    )?;
                    let name = scoped(&scoped(&step.name, "produces"), name);
                    tree.produced.insert(
                        name.clone(),
                        Produced {
                            path,
                            producer: index,
                        },
                    );
                    produces.push(name);
                }
            }

            // A word of what the step runs, held to reading only the produced files the step is ordered after.
            let word = |word: &toml_edit::Value| -> Result<Word, Refusal> {
                let text = word
                    .as_str()
                    .ok_or_else(|| file.refuse(word.span(), "a word is a string".to_string()))?;
                let word_value = word_of(
                    file,
                    text,
                    word.span(),
                    &kinds,
                    &tree.variables,
                    &tree.given,
                )?;
                for fill in word_value.fills() {
                    if let Fill::Produced(name) = fill
                        && !consumes.contains(name)
                        && !produces.contains(name)
                    {
                        return Err(file.refuse(
                            word.span(),
                            format!("`{}` is a file this step neither consumes nor produces, so nothing orders it after the step that makes it", from_top(name)),
                        ));
                    }
                }
                Ok(word_value)
            };
            let mut run = Vec::new();
            if let Some(item) = step.table.get("run") {
                let instructions = item.as_array().ok_or_else(|| {
                    file.refuse(item.span(), "`run` is a list of instructions".to_string())
                })?;
                for instruction in instructions.iter() {
                    if let Some(table) = instruction.as_inline_table() {
                        run.push(match table.contains_key("do") {
                            true => command_of(
                                file,
                                table,
                                instruction.span(),
                                &tree.variables,
                                &tree.given,
                                &word,
                            )?,
                            false => write_of(
                                file,
                                table,
                                instruction.span(),
                                &kinds,
                                &tree.variables,
                                &tree.given,
                                &word,
                            )?,
                        });
                        continue;
                    }
                    let words = instruction
                        .as_array()
                        .filter(|words| !words.is_empty())
                        .ok_or_else(|| {
                            file.refuse(
                                instruction.span(),
                                "an instruction is a command, a list of words or `{ in = directory, do = words }`, or a write, `{ write = path, contents = text }`".to_string(),
                            )
                        })?;
                    let words = words.iter().map(&word).collect::<Result<Vec<_>, _>>()?;
                    run.push(Instruction::Command {
                        words,
                        directory: None,
                    });
                }
            }

            if run.is_empty() && requires.is_empty() {
                return Err(file.refuse(
                    step.span.clone(),
                    format!(
                        "step `{}` neither runs nor requires anything",
                        from_top(&step.name)
                    ),
                ));
            }

            steps.push(Step {
                name: step.name.clone(),
                description,
                requires,
                consumes,
                produces,
                run,
            });
        }
        tree.steps = steps;

        for (index, step) in tree.steps.iter().enumerate() {
            for ancestor in (0..step.name.len()).map(|length| step.name[..length].to_vec()) {
                if let Some(group) = tree.groups.get_mut(&ancestor) {
                    group.steps.push(index);
                }
            }
        }

        tree.refuse_cycles(&files, &written)?;

        Ok(tree)
    }

    /// The steps `name` stands for: itself where it is a step, every step it holds where it is a group.
    pub fn steps_of(&self, name: &[String]) -> Vec<usize> {
        match self.groups.get(name) {
            Some(group) => group.steps.clone(),
            None => self
                .steps
                .iter()
                .position(|step| step.name == name)
                .into_iter()
                .collect(),
        }
    }

    /// The steps `step` runs after: every step its requirements stand for, and the producer of each file it consumes.
    pub fn prerequisites(&self, step: usize) -> Vec<usize> {
        let step = &self.steps[step];
        let mut after = Vec::new();
        for required in &step.requires {
            for other in self.steps_of(required) {
                if !after.contains(&other) {
                    after.push(other);
                }
            }
        }
        for consumed in &step.consumes {
            let producer = self.produced[consumed].producer;
            if !after.contains(&producer) {
                after.push(producer);
            }
        }

        after
    }

    /// The command-line variables a step's instructions use.
    pub fn given_to(&self, step: usize) -> BTreeSet<Name> {
        self.steps[step]
            .run
            .iter()
            .flat_map(Instruction::words)
            .flat_map(Word::fills)
            .filter_map(|fill| match fill {
                Fill::Given(name) => Some(name.clone()),
                _ => None,
            })
            .collect()
    }

    /// Refuse a tool shipped in one kept as a lone executable, or in one shipped itself.
    fn refuse_tools(&self, files: &[File<'_>], written: &[Written<'_>]) -> Result<(), Refusal> {
        for tool in written {
            let file = &files[tool.file];
            let Tool::Shipped { archive, .. } = &self.tools[&tool.name] else {
                continue;
            };
            match &self.tools[archive] {
                Tool::Fetched(owner)
                    if owner
                        .downloads
                        .values()
                        .all(|published| published.shape == Shape::Tarball) => {}
                Tool::Fetched(_) => {
                    return Err(file.refuse(
                        tool.span.clone(),
                        format!("`{}` is kept as a lone executable on some host, so nothing ships beside it", from_top(archive)),
                    ));
                }
                Tool::Shipped { .. } => {
                    return Err(file.refuse(
                        tool.span.clone(),
                        format!(
                            "`{}` ships in another tool's download itself",
                            from_top(archive)
                        ),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Refuse a step that runs after itself, through what it requires or consumes.
    fn refuse_cycles(&self, files: &[File<'_>], written: &[Written<'_>]) -> Result<(), Refusal> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Mark {
            Unvisited,
            Visiting,
            Done,
        }

        fn visit(
            tree: &Tree,
            step: usize,
            marks: &mut [Mark],
            trail: &mut Vec<usize>,
        ) -> Option<Vec<usize>> {
            match marks[step] {
                Mark::Done => return None,
                Mark::Visiting => {
                    let start = trail.iter().position(|other| *other == step).unwrap_or(0);
                    return Some(trail[start..].to_vec());
                }
                Mark::Unvisited => {}
            }

            marks[step] = Mark::Visiting;
            trail.push(step);
            for other in tree.prerequisites(step) {
                if let Some(cycle) = visit(tree, other, marks, trail) {
                    return Some(cycle);
                }
            }
            trail.pop();
            marks[step] = Mark::Done;

            None
        }

        let mut marks = vec![Mark::Unvisited; self.steps.len()];
        for step in 0..self.steps.len() {
            if let Some(cycle) = visit(self, step, &mut marks, &mut Vec::new()) {
                let first = &written[cycle[0]];
                return Err(files[first.file].refuse(
                    first.span.clone(),
                    format!(
                        "these steps run after each other: {}",
                        cycle
                            .iter()
                            .map(|step| from_top(&self.steps[*step].name))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                ));
            }
        }

        Ok(())
    }
}

/// The name of `key` declared under the table `scope` names.
fn scoped(scope: &[String], key: &str) -> Name {
    let mut name = scope.to_vec();
    name.push(key.to_string());
    name
}

/// A name as written: the keys from the top after its leading `.`, and `None` for one written without it.
fn resolved(text: &str) -> Option<Name> {
    text.strip_prefix('.')
        .map(|path| path.split('.').map(str::to_string).collect())
}

/// What refuses a name written without its leading `.`.
fn unrooted(text: &str) -> String {
    format!(
        "`{text}` is no path: a name is written from the top with a leading `.`, as `.compiler.edition`"
    )
}

/// Declare `name` as a `kind`, refusing a name already declared.
fn declare(
    kinds: &mut BTreeMap<Name, Kind>,
    file: &File<'_>,
    name: &Name,
    kind: Kind,
    span: Option<Range<usize>>,
) -> Result<(), Refusal> {
    if kinds.insert(name.clone(), kind).is_some() {
        return Err(file.refuse(span, format!("`{}` is declared twice", from_top(name))));
    }

    Ok(())
}

/// The keys a value's table holds, besides which it holds none: a command whose output it is, `run`; another file's key, `read` and `key`; or what the command line gives, `flag`, or `argument` with its `else`; and the `help` a given value states.
const VALUE_KEYS: [&str; 7] = ["run", "read", "key", "flag", "argument", "else", "help"];

/// Whether `item`, at a file's top, is a value rather than a task: a string or a list of words, or a table that states no `what` and holds a value's keys alone, a `run` among them a list of words rather than of instructions.
fn valued(item: &Item) -> bool {
    match item.as_table_like() {
        None => {
            item.is_str()
                || item
                    .as_array()
                    .is_some_and(|words| words.iter().all(|word| word.is_str()))
        }
        Some(table) => {
            !table.is_empty()
                && table.iter().all(|(key, _)| VALUE_KEYS.contains(&key))
                && table.get("run").is_none_or(|run| {
                    run.as_array()
                        .is_some_and(|words| words.iter().all(|word| word.is_str()))
                })
        }
    }
}

/// Whether `item`, at a file's top, is a table of values: a table that is no value and holds no key of a task's, `what` among them, each of whose entries is a value or a table of values in turn, named by its path, `.zed.scripts.pins`. A table holding a task's key is a task, refused where it states no `what`.
fn of_values(item: &Item) -> bool {
    item.as_table_like().is_some_and(|table| {
        !table.is_empty()
            && !table.iter().any(|(key, _)| STEP_KEYS.contains(&key))
            && table
                .iter()
                .all(|(_, entry)| valued(entry) || of_values(entry))
    })
}

/// Declare `item`, at `name`: a value, the command line's where it is a `flag` or an `argument` and a constant of the run otherwise, or a table of values, each declared at its path below `name`.
fn values_of(
    file: &File<'_>,
    name: Name,
    item: &Item,
    span: Option<Range<usize>>,
    kinds: &mut BTreeMap<Name, Kind>,
    tree: &mut Tree,
) -> Result<(), Refusal> {
    if !valued(item)
        && let Some(table) = item.as_table_like()
    {
        for (key, entry) in table.iter() {
            let span = table.key(key).and_then(|key| key.span());
            values_of(file, scoped(&name, key), entry, span, kinds, tree)?;
        }

        return Ok(());
    }
    match item.get("flag").or(item.get("argument")) {
        Some(_) => {
            declare(kinds, file, &name, Kind::Given, span)?;
            let given = given_of(file, item)?;
            tree.given.insert(name, given);
        }
        None => {
            declare(kinds, file, &name, Kind::Variable, span)?;
            let variable = variable_of(file, item)?;
            tree.variables.insert(name, variable);
        }
    }

    Ok(())
}

/// Walk the table `name`: a step where it holds a step's keys, a group of steps and groups otherwise.
fn walk<'a>(
    file: &File<'_>,
    index: usize,
    name: Name,
    table: &'a dyn TableLike,
    span: Option<Range<usize>>,
    tree: &mut Tree,
    written: &mut Vec<Written<'a>>,
) -> Result<(), Refusal> {
    // A table holding tables other than a step's own is a group, whose `what` is its own; any other is a step.
    let is_step = table
        .iter()
        .any(|(key, _)| key != "what" && STEP_KEYS.contains(&key))
        || !table
            .iter()
            .any(|(key, item)| !STEP_KEYS.contains(&key) && item.is_table_like());
    if is_step {
        for (key, item) in table.iter() {
            if !STEP_KEYS.contains(&key) {
                let span = table.key(key).and_then(|key| key.span());
                return Err(file.refuse(
                    span,
                    match key {
                        "tools" => TOOLS_AT_THE_ROOT.to_string(),
                        _ if item.is_table() => {
                            format!("`{key}` is a table inside a step, which holds no tables")
                        }
                        _ => format!(
                            "`{key}` is no key of a step, and a value belongs at a file's top"
                        ),
                    },
                ));
            }
        }
        written.push(Written {
            file: index,
            name,
            table,
            span,
        });
        return Ok(());
    }

    if table.is_empty() {
        return Err(file.refuse(span, format!("`{}` is empty", from_top(&name))));
    }
    let description = match table.get("what") {
        Some(item) => text_of(file, item, "what")?,
        None => {
            return Err(file.refuse(
                span,
                format!(
                    "group `{}` has no `what`, which a group states of itself",
                    from_top(&name)
                ),
            ));
        }
    };
    tree.groups.insert(
        name.clone(),
        Group {
            description,
            children: Vec::new(),
            steps: Vec::new(),
        },
    );
    for (key, item) in table.iter().filter(|(key, _)| *key != "what") {
        let span = table.key(key).and_then(|key| key.span());
        let child = item.as_table_like().ok_or_else(|| {
            file.refuse(
                span.clone(),
                format!(
                    "`{key}` is out of place: a group holds its `what` and its steps and groups"
                ),
            )
        })?;
        let child_name = scoped(&name, key);
        tree.groups
            .get_mut(&name)
            .expect("the group was just inserted")
            .children
            .push(child_name.clone());
        walk(file, index, child_name, child, item.span(), tree, written)?;
    }

    Ok(())
}

/// The string `item` holds, as the value of `key`.
fn text_of(file: &File<'_>, item: &Item, key: &str) -> Result<String, Refusal> {
    item.as_str()
        .map(str::to_string)
        .ok_or_else(|| file.refuse(item.span(), format!("`{key}` is a string")))
}

/// The spellings `item` states for a flag: one string, or a list of a short spelling and a long one, `-E` and `--filterset`, each kind once.
fn flags_of(file: &File<'_>, item: &Item) -> Result<Vec<String>, Refusal> {
    let spellings = match (item.as_str(), item.as_array()) {
        (Some(spelling), _) => vec![spelling.to_string()],
        (None, Some(list)) => words_of(file, list)?,
        (None, None) => Vec::new(),
    };
    let named = |name: &str| name.starts_with(char::is_alphanumeric);
    let long = |spelling: &&String| spelling.strip_prefix("--").is_some_and(named);
    let short = |spelling: &&String| {
        spelling.chars().count() == 2 && spelling.strip_prefix('-').is_some_and(named)
    };

    match (
        spellings.len(),
        spellings.iter().filter(short).count(),
        spellings.iter().filter(long).count(),
    ) {
        (1, 1, 0) | (1, 0, 1) | (2, 1, 1) => Ok(spellings),
        _ => Err(file.refuse(
            item.span(),
            "a flag is one spelling, as `-E` or `--filterset`, or a list of a short one and a long one".to_string(),
        )),
    }
}

/// A string as written, with where it stands.
type Placed = (String, Option<Range<usize>>);

/// The strings of the list `item` holds, as the value of `key`, each with where it stands.
fn strings_of(file: &File<'_>, item: Option<&Item>, key: &str) -> Result<Vec<Placed>, Refusal> {
    let Some(item) = item else {
        return Ok(Vec::new());
    };
    let list = item
        .as_array()
        .ok_or_else(|| file.refuse(item.span(), format!("`{key}` is a list of names")))?;

    list.iter()
        .map(|value| {
            value
                .as_str()
                .map(|text| (text.to_string(), value.span()))
                .ok_or_else(|| file.refuse(value.span(), format!("`{key}` is a list of names")))
        })
        .collect()
}

/// The words of a list, each a string.
fn words_of(file: &File<'_>, list: &Array) -> Result<Vec<String>, Refusal> {
    list.iter()
        .map(|value| {
            value
                .as_str()
                .map(str::to_string)
                .ok_or_else(|| file.refuse(value.span(), "a word is a string".to_string()))
        })
        .collect()
}

/// The tool `written` declares, its URLs resolved against every declaration.
fn tool_of(
    file: &File<'_>,
    written: &Written<'_>,
    kinds: &BTreeMap<Name, Kind>,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
) -> Result<Tool, Refusal> {
    let table = written.table;
    let key = written.name.last().expect("a tool is named");
    let only = |allowed: &[&str]| match table.iter().find(|(key, _)| !allowed.contains(key)) {
        Some((other, _)) => Err(file.refuse(
            table.key(other).and_then(|key| key.span()),
            format!(
                "`{other}` is no key of tool `{key}`, which holds {}",
                allowed
                    .iter()
                    .map(|key| format!("`{key}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        )),
        None => Ok(()),
    };
    let path = table
        .get("path")
        .map(|item| text_of(file, item, "path"))
        .transpose()?;

    if let Some(archive) = table.get("archive") {
        only(&SHIPPED_KEYS)?;
        let text = text_of(file, archive, "archive")?;
        let name = resolved(&text).ok_or_else(|| file.refuse(archive.span(), unrooted(&text)))?;
        if kinds.get(&name) != Some(&Kind::Tool) {
            return Err(file.refuse(archive.span(), format!("`{text}` is no tool")));
        }
        let path = path.ok_or_else(|| {
            file.refuse(
                written.span.clone(),
                format!("tool `{key}` ships in `{text}`, and names its `path` there"),
            )
        })?;

        return Ok(Tool::Shipped {
            archive: name,
            path,
        });
    }

    only(&FETCHED_KEYS)?;
    let version = table.get("version").ok_or_else(|| {
        file.refuse(
            written.span.clone(),
            format!("tool `{key}` states its `version`, or the `archive` it ships in"),
        )
    })?;
    let urls = hosts_of(file, table.get("url"), "url")?;
    let sums = hosts_of(file, table.get("sha256"), "sha256")?;
    if urls.is_empty() {
        return Err(file.refuse(
            written.span.clone(),
            format!("tool `{key}` states a `url` and a `sha256` for each host it runs on"),
        ));
    }
    if let Some((triple, item)) = sums
        .iter()
        .find(|(triple, _)| !urls.iter().any(|(other, _)| other == triple))
    {
        return Err(file.refuse(
            item.span(),
            format!("`sha256.{triple}` has no `url.{triple}` beside it"),
        ));
    }

    let mut downloads = BTreeMap::new();
    for (triple, item) in urls {
        let text = item
            .as_str()
            .ok_or_else(|| file.refuse(item.span(), format!("`url.{triple}` is a string")))?;
        let url = word_of(file, text, item.span(), kinds, variables, given)?;
        let shape = match url.0.last() {
            Some(Piece::Text(tail)) if tail.ends_with(".tar.gz") => Shape::Tarball,
            Some(Piece::Text(tail)) if tail.ends_with(".gz") => Shape::Gzipped,
            Some(Piece::Text(_)) => Shape::Bare,
            Some(Piece::Value(_)) | None => {
                return Err(file.refuse(
                    item.span(),
                    "a download's URL ends in the file it names, which a value in braces is not"
                        .to_string(),
                ));
            }
        };
        let sum = sums
            .iter()
            .find(|(other, _)| *other == triple)
            .map(|(_, item)| *item)
            .ok_or_else(|| {
                file.refuse(
                    item.span(),
                    format!("`url.{triple}` has no `sha256.{triple}` beside it"),
                )
            })?;
        let sha256 = text_of(file, sum, "sha256")?;
        if sha256.len() != 64
            || !sha256
                .chars()
                .all(|digit| matches!(digit, '0'..='9' | 'a'..='f'))
        {
            return Err(file.refuse(
                sum.span(),
                format!("`sha256.{triple}` is a SHA-256, 64 lowercase hexadecimal digits"),
            ));
        }
        downloads.insert(triple.to_string(), Published { url, sha256, shape });
    }

    Ok(Tool::Fetched(Fetched {
        version: text_of(file, version, "version")?,
        downloads,
        path: path.unwrap_or_else(|| key.clone()),
    }))
}

/// The data `written` declares: its version, where it is published, what of it is kept and the digest of that, each stated once, whatever host a run is on.
fn data_of(
    file: &File<'_>,
    written: &Written<'_>,
    kinds: &BTreeMap<Name, Kind>,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
) -> Result<Data, Refusal> {
    let table = written.table;
    let key = written.name.last().expect("data is named");
    if let Some((other, _)) = table.iter().find(|(key, _)| !DATA_KEYS.contains(key)) {
        return Err(file.refuse(
            table.key(other).and_then(|key| key.span()),
            format!("`{other}` is no key of data `{key}`, which holds `version`, `url`, `sha256`, `path` or `files`, and `beside` where a crate's build reads it"),
        ));
    }
    let stated = |name: &str| {
        table.get(name).ok_or_else(|| {
            file.refuse(
                written.span.clone(),
                format!("data `{key}` states its `{name}`"),
            )
        })
    };

    let url = stated("url")?;
    let text = url
        .as_str()
        .ok_or_else(|| file.refuse(url.span(), "the `url` of data is a string".to_string()))?;
    let url_word = word_of(file, text, url.span(), kinds, variables, given)?;
    let archive = matches!(url_word.0.last(), Some(Piece::Text(tail)) if tail.ends_with(".tar.gz"));
    let form = match (table.get("path"), table.get("files")) {
        (Some(path), None) if archive => Form::Directory(text_of(file, path, "path")?),
        (None, None) if archive => Form::Directory(String::new()),
        (Some(_), None) => {
            return Err(file.refuse(
                url.span(),
                "data kept as the `path` of an archive names the gzipped tarball in its `url`, `.tar.gz`"
                    .to_string(),
            ));
        }
        (None, Some(_)) if archive => {
            return Err(file.refuse(
                url.span(),
                "data kept as `files` names the directory they stand under in its `url`, no archive"
                    .to_string(),
            ));
        }
        (None, Some(files)) => {
            let listed = strings_of(file, Some(files), "files")?;
            if listed.is_empty() {
                return Err(file.refuse(
                    files.span(),
                    format!("data `{key}` keeps at least one of its `files`"),
                ));
            }
            for (path, span) in &listed {
                let within = !path.is_empty()
                    && path
                        .split('/')
                        .all(|part| !part.is_empty() && part != "." && part != "..");
                if !within {
                    return Err(file.refuse(
                        span.clone(),
                        format!("`{path}` is no path under the `url`: its parts are names, a `/` between them"),
                    ));
                }
            }
            Form::Files(listed.into_iter().map(|(path, _)| path).collect())
        }
        _ => {
            return Err(file.refuse(
                written.span.clone(),
                format!("data `{key}` states what is kept of it: an archive in its `url`, with the `path` kept of it or none for the whole, or its `files`, and one of them"),
            ));
        }
    };
    let sum = stated("sha256")?;
    let sha256 = text_of(file, sum, "sha256")?;
    if sha256.len() != 64
        || !sha256
            .chars()
            .all(|digit| matches!(digit, '0'..='9' | 'a'..='f'))
    {
        return Err(file.refuse(
            sum.span(),
            "the `sha256` of data is the digest of its files, 64 lowercase hexadecimal digits"
                .to_string(),
        ));
    }

    let beside = match table.get("beside") {
        Some(reader) => {
            let named = text_of(file, reader, "beside")?;
            let one = !named.is_empty() && !named.contains(['/', '\\']) && !named.starts_with('.');
            if !one {
                return Err(file.refuse(
                    reader.span(),
                    format!("`{named}` is no crate: data is kept `beside` the crate whose build reads it, named by its directory at the repository's root"),
                ));
            }
            Some(named)
        }
        None => None,
    };

    Ok(Data {
        version: text_of(file, stated("version")?, "version")?,
        url: url_word,
        form,
        sha256,
        beside,
    })
}

/// The entries of the table `item` holds under `key`, a value a host, by its triple.
fn hosts_of<'a>(
    file: &File<'_>,
    item: Option<&'a Item>,
    key: &str,
) -> Result<Vec<(&'a str, &'a Item)>, Refusal> {
    let Some(item) = item else {
        return Ok(Vec::new());
    };
    let table = item.as_table_like().ok_or_else(|| {
        file.refuse(
            item.span(),
            format!("`{key}` is a table of hosts, each by its triple, as `{key}.x86_64-unknown-linux-gnu`"),
        )
    })?;

    Ok(table.iter().collect())
}

/// The variable `item` declares.
/// What the command line gives a run as `item` declares it: a flag, by its spellings, or a positional argument, by its placeholder and what it is where none is given.
fn given_of(file: &File<'_>, item: &Item) -> Result<Given, Refusal> {
    let table = item.as_table_like().ok_or_else(|| {
        file.refuse(
            item.span(),
            "a given value is a table: a flag (`flag`) or an argument (`argument`)".to_string(),
        )
    })?;
    let text = |key: &str| -> Result<Option<String>, Refusal> {
        table
            .get(key)
            .map(|item| text_of(file, item, key))
            .transpose()
    };
    let keys = table.iter().map(|(key, _)| key).collect::<BTreeSet<_>>();
    let only = |allowed: &[&str]| -> Result<(), Refusal> {
        match keys.iter().find(|key| !allowed.contains(key)) {
            Some(key) => Err(file.refuse(
                table.key(key).and_then(|key| key.span()),
                format!("`{key}` is no key of this given value"),
            )),
            None => Ok(()),
        }
    };

    if let Some(flag) = table.get("flag") {
        only(&["flag", "help"])?;
        return Ok(Given::Flag {
            flags: flags_of(file, flag)?,
            help: text("help")?.unwrap_or_default(),
        });
    }
    if let Some(placeholder) = text("argument")? {
        only(&["argument", "else", "help"])?;
        return Ok(Given::Argument {
            placeholder,
            else_: text("else")?,
            help: text("help")?.unwrap_or_default(),
        });
    }

    Err(file.refuse(
        item.span(),
        "a given value is a flag (`flag`) or an argument (`argument`)".to_string(),
    ))
}

fn variable_of(file: &File<'_>, item: &Item) -> Result<Variable, Refusal> {
    if let Some(text) = item.as_str() {
        return Ok(Variable::Words(vec![runner_word(file, text, item.span())?]));
    }
    if let Some(list) = item.as_array() {
        let words = list
            .iter()
            .map(|value| {
                let text = value
                    .as_str()
                    .ok_or_else(|| file.refuse(value.span(), "a word is a string".to_string()))?;

                runner_word(file, text, value.span())
            })
            .collect::<Result<_, _>>()?;

        return Ok(Variable::Words(words));
    }

    let table = item.as_table_like().ok_or_else(|| {
        file.refuse(
            item.span(),
            "a variable is a string, a list of words, or a table saying where its value comes from"
                .to_string(),
        )
    })?;
    let text = |key: &str| -> Result<Option<String>, Refusal> {
        table
            .get(key)
            .map(|item| text_of(file, item, key))
            .transpose()
    };
    let keys = table.iter().map(|(key, _)| key).collect::<BTreeSet<_>>();
    let only = |allowed: &[&str]| -> Result<(), Refusal> {
        match keys.iter().find(|key| !allowed.contains(key)) {
            Some(key) => Err(file.refuse(
                table.key(key).and_then(|key| key.span()),
                format!("`{key}` is no key of this variable"),
            )),
            None => Ok(()),
        }
    };

    if let Some(command) = table.get("run") {
        only(&["run"])?;
        let list = command
            .as_array()
            .filter(|words| !words.is_empty())
            .ok_or_else(|| {
                file.refuse(
                    command.span(),
                    "`run` is a command, a list of words".to_string(),
                )
            })?;
        return Ok(Variable::Output(words_of(file, list)?));
    }
    if let Some(path) = text("read")? {
        only(&["read", "key"])?;
        let key = text("key")?.ok_or_else(|| {
            file.refuse(
                item.span(),
                "a variable read from a file names the `key` it reads there".to_string(),
            )
        })?;
        return Ok(Variable::Read {
            path,
            key: key.split('.').map(str::to_string).collect(),
        });
    }

    Err(file.refuse(
        item.span(),
        "a variable's table is a command's output (`run`) or a key of a file (`read`)".to_string(),
    ))
}

/// The value `value` stands for as one string, where an instruction's `key` takes one: never a value that stands for several words or none.
fn single(
    file: &File<'_>,
    value: &toml_edit::Value,
    key: &str,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
    word: &dyn Fn(&toml_edit::Value) -> Result<Word, Refusal>,
) -> Result<Word, Refusal> {
    let written = word(value)?;
    let several = match written.alone() {
        Some(Fill::Variable(name)) => {
            matches!(variables.get(name), Some(Variable::Output(_))).then_some(name)
        }
        Some(Fill::Given(name)) => {
            matches!(given.get(name), Some(Given::Flag { .. })).then_some(name)
        }
        _ => None,
    };
    match several {
        Some(name) => Err(file.refuse(
            value.span(),
            format!(
                "an instruction's `{key}` is one string, and `{}` may stand for several words or none",
                from_top(name)
            ),
        )),
        None => Ok(written),
    }
}

/// The directory an instruction states, `in`, one string read from the repository's root, or `None` where it states none and so stands at the root.
fn directory_of(
    file: &File<'_>,
    table: &toml_edit::InlineTable,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
    word: &dyn Fn(&toml_edit::Value) -> Result<Word, Refusal>,
) -> Result<Option<Word>, Refusal> {
    table
        .get("in")
        .map(|value| single(file, value, "in", variables, given, word))
        .transpose()
}

/// The write `table` states: the file its `write` names, under its `in` where it states one, and the `contents` it holds, each one string.
fn write_of(
    file: &File<'_>,
    table: &toml_edit::InlineTable,
    span: Option<Range<usize>>,
    kinds: &BTreeMap<Name, Kind>,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
    word: &dyn Fn(&toml_edit::Value) -> Result<Word, Refusal>,
) -> Result<Instruction, Refusal> {
    if let Some((key, _)) = table.iter().find(|(key, _)| !WRITE_KEYS.contains(key)) {
        return Err(file.refuse(
            span,
            format!("a write holds `in`, `write`, and `contents` or `template`, and `{key}` is none of them"),
        ));
    }
    let path = table.get("write").ok_or_else(|| {
        file.refuse(
            span.clone(),
            "a write names the file it writes, `write`".to_string(),
        )
    })?;
    let contents = match (table.get("contents"), table.get("template")) {
        (Some(contents), None) => single(file, contents, "contents", variables, given, word)?,
        (None, Some(template)) => template_of(file, template, kinds, variables, given)?,
        _ => {
            return Err(file.refuse(
                span,
                "a write holds what it writes as its `contents` or as a `template`, one of the two"
                    .to_string(),
            ));
        }
    };

    Ok(Instruction::Write {
        path: single(file, path, "write", variables, given, word)?,
        contents,
        directory: directory_of(file, table, variables, given, word)?,
    })
}

/// A command written as a table: its words, `do`, and the directory it runs in, `in`, read from the repository's root as every path a step states is.
fn command_of(
    file: &File<'_>,
    table: &toml_edit::InlineTable,
    span: Option<Range<usize>>,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
    word: &dyn Fn(&toml_edit::Value) -> Result<Word, Refusal>,
) -> Result<Instruction, Refusal> {
    if let Some((key, _)) = table.iter().find(|(key, _)| !COMMAND_KEYS.contains(key)) {
        return Err(file.refuse(
            span,
            format!(
                "a command holds its words, `do`, and where it runs, `in`, and `{key}` is neither"
            ),
        ));
    }
    let words = table
        .get("do")
        .and_then(toml_edit::Value::as_array)
        .filter(|words| !words.is_empty())
        .ok_or_else(|| {
            file.refuse(
                span.clone(),
                "a command's `do` is its words, a list of strings".to_string(),
            )
        })?
        .iter()
        .map(word)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Instruction::Command {
        words,
        directory: directory_of(file, table, variables, given, word)?,
    })
}

/// The word `text` writes, its braces resolved in `file`'s scope.
/// A variable's word as written: its text and the values the runner fills, `{!name}`, and no name of the tree's, so a variable is a constant of the run whatever step reads it, and no variable reads another.
fn runner_word(file: &File<'_>, text: &str, span: Option<Range<usize>>) -> Result<Word, Refusal> {
    if text
        .match_indices('{')
        .any(|(open, _)| !text[open + 1..].starts_with('!'))
    {
        return Err(file.refuse(
            span,
            format!("`{text}` names a value of the tree, where a variable's words hold only what the runner fills, `{{!name}}`"),
        ));
    }

    word_of(
        file,
        text,
        span,
        &BTreeMap::new(),
        &BTreeMap::new(),
        &BTreeMap::new(),
    )
}

fn word_of(
    file: &File<'_>,
    text: &str,
    span: Option<Range<usize>>,
    kinds: &BTreeMap<Name, Kind>,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
) -> Result<Word, Refusal> {
    let refuse = |message: String| file.refuse(span.clone(), message);

    let mut pieces = Vec::new();
    let mut rest = text;
    while let Some(open) = rest.find('{') {
        if open > 0 {
            pieces.push(Piece::Text(rest[..open].to_string()));
        }
        let close = rest[open..]
            .find('}')
            .map(|close| open + close)
            .ok_or_else(|| refuse(format!("`{text}` opens a brace it does not close")))?;
        let fill = fill_of(&rest[open + 1..close], kinds, &refuse)?;
        pieces.push(Piece::Value(fill));
        rest = &rest[close + 1..];
    }
    if rest.contains('}') {
        return Err(refuse(format!("`{text}` closes a brace it did not open")));
    }
    if !rest.is_empty() {
        pieces.push(Piece::Text(rest.to_string()));
    }

    let word = Word(pieces);
    if word.alone().is_none()
        && let Some(name) = word
            .fills()
            .find_map(|fill| several(fill, variables, given))
    {
        return Err(refuse(format!(
            "`{}` stands for several words or none, so it is a word of its own",
            from_top(name)
        )));
    }

    Ok(word)
}

/// The value braces hold, `written` being what stands between them, in a word or a template alike: a value the runner fills where it opens with `!`, and otherwise the name of a variable, a given value, a tool, data or a produced file, read from the top.
fn fill_of(
    written: &str,
    kinds: &BTreeMap<Name, Kind>,
    refuse: &dyn Fn(String) -> Refusal,
) -> Result<Fill, Refusal> {
    match written.strip_prefix('!') {
        Some("host_triple") => Ok(Fill::HostTriple),
        Some("cargo_target_dir") => Ok(Fill::CargoTargetDir),
        Some("cargo_home") => Ok(Fill::CargoHome),
        Some("rust_sysroot") => Ok(Fill::RustSysroot),
        Some(filled) => Err(refuse(format!(
            "`!{filled}` is no value the runner fills, which are {}",
            BUILT_INS
                .iter()
                .map(|name| format!("`!{name}`"))
                .collect::<Vec<_>>()
                .join(", ")
        ))),
        None => {
            let name = resolved(written).ok_or_else(|| refuse(unrooted(written)))?;
            match kinds.get(&name) {
                Some(Kind::Variable) => Ok(Fill::Variable(name)),
                Some(Kind::Given) => Ok(Fill::Given(name)),
                Some(Kind::Produced) => Ok(Fill::Produced(name)),
                Some(Kind::Tool) => Ok(Fill::Tool(name)),
                Some(Kind::Data) => Ok(Fill::Data(name)),
                Some(Kind::Task) => Err(refuse(format!(
                    "`{written}` is a step or group, which has no value"
                ))),
                None => Err(refuse(format!("`{written}` names nothing"))),
            }
        }
    }
}

/// The name of the variable or the given value `fill` is, where it stands for several words or none, and so cannot sit beside other text.
fn several<'a>(
    fill: &'a Fill,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
) -> Option<&'a Name> {
    match fill {
        Fill::Variable(name) => variables
            .get(name)
            .is_some_and(|variable| !variable.is_one_word())
            .then_some(name),
        Fill::Given(name) => given
            .get(name)
            .is_some_and(|given| !given.is_one_word())
            .then_some(name),
        _ => None,
    }
}

/// The template `value` names, a file of the tree read from the repository's root, as one string: its text with each value it holds filled, written as the task files write one, `{.path}` or `{!name}` up to the next `}`, and every other brace kept as text, since a template is a file of another language whose braces are its own.
fn template_of(
    file: &File<'_>,
    value: &toml_edit::Value,
    kinds: &BTreeMap<Name, Kind>,
    variables: &BTreeMap<Name, Variable>,
    given: &BTreeMap<Name, Given>,
) -> Result<Word, Refusal> {
    let path = value.as_str().ok_or_else(|| {
        file.refuse(
            value.span(),
            "a write's `template` is the path of a template, a string".to_string(),
        )
    })?;
    let refuse =
        |message: String| file.refuse(value.span(), format!("the template `{path}`: {message}"));
    let text = fs::read_to_string(root().join(path)).map_err(|error| refuse(error.to_string()))?;

    let mut pieces = Vec::new();
    let mut rest = text.as_str();
    while let Some((open, _)) = rest
        .match_indices('{')
        .find(|(at, _)| matches!(rest.as_bytes().get(at + 1), Some(b'.' | b'!')))
    {
        if open > 0 {
            pieces.push(Piece::Text(rest[..open].to_string()));
        }
        let close = rest[open..]
            .find('}')
            .map(|close| open + close)
            .ok_or_else(|| refuse("a value opens with no `}` to close it".to_string()))?;
        let fill = fill_of(&rest[open + 1..close], kinds, &refuse)?;
        if let Some(name) = several(&fill, variables, given) {
            return Err(refuse(format!(
                "`{}` stands for several words or none, and a template is one string",
                from_top(name)
            )));
        }
        pieces.push(Piece::Value(fill));
        rest = &rest[close + 1..];
    }
    if !rest.is_empty() {
        pieces.push(Piece::Text(rest.to_string()));
    }

    Ok(Word(pieces))
}
