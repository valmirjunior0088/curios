//! The task files read into one tree, and every refusal reading them can make.
//!
//! **A file is a group, and a key's meaning is where it sits.** `xtask/tasks.toml` is the group the files in `xtask/tasks/` belong to, each a group named after its file. A file's top level holds `description`, `variables`, `programs`, and its steps and groups; a group holds only steps and groups; a step holds `description`, `requires`, `consumes`, `produces`, `run` and `dir`, and no table of its own. So whether a key is an attribute or a child is read off its position, and one out of place is refused rather than guessed at.
//!
//! **A name is a path, and it names one thing.** A file's variables, produced files, steps and groups share one namespace, and a name declared twice in it is refused. Without a dot a name is the file's own; with dots it is a path from the top, as the command line spells it. `requires` names steps and groups, `consumes` produced files, and the braces in a command a variable or a produced file. Each resolves as the tree is read, so a name that names nothing, or the wrong kind of thing, is refused before anything runs, and so is a command that reads a file its step does not consume, which would run without the edge that orders it.

use {
    crate::root,
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt, fs,
        ops::Range,
    },
    toml_edit::{Array, Document, Item, TableLike},
};

#[cfg(test)]
mod test_support;

#[cfg(test)]
mod instruction_tests;
#[cfg(test)]
mod layout_tests;
#[cfg(test)]
mod name_tests;
#[cfg(test)]
mod program_tests;

/// The file whose group holds the files in [`DIRECTORY`], from the repository's root.
const ROOT_FILE: &str = "xtask/tasks.toml";

/// The directory whose files are the top's groups, each named after its file.
const DIRECTORY: &str = "xtask/tasks";

/// The words `cargo xtask` keeps for its commands outside the tree, which no task may take.
pub const COMMANDS: [&str; 2] = ["release", "installer"];

/// The values the runner fills without a declaration.
const BUILT_INS: [&str; 2] = ["host", "target"];

/// The keys a step may hold.
const STEP_KEYS: [&str; 6] = [
    "description",
    "requires",
    "consumes",
    "produces",
    "run",
    "dir",
];

/// What names a task, a variable or a produced file: its words from the top, as the command line spells a task.
pub type Name = Vec<String>;

/// A name as a reader writes it, with dots.
pub fn dotted(name: &[String]) -> String {
    name.join(".")
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
    pub produced: BTreeMap<Name, Produced>,
    /// What the steps' commands launch, by the program's name, where the files say what version a run needs or what installs it.
    pub programs: BTreeMap<String, Program>,
}

#[derive(Debug)]
pub struct Group {
    /// A file's description; a group inside a file has none, and is described by its steps.
    pub description: Option<String>,
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
    /// Where the commands run, from the repository's root.
    pub dir: Option<String>,
}

/// One thing a step runs.
#[derive(Debug)]
pub enum Instruction {
    /// A command: the words a person would type, the program first.
    Command(Vec<Word>),
    /// A file written whole: where, from where the step runs, and what it holds.
    Write { path: Word, contents: Word },
}

impl Instruction {
    /// Every word the instruction holds.
    pub fn words(&self) -> impl Iterator<Item = &Word> {
        match self {
            Instruction::Command(words) => words.iter().collect::<Vec<_>>(),
            Instruction::Write { path, contents } => vec![path, contents],
        }
        .into_iter()
    }
}

/// The keys a write holds, besides which it holds none.
const WRITE_KEYS: [&str; 2] = ["write", "contents"];

/// A file a step makes: where it is, and which step makes it.
#[derive(Debug)]
pub struct Produced {
    pub path: Word,
    pub producer: usize,
}

/// A program the steps launch, as the files declare it.
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub pin: Option<Pin>,
    /// What installs it, at its pin where it has one.
    pub install: Option<String>,
}

/// The version of a program a run needs.
#[derive(Debug, PartialEq, Eq)]
pub enum Pin {
    /// This version and no other: `version = "2.6.2"`.
    Exact(String),
    /// This version or a later one: `version = ">= 22"`.
    Minimum(String),
}

impl Pin {
    pub fn version(&self) -> &str {
        match self {
            Pin::Exact(version) | Pin::Minimum(version) => version,
        }
    }
}

/// A named value of a file, the same for the whole run.
#[derive(Debug, PartialEq, Eq)]
pub enum Variable {
    /// Words as written: a string is one, a list each of its items.
    Words(Vec<String>),
    /// What a command prints, a word to a line, asked once a run and only where a step that uses it runs.
    Output(Vec<String>),
    /// Given on the command line by `flag` with a value: the flag and the value where it is given, nothing where it is not.
    Flag { flag: String, help: String },
    /// Given on the command line as a positional argument, `otherwise` where it is not.
    Argument {
        placeholder: String,
        otherwise: String,
        help: String,
    },
}

impl Variable {
    /// Whether the variable always stands for one word, so it may sit inside a word beside other text.
    fn is_one_word(&self) -> bool {
        match self {
            Variable::Words(words) => words.len() == 1,
            Variable::Argument { .. } => true,
            Variable::Output(_) | Variable::Flag { .. } => false,
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
    /// The triple `xtask` was built for.
    Host,
    /// Cargo's target directory.
    Target,
    Variable(Name),
    /// A produced file's path.
    Produced(Name),
}

impl Word {
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

/// Why the task files cannot be run, and where.
#[derive(Debug, PartialEq, Eq)]
pub struct Refusal {
    file: String,
    /// The line, the column, the line's text and how many characters to mark, where the refusal has a place.
    at: Option<(usize, usize, String, usize)>,
    message: String,
}

impl Refusal {
    fn in_file(file: &str, message: String) -> Self {
        Refusal {
            file: file.to_string(),
            at: None,
            message,
        }
    }

    fn at(file: &str, text: &str, span: Option<Range<usize>>, message: String) -> Self {
        let Some(span) = span else {
            return Refusal::in_file(file, message);
        };

        let start = span.start.min(text.len());
        let line_start = text[..start].rfind('\n').map_or(0, |at| at + 1);
        let line_end = text[start..].find('\n').map_or(text.len(), |at| start + at);
        let line = text[line_start..line_end].to_string();
        let column = text[line_start..start].chars().count();
        let width = text[start..span.end.min(line_end).max(start)]
            .chars()
            .count()
            .max(1);

        Refusal {
            file: file.to_string(),
            at: Some((
                text[..start].matches('\n').count() + 1,
                column + 1,
                line,
                width,
            )),
            message,
        }
    }
}

impl fmt::Display for Refusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.at {
            None => write!(formatter, "{}: {}", self.file, self.message),
            Some((line, column, text, width)) => {
                let gutter = line.to_string().len();
                write!(
                    formatter,
                    "{}:{line}:{column}: {}\n{:gutter$} |\n{line} | {text}\n{:gutter$} | {:pad$}{}",
                    self.file,
                    self.message,
                    "",
                    "",
                    "",
                    "^".repeat(*width),
                    pad = column - 1,
                )
            }
        }
    }
}

/// One file as read: where it is, its text, and the name its group takes.
struct File<'a> {
    path: &'a str,
    text: &'a str,
    scope: Name,
}

impl File<'_> {
    fn refuse(&self, span: Option<Range<usize>>, message: String) -> Refusal {
        Refusal::at(self.path, self.text, span, message)
    }
}

/// A step as written, before its names resolve.
struct Written<'a> {
    file: usize,
    name: Name,
    table: &'a dyn TableLike,
    span: Option<Range<usize>>,
}

/// What a name in a file's namespace is.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Task,
    Variable,
    Produced,
}

impl Tree {
    /// The task files of the checkout, read and resolved.
    pub fn read() -> Result<Self, Refusal> {
        let read = |path: &str| {
            fs::read_to_string(root().join(path))
                .map_err(|error| Refusal::in_file(path, format!("cannot be read: {error}")))
        };

        let mut files = vec![(ROOT_FILE.to_string(), read(ROOT_FILE)?)];
        let directory = root().join(DIRECTORY);
        let mut names = fs::read_dir(&directory)
            .map_err(|error| Refusal::in_file(DIRECTORY, format!("cannot be read: {error}")))?
            .map(|entry| {
                entry
                    .map(|entry| entry.file_name().to_string_lossy().into_owned())
                    .map_err(|error| {
                        Refusal::in_file(DIRECTORY, format!("cannot be read: {error}"))
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;
        names.sort();
        for name in names {
            let path = format!("{DIRECTORY}/{name}");
            if !name.ends_with(".toml") {
                return Err(Refusal::in_file(
                    &path,
                    "is no task file: the directory holds `.toml` files alone".to_string(),
                ));
            }
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
            produced: BTreeMap::new(),
            programs: BTreeMap::new(),
        };
        let mut kinds = BTreeMap::<Name, Kind>::new();
        let mut written = Vec::new();

        // The first pass declares every name, so the second resolves against the whole tree whatever order its files come in.
        for (index, (file, document)) in files.iter().zip(&documents).enumerate() {
            let top = document.as_table();
            let mut description = None;
            for (key, item) in top.iter() {
                let span = top.key(key).and_then(|key| key.span());
                match key {
                    "description" => description = Some(text_of(file, item, "description")?),
                    "variables" => {
                        let table = item.as_table_like().ok_or_else(|| {
                            file.refuse(item.span(), "`variables` is a table of names".to_string())
                        })?;
                        for (name, value) in table.iter() {
                            let name = scoped(&file.scope, name);
                            declare(&mut kinds, file, &name, Kind::Variable, value.span())?;
                            let variable = variable_of(file, value)?;
                            tree.variables.insert(name, variable);
                        }
                    }
                    "programs" => {
                        let table = item.as_table_like().ok_or_else(|| {
                            file.refuse(
                                item.span(),
                                "`programs` is a table of programs by name".to_string(),
                            )
                        })?;
                        for (name, value) in table.iter() {
                            let program = program_of(file, value)?;
                            if tree.programs.insert(name.to_string(), program).is_some() {
                                return Err(file.refuse(
                                    table.key(name).and_then(|key| key.span()),
                                    format!("program `{name}` is declared twice"),
                                ));
                            }
                        }
                    }
                    _ => {
                        let name = scoped(&file.scope, key);
                        if file.scope.is_empty() && COMMANDS.contains(&key) {
                            return Err(file.refuse(
                                span,
                                format!("`{key}` is a command of `cargo xtask` outside the tree"),
                            ));
                        }
                        declare(&mut kinds, file, &name, Kind::Task, span.clone())?;
                        let table = item.as_table_like().ok_or_else(|| {
                            file.refuse(
                                span.clone(),
                                format!("`{key}` is out of place: a file's top holds `description`, `variables`, `programs`, and its steps and groups as tables"),
                            )
                        })?;
                        tree.groups
                            .entry(file.scope.clone())
                            .or_insert_with(|| Group {
                                description: None,
                                children: Vec::new(),
                                steps: Vec::new(),
                            })
                            .children
                            .push(name.clone());
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
                file.refuse(
                    None,
                    "has no `description`, which a file's top states".to_string(),
                )
            })?;
            let group = tree
                .groups
                .entry(file.scope.clone())
                .or_insert_with(|| Group {
                    description: None,
                    children: Vec::new(),
                    steps: Vec::new(),
                });
            group.description = Some(description);
            if !file.scope.is_empty() {
                tree.groups
                    .entry(Vec::new())
                    .or_insert_with(|| Group {
                        description: None,
                        children: Vec::new(),
                        steps: Vec::new(),
                    })
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
                    let name = scoped(&file.scope, name);
                    declare(&mut kinds, file, &name, Kind::Produced, value.span())?;
                }
            }
        }

        // The second pass resolves each step's names against every declaration.
        let mut steps = Vec::new();
        for (index, step) in written.iter().enumerate() {
            let file = &files[step.file];
            let resolve = |text: &str, span: Option<Range<usize>>| {
                let name = resolved(&file.scope, text);
                match kinds.get(&name) {
                    Some(kind) => Ok((name, *kind)),
                    None => Err(file.refuse(span, format!("`{text}` names nothing"))),
                }
            };

            let description = match step.table.get("description") {
                Some(item) => text_of(file, item, "description")?,
                None => {
                    return Err(file.refuse(
                        step.span.clone(),
                        format!("step `{}` has no `description`", dotted(&step.name)),
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
                    let path = word_of(file, path, value.span(), &kinds, &tree.variables)?;
                    let name = scoped(&file.scope, name);
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
                let word_value = word_of(file, text, word.span(), &kinds, &tree.variables)?;
                for fill in word_value.fills() {
                    if let Fill::Produced(name) = fill
                        && !consumes.contains(name)
                        && !produces.contains(name)
                    {
                        return Err(file.refuse(
                            word.span(),
                            format!("`{}` is a file this step neither consumes nor produces, so nothing orders it after the step that makes it", dotted(name)),
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
                        run.push(write_of(
                            file,
                            table,
                            instruction.span(),
                            &tree.variables,
                            &word,
                        )?);
                        continue;
                    }
                    let words = instruction
                        .as_array()
                        .filter(|words| !words.is_empty())
                        .ok_or_else(|| {
                            file.refuse(
                                instruction.span(),
                                "an instruction is a command, a list of words, or a write, `{ write = path, contents = text }`".to_string(),
                            )
                        })?;
                    run.push(Instruction::Command(
                        words.iter().map(&word).collect::<Result<_, _>>()?,
                    ));
                }
            }

            if run.is_empty() && requires.is_empty() {
                return Err(file.refuse(
                    step.span.clone(),
                    format!(
                        "step `{}` neither runs nor requires anything",
                        dotted(&step.name)
                    ),
                ));
            }

            let dir = match step.table.get("dir") {
                Some(item) => Some(text_of(file, item, "dir")?),
                None => None,
            };

            steps.push(Step {
                name: step.name.clone(),
                description,
                requires,
                consumes,
                produces,
                run,
                dir,
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
                Fill::Variable(name)
                    if matches!(
                        self.variables.get(name),
                        Some(Variable::Flag { .. } | Variable::Argument { .. })
                    ) =>
                {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect()
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
                            .map(|step| dotted(&self.steps[*step].name))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                ));
            }
        }

        Ok(())
    }
}

/// `key` declared in the namespace of the file whose group is `scope`.
fn scoped(scope: &[String], key: &str) -> Name {
    let mut name = scope.to_vec();
    name.push(key.to_string());
    name
}

/// A name as written in the file whose group is `scope`: its own without a dot, a path from the top with dots.
fn resolved(scope: &[String], text: &str) -> Name {
    match text.contains('.') {
        true => text.split('.').map(str::to_string).collect(),
        false => scoped(scope, text),
    }
}

/// Declare `name` as a `kind`, refusing a name its namespace already holds.
fn declare(
    kinds: &mut BTreeMap<Name, Kind>,
    file: &File<'_>,
    name: &Name,
    kind: Kind,
    span: Option<Range<usize>>,
) -> Result<(), Refusal> {
    if kind != Kind::Task
        && let [last] = &name[name.len() - 1..]
        && BUILT_INS.contains(&last.as_str())
    {
        return Err(file.refuse(
            span,
            format!("`{last}` is a value the runner fills, which no file declares"),
        ));
    }
    if kinds.insert(name.clone(), kind).is_some() {
        return Err(file.refuse(span, format!("`{}` is declared twice", dotted(name))));
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
    let is_step = table.iter().any(|(key, _)| STEP_KEYS.contains(&key));
    if is_step {
        for (key, item) in table.iter() {
            if !STEP_KEYS.contains(&key) {
                let span = table.key(key).and_then(|key| key.span());
                return Err(file.refuse(
                    span,
                    match key {
                        "variables" | "programs" => format!("`{key}` belongs at a file's top"),
                        _ if item.is_table() => {
                            format!("`{key}` is a table inside a step, which holds no tables")
                        }
                        _ => format!("`{key}` is no key of a step"),
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
        return Err(file.refuse(span, format!("`{}` is empty", dotted(&name))));
    }
    tree.groups.insert(
        name.clone(),
        Group {
            description: None,
            children: Vec::new(),
            steps: Vec::new(),
        },
    );
    for (key, item) in table.iter() {
        let span = table.key(key).and_then(|key| key.span());
        let child = item.as_table_like().ok_or_else(|| {
            file.refuse(
                span.clone(),
                format!("`{key}` is out of place: a group holds only steps and groups"),
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

/// The program `item` declares: the `version` it must be, or reach where `>=` leads it, and the command that installs it, `{version}` in it the version's numbers.
fn program_of(file: &File<'_>, item: &Item) -> Result<Program, Refusal> {
    let table = item.as_table_like().ok_or_else(|| {
        file.refuse(
            item.span(),
            "a program is a table of its `version` and its `install`".to_string(),
        )
    })?;
    if let Some((key, _)) = table
        .iter()
        .find(|(key, _)| !["version", "install"].contains(key))
    {
        return Err(file.refuse(
            table.key(key).and_then(|key| key.span()),
            format!("`{key}` is no key of a program"),
        ));
    }
    let text = |key: &str| -> Result<Option<String>, Refusal> {
        table
            .get(key)
            .map(|item| text_of(file, item, key))
            .transpose()
    };

    let written = text("version")?;
    let pin = written
        .as_deref()
        .map(|version| match version.strip_prefix(">=") {
            Some(minimum) => Pin::Minimum(minimum.trim_start().to_string()),
            None => Pin::Exact(version.to_string()),
        });
    if let Some(pin) = &pin
        && !pin.version().split('.').all(|part| {
            !part.is_empty() && part.chars().all(|character| character.is_ascii_digit())
        })
    {
        return Err(file.refuse(
            item.span(),
            format!(
                "a version is numbers between dots, after `>=` for a minimum, and `{}` is not",
                written.unwrap_or_default()
            ),
        ));
    }
    let install = match (text("install")?, &pin) {
        (Some(install), Some(pin)) => Some(install.replace("{version}", pin.version())),
        (Some(install), None) if install.contains("{version}") => {
            return Err(file.refuse(
                item.span(),
                "`{version}` is filled from a pin, and this program has none".to_string(),
            ));
        }
        (install, _) => install,
    };

    Ok(Program { pin, install })
}

/// The variable `item` declares.
fn variable_of(file: &File<'_>, item: &Item) -> Result<Variable, Refusal> {
    if let Some(text) = item.as_str() {
        return Ok(Variable::Words(vec![text.to_string()]));
    }
    if let Some(list) = item.as_array() {
        return Ok(Variable::Words(words_of(file, list)?));
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
    if let Some(flag) = text("flag")? {
        only(&["flag", "help"])?;
        return Ok(Variable::Flag {
            flag,
            help: text("help")?.unwrap_or_default(),
        });
    }
    if let Some(placeholder) = text("argument")? {
        only(&["argument", "else", "help"])?;
        return Ok(Variable::Argument {
            placeholder,
            otherwise: text("else")?.ok_or_else(|| {
                file.refuse(
                    item.span(),
                    "an argument states what it is `else`, where none is given".to_string(),
                )
            })?,
            help: text("help")?.unwrap_or_default(),
        });
    }

    Err(file.refuse(
        item.span(),
        "a variable's table is a command's output (`run`), a flag (`flag`) or an argument (`argument`)".to_string(),
    ))
}

/// The word `text` writes, its braces resolved in `file`'s scope.
/// The write `table` states: the file its `write` names and the `contents` it holds, each one string, so neither may be a value that stands for several words or none.
fn write_of(
    file: &File<'_>,
    table: &toml_edit::InlineTable,
    span: Option<Range<usize>>,
    variables: &BTreeMap<Name, Variable>,
    word: &dyn Fn(&toml_edit::Value) -> Result<Word, Refusal>,
) -> Result<Instruction, Refusal> {
    if let Some((key, _)) = table.iter().find(|(key, _)| !WRITE_KEYS.contains(key)) {
        return Err(file.refuse(
            span,
            format!("a write holds `write` and `contents`, and `{key}` is neither"),
        ));
    }
    let one = |key: &str| -> Result<Word, Refusal> {
        let value = table.get(key).ok_or_else(|| {
            file.refuse(
                span.clone(),
                format!("a write names the file it writes, `write`, and what it holds, `contents`, and this one lacks `{key}`"),
            )
        })?;
        let written = word(value)?;
        if let Some(Fill::Variable(name)) = written.alone()
            && matches!(
                variables.get(name),
                Some(Variable::Output(_) | Variable::Flag { .. })
            )
        {
            return Err(file.refuse(
                value.span(),
                format!(
                    "a write's `{key}` is one string, and `{}` may stand for several words or none",
                    dotted(name)
                ),
            ));
        }
        Ok(written)
    };

    Ok(Instruction::Write {
        path: one("write")?,
        contents: one("contents")?,
    })
}

fn word_of(
    file: &File<'_>,
    text: &str,
    span: Option<Range<usize>>,
    kinds: &BTreeMap<Name, Kind>,
    variables: &BTreeMap<Name, Variable>,
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
        let written = &rest[open + 1..close];
        let fill = match written {
            "host" => Fill::Host,
            "target" => Fill::Target,
            _ => {
                let name = resolved(&file.scope, written);
                match kinds.get(&name) {
                    Some(Kind::Variable) => Fill::Variable(name),
                    Some(Kind::Produced) => Fill::Produced(name),
                    Some(Kind::Task) => {
                        return Err(refuse(format!(
                            "`{written}` is a step or group, which has no value"
                        )));
                    }
                    None => return Err(refuse(format!("`{written}` names nothing"))),
                }
            }
        };
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
    if word.alone().is_none() {
        for fill in word.fills() {
            if let Fill::Variable(name) = fill
                && variables
                    .get(name)
                    .is_some_and(|variable| !variable.is_one_word())
            {
                return Err(refuse(format!(
                    "`{}` stands for several words or none, so it is a word of its own",
                    dotted(name)
                )));
            }
        }
    }

    Ok(word)
}
