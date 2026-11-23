//! What a run fills a command's braces with.
//!
//! **A variable is a constant of the run.** Each has one value for the whole run: what its file writes, what the command line gave, or what a command printed, asked once and only where a step that runs uses it. The values the runner fills, each written after a `!`, are the triple `xtask` was built for, Cargo's target directory, Cargo's home and the toolchain's root, the last asked of the pinned `rustc` once a run; a tool stands for its executable's path, kept for the host the run is on; and a produced file stands for its path, from the repository's root wherever a step runs.

use {
    crate::{
        Fill, HOST_TRIPLE, Name, Piece, Tree, Variable, Word, ask, ask_launched, cargo_home,
        executable, from_top, launch, root, target_directory,
    },
    std::{
        cell::{OnceCell, RefCell},
        collections::BTreeMap,
        path::PathBuf,
        process::Command,
    },
};

#[cfg(test)]
mod tests;

/// What the command line gave, by the name of the variable it fills.
pub type Given = BTreeMap<Name, String>;

/// The values one run fills braces with.
pub struct Values<'a> {
    tree: &'a Tree,
    given: &'a Given,
    printed: RefCell<BTreeMap<Name, Vec<String>>>,
    sysroot: OnceCell<String>,
}

impl<'a> Values<'a> {
    pub fn new(tree: &'a Tree, given: &'a Given) -> Self {
        Values {
            tree,
            given,
            printed: RefCell::new(BTreeMap::new()),
            sysroot: OnceCell::new(),
        }
    }

    /// The words a command is, or `None` where a command's output it reads came back empty, so the command has nothing to run on.
    pub fn command(&self, words: &[Word]) -> Result<Option<Vec<String>>, String> {
        let mut command = Vec::new();
        for word in words {
            match self.word(word)? {
                Some(words) => command.extend(words),
                None => return Ok(None),
            }
        }

        Ok(Some(command))
    }

    /// Where the produced file `name` is.
    pub fn path(&self, name: &Name) -> Result<PathBuf, String> {
        let path = self.text(&self.tree.produced[name].path)?;

        Ok(root().join(path))
    }

    /// The words `word` stands for, or `None` where it reads a command's output that came back empty.
    fn word(&self, word: &Word) -> Result<Option<Vec<String>>, String> {
        let Some(fill) = word.alone() else {
            return self.text(word).map(|text| Some(vec![text]));
        };

        match fill {
            Fill::Variable(name) => match &self.tree.variables[name] {
                Variable::Words(words) => Ok(Some(words.clone())),
                Variable::Output(command) => {
                    let printed = self.printed(name, command)?;
                    Ok((!printed.is_empty()).then_some(printed))
                }
                // The command is handed the long spelling where the flag has one, so the echoed line reads by itself.
                Variable::Flag { flags, .. } => Ok(Some(
                    self.given
                        .get(name)
                        .and_then(|value| {
                            let spelling = flags
                                .iter()
                                .find(|flag| flag.starts_with("--"))
                                .or(flags.first())?;
                            Some(vec![spelling.clone(), value.clone()])
                        })
                        .unwrap_or_default(),
                )),
                Variable::Argument { otherwise, .. } => Ok(Some(vec![
                    self.given.get(name).unwrap_or(otherwise).clone(),
                ])),
            },
            Fill::HostTriple
            | Fill::CargoTargetDir
            | Fill::CargoHome
            | Fill::RustSysroot
            | Fill::Produced(_)
            | Fill::Tool(_) => self.text(word).map(|text| Some(vec![text])),
        }
    }

    /// The one string `word` stands for: its text, and each value it holds as the one word that value is.
    pub fn text(&self, word: &Word) -> Result<String, String> {
        let mut text = String::new();
        for piece in &word.0 {
            match piece {
                Piece::Text(written) => text.push_str(written),
                Piece::Value(Fill::HostTriple) => text.push_str(HOST_TRIPLE),
                Piece::Value(Fill::CargoTargetDir) => {
                    text.push_str(&target_directory().to_string_lossy())
                }
                Piece::Value(Fill::CargoHome) => text.push_str(&cargo_home().to_string_lossy()),
                Piece::Value(Fill::RustSysroot) => text.push_str(self.sysroot()?),
                Piece::Value(Fill::Produced(name)) => {
                    text.push_str(&self.path(name)?.to_string_lossy())
                }
                Piece::Value(Fill::Tool(name)) => {
                    text.push_str(&executable(self.tree, name).to_string_lossy())
                }
                Piece::Value(Fill::Variable(name)) => match &self.tree.variables[name] {
                    Variable::Words(words) => text.push_str(&words.concat()),
                    Variable::Argument { otherwise, .. } => {
                        text.push_str(self.given.get(name).unwrap_or(otherwise))
                    }
                    Variable::Output(_) | Variable::Flag { .. } => {
                        unreachable!("the reader keeps `{}` a word of its own", from_top(name))
                    }
                },
            }
        }

        Ok(text)
    }

    /// The toolchain's root, as the `rustc` the repository pins prints it, asked once a run: where a toolchain's own sources stand, which a build records where they are installed.
    fn sysroot(&self) -> Result<&str, String> {
        if let Some(sysroot) = self.sysroot.get() {
            return Ok(sysroot);
        }
        let printed = ask(Command::new("rustc"), &["--print", "sysroot"]).map_err(|error| {
            format!("`{{!rust_sysroot}}` is what `rustc --print sysroot` prints: {error}")
        })?;

        Ok(self.sysroot.get_or_init(|| printed.trim().to_string()))
    }

    /// What `command` prints, a word to a line, asked once a run, launched as a step's command is.
    fn printed(&self, name: &Name, command: &[String]) -> Result<Vec<String>, String> {
        if let Some(printed) = self.printed.borrow().get(name) {
            return Ok(printed.clone());
        }

        let output = launch(command).and_then(ask_launched).map_err(|error| {
            format!(
                "`{}` is what `{}` prints: {error}",
                from_top(name),
                command.join(" ")
            )
        })?;
        let printed = output
            .lines()
            .filter(|line| !line.is_empty())
            .map(str::to_string)
            .collect::<Vec<_>>();
        self.printed
            .borrow_mut()
            .insert(name.clone(), printed.clone());

        Ok(printed)
    }
}
