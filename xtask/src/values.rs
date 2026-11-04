//! What a run fills a command's braces with.
//!
//! **A variable is a constant of the run.** Each has one value for the whole run: what its file writes, what the command line gave, or what a command printed, asked once and only where a step that runs uses it. The two built-ins are the triple `xtask` was built for and cargo's target directory, and a produced file stands for its path, from the repository's root wherever a step runs.

use {
    crate::{
        Fill, HOST_TRIPLE, Name, Piece, Tree, Variable, Word, ask, dotted, root, target_directory,
    },
    std::{cell::RefCell, collections::BTreeMap, path::PathBuf, process::Command},
};

#[cfg(test)]
mod tests;

/// What the command line gave, by the name of the variable it fills.
pub(crate) type Given = BTreeMap<Name, String>;

/// The values one run fills braces with.
pub(crate) struct Values<'a> {
    tree: &'a Tree,
    given: &'a Given,
    printed: RefCell<BTreeMap<Name, Vec<String>>>,
}

impl<'a> Values<'a> {
    pub(crate) fn new(tree: &'a Tree, given: &'a Given) -> Self {
        Values {
            tree,
            given,
            printed: RefCell::new(BTreeMap::new()),
        }
    }

    /// The words a command is, or `None` where a command's output it reads came back empty, so the command has nothing to run on.
    pub(crate) fn command(&self, words: &[Word]) -> Result<Option<Vec<String>>, String> {
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
    pub(crate) fn path(&self, name: &Name) -> Result<PathBuf, String> {
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
                Variable::Flag { flag, .. } => Ok(Some(
                    self.given
                        .get(name)
                        .map(|value| vec![flag.clone(), value.clone()])
                        .unwrap_or_default(),
                )),
                Variable::Argument { otherwise, .. } => Ok(Some(vec![
                    self.given.get(name).unwrap_or(otherwise).clone(),
                ])),
            },
            Fill::Host | Fill::Target | Fill::Produced(_) => {
                self.text(word).map(|text| Some(vec![text]))
            }
        }
    }

    /// The one string `word` stands for: its text, and each value it holds as the one word that value is.
    fn text(&self, word: &Word) -> Result<String, String> {
        let mut text = String::new();
        for piece in &word.0 {
            match piece {
                Piece::Text(written) => text.push_str(written),
                Piece::Value(Fill::Host) => text.push_str(HOST_TRIPLE),
                Piece::Value(Fill::Target) => text.push_str(&target_directory().to_string_lossy()),
                Piece::Value(Fill::Produced(name)) => {
                    text.push_str(&self.path(name)?.to_string_lossy())
                }
                Piece::Value(Fill::Variable(name)) => match &self.tree.variables[name] {
                    Variable::Words(words) => text.push_str(&words.concat()),
                    Variable::Argument { otherwise, .. } => {
                        text.push_str(self.given.get(name).unwrap_or(otherwise))
                    }
                    Variable::Output(_) | Variable::Flag { .. } => {
                        unreachable!("the reader keeps `{}` a word of its own", dotted(name))
                    }
                },
            }
        }

        Ok(text)
    }

    /// What `command` prints, a word to a line, asked once a run.
    fn printed(&self, name: &Name, command: &[String]) -> Result<Vec<String>, String> {
        if let Some(printed) = self.printed.borrow().get(name) {
            return Ok(printed.clone());
        }

        let (program, arguments) = command
            .split_first()
            .expect("the reader refuses a command of no words");
        let output = ask(
            Command::new(program),
            &arguments.iter().map(String::as_str).collect::<Vec<_>>(),
        )
        .map_err(|error| {
            format!(
                "`{}` is what `{}` prints: {error}",
                dotted(name),
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
