//! What a manifest states, refused: what is said of it, and the place in the manifest it is said of.
//!
//! **A place is the keys that lead to it, and no offset is carried through the records.** A manifest is read into records that keep no positions, and a refusal may be written long after the read: of a row another manifest's row conflicts with, of a member an umbrella lists. So a refusal names its place as its reader would, `dependencies`, `json`, `rev`, and the text is read for where that stands once, when there is a refusal to report.

#[cfg(test)]
mod tests;

use {
    curios_common::{About, Report, Said, Source, Span},
    std::{fmt, ops::Range, sync::Arc},
    toml_edit::{Document, Item, Table, TableLike, Value},
};

/// One step into a manifest: the key of a table, or a row of an array by its position.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Step {
    Key(String),
    Row(usize),
}

impl Step {
    /// The step to the entry of a table `key` names.
    pub fn key(key: impl Into<String>) -> Self {
        Self::Key(key.into())
    }
}

impl From<&str> for Step {
    fn from(key: &str) -> Self {
        Self::key(key)
    }
}

/// Where in a manifest a refusal stands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum At {
    /// The manifest as a whole: what it is, or what it lacks.
    Whole,
    /// What the steps lead to, at its key: an entry of a table, or a row of an array at its head.
    Entry(Vec<Step>),
    /// What the steps lead to, at its value.
    Value(Vec<Step>),
    /// A range of the text, as the reader of TOML gives one.
    Range(Range<usize>),
}

/// What a manifest states, refused, before it is known which file states it: a manifest is read from its text alone, and whoever knows the file places the refusal in it ([`Refused::located`]).
#[derive(Debug, Clone, PartialEq)]
pub struct Refused {
    pub said: Said,
    pub at: At,
    /// Each command that is a way out.
    pub tries: Vec<String>,
}

impl Refused {
    /// A refusal under `tag`, of the manifest as a whole until it is said where.
    pub fn of(tag: &str) -> Self {
        Self {
            said: Said::line(tag),
            at: At::Whole,
            tries: Vec::new(),
        }
    }

    /// A refusal that says what `said` says.
    pub fn saying(said: Said) -> Self {
        Self {
            said,
            at: At::Whole,
            tries: Vec::new(),
        }
    }

    /// This refusal, resting on `fact` as well.
    pub fn stating(mut self, fact: impl fmt::Display) -> Self {
        self.said.stating(fact);
        self
    }

    /// This refusal, at the key of what `steps` lead to.
    pub fn entry<S: Into<Step>>(mut self, steps: impl IntoIterator<Item = S>) -> Self {
        self.at = At::Entry(steps.into_iter().map(Into::into).collect());
        self
    }

    /// This refusal, at the value of what `steps` lead to.
    pub fn value<S: Into<Step>>(mut self, steps: impl IntoIterator<Item = S>) -> Self {
        self.at = At::Value(steps.into_iter().map(Into::into).collect());
        self
    }

    /// This refusal, at `range` of the text.
    pub fn range(mut self, range: Range<usize>) -> Self {
        self.at = At::Range(range);
        self
    }

    /// This refusal, with `command` as a way out.
    pub fn trying(mut self, command: impl Into<String>) -> Self {
        self.tries.push(command.into());
        self
    }

    /// This refusal, written of a part of a manifest, as one of the manifest that holds the part where `steps` lead: a row's refusal is written by what reads the row, and placed by what knows where the row sits.
    pub fn under<S: Into<Step>>(mut self, steps: impl IntoIterator<Item = S>) -> Self {
        let lead = |rest: Vec<Step>| steps.into_iter().map(Into::into).chain(rest).collect();
        self.at = match self.at {
            At::Whole => At::Entry(lead(Vec::new())),
            At::Entry(rest) => At::Entry(lead(rest)),
            At::Value(rest) => At::Value(lead(rest)),
            At::Range(range) => At::Range(range),
        };
        self
    }

    /// The report of this refusal in `source`, the manifest's text: about the place its steps lead to, shown with its line, and about the file as a whole where it names no place or the text holds none there.
    pub fn located(self, source: &Arc<Source>) -> Report {
        let Self { said, at, tries } = self;
        let place = found(source.text(), &at)
            .map(|range| About::Place(Span::new(Arc::clone(source), range.start, range.end)));
        let whole = || source.path().map(|path| About::Path(path.to_path_buf()));
        let report = match place.or_else(whole) {
            Some(about) => Report::about(about, said),
            None => Report::of(None, said),
        };

        tries.into_iter().fold(report, Report::trying)
    }
}

/// Where `at` stands in `text`, a manifest's, where it stands anywhere.
fn found(text: &str, at: &At) -> Option<Range<usize>> {
    let (steps, keyed) = match at {
        At::Whole => return None,
        At::Range(range) => return Some(range.clone()),
        At::Entry(steps) => (steps, true),
        At::Value(steps) => (steps, false),
    };
    let document = Document::parse(text).ok()?;
    let mut node = Node::Table(document.as_table());
    let mut key = None;
    for step in steps {
        (key, node) = node.stepped(step)?;
    }

    match (keyed, key) {
        (true, Some(key)) => Some(key),
        _ => node.span(),
    }
}

/// What a step into a manifest's text arrives at.
enum Node<'a> {
    Table(&'a Table),
    Item(&'a Item),
    Value(&'a Value),
}

impl<'a> Node<'a> {
    /// What `step` arrives at from here, behind where the key that leads there is written, for a step that has one.
    fn stepped(&self, step: &Step) -> Option<(Option<Range<usize>>, Node<'a>)> {
        match (self, step) {
            (_, Step::Key(name)) => {
                let table: &'a dyn TableLike = match self {
                    Node::Table(table) => *table,
                    Node::Item(item) => item.as_table_like()?,
                    Node::Value(value) => value.as_inline_table()?,
                };
                let (key, item) = table.get_key_value(name)?;

                Some((key.span(), Node::Item(item)))
            }
            (Node::Item(Item::ArrayOfTables(rows)), Step::Row(index)) => {
                Some((None, Node::Table(rows.get(*index)?)))
            }
            (
                Node::Item(Item::Value(Value::Array(array))) | Node::Value(Value::Array(array)),
                Step::Row(index),
            ) => Some((None, Node::Value(array.get(*index)?))),
            _ => None,
        }
    }

    fn span(&self) -> Option<Range<usize>> {
        match self {
            Node::Table(table) => table.span(),
            Node::Item(item) => item.span(),
            Node::Value(value) => value.span(),
        }
    }
}
