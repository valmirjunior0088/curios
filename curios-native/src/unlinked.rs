//! What the host refuses of a foreign module, or of what a program is linked against, before the program runs.

use std::fmt;

/// A refusal of linking as data: its tag, the facts beneath it, and the module it is said of where it is of one, by the package whose manifest declares the module's row and the name that row gives it.
///
/// **Data, because two readers show it two ways.** The compiler places it at the row that declares the module and draws it as it draws every report; a built executable has no compiler beside it, and says it on one line ([`fmt::Display`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unlinked {
    pub tag: &'static str,
    pub facts: Vec<String>,
    pub module: Option<(String, String)>,
}

impl Unlinked {
    /// A refusal under `tag`, of no module in particular until it is said of one.
    pub fn of(tag: &'static str) -> Self {
        Self {
            tag,
            facts: Vec::new(),
            module: None,
        }
    }

    /// This refusal, resting on `fact` as well.
    pub fn stating(mut self, fact: impl fmt::Display) -> Self {
        self.facts.push(fact.to_string());
        self
    }

    /// This refusal, of the module the row `name` of the package `package` declares.
    pub fn about(mut self, package: &str, name: &str) -> Self {
        self.module = Some((package.to_string(), name.to_string()));
        self
    }
}

impl fmt::Display for Unlinked {
    /// The refusal on one line, as a program that could not be started says it: the tag, then each fact.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.facts.is_empty() {
            true => formatter.write_str(self.tag),
            false => write!(formatter, "{}: {}", self.tag, self.facts.join("; ")),
        }
    }
}
