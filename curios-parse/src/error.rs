use {
    super::{ParserState, obtained, shown, unseen},
    curios_common::{Report, Said, Source, Span},
    std::{borrow::Cow, sync::Arc},
};

/// What a failure the grammar words says, a fact a line in the order a reader is told them: what stands here, then the rule it is held to, then what to write. One statement is a fact alone, so a message of one is written as its text.
///
/// **A constant's facts are borrowed.** Nearly every failure is backtracked and dropped, so one a grammar words from a constant, a text or a list of them, allocates nothing; only facts a failure spells as it is raised are owned.
#[derive(Debug, Clone)]
pub struct Stated(Facts);

#[derive(Debug, Clone)]
enum Facts {
    One(Cow<'static, str>),
    Constant(&'static [&'static str]),
    Spelled(Vec<String>),
}

impl Stated {
    fn facts(&self) -> Vec<&str> {
        match &self.0 {
            Facts::One(fact) => vec![fact],
            Facts::Constant(facts) => facts.to_vec(),
            Facts::Spelled(facts) => facts.iter().map(String::as_str).collect(),
        }
    }
}

impl From<&'static str> for Stated {
    fn from(fact: &'static str) -> Self {
        Self(Facts::One(Cow::Borrowed(fact)))
    }
}

impl From<String> for Stated {
    fn from(fact: String) -> Self {
        Self(Facts::One(Cow::Owned(fact)))
    }
}

impl From<&'static [&'static str]> for Stated {
    fn from(facts: &'static [&'static str]) -> Self {
        Self(Facts::Constant(facts))
    }
}

impl<const N: usize> From<&'static [&'static str; N]> for Stated {
    fn from(facts: &'static [&'static str; N]) -> Self {
        Self(Facts::Constant(facts))
    }
}

impl From<Vec<String>> for Stated {
    fn from(facts: Vec<String>) -> Self {
        Self(Facts::Spelled(facts))
    }
}

/// What a failure says.
///
/// **Rendered where the failure is reported, not where it is raised.** A grammar tries a literal at every position an alternative could begin, and nearly every such failure is backtracked and dropped: one that formatted its message as it was raised would allocate for each. So a missed literal holds the literal, and what stood in its place is read off the source when a report asks; a message the grammar wrote is borrowed where it is a constant.
#[derive(Debug, Clone)]
enum Message {
    /// Facts in hand.
    Said(Stated),
    /// A literal that was not there.
    Expected(&'static str),
    /// A literal that was there and must not be.
    Unexpected(&'static str),
}

/// A parse failure: a message at a byte offset into its source. It also carries the commitment flag: an error [`commit`](crate::commit) marked aborts [`Parser::or`](crate::Parser::or) and the repetition combinators instead of being backtracked, and every other error backtracks. Outside this crate its fields are private: a report is read through [`ParserError::report`] or [`ParserError::format`], and a caller recovering past the failure reads what it was about through [`ParserError::tagged`].
#[derive(Debug, Clone)]
pub struct ParserError {
    committed: bool,
    offset: usize,
    /// Where the report's span begins when the failure is about a run of text rather than a point — a keyword read and refused, whose caret then underlines the word instead of standing after it. Backtracking reads `committed` alone; `offset` only ranks two uncommitted failures against each other. The span's start feeds neither.
    from: Option<usize>,
    message: Message,
    source: Arc<Source>,
    /// What the failure was about, when an alternative said so with [`tagging`](crate::tagging): the declaration whose head it had read. Read by a caller recovering past the failure, to say what was there; nothing else consults it.
    tag: Option<String>,
}

impl ParserError {
    pub(crate) fn new<M>(state: ParserState, message: M) -> Self
    where
        M: Into<Stated>,
    {
        Self::saying(state, Message::Said(message.into()))
    }

    /// Where the failure sits, a byte offset into its source.
    pub(crate) fn offset(&self) -> usize {
        self.offset
    }

    /// Whether the failure sits where `state` stands: one its parser raised before reading anything.
    pub(crate) fn stands_at(&self, state: &ParserState) -> bool {
        self.offset == state.offset()
    }

    /// Of this failure and `other`, the one that read further, this one on a tie: which of two uncommitted alternatives a report names.
    pub(crate) fn further(self, other: Self) -> Self {
        match self.offset >= other.offset {
            true => self,
            false => other,
        }
    }

    /// The failure of a literal that was not at `state`: the literal expected, and what was obtained in its place as [`obtained`] says it, up to the literal's length.
    pub(crate) fn expected(state: ParserState, expected: &'static str) -> Self {
        Self::saying(state, Message::Expected(expected))
    }

    /// The failure of a literal that is at `state` and must not be.
    pub(crate) fn unexpected(state: ParserState, unexpected: &'static str) -> Self {
        Self::saying(state, Message::Unexpected(unexpected))
    }

    fn saying(state: ParserState, message: Message) -> Self {
        Self {
            committed: false,
            offset: state.offset(),
            from: None,
            message,
            source: state.source().clone(),
            tag: None,
        }
    }

    /// What the failure says: that the text is no form of the grammar, and beneath it the facts — see [`Message`].
    ///
    /// A failure whose caret stands at a character its reader cannot see names the character, where what it says does not already: the caret would point at nothing, or at what reads as a blank between two tokens.
    fn said(&self) -> Said {
        let mut said = Said::line("syntax error");
        match &self.message {
            Message::Said(stated) => stated
                .facts()
                .into_iter()
                .for_each(|fact| said.stating(fact)),
            Message::Unexpected(unexpected) => {
                said.stating(format_args!("unexpected: `{unexpected}`"));
            }
            Message::Expected(expected) => {
                said.stating(format_args!("expected: `{expected}`"));
                said.stating(format_args!(
                    "obtained: {}",
                    obtained(&self.source.text()[self.offset..], expected.chars().count())
                ));
            }
        }

        let at = self.from.unwrap_or(self.offset);
        if let Some(char) = self.source.text()[at..].chars().next()
            && unseen(char)
        {
            let code = shown(char.encode_utf8(&mut [0; 4]));
            if !said.to_string().contains(&code) {
                said.stating(format_args!(
                    "{code} stands here, a character the grammar does not take"
                ));
            }
        }

        said
    }

    /// Whether an alternative [`commit`](crate::commit)ted to this failure — the diagnosis, rather than a guess a sibling may still improve on. What [`Parser::or`](crate::Parser::or) and the repetition combinators stop at.
    pub(crate) fn is_committed(&self) -> bool {
        self.committed
    }

    pub(crate) fn tag(self, tag: Option<String>) -> Self {
        Self { tag, ..self }
    }

    /// What the failure was about, when an alternative said so — see [`tagging`](crate::tagging).
    pub fn tagged(&self) -> Option<&str> {
        self.tag.as_deref()
    }

    pub(crate) fn from(self, start: usize) -> Self {
        Self {
            from: Some(start.min(self.offset)),
            ..self
        }
    }

    pub(crate) fn uncommit(self) -> Self {
        Self {
            committed: false,
            ..self
        }
    }

    pub(crate) fn commit(self) -> Self {
        Self {
            committed: true,
            ..self
        }
    }

    pub(crate) fn with_message<M: Into<Stated>>(self, message: M) -> Self {
        Self {
            message: Message::Said(message.into()),
            ..self
        }
    }

    /// The error as data: what it says at a span ending at the failure offset — empty, at the point the parser stopped, unless the failure named the run of text it is about — which is what the caret of [`format`](Self::format) points at, so a consumer reading the span sees exactly where the rendering does.
    pub fn report(&self) -> Report {
        Report::of(
            Some(Span::new(
                self.source.clone(),
                self.from.unwrap_or(self.offset),
                self.offset,
            )),
            self.said(),
        )
    }

    /// Renders the error for humans: the message, then a caret snippet pointing into the offending line — the form the CLI and pipeline surface to the user. [`report`](Self::report) rendered, so the two cannot disagree about where.
    pub fn format(&self) -> String {
        self.report().render()
    }
}
