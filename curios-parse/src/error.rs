use {
    super::ParserState,
    curios_utilities::{Report, Source, Span},
    std::sync::Arc,
};

/// A parse failure: a message at a byte offset into its source. It also carries the commitment flag: an error [`commit`](crate::commit) marked aborts [`Parser::or`](crate::Parser::or) and the repetition combinators instead of being backtracked, and every other error backtracks. Outside this crate its fields are private: a report is read through [`ParserError::report`] or [`ParserError::format`], and a caller recovering past the failure reads what it was about through [`ParserError::tagged`].
#[derive(Debug, Clone)]
pub struct ParserError {
    committed: bool,
    pub(crate) offset: usize,
    /// Where the report's span begins when the failure is about a run of text rather than a point — a keyword read and refused, whose caret then underlines the word instead of standing after it. Backtracking reads `committed` alone; `offset` only ranks two uncommitted failures against each other. The span's start feeds neither.
    from: Option<usize>,
    message: String,
    source: Arc<Source>,
    /// What the failure was about, when an alternative said so with [`tagging`](crate::tagging): the declaration whose head it had read. Read by a caller recovering past the failure, to say what was there; nothing else consults it.
    tag: Option<String>,
}

impl ParserError {
    pub(crate) fn new<M>(state: ParserState, message: M) -> Self
    where
        M: Into<String>,
    {
        Self {
            committed: false,
            offset: state.offset,
            from: None,
            message: message.into(),
            source: state.source.clone(),
            tag: None,
        }
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

    pub(crate) fn with_message<M: Into<String>>(self, message: M) -> Self {
        Self {
            message: message.into(),
            ..self
        }
    }

    /// The error as data: its message at a span ending at the failure offset — empty, at the point the parser stopped, unless the failure named the run of text it is about — which is what the caret of [`format`](Self::format) points at, so a consumer reading the span sees exactly where the rendering does.
    pub fn report(&self) -> Report {
        Report::at(
            Span::new(
                self.source.clone(),
                self.from.unwrap_or(self.offset),
                self.offset,
            ),
            self.message.clone(),
        )
    }

    /// Renders the error for humans: the message, then a caret snippet pointing into the offending line — the form the CLI and pipeline surface to the user. [`report`](Self::report) rendered, so the two cannot disagree about where.
    pub fn format(&self) -> String {
        self.report().render()
    }
}
