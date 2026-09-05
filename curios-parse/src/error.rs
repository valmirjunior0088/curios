use {
    super::ParserState,
    curios_utilities::{Report, Source, Span},
    std::rc::Rc,
};

/// A parse failure: a message at a byte offset into its source. It also carries the commitment flag behind progress-based backtracking — an error that is still fatal and sits past the choice point aborts [`Parser::or`](crate::Parser::or) and the repetition combinators instead of being backtracked, unless [`catch`](crate::catch) downgraded it. Outside this crate the error is opaque except for [`ParserError::format`].
#[derive(Debug, Clone)]
pub struct ParserError {
    fatal: bool,
    pub(crate) offset: usize,
    /// Where the report's span begins when the failure is about a run of text rather than a point — a keyword read and refused, whose caret then underlines the word instead of standing after it. Commitment reads `offset` alone, so the span's start changes nothing about backtracking.
    from: Option<usize>,
    message: String,
    source: Rc<Source>,
}

impl ParserError {
    pub(crate) fn new<M>(state: ParserState, message: M) -> Self
    where
        M: Into<String>,
    {
        Self {
            fatal: true,
            offset: state.offset,
            from: None,
            message: message.into(),
            source: state.source.clone(),
        }
    }

    pub(crate) fn from(self, start: usize) -> Self {
        Self {
            from: Some(start.min(self.offset)),
            ..self
        }
    }

    pub(crate) fn catch(self) -> Self {
        Self {
            fatal: false,
            ..self
        }
    }

    pub(crate) fn commit(self) -> Self {
        Self {
            fatal: true,
            ..self
        }
    }

    pub(crate) fn with_message<M: Into<String>>(self, message: M) -> Self {
        Self {
            message: message.into(),
            ..self
        }
    }

    pub(crate) fn is_uncaught(&self, state: ParserState) -> bool {
        self.fatal && self.offset != state.offset
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
