use {
    super::ParserState,
    curios_utilities::{Source, Span},
    std::rc::Rc,
};

/// A parse failure: a message at a byte offset into its source. It also carries the commitment flag behind progress-based backtracking — an error that is still fatal and sits past the choice point aborts [`Parser::or`](crate::Parser::or) and the repetition combinators instead of being backtracked, unless [`catch`](crate::catch) downgraded it. Outside this crate the error is opaque except for [`ParserError::format`].
#[derive(Debug, Clone)]
pub struct ParserError {
    fatal: bool,
    pub(crate) offset: usize,
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
            message: message.into(),
            source: state.source.clone(),
        }
    }

    pub(crate) fn catch(self) -> Self {
        Self {
            fatal: false,
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

    /// Renders the error for humans: the message, then a caret snippet (via `Span::render_snippet` on an empty span at the failure offset) pointing into the offending line — the form the CLI and pipeline surface to the user.
    pub fn format(&self) -> String {
        format!(
            "{message}\n\n{snippet}",
            message = self.message,
            snippet = Span::new(self.source.clone(), self.offset, self.offset).render_snippet(),
        )
    }
}
