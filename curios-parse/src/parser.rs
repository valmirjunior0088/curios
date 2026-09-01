use super::{ParserError, ParserState};

type ParserResult<'a, A> = Result<(A, ParserState<'a>), ParserError>;

/// A single-use parser: a boxed `FnOnce` from an input position to a value plus the rest of the input, or a [`ParserError`]. Being `FnOnce` lets combinators move captured values into results without cloning, and is why the repetition combinators ([`many0`](crate::many0), [`sep_by0`](crate::sep_by0), ...) take parser-*building* closures — each iteration needs a fresh instance.
pub struct Parser<'a, A>(Box<dyn FnOnce(ParserState<'a>) -> ParserResult<'a, A> + 'a>);

impl<'a, A> Parser<'a, A>
where
    A: 'a,
{
    pub(crate) fn new<F>(f: F) -> Self
    where
        F: FnOnce(ParserState<'a>) -> ParserResult<'a, A> + 'a,
    {
        Parser(Box::new(f))
    }

    pub(crate) fn parse(self, state: ParserState<'a>) -> ParserResult<'a, A> {
        (self.0)(state)
    }

    /// Ordered choice with progress-based commitment: the second alternative is tried only if the first failed *without consuming input* (or its error was downgraded by [`catch`]) — a failure past the choice point means the first alternative was the right branch and its error is the real diagnosis. When both fail recoverably, the error that got further into the input is reported, since it is almost always the more informative one.
    pub fn or(self, parser: Parser<'a, A>) -> Self {
        Parser::new(move |state| {
            let first = match self.parse(state) {
                Ok((item, state)) => return Ok((item, state)),
                Err(error) if error.is_uncaught(state) => return Err(error),
                Err(error) => error,
            };

            let second = match parser.parse(state) {
                Ok((item, state)) => return Ok((item, state)),
                Err(error) => error,
            };

            if first.offset >= second.offset {
                Err(first)
            } else {
                Err(second)
            }
        })
    }

    /// Sequences two parsers and pairs their outputs. A failure in the second half typically *has* consumed input by then, so under [`Parser::or`]'s progress rule it commits — wrap the whole sequence in [`catch`] when the alternatives share a prefix.
    pub fn and<B>(self, parser: Parser<'a, B>) -> Parser<'a, (A, B)>
    where
        B: 'a,
    {
        Parser::new(move |state| {
            let (left, state) = self.parse(state)?;
            let (right, state) = parser.parse(state)?;

            Ok(((left, right), state))
        })
    }

    /// Sequences like [`Parser::and`] but keeps only the *left* output — for trailing punctuation or whitespace that must be consumed but carries no information.
    pub fn and_drop<B>(self, parser: Parser<'a, B>) -> Parser<'a, A>
    where
        B: 'a,
    {
        Parser::new(move |state| {
            let (left, state) = self.parse(state)?;
            let (_, state) = parser.parse(state)?;

            Ok((left, state))
        })
    }

    /// Sequences like [`Parser::and`] but keeps only the *right* output — for a leading keyword or opening delimiter whose text carries no information once matched.
    pub fn and_keep<B>(self, parser: Parser<'a, B>) -> Parser<'a, B>
    where
        B: 'a,
    {
        Parser::new(move |state| {
            let (_, state) = self.parse(state)?;
            let (right, state) = parser.parse(state)?;

            Ok((right, state))
        })
    }

    /// Transforms the parsed value on success — the functor map; consumption and failure behavior are untouched.
    pub fn map<B, F>(self, f: F) -> Parser<'a, B>
    where
        B: 'a,
        F: FnOnce(A) -> B + 'a,
    {
        Parser::new(move |state| {
            let (item, state) = self.parse(state)?;

            Ok(((f)(item), state))
        })
    }

    /// Replaces the failure's message while keeping its offset and fatality, so a low-level token error ("Expected '('...") can be reworded as a domain-level one without changing where the caret points or how [`Parser::or`] commitment behaves.
    pub fn map_err<M>(self, message: M) -> Parser<'a, A>
    where
        M: Into<String> + 'a,
    {
        Parser::new(move |state| {
            self.parse(state)
                .map_err(|error| error.with_message(message))
        })
    }

    /// Monadic bind: the next parser is *chosen from* the first's output, which is what [`Parser::and`] cannot express — e.g. dispatching on the character just read, or turning a parsed value into [`fail`](crate::fail) after semantic inspection.
    pub fn flat_map<B, F>(self, f: F) -> Parser<'a, B>
    where
        B: 'a,
        F: FnOnce(A) -> Parser<'a, B> + 'a,
    {
        Parser::new(move |state| {
            let (item, state) = self.parse(state)?;

            (f)(item).parse(state)
        })
    }
}

/// Downgrades the parser's failure to recoverable even when it consumed input, so an enclosing [`Parser::or`] or repetition backtracks instead of aborting. The escape hatch from progress-based commitment, for alternatives that share a prefix — e.g. the WAT parser wraps each `(keyword` head in `catch` so consuming the `(` while probing one form doesn't kill the others.
/// Upgrades the parser's failure to fatal, so an enclosing [`Parser::or`] or repetition stops at it instead of trying the next alternative. The dual of [`catch`]: `catch` says a failure must not kill its siblings, this says that past this point the failure *is* the diagnosis.
///
/// Only meaningful once input has been consumed, because commitment is progress-based — [`ParserError::is_uncaught`] asks for a fatal error whose offset has moved past the choice point, so committing a parser that fails without consuming anything still backtracks. The use it exists for is a keyword-dispatched alternative: the head is already eaten when the body runs, so the body's failure is always past the choice point.
pub fn commit<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(ParserError::commit))
}

pub fn catch<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(|error| error.catch()))
}

/// Defers building the parser until it is actually run. This is what lets the grammar be recursive: `parse_term`'s alternatives refer to `lazy(parse_term)` instead of calling it eagerly, which would recurse forever while merely *constructing* the parser.
pub fn lazy<'a, T, F>(f: F) -> Parser<'a, T>
where
    T: 'a,
    F: FnOnce() -> Parser<'a, T> + 'a,
{
    Parser::new(move |state| f().parse(state))
}
