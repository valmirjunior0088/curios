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

    /// Ordered choice: the second alternative is tried whenever the first failed, however much input it read, unless that failure was [`commit`]ted — a commitment says that alternative was the right branch and its error is the real diagnosis.
    ///
    /// Commitment is read on *both* sides, and it outranks the offset. When neither alternative committed, the error that got further into the input is reported, since it is almost always the more informative one — but that is a heuristic over two guesses, and a committed error is not a guess. Weighing a commitment by offset would discard the diagnosis an alternative asked for whenever a sibling happened to read further before giving up.
    pub fn or(self, parser: Parser<'a, A>) -> Self {
        Parser::new(move |state| {
            let first = match self.parse(state) {
                Ok((item, state)) => return Ok((item, state)),
                Err(error) if error.is_uncaught() => return Err(error),
                Err(error) => error,
            };

            let second = match parser.parse(state) {
                Ok((item, state)) => return Ok((item, state)),
                Err(error) if error.is_uncaught() => return Err(error),
                Err(error) => error,
            };

            if first.offset >= second.offset {
                Err(first)
            } else {
                Err(second)
            }
        })
    }

    /// Sequences two parsers and pairs their outputs. A failure in either half backtracks like any other, so an alternative that must own its tail says so with [`commit`] once its discriminating prefix is read.
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

/// Marks the parser's failure as the diagnosis, so an enclosing [`Parser::or`] or repetition stops at it instead of trying the next alternative.
///
/// The one source of commitment: a failure backtracks until something says otherwise. Written once an alternative has read the prefix that discriminates it — `parse_struct_pattern` reads `Name {` and commits, so a missing `}` is reported against the pattern rather than sending the whole term grammar looking for another reading. [`uncommit`] is how a caller that may legitimately re-read the same text takes it back.
pub fn commit<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(ParserError::commit))
}

/// Takes back a [`commit`] made inside `parser`, so an enclosing [`Parser::or`] or repetition may still try its next alternative. The dual of `commit`, and the only way past one: commitment does not decay with distance, so a refusal a nested grammar commits to travels out of every caller that does not stop it here.
///
/// Written where one grammar deliberately refuses what another may legitimately re-read — `parse_bind_arm` over `parse_qualified_match_pattern`, whose refusal of `Option/some(n)` as a constructor pattern must not prevent a `choose` condition arm from reading the same text as an ordinary call.
pub fn uncommit<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(|error| error.uncommit()))
}

/// Runs the parser and hands its output back at the position it started from, consuming nothing. A positive look-ahead — the dual of [`not_ahead`](crate::not_ahead) — for a grammar that must inspect the next word before choosing among alternatives none of which may be denied their turn at it.
pub fn look_ahead<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map(|(item, _)| (item, state)))
}

/// Defers building the parser until it is actually run. This is what lets the grammar be recursive: `parse_term`'s alternatives refer to `lazy(parse_term)` instead of calling it eagerly, which would recurse forever while merely *constructing* the parser.
pub fn lazy<'a, T, F>(f: F) -> Parser<'a, T>
where
    T: 'a,
    F: FnOnce() -> Parser<'a, T> + 'a,
{
    Parser::new(move |state| f().parse(state))
}
