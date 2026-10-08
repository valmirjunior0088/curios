use {
    super::{ParserError, ParserState},
    std::borrow::Cow,
};

type ParserResult<'a, A> = Result<(A, ParserState<'a>), ParserError>;

/// A single-use parser: a boxed `FnOnce` from an input position to a value plus the rest of the input, or a [`ParserError`]. Being `FnOnce` lets combinators move captured values into results without cloning, and is why the repetition combinators ([`many0`](crate::many0), [`sep_by0`](crate::sep_by0), ...) take parser-*building* closures — each iteration needs a fresh instance.
pub struct Parser<'a, A>(Box<dyn FnOnce(ParserState<'a>) -> ParserResult<'a, A> + 'a>);

/// What a combinator takes where it wants a parser: a [`Parser`] in hand, or a nonterminal — a function that builds one — which is called where the parser would run, and nowhere else.
///
/// **A parser that is never run is never built.** A grammar function answers a parser by building it, every operand with it, and most of what a grammar builds an ordered choice never reaches: the alternatives after the one that matched, and what follows a token that was not there. An operand written as its nonterminal — `term.and_drop(parse_whitespace)`, `open.or(|| parse_literal("["))` — costs nothing until the combinator holding it gets that far, since a function that captures nothing is no allocation; written as a call, it is built where the combinator is.
pub trait IntoParser<'a, A> {
    /// The parser, built now.
    fn into_parser(self) -> Parser<'a, A>;
}

impl<'a, A> IntoParser<'a, A> for Parser<'a, A> {
    fn into_parser(self) -> Self {
        self
    }
}

impl<'a, A, F> IntoParser<'a, A> for F
where
    F: FnOnce() -> Parser<'a, A>,
{
    fn into_parser(self) -> Parser<'a, A> {
        self()
    }
}

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
    pub fn or(self, parser: impl IntoParser<'a, A> + 'a) -> Self {
        Parser::new(move |state| {
            let first = match self.parse(state) {
                Ok((item, state)) => return Ok((item, state)),
                Err(error) if error.is_committed() => return Err(error),
                Err(error) => error,
            };

            let second = match parser.into_parser().parse(state) {
                Ok((item, state)) => return Ok((item, state)),
                Err(error) if error.is_committed() => return Err(error),
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
    pub fn and<B>(self, parser: impl IntoParser<'a, B> + 'a) -> Parser<'a, (A, B)>
    where
        B: 'a,
    {
        Parser::new(move |state| {
            let (left, state) = self.parse(state)?;
            let (right, state) = parser.into_parser().parse(state)?;

            Ok(((left, right), state))
        })
    }

    /// Sequences like [`Parser::and`] but keeps only the *left* output — for trailing punctuation or whitespace that must be consumed but carries no information.
    pub fn and_drop<B>(self, parser: impl IntoParser<'a, B> + 'a) -> Parser<'a, A>
    where
        B: 'a,
    {
        Parser::new(move |state| {
            let (left, state) = self.parse(state)?;
            let (_, state) = parser.into_parser().parse(state)?;

            Ok((left, state))
        })
    }

    /// Sequences like [`Parser::and`] but keeps only the *right* output — for a leading keyword or opening delimiter whose text carries no information once matched.
    pub fn and_keep<B>(self, parser: impl IntoParser<'a, B> + 'a) -> Parser<'a, B>
    where
        B: 'a,
    {
        Parser::new(move |state| {
            let (_, state) = self.parse(state)?;
            let (right, state) = parser.into_parser().parse(state)?;

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

    /// Replaces the failure's message while keeping its offset and commitment, so a low-level token error ("Expected '('...") can be reworded as a domain-level one without changing where the caret points or how [`Parser::or`] commitment behaves.
    pub fn map_err<M>(self, message: M) -> Parser<'a, A>
    where
        M: Into<Cow<'static, str>> + 'a,
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
/// The one source of commitment: a failure backtracks until something says otherwise. Written once an alternative has read the prefix that discriminates it — `parse_func` reads `(…) =>` and commits to the body, so a broken body is reported against the lambda rather than sending the whole term grammar looking for another reading. [`uncommit`] is how a caller that may legitimately re-read the same text takes it back.
pub fn commit<'a, T>(parser: impl IntoParser<'a, T> + 'a) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| {
        parser
            .into_parser()
            .parse(state)
            .map_err(ParserError::commit)
    })
}

/// Takes back a [`commit`] made inside `parser`, so an enclosing [`Parser::or`] or repetition may still try its next alternative. The dual of `commit`, and the only way past one: commitment does not decay with distance, so a refusal a nested grammar commits to travels out of every caller that does not stop it here.
///
/// Written where one grammar deliberately refuses what another may legitimately re-read — `parse_func_type` over its parameter telescope, whose refusal of `(a, b)` as a list of annotated binders must not prevent the same text being read as a tuple or a parenthesized term.
pub fn uncommit<'a, T>(parser: impl IntoParser<'a, T> + 'a) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| {
        parser
            .into_parser()
            .parse(state)
            .map_err(|error| error.uncommit())
    })
}

/// Names what `parser`'s failure is about — the declaration whose head an alternative had read, or `None` to say it is about nothing nameable — for a caller that recovers past the failure and needs to say what was there. The outermost name wins, since each wrapper restates it on the way out: a failure inside an inline body belongs to the item enclosing it, and a body's own local binding is not what a top-level item declared.
pub fn tagging<'a, T>(tag: Option<String>, parser: impl IntoParser<'a, T> + 'a) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| {
        parser
            .into_parser()
            .parse(state)
            .map_err(|error| error.tag(tag))
    })
}

/// Runs the parser and hands its output back at the position it started from, consuming nothing. A positive look-ahead — the dual of [`not_ahead`](crate::not_ahead) — for a grammar that must inspect the next word before choosing among alternatives none of which may be denied their turn at it.
pub fn look_ahead<'a, T>(parser: impl IntoParser<'a, T> + 'a) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| {
        parser
            .into_parser()
            .parse(state)
            .map(|(item, _)| (item, state))
    })
}

/// Defers building the parser until it is actually run. This is what lets the grammar be recursive: `parse_term`'s alternatives refer to `lazy(parse_term)` instead of calling it eagerly, which would recurse forever while merely *constructing* the parser.
pub fn lazy<'a, T, F>(f: F) -> Parser<'a, T>
where
    T: 'a,
    F: FnOnce() -> Parser<'a, T> + 'a,
{
    Parser::new(move |state| f().parse(state))
}
