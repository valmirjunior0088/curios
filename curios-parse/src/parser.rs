use {
    super::{ParserError, ParserState},
    std::borrow::Cow,
};

/// What a parser answers at a position: its value and where the input stands past it, or a [`ParserError`].
pub type Parsed<'a, A> = Result<(A, ParserState<'a>), ParserError>;

/// A single-use parser: anything that is run once at an input position and answers a value with the rest of the input, or a [`ParserError`]. Being single-use lets a combinator move what it captured into its result without cloning, and is why the repetition combinators ([`many0`](crate::many0), [`sep_by0`](crate::sep_by0), ...) take what hands them a parser for each iteration ([`Repeated`](crate::Repeated)).
///
/// **Every function of the input is one**, so a nonterminal is a plain function — `fn term<'a>(state: ParserState<'a>) -> Parsed<'a, Term>` — whose body builds its combinators as values of its own and runs them, and a combinator returns the closure it is. Nothing is boxed: a grammar function is compiled once and run at every position, a combinator it builds on the way is a value on its stack, and a nonterminal that names itself, or another that names it back, is a function calling a function.
///
/// **A grammar function that takes arguments returns one closure of the input**, which builds its chain where it runs — `move |state| take_exact(expected).and_drop(parse_whitespace).parse(state)` — so its type is what it captures. Returned as the chain itself, its type is the chain's, and a function over such a function nests it into the type of every caller, which the compiler's checks of each caller pay for, the borrow checker's most.
pub trait Parser<'a, A>: Sized {
    /// Run at `state`.
    fn parse(self, state: ParserState<'a>) -> Parsed<'a, A>;

    /// Ordered choice: the second alternative is tried whenever the first failed, however much input it read, unless that failure was [`commit`]ted — a commitment says that alternative was the right branch and its error is the real diagnosis.
    ///
    /// Commitment is read on *both* sides, and it outranks the offset. When neither alternative committed, the error that got further into the input is reported, since it is almost always the more informative one — but that is a heuristic over two guesses, and a committed error is not a guess. Weighing a commitment by offset would discard the diagnosis an alternative asked for whenever a sibling happened to read further before giving up.
    fn or<M>(self, parser: impl IntoParser<'a, A, M>) -> impl Parser<'a, A> {
        move |state: ParserState<'a>| {
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
        }
    }

    /// Sequences two parsers and pairs their outputs. A failure in either half backtracks like any other, so an alternative that must own its tail says so with [`commit`] once its discriminating prefix is read.
    fn and<B, M>(self, parser: impl IntoParser<'a, B, M>) -> impl Parser<'a, (A, B)> {
        move |state: ParserState<'a>| {
            let (left, state) = self.parse(state)?;
            let (right, state) = parser.into_parser().parse(state)?;

            Ok(((left, right), state))
        }
    }

    /// Sequences like [`Parser::and`] but keeps only the *left* output — for trailing punctuation or whitespace that must be consumed but carries no information.
    fn and_drop<B, M>(self, parser: impl IntoParser<'a, B, M>) -> impl Parser<'a, A> {
        move |state: ParserState<'a>| {
            let (left, state) = self.parse(state)?;
            let (_, state) = parser.into_parser().parse(state)?;

            Ok((left, state))
        }
    }

    /// Sequences like [`Parser::and`] but keeps only the *right* output — for a leading keyword or opening delimiter whose text carries no information once matched.
    fn and_keep<B, M>(self, parser: impl IntoParser<'a, B, M>) -> impl Parser<'a, B> {
        move |state: ParserState<'a>| {
            let (_, state) = self.parse(state)?;

            parser.into_parser().parse(state)
        }
    }

    /// Transforms the parsed value on success — the functor map; consumption and failure behavior are untouched.
    fn map<B>(self, f: impl FnOnce(A) -> B) -> impl Parser<'a, B> {
        move |state: ParserState<'a>| {
            let (item, state) = self.parse(state)?;

            Ok((f(item), state))
        }
    }

    /// Replaces the failure's message while keeping its offset and commitment, so a low-level token error ("Expected '('...") can be reworded as a domain-level one without changing where the caret points or how [`Parser::or`] commitment behaves.
    fn map_err(self, message: impl Into<Cow<'static, str>>) -> impl Parser<'a, A> {
        move |state: ParserState<'a>| {
            self.parse(state)
                .map_err(|error| error.with_message(message))
        }
    }

    /// Monadic bind: the next parser is *chosen from* the first's output, which is what [`Parser::and`] cannot express — e.g. dispatching on the character just read, or turning a parsed value into [`fail`](crate::fail) after semantic inspection. `f` answers one type of parser, so branches that answer parsers of different shapes are each [`Parser::boxed`].
    fn flat_map<B, P>(self, f: impl FnOnce(A) -> P) -> impl Parser<'a, B>
    where
        P: Parser<'a, B>,
    {
        move |state: ParserState<'a>| {
            let (item, state) = self.parse(state)?;

            f(item).parse(state)
        }
    }

    /// This parser boxed — see [`Boxed`].
    fn boxed(self) -> Boxed<'a, A>
    where
        Self: 'a,
    {
        Boxed(Box::new(move |state| self.parse(state)))
    }

    /// Transforms the parsed value into another, or into a refusal said as the message `f` answers, standing where the value ended: what a grammar writes where it reads a run of characters and then decides whether it spells what was wanted.
    fn and_then<B, S>(self, f: impl FnOnce(A) -> Result<B, S>) -> impl Parser<'a, B>
    where
        S: Into<Cow<'static, str>>,
    {
        move |state: ParserState<'a>| {
            let (item, state) = self.parse(state)?;

            match f(item) {
                Ok(item) => Ok((item, state)),
                Err(message) => Err(ParserError::new(state, message)),
            }
        }
    }
}

impl<'a, A, F> Parser<'a, A> for F
where
    F: FnOnce(ParserState<'a>) -> Parsed<'a, A>,
{
    fn parse(self, state: ParserState<'a>) -> Parsed<'a, A> {
        self(state)
    }
}

/// A parser whose shape is chosen where it runs, boxed: what the arms of a choice made on a value read take, where each arm is a parser of another shape. The one allocation a grammar makes for a parser, at the site that asks for it and only where that site is reached.
pub struct Boxed<'a, A>(Box<dyn FnOnce(ParserState<'a>) -> Parsed<'a, A> + 'a>);

impl<'a, A> Parser<'a, A> for Boxed<'a, A> {
    fn parse(self, state: ParserState<'a>) -> Parsed<'a, A> {
        (self.0)(state)
    }
}

/// What a combinator takes where it wants a parser: a [`Parser`] in hand, or a function that builds one, which is called where the parser would run, and nowhere else. `M` says which, and is never written: it is what lets one argument be read both ways.
pub trait IntoParser<'a, A, M> {
    type Parser: Parser<'a, A>;

    /// The parser, built now.
    fn into_parser(self) -> Self::Parser;
}

/// A parser in hand.
pub struct Ready;

/// A function that builds a parser.
pub struct Built;

impl<'a, A, P> IntoParser<'a, A, Ready> for P
where
    P: Parser<'a, A>,
{
    type Parser = P;

    fn into_parser(self) -> P {
        self
    }
}

impl<'a, A, P, F> IntoParser<'a, A, Built> for F
where
    P: Parser<'a, A>,
    F: FnOnce() -> P,
{
    type Parser = P;

    fn into_parser(self) -> P {
        self()
    }
}

/// Marks the parser's failure as the diagnosis, so an enclosing [`Parser::or`] or repetition stops at it instead of trying the next alternative.
///
/// The one source of commitment: a failure backtracks until something says otherwise. Written once an alternative has read the prefix that discriminates it — `parse_func` reads `(…) =>` and commits to the body, so a broken body is reported against the lambda rather than sending the whole term grammar looking for another reading. [`uncommit`] is how a caller that may legitimately re-read the same text takes it back.
pub fn commit<'a, T, M>(parser: impl IntoParser<'a, T, M>) -> impl Parser<'a, T> {
    move |state: ParserState<'a>| {
        parser
            .into_parser()
            .parse(state)
            .map_err(ParserError::commit)
    }
}

/// `parser`, its failure said as `message` where it read nothing: what a production of several alternatives takes. Each alternative that declines at the production's first character fails there, and [`Parser::or`] reports the first of them, which names one alternative's word and not the production's. A failure past the first character, or a committed one, is an alternative's own diagnosis and stands.
pub fn expecting<'a, T, M>(
    message: &'static str,
    parser: impl IntoParser<'a, T, M>,
) -> impl Parser<'a, T> {
    move |state: ParserState<'a>| {
        let start = state.offset;

        parser.into_parser().parse(state).map_err(|error| {
            match error.is_committed() || error.offset != start {
                true => error,
                false => error.with_message(message),
            }
        })
    }
}

/// Takes back a [`commit`] made inside `parser`, so an enclosing [`Parser::or`] or repetition may still try its next alternative. The dual of `commit`, and the only way past one: commitment does not decay with distance, so a refusal a nested grammar commits to travels out of every caller that does not stop it here.
///
/// Written where one grammar deliberately refuses what another may legitimately re-read — `parse_func_type` over its parameter telescope, whose refusal of `(a, b)` as a list of annotated binders must not prevent the same text being read as a tuple or a parenthesized term.
pub fn uncommit<'a, T, M>(parser: impl IntoParser<'a, T, M>) -> impl Parser<'a, T> {
    move |state: ParserState<'a>| {
        parser
            .into_parser()
            .parse(state)
            .map_err(|error| error.uncommit())
    }
}

/// Names what `parser`'s failure is about — the declaration whose head an alternative had read, or `None` to say it is about nothing nameable — for a caller that recovers past the failure and needs to say what was there. The outermost name wins, since each wrapper restates it on the way out: a failure inside an inline body belongs to the item enclosing it, and a body's own local binding is not what a top-level item declared.
pub fn tagging<'a, T, M>(
    tag: Option<String>,
    parser: impl IntoParser<'a, T, M>,
) -> impl Parser<'a, T> {
    move |state: ParserState<'a>| {
        parser
            .into_parser()
            .parse(state)
            .map_err(|error| error.tag(tag))
    }
}

/// Defers building the parser until it is run: a nonterminal handed over as the function that builds it. A function of the input needs none of it, being run where it is reached.
pub fn lazy<'a, T, M>(parser: impl IntoParser<'a, T, M>) -> impl Parser<'a, T> {
    move |state: ParserState<'a>| parser.into_parser().parse(state)
}

/// Runs the parser and hands its output back at the position it started from, consuming nothing. A positive look-ahead — the dual of [`not_ahead`](crate::not_ahead) — for a grammar that must inspect the next word before choosing among alternatives none of which may be denied their turn at it.
pub fn look_ahead<'a, T, M>(parser: impl IntoParser<'a, T, M>) -> impl Parser<'a, T> {
    move |state: ParserState<'a>| {
        parser
            .into_parser()
            .parse(state)
            .map(|(item, _)| (item, state))
    }
}
