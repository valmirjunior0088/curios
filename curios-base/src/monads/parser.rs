use {
    crate::Source,
    std::{any::Any, cell::RefCell, collections::HashMap, rc::Rc},
};

#[derive(Debug, Clone, Copy)]
struct ParserState<'a> {
    offset: usize,
    string: &'a str,
    source: &'a Rc<Source>,
}

impl<'a> ParserState<'a> {
    fn new(source: &'a Rc<Source>) -> Self {
        Self {
            offset: 0,
            string: &source.text,
            source,
        }
    }

    fn take_exact(self, width: usize) -> Option<(&'a str, Self)> {
        Some((
            self.string.get(..width)?,
            Self {
                offset: self.offset + width,
                string: self.string.get(width..)?,
                source: self.source,
            },
        ))
    }

    fn take_n(self, n: usize) -> Option<(&'a str, Self)> {
        let mut iter = self.string.char_indices();

        for _ in 0..n {
            iter.next()?;
        }

        let width = iter.next().map_or(self.string.len(), |(i, _)| i);

        Some((
            &self.string[..width],
            Self {
                offset: self.offset + width,
                string: &self.string[width..],
                source: self.source,
            },
        ))
    }

    fn take_while<F>(self, mut predicate: F) -> (&'a str, Self)
    where
        F: FnMut(char) -> bool,
    {
        let offset = self
            .string
            .find(|char| !predicate(char))
            .unwrap_or(self.string.len());

        let (output, string) = self.string.split_at(offset);

        (
            output,
            Self {
                offset: self.offset + offset,
                string,
                source: self.source,
            },
        )
    }

    fn is_finished(&self) -> bool {
        self.string.is_empty()
    }

    /// Rebuilds the state at an absolute byte `offset` into the same source. Used
    /// by [`memoize`] to resume from a cached parse without re-walking the input;
    /// all parser offsets are byte offsets, so slicing `source.text` is exact.
    fn jump_to(self, offset: usize) -> Self {
        Self {
            offset,
            string: &self.source.text[offset..],
            source: self.source,
        }
    }
}

/// A parse failure: a message at a byte offset into its source. It also carries the commitment flag behind progress-based backtracking — an error that is still fatal and sits past the choice point aborts [`Parser::or`] and the repetition combinators instead of being backtracked, unless [`catch`] downgraded it. Outside this module the error is opaque except for [`ParserError::format`].
#[derive(Debug, Clone)]
pub struct ParserError {
    fatal: bool,
    offset: usize,
    message: String,
    source: Rc<Source>,
}

impl ParserError {
    fn new<M>(state: ParserState, message: M) -> Self
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

    fn catch(self) -> Self {
        Self {
            fatal: false,
            ..self
        }
    }

    fn with_message<M: Into<String>>(self, message: M) -> Self {
        Self {
            message: message.into(),
            ..self
        }
    }

    fn is_uncaught(&self, state: ParserState) -> bool {
        self.fatal && self.offset != state.offset
    }

    /// Renders the error for humans: the message, then a caret snippet (via `Span::render_snippet` on an empty span at the failure offset) pointing into the offending line — the form the CLI and pipeline surface to the user.
    pub fn format(&self) -> String {
        format!(
            "{message}\n\n{snippet}",
            message = self.message,
            snippet =
                crate::Span::new(self.source.clone(), self.offset, self.offset).render_snippet(),
        )
    }
}

type ParserResult<'a, A> = Result<(A, ParserState<'a>), ParserError>;

/// A single-use parser: a boxed `FnOnce` from an input position to a value plus the rest of the input, or a [`ParserError`]. Being `FnOnce` lets combinators move captured values into results without cloning, and is why the repetition combinators ([`many0`], [`sep_by0`], ...) take parser-*building* closures — each iteration needs a fresh instance.
pub struct Parser<'a, A>(Box<dyn FnOnce(ParserState<'a>) -> ParserResult<'a, A> + 'a>);

impl<'a, A> Parser<'a, A>
where
    A: 'a,
{
    fn new<F>(f: F) -> Self
    where
        F: FnOnce(ParserState<'a>) -> ParserResult<'a, A> + 'a,
    {
        Parser(Box::new(f))
    }

    fn parse(self, state: ParserState<'a>) -> ParserResult<'a, A> {
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

    /// Monadic bind: the next parser is *chosen from* the first's output, which is what [`Parser::and`] cannot express — e.g. dispatching on the character just read, or turning a parsed value into [`fail`] after semantic inspection.
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

/// One cached parse at a given grammar key and start offset: either the produced
/// value (type-erased, since the table is shared across the memoized parsers) paired
/// with the end offset to resume at, or the verbatim error the parser failed with.
type MemoEntry = Result<(Rc<dyn Any>, usize), ParserError>;

thread_local! {
    /// Packrat cache for [`memoize`]d parsers, keyed by `(grammar key, byte offset)`.
    /// Cleared at the start of every [`run_parser`] so offsets never collide across
    /// independent parses. Per-thread, so concurrent parses (e.g. the test suite)
    /// don't share it.
    static MEMO: RefCell<HashMap<(u32, usize), MemoEntry>> = RefCell::new(HashMap::new());
}

/// Wraps a parser so its result at each start offset is computed once and reused.
/// This is what makes the term grammar linear instead of exponential: the same
/// position is probed by several overlapping alternatives (a `(` is tried as a
/// dependent function type, then a non-dependent one, then a lambda, then parens),
/// and without memoization each retry re-parses the whole nested subterm.
///
/// Sound as straight packrat because the wrapped parsers (`parse_term`,
/// `parse_atomic_term`) are pure functions of the offset — parsing carries no
/// symbol table or other context that could make the same input parse differently.
/// `key` distinguishes the grammar nonterminals that share the table. The wrapped
/// parser must never re-enter itself at the *same* offset without consuming input
/// (no left recursion), which the term grammar satisfies.
pub fn memoize<'a, A>(key: u32, parser: Parser<'a, A>) -> Parser<'a, A>
where
    A: Clone + 'static,
{
    Parser::new(move |state| {
        let offset = state.offset;

        if let Some(entry) = MEMO.with(|memo| memo.borrow().get(&(key, offset)).cloned()) {
            return match entry {
                Ok((value, end)) => {
                    let value = value
                        .downcast_ref::<A>()
                        .expect("memoized parser reused a key for a different type")
                        .clone();

                    Ok((value, state.jump_to(end)))
                }
                Err(error) => Err(error),
            };
        }

        let result = parser.parse(state);

        let entry: MemoEntry = match &result {
            Ok((value, next)) => Ok((Rc::new(value.clone()) as Rc<dyn Any>, next.offset)),
            Err(error) => Err(error.clone()),
        };

        MEMO.with(|memo| memo.borrow_mut().insert((key, offset), entry));

        result
    })
}

/// The entry point: runs `parser` from the start of `source`, first clearing the packrat table so [`memoize`]d results from a previous parse can never be replayed against this input. Does *not* require the input to be fully consumed — end the grammar with [`take_eof`] if trailing text should be an error.
pub fn run_parser<'a, A>(parser: Parser<'a, A>, source: &'a Rc<Source>) -> Result<A, ParserError>
where
    A: 'a,
{
    MEMO.with(|memo| memo.borrow_mut().clear());

    parser.parse(ParserState::new(source)).map(|(item, _)| item)
}

/// Succeeds with `a` without consuming input — the monadic return, for injecting an already-known value into a combinator chain.
pub fn pure<'a, A>(a: A) -> Parser<'a, A>
where
    A: 'a,
{
    Parser::new(move |state| Ok((a, state)))
}

/// Always fails with `message` at the current offset. Since it consumes nothing, the failure is recoverable by an enclosing [`Parser::or`] at the same position — the usual way to reject a value from inside [`Parser::flat_map`] after semantic inspection.
pub fn fail<'a, A, S>(message: S) -> Parser<'a, A>
where
    A: 'a,
    S: Into<String> + 'a,
{
    Parser::new(move |state| Err(ParserError::new(state, message)))
}

/// Consumes exactly the literal `expected`, yielding nothing. On mismatch the error sits at the *pre-consumption* offset, so failing here never commits — a keyword or punctuation probe is always safe as the first token of an [`Parser::or`] alternative.
pub fn take_exact<'a>(expected: &'static str) -> Parser<'a, ()> {
    Parser::new(move |state| match state.take_exact(expected.len()) {
        Some((obtained, state)) if expected == obtained => Ok(((), state)),
        Some((obtained, _)) => Err(ParserError::new(
            state,
            format!("Expected '{expected}', obtained '{obtained}'"),
        )),
        None => Err(ParserError::new(
            state,
            format!("Expected '{expected}', obtained 'end-of-file'"),
        )),
    })
}

/// Consumes exactly `expected` characters — counted as `char`s, not bytes — and yields them; fails if the input ends first. How the lexical layer grabs one character to dispatch on via [`Parser::flat_map`], e.g. reading the character after a string-literal `\` escape.
pub fn take_n<'a>(expected: usize) -> Parser<'a, &'a str> {
    Parser::new(move |state| match state.take_n(expected) {
        Some((obtained, state)) => Ok((obtained, state)),
        None => Err(ParserError::new(
            state,
            format!("Expected {expected} character(s), obtained 'end-of-file'"),
        )),
    })
}

/// Consumes the longest (possibly empty) prefix of characters satisfying `f` and yields it. Never fails — pair it with a non-empty check (or start it with a mandatory first character) when the empty match must not count as progress.
pub fn take_while<'a, F>(f: F) -> Parser<'a, &'a str>
where
    F: FnMut(char) -> bool + 'a,
{
    Parser::new(move |state| Ok(state.take_while(f)))
}

/// Succeeds (consuming nothing) only when the input is exhausted. Top-level grammars end with this so trailing garbage is a parse error — [`run_parser`] itself does not require the input to be fully consumed.
pub fn take_eof<'a>() -> Parser<'a, ()> {
    Parser::new(|state| match state.is_finished() {
        true => Ok(((), state)),
        false => Err(ParserError::new(state, "Expected 'end-of-file'")),
    })
}

/// Succeeds (consuming nothing) only when the byte immediately before the
/// current offset is whitespace, or we are at the start of input. The infix
/// operator parser uses it to require a space on the *left* of an operator —
/// the right space is required by consuming whitespace after the symbol — so
/// `a - 42` is subtraction while the glued `-42` stays a literal.
pub fn preceded_by_space<'a>() -> Parser<'a, ()> {
    Parser::new(|state| {
        let preceded = state.offset == 0
            || state.source.text[..state.offset]
                .chars()
                .next_back()
                .is_some_and(char::is_whitespace);

        match preceded {
            true => Ok(((), state)),
            false => Err(ParserError::new(
                state,
                "expected whitespace before operator",
            )),
        }
    })
}

/// Succeeds (consuming nothing) only when the remaining input does *not* start
/// with `unexpected`. A negative look-ahead — e.g. to read a postfix `!` only
/// when it is not the start of the `!=` operator.
pub fn not_ahead<'a>(unexpected: &'static str) -> Parser<'a, ()> {
    Parser::new(move |state| match state.string.starts_with(unexpected) {
        true => Err(ParserError::new(
            state,
            format!("unexpected '{unexpected}'"),
        )),
        false => Ok(((), state)),
    })
}

/// Downgrades the parser's failure to recoverable even when it consumed input, so an enclosing [`Parser::or`] or repetition backtracks instead of aborting. The escape hatch from progress-based commitment, for alternatives that share a prefix — e.g. the WAT parser wraps each `(keyword` head in `catch` so consuming the `(` while probing one form doesn't kill the others.
pub fn catch<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(|error| error.catch()))
}

/// Pairs the parser's output with the [`crate::Span`] covering exactly the bytes it consumed — how surface parsers attach source locations, so wrap the whole construct rather than its pieces.
pub fn spanned<'a, T>(parser: Parser<'a, T>) -> Parser<'a, (crate::Span, T)>
where
    T: 'a,
{
    Parser::new(move |state| {
        let start = state.offset;
        let (item, state) = parser.parse(state)?;

        Ok((
            (
                crate::Span::new(state.source.clone(), start, state.offset),
                item,
            ),
            state,
        ))
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

/// Zero or more repetitions of the parser `f` builds. Takes a parser-building closure rather than a parser because [`Parser`] is single-use — every iteration needs a fresh instance. Stops at the first recoverable failure; an uncaught (fatal, input-consuming) failure aborts the whole parse, and a repetition that succeeds without consuming input panics rather than loop forever.
pub fn many0<'a, T, F>(mut f: F) -> Parser<'a, Vec<T>>
where
    T: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
{
    Parser::new(move |mut state| {
        let mut items = Vec::new();

        let error = loop {
            match f().parse(state) {
                Ok((item, next_state)) => {
                    if state.offset == next_state.offset {
                        panic!("Infinite repetition");
                    }

                    items.push(item);
                    state = next_state;
                }
                Err(error) => break error,
            }
        };

        if error.is_uncaught(state) {
            return Err(error);
        }

        Ok((items, state))
    })
}

/// Like [`many0`] but the first item is mandatory: its failure is the whole parser's failure rather than an empty list.
pub fn many1<'a, T, F>(mut f: F) -> Parser<'a, Vec<T>>
where
    T: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
{
    Parser::new(move |state| {
        let offset = state.offset;
        let (item, mut state) = f().parse(state)?;

        if offset == state.offset {
            panic!("Infinite repetition");
        }

        let mut items = vec![item];

        let error = loop {
            match f().parse(state) {
                Ok((item, next_state)) => {
                    if state.offset == next_state.offset {
                        panic!("Infinite repetition");
                    }

                    items.push(item);
                    state = next_state;
                }
                Err(error) => break error,
            }
        };

        if error.is_uncaught(state) {
            return Err(error);
        }

        Ok((items, state))
    })
}

/// A bookmarked parse position, for building a [`crate::Span`] after the
/// fact from two positions captured at different points in a grammar —
/// [`spanned`] only covers what its one wrapped parser itself consumes, so a
/// node whose span should reach further (e.g. through a tail parsed by a
/// separate step) needs this instead.
#[derive(Debug, Clone)]
pub struct Mark {
    offset: usize,
    source: Rc<Source>,
}

impl Mark {
    /// The span between this mark and `end` (order-independent — whichever
    /// offset is smaller becomes the start).
    pub fn to(&self, end: &Mark) -> crate::Span {
        let (start, end) = match self.offset <= end.offset {
            true => (self.offset, end.offset),
            false => (end.offset, self.offset),
        };

        crate::Span::new(self.source.clone(), start, end)
    }
}

/// The current parse position as a value, for later use with [`Mark::to`].
/// Consumes no input.
pub fn mark<'a>() -> Parser<'a, Mark> {
    Parser::new(|state| {
        Ok((
            Mark {
                offset: state.offset,
                source: state.source.clone(),
            },
            state,
        ))
    })
}

/// Consumes one separator between `sep_by*` items: `Ok(Some(state))` advances
/// past it, `Ok(None)` means it wasn't there (a recoverable failure — the
/// caller ends the list), and an uncaught failure propagates. Panics on
/// zero-width progress, like the repetition combinators. Shared by
/// [`sep_by0`], [`sep_by0_trailing`], and [`sep_by1`], which otherwise had
/// this loop step verbatim three times over.
fn parse_separator<'a, S, G>(
    g: &mut G,
    state: ParserState<'a>,
) -> Result<Option<ParserState<'a>>, ParserError>
where
    S: 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    match g().parse(state) {
        Ok((_, next_state)) if state.offset == next_state.offset => {
            panic!("Infinite repetition")
        }
        Ok((_, next_state)) => Ok(Some(next_state)),
        Err(error) if error.is_uncaught(state) => Err(error),
        Err(_) => Ok(None),
    }
}

/// Zero or more `f` items separated by `g` (separators dropped). An empty list is fine, but once a separator is consumed the next item is mandatory — no trailing separator; use [`sep_by0_trailing`] where one is legal. As with [`many0`], the closures build a fresh single-use [`Parser`] per iteration, and an iteration that consumes no input panics as an infinite repetition.
pub fn sep_by0<'a, T, S, F, G>(mut f: F, mut g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    Parser::new(move |state| {
        let offset = state.offset;

        let (item, mut state) = match f().parse(state) {
            Ok(output) => output,
            Err(error) if error.is_uncaught(state) => return Err(error),
            Err(_) => return Ok((Vec::new(), state)),
        };

        if offset == state.offset {
            panic!("Infinite repetition");
        }

        let mut items = vec![item];

        while let Some(next_state) = parse_separator(&mut g, state)? {
            let offset = next_state.offset;
            let (item, next_state) = f().parse(next_state)?;

            if offset == next_state.offset {
                panic!("Infinite repetition");
            }

            items.push(item);
            state = next_state;
        }

        Ok((items, state))
    })
}

/// Like [`sep_by0`], but admits (and drops) one trailing separator: a
/// separator followed by a failed item parse ends the list with the separator
/// consumed instead of failing, so `{ a, b, }` parses like `{ a, b }`.
pub fn sep_by0_trailing<'a, T, S, F, G>(mut f: F, mut g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    Parser::new(move |state| {
        let offset = state.offset;

        let (item, mut state) = match f().parse(state) {
            Ok(output) => output,
            Err(error) if error.is_uncaught(state) => return Err(error),
            Err(_) => return Ok((Vec::new(), state)),
        };

        if offset == state.offset {
            panic!("Infinite repetition");
        }

        let mut items = vec![item];

        while let Some(next_state) = parse_separator(&mut g, state)? {
            let offset = next_state.offset;
            match f().parse(next_state) {
                Ok((item, next_state)) => {
                    if offset == next_state.offset {
                        panic!("Infinite repetition");
                    }

                    items.push(item);
                    state = next_state;
                }
                Err(error) if error.is_uncaught(next_state) => return Err(error),
                // The separator was trailing: keep it consumed, end the list.
                Err(_) => {
                    state = next_state;
                    break;
                }
            }
        }

        Ok((items, state))
    })
}

/// One or more `f` items separated by `g` (separators dropped). Commits after each successful separator: a separator not followed by an item is an error, so trailing separators are rejected — [`sep_by0_trailing`] is the variant that admits them.
pub fn sep_by1<'a, T, S, F, G>(mut f: F, mut g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    Parser::new(move |state| {
        let offset = state.offset;
        let (item, mut state) = f().parse(state)?;

        if offset == state.offset {
            panic!("Infinite repetition");
        }

        let mut items = vec![item];

        while let Some(next_state) = parse_separator(&mut g, state)? {
            let offset = next_state.offset;
            let (item, next_state) = f().parse(next_state)?;

            if offset == next_state.offset {
                panic!("Infinite repetition");
            }

            items.push(item);
            state = next_state;
        }

        Ok((items, state))
    })
}

/// Like [`sep_by1`], but admits (and drops) one trailing separator — the
/// nonempty sibling of [`sep_by0_trailing`]: `(a, b,)` parses like `(a, b)`,
/// while an empty list still fails on the first item.
pub fn sep_by1_trailing<'a, T, S, F, G>(mut f: F, mut g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    Parser::new(move |state| {
        let offset = state.offset;
        let (item, mut state) = f().parse(state)?;

        if offset == state.offset {
            panic!("Infinite repetition");
        }

        let mut items = vec![item];

        while let Some(next_state) = parse_separator(&mut g, state)? {
            let offset = next_state.offset;
            match f().parse(next_state) {
                Ok((item, next_state)) => {
                    if offset == next_state.offset {
                        panic!("Infinite repetition");
                    }

                    items.push(item);
                    state = next_state;
                }
                Err(error) if error.is_uncaught(next_state) => return Err(error),
                // The separator was trailing: keep it consumed, end the list.
                Err(_) => {
                    state = next_state;
                    break;
                }
            }
        }

        Ok((items, state))
    })
}
