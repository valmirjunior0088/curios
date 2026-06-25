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

    pub fn map_err<M>(self, message: M) -> Parser<'a, A>
    where
        M: Into<String> + 'a,
    {
        Parser::new(move |state| {
            self.parse(state)
                .map_err(|error| error.with_message(message))
        })
    }

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
/// Sound as straight packrat because the wrapped parsers ([`parse_term`],
/// [`parse_atomic_term`]) are pure functions of the offset — parsing carries no
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

pub fn run_parser<'a, A>(parser: Parser<'a, A>, source: &'a Rc<Source>) -> Result<A, ParserError>
where
    A: 'a,
{
    MEMO.with(|memo| memo.borrow_mut().clear());

    parser.parse(ParserState::new(source)).map(|(item, _)| item)
}

pub fn pure<'a, A>(a: A) -> Parser<'a, A>
where
    A: 'a,
{
    Parser::new(move |state| Ok((a, state)))
}

pub fn fail<'a, A, S>(message: S) -> Parser<'a, A>
where
    A: 'a,
    S: Into<String> + 'a,
{
    Parser::new(move |state| Err(ParserError::new(state, message)))
}

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

pub fn take_n<'a>(expected: usize) -> Parser<'a, &'a str> {
    Parser::new(move |state| match state.take_n(expected) {
        Some((obtained, state)) => Ok((obtained, state)),
        None => Err(ParserError::new(
            state,
            format!("Expected {expected} character(s), obtained 'end-of-file'"),
        )),
    })
}

pub fn take_while<'a, F>(f: F) -> Parser<'a, &'a str>
where
    F: FnMut(char) -> bool + 'a,
{
    Parser::new(move |state| Ok(state.take_while(f)))
}

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

pub fn catch<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(|error| error.catch()))
}

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

pub fn lazy<'a, T, F>(f: F) -> Parser<'a, T>
where
    T: 'a,
    F: FnOnce() -> Parser<'a, T> + 'a,
{
    Parser::new(move |state| f().parse(state))
}

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

        loop {
            let next_state = match g().parse(state) {
                Ok((_, next_state)) if state.offset == next_state.offset => {
                    panic!("Infinite repetition")
                }
                Ok((_, next_state)) => next_state,
                Err(error) if error.is_uncaught(state) => return Err(error),
                Err(_) => break,
            };

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

        loop {
            let next_state = match g().parse(state) {
                Ok((_, next_state)) if state.offset == next_state.offset => {
                    panic!("Infinite repetition")
                }
                Ok((_, next_state)) => next_state,
                Err(error) if error.is_uncaught(state) => return Err(error),
                Err(_) => break,
            };

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
