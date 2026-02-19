#[derive(Debug, Clone, Copy)]
struct ParserState<'a> {
    offset: usize,
    string: &'a str,
}

impl<'a> ParserState<'a> {
    fn new(string: &'a str) -> Self {
        Self { offset: 0, string }
    }

    fn take_exact(self, width: usize) -> Option<(&'a str, Self)> {
        Some((
            self.string.get(..width)?,
            Self {
                offset: self.offset + width,
                string: self.string.get(width..)?,
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
            },
        )
    }

    fn is_finished(&self) -> bool {
        self.string.is_empty()
    }
}

#[derive(Debug)]
pub struct ParserError {
    fatal: bool,
    offset: usize,
    message: String,
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
        }
    }

    fn catch(self) -> Self {
        Self {
            fatal: false,
            ..self
        }
    }

    fn is_uncaught(&self, state: ParserState) -> bool {
        self.fatal && self.offset != state.offset
    }

    pub fn format(&self, string: &str) -> String {
        let start = string[..self.offset]
            .rfind('\n')
            .map(|index| 1 + index)
            .unwrap_or(0);

        let end = string[self.offset..]
            .find('\n')
            .map(|index| self.offset + index)
            .unwrap_or(string.len());

        let number = 1 + string[..start]
            .bytes()
            .filter(|&byte| byte == b'\n')
            .count();

        format!(
            "{message}\n\n{number:>5} | {line}\n{padding:>5} | {caret}",
            message = self.message,
            number = number,
            line = &string[start..end],
            padding = "",
            caret = format!("{}^", " ".repeat(self.offset - start)),
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

pub fn parse<'a, A>(parser: Parser<'a, A>, string: &'a str) -> Result<A, ParserError>
where
    A: 'a,
{
    parser.parse(ParserState::new(string)).map(|(item, _)| item)
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

pub fn catch<'a, T>(parser: Parser<'a, T>) -> Parser<'a, T>
where
    T: 'a,
{
    Parser::new(move |state| parser.parse(state).map_err(|error| error.catch()))
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
