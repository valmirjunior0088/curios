use super::{Mark, Parser, ParserError};

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

/// [`fail`] about the text from `start` to the current offset: the failure still sits at the current offset, so it commits or backtracks exactly as `fail` would, and only the reported span reaches back — for a word read and refused, so the caret underlines the word rather than standing after it.
pub fn fail_from<'a, A, S>(start: &Mark, message: S) -> Parser<'a, A>
where
    A: 'a,
    S: Into<String> + 'a,
{
    let start = start.offset();
    Parser::new(move |state| Err(ParserError::new(state, message).from(start)))
}

/// Consumes exactly the literal `expected`, yielding nothing. On mismatch the error sits at the *pre-consumption* offset, so failing here never commits — a keyword or punctuation probe is always safe as the first token of an [`Parser::or`] alternative. The mismatch message shows what actually follows, counted in characters rather than the literal's bytes, so non-ASCII input never truncates mid-character or misreports as end-of-file — and cut at the first whitespace, so a token shorter than the literal is quoted alone rather than with its neighbour: `=` where `=>` was expected read as `'= '`.
pub fn take_exact<'a>(expected: &'static str) -> Parser<'a, ()> {
    Parser::new(move |state| match state.string.starts_with(expected) {
        true => Ok(((), state.jump_to(state.offset + expected.len()))),
        false => {
            let obtained: String = state
                .string
                .chars()
                .take(expected.chars().count())
                .take_while(|char| !char.is_whitespace())
                .collect();

            Err(ParserError::new(
                state,
                match obtained.is_empty() {
                    true => format!("Expected '{expected}', obtained 'end-of-file'"),
                    false => format!("Expected '{expected}', obtained '{obtained}'"),
                },
            ))
        }
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

/// Succeeds (consuming nothing) only when the input is exhausted. Top-level grammars end with this so trailing garbage is a parse error — [`run_parser`](crate::run_parser) itself does not require the input to be fully consumed.
pub fn take_eof<'a>() -> Parser<'a, ()> {
    Parser::new(|state| match state.is_finished() {
        true => Ok(((), state)),
        false => Err(ParserError::new(state, "Expected 'end-of-file'")),
    })
}

/// Succeeds (consuming nothing) only when the byte immediately before the current offset is whitespace, or we are at the start of input. The infix operator parser uses it to require a space on the *left* of an operator — the right space is required by consuming whitespace after the symbol — so `a - 42` is subtraction while the glued `-42` stays a literal.
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

/// Succeeds (consuming nothing) only when the remaining input does *not* start with `unexpected`. A negative look-ahead — e.g. to read a postfix `!` only when it is not the start of the `!=` operator.
pub fn not_ahead<'a>(unexpected: &'static str) -> Parser<'a, ()> {
    Parser::new(move |state| match state.string.starts_with(unexpected) {
        true => Err(ParserError::new(
            state,
            format!("unexpected '{unexpected}'"),
        )),
        false => Ok(((), state)),
    })
}
