use super::{Parser, ParserError, ParserState};

/// The one loop behind [`many0`]/[`many1`]: accumulate until the first recoverable failure, panicking on zero-width progress; `require_first` decides whether an empty result reports that failure instead of an empty list.
fn many_core<'a, T, F>(mut f: F, require_first: bool) -> Parser<'a, Vec<T>>
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

        if (require_first && items.is_empty()) || error.is_uncaught(state) {
            return Err(error);
        }

        Ok((items, state))
    })
}

/// Zero or more repetitions of the parser `f` builds. Takes a parser-building closure rather than a parser because [`Parser`] is single-use — every iteration needs a fresh instance. Stops at the first recoverable failure; an uncaught (fatal, input-consuming) failure aborts the whole parse, and a repetition that succeeds without consuming input panics rather than loop forever.
pub fn many0<'a, T, F>(f: F) -> Parser<'a, Vec<T>>
where
    T: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
{
    many_core(f, false)
}

/// Like [`many0`] but the first item is mandatory: its failure is the whole parser's failure rather than an empty list.
pub fn many1<'a, T, F>(f: F) -> Parser<'a, Vec<T>>
where
    T: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
{
    many_core(f, true)
}

/// Consumes one separator between `sep_by*` items: `Ok(Some(state))` advances past it, `Ok(None)` means it wasn't there (a recoverable failure — the caller ends the list), and an uncaught failure propagates. Panics on zero-width progress, like the repetition combinators. The separator step of [`sep_by_core`]'s loop.
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

/// The one loop behind the four `sep_by*` combinators. `require_first` decides whether a failed first item is the whole parser's failure or an empty list; `trailing` whether a separator followed by a recoverable item failure ends the list with the separator consumed instead of failing. Panics on zero-width progress, like [`many_core`].
fn sep_by_core<'a, T, S, F, G>(
    mut f: F,
    mut g: G,
    require_first: bool,
    trailing: bool,
) -> Parser<'a, Vec<T>>
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
            Err(error) if require_first || error.is_uncaught(state) => return Err(error),
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
                Err(error) if !trailing || error.is_uncaught(next_state) => return Err(error),
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

/// Zero or more `f` items separated by `g` (separators dropped). An empty list is fine, but once a separator is consumed the next item is mandatory — no trailing separator; use [`sep_by0_trailing`] where one is legal. As with [`many0`], the closures build a fresh single-use [`Parser`] per iteration, and an iteration that consumes no input panics as an infinite repetition.
pub fn sep_by0<'a, T, S, F, G>(f: F, g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    sep_by_core(f, g, false, false)
}

/// Like [`sep_by0`], but admits (and drops) one trailing separator: a separator followed by a failed item parse ends the list with the separator consumed instead of failing, so `{ a, b, }` parses like `{ a, b }`.
pub fn sep_by0_trailing<'a, T, S, F, G>(f: F, g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    sep_by_core(f, g, false, true)
}

/// One or more `f` items separated by `g` (separators dropped). Commits after each successful separator: a separator not followed by an item is an error, so trailing separators are rejected — [`sep_by0_trailing`] is the variant that admits them.
pub fn sep_by1<'a, T, S, F, G>(f: F, g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    sep_by_core(f, g, true, false)
}

/// Like [`sep_by1`], but admits (and drops) one trailing separator — the nonempty sibling of [`sep_by0_trailing`]: `(a, b,)` parses like `(a, b)`, while an empty list still fails on the first item.
pub fn sep_by1_trailing<'a, T, S, F, G>(f: F, g: G) -> Parser<'a, Vec<T>>
where
    T: 'a,
    S: 'a,
    F: FnMut() -> Parser<'a, T> + 'a,
    G: FnMut() -> Parser<'a, S> + 'a,
{
    sep_by_core(f, g, true, true)
}
