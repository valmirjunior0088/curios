use {
    super::{Built, Parser, ParserError, ParserState, Ready},
    curios_utilities::Span,
};

/// What a repetition takes: something that hands it a parser for each iteration, a [`Parser`] being single-use. A function that builds one, called once an iteration, or a parser that is `Copy` — a nonterminal, which is a function of the input and so is one. `M` says which, and is never written.
pub trait Repeated<'a, T, M> {
    type Parser: Parser<'a, T>;

    /// The parser of the next iteration.
    fn next(&mut self) -> Self::Parser;
}

impl<'a, T, P, F> Repeated<'a, T, Built> for F
where
    P: Parser<'a, T>,
    F: FnMut() -> P,
{
    type Parser = P;

    fn next(&mut self) -> P {
        self()
    }
}

impl<'a, T, P> Repeated<'a, T, Ready> for P
where
    P: Parser<'a, T> + Copy,
{
    type Parser = P;

    fn next(&mut self) -> P {
        *self
    }
}

/// One item of a [`recover`]ed repetition: what parsed, with the text its parse consumed, or what broke and the text the loop skipped to resume.
#[derive(Debug)]
pub enum Recovered<T> {
    Item { item: T, span: Span },
    Broken { error: ParserError, span: Span },
}

/// [`many0`] that resynchronizes past a committed failure instead of aborting on it. The failure is kept as [`Recovered::Broken`] with the span from where the item began to where parsing resumed, and `anchor` says where that is: given the source text and the offset the failure was reported at, the offset of the next place an item could begin, or `None` for nowhere, which ends the loop at the end of the input. An uncommitted failure ends the loop exactly as [`many0`]'s does, so a caller's tail grammar still gets its turn and the item-or-tail ambiguity gains no heuristic. A candidate anchor is never trusted: the item parser runs at it, and a failure there that does not commit only ends the loop. Progress is forced past the item's first character, so a failure reported at the item's own start cannot resume at itself.
pub fn recover<'a, T, M>(
    mut each: impl Repeated<'a, T, M>,
    anchor: impl Fn(&str, usize) -> Option<usize>,
) -> impl Parser<'a, Vec<Recovered<T>>> {
    move |mut state: ParserState<'a>| {
        let mut items = Vec::new();

        loop {
            let start = state.offset;
            match each.next().parse(state) {
                Ok((item, next_state)) => {
                    if start == next_state.offset {
                        panic!("Infinite repetition");
                    }

                    items.push(Recovered::Item {
                        item,
                        span: Span::new(next_state.source.clone(), start, next_state.offset),
                    });
                    state = next_state;
                }
                Err(error) if error.is_committed() => {
                    let text = &state.source.text;
                    let past_start = start + text[start..].chars().next().map_or(0, char::len_utf8);
                    let from = error.offset.max(past_start);
                    let resume = anchor(text, from).unwrap_or(text.len());

                    items.push(Recovered::Broken {
                        error,
                        span: Span::new(state.source.clone(), start, resume),
                    });
                    state = state.jump_to(resume);
                }
                Err(_) => return Ok((items, state)),
            }
        }
    }
}

/// The one loop behind [`many0`]/[`many1`]: accumulate until the first recoverable failure, panicking on zero-width progress; `require_first` decides whether an empty result reports that failure instead of an empty list.
fn many_core<'a, T, M>(
    mut each: impl Repeated<'a, T, M>,
    require_first: bool,
) -> impl Parser<'a, Vec<T>> {
    move |mut state: ParserState<'a>| {
        let mut items = Vec::new();

        let error = loop {
            match each.next().parse(state) {
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

        if (require_first && items.is_empty()) || error.is_committed() {
            return Err(error);
        }

        Ok((items, state))
    }
}

/// Zero or more repetitions of the parser `each` hands over. Takes what hands it a parser for each iteration ([`Repeated`]) rather than a parser because [`Parser`] is single-use — every iteration needs a fresh instance. Stops at the first recoverable failure; a [`commit`](crate::commit)ted failure aborts the whole parse, and a repetition that succeeds without consuming input panics rather than loop forever.
pub fn many0<'a, T, M>(each: impl Repeated<'a, T, M>) -> impl Parser<'a, Vec<T>> {
    many_core(each, false)
}

/// Like [`many0`] but the first item is mandatory: its failure is the whole parser's failure rather than an empty list.
pub fn many1<'a, T, M>(each: impl Repeated<'a, T, M>) -> impl Parser<'a, Vec<T>> {
    many_core(each, true)
}

/// Consumes one separator between `sep_by*` items: `Ok(Some(state))` advances past it, `Ok(None)` means it wasn't there (a recoverable failure — the caller ends the list), and a committed failure propagates. Panics on zero-width progress, like the repetition combinators. The separator step of [`sep_by_core`]'s loop.
fn parse_separator<'a, S>(
    separator: impl Parser<'a, S>,
    state: ParserState<'a>,
) -> Result<Option<ParserState<'a>>, ParserError> {
    match separator.parse(state) {
        Ok((_, next_state)) if state.offset == next_state.offset => {
            panic!("Infinite repetition")
        }
        Ok((_, next_state)) => Ok(Some(next_state)),
        Err(error) if error.is_committed() => Err(error),
        Err(_) => Ok(None),
    }
}

/// The one loop behind the four `sep_by*` combinators. `require_first` decides whether a failed first item is the whole parser's failure or an empty list; `trailing` whether a separator followed by a recoverable item failure ends the list with the separator consumed instead of failing. Panics on zero-width progress, like [`many_core`].
fn sep_by_core<'a, T, S, M, N>(
    mut each: impl Repeated<'a, T, M>,
    mut separator: impl Repeated<'a, S, N>,
    require_first: bool,
    trailing: bool,
) -> impl Parser<'a, Vec<T>> {
    move |state: ParserState<'a>| {
        let offset = state.offset;

        let (item, mut state) = match each.next().parse(state) {
            Ok(output) => output,
            Err(error) if require_first || error.is_committed() => return Err(error),
            Err(_) => return Ok((Vec::new(), state)),
        };

        if offset == state.offset {
            panic!("Infinite repetition");
        }

        let mut items = vec![item];

        while let Some(next_state) = parse_separator(separator.next(), state)? {
            let offset = next_state.offset;
            match each.next().parse(next_state) {
                Ok((item, next_state)) => {
                    if offset == next_state.offset {
                        panic!("Infinite repetition");
                    }

                    items.push(item);
                    state = next_state;
                }
                Err(error) if !trailing || error.is_committed() => return Err(error),
                // The separator was trailing: keep it consumed, end the list.
                Err(_) => {
                    state = next_state;
                    break;
                }
            }
        }

        Ok((items, state))
    }
}

/// Zero or more `each` items separated by `separator` (separators dropped). An empty list is fine, but once a separator is consumed the next item is mandatory — no trailing separator; use [`sep_by0_trailing`] where one is legal. As with [`many0`], `each` and `separator` hand over a fresh single-use [`Parser`] per iteration, and an iteration that consumes no input panics as an infinite repetition.
pub fn sep_by0<'a, T, S, M, N>(
    each: impl Repeated<'a, T, M>,
    separator: impl Repeated<'a, S, N>,
) -> impl Parser<'a, Vec<T>> {
    sep_by_core(each, separator, false, false)
}

/// Like [`sep_by0`], but admits (and drops) one trailing separator: a separator followed by a failed item parse ends the list with the separator consumed instead of failing, so `{ a, b, }` parses like `{ a, b }`.
pub fn sep_by0_trailing<'a, T, S, M, N>(
    each: impl Repeated<'a, T, M>,
    separator: impl Repeated<'a, S, N>,
) -> impl Parser<'a, Vec<T>> {
    sep_by_core(each, separator, false, true)
}

/// One or more `each` items separated by `separator` (separators dropped). Once a separator is consumed the next item is mandatory, as in [`sep_by0`], so trailing separators are rejected — [`sep_by1_trailing`] is the variant that admits them.
pub fn sep_by1<'a, T, S, M, N>(
    each: impl Repeated<'a, T, M>,
    separator: impl Repeated<'a, S, N>,
) -> impl Parser<'a, Vec<T>> {
    sep_by_core(each, separator, true, false)
}

/// Like [`sep_by1`], but admits (and drops) one trailing separator — the nonempty sibling of [`sep_by0_trailing`]: `(a, b,)` parses like `(a, b)`, while an empty list still fails on the first item.
pub fn sep_by1_trailing<'a, T, S, M, N>(
    each: impl Repeated<'a, T, M>,
    separator: impl Repeated<'a, S, N>,
) -> impl Parser<'a, Vec<T>> {
    sep_by_core(each, separator, true, true)
}
