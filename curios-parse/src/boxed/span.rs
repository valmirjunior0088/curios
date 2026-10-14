use {
    super::Parser,
    crate::{IntoParser, Mark, Parser as _},
    curios_utilities::Span,
};

/// Pairs the parser's output with the [`Span`] covering exactly the bytes it consumed — how surface parsers attach source locations, so wrap the whole construct rather than its pieces.
pub fn spanned<'a, T, M>(parser: impl IntoParser<'a, T, M> + 'a) -> Parser<'a, (Span, T)>
where
    T: 'a,
    M: 'a,
{
    Parser::new(move |state| {
        let start = state.offset;
        let (item, state) = parser.into_parser().parse(state)?;

        Ok((
            (Span::new(state.source.clone(), start, state.offset), item),
            state,
        ))
    })
}

/// The current parse position as a value, for later use with [`Mark::to`]. Consumes no input.
pub fn mark<'a>() -> Parser<'a, Mark> {
    Parser::new(|state| Ok((Mark::at(state), state)))
}
