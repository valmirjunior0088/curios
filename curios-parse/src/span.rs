use {
    super::Parser,
    curios_utilities::{Source, Span},
    std::rc::Rc,
};

/// Pairs the parser's output with the [`Span`] covering exactly the bytes it consumed — how surface parsers attach source locations, so wrap the whole construct rather than its pieces.
pub fn spanned<'a, T>(parser: Parser<'a, T>) -> Parser<'a, (Span, T)>
where
    T: 'a,
{
    Parser::new(move |state| {
        let start = state.offset;
        let (item, state) = parser.parse(state)?;

        Ok((
            (Span::new(state.source.clone(), start, state.offset), item),
            state,
        ))
    })
}

/// A bookmarked parse position, for building a [`Span`] after the fact from two positions captured at different points in a grammar — [`spanned`] only covers what its one wrapped parser itself consumes, so a node whose span should reach further (e.g. through a tail parsed by a separate step) needs this instead.
#[derive(Debug, Clone)]
pub struct Mark {
    offset: usize,
    source: Rc<Source>,
}

impl Mark {
    /// The span between this mark and `end` (order-independent — whichever offset is smaller becomes the start).
    pub fn to(&self, end: &Mark) -> Span {
        let (start, end) = match self.offset <= end.offset {
            true => (self.offset, end.offset),
            false => (end.offset, self.offset),
        };

        Span::new(self.source.clone(), start, end)
    }
}

/// The current parse position as a value, for later use with [`Mark::to`]. Consumes no input.
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
