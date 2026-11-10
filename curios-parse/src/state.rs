use {
    curios_common::{Source, Span},
    std::sync::Arc,
};

/// Where a parse stands in its input: what a parser is a function of. Opaque to a grammar, which hands it on.
#[derive(Debug, Clone, Copy)]
pub struct ParserState<'a> {
    offset: usize,
    string: &'a str,
    source: &'a Arc<Source>,
}

impl<'a> ParserState<'a> {
    pub(crate) fn new(source: &'a Arc<Source>) -> Self {
        Self {
            offset: 0,
            string: &source.text,
            source,
        }
    }

    /// Where the state stands, a byte offset into its source: what a memo is keyed by and a failure or a mark records.
    pub(crate) fn offset(&self) -> usize {
        self.offset
    }

    /// The source the state reads.
    pub(crate) fn source(&self) -> &'a Arc<Source> {
        self.source
    }

    /// The input from here to its end.
    pub(crate) fn rest(&self) -> &'a str {
        self.string
    }

    /// The character just before here, or `None` at the start of the input.
    pub(crate) fn char_before(&self) -> Option<char> {
        self.source.text[..self.offset].chars().next_back()
    }

    /// The state past `literal`, where the input here starts with it.
    pub(crate) fn after(self, literal: &str) -> Option<Self> {
        self.string
            .starts_with(literal)
            .then(|| self.jump_to(self.offset + literal.len()))
    }

    /// Whether the parse stands elsewhere than at `earlier`: what a repetition asks of a step to know it consumed input.
    pub(crate) fn moved_from(&self, earlier: &Self) -> bool {
        self.offset != earlier.offset
    }

    /// The span from here to `end`.
    pub(crate) fn span_to(&self, end: &Self) -> Span {
        Span::new(self.source.clone(), self.offset, end.offset)
    }

    /// Where a repetition resumes past a committed failure at `failed`: the first place `anchor` finds in the text from `failed` on, or from past the first character here where the failure sat no further, with the span from here to it.
    pub(crate) fn resumed(
        self,
        failed: usize,
        anchor: impl Fn(&str, usize) -> Option<usize>,
    ) -> (Span, Self) {
        let text = &self.source.text;
        let past_start = self.offset + text[self.offset..].chars().next().map_or(0, char::len_utf8);
        let resume = anchor(text, failed.max(past_start)).unwrap_or(text.len());

        (
            Span::new(self.source.clone(), self.offset, resume),
            self.jump_to(resume),
        )
    }

    pub(crate) fn take_n(self, n: usize) -> Option<(&'a str, Self)> {
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

    pub(crate) fn take_while<F>(self, mut predicate: F) -> (&'a str, Self)
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

    pub(crate) fn is_finished(&self) -> bool {
        self.string.is_empty()
    }

    /// Rebuilds the state at an absolute byte `offset` into the same source. Used by [`memoize`](crate::memoize) to resume from a cached parse without re-walking the input, and by [`take_exact`](crate::take_exact) to step over a matched literal; all parser offsets are byte offsets, so slicing `source.text` is exact.
    pub(crate) fn jump_to(self, offset: usize) -> Self {
        Self {
            offset,
            string: &self.source.text[offset..],
            source: self.source,
        }
    }
}
