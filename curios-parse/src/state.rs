use {curios_utilities::Source, std::rc::Rc};

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParserState<'a> {
    pub(crate) offset: usize,
    pub(crate) string: &'a str,
    pub(crate) source: &'a Rc<Source>,
}

impl<'a> ParserState<'a> {
    pub(crate) fn new(source: &'a Rc<Source>) -> Self {
        Self {
            offset: 0,
            string: &source.text,
            source,
        }
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
