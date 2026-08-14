use std::{
    fs,
    hash::{Hash, Hasher},
    io,
    path::PathBuf,
    rc::Rc,
};

/// One unit of input text — a file with its path, or pathless inline text — always handed out as `Rc<Source>` so every [`Span`] into it shares the one allocation instead of copying or re-reading the text.
#[derive(Debug)]
#[curios_archive::archived]
pub struct Source {
    #[archived_with(curios_archive::Map<curios_archive::AsString>)]
    pub path: Option<PathBuf>,
    pub text: String,
}

impl Source {
    fn new(path: impl Into<PathBuf>, text: impl Into<String>) -> Rc<Self> {
        Rc::new(Self {
            path: Some(path.into()),
            text: text.into(),
        })
    }

    /// A source with no backing file — embedded or test input handed to a parser as a bare string. Its diagnostics render the snippet without a file-location header.
    pub fn inline(text: impl Into<String>) -> Rc<Self> {
        Rc::new(Self {
            path: None,
            text: text.into(),
        })
    }

    /// Loads the file at `path` as a source, keeping the path so diagnostics can print a `--> path:line` header.
    pub fn read(path: impl Into<PathBuf>) -> io::Result<Rc<Self>> {
        let path = path.into();
        let text = fs::read_to_string(&path)?;

        Ok(Self::new(path, text))
    }
}

/// A half-open byte range `[start, end)` into a shared [`Source`] — how every pipeline stage points a diagnostic back at the text that caused it. Equality and hashing identify the source by `Rc` pointer rather than content, so spans from separately loaded sources never alias even when their texts match, and hashing never walks the text.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Span {
    pub source: Rc<Source>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Public because the thing that mints spans from byte offsets is the parser, and that now lives outside this crate as `curios-parse`. It was `pub(crate)` only while the two shared one.
    pub fn new(source: Rc<Source>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    /// The 1-based line and column of the span's start — the coordinate a diagnostic names a location by. Columns count Unicode scalar values on the line, matching the caret position in [`Span::render_snippet`].
    pub fn line_column(&self) -> (usize, usize) {
        let source = &self.source.text;

        let line_start = source[..self.start]
            .rfind('\n')
            .map(|index| 1 + index)
            .unwrap_or(0);

        let line = 1 + source[..line_start]
            .bytes()
            .filter(|&byte| byte == b'\n')
            .count();
        let column = 1 + source[line_start..self.start].chars().count();

        (line, column)
    }

    /// Renders the span as a compiler diagnostic: a `--> path:line:column` header when the source is a file (omitted for inline sources), the 1-based-numbered source line containing the span's start, and a caret underline positioned and sized by Unicode scalar count, matching [`Span::line_column`]. The underline is clamped to that first line and is at least one `^` wide, so multi-line and empty spans still point somewhere visible.
    pub fn render_snippet(&self) -> String {
        let source = &self.source.text;
        let start = self.start;
        let end = self.end;

        let line_start = source[..start]
            .rfind('\n')
            .map(|index| 1 + index)
            .unwrap_or(0);

        let line_end = source[start..]
            .find('\n')
            .map(|index| start + index)
            .unwrap_or(source.len());

        let (number, column) = self.line_column();

        let width = source[start..end.min(line_end)].chars().count();
        let caret = format!(
            "{}{}",
            " ".repeat(source[line_start..start].chars().count()),
            "^".repeat(width.max(1))
        );

        let snippet = format!(
            "{number:>5} | {line}\n{padding:>5} | {caret}",
            number = number,
            line = &source[line_start..line_end],
            padding = "",
        );

        match &self.source.path {
            Some(path) => format!("   --> {}:{number}:{column}\n{snippet}", path.display()),
            None => snippet,
        }
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.source, &other.source)
            && self.start == other.start
            && self.end == other.end
    }
}

impl Eq for Span {}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.source) as usize).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_column_is_one_based_and_counts_scalars() {
        let source = Source::inline("first\nsécond line\n");

        // `l` of "line": line 2, after "sécond " — seven scalars in, so column 8 despite the two-byte `é`.
        let offset = source.text.find("line").unwrap();
        let span = Span::new(Rc::clone(&source), offset, offset + 4);
        assert_eq!(span.line_column(), (2, 8));

        // The very first byte.
        let span = Span::new(source, 0, 1);
        assert_eq!(span.line_column(), (1, 1));
    }

    #[test]
    fn caret_aligns_by_scalar_count_on_non_ascii_lines() {
        let source = Source::inline("sécond line\n");

        // "line" starts at byte 8 but after seven scalars — seven spaces of padding, four carets.
        let offset = source.text.find("line").unwrap();
        let span = Span::new(Rc::clone(&source), offset, offset + 4);
        assert_eq!(
            span.render_snippet(),
            "    1 | sécond line\n      |        ^^^^"
        );

        // A span covering the two-byte `é` is one scalar wide — one caret, one space of padding.
        let offset = source.text.find('é').unwrap();
        let span = Span::new(source, offset, offset + 'é'.len_utf8());
        assert_eq!(span.render_snippet(), "    1 | sécond line\n      |  ^");
    }
}
