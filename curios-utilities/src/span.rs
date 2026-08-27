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

    /// A source whose text arrived with a name but no file — the program handed to the compiler on standard input. Diagnostics render `label` in the `--> label:line:column` header exactly as they render a path, which is the whole point: text that never touched the disk still has line numbers worth naming, and [`inline`](Self::inline) drops them.
    ///
    /// The label is a display name and is never opened. Nothing reads a source back, and the only paths any consumer *records* are those of module files, which arrive through [`read`](Self::read) — so a label that no filesystem would answer for cannot be mistaken later for one that would.
    pub fn labelled(label: &str, text: impl Into<String>) -> Rc<Self> {
        Self::new(label, text)
    }

    /// Text standing in for the file at `path` — an editor's unsaved buffer, consulted where the file would have been read. Diagnostics name the path exactly as they would had it been read, because to everything downstream it was.
    pub fn held(path: impl Into<PathBuf>, text: impl Into<String>) -> Rc<Self> {
        Self::new(path, text)
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

/// A diagnostic as data: the message, and the span it is about when it is about one. Every stage's error renders through this — [`Report::render`] is the message followed by [`Span::render_snippet`], the one shape a Curios diagnostic has ever had — so a consumer that wants the location gets it as a span rather than by parsing the `-->` header back out of the text. The message is text rather than the stage's own error, deliberately: a report is what a stage *said*, and it survives the crate boundary that the error, with its terms and spellings, does not.
#[derive(Debug, Clone)]
pub struct Report {
    pub span: Option<Span>,
    pub message: String,
}

impl Report {
    /// A report about `span`.
    pub fn at(span: Span, message: impl Into<String>) -> Self {
        Self {
            span: Some(span),
            message: message.into(),
        }
    }

    /// A report about nothing in particular: a manifest, a store, a kernel refusal that names an item rather than a position.
    pub fn unlocated(message: impl Into<String>) -> Self {
        Self {
            span: None,
            message: message.into(),
        }
    }

    /// The report as a reader sees it: the message, then a blank line and the snippet when there is a span.
    pub fn render(&self) -> String {
        match &self.span {
            Some(span) => format!("{}\n\n{}", self.message, span.render_snippet()),
            None => self.message.clone(),
        }
    }

    /// Several reports rendered in order, a blank line between each — how a goal batch has always read.
    pub fn render_all(reports: &[Report]) -> String {
        reports
            .iter()
            .map(Report::render)
            .collect::<Vec<_>>()
            .join("\n\n")
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
mod tests;
