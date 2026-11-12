use {
    crate::{Kind, Style},
    curios_unicode::{
        EastAsianWidth, GraphemeBreak, east_asian_width, grapheme_break, graphemes,
        is_emoji_presentation,
    },
    std::{
        fmt, fs,
        hash::{Hash, Hasher},
        io, iter,
        path::{Path, PathBuf},
        sync::{Arc, OnceLock},
    },
};

/// The widest a line of Curios is written within, in scalar values, and the width the formatter writes a file within where nothing states a narrower one: the formatter breaks no line past the width it is given, and a report quotes a longer one as a window this wide around its marks, so a line the formatter wrote is quoted whole.
pub const LINE_WIDTH: usize = 100;

/// The columns `cluster` takes on a terminal, which is what a caret under it is padded and drawn by: two where it holds a scalar East Asian Width calls wide or fullwidth, one that presents as an emoji, or the selector that asks for emoji presentation; none where it opens with a mark, a joiner or a control, which a terminal draws over its neighbour or not at all; one otherwise. A tab is the caller's, since it pads as a tab.
fn columns(cluster: &str) -> usize {
    let wide = cluster.chars().any(|scalar| {
        matches!(
            east_asian_width(scalar),
            EastAsianWidth::W | EastAsianWidth::F
        ) || is_emoji_presentation(scalar)
            || scalar == '\u{FE0F}'
    });
    let opening = cluster.chars().next().map(grapheme_break);

    match (wide, opening) {
        (true, _) => 2,
        (false, Some(GraphemeBreak::Extend | GraphemeBreak::Zwj | GraphemeBreak::Control)) => 0,
        _ => 1,
    }
}

/// One unit of input text — a file with its path, or pathless inline text — always handed out as `Arc<Source>` so every [`Span`] into it shares the one allocation instead of copying or re-reading the text, and so a span can cross a thread. Reference-counted rather than interned: a language server loads a new text on every edit, and a count is what frees the old one once nothing points into it.
#[derive(Debug)]
#[curios_archive::archived]
pub struct Source {
    #[archived_with(curios_archive::Map<curios_archive::AsString>)]
    path: Option<PathBuf>,
    text: String,
    /// The byte offset each line begins at, found the first time a place in the text is named or a report quotes it, so that naming a place is a search rather than a count of every line before it. Never stored, being read off `text`.
    #[archived_with(curios_archive::Skip)]
    lines: OnceLock<Box<[usize]>>,
}

impl Source {
    /// The source `text` is, read from `path` where it was read from a file: what each constructor below hands out shared.
    pub fn of(path: Option<PathBuf>, text: impl Into<String>) -> Self {
        Self {
            path,
            text: text.into(),
            lines: OnceLock::new(),
        }
    }

    fn new(path: impl Into<PathBuf>, text: impl Into<String>) -> Arc<Self> {
        Arc::new(Self::of(Some(path.into()), text))
    }

    /// A source with no backing file — embedded or test input handed to a parser as a bare string. Its diagnostics close on the line and the column alone.
    pub fn inline(text: impl Into<String>) -> Arc<Self> {
        Arc::new(Self::of(None, text))
    }

    /// A source whose text arrived with a name but no file — the program handed to the compiler on standard input. Diagnostics name `label` in the place a report closes on, `label:line:column`, exactly as they name a path, which is the whole point: text that never touched the disk still has line numbers worth naming, and [`inline`](Self::inline) drops them.
    ///
    /// The label is a display name and is never opened. Nothing reads a source back, and the only paths any consumer *records* are those of module files, which arrive through [`read`](Self::read) — so a label that no filesystem would answer for cannot be mistaken later for one that would.
    pub fn labelled(label: &str, text: impl Into<String>) -> Arc<Self> {
        Self::new(label, text)
    }

    /// Text standing in for the file at `path` — an editor's unsaved buffer, consulted where the file would have been read. Diagnostics name the path exactly as they would had it been read, because to everything downstream it was.
    pub fn held(path: impl Into<PathBuf>, text: impl Into<String>) -> Arc<Self> {
        Self::new(path, text)
    }

    /// Loads the file at `path` as a source, keeping the path so a diagnostic can close on the place, `path:line:column`.
    pub fn read(path: impl Into<PathBuf>) -> io::Result<Arc<Self>> {
        let path = path.into();
        let text = fs::read_to_string(&path)?;

        Ok(Self::new(path, text))
    }

    /// The file the text was read from or stands in for, or the label it arrived under; `None` for inline text.
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    /// The text, which every [`Span`] into this source indexes by byte offset.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// The byte offset of the 1-based `line` and `column`, the coordinate [`Span::line_column`] names a place by, so a place a diagnostic prints is one that can be asked about: columns count Unicode scalar values, and the column past a line's last is its end. `None` for a place the text does not hold.
    pub fn offset_of(&self, line: usize, column: usize) -> Option<usize> {
        let start = *self.line_starts().get(line.checked_sub(1)?)?;
        let written = self.text[start..].split('\n').next().unwrap_or_default();
        let within = column.checked_sub(1)?;

        match written.char_indices().nth(within) {
            Some((offset, _)) => Some(start + offset),
            None => (within == written.chars().count()).then_some(start + written.len()),
        }
    }

    /// The line `offset` stands on, counted from zero, and the byte offset that line begins at.
    pub fn line_of(&self, offset: usize) -> (usize, usize) {
        let starts = self.line_starts();
        let line = starts.partition_point(|start| *start <= offset) - 1;

        (line, starts[line])
    }

    /// The byte offset line `line`, counted from zero, ends at: its line break's, or the text's end for the last.
    pub fn line_end(&self, line: usize) -> usize {
        self.line_starts()
            .get(line + 1)
            .map_or(self.text.len(), |next| next - 1)
    }

    fn line_starts(&self) -> &[usize] {
        self.lines.get_or_init(|| {
            iter::once(0)
                .chain(self.text.match_indices('\n').map(|(at, _)| at + 1))
                .collect()
        })
    }
}

/// A half-open byte range `[start, end)` into a shared [`Source`] — how every pipeline stage points a diagnostic back at the text that caused it. Equality and hashing identify the source by pointer rather than content, so spans from separately loaded sources never alias even when their texts match, and hashing never walks the text.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct Span {
    pub source: Arc<Source>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// The span of `source` from byte `start` to byte `end`.
    pub fn new(source: Arc<Source>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    /// The 1-based line and column of the span's start — the coordinate a diagnostic names a location by. Columns count Unicode scalar values on the line, matching where [`Span::pane`] stands its marks.
    pub fn line_column(&self) -> (usize, usize) {
        let (line, line_start) = self.source.line_of(self.start);
        let column = 1 + self.source.text[line_start..self.start].chars().count();

        (line + 1, column)
    }

    /// The pane of the reader's code a report shows for the span: the line it starts on, as written, and the marks beneath what it covers of that line, each column counted as a terminal draws it.
    ///
    /// **A line past [`LINE_WIDTH`] is quoted as a window around the span's start**, [`LINE_WIDTH`] scalars of it with `…` where it is cut, so a report costs what it shows rather than its line, and a line the formatter wrote is quoted whole.
    pub fn pane(&self) -> Pane {
        let source = &self.source.text;
        let start = self.start;
        let end = self.end;

        let (line, line_start) = self.source.line_of(start);
        let line_end = self.source.line_end(line);
        let (shown_start, shown_end) = window(source, line_start, line_end, start);
        let (opening, closing) = (
            if shown_start > line_start { "…" } else { "" },
            if shown_end < line_end { "…" } else { "" },
        );

        let width: usize = graphemes(&source[start..end.clamp(start, shown_end)])
            .map(|cluster| match cluster {
                "\t" => 1,
                _ => columns(cluster),
            })
            .sum();

        // A cluster pads as the columns it takes, and a tab pads as a tab: the line above is printed verbatim, so padding a tab with a space would put the mark one column along where the text moves eight, and every tab-indented line would report a mark short of what it marks.
        let indent: String = iter::once(" ".repeat(opening.chars().count()))
            .chain(
                graphemes(&source[shown_start..start]).map(|cluster| match cluster {
                    "\t" => "\t".to_string(),
                    _ => " ".repeat(columns(cluster)),
                }),
            )
            .collect();

        Pane {
            number: line + 1,
            line: format!("{opening}{}{closing}", &source[shown_start..shown_end]),
            indent,
            marks: "^".repeat(width.max(1)),
        }
    }

    /// Where the span begins, as a reader of `style` is told it: the file, spelled from where they stand, then the line and the column, the one token a terminal opens; the line and the column alone for text that has no file.
    pub fn place(&self, style: &Style) -> String {
        let (line, column) = self.line_column();

        match &self.source.path {
            Some(path) => format!("{}:{line}:{column}", style.spelled(path)),
            None => format!("{line}:{column}"),
        }
    }
}

/// One pane of the reader's code, as a report shows it: the number of its line, the line, and the marks beneath it with the padding that stands them under what they mark.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pane {
    pub number: usize,
    pub line: String,
    pub indent: String,
    pub marks: String,
}

/// The part of the line from `line_start` to `line_end` a report quotes around `at`, as byte offsets: the whole line where it holds no more than [`LINE_WIDTH`] scalars, and otherwise [`LINE_WIDTH`] of them with a third before `at`, moved back from the line's end where fewer stand after it. Each bound is found by walking at most [`LINE_WIDTH`] scalars from `at`, so quoting a long line costs the window and not the line.
fn window(text: &str, line_start: usize, line_end: usize, at: usize) -> (usize, usize) {
    let line = &text[line_start..line_end];
    if line.len() <= LINE_WIDTH || line.chars().count() <= LINE_WIDTH {
        return (line_start, line_end);
    }
    let back = |from: usize, scalars: usize| {
        text[line_start..from]
            .char_indices()
            .rev()
            .nth(scalars - 1)
            .map_or(line_start, |(offset, _)| line_start + offset)
    };
    let left = back(at, LINE_WIDTH / 3);
    match text[left..line_end].char_indices().nth(LINE_WIDTH) {
        Some((offset, _)) => (left, left + offset),
        None => (back(line_end, LINE_WIDTH), line_end),
    }
}

/// A diagnostic as data: the message, and the span it is about when it is about one. Every stage's error renders through this — [`Report::rendered`] is the one shape a Curios diagnostic has — so a consumer that wants the location gets it as a span rather than by parsing the place back out of the text. The message is text rather than the stage's own error, deliberately: a report is what a stage *said*, and it survives the crate boundary that the error, with its terms and spellings, does not.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Report {
    pub span: Option<Span>,
    /// The kind of thing reported, as a tag: a few words that name it wherever it is met, and nothing of the program it is met in.
    pub headline: String,
    /// What is reported of this program, a fact each, in the order they are said: what stands here, then what it is held to, then the way out. A fact too wide for a line runs over several, each after its first indented as the fact writes it.
    pub facts: Vec<String>,
    /// Each further place the report is about, in the order it says them — see [`Note`].
    pub notes: Vec<Note>,
}

/// What a report says before it stands anywhere, as whoever says it builds it: a tag, and each fact beneath it. The first line begun is the tag, and every one begun after it a fact, so which is which is how it was built and nothing reads it back out of text.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Said {
    lines: Vec<String>,
}

impl Said {
    /// What is said, in `headline` alone.
    pub fn line(headline: impl Into<String>) -> Self {
        Self {
            lines: vec![headline.into()],
        }
    }

    /// Go on with the line in hand.
    pub fn text(&mut self, text: impl fmt::Display) {
        match self.lines.last_mut() {
            Some(line) => line.push_str(&text.to_string()),
            None => self.lines.push(text.to_string()),
        }
    }

    /// Begin a fact: what is written next stands on a line of its own, beneath what is said.
    pub fn fact(&mut self) {
        // A fact begun before anything is said rests beneath a line that is still to come.
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }
        self.lines.push(String::new());
    }

    /// `fact`, whole, as a fact of its own.
    pub fn stating(&mut self, fact: impl fmt::Display) {
        self.fact();
        self.text(fact);
    }

    /// Everything `other` says, after this: its line as a fact, and its facts after it.
    pub fn extend(&mut self, other: Said) {
        for line in other.lines {
            self.stating(line);
        }
    }

    /// Whether nothing was said.
    pub fn is_empty(&self) -> bool {
        self.lines.iter().all(String::is_empty)
    }

    /// The line, and the facts it rests on.
    pub fn into_parts(self) -> (String, Vec<String>) {
        let mut lines = self.lines.into_iter();

        (lines.next().unwrap_or_default(), lines.collect())
    }
}

impl fmt::Display for Said {
    /// What is said as one text: the line, and each fact on a line beneath it, indented as a detail of it.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((line, facts)) = self.lines.split_first() {
            formatter.write_str(line)?;
            for fact in facts {
                write!(formatter, "\n  {}", fact.replace('\n', "\n  "))?;
            }
        }

        Ok(())
    }
}

/// A further place a report is about, with the line that says what stands there: the first declaration of a name declared twice, each other member of a cycle. A report is one thing said, and some things are said about two places; a note is how the second is a span rather than a line number in the message, so a consumer placing a diagnostic can show it and a reader is shown its text.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Note {
    pub span: Span,
    pub message: String,
}

impl Report {
    /// A report about `span` that says `headline`.
    pub fn at(span: Span, headline: impl Into<String>) -> Self {
        Self::of(Some(span), Said::line(headline))
    }

    /// A report about nothing in particular that says `headline`: a manifest, a store, a certifier refusal that names an item rather than a position.
    pub fn unlocated(headline: impl Into<String>) -> Self {
        Self::of(None, Said::line(headline))
    }

    /// A report of what `said` says, about `span` where it is about a place.
    pub fn of(span: Option<Span>, said: Said) -> Self {
        let (headline, facts) = said.into_parts();

        Self {
            span,
            headline,
            facts,
            notes: Vec::new(),
        }
    }

    /// This report, resting on `fact` as well.
    pub fn stating(mut self, fact: impl Into<String>) -> Self {
        self.facts.push(fact.into());
        self
    }

    /// What the report says as one text, for whoever shows a diagnostic as a message beside its place: its first line, and each fact on a line beneath it.
    pub fn message(&self) -> String {
        let mut said = Said::line(self.headline.clone());
        for fact in &self.facts {
            said.stating(fact);
        }

        said.to_string()
    }

    /// [`message`](Self::message) for a reader who lists reports by their first line, where a tag alone names nothing of the program: the tag and the first fact on that line, and each further fact on a line beneath it. A first fact that runs over several lines stays beneath the tag.
    pub fn listed(&self) -> String {
        let (first, rest) = match self.facts.split_first() {
            Some((first, rest)) if !first.contains('\n') => {
                (format!("{}: {first}", self.headline), rest)
            }
            _ => (self.headline.clone(), self.facts.as_slice()),
        };
        let mut said = Said::line(first);
        for fact in rest {
            said.stating(fact);
        }

        said.to_string()
    }

    /// This report, about `span` as well, where `message` says what stands.
    pub fn noting(mut self, span: Span, message: impl Into<String>) -> Self {
        self.notes.push(Note {
            span,
            message: message.into(),
        });
        self
    }

    /// The report as a reader of `style` sees it, a block: what it says, alone on a line; what that rests on, a line a fact, on a trunk; each further place it is about, its code closed by what stands there; and its own place, its code closed by the word of what the report is, `kind`.
    ///
    /// ```text
    /// `a` is already declared
    /// │
    /// │  1 │ let a = 1;
    /// │    │     ^
    /// │
    /// ├─ first declared here; lib.crs:1:5
    /// │
    /// │  2 │ let a = 2;
    /// │    │     ^
    /// │
    /// ╰─ error; lib.crs:2:5
    /// ```
    ///
    /// **The report's own place is its last line**, so the line a reader's eye ends on, and a terminal opens, is where the report stands; every further place stands before it.
    ///
    /// **One gutter serves every pane of a report**: each line's number ends in the column the widest ends in, so every pane's code begins in one column. An empty line of the trunk stands above and below a pane, which sets the code apart from what is said of it.
    pub fn rendered(&self, kind: Kind, style: &Style) -> String {
        let trunk = style.dim("│");
        let word = match kind.troubled() {
            true => style.troubled(kind.word()),
            false => kind.word().to_string(),
        };
        let gutter = self
            .notes
            .iter()
            .map(|note| &note.span)
            .chain(&self.span)
            .map(|span| span.pane().number.to_string().len())
            .max()
            .unwrap_or(0);
        let pane = |span: &Span| {
            let Pane {
                number,
                line,
                indent,
                marks,
            } = span.pane();
            let marks = match kind.troubled() {
                true => style.troubled(marks),
                false => marks,
            };

            [
                trunk.clone(),
                format!(
                    "{trunk}  {} {line}",
                    style.dim(format!("{number:>gutter$} │"))
                ),
                format!(
                    "{trunk}  {} {indent}{marks}",
                    style.dim(format!("{:gutter$} │", ""))
                ),
                trunk.clone(),
            ]
        };

        let mut lines = vec![style.bold(&self.headline)];
        lines.extend(
            self.facts
                .iter()
                .flat_map(|fact| fact.split('\n'))
                .map(|line| format!("{trunk}  {line}")),
        );
        for note in &self.notes {
            lines.extend(pane(&note.span));
            lines.push(format!(
                "{} {}; {}",
                style.dim("├─"),
                note.message,
                note.span.place(style)
            ));
        }
        match &self.span {
            Some(span) => {
                lines.extend(pane(span));
                lines.push(format!("{} {word}; {}", style.dim("╰─"), span.place(style)));
            }
            None => lines.push(format!("{} {word}", style.dim("╰─"))),
        }

        lines.join("\n")
    }

    /// The report as an error a reader standing nowhere, who sees no colour, is shown: what a `Display` that has no reader to ask writes.
    pub fn render(&self) -> String {
        self.rendered(Kind::Error, &Style::plain())
    }

    /// Several reports of one kind rendered in order for a reader of `style`, a blank line between each.
    pub fn rendered_all(reports: &[Report], kind: Kind, style: &Style) -> String {
        reports
            .iter()
            .map(|report| report.rendered(kind, style))
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    /// [`Report::rendered_all`] as errors, for a reader standing nowhere who sees no colour.
    pub fn render_all(reports: &[Report]) -> String {
        Self::rendered_all(reports, Kind::Error, &Style::plain())
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.source, &other.source)
            && self.start == other.start
            && self.end == other.end
    }
}

impl Eq for Span {}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.source) as usize).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

#[cfg(test)]
mod tests;
