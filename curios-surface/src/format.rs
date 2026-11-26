//! The canonical formatter behind `curios format`: parse with comments, render the file as one document, verify, emit.
//!
//! The laws: formatting never changes a program — the output is reparsed and compared structurally, and a mismatch refuses to emit; every captured comment appears in the output exactly once — the verifier recounts them; nothing is ever reordered; **a comment stays where it was written** — one on a line of its own between the two tokens it stood between, one after code on the line that code lands on; and **the result is idempotent**. `format/place_tests.rs` checks the last two from every position a comment can be written in, in its fixtures and across the sources of the tree, rather than leaving them to be believed.
//!
//! **A comment's place is decided by the renderer, from the source text.** Where a comment goes is a fact about the *output*: one riding a line's end follows whatever was written last there — frequently punctuation the enclosing printer emits, which no node of the tree owns — and one on its own line stands between two tokens, either of which may be such punctuation. So the printer says nothing about comments at all: the renderer is handed the text the file was read from as an [`Original`] and follows it as it writes, and a comment is due exactly when everything written before it has gone out. `README.md` states the decision and what it rejected.
//!
//! Comments are classified once, from the source text: one with source content earlier on its own line *rides* that line, and every other takes a line of its own. Nothing is classified twice and nothing is decided twice.
//!
//! **One document for the whole file**, built and rendered once by [`emit`], which says why.
//!
//! **A file is formatted within a [`Format`]**: the width of its lines, the columns a level of nesting indents them by, and how much of a match one line keeps ([`FlatMatch`]). Every law holds at each line width it admits, [`LINE_WIDTHS`], at each indentation, [`INDENT_WIDTHS`], under the default flat match and at each extreme the settings admit, which `format/width_tests.rs` and `format/place_tests.rs` hold over the sources of the tree. Where a file's format comes from is `curios-package`'s `FormatSettings`.

use {
    super::{
        CoreFormatInput, FormatInput, TopItem, WrittenIn, between_items, parse_core_for_format,
        parse_for_format, print_core_entry, print_core_item, print_term, print_top_item,
    },
    curios_common::{About, MAX_LINE_WIDTH, OsError, Report, Said, Source, Span},
    curios_parse::is_blank,
    curios_print::{Original, Owed, Printer, flat, hard_line, reached, run_printer_placing},
    std::{cell::RefCell, fmt, path::Path, str::FromStr, sync::Arc},
};

/// The narrowest width a file may be formatted within, a page's narrow column, as a cheatsheet's: below it a line nothing can break — a long name, a deep nesting's indentation — is most of what a narrower width would change.
pub const MIN_LINE_WIDTH: usize = 50;

/// The widths a file may be formatted within, narrowest first: a page's narrow column, a terminal's line, which most style guides keep, and [`MAX_LINE_WIDTH`], the default. The formatter's laws are held over the sources of the tree at each, so no width between them is admitted, the laws never held there.
pub const LINE_WIDTHS: [usize; 3] = [MIN_LINE_WIDTH, 80, MAX_LINE_WIDTH];

/// The values a setting admits, as a refusal and a help text name them: [`LINE_WIDTHS`] as `` `50`, `80` or `100` ``.
pub fn spelled_choices(choices: &[usize]) -> String {
    let spelled = choices
        .iter()
        .map(|choice| format!("`{choice}`"))
        .collect::<Vec<_>>();
    match spelled.split_last() {
        Some((last, rest)) => format!("{} or {last}", rest.join(", ")),
        None => String::new(),
    }
}

/// The columns a formatted line is written within, counted in scalar values: one of [`LINE_WIDTHS`], [`MAX_LINE_WIDTH`] where nothing states another. No wider, since a report quotes a line as a window [`MAX_LINE_WIDTH`] wide, and a line the formatter wrote is quoted whole.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Width(usize);

impl Width {
    /// `columns` as a width, refused outside the widths the formatter admits.
    pub fn new(columns: usize) -> Result<Self, String> {
        match LINE_WIDTHS.contains(&columns) {
            true => Ok(Self(columns)),
            false => Err(format!(
                "a line width is {} columns, and `{columns}` is not",
                spelled_choices(&LINE_WIDTHS)
            )),
        }
    }

    /// The columns this width holds.
    pub fn columns(self) -> usize {
        self.0
    }
}

impl Default for Width {
    fn default() -> Self {
        Self(MAX_LINE_WIDTH)
    }
}

impl FromStr for Width {
    type Err = String;

    fn from_str(spelling: &str) -> Result<Self, String> {
        let columns = spelling.parse().map_err(|_| {
            format!(
                "a line width is a number of columns, {}, not `{spelling}`",
                spelled_choices(&LINE_WIDTHS)
            )
        })?;
        Self::new(columns)
    }
}

/// The indentation where nothing states another, the corpus's four columns.
pub const DEFAULT_INDENT_WIDTH: usize = 4;

/// The indentations a file may be formatted at: two columns, which a narrow page wants, four nesting levels taking eight columns where the default takes sixteen, and [`DEFAULT_INDENT_WIDTH`]. The formatter's laws are held over the sources of the tree at each, so no other is admitted.
pub const INDENT_WIDTHS: [usize; 2] = [2, DEFAULT_INDENT_WIDTH];

/// The columns a level of nesting indents a line by, written as spaces: one of [`INDENT_WIDTHS`], [`DEFAULT_INDENT_WIDTH`] where nothing states another. A fact of the page, as its width is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Indent(usize);

impl Indent {
    /// `columns` as an indentation, refused outside the indentations the formatter admits.
    pub fn new(columns: usize) -> Result<Self, String> {
        match INDENT_WIDTHS.contains(&columns) {
            true => Ok(Self(columns)),
            false => Err(format!(
                "an indentation is {} columns, and `{columns}` is not",
                spelled_choices(&INDENT_WIDTHS)
            )),
        }
    }

    /// The columns a level indents by.
    pub fn columns(self) -> usize {
        self.0
    }
}

impl Default for Indent {
    fn default() -> Self {
        Self(DEFAULT_INDENT_WIDTH)
    }
}

impl FromStr for Indent {
    type Err = String;

    fn from_str(spelling: &str) -> Result<Self, String> {
        let columns = spelling.parse().map_err(|_| {
            format!(
                "an indentation is a number of columns, {}, not `{spelling}`",
                spelled_choices(&INDENT_WIDTHS)
            )
        })?;
        Self::new(columns)
    }
}

/// The most a match keeps on one line, from `match` to `end`: its span in columns and its arms. A decision on one line is read as one thing, and past some span or some count of arms the eye loses which arm a body belongs to, and a dispatch table is read as rows: a property of the reader, not of the page, so it is the same at every line width, and a page narrower than the span decides by what fits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FlatMatch {
    width: usize,
    arms: usize,
}

/// The columns a match keeps on one line where nothing states another.
pub const DEFAULT_FLAT_MATCH_WIDTH: usize = 70;

/// The arms a match keeps on one line where nothing states another.
pub const DEFAULT_FLAT_MATCH_ARMS: usize = 3;

/// The fewest arms a match may be told it keeps on one line: a match kept off every line is the span's to say, `0` columns, and one way to say it is enough.
pub const MIN_FLAT_MATCH_ARMS: usize = 1;

/// The most arms a match may be told it keeps on one line: the most a line [`MAX_LINE_WIDTH`] wide shows of the narrowest match the printer writes, one with no head whose every arm is a guard or `_` and a body of a character each, `match | b => 0 | _ => 0 end`. A count past it keeps no match on one line that this count would not.
pub const MAX_FLAT_MATCH_ARMS: usize = (MAX_LINE_WIDTH - "match end".len()) / " | b => 0".len();

/// The setting of a [`FlatMatch`] a stated value was refused for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlatMatchSetting {
    Width,
    Arms,
}

impl FlatMatch {
    /// What reports, Curios/Core dumps and documentation pages keep of a match on one line, whatever a package states, and what a file is formatted with where nothing states another.
    pub const DEFAULT: Self = Self {
        width: DEFAULT_FLAT_MATCH_WIDTH,
        arms: DEFAULT_FLAT_MATCH_ARMS,
    };

    /// A match kept on one line within `width` columns and `arms` arms, each its default where it is `None`, on lines `line` wide: a stated width past the line is refused, since no line could show it, and the default is the line's where the line is narrower; arms fewer than [`MIN_FLAT_MATCH_ARMS`] or more than [`MAX_FLAT_MATCH_ARMS`] are refused.
    pub fn within(
        line: Width,
        width: Option<usize>,
        arms: Option<usize>,
    ) -> Result<Self, (FlatMatchSetting, String)> {
        let width = match width {
            Some(width) if width > line.columns() => {
                return Err((
                    FlatMatchSetting::Width,
                    format!(
                        "a flat match's width is at most the line width, and `{width}` is past `{}`",
                        line.columns()
                    ),
                ));
            }
            Some(width) => width,
            None => DEFAULT_FLAT_MATCH_WIDTH.min(line.columns()),
        };

        let arms = match arms {
            Some(arms) if arms < MIN_FLAT_MATCH_ARMS => {
                return Err((
                    FlatMatchSetting::Arms,
                    format!(
                        "a flat match keeps at least {MIN_FLAT_MATCH_ARMS} arm, and `{arms}` is fewer; a match kept off every line is `flat-match-width = 0`"
                    ),
                ));
            }
            Some(arms) if arms > MAX_FLAT_MATCH_ARMS => {
                return Err((
                    FlatMatchSetting::Arms,
                    format!(
                        "a flat match keeps at most {MAX_FLAT_MATCH_ARMS} arms, the most a line of {MAX_LINE_WIDTH} columns shows, and `{arms}` is more"
                    ),
                ));
            }
            Some(arms) => arms,
            None => DEFAULT_FLAT_MATCH_ARMS,
        };

        Ok(Self { width, arms })
    }

    /// The most columns a match spans on one line.
    pub fn width(self) -> usize {
        self.width
    }

    /// The most arms a match keeps on one line.
    pub fn arms(self) -> usize {
        self.arms
    }
}

impl Default for FlatMatch {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// What a file is formatted within: the width of its lines, the columns a level of nesting indents them by, and how much of a match one line keeps.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Format {
    pub line_width: Width,
    pub indent_width: Indent,
    pub flat_match: FlatMatch,
}

/// The formatter's verdict on one source: the canonical text, tagged by whether it differs from what was read. The formatter itself is pure — whether a `Changed` result fails a check or rewrites a file is the caller's policy.
#[derive(Debug, PartialEq, Eq)]
pub enum Formatted {
    Unchanged(String),
    Changed(String),
}

impl Formatted {
    /// Format `source`, written in `written`, canonically within `format`. `Err` is the report of a refusal: a parse failure, where the text stops being one, or a verification failure — the output failing to reparse to the same file with the same comments — in which case nothing should be written.
    pub fn from_source(
        source: &Arc<Source>,
        written: WrittenIn,
        format: Format,
    ) -> Result<Self, Box<Report>> {
        let output = match written {
            WrittenIn::CoreText => core_text_formatted(source, format)?,
            WrittenIn::Program => program_formatted(source, format)?,
        };

        Ok(match output == source.text() {
            true => Formatted::Unchanged(output),
            false => Formatted::Changed(output),
        })
    }

    /// [`Formatted::from_source`] for the file at `path`, written in what its name says ([`WrittenIn::of`]), which a refusal is about. Writing a `Changed` result back is the caller's policy.
    pub fn from_path(path: &Path, format: Format) -> Result<Self, Box<Report>> {
        let source = Source::read(path).map_err(|error| {
            let mut said = Said::line("unreadable file");
            said.stating(OsError(&error));

            Report::about(About::Path(path.to_path_buf()), said)
        })?;

        Self::from_source(&source, WrittenIn::of(path), format)
    }
}

/// What the renderer follows: the text `source` was read from, with the comments the parse consumed, each classified.
fn original<'a>(source: &'a Source, comments: &[Span]) -> Original<'a> {
    Original {
        text: source.text(),
        comments: classify(source.text(), comments),
        added,
        dropped,
        closes,
    }
}

/// A program or a module, formatted within `format` and verified.
fn program_formatted(source: &Arc<Source>, format: Format) -> Result<String, Box<Report>> {
    let input = parse_for_format(source).map_err(|error| error.report())?;
    // Every comment goes to the renderer with the text it was written in, which is all that decides its place — see `Original`.
    let original = original(source, &input.comments);
    let expected_comments = original.comments.len();
    let output = emit(&input, &original, format);

    verify(&input, expected_comments, &output).map_err(|words| unverified(source, &words))?;
    Ok(output)
}

/// A file of Curios/Core, formatted within `format` and verified as a program is. It is read and not checked: a file its reader would refuse — a field its node does not hold, a form Curios/Core refuses — is formatted all the same.
fn core_text_formatted(source: &Arc<Source>, format: Format) -> Result<String, Box<Report>> {
    let input = parse_core_for_format(source).map_err(|error| error.report())?;
    let original = original(source, &input.comments);
    let expected_comments = original.comments.len();
    let output = emit_core_text(&input, &original, format);

    verify_core_text(&input, expected_comments, &output)
        .map_err(|words| unverified(source, &words))?;
    Ok(output)
}

/// The refusal of an output that failed its verification, which is the formatter's own fault and no fault of `source`: about the file it was read from, where it was read from one, and the text it is named as otherwise.
fn unverified(source: &Source, words: &str) -> Box<Report> {
    let mut said = Said::line("compiler fault");
    said.stating("the formatter's output does not read back as the text it formatted");
    said.stating(words);
    said.stating("nothing was written");

    Box::new(match (source.file(), source.name()) {
        (Some(file), _) => Report::about(About::Path(file.to_path_buf()), said),
        (None, Some(name)) => Report::about(About::Named(name.to_string()), said),
        (None, None) => Report::of(None, said),
    })
}

/// The one character the printer writes where the source may hold none: the comma closing a broken list.
fn added(char: char) -> bool {
    char == ','
}

/// What the source may hold and the printer leaves out: a list's last comma, and the parentheses the tree does not record.
fn dropped(char: char) -> bool {
    matches!(char, ',' | '(' | ')')
}

/// What may still follow a token on the line its comment rides: the punctuation that closes what the token ends.
fn closes(char: char) -> bool {
    matches!(char, ')' | ']' | '}' | ',' | ';')
}

/// The captured comments, classified, in the ascending order the parse recorded them in: one with source content earlier on its own line rides that line, and every other has a line of its own.
fn classify(source: &str, spans: &[Span]) -> Vec<Owed> {
    spans
        .iter()
        .map(|span| {
            let line_start = source[..span.start]
                .rfind('\n')
                .map(|index| index + 1)
                .unwrap_or(0);
            Owed {
                at: span.start,
                end: span.end,
                own_line: source[line_start..span.start].chars().all(is_blank),
                text: source[span.start..span.end].trim_end().to_string(),
            }
        })
        .collect()
}

/// Render one document within `format`'s width at its indentation, following `original`.
fn render(printer: Printer, original: &Original<'_>, format: Format) -> String {
    struct Within<'a, 'b>(RefCell<Option<Printer>>, &'a Original<'b>, Format);
    impl fmt::Display for Within<'_, '_> {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let printer = self.0.borrow_mut().take().expect("rendered once");
            let Format {
                line_width,
                indent_width,
                ..
            } = self.2;

            run_printer_placing(
                printer,
                formatter,
                indent_width.columns(),
                line_width.columns(),
                self.1,
            )
        }
    }
    Within(RefCell::new(Some(printer)), original, format).to_string()
}

/// The parsed file as one document: items separated by exactly one blank line — except a `use` following a `use` or a `mod X;`, which keeps the separation it was written with, stacked as the corpus writes its import heads and its `mod X; use X/{…}` pairs, or one blank line apart however many were written — and the tail last.
///
/// **One document for the whole file, rendered once.** Every comment is placed by that single run, which is what lets a comment between two items be placed at all: rendering each item separately would give each its own renderer, and a comment belonging to neither would have to be spliced in as text by this function. Nothing here handles comments.
pub fn emit(input: &FormatInput, original: &Original<'_>, format: Format) -> String {
    let mut parts: Vec<Printer> = Vec::new();
    let mut previous: Option<&TopItem> = None;

    for (item, span) in input.module.items.iter().zip(&input.item_spans) {
        let apart = previous.map(|_| written_apart(original.text, span.start));
        parts.push(between_items(previous, item, apart));

        // Marked at both ends, as a term is — see `print_term`.
        parts.push(reached(span.start));
        parts.push(print_top_item(item.clone(), format.flat_match));
        parts.push(reached(span.end));
        previous = Some(item);
    }

    if let Some(tail) = &input.tail {
        if previous.is_some() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(print_term(tail.clone(), format.flat_match));
    }

    format!("{}\n", render(flat(parts), original, format))
}

/// A file of Curios/Core as one document, as a program's is ([`emit`]): its items a blank line apart, each marked at both ends, and the entry last.
fn emit_core_text(input: &CoreFormatInput, original: &Original<'_>, format: Format) -> String {
    let mut parts: Vec<Printer> = Vec::new();

    for (item, span) in input.file.items.iter().zip(&input.item_spans) {
        if !parts.is_empty() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(reached(span.start));
        parts.push(print_core_item(item.clone(), format.flat_match));
        parts.push(reached(span.end));
    }

    if let Some(entry) = &input.file.entry {
        if !parts.is_empty() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(print_core_entry(entry.clone(), format.flat_match));
    }

    format!("{}\n", render(flat(parts), original, format))
}

/// Whether a blank line stands between the item that starts at `start` and the code before it, read back over the comment lines between them: a comment is no blank line, and an item sharing its line with the one before is not apart from it. Read from the item's start, since an item's span runs on over the blank lines and comments the parse consumed after it.
fn written_apart(text: &str, start: usize) -> bool {
    let line_start = text[..start].rfind('\n').map_or(0, |index| index + 1);
    if !text[line_start..start].chars().all(is_blank) {
        return false;
    }
    for line in text[..line_start].lines().rev() {
        if line.chars().all(is_blank) {
            return true;
        }
        if !line.trim_start().starts_with("--") {
            return false;
        }
    }
    false
}

/// The refuse-to-write gate: the output must reparse to the same program (structural equality — spans are excluded from it by design) carrying the same number of comments.
fn verify(input: &FormatInput, expected_comments: usize, output: &str) -> Result<(), String> {
    let reparsed_source = Source::inline(output);
    let reparsed = parse_for_format(&reparsed_source).map_err(|error| {
        format!(
            "formatting produced unparseable output:\n{}",
            error.format()
        )
    })?;

    if reparsed.module != input.module || reparsed.tail != input.tail {
        return Err("formatting changed the parsed program; refusing to write".to_string());
    }
    if reparsed.comments.len() != expected_comments {
        return Err(format!(
            "formatting lost comments ({} in, {} out); refusing to write",
            expected_comments,
            reparsed.comments.len()
        ));
    }
    Ok(())
}

/// [`verify`] for a file of Curios/Core: the output must reparse to the same file, carrying the same number of comments.
fn verify_core_text(
    input: &CoreFormatInput,
    expected_comments: usize,
    output: &str,
) -> Result<(), String> {
    let reparsed = parse_core_for_format(&Source::inline(output)).map_err(|error| {
        format!(
            "formatting produced unparseable output:\n{}",
            error.format()
        )
    })?;

    if reparsed.file != input.file {
        return Err("formatting changed the parsed file; refusing to write".to_string());
    }
    if reparsed.comments.len() != expected_comments {
        return Err(format!(
            "formatting lost comments ({} in, {} out); refusing to write",
            expected_comments,
            reparsed.comments.len()
        ));
    }
    Ok(())
}

#[cfg(test)]
mod comment_tests;
#[cfg(test)]
mod core_text_tests;
#[cfg(test)]
mod layout_tests;
#[cfg(test)]
mod place_tests;
#[cfg(test)]
mod program_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod width_tests;
