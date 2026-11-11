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
//! **The width is the one setting**, and every law holds at each width it admits: [`NARROWEST`] through [`LINE_WIDTH`], which `format/width_tests.rs` and `format/place_tests.rs` hold over the sources of the tree at the narrowest. Where a file's width comes from is `curios-package`'s `FormatSettings`.

use {
    super::{
        CoreFormatInput, FormatInput, TopItem, between_items, parse_core_for_format,
        parse_for_format, print_core_entry, print_core_item, print_term, print_top_item,
    },
    curios_common::{LINE_WIDTH, Source, Span},
    curios_parse::is_blank,
    curios_print::{Original, Owed, Printer, flat, hard_line, reached, run_printer_placing},
    std::{cell::RefCell, fmt, path::Path, str::FromStr, sync::Arc},
};

/// The narrowest width a file may be formatted within. The formatter's laws are held over the sources of the tree at it, and below it a line nothing can break — a long name, a deep nesting's indentation — is most of what a narrower width would change.
pub const NARROWEST: usize = 50;

/// The columns a formatted line is written within, counted in scalar values: one of [`NARROWEST`] through [`LINE_WIDTH`], which is the default. No wider, since a report quotes a line as a window [`LINE_WIDTH`] wide, and a line the formatter wrote is quoted whole.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Width(usize);

impl Width {
    /// `columns` as a width, refused outside the widths the formatter admits.
    pub fn new(columns: usize) -> Result<Self, String> {
        match (NARROWEST..=LINE_WIDTH).contains(&columns) {
            true => Ok(Self(columns)),
            false => Err(format!(
                "a width is {NARROWEST} through {LINE_WIDTH} columns, and {columns} is not"
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
        Self(LINE_WIDTH)
    }
}

impl FromStr for Width {
    type Err = String;

    fn from_str(spelling: &str) -> Result<Self, String> {
        let columns = spelling.parse().map_err(|_| {
            format!(
                "a width is a number of columns, {NARROWEST} through {LINE_WIDTH}, not {spelling:?}"
            )
        })?;
        Self::new(columns)
    }
}

/// The formatter's verdict on one source: the canonical text, tagged by whether it differs from what was read. The formatter itself is pure — whether a `Changed` result fails a check or rewrites a file is the caller's policy.
#[derive(Debug, PartialEq, Eq)]
pub enum Formatted {
    Unchanged(String),
    Changed(String),
}

impl Formatted {
    /// Format `source` canonically within `width`: as Core text where it was read from a `.crsc`, and as a program or a module otherwise. `Err` is a human-readable refusal: a parse failure, or a verification failure — the output failing to reparse to the same file with the same comments — in which case nothing should be written.
    pub fn from_source(source: &Arc<Source>, width: Width) -> Result<Self, String> {
        let output = match written_as_core_text(source) {
            true => core_text_formatted(source, width)?,
            false => program_formatted(source, width)?,
        };

        Ok(match output == source.text() {
            true => Formatted::Unchanged(output),
            false => Formatted::Changed(output),
        })
    }

    /// [`Formatted::from_source`] for the file at `path`, errors located by it. Writing a `Changed` result back is the caller's policy.
    pub fn from_path(path: &Path, width: Width) -> Result<Self, String> {
        let located = |error: String| format!("{}: {error}", path.display());
        let source = Source::read(path).map_err(|error| located(error.to_string()))?;
        Self::from_source(&source, width).map_err(located)
    }
}

/// The canonical indent — the corpus's four spaces.
const INDENT: usize = 4;

/// Whether `source` was read from a file of Core text, `.crsc`, by the extension of the path it was read from.
fn written_as_core_text(source: &Source) -> bool {
    source.path().is_some_and(|path| {
        path.extension()
            .is_some_and(|extension| extension == "crsc")
    })
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

/// A program or a module, formatted within `width` and verified.
fn program_formatted(source: &Arc<Source>, width: Width) -> Result<String, String> {
    let input = parse_for_format(source).map_err(|error| error.format())?;
    // Every comment goes to the renderer with the text it was written in, which is all that decides its place — see `Original`.
    let original = original(source, &input.comments);
    let expected_comments = original.comments.len();
    let output = emit(&input, &original, width);

    verify(&input, expected_comments, &output)?;
    Ok(output)
}

/// A file of Core text, formatted within `width` and verified as a program is. It is read and not checked: a file its reader would refuse — a field its node does not hold, a form Core text refuses — is formatted all the same.
fn core_text_formatted(source: &Arc<Source>, width: Width) -> Result<String, String> {
    let input = parse_core_for_format(source).map_err(|error| error.format())?;
    let original = original(source, &input.comments);
    let expected_comments = original.comments.len();
    let output = emit_core_text(&input, &original, width);

    verify_core_text(&input, expected_comments, &output)?;
    Ok(output)
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

/// Render one document within `width` at the canonical indent, following `original`.
fn render(printer: Printer, original: &Original<'_>, width: Width) -> String {
    struct Within<'a, 'b>(RefCell<Option<Printer>>, &'a Original<'b>, Width);
    impl fmt::Display for Within<'_, '_> {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let printer = self.0.borrow_mut().take().expect("rendered once");

            run_printer_placing(printer, formatter, INDENT, self.2.columns(), self.1)
        }
    }
    Within(RefCell::new(Some(printer)), original, width).to_string()
}

/// The parsed file as one document: items separated by exactly one blank line — except a `use` following a `use` or a `mod X;`, which keeps the separation it was written with, stacked as the corpus writes its import heads and its `mod X; use X/{…}` pairs, or one blank line apart however many were written — and the tail last.
///
/// **One document for the whole file, rendered once.** Every comment is placed by that single run, which is what lets a comment between two items be placed at all: rendering each item separately would give each its own renderer, and a comment belonging to neither would have to be spliced in as text by this function. Nothing here handles comments.
pub fn emit(input: &FormatInput, original: &Original<'_>, width: Width) -> String {
    let mut parts: Vec<Printer> = Vec::new();
    let mut previous: Option<&TopItem> = None;

    for (item, span) in input.module.items.iter().zip(&input.item_spans) {
        let apart = previous.map(|_| written_apart(original.text, span.start));
        parts.push(between_items(previous, item, apart));

        // Marked at both ends, as a term is — see `print_term`.
        parts.push(reached(span.start));
        parts.push(print_top_item(item.clone()));
        parts.push(reached(span.end));
        previous = Some(item);
    }

    if let Some(tail) = &input.tail {
        if previous.is_some() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(print_term(tail.clone()));
    }

    format!("{}\n", render(flat(parts), original, width))
}

/// A file of Core text as one document, as a program's is ([`emit`]): its items a blank line apart, each marked at both ends, and the entry last.
fn emit_core_text(input: &CoreFormatInput, original: &Original<'_>, width: Width) -> String {
    let mut parts: Vec<Printer> = Vec::new();

    for (item, span) in input.file.items.iter().zip(&input.item_spans) {
        if !parts.is_empty() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(reached(span.start));
        parts.push(print_core_item(item.clone()));
        parts.push(reached(span.end));
    }

    if let Some(entry) = &input.file.entry {
        if !parts.is_empty() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(print_core_entry(entry.clone()));
    }

    format!("{}\n", render(flat(parts), original, width))
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

/// [`verify`] for a file of Core text: the output must reparse to the same file, carrying the same number of comments.
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
