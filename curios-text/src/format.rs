//! The canonical formatter behind `curios format`: parse with comments, render the file as one document, verify, emit.
//!
//! The laws: formatting never changes a program — the output is reparsed and compared structurally, and a mismatch refuses to emit; every captured comment appears in the output exactly once — the verifier recounts them; nothing is ever reordered; **a comment stays where it was written** — one on a line of its own between the two tokens it stood between, one after code on the line that code lands on; and **the result is idempotent**. `format/place_tests.rs` checks the last two from every position a comment can be written in, in its fixtures and across the sources of the tree, rather than leaving them to be believed.
//!
//! **A comment's place is decided by the renderer, from the source text.** Where a comment goes is a fact about the *output*: one riding a line's end follows whatever was written last there — frequently punctuation the enclosing printer emits, which no node of the tree owns — and one on its own line stands between two tokens, either of which may be such punctuation. So the printer says nothing about comments at all: the renderer is handed the text the file was read from as an [`Original`] and follows it as it writes, and a comment is due exactly when everything written before it has gone out. `README.md` states the decision and what it rejected.
//!
//! Comments are classified once, from the source text: one with source content earlier on its own line *rides* that line, and every other takes a line of its own. Nothing is classified twice and nothing is decided twice.
//!
//! **One document for the whole file**, built and rendered once by [`emit`], which says why.

use {
    super::{FormatInput, TopItem, between_items, parse_for_format, print_term, print_top_item},
    curios_print::{Original, Owed, Printer, flat, hard_line, reached, run_printer_placing},
    curios_utilities::{Source, Span},
    std::{cell::RefCell, fmt, path::Path, sync::Arc},
};

/// The formatter's verdict on one source: the canonical text, tagged by whether it differs from what was read. The formatter itself is pure — whether a `Changed` result fails a check or rewrites a file is the caller's policy.
#[derive(Debug, PartialEq, Eq)]
pub enum Formatted {
    Unchanged(String),
    Changed(String),
}

impl Formatted {
    /// Format `source` canonically. `Err` is a human-readable refusal: a parse failure, or a verification failure — the output failing to reparse to the same program with the same comments — in which case nothing should be written.
    pub fn from_source(source: &Arc<Source>) -> Result<Self, String> {
        let input = parse_for_format(source).map_err(|error| error.format())?;
        let comments = classify(&source.text, &input.comments);
        let expected_comments = comments.len();

        // Every comment goes to the renderer with the text it was written in, which is all that decides its place — see `Original`.
        let output = emit(
            &input,
            &Original {
                text: &source.text,
                comments,
                added,
                dropped,
                closes,
            },
        );

        verify(&input, expected_comments, &output)?;
        Ok(match output == source.text {
            true => Formatted::Unchanged(output),
            false => Formatted::Changed(output),
        })
    }

    /// [`Formatted::from_source`] for the file at `path`, errors located by it. Writing a `Changed` result back is the caller's policy.
    pub fn from_path(path: &Path) -> Result<Self, String> {
        let located = |error: String| format!("{}: {error}", path.display());
        let source = Source::read(path).map_err(|error| located(error.to_string()))?;
        Self::from_source(&source).map_err(located)
    }
}

/// The canonical width — the same target goal reports render within.
const WIDTH: usize = 100;

/// The canonical indent — the corpus's four spaces.
const INDENT: usize = 4;

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
                own_line: source[line_start..span.start]
                    .chars()
                    .all(char::is_whitespace),
                text: source[span.start..span.end].trim_end().to_string(),
            }
        })
        .collect()
}

/// Render one document at the canonical width and indent, following `original`.
fn render(printer: Printer, original: &Original<'_>) -> String {
    struct Within<'a, 'b>(RefCell<Option<Printer>>, &'a Original<'b>);
    impl fmt::Display for Within<'_, '_> {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let printer = self.0.borrow_mut().take().expect("rendered once");

            run_printer_placing(printer, formatter, INDENT, WIDTH, self.1)
        }
    }
    Within(RefCell::new(Some(printer)), original).to_string()
}

/// The parsed file as one document: items separated by exactly one blank line — except consecutive `use` declarations and a `use` directly following a `mod`, which stack with none, as the corpus writes its import heads and its `mod X; use X/{…}` pairs — and the tail last.
///
/// **One document for the whole file, rendered once.** Every comment is placed by that single run, which is what lets a comment between two items be placed at all: rendering each item separately would give each its own renderer, and a comment belonging to neither would have to be spliced in as text by this function. Nothing here handles comments.
pub(crate) fn emit(input: &FormatInput, original: &Original<'_>) -> String {
    let mut parts: Vec<Printer> = Vec::new();
    let mut previous: Option<&TopItem> = None;

    for (item, span) in input.module.items.iter().zip(&input.item_spans) {
        parts.push(between_items(previous, item));

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

    format!("{}\n", render(flat(parts), original))
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

#[cfg(test)]
mod place_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;
