//! The canonical formatter behind `curios format`: parse with comments, weave them through width-aware rendering, verify, emit.
//!
//! The laws: formatting never changes a program — the output is reparsed and compared structurally, and a mismatch refuses to emit; every captured comment appears in the output exactly once — the verifier recounts them; the result is deterministic and idempotent; and nothing is ever reordered, which is also what makes the weave correct: printing follows source order, so each parsed node may simply *claim* every not-yet-claimed comment written before its own start.
//!
//! Comments are classified once, from the source text: a comment with source content earlier on its own line is *trailing* — it rides that line's end via the printer's line-suffix channel — and every other comment *leads* the next element, breaking onto its own line. A trailing comment's suffix fails the fits scan, so the group it lands in always breaks: the comment-is-a-hard-break law needs no special-casing. The weave is a thread-local present only during a `format` run; `Display` paths never consult it, so ordinary printing is untouched.

use {
    super::{FormatInput, TopItem, parse_for_format},
    crate::print::{print_term, print_top_item},
    curios_print::{Owed, Printer, begins, flat, hard_line, reaches, run_printer_placing},
    curios_utilities::{Source, Span},
    std::{cell::RefCell, fmt, path::Path, rc::Rc},
};

/// The formatter's verdict on one source: the canonical text, tagged by whether it differs from what was read. The formatter itself is pure — whether a `Changed` result fails a check or rewrites a file is the caller's policy.
#[derive(Debug, PartialEq, Eq)]
pub enum Formatted {
    Unchanged(String),
    Changed(String),
}

impl Formatted {
    /// Format `source` canonically. `Err` is a human-readable refusal: a parse failure, or a verification failure — the output failing to reparse to the same program with the same comments — in which case nothing should be written.
    pub fn from_source(source: &Rc<Source>) -> Result<Self, String> {
        let input = parse_for_format(source).map_err(|error| error.format())?;
        let comments = classify(&source.text, input.comments.clone());
        let expected_comments = comments.len();

        // Every comment goes to the renderer, which is the only thing that knows where an output line begins and ends — see `Owed`. Nothing here decides a comment's place; the printer only marks how far into the source its document has reached.
        OWED.with(|owed| {
            *owed.borrow_mut() = comments
                .into_iter()
                .map(|comment| Owed {
                    at: comment.start,
                    own_line: !comment.trailing,
                    text: format!(" {}", comment.text),
                })
                .collect();
        });
        let output = emit(&input);

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

/// One captured comment, classified: its start offset, verbatim text, and whether source content precedes it on its own line (trailing) or not (leading).
struct Comment {
    start: usize,
    text: String,
    trailing: bool,
}

thread_local! {
    /// The comments this run owes the renderer, by the offset each was written at — ascending, since `classify` walks the parse's own ascending spans.
    ///
    /// Present only while [`Formatted::from_source`] renders, so `Display` printing never carries any and renders exactly as it always did.
    static OWED: RefCell<Vec<Owed>> = const { RefCell::new(Vec::new()) };
}

fn classify(source: &str, spans: Vec<Span>) -> Vec<Comment> {
    spans
        .into_iter()
        .map(|span| {
            let line_start = source[..span.start]
                .rfind('\n')
                .map(|index| index + 1)
                .unwrap_or(0);
            Comment {
                start: span.start,
                text: source[span.start..span.end].to_string(),
                trailing: source[line_start..span.start]
                    .chars()
                    .any(|char| !char.is_whitespace()),
            }
        })
        .collect()
}

/// Render one document at the canonical width and indent.
fn render(printer: Printer) -> String {
    struct Within(RefCell<Option<Printer>>);
    impl fmt::Display for Within {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let printer = self.0.borrow_mut().take().expect("rendered once");
            let owed = OWED.with(|owed| std::mem::take(&mut *owed.borrow_mut()));

            run_printer_placing(printer, formatter, INDENT, WIDTH, owed)
        }
    }
    Within(RefCell::new(Some(printer))).to_string()
}

/// The parsed file as one document: items separated by exactly one blank line — except consecutive `use` declarations and a `use` directly following a `mod`, which stack with none, as the corpus writes its import heads and its `mod X; use X/{…}` pairs — and the tail last.
///
/// **One document for the whole file, rendered once.** Every comment is placed by that single run, which is what lets a comment between two items be placed at all: rendering each item separately gave each its own renderer, and a comment belonging to neither had to be spliced in as text by this function. Nothing here handles comments now; it marks where each item begins and lets the renderer do the rest.
fn emit(input: &FormatInput) -> String {
    let mut parts: Vec<Printer> = Vec::new();
    let mut previous: Option<&TopItem> = None;

    for (item, span) in input.module.items.iter().zip(&input.item_spans) {
        match previous {
            None => {}
            Some(TopItem::Use(_) | TopItem::Mod(_)) if matches!(item, TopItem::Use(_)) => {
                parts.push(hard_line());
            }
            Some(_) => parts.extend([hard_line(), hard_line()]),
        }

        // Marked at both ends, as a term is: the start is where a comment leading the item stops leading whatever came before, and the end is what makes a comment written after the item's own `;` owed before the break that would carry it to the next item.
        parts.push(begins(span.start));
        parts.push(print_top_item(item.clone()));
        parts.push(reaches(span.end));
        previous = Some(item);
    }

    if let Some(tail) = &input.tail {
        if previous.is_some() {
            parts.extend([hard_line(), hard_line()]);
        }
        parts.push(print_term(tail.clone()));
    }

    format!("{}\n", render(flat(parts)))
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
mod tests;
