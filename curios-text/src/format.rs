//! The canonical formatter behind `curios format`: parse with comments, weave them through width-aware rendering, verify, emit.
//!
//! The laws (see `documentation/compiler/10_FORMATTER_SPEC.md` while it lives): formatting never changes a program — the output is reparsed and compared structurally, and a mismatch refuses to emit; every captured comment appears in the output exactly once — the verifier recounts them; the result is deterministic and idempotent; and nothing is ever reordered, which is also what makes the weave correct: printing follows source order, so each parsed node may simply *claim* every not-yet-claimed comment written before its own start.
//!
//! Comments are classified once, from the source text: a comment with source content earlier on its own line is *trailing* — it rides that line's end via the printer's line-suffix channel — and every other comment *leads* the next element, breaking onto its own line. A trailing comment's suffix fails the fits scan, so the group it lands in always breaks: the comment-is-a-hard-break law needs no special-casing. The weave is a thread-local present only during a `format` run; `Display` paths never consult it, so ordinary printing is untouched.

use {
    super::{FormatInput, TopItem, parse_for_format},
    crate::print::{print_term, print_top_item},
    curios_base::{
        Source, Span,
        printer::{Printer, run_printer_within},
    },
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

        WEAVER.with(|weaver| *weaver.borrow_mut() = Some(comments));
        let output = emit(&input);
        WEAVER.with(|weaver| *weaver.borrow_mut() = None);

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

    /// The canonical text, either way.
    pub fn into_text(self) -> String {
        match self {
            Formatted::Unchanged(text) | Formatted::Changed(text) => text,
        }
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
    /// The active weave: comments not yet claimed, in offset order. Present only while [`Formatted::from_source`] renders, so `Display` printing never consults it.
    static WEAVER: RefCell<Option<Vec<Comment>>> = const { RefCell::new(None) };
}

/// Claim every unclaimed comment starting before `offset`, in source order, as `(text, trailing)` pairs. Empty outside a format run — the printer calls this unconditionally and ordinary printing pays one thread-local read.
pub(crate) fn claim_comments_before(offset: usize) -> Vec<(String, bool)> {
    WEAVER.with(|weaver| match &mut *weaver.borrow_mut() {
        Some(comments) => {
            let split = comments.partition_point(|comment| comment.start < offset);
            comments
                .drain(..split)
                .map(|comment| (comment.text, comment.trailing))
                .collect()
        }
        None => Vec::new(),
    })
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
            run_printer_within(printer, formatter, INDENT, WIDTH)
        }
    }
    Within(RefCell::new(Some(printer))).to_string()
}

/// Render the parsed file: items separated by exactly one blank line — except consecutive `use` declarations and a `use` directly following a `mod`, which stack with none, as the corpus writes its import heads and its `mod X; use X/{…}` pairs — each preceded by its leading comments, trailing comments riding their lines, dangling comments closing the file. Element-boundary comments are claimed by this driver — a trailing one appends to the previous element's final line, a leading one gets its own lines before the next element — while comments *interior* to an element are claimed during its document's build (an interior trailing comment surfaces at the following break, one line later than written: conserved and valid, a known coarseness).
fn emit(input: &FormatInput) -> String {
    let mut out = String::new();
    let mut previous: Option<&TopItem> = None;

    // Split a boundary claim: trailing comments ride the previous element's last line, leading ones return as the next element's preamble.
    let boundary = |out: &mut String, upto: usize| -> Vec<String> {
        let mut lead = Vec::new();
        for (text, trailing) in claim_comments_before(upto) {
            if trailing {
                out.push(' ');
                out.push_str(&text);
            } else {
                lead.push(text);
            }
        }
        lead
    };

    for (item, span) in input.module.items.iter().zip(&input.item_spans) {
        // Claimed before the item's document builds, or the item's first term would weave these into its own middle.
        let lead = boundary(&mut out, span.start);
        match previous {
            None => {}
            Some(TopItem::Use(_) | TopItem::Mod(_)) if matches!(item, TopItem::Use(_)) => {
                out.push('\n')
            }
            Some(_) => out.push_str("\n\n"),
        }
        for text in lead {
            out.push_str(&text);
            out.push('\n');
        }
        out.push_str(&render(print_top_item(item.clone())));
        previous = Some(item);
    }

    if let Some(tail) = &input.tail {
        let upto = tail.span().map(|span| span.start).unwrap_or(usize::MAX);
        let lead = boundary(&mut out, upto);
        if previous.is_some() {
            out.push_str("\n\n");
        }
        for text in lead {
            out.push_str(&text);
            out.push('\n');
        }
        out.push_str(&render(print_term(tail.clone())));
    }

    for (text, trailing) in claim_comments_before(usize::MAX) {
        if trailing {
            out.push(' ');
            out.push_str(&text);
        } else {
            out.push_str("\n\n");
            out.push_str(&text);
        }
    }

    out.push('\n');
    out
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
