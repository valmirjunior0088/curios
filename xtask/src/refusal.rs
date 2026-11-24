//! Why the task files cannot be run: what is wrong, and where it is written.
//!
//! **This crate draws its own refusals.** It fetches and builds everything else, so it links no crate of the compiler's ([`README.md`](../README.md)): a refusal is the message, the line it is about with a caret under what is refused, and the file with the line and the column, each counted in characters from one.

use std::{fmt::Write, ops::Range, path::PathBuf};

#[cfg(test)]
mod tests;

/// Why the task files cannot be run, about the file that says so, or the place in it where it has one.
#[derive(Debug)]
pub struct Refusal {
    /// What is wrong.
    pub headline: String,
    /// The file that says so, from the repository's root.
    pub path: PathBuf,
    /// Where in the file, where the refusal is about one place of it.
    pub place: Option<Place>,
}

/// A place in a file: the line it is on and the column it starts at, each from one, that line's text, and how many characters of it are refused.
#[derive(Debug)]
pub struct Place {
    pub line: usize,
    pub column: usize,
    pub text: String,
    pub width: usize,
}

impl Refusal {
    /// A refusal about the file at `path` as a whole.
    pub fn about(path: &str, headline: String) -> Self {
        Self {
            headline,
            path: PathBuf::from(path),
            place: None,
        }
    }

    /// A refusal about the bytes `span` of `text`, which the file at `path` holds: the line the span starts on, and of the span what stands on that line, one character at the least.
    pub fn at(path: &str, text: &str, span: Range<usize>, headline: String) -> Self {
        let mut start = span.start.min(text.len());
        while !text.is_char_boundary(start) {
            start -= 1;
        }
        let line_start = text[..start].rfind('\n').map_or(0, |at| at + 1);
        let line_end = text[start..].find('\n').map_or(text.len(), |at| start + at);
        let mut end = span.end.clamp(start, line_end);
        while !text.is_char_boundary(end) {
            end -= 1;
        }

        Self {
            headline,
            path: PathBuf::from(path),
            place: Some(Place {
                line: text[..start].matches('\n').count() + 1,
                column: text[line_start..start].chars().count() + 1,
                text: text[line_start..line_end].to_string(),
                width: text[start..end].chars().count().max(1),
            }),
        }
    }

    /// The refusal as it is printed: the message, the line it is about with a caret under what is refused, and the file with the line and the column; `painted` where what it is written to is a terminal that takes colour.
    pub fn rendered(&self, painted: bool) -> String {
        let paint = |code: &str, text: &str| match painted {
            true => format!("\u{1b}[{code}m{text}\u{1b}[0m"),
            false => text.to_string(),
        };
        let mut rendered = paint("1", &self.headline);
        rendered.push_str("\n│\n");
        let mut at = self.path.display().to_string();
        if let Some(place) = &self.place {
            let number = place.line.to_string();
            let margin = " ".repeat(number.len());
            let _ = writeln!(rendered, "│  {number} │ {}", place.text);
            let _ = writeln!(
                rendered,
                "│  {margin} │ {}{}",
                " ".repeat(place.column - 1),
                paint("31", &"^".repeat(place.width))
            );
            rendered.push_str("│\n");
            let _ = write!(at, ":{}:{}", place.line, place.column);
        }
        let _ = write!(rendered, "└─ {} {at}", paint("31", "[error]"));

        rendered
    }
}
