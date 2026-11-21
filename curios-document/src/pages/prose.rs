//! A documentation comment as a page shows it: paragraphs, separated in the source by an empty `---` line, each a run of text and code spans. A code span is what a pair of backticks encloses, the one piece of notation the prose grammar has; a backtick with no partner is text, so a comment is never refused for its punctuation.

/// One paragraph of a comment.
pub struct Paragraph {
    pub spans: Vec<Span>,
}

/// A run of a paragraph: text, or the inside of a pair of backticks.
pub enum Span {
    Text(String),
    Code(String),
}

/// The text of each paragraph of a comment's lines, its lines joined by a space; `None` is no comment, and so no paragraphs.
pub fn texts(lines: Option<&[String]>) -> Vec<String> {
    lines
        .unwrap_or_default()
        .split(|line| line.is_empty())
        .filter(|paragraph| !paragraph.is_empty())
        .map(|paragraph| paragraph.join(" "))
        .collect()
}

/// The paragraphs of a comment's lines ([`texts`]), each cut at its code spans.
pub fn paragraphs(lines: Option<&[String]>) -> Vec<Paragraph> {
    texts(lines)
        .iter()
        .map(|paragraph| Paragraph {
            spans: spans(paragraph),
        })
        .collect()
}

/// `text` cut at its backtick pairs. An empty pair contributes nothing, and an unpaired backtick is text.
pub fn spans(text: &str) -> Vec<Span> {
    let mut spans = Vec::new();
    let mut rest = text;

    while let Some(open) = rest.find('`') {
        let Some(close) = rest[open + 1..].find('`') else {
            break;
        };
        if open > 0 {
            spans.push(Span::Text(rest[..open].to_string()));
        }
        if close > 0 {
            spans.push(Span::Code(rest[open + 1..open + 1 + close].to_string()));
        }
        rest = &rest[open + close + 2..];
    }

    if !rest.is_empty() {
        spans.push(Span::Text(rest.to_string()));
    }

    spans
}

#[cfg(test)]
mod tests;
