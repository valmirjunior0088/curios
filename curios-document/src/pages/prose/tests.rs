//! How a comment is cut: paragraphs at empty lines, code spans at backtick pairs.

use super::*;

fn rendered(paragraph: &Paragraph) -> String {
    paragraph
        .spans
        .iter()
        .map(|span| match span {
            Span::Text(text) => text.clone(),
            Span::Code(code) => format!("<{code}>"),
        })
        .collect()
}

#[test]
fn a_comment_splits_at_empty_lines_and_backtick_pairs() {
    let lines = [
        "Whether `m` holds a value.".to_string(),
        "Decided at `some`.".to_string(),
        String::new(),
        "An empty pair `` is nothing, and an unpaired ` stays.".to_string(),
    ];
    let split = paragraphs(Some(&lines[..]));

    assert_eq!(split.len(), 2);
    assert_eq!(
        rendered(&split[0]),
        "Whether <m> holds a value. Decided at <some>."
    );
    // Pairs are read left to right, so the unpaired backtick is the one nothing follows.
    assert_eq!(
        rendered(&split[1]),
        "An empty pair  is nothing, and an unpaired ` stays."
    );
}

#[test]
fn no_comment_is_no_paragraph() {
    assert!(paragraphs(None).is_empty());
    assert!(paragraphs(Some(&[String::new()])).is_empty());
}
