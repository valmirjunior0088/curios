//! A following run: where the comments of an [`Original`] land as the document read from it is written.

use {
    super::{Original, Owed, run_printer_placing},
    crate::{Printer, ends, fill, flat, group, hard_line, indent, line, pure, reached, soft_line},
    std::{cell::Cell, fmt},
};

/// The comments of `source` as a formatter hands them over: every `--` to its line's end, riding where code precedes it on the line.
fn comments(source: &str) -> Vec<Owed> {
    let mut found = Vec::new();
    let mut offset = 0;
    for line in source.split('\n') {
        if let Some(column) = line.find("--") {
            found.push(Owed {
                at: offset + column,
                end: offset + line.len(),
                own_line: line[..column].trim().is_empty(),
                text: line[column..].to_string(),
            });
        }
        offset += line.len() + 1;
    }

    found
}

/// `document` at `width` and an indent step of two, following `source`, under the differences a list printer allows itself: a comma it may add or leave out, and parentheses it may leave out.
fn placed(document: Printer, source: &str, width: usize) -> String {
    struct Placing<'a>(Cell<Option<Printer>>, &'a Original<'a>, usize);
    impl fmt::Display for Placing<'_> {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let printer = self.0.take().expect("rendered once");
            run_printer_placing(printer, formatter, 2, self.2, self.1)
        }
    }

    let original = Original {
        text: source,
        comments: comments(source),
        added: |char| char == ',',
        dropped: |char| matches!(char, ',' | '(' | ')'),
        closes: |char| matches!(char, ')' | ']' | '}' | ',' | ';'),
    };
    Placing(Cell::new(Some(document)), &original, width).to_string()
}

/// `f(a, b)` as a call printer builds it: flat when it fits, an argument a line when it does not.
fn call() -> Printer {
    group(flat([
        pure("f("),
        indent(flat([soft_line(), pure("a"), pure(","), line(), pure("b")])),
        pure(")"),
    ]))
}

/// `[a, b]` as a list printer builds it, its members ending in the note.
fn list() -> Printer {
    group(flat([
        pure("["),
        indent(flat([
            soft_line(),
            pure("a"),
            pure(","),
            line(),
            pure("b"),
            ends(),
        ])),
        pure("]"),
    ]))
}

#[test]
fn a_comment_before_a_closer_takes_its_line_at_the_members_indentation() {
    assert_eq!(
        placed(list(), "[\n  a,\n  b,\n  -- c\n]", 80),
        "[\n  a,\n  b\n  -- c\n]"
    );
}

#[test]
fn a_comment_riding_the_last_member_past_its_comma_keeps_the_closer_on_its_line() {
    assert_eq!(placed(list(), "[a, b, -- last\n]", 80), "[a, b] -- last");
}

#[test]
fn a_comment_in_an_empty_block_takes_a_line_inside_it() {
    let document = flat([pure("{"), indent(ends()), pure("}")]);

    assert_eq!(
        placed(document, "{\n  -- nothing yet\n}", 80),
        "{\n  -- nothing yet\n}"
    );
}

#[test]
fn a_comment_after_code_rides_the_line_its_code_lands_on() {
    assert_eq!(
        placed(call(), "f(a, -- one\n  b)", 80),
        "f(\n  a, -- one\n  b)"
    );
}

#[test]
fn closing_punctuation_slides_under_the_comment_riding_its_line() {
    let document = flat([call(), pure(";")]);

    assert_eq!(placed(document, "f(a, b -- two\n);", 80), "f(a, b); -- two");
}

#[test]
fn a_comment_on_a_line_of_its_own_stays_between_the_characters_it_stood_between() {
    assert_eq!(
        placed(call(), "f(\n  a,\n  -- lead\n  b)", 80),
        "f(\n  a,\n  -- lead\n  b)"
    );
}

#[test]
fn a_comment_where_no_line_can_begin_breaks_the_line_and_hangs_what_follows() {
    let document = flat([pure("x"), pure(": "), pure("Nat")]);

    assert_eq!(
        placed(document, "x:\n  -- about\n  Nat", 80),
        "x:\n  -- about\n  Nat"
    );
}

#[test]
fn text_that_closes_nothing_starts_a_hanging_line_after_a_riding_comment() {
    let document = flat([pure("x"), pure(": "), pure("Nat")]);

    assert_eq!(
        placed(document, "x: -- about\n  Nat", 80),
        "x: -- about\n  Nat"
    );
}

#[test]
fn a_hanging_line_ends_at_the_documents_next_newline() {
    let document = flat([
        pure("x"),
        pure(": "),
        pure("Nat"),
        pure(","),
        hard_line(),
        pure("y"),
    ]);

    assert_eq!(
        placed(document, "x: -- about\n  Nat,\ny", 80),
        "x: -- about\n  Nat,\ny"
    );
}

#[test]
fn a_comma_the_document_adds_stays_on_the_member_it_closes() {
    let document = flat([
        pure("{"),
        indent(flat([
            hard_line(),
            pure("a"),
            pure(","),
            hard_line(),
            pure("b"),
            pure(","),
        ])),
        hard_line(),
        pure("}"),
    ]);

    assert_eq!(
        placed(document, "{\n  a\n  -- next\n  b\n}", 80),
        "{\n  a,\n  -- next\n  b,\n}"
    );
}

#[test]
fn a_character_the_document_leaves_out_is_stepped_over() {
    let document = flat([pure("a"), pure(" + "), pure("b")]);

    assert_eq!(
        placed(document, "((a)) -- note\n+ b", 80),
        "a -- note\n  + b"
    );
}

#[test]
fn a_comment_behind_what_the_document_leaves_out_arrives_before_its_line_ends() {
    let document = flat([pure("(a)"), hard_line(), pure("+ b")]);

    assert_eq!(
        placed(document, "((a)) -- note\n+ b", 80),
        "(a) -- note\n+ b"
    );
}

#[test]
fn a_comma_past_what_the_document_left_out_is_the_originals_own() {
    let document = flat([pure("a"), pure(","), hard_line(), pure("b")]);

    assert_eq!(placed(document, "(a), -- one\nb", 80), "a, -- one\nb");
}

#[test]
fn a_respelled_literal_holds_the_cursor_until_the_mark_past_it() {
    let document = flat([
        pure("f("),
        reached(2),
        pure("+1500.0"),
        reached(7),
        pure(","),
        hard_line(),
        pure("b)"),
    ]);

    assert_eq!(
        placed(document, "f(1.5e3, -- note\nb)", 80),
        "f(+1500.0, -- note\nb)"
    );
}

#[test]
fn a_second_comment_owed_to_an_occupied_line_takes_the_next() {
    let document = flat([pure("a"), pure(")")]);

    assert_eq!(
        placed(document, "a -- one\n) -- two", 80),
        "a) -- one\n-- two"
    );
}

#[test]
fn a_fill_wraps_at_the_gap_a_comment_ends_the_line_in() {
    let document = flat([pure("{"), indent(fill([pure("a,"), pure("b")])), pure("}")]);

    assert_eq!(placed(document, "{a, -- one\n b}", 80), "{a, -- one\n  b}");
}

#[test]
fn a_comment_above_everything_opens_the_output() {
    assert_eq!(placed(pure("a"), "-- top\na", 80), "-- top\na");
}

#[test]
fn comments_past_the_last_character_close_the_output() {
    assert_eq!(
        placed(pure("a"), "a -- rides\n-- ends", 80),
        "a -- rides\n-- ends"
    );
}

#[test]
fn a_group_a_comment_arrives_at_the_end_of_stays_flat() {
    assert_eq!(placed(call(), "f(a, b) -- after", 80), "f(a, b) -- after");
}
