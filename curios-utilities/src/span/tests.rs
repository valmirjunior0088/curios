use super::*;

#[test]
fn line_column_is_one_based_and_counts_scalars() {
    let source = Source::inline("first\nsécond line\n");

    // `l` of "line": line 2, after "sécond " — seven scalars in, so column 8 despite the two-byte `é`.
    let offset = source.text.find("line").unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 4);
    assert_eq!(span.line_column(), (2, 8));

    // The very first byte.
    let span = Span::new(source, 0, 1);
    assert_eq!(span.line_column(), (1, 1));
}

#[test]
fn an_offset_is_found_by_the_line_and_column_it_is_named_by() {
    let source = Source::inline("first\nsécond line\n\nlast");

    for offset in (0..=source.text.len()).filter(|offset| source.text.is_char_boundary(*offset)) {
        let (line, column) = Span::new(Arc::clone(&source), offset, offset).line_column();
        assert_eq!(source.offset_of(line, column), Some(offset));
    }

    // A column past a line's end, a line past the last and a count from zero name nothing.
    assert_eq!(source.offset_of(1, 7), None);
    assert_eq!(source.offset_of(5, 1), None);
    assert_eq!(source.offset_of(0, 1), None);
    assert_eq!(source.offset_of(1, 0), None);
}

#[test]
fn caret_aligns_by_columns_on_non_ascii_lines() {
    let source = Source::inline("sécond line\n");

    // "line" starts at byte 8 but after seven columns — seven spaces of padding, four carets.
    let offset = source.text.find("line").unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 4);
    assert_eq!(
        span.render_snippet(),
        "    1 | sécond line\n      |        ^^^^"
    );

    // A span covering the two-byte `é` is one column wide — one caret, one space of padding.
    let offset = source.text.find('é').unwrap();
    let span = Span::new(source, offset, offset + 'é'.len_utf8());
    assert_eq!(span.render_snippet(), "    1 | sécond line\n      |  ^");
}

// A wide character takes two columns on a terminal and a combining mark none, so the caret stands under what it underlines: after `日本 ` the padding is five spaces, an ideograph is two carets, and an `e` with its accent written as two scalars pads as the one column it is drawn in.
#[test]
fn caret_stands_under_wide_characters_and_combined_marks() {
    let source = Source::inline("日本 = e\u{301}x\n");

    let offset = source.text.find('=').unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 1);
    assert_eq!(
        span.render_snippet(),
        "    1 | 日本 = e\u{301}x\n      |      ^"
    );

    let offset = source.text.find('本').unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + '本'.len_utf8());
    assert_eq!(
        span.render_snippet(),
        "    1 | 日本 = e\u{301}x\n      |   ^^"
    );

    let offset = source.text.find('x').unwrap();
    let span = Span::new(source, offset, offset + 1);
    assert_eq!(
        span.render_snippet(),
        "    1 | 日本 = e\u{301}x\n      |         ^"
    );
}

#[test]
fn caret_padding_reproduces_the_tabs_the_printed_line_keeps() {
    let source = Source::inline("\t\tvalue\n");

    // The line is printed verbatim, so the padding carries the same two tabs rather than two spaces: a space each would move the caret one column where the text moves eight.
    let offset = source.text.find("value").unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 5);
    assert_eq!(
        span.render_snippet(),
        "    1 | \t\tvalue\n      | \t\t^^^^^"
    );
}

#[test]
fn a_note_is_rendered_under_the_report_as_its_line_over_its_own_snippet() {
    let source = Source::inline("let a = 1;\nlet a = 2;\n");
    let first = Span::new(Arc::clone(&source), 4, 5);
    let second = Span::new(source, 15, 16);

    let report = Report::at(second, "`a` is already declared").noting(first, "first declared here");

    // The note's line is indented as a detail of the message is, so it reads as part of the report above it and not as a report of its own.
    assert_eq!(
        report.render(),
        "`a` is already declared\n\n    2 | let a = 2;\n      |     ^\n\n  first declared here\n\n    1 | let a = 1;\n      |     ^"
    );
}

#[test]
fn a_line_within_the_width_is_quoted_whole() {
    let line = "x".repeat(LINE_WIDTH);
    let source = Source::inline(format!("{line}\n"));
    let span = Span::new(Arc::clone(&source), 50, 51);

    let snippet = span.render_snippet();
    assert!(snippet.contains(&line), "{snippet}");
    assert!(!snippet.contains('…'), "{snippet}");
}

#[test]
fn a_line_past_the_width_is_quoted_as_a_window_around_its_mark() {
    let line = format!("{}mark{}", "a".repeat(500), "b".repeat(500));
    let source = Source::inline(format!("{line}\n"));
    let offset = line.find("mark").unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 4);

    // A third of the window stands before the mark, the rest after it, and each cut end is marked; the caret stands under the mark as the window draws it.
    let before = LINE_WIDTH / 3;
    let shown = format!(
        "…{}mark{}…",
        "a".repeat(before),
        "b".repeat(LINE_WIDTH - before - 4)
    );
    assert_eq!(
        span.render_snippet(),
        format!("    1 | {shown}\n      | {}^^^^", " ".repeat(1 + before))
    );
    assert_eq!(span.line_column(), (1, 501));
}

#[test]
fn a_mark_near_a_long_line_s_end_is_quoted_with_the_end() {
    let line = format!("{}end", "a".repeat(500));
    let source = Source::inline(line.clone());
    let span = Span::new(Arc::clone(&source), 500, 503);

    // Fewer scalars stand after the mark than the window holds, so the window ends where the line does, cut at its start alone.
    let shown = format!("…{}end", "a".repeat(LINE_WIDTH - 3));
    assert_eq!(
        span.render_snippet(),
        format!(
            "    1 | {shown}\n      | {}^^^",
            " ".repeat(1 + LINE_WIDTH - 3)
        )
    );
}

#[test]
fn a_span_wider_than_the_window_is_underlined_to_its_edge() {
    let line = "c".repeat(1_000);
    let source = Source::inline(line);
    let span = Span::new(Arc::clone(&source), 0, 1_000);

    let snippet = span.render_snippet();
    assert!(
        snippet.ends_with(&format!("| {}", "^".repeat(LINE_WIDTH))),
        "{snippet}"
    );
}

#[test]
fn a_place_is_named_on_every_line_of_a_text_without_a_final_break() {
    let source = Source::inline("one\ntwo\nthree");
    let last = source.text.find("three").unwrap();

    assert_eq!(
        Span::new(Arc::clone(&source), last, last).line_column(),
        (3, 1)
    );
    assert_eq!(source.offset_of(3, 6), Some(source.text.len()));
    assert_eq!(source.offset_of(4, 1), None);
}
