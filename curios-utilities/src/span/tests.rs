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
fn caret_aligns_by_scalar_count_on_non_ascii_lines() {
    let source = Source::inline("sécond line\n");

    // "line" starts at byte 8 but after seven scalars — seven spaces of padding, four carets.
    let offset = source.text.find("line").unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 4);
    assert_eq!(
        span.render_snippet(),
        "    1 | sécond line\n      |        ^^^^"
    );

    // A span covering the two-byte `é` is one scalar wide — one caret, one space of padding.
    let offset = source.text.find('é').unwrap();
    let span = Span::new(source, offset, offset + 'é'.len_utf8());
    assert_eq!(span.render_snippet(), "    1 | sécond line\n      |  ^");
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
