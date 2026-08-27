use super::*;

#[test]
fn line_column_is_one_based_and_counts_scalars() {
    let source = Source::inline("first\nsécond line\n");

    // `l` of "line": line 2, after "sécond " — seven scalars in, so column 8 despite the two-byte `é`.
    let offset = source.text.find("line").unwrap();
    let span = Span::new(Rc::clone(&source), offset, offset + 4);
    assert_eq!(span.line_column(), (2, 8));

    // The very first byte.
    let span = Span::new(source, 0, 1);
    assert_eq!(span.line_column(), (1, 1));
}

#[test]
fn caret_aligns_by_scalar_count_on_non_ascii_lines() {
    let source = Source::inline("sécond line\n");

    // "line" starts at byte 8 but after seven scalars — seven spaces of padding, four carets.
    let offset = source.text.find("line").unwrap();
    let span = Span::new(Rc::clone(&source), offset, offset + 4);
    assert_eq!(
        span.render_snippet(),
        "    1 | sécond line\n      |        ^^^^"
    );

    // A span covering the two-byte `é` is one scalar wide — one caret, one space of padding.
    let offset = source.text.find('é').unwrap();
    let span = Span::new(source, offset, offset + 'é'.len_utf8());
    assert_eq!(span.render_snippet(), "    1 | sécond line\n      |  ^");
}
