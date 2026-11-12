use super::*;

/// The line of `span`'s pane, and its marks behind the padding that stands them under what they mark.
fn paned(span: &Span) -> (String, String) {
    let pane = span.pane();

    (pane.line, format!("{}{}", pane.indent, pane.marks))
}

/// A pane's line and its padded marks, as [`paned`] gives them.
fn paned_as(line: &str, marks: &str) -> (String, String) {
    (line.to_string(), marks.to_string())
}

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
    assert_eq!(paned(&span), paned_as("sécond line", "       ^^^^"));

    // A span covering the two-byte `é` is one column wide — one caret, one space of padding.
    let offset = source.text.find('é').unwrap();
    let span = Span::new(source, offset, offset + 'é'.len_utf8());
    assert_eq!(paned(&span), paned_as("sécond line", " ^"));
}

// A wide character takes two columns on a terminal and a combining mark none, so the caret stands under what it underlines: after `日本 ` the padding is five spaces, an ideograph is two carets, and an `e` with its accent written as two scalars pads as the one column it is drawn in.
#[test]
fn caret_stands_under_wide_characters_and_combined_marks() {
    let source = Source::inline("日本 = e\u{301}x\n");

    let offset = source.text.find('=').unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 1);
    assert_eq!(paned(&span), paned_as("日本 = e\u{301}x", "     ^"));

    let offset = source.text.find('本').unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + '本'.len_utf8());
    assert_eq!(paned(&span), paned_as("日本 = e\u{301}x", "  ^^"));

    let offset = source.text.find('x').unwrap();
    let span = Span::new(source, offset, offset + 1);
    assert_eq!(paned(&span), paned_as("日本 = e\u{301}x", "        ^"));
}

#[test]
fn caret_padding_reproduces_the_tabs_the_printed_line_keeps() {
    let source = Source::inline("\t\tvalue\n");

    // The line is printed verbatim, so the padding carries the same two tabs rather than two spaces: a space each would move the caret one column where the text moves eight.
    let offset = source.text.find("value").unwrap();
    let span = Span::new(Arc::clone(&source), offset, offset + 5);
    assert_eq!(paned(&span), paned_as("\t\tvalue", "\t\t^^^^^"));
}

/// A report is a block: what it says, what that rests on along a trunk, each further place closed by what stands there, and its own place closing it under the word of what it is, an empty line of the trunk above and below each pane of code.
#[test]
fn a_report_is_a_block_closed_by_its_own_place() {
    let source = Source::labelled("lib.crs", "let a = 1;\nlet a = 2;\n");
    let first = Span::new(Arc::clone(&source), 4, 5);
    let second = Span::new(source, 15, 16);

    let report = Report::at(second, "`a` is already declared")
        .stating("a name is declared once in its module")
        .noting(first, "first declared here");

    assert_eq!(
        report.rendered(Kind::Error, &Style::plain()),
        concat!(
            "`a` is already declared\n",
            "│  a name is declared once in its module\n",
            "│\n",
            "│  1 │ let a = 1;\n",
            "│    │     ^\n",
            "│\n",
            "├─ first declared here; lib.crs:1:5\n",
            "│\n",
            "│  2 │ let a = 2;\n",
            "│    │     ^\n",
            "│\n",
            "╰─ [error] lib.crs:2:5",
        )
    );
}

/// Every pane of one report shares a gutter: a line's number ends in the column the widest ends in, so the code of each begins in one column.
#[test]
fn the_panes_of_a_report_share_one_gutter() {
    let text = format!("{}let a = b;\nlet b = a;\n", "\n".repeat(8));
    let source = Source::labelled("lib.crs", text.clone());
    let ninth = text.find("a =").unwrap();
    let tenth = text.find("b =").unwrap();
    let report = Report::at(
        Span::new(Arc::clone(&source), ninth, ninth + 1),
        "`a` and `b` need one another",
    )
    .noting(Span::new(source, tenth, tenth + 1), "`b` is declared here");

    assert_eq!(
        report.rendered(Kind::Error, &Style::plain()),
        concat!(
            "`a` and `b` need one another\n",
            "│\n",
            "│  10 │ let b = a;\n",
            "│     │     ^\n",
            "│\n",
            "├─ `b` is declared here; lib.crs:10:5\n",
            "│\n",
            "│   9 │ let a = b;\n",
            "│     │     ^\n",
            "│\n",
            "╰─ [error] lib.crs:9:5",
        )
    );
}

/// A report about no place closes on its word alone, and one about text no file holds on the line and the column.
#[test]
fn a_report_closes_on_what_it_has_of_a_place() {
    assert_eq!(
        Report::unlocated("unused dependency").rendered(Kind::Lint, &Style::plain()),
        "unused dependency\n╰─ [lint]"
    );

    let span = Span::new(Source::inline("x\n"), 0, 1);
    assert_eq!(
        Report::at(span, "`x` is not in scope").rendered(Kind::Error, &Style::plain()),
        "`x` is not in scope\n│\n│  1 │ x\n│    │ ^\n│\n╰─ [error] 1:1"
    );
}

/// Colour is a layer: what a reader who sees it is written, stripped of its codes, is what one who does not is written, and only what went wrong is painted as trouble.
#[test]
fn a_painted_report_stripped_of_its_codes_is_the_plain_one() {
    let stripped = |text: &str| {
        let mut plain = String::new();
        let mut rest = text;
        while let Some((before, coded)) = rest.split_once("\x1b[") {
            plain.push_str(before);
            rest = coded.split_once('m').map_or("", |(_, after)| after);
        }
        plain + rest
    };
    let painted = Style {
        here: None,
        painted: true,
    };
    let report = Report::at(
        Span::new(Source::labelled("lib.crs", "let a = 2;\n"), 4, 5),
        "`a` is already declared",
    )
    .stating("delete one");

    for kind in [Kind::Error, Kind::Goal, Kind::Lint] {
        let seen = report.rendered(kind, &painted);
        assert_eq!(stripped(&seen), report.rendered(kind, &Style::plain()));
        assert_eq!(
            seen.contains("\x1b[31m"),
            kind == Kind::Error,
            "{kind:?}: {seen:?}"
        );
    }
}

#[test]
fn a_line_within_the_width_is_quoted_whole() {
    let line = "x".repeat(LINE_WIDTH);
    let source = Source::inline(format!("{line}\n"));
    let span = Span::new(Arc::clone(&source), 50, 51);

    let (quoted, _) = paned(&span);
    assert_eq!(quoted, line);
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
        paned(&span),
        (shown, format!("{}^^^^", " ".repeat(1 + before)))
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
        paned(&span),
        (shown, format!("{}^^^", " ".repeat(1 + LINE_WIDTH - 3)))
    );
}

#[test]
fn a_span_wider_than_the_window_is_underlined_to_its_edge() {
    let line = "c".repeat(1_000);
    let source = Source::inline(line);
    let span = Span::new(Arc::clone(&source), 0, 1_000);

    let (_, marks) = paned(&span);
    assert_eq!(marks, "^".repeat(LINE_WIDTH));
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

/// What is said is built a line at a time: the first line begun is what is said and every one begun after it a fact, and written out as one text each fact stands beneath, indented, a fact that runs over lines keeping its own.
#[test]
fn what_is_said_is_a_line_and_the_facts_begun_after_it() {
    let mut said = Said::default();
    said.text("type ");
    said.text("mismatch");
    said.fact();
    said.text("inferred: Nat");
    said.stating("expected: List(\n  Str)");

    assert_eq!(
        said.to_string(),
        "type mismatch\n  inferred: Nat\n  expected: List(\n    Str)"
    );
    let report = Report::of(None, said);
    assert_eq!(report.headline, "type mismatch");
    assert_eq!(report.facts, ["inferred: Nat", "expected: List(\n  Str)"]);
    assert_eq!(
        report.rendered(Kind::Error, &Style::plain()),
        "type mismatch\n│  inferred: Nat\n│  expected: List(\n│    Str)\n╰─ [error]"
    );
}
