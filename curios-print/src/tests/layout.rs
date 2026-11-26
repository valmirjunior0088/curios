//! How a document is laid out at a width: a group flat or broken, soft and hard lines, indents, fills and flows, look-ahead, blocks, and the spaces a line keeps.

use {super::test_support::*, crate::*};

/// `a` and `b` separated by soft lines under one group — the canonical fits-or-breaks document.
fn pair() -> Printer {
    group(flat([pure("aaa"), line(), pure("bbb")]))
}

#[test]
fn without_a_width_every_group_is_flat() {
    assert_eq!(render(pair(), None), "aaa bbb");
}

#[test]
fn a_group_that_exactly_fits_stays_flat() {
    // "aaa bbb" is seven characters.
    assert_eq!(render(pair(), Some(7)), "aaa bbb");
}

#[test]
fn a_group_one_short_of_fitting_breaks() {
    assert_eq!(render(pair(), Some(6)), "aaa\nbbb");
}

#[test]
fn a_soft_line_vanishes_flat_and_breaks_broken() {
    let document = || group(flat([pure("aaa"), soft_line(), pure("bbb")]));
    assert_eq!(render(document(), Some(6)), "aaabbb");
    assert_eq!(render(document(), Some(5)), "aaa\nbbb");
}

#[test]
fn a_group_breaks_for_the_unbreakable_content_trailing_it() {
    // The group alone fits width 9 exactly, but the look-ahead sees " tail" with no break point before it — flat would overrun, so the group breaks.
    let document = |tail| flat([group(flat([pure("aaa"), line(), pure("bbb")])), pure(tail)]);
    assert_eq!(render(document(" tail"), Some(9)), "aaa\nbbb tail");
    assert_eq!(render(document(""), Some(9)), "aaa bbb");
}

#[test]
fn look_ahead_stops_at_the_next_break_opportunity() {
    // The trailing content ends this line at its own soft line (broken mode outside any fitting group), so only " t" counts against the first group's budget.
    let document = flat([
        group(flat([pure("aaa"), line(), pure("bbb")])),
        pure(" t"),
        line(),
        pure("cccccccccc"),
    ]);
    assert_eq!(render(document, Some(9)), "aaa bbb t\ncccccccccc");
}

#[test]
fn a_hard_line_forces_every_enclosing_group() {
    let document = group(flat([
        pure("aaa"),
        line(),
        group(flat([pure("bbb"), hard_line(), pure("ccc")])),
    ]));
    // Width 100 fits everything by count, but the mandatory break refuses flatness all the way up.
    assert_eq!(render(document, Some(100)), "aaa\nbbb\nccc");
}

#[test]
fn a_line_outside_any_group_breaks() {
    let document = flat([pure("aaa"), line(), pure("bbb")]);
    assert_eq!(render(document, None), "aaa\nbbb");
}

#[test]
fn a_broken_group_indents_its_continuation_lines() {
    let document = || {
        group(flat([
            pure("head("),
            indent(flat([soft_line(), pure("argument")])),
            soft_line(),
            pure(")"),
        ]))
    };
    assert_eq!(render(document(), Some(10)), "head(\n  argument\n)");
    assert_eq!(render(document(), Some(20)), "head(argument)");
}

#[test]
fn if_break_spells_per_mode_without_breaking() {
    // The broken-only trailing comma: absent flat, present broken — and never the cause of the break.
    let document = || {
        group(flat([
            pure("("),
            indent(flat([
                soft_line(),
                pure("a"),
                pure(","),
                line(),
                pure("b"),
                if_break("", ","),
            ])),
            soft_line(),
            pure(")"),
        ]))
    };
    assert_eq!(render(document(), Some(10)), "(a, b)");
    assert_eq!(render(document(), Some(3)), "(\n  a,\n  b,\n)");
}

#[test]
fn a_fill_is_measured_with_the_gaps_it_will_emit() {
    // The spaces between the names are part of the fill's width, so what follows one starts where the gaps leave off and not where the names do. Counted short, this fits at eight.
    let document = || group(flat([fill([pure("aa,"), pure("bb")]), line(), pure("cc")]));
    assert_eq!(render(document(), Some(8)), "aa, bb\ncc");
    assert_eq!(render(document(), Some(9)), "aa, bb cc");
}

#[test]
fn a_wrapping_fill_does_not_break_its_group() {
    // A fill wraps rather than overrunning, so the line ends inside it and the scan decides in favor. The alternative is the layout this rule exists to prevent: the group breaking *and* the fill wrapping, which spends a line on the delimiter and then wraps anyway.
    let document = group(flat([
        pure("("),
        soft_line(),
        fill([pure("aaa,"), pure("bbb,"), pure("ccc")]),
        pure(")"),
    ]));
    assert_eq!(render(document, Some(9)), "(aaa,\nbbb, ccc)");
}

#[test]
fn a_wrapping_flow_preserves_its_enclosing_groups_breaks() {
    let document = group(flat([
        pure("head"),
        line(),
        pure("["),
        indent(flow(["aaa,", "bbb,", "ccc"].map(|text| FillItem {
            printer: pure(text),
            compact: true,
        }))),
        pure("]"),
        line(),
        pure("longtail"),
    ]));
    assert_eq!(
        render(document, Some(10)),
        "head\n[aaa, bbb,\n  ccc]\nlongtail"
    );
}

#[test]
fn a_fills_last_item_makes_room_for_what_trails_it() {
    // The closer has no break point before it, so it shares the last item's line — and must be measured against it, or the line overruns by exactly its width.
    let document = || flat([fill([pure("aa,"), pure("bb")]), pure(");")]);
    assert_eq!(render(document(), Some(7)), "aa,\nbb);");
    assert_eq!(render(document(), Some(8)), "aa, bb);");
}

#[test]
fn a_newline_in_literal_text_ends_the_scan_within_budget() {
    // The scan only guarantees the first line, so a literal newline before the budget runs out means the group fits.
    let document = group(flat([
        pure("aaa\nlong tail beyond any width"),
        line(),
        pure("bbb"),
    ]));
    assert_eq!(
        render(document, Some(4)),
        "aaa\nlong tail beyond any width bbb"
    );
}

#[test]
fn a_blank_line_inside_an_indent_carries_no_trailing_whitespace() {
    // Indentation is emitted lazily, owed by the next line that has content, so two hard lines inside an `indent` leave no line of spaces between them. Nothing reparses differently for trailing spaces, so only a test sees them.
    let document = indent(flat([
        pure("first"),
        hard_line(),
        hard_line(),
        pure("second"),
    ]));

    assert_eq!(render(document, None), "  first\n\n  second");
}

#[test]
fn a_name_respelled_before_rendering_is_laid_out_at_its_new_width() {
    // The long spelling overruns ten columns and the short one fits: the group renders flat only because the name was respelled before anything measured it.
    let mut document = group(flat([named("/long/path/a"), line(), named("/long/path/b")]));
    let mut seen = Vec::new();
    each_named(&mut document, |text, _| {
        seen.push(text.clone());
        *text = text.trim_start_matches("/long/path/").to_string();
    });

    let (text, annotations) = render_annotated(document, 2, 10);
    assert_eq!(seen, ["/long/path/a", "/long/path/b"]);
    assert_eq!(text, "a b");
    assert_eq!(
        annotations
            .iter()
            .map(|annotation| annotation.text.as_str())
            .collect::<Vec<_>>(),
        ["a", "b"]
    );
}

#[test]
fn a_word_is_annotated_as_one_and_is_no_name() {
    let mut document = flat([word("let"), pure(" "), named("let"), pure(" = 1")]);
    let mut names = Vec::new();
    each_named(&mut document, |text, _| names.push(text.clone()));

    let (text, annotations) = render_annotated(document, 2, 80);
    assert_eq!(names, ["let"], "a word is no name to resolve");
    assert_eq!(text, "let let = 1");
    assert_eq!(
        annotations
            .iter()
            .map(|annotation| (annotation.start, annotation.end, &annotation.annotated))
            .collect::<Vec<_>>(),
        [
            (0, 3, &Annotated::Word),
            (4, 7, &Annotated::Name { written: None }),
        ]
    );
}

/// A part is reported from its first character to its last: the indentation of the line it opens on, the spaces held before it and what trails it on its line are no part of it, and a name inside it is reported as any name is.
#[test]
fn a_part_is_reported_from_its_first_character_to_its_last() {
    let document = flat([
        pure("Point {"),
        indent(flat([
            hard_line(),
            part(flat([pure("x: "), named("Nat")])),
            pure(","),
        ])),
        hard_line(),
        pure("}"),
    ]);

    let (text, annotations) = render_annotated(document, 4, 80);
    assert_eq!(text, "Point {\n    x: Nat,\n}");
    assert_eq!(
        annotations
            .iter()
            .map(|annotation| (
                &text[annotation.start..annotation.end],
                &annotation.annotated
            ))
            .collect::<Vec<_>>(),
        [
            ("Nat", &Annotated::Name { written: None }),
            ("x: Nat", &Annotated::Part),
        ]
    );
}

#[test]
fn no_line_ends_in_a_space_and_a_document_keeps_the_spaces_it_ends_with() {
    assert_eq!(
        render(flat([pure("a "), hard_line(), pure("b ")]), None),
        "a\nb "
    );
}

#[test]
fn a_block_takes_a_line_of_its_own_only_where_it_does_not_fit_flat_on_the_one_it_is_reached_on() {
    let block = || own_line_if_broken(flat([pure("#{a}"), line(), pure("b")]));

    assert_eq!(render(flat([pure("x: "), block()]), Some(80)), "x: #{a} b");
    assert_eq!(
        render(flat([pure("x: "), block()]), Some(8)),
        "x:\n  #{a}\n  b"
    );
    assert_eq!(render(block(), Some(4)), "#{a}\nb");
    assert_eq!(
        render(
            flat([
                pure("x: "),
                own_line_if_broken(flat([
                    pure("#{a}"),
                    line(),
                    pure("b"),
                    hard_line(),
                    pure("c")
                ]))
            ]),
            Some(80)
        ),
        "x:\n  #{a}\n  b\n  c"
    );
    assert_eq!(
        render(
            group(flat([pure("f("), soft_line(), block(), pure(")")])),
            Some(80)
        ),
        "f(#{a} b)"
    );
    assert_eq!(
        render(
            group(flat([pure("f("), soft_line(), block(), pure(")")])),
            Some(8)
        ),
        "f(\n#{a} b)"
    );
}

#[test]
fn a_block_that_writes_a_newline_takes_a_line_of_its_own_at_any_width() {
    let block = || own_line_if_broken(flat([pure("#{a}"), line(), pure("b\nc")]));

    assert_eq!(
        render(flat([pure("x: "), block()]), Some(80)),
        "x:\n  #{a}\n  b\n  c"
    );
    assert_eq!(
        render(flat([pure("x: "), block()]), None),
        "x:\n  #{a}\n  b\n  c"
    );
}
