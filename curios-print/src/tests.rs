use {
    super::*,
    std::{cell::Cell, fmt, rc::Rc},
};

/// A document nests as deep as the term it prints, and both walks over it — printing and freeing — must survive that. Depth is not steps, so no reduction budget bounds either one; only an explicit stack does.
fn nested(depth: usize) -> Printer {
    let mut document = pure("x");
    for _ in 0..depth {
        document = indent(flat([pure("("), document, pure(")")]));
    }
    document
}

fn render(printer: Printer, width: Option<usize>) -> String {
    struct Render(Cell<Option<(Printer, Option<usize>)>>);
    impl fmt::Display for Render {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let (printer, width) = self.0.take().expect("rendered once");
            match width {
                Some(width) => run_printer_within(printer, formatter, 2, width),
                None => run_printer(printer, formatter, 2),
            }
        }
    }
    Render(Cell::new(Some((printer, width)))).to_string()
}

/// `a` and `b` separated by soft lines under one group — the canonical fits-or-breaks document.
fn pair() -> Printer {
    group(flat([pure("aaa"), line(), pure("bbb")]))
}

#[test]
fn a_deep_document_is_freed_without_recursing() {
    drop(nested(100_000));
}

#[test]
fn a_deep_document_is_printed_without_recursing() {
    struct Deep;
    impl fmt::Display for Deep {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            run_printer(nested(100_000), formatter, 2)
        }
    }

    let printed = Deep.to_string();

    assert_eq!(printed.matches('(').count(), 100_000);
    assert_eq!(printed.matches(')').count(), 100_000);
}

/// The layout variants must survive the same depth as the rest of the algebra, in all three walks: drop, unbounded print, and the measuring print. No `indent` here — a broken 100k-deep document would otherwise write quadratically many indentation spaces, and depth is what this exercises, not volume.
fn nested_groups(depth: usize) -> Printer {
    let mut document = pure("x");
    for _ in 0..depth {
        document = group(flat([pure("("), soft_line(), document, pure(")")]));
    }
    document
}

#[test]
fn a_deep_grouped_document_is_freed_without_recursing() {
    drop(nested_groups(100_000));
}

#[test]
fn a_deep_grouped_document_is_printed_and_measured_without_recursing() {
    let printed = render(nested_groups(100_000), Some(30));
    assert_eq!(printed.matches('(').count(), 100_000);
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
fn measurement_forces_each_thunk_at_most_once() {
    let forced = Rc::new(Cell::new(0usize));
    let counter = Rc::clone(&forced);
    let document = group(flat([
        pure("aaa"),
        line(),
        deferred(move || {
            counter.set(counter.get() + 1);
            pure("bbb")
        }),
    ]));

    // The scan forces the thunk to measure it; printing then consumes the materialized document rather than forcing again.
    assert_eq!(render(document, Some(7)), "aaa bbb");
    assert_eq!(forced.get(), 1);
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
fn a_line_suffix_rides_to_the_end_of_its_line() {
    let document = flat([
        pure("code"),
        line_suffix(" -- trailing"),
        pure(" more"),
        hard_line(),
        pure("next"),
    ]);
    assert_eq!(render(document, None), "code more -- trailing\nnext");
}

#[test]
fn a_line_suffix_flushes_at_document_end() {
    let document = flat([pure("code"), line_suffix(" -- last")]);
    assert_eq!(render(document, None), "code -- last");
}

#[test]
fn a_line_suffix_forces_its_group_to_break() {
    // A trailing comment means the line must end, so the group cannot stay flat however wide the width.
    let document = group(flat([pure("a"), line_suffix(" -- c"), line(), pure("b")]));
    assert_eq!(render(document, Some(100)), "a -- c\nb");
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
