use super::*;

fn anchor(name: &str) -> Anchor {
    Anchor::of(Anchored::Declaration(Qualifier::from([name])))
}

/// The span of `needle`'s first occurrence in `source`.
fn span_of(source: &Arc<Source>, needle: &str) -> Span {
    let start = source
        .text
        .find(needle)
        .expect("the fixture's text holds what it is asked for");

    Span::new(source.clone(), start, start + needle.len())
}

fn text_of(span: &Span) -> &str {
    &span.source.text[span.start..span.end]
}

/// An extent is taken in its declaration's text and resolved back to the same bytes.
#[test]
fn an_extent_resolves_to_the_text_it_was_taken_from() {
    let source = Source::inline("let first = 1;\nlet second = first;\n");
    let mut anchors = Anchors::default();
    let within = anchors.site(anchor("second"), &span_of(&source, "let second = first;"));

    let extent = within
        .extent(&span_of(&source, "first;"))
        .expect("the span lies in the declaration");

    assert_eq!(extent.start, 13);
    assert_eq!(
        anchors.resolve(&extent).as_ref().map(text_of),
        Some("first;")
    );
}

/// A declaration an edit above it moves is written where it now stands: its extents say nothing of where its text begins, so they resolve against the later text unchanged.
#[test]
fn an_extent_follows_its_declaration_to_where_an_edit_moved_it() {
    let before = Source::inline("let first = 1;\nlet second = first;\n");
    let after = Source::inline("let first = 1;\n\nlet inserted = 0;\nlet second = first;\n");
    let mut earlier = Anchors::default();
    let extent = earlier
        .site(anchor("second"), &span_of(&before, "let second = first;"))
        .extent(&span_of(&before, "= first"))
        .expect("the span lies in the declaration");

    let mut later = Anchors::default();
    later.site(anchor("second"), &span_of(&after, "let second = first;"));
    let resolved = later.resolve(&extent).expect("the anchor is sited");

    assert_eq!(text_of(&resolved), "= first");
    assert_eq!(resolved.line_column(), (4, 12));
}

/// An extent taken in a text its declaration no longer has resolves to the declaration, never to offsets that lay on the earlier text.
///
/// Mutation-checked against the stamp: resolved by its offsets alone, the extent lands on `= 0; -`.
#[test]
fn an_extent_of_a_changed_text_resolves_to_the_declaration() {
    let before = Source::inline("let second = first;\n");
    let after = Source::inline("let second   = 0; -- was first\n");
    let mut earlier = Anchors::default();
    let extent = earlier
        .site(anchor("second"), &span_of(&before, "let second = first;"))
        .extent(&span_of(&before, "first;"))
        .expect("the span lies in the declaration");

    let mut later = Anchors::default();
    later.site(anchor("second"), &span_of(&after, "let second   = 0;"));

    assert_eq!(
        later.resolve(&extent).as_ref().map(text_of),
        Some("let second   = 0;"),
    );
}

/// A span is given an extent only in the text that holds it, and an anchor no table holds resolves to nothing.
#[test]
fn a_span_outside_the_anchor_has_no_extent_and_an_unsited_anchor_no_span() {
    let source = Source::inline("let first = 1;\nlet second = first;\n");
    let elsewhere = Source::inline("let first = 1;\nlet second = first;\n");
    let mut anchors = Anchors::default();
    let within = anchors.site(anchor("second"), &span_of(&source, "let second = first;"));

    assert_eq!(within.extent(&span_of(&source, "let first")), None);
    assert_eq!(within.extent(&span_of(&elsewhere, "first;")), None);

    let extent = within
        .extent(&span_of(&source, "second"))
        .expect("the span lies in the declaration");
    assert!(Anchors::default().resolve(&extent).is_none());
}

/// Two anchors of one declaration are one allocation, and anchors order by what they anchor.
#[test]
fn an_anchor_is_one_allocation_for_what_it_anchors() {
    assert_eq!(anchor("same"), anchor("same"));
    assert!(ptr::eq(
        anchor("same").anchored(),
        anchor("same").anchored()
    ));
    assert!(anchor("a") < anchor("b"));
    assert_ne!(anchor("same"), Anchor::of(Anchored::Entry));
}

/// A module's unnamed declarations share an anchor and are told apart by their texts: each resolves to its own, wherever the others stand, and one whose text is gone resolves to nothing, there being no one declaration to stand for it.
#[test]
fn an_unnamed_declaration_is_found_by_its_text() {
    let unnamed = Anchor::of(Anchored::Unnamed(Qualifier::from(["shapes"])));
    let before = Source::inline("satisfy Show(Nat) = one;\nsatisfy Show(Bool) = two;\n");
    let after = Source::inline("satisfy Show(Bool) = two;\nsatisfy Show(Nat) = three;\n");
    let mut earlier = Anchors::default();
    let nat = earlier
        .site(unnamed, &span_of(&before, "satisfy Show(Nat) = one;"))
        .extent(&span_of(&before, "one"))
        .expect("the span lies in the declaration");
    let bool = earlier
        .site(unnamed, &span_of(&before, "satisfy Show(Bool) = two;"))
        .extent(&span_of(&before, "two"))
        .expect("the span lies in the declaration");

    let mut later = Anchors::default();
    later.site(unnamed, &span_of(&after, "satisfy Show(Bool) = two;"));
    later.site(unnamed, &span_of(&after, "satisfy Show(Nat) = three;"));

    let resolved = later.resolve(&bool).expect("the text still stands");
    assert_eq!(text_of(&resolved), "two");
    assert_eq!(resolved.line_column(), (1, 22));
    assert!(later.resolve(&nat).is_none());
}
