//! Resynchronization past a broken item: what is skipped, where parsing resumes, what a broken item says it declared, and which reading refuses instead.

use {crate::*, curios_utilities::Source};

fn items(text: &str) -> Vec<TopItem> {
    Module::parse(&Source::inline(text)).unwrap().items
}

fn broken(item: &TopItem) -> &TopBroken {
    match item {
        TopItem::Broken(broken) => broken,
        other => panic!("not a broken item: {other:?}"),
    }
}

#[test]
fn a_broken_item_is_skipped_to_the_next_anchor() {
    let items = items("pub let a : Nat = ;\npub let b : Nat = 1;\n");

    let [first, second] = items.as_slice() else {
        panic!("two items, got {items:?}");
    };
    let first = broken(first);
    assert_eq!(first.text(), "pub let a : Nat = ;\n");
    assert!(
        first.report.message.contains("expected a term"),
        "{}",
        first.report.message
    );
    assert!(matches!(second, TopItem::Let(_)));
}

/// A line at column 0 that begins with no head word is skipped without a word: it is part of the broken text, and the next anchor is what resumes the loop.
#[test]
fn a_line_that_begins_no_item_is_skipped_silently() {
    let items = items("pub let a : Nat = ;\nnot an item\n  neither this\nlet b : Nat = 1;\n");

    let [first, second] = items.as_slice() else {
        panic!("two items, got {items:?}");
    };
    assert_eq!(
        broken(first).text(),
        "pub let a : Nat = ;\nnot an item\n  neither this\n"
    );
    assert!(matches!(second, TopItem::Let(_)));
}

/// An anchor is a candidate, not a verdict: one whose text is no item ends the loop uncommitted, exactly as it would have without recovery — here an unannotated `let`, which a module has no tail for.
#[test]
fn a_candidate_anchor_that_is_no_item_ends_the_loop_uncommitted() {
    let error = Module::parse(&Source::inline("pub let a : Nat = ;\nlet x = 1;\n")).unwrap_err();

    assert_eq!(
        error.report().span.expect("located").line_column().0,
        2,
        "reported at the candidate"
    );
}

#[test]
fn an_uncommitted_failure_still_ends_the_loop() {
    let entrypoint = "let x = 1;\nx".parse::<Entrypoint>().unwrap();

    assert!(entrypoint.module.items.is_empty());
}

#[test]
fn a_broken_item_names_what_its_head_declared() {
    let by_let = items("pub let broken : Nat = ;\n");
    assert_eq!(
        broken(&by_let[0])
            .declares
            .as_ref()
            .map(|label| label.as_str()),
        Some("broken")
    );

    let by_induct = items("induct Bad : pub Type\n| bad(x : )\nend\n");
    assert_eq!(
        broken(&by_induct[0])
            .declares
            .as_ref()
            .map(|label| label.as_str()),
        Some("Bad")
    );

    let by_nothing = items("pub let = 1;\n");
    assert!(broken(&by_nothing[0]).declares.is_none());
}

/// A failure inside an inline module body is the module's, and the module declares no name: the name the body's item read stays out of the enclosing scope.
#[test]
fn a_broken_inline_module_is_one_broken_item_declaring_nothing() {
    let items = items("mod inner\n    pub let x : Nat = ;\nend\npub let after : Nat = 1;\n");

    let [first, second] = items.as_slice() else {
        panic!("two items, got {items:?}");
    };
    assert!(broken(first).declares.is_none());
    assert!(matches!(second, TopItem::Let(_)));
}

#[test]
fn the_whole_or_nothing_reading_refuses_at_the_first_broken_item() {
    let error = "pub let a : Nat = ;\npub let b : Nat = 1;"
        .parse::<Module>()
        .unwrap_err();

    assert!(
        error.format().contains("expected a term"),
        "{}",
        error.format()
    );
    assert_eq!(
        error.report().span.expect("located").line_column().0,
        1,
        "the first item's own location"
    );
}

/// A broken last item swallows a program's tail, since a tail begins with no word an anchor could find; the report is the item's, not a term missing at the end of the input.
#[test]
fn a_broken_last_item_reports_itself_rather_than_the_missing_tail() {
    let error = Entrypoint::supplied("<stdin>", "pub let a : Nat = ;\n/std/print(\"\")\n")
        .err()
        .expect("the tail was swallowed");

    assert!(
        error.format().contains("expected a term"),
        "{}",
        error.format()
    );
    assert_eq!(
        error.report().span.expect("located").line_column().0,
        1,
        "the item's own location"
    );
}
