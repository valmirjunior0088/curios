//! What the one-shot transport does around the engine: a file target the disk does not hold is refused before it is placed, and one fact reached by several subjects is rendered once.

use {
    crate::{Asked, Origin, Subject, rendered},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::Overlay,
    curios_utilities::test_support::Temporary,
    std::fs,
};

/// One fact is reported once, however many subjects reach it.
///
/// **A package's subjects overlap.** `wonder diagnostics` with no target asks about the library and about every executable the package declares, and every executable is compiled against that library — so without the collapse one unbound variable in it would print once per subject, three times in a package declaring two programs, and an agent walking its errors one at a time would walk the same one three times.
///
/// Two subjects over one text stand in for that overlap. What the collapse rests on is only that equal renderings are one fact, which the second half pins from the other side: two subjects saying different things still say both.
#[test]
fn one_fact_reached_by_two_subjects_is_rendered_once() {
    let asked = |text: &str| Asked {
        subject: Subject::Entry {
            units: Vec::new(),
            declares: None,
            origin: Origin::Text {
                label: "<stdin>".to_string(),
                text: text.to_string(),
            },
            unlinked: None,
        },
        store: None,
    };

    let (same, _) = rendered(
        vec![asked("/std/print(nope)"), asked("/std/print(nope)")],
        DEFAULT_STEP_BUDGET,
        &Overlay::default(),
    );
    let [one] = same.as_slice() else {
        panic!("two subjects, one fact, got {same:?}");
    };
    assert!(one.contains("`nope` is not in scope"), "{one}");

    let (different, _) = rendered(
        vec![asked("/std/print(nope)"), asked("/std/print(other)")],
        DEFAULT_STEP_BUDGET,
        &Overlay::default(),
    );
    assert_eq!(
        different.len(),
        2,
        "two subjects, two facts, got {different:?}"
    );
}

/// A file target the disk does not hold could not be asked about: the one-shot transport refuses it before membership places it, in `run`'s words, rather than answering with the read failure as a diagnostic and exit 0 — or, under a package, placing the missing file as a library module and answering about the library.
#[test]
fn a_file_target_the_disk_does_not_hold_is_refused_before_it_is_placed() {
    // Asked of a file inside a directory nothing has created yet, and then of that directory once it exists.
    let root = Temporary::new("wonder", "unheld");

    let missing = root.join("nothing.crs");
    let refusal = crate::file_target(missing.clone()).unwrap_err();
    assert!(
        refusal.starts_with(&format!("failed to read {}: ", missing.display())),
        "{refusal}"
    );

    fs::create_dir_all(&root).unwrap();
    let refusal = crate::file_target(root.to_path_buf()).unwrap_err();
    assert!(
        refusal.starts_with(&format!("failed to read {}: ", root.display())),
        "{refusal}"
    );
}

/// A place is written as a diagnostic prints one, and anything else is refused in words that show the form.
#[test]
fn a_place_is_a_line_and_a_column_each_from_one() {
    assert_eq!(
        "12:5".parse::<crate::Place>(),
        Ok(crate::Place {
            line: 12,
            column: 5
        })
    );

    for written in ["12", "0:1", "1:0", "a:b", "1:2:3", ""] {
        let refusal = written
            .parse::<crate::Place>()
            .expect_err("no line and column, each from one");
        assert!(refusal.contains("as in `12:5`"), "{refusal}");
    }
}
