//! The language reference's examples check clean: every `crs` block of `documentation/syntax.md` is a whole program or module, asked about as `curios wonder -` asks about standard input, and reports nothing — no error, goal or lint. A form shown on its own, outside a program, is a `text` block, which nothing compiles.

use {
    super::test_support::*,
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    curios_wonder::{Origin, Subject, diagnostics},
    std::{fs, path::Path},
};

#[test]
fn every_crs_example_reports_nothing() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../documentation/syntax.md");
    let reference = fs::read_to_string(&path).expect("the reference is readable");
    let examples = fenced(&reference, "crs");
    assert!(!examples.is_empty(), "the reference holds examples");

    let mut wrong = Vec::new();
    for (line, example) in examples {
        let subject = Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                label: format!("the example at syntax.md:{line}"),
                text: example,
            },
            declares: None,
            unlinked: None,
        };
        let reports = diagnostics(DEFAULT_BUDGET, subject, &Overlay::default(), None);
        wrong.extend(reports.diagnostics.iter().map(|report| report.render()));
    }

    assert!(
        wrong.is_empty(),
        "{} reports:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}
