//! The programs this tree ships lint clean and are written as `curios format` writes them: every instrument under `programs/`, loose as `curios lint` places a file no unit declares. A lint is exact and always on, so a finding here is fixed in the program rather than excused.

use {
    super::test_support::*,
    curios_common::Style,
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    curios_wonder::{Origin, Severity, Subject, diagnostics},
};

/// The lints `subject` reports, rendered; an error is a lint here too, since a program that does not compile cannot be said to lint clean.
fn findings(subject: Subject) -> Vec<String> {
    diagnostics(
        DEFAULT_BUDGET,
        subject,
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .diagnostics
    .into_iter()
    .filter(|diagnostic| matches!(diagnostic.severity, Severity::Lint | Severity::Error))
    .map(|diagnostic| diagnostic.rendered(&Style::plain()))
    .collect()
}

#[test]
fn every_program_lints_clean() {
    let mut wrong = Vec::new();
    for path in programs(&workspace().join("programs")) {
        wrong.extend(findings(Subject::Entry {
            units: Vec::new(),
            origin: Origin::File(path),
            declares: None,
            package: None,
        }));
    }
    assert!(
        wrong.is_empty(),
        "{} findings:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}
