//! The programs this tree ships lint clean and are written as `curios format` writes them: every instrument under `programs/`, loose as `curios lint` places a file no unit declares. A lint is exact and always on, so a finding here is fixed in the program rather than excused.

use {
    super::test_support::*,
    curios_common::Style,
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    curios_surface::{Formatted, Width},
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
        }));
    }
    assert!(
        wrong.is_empty(),
        "{} findings:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}

/// **The programs are written in the canonical form `curios format` produces**, as `curios-prelude`'s `every_source_is_canonically_formatted` holds `/std` and its corpus: a tree nothing holds drifts, and a drifted file hands whoever formats it a diff that says nothing about their own change.
#[test]
fn every_program_source_is_canonically_formatted() {
    let mut wrong = Vec::new();

    for path in programs(&workspace().join("programs")) {
        match Formatted::from_path(&path, Width::default()) {
            Ok(Formatted::Unchanged(_)) => {}
            Ok(Formatted::Changed(_)) => wrong.push(format!("{}: not canonical", path.display())),
            Err(refusal) => wrong.push(format!(
                "{}: {refusal}",
                path.display(),
                refusal = refusal.render()
            )),
        }
    }

    assert!(
        wrong.is_empty(),
        "{} of these sources are not as `curios format` writes them:\n  {}\n\nrun `cargo run --package curios -- format <file>` on each",
        wrong.len(),
        wrong.join("\n  ")
    );
}
