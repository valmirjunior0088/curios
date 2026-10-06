//! The programs this tree ships lint clean and are written as `curios format` writes them: every instrument under `programs/`, loose as `curios lint` places a file no unit declares. A lint is exact and always on, so a finding here is fixed in the program rather than excused.

use {
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::{Formatted, Overlay},
    curios_wonder::{Origin, Severity, Subject, diagnostics},
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

fn workspace() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("the crate sits in the workspace")
        .to_path_buf()
}

/// Every `.crs` under `directory`, one level of subdirectory included — the layout `programs/README.md` states: a bare file is an instrument, a directory carries one program in every spelling.
fn programs(directory: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    for entry in fs::read_dir(directory).expect("the directory is readable") {
        let path = entry.expect("the entry is readable").path();
        if path.is_dir() {
            found.extend(programs(&path));
        } else if path.extension().is_some_and(|extension| extension == "crs") {
            found.push(path);
        }
    }
    found.sort();
    found
}

/// The lints `subject` reports, rendered; an error is a lint here too, since a program that does not compile cannot be said to lint clean.
fn findings(subject: Subject) -> Vec<String> {
    diagnostics(DEFAULT_STEP_BUDGET, subject, &Overlay::default(), None)
        .into_iter()
        .filter(|diagnostic| matches!(diagnostic.severity, Severity::Lint | Severity::Error))
        .map(|diagnostic| diagnostic.render())
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
            unlinked: None,
        }));
    }
    assert!(
        wrong.is_empty(),
        "{} findings:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}

/// **The programs are written in the canonical form `curios format` produces**, as `curios-prelude-archive`'s `every_authored_source_is_canonically_formatted` holds `/std`: a tree nothing holds drifts, and a drifted file hands whoever formats it a diff that says nothing about their own change.
#[test]
fn every_program_source_is_canonically_formatted() {
    let mut wrong = Vec::new();

    for path in programs(&workspace().join("programs")) {
        match Formatted::from_path(&path) {
            Ok(Formatted::Unchanged(_)) => {}
            Ok(Formatted::Changed(_)) => wrong.push(format!("{}: not canonical", path.display())),
            Err(refusal) => wrong.push(format!("{}: {refusal}", path.display())),
        }
    }

    assert!(
        wrong.is_empty(),
        "{} of these sources are not as `curios format` writes them:\n  {}\n\nrun `cargo run --package curios -- format <file>` on each",
        wrong.len(),
        wrong.join("\n  ")
    );
}
