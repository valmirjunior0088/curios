//! The programs this tree ships lint clean: every instrument under `programs/`, standalone as `curios lint` would place it, and every corpus unit under `tests/corpus/`, as its library. A lint is exact and always on, so a finding here is fixed in the program rather than excused.

use {
    crate::{Origin, Severity, Subject, diagnostics},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::{Overlay, RootSource},
    curios_utilities::RootKind,
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
        }));
    }
    assert!(
        wrong.is_empty(),
        "{} findings:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}

#[test]
fn every_corpus_unit_lints_clean() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("tests")
        .join("corpus");
    let mut wrong = Vec::new();
    for header in programs(&root)
        .into_iter()
        .filter(|path| path.parent() == Some(root.as_path()))
    {
        let unit = header
            .file_stem()
            .expect("a `.crs` file has a stem")
            .to_string_lossy()
            .into_owned();
        let mounted = RootSource::mounted(&unit, RootKind::Ordinary, &header, root.join(&unit));
        wrong.extend(findings(Subject::Unit {
            units: vec![mounted],
        }));
    }
    assert!(
        wrong.is_empty(),
        "{} findings:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}
