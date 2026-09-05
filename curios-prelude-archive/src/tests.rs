//! What this crate's own sources must be true of, beyond elaborating.

use {
    crate::{SYNTAX, sources::authored_prelude},
    curios_text::{Formatted, prepare_prelude},
    std::{fs, path::PathBuf},
};

/// Every `.crs` file this crate authors, in the two trees it owns.
///
/// Walked rather than listed, for the reason the build script discovers its inputs rather than naming them: a module added without being registered is a mistake the Curios index catches, and one added without being formatted should not need a second list to catch it.
fn authored() -> Vec<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut sources = Vec::new();
    let mut pending = Vec::from([root.join("std"), root.join("syn")]);

    while let Some(directory) = pending.pop() {
        for entry in fs::read_dir(&directory).expect("a directory this crate owns") {
            let path = entry.expect("a readable entry").path();

            match path.is_dir() {
                true => pending.push(path),
                false if path.extension().is_some_and(|kind| kind == "crs") => sources.push(path),
                false => {}
            }
        }
    }

    sources.sort();
    sources
}

/// **The standard and syntax libraries are written in the canonical form `curios format` produces.**
///
/// The formatter is only worth having if it can be run on the corpus that motivated it, and `curios format --check` is only a guarantee where something checks it. Nothing did: 23 of these files drifted from canonical form, so anyone running the formatter over the tree got a diff of a few hundred lines and learned nothing about their own change. That is also how a formatter defect survives — the one that placed a comment on the wrong arm went unnoticed because nobody could format the corpus to see it.
///
/// Formatting is syntax-only — no prelude, no elaboration, no compiler — so this belongs in the ordinary suite rather than behind `--ignored`, and it is this crate's rather than `curios-text`'s because these are the sources this crate authors. A failure names the file, and `curios format <file>` is the fix.
#[test]
fn every_authored_source_is_canonically_formatted() {
    let mut wrong = Vec::new();

    for path in authored() {
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

/// **The standard and syntax libraries lint clean.**
///
/// The lints are exact and always on, so the honest test of them is the largest corpus in the tree: an import nothing resolves through, a binder nothing reads or a private declaration nothing reaches in `/std` is a finding to fix there, not a rule to relax. Lowering is the whole cost — the same lowering the build script pays — so this belongs in the ordinary suite. A failure renders each lint as `curios lint` would.
#[test]
fn every_authored_source_is_lint_clean() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prepared = prepare_prelude(&authored_prelude(&manifest), &SYNTAX)
        .unwrap_or_else(|error| panic!("the prelude failed to lower: {}", error.format()));
    let lints = prepared
        .lints()
        .iter()
        .map(|lint| lint.render())
        .collect::<Vec<_>>();
    assert!(
        lints.is_empty(),
        "{} lints in the prelude:\n\n{}",
        lints.len(),
        lints.join("\n\n")
    );
}
