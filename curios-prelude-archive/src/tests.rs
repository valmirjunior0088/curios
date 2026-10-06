//! What the sources this crate elaborates must be true of, beyond elaborating.

use {
    crate::with_prelude,
    curios_std::{STD_DESCRIPTION, STD_NAME, std_directory, std_source},
    curios_text::{Formatted, SYNTAX, prepare_prelude, sys_source},
    std::{fs, path::PathBuf},
};

/// Every `.crs` file `/std` is authored in, in the one tree `curios-std` holds.
///
/// Walked rather than listed, for the reason the build script discovers its inputs rather than naming them: a module added without being registered is a mistake `the_std_record_names_every_authored_source_and_no_other` catches against this walk, and one added without being formatted should not need a second list to catch it.
pub(crate) fn authored() -> Vec<PathBuf> {
    let mut sources = Vec::new();
    let mut pending = Vec::from([std_directory()]);

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

/// **The standard library is written in the canonical form `curios format` produces.**
///
/// `curios format --check` is only a guarantee where something checks it. A corpus drifted from canonical form hands anyone formatting the tree a diff that says nothing about their own change, and hides a formatter defect — a comment placed on the wrong arm — from the one corpus large enough to show it.
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

/// **The standard library lints clean.**
///
/// The lints are exact and always on, so the honest test of them is the largest corpus in the tree: an import nothing resolves through, a binder nothing reads or a private declaration nothing reaches in `/std` is a finding to fix there, not a rule to relax. A binder only a proof the elaborator wrote reads is used, which only elaboration says, so the lowering here is credited with what the build script's elaboration recorded; lowering and restoring are the whole cost, so this belongs in the ordinary suite. A failure renders each lint as `curios lint` would.
#[test]
fn every_authored_source_is_lint_clean() {
    // Both roots, in the order the build script folds them: `/std`'s imports resolve only against a lowered `/sys`, and an unresolved import is not a lint but a lowering failure.
    let mut sys = prepare_prelude(&sys_source(), &[], &SYNTAX)
        .unwrap_or_else(|error| panic!("/sys failed to lower: {}", error.format()));
    let mut std = prepare_prelude(&std_source(), &[&sys], &SYNTAX)
        .unwrap_or_else(|error| panic!("/std failed to lower: {}", error.format()));
    // Credited as a recompile credits a declaration it reuses: with what the build's elaboration of the same sources recorded as read by a proof it wrote.
    with_prelude(|units| {
        sys.credit_reused(units[0].text(), |_| true);
        std.credit_reused(units[1].text(), |_| true);
    });
    let lints = [&sys, &std]
        .iter()
        .flat_map(|prepared| prepared.lints())
        .map(|lint| lint.render())
        .collect::<Vec<_>>();
    assert!(
        lints.is_empty(),
        "{} lints in the prelude:\n\n{}",
        lints.len(),
        lints.join("\n\n")
    );
}

/// **`/std` is an ordinary package, and the two strings the archive build holds are the ones its manifest declares.**
///
/// The build script cannot read the manifest: `curios-package` would become a build prerequisite of this crate, and so of every crate that reaches the prelude, which would re-elaborate the whole standard library on every edit to a manifest parser. So the name and the description are constants there, and this is what keeps them honest — a dev-dependency, the way this crate already takes `curios-cert`, so the parser is linked by the test and by nothing that ships.
#[test]
fn the_archive_build_agrees_with_the_standard_library_manifest() {
    let manifest = std_directory();
    let curios_package::Manifest::Package(package) =
        curios_package::Manifest::from_path(&manifest.join("curios.toml"))
            .expect("the standard library's manifest parses")
    else {
        panic!("the standard library declares a package, not an umbrella");
    };

    assert_eq!(package.name, STD_NAME);
    assert_eq!(package.description.as_deref(), Some(STD_DESCRIPTION));
    assert!(
        manifest.join("lib.crs").is_file(),
        "a package's library header sits beside its manifest"
    );
}
