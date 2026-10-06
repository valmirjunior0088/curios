//! The library's own claims, written in Curios: each `tests/<unit>.crs` beside this file mounted as a unit, compiled once as its own test program, and every test it declares run in an instantiation of its own.
//!
//! A unit's tests share one `Fold::tests` and one `curios::to_cwasm` rather than a full compile per fixture, so the prelude-linked baseline is paid once for the whole unit, and a run of the precompiled module is milliseconds. The units stay separate for two reasons that pull the same way: cargo runs them in parallel, and a compile error costs one unit's results rather than the corpus entire.
//!
//! Nothing here reaches `curios-package`. A unit is mounted from a header and a directory directly, so the corpus needs no manifest and is not a project: it sits inside the library's directory, where no header of `/std` declares it, `wonder` and `curios test` do not reach these files, and `cargo test` is the channel, exactly as it is for the `/std` sources they exercise.

mod runner;
use runner::*;

use {
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::{Formatted, Overlay, RootSource},
    curios_utilities::RootKind,
    curios_wonder::{Severity, Subject, diagnostics},
    std::{fs, path::PathBuf},
};

/// One `#[test]` per corpus unit, and the roster [`every_corpus_unit_is_mounted`] checks the tree against, declared from one list so the two cannot disagree.
macro_rules! corpus {
    ($($unit:ident),* $(,)?) => {
        $(
            #[test]
            fn $unit() {
                run_unit(stringify!($unit));
            }
        )*

        const MOUNTED: &[&str] = &[$(stringify!($unit)),*];
    };
}

corpus! { strings, data, aggregates, numeric, flt, cli, tui, big_nat }

/// A unit header with no row in `corpus!` would be compiled by nothing and silently pass, which is the one failure mode this arrangement has that a per-fixture Rust test does not.
#[test]
fn every_corpus_unit_is_mounted() {
    let mut headers = fs::read_dir(root())
        .expect("the corpus directory is readable")
        .map(|entry| entry.expect("the entry is readable").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "crs"))
        .map(|path| {
            path.file_stem()
                .expect("a `.crs` file has a stem")
                .to_string_lossy()
                .into_owned()
        })
        .collect::<Vec<_>>();
    headers.sort();

    let mut mounted = MOUNTED
        .iter()
        .map(|unit| (*unit).to_owned())
        .collect::<Vec<_>>();
    mounted.sort();

    assert_eq!(headers, mounted);
}

/// Every `.crs` file under `directory`, at any depth, in path order.
fn sources_under(directory: PathBuf) -> Vec<PathBuf> {
    let mut sources = Vec::new();
    let mut pending = vec![directory];

    while let Some(directory) = pending.pop() {
        for entry in fs::read_dir(&directory).expect("a directory of this crate") {
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

/// **The corpus is written in the canonical form `curios format` produces**, as `curios-prelude-archive`'s `every_authored_source_is_canonically_formatted` holds `/std`: a tree nothing holds drifts, and a drifted file hands whoever formats it a diff that says nothing about their own change.
#[test]
fn every_corpus_source_is_canonically_formatted() {
    let mut wrong = Vec::new();

    for path in sources_under(root()) {
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

/// **Every corpus unit lints clean**, as its library. A lint is exact and always on, so a finding here is fixed in the unit rather than excused, and an error is one too, since a unit that does not compile cannot be said to lint clean.
#[test]
fn every_corpus_unit_lints_clean() {
    let root = root();
    let mut wrong = Vec::new();

    for &unit in MOUNTED {
        let mounted = RootSource::mounted(
            unit,
            RootKind::Ordinary,
            root.join(format!("{unit}.crs")),
            root.join(unit),
        );
        wrong.extend(
            diagnostics(
                DEFAULT_STEP_BUDGET,
                Subject::Unit {
                    units: vec![mounted],
                },
                &Overlay::default(),
                None,
            )
            .into_iter()
            .filter(|diagnostic| matches!(diagnostic.severity, Severity::Lint | Severity::Error))
            .map(|diagnostic| diagnostic.render()),
        );
    }

    assert!(
        wrong.is_empty(),
        "{} findings:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}
