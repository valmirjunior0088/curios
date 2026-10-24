//! The library's own claims, written in Curios: each `tests/<unit>.crs` beside this file mounted as a unit, compiled once as its own test program, and every test it declares run in an instantiation of its own.
//!
//! A unit's tests share one `Fold::tests` and one `curios::to_cwasm` rather than a full compile per fixture, so the prelude-linked baseline is paid once for the whole unit, and a run of the precompiled module is milliseconds. The units stay separate for two reasons that pull the same way: cargo runs them in parallel, and a compile error costs one unit's results rather than the corpus entire.
//!
//! Nothing here reaches `curios-package`. A unit is mounted from a header and a directory directly, so the corpus needs no manifest and is not a project: it sits inside the library's directory, where no header of `/std` declares it, `wonder` and `curios test` do not reach these files, and `cargo test` is the channel, exactly as it is for the `/std` sources they exercise.

mod runner;
use runner::*;

use {
    crate::std_directory,
    curios::run_wasm,
    curios_native::{ForeignBindings, MockHost},
    curios_pipeline::{DEFAULT_BUDGET, compile_with_prelude},
    curios_text::{Entrypoint, Formatted, Overlay, RootSource},
    curios_unicode::{
        east_asian_width, grapheme_break, indic_conjunct_break, is_emoji_presentation,
        is_extended_pictographic, test_support::grapheme_break_cases,
    },
    curios_utilities::{RootKind, fill},
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

corpus! { strings, data, aggregates, numeric, flt, cli, tui, http, io, unicode }

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

/// A verdict is what a test program answers, so a corpus test that exits has reported nothing and fails whatever its code: read as a verdict, an exit of `0` would pass a test that asserts nothing.
#[test]
fn a_test_that_exits_before_it_reports_fails_whatever_its_code() {
    let mut unit = RootSource::supplied();
    unit.insert_root(
        "exits",
        RootKind::Ordinary,
        "use /std/{Test, Process};\n\ntest quits_early =\n    Test/perform(() => Process/exit(@Test({}), 0));\n\ntest fails_quietly =\n    Test/perform(() => Process/exit(@Test({}), 1));\n"
            .parse()
            .expect("the unit parses"),
    );

    let (passed, failures) = outcomes("exits", unit);

    assert_eq!(passed, 0);
    assert_eq!(
        failures,
        "/exits/quits_early: exited 0\n/exits/fails_quietly: exited 1\n"
    );
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

/// **The library and its corpus are written in the canonical form `curios format` produces.**
///
/// `curios format --check` is only a guarantee where something checks it. A tree drifted from canonical form hands anyone formatting it a diff that says nothing about their own change, and hides a formatter defect — a comment placed on the wrong arm — from the one corpus large enough to show it.
///
/// Formatting is syntax-only — no prelude, no elaboration — so one walk holds every `.crs` file this crate authors, the library's modules and the corpus units alike. A failure names the file, and `curios format <file>` is the fix.
#[test]
fn every_source_is_canonically_formatted() {
    let mut wrong = Vec::new();

    for path in sources_under(std_directory()) {
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
                DEFAULT_BUDGET,
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

/// The text of a run of scalars as a Curios string literal spells it, each by its escape.
fn literal(scalars: &[u32]) -> String {
    let escaped: String = scalars
        .iter()
        .map(|scalar| format!("\\u{{{scalar:X}}}"))
        .collect();

    format!("\"{escaped}\"")
}

/// Every case of Unicode's `GraphemeBreakTest.txt`, at the version `curios-unicode` pins, segments through `/std/Str/graphemes` into the clusters the database states: one unit of one test over every case.
#[test]
fn every_conformance_case_segments_through_the_library_as_the_database_states() {
    let cases = grapheme_break_cases()
        .iter()
        .map(|case| {
            let clusters: Vec<String> = case.iter().map(|cluster| literal(cluster)).collect();
            format!("({}, [{}])", literal(&case.concat()), clusters.join(", "))
        })
        .collect::<Vec<_>>()
        .join(",\n        ");
    let mut unit = RootSource::supplied();
    unit.insert_root(
        "conformance",
        RootKind::Ordinary,
        fill!(
            r#"
            use /std/{Str, List, Test};

            let cases: List({Str, List(Str)}) =
                [
                    ${cases}];

            test every_case_segments_as_stated =
                Test/all(List/map(cases, (case) => Test/equal(Str/graphemes(case.0), case.1)));
            "#,
            cases
        )
        .parse()
        .expect("the unit parses"),
    );

    let (passed, failures) = outcomes("conformance", unit);

    assert!(failures.is_empty(), "\n{failures}");
    assert_eq!(passed, 1);
}

/// What `/std/Char` answers of each scalar read on standard input, one line a scalar.
const PROPERTIES_PROGRAM: &str = r#"
use /std/{Nat, Str, Char, Option, Io, Try, Process, Spell, print};
use /std/Io/{Reader};
use /std/Char/{GraphemeBreak, IndicConjunctBreak, EastAsianWidth};

let answer(c: Char) -> Str =
    Str/join(" ", [
        Spell/spell(GraphemeBreak/of_char(c)),
        Spell/spell(IndicConjunctBreak/of_char(c)),
        Spell/spell(EastAsianWidth/of_char(c)),
        Spell/spell(Char/is_extended_pictographic(c)),
        Spell/spell(Char/is_emoji_presentation(c))]);

let each(r: Reader(Process/Input)) -> Try(Io, Io/Error, {}) =
    let line = Reader/line_text(r)!;
    match line
    | none() => Try/pure(())
    | some(text) =>
        let shown =
            match Option/bind(Nat/of_str(text), Char/of_nat)
            | some(c) => answer(c)
            | none() => "unread"
            end;
        let _ = print(Str/concat(shown, "\n"))!;
        each(r)
    end;

let main() -> Try(Io, Io/Error, {}) =
    let r = Reader/new(Process/stdin)!;
    each(r);

main()
"#;

/// `curios-unicode`'s answer for `scalar`, spelled as [`PROPERTIES_PROGRAM`] prints `/std/Char`'s.
fn database(scalar: char) -> String {
    format!(
        "GraphemeBreak/{}() IndicConjunctBreak/{}() EastAsianWidth/{}() {} {}",
        grapheme_break(scalar).sys_name(),
        indic_conjunct_break(scalar).sys_name(),
        east_asian_width(scalar).sys_name(),
        is_extended_pictographic(scalar),
        is_emoji_presentation(scalar),
    )
}

/// `/std/Char` answers every property as `curios-unicode` does wherever a table's answer changes and just before: every run's first and last scalar and those outside it, which is where a misread of the packed runs — their order, their width, a search by halves stopping short — would show.
#[test]
fn the_library_reads_every_property_as_the_database_does_at_every_run_s_edges() {
    let mut scalars: Vec<char> = Vec::new();
    let mut previous: Option<(char, String)> = None;
    for scalar in (0..=0x10_FFFF).filter_map(char::from_u32) {
        let answer = database(scalar);
        if previous
            .as_ref()
            .is_none_or(|(_, before)| *before != answer)
        {
            if let Some((before, _)) = previous.take()
                && scalars.last() != Some(&before)
            {
                scalars.push(before);
            }
            scalars.push(scalar);
        }
        previous = Some((scalar, answer));
    }
    scalars.push('\u{10FFFF}');

    let entrypoint: Entrypoint = PROPERTIES_PROGRAM.parse().expect("the program parses");
    let (module, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .expect("the program compiles");
    let (host, io) = MockHost::builder()
        .stdin_lines(scalars.iter().map(|&scalar| u32::from(scalar).to_string()))
        .build();
    run_wasm(&module, foreigns, host, ForeignBindings::empty()).expect("the program runs");

    let printed = String::from_utf8(io.output()).expect("the program prints text");
    let lines: Vec<&str> = printed.lines().collect();
    let wrong: Vec<String> = scalars
        .iter()
        .zip(&lines)
        .filter(|&(&scalar, line)| database(scalar) != **line)
        .map(|(&scalar, line)| {
            format!(
                "U+{:04X}: {line}, where the database answers {}",
                u32::from(scalar),
                database(scalar)
            )
        })
        .collect();

    assert_eq!(lines.len(), scalars.len());
    assert!(
        wrong.is_empty(),
        "{} scalars read otherwise:\n{}",
        wrong.len(),
        wrong.join("\n")
    );
}
