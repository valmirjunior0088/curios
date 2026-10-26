//! The library's own claims, written in Curios as the `tests` modules of `/std`, run as `curios test` runs a package's: the package at [`std_directory`] tested through `curios::test_subjects`, one Rust test per module of tests narrowed to it, each test run in an instantiation of its own under the mock host. Beside them, the claims too large to write by hand: every Unicode conformance case and every property run's edges, as programs this file writes.
//!
//! One Rust test per module of tests, because nextest runs each in a process of its own: the modules' back ends run on every core, and a failure names the module. Each process compiles the tested `/std` over the archived one, so only its tests are elaborated, and files what it compiled into one store under the build's scratch directory, which a rerun of an unchanged build takes whole.

use {
    curios::{TestSubject, run_wasm, test_subjects},
    curios_native::{ForeignBindings, MockHost, TestEnd},
    curios_package::{Selection, Spelling},
    curios_pipeline::{DEFAULT_BUDGET, compile_with_prelude},
    curios_prelude::std_directory,
    curios_text::{Entrypoint, Formatted, Overlay, RootSource},
    curios_unicode::{
        east_asian_width, grapheme_break, indic_conjunct_break, is_emoji_presentation,
        is_extended_pictographic, test_support::grapheme_break_cases,
    },
    curios_utilities::{Qualifier, fill},
    curios_verdicts::Verdicts,
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

/// One `#[test]` per module of tests `/std` declares, each by the subject it stands in — none for the root's own — and the roster [`every_test_stands_in_a_rostered_module`] holds the library against, declared from one list so the two cannot disagree.
macro_rules! claims {
    ($($name:ident => [$($subject:literal)?]),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                run_module(&tests_of(&[$($subject)?]));
            }
        )*

        const ROSTER: &[&[&str]] = &[$(&[$($subject)?]),*];
    };
}

claims! {
    root => [],
    str => ["Str"],
    char => ["Char"],
    json => ["Json"],
    html => ["Html"],
    parse => ["Parse"],
    map => ["Map"],
    path => ["Path"],
    fmt => ["Fmt"],
    time => ["time"],
    nat => ["Nat"],
    int => ["Int"],
    flt => ["Flt"],
    bytes => ["Bytes"],
    list => ["List"],
    vec => ["Vec"],
    io => ["Io"],
    program => ["Program"],
    cli => ["Cli"],
    http => ["http"],
    tui => ["Tui"],
}

/// The module of tests `subject` declares, `/std/<subject>/tests`, or the root's own, `/std/tests`, for none.
fn tests_of(subject: &[&str]) -> Qualifier {
    Qualifier::from(
        ["std"]
            .into_iter()
            .chain(subject.iter().copied())
            .chain(["tests"]),
    )
}

/// The store every test files what it compiled into, under this build's scratch directory: keyed by the test binary's own digest, as every store is keyed by its compiler's, so a rebuilt compiler finds nothing it did not compile.
fn store() -> Verdicts {
    Verdicts::at(Path::new(env!("CARGO_TARGET_TMPDIR")).join("claims"))
}

/// The library's one test subject: the package at [`std_directory`] as `curios test` selects it there.
fn library() -> TestSubject {
    let selection = Selection::of(
        Spelling::of(None),
        None,
        &std_directory(),
        &Overlay::default(),
    )
    .unwrap_or_else(|refusal| panic!("the standard library's package is refused: {refusal}"));
    let mut subjects = test_subjects(selection, || {
        unreachable!("the package entire reads nothing on standard input")
    })
    .unwrap_or_else(|error| panic!("the standard library is no test subject:\n{error}"));

    assert_eq!(subjects.len(), 1, "the standard library is one library");
    subjects.remove(0)
}

/// Every test `/std` declares, by its path.
fn declared() -> Vec<Qualifier> {
    let subject = library();
    let store = subject.filed(|_| Some(store()));

    subject
        .compile(DEFAULT_BUDGET, store.as_ref(), &[], |_| {})
        .unwrap_or_else(|error| panic!("the standard library failed to compile:\n{error}"))
        .declared()
        .to_vec()
}

/// Compile the tested `/std` with the tests at or under `module` scheduled, and run each of them.
///
/// Failures are collected rather than raised at the first. Each test runs in an instantiation of its own, so one failing says nothing about the rest, and a run that stopped early would hide every test after it.
fn run_module(module: &Qualifier) {
    let subject = library();
    let store = subject.filed(|_| Some(store()));
    let tested = subject
        .compile(
            DEFAULT_BUDGET,
            store.as_ref(),
            std::slice::from_ref(module),
            |_| {},
        )
        .unwrap_or_else(|error| panic!("`{}` failed to compile:\n{error}", module.join()));
    let run = tested
        .loaded(&subject)
        .unwrap_or_else(|refusal| panic!("`{}` failed to load: {refusal}", module.join()))
        .unwrap_or_else(|| panic!("`{}` declares no tests", module.join()));

    let mut passed = 0usize;
    let mut failures = String::new();
    for (index, record) in tested.records().iter().enumerate() {
        let mut held = None;
        let outcome = run.run(index, |arguments| {
            let (host, io) = MockHost::builder().args(arguments).build();
            held = Some(io);
            host
        });
        let reported = held.map_or_else(String::new, |io| {
            String::from_utf8_lossy(&io.output()).into_owned()
        });

        match outcome {
            // The guest printed `path: passed` and answered that the test held.
            Ok(TestEnd::Answered(true)) => passed += 1,
            // The guest printed its own outcome line and its report; the body as written is what only the records know, appended beneath it.
            Ok(TestEnd::Answered(false)) => {
                failures.push_str(&reported);
                if !record.body.is_empty() {
                    failures.push_str(&format!("{}\n", record.body));
                }
            }
            // An exit never reaches the guest's printing, so the line is written here, and it is a failure whatever its code: a verdict is answered, never exited with.
            Ok(TestEnd::Exited(code)) => {
                failures.push_str(&format!("{reported}{}: exited {code}\n", record.path));
            }
            // Nor does a trap reach it.
            Err(error) => {
                failures.push_str(&format!("{reported}{}: {error}\n", record.path));
            }
        }
    }

    eprintln!("{}: {passed} passed", module.join());

    assert!(failures.is_empty(), "\n{failures}");
}

/// A test outside every rostered module would be run by nothing and silently pass, which is the one failure mode this arrangement has that a test per declaration does not; and a rostered module holding none would run nothing.
#[test]
fn every_test_stands_in_a_rostered_module() {
    let modules = ROSTER
        .iter()
        .map(|subject| tests_of(subject))
        .collect::<Vec<_>>();
    let declared = declared();

    let unrostered = declared
        .iter()
        .filter(|test| !modules.iter().any(|module| test.is_within(module)))
        .map(Qualifier::join)
        .collect::<Vec<_>>();
    let empty = modules
        .iter()
        .filter(|module| !declared.iter().any(|test| test.is_within(module)))
        .map(Qualifier::join)
        .collect::<Vec<_>>();

    assert!(
        unrostered.is_empty(),
        "tests no rostered module holds: {unrostered:#?}"
    );
    assert!(
        empty.is_empty(),
        "rostered modules with no test: {empty:#?}"
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

/// **The library is written in the canonical form `curios format` produces.**
///
/// `curios format --check` is only a guarantee where something checks it. A tree drifted from canonical form hands anyone formatting it a diff that says nothing about their own change, and hides a formatter defect — a comment placed on the wrong arm — from the one corpus large enough to show it.
///
/// Formatting is syntax-only — no prelude, no elaboration — so one walk holds every `.crs` file this crate authors, its tests among them. A failure names the file, and `curios format <file>` is the fix.
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

/// What `program` prints, compiled against the prelude alone and run under the mock host with `stdin` on its standard input.
fn printed(program: &str, stdin: impl IntoIterator<Item = String>) -> String {
    let entrypoint: Entrypoint = program.parse().expect("the program parses");
    let (module, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .unwrap_or_else(|error| panic!("the program failed to compile:\n{error}"));
    let (host, io) = MockHost::builder().stdin_lines(stdin).build();
    run_wasm(&module, foreigns, host, ForeignBindings::empty()).expect("the program runs");

    String::from_utf8(io.output()).expect("the program prints text")
}

/// The text of a run of scalars as a Curios string literal spells it, each by its escape.
fn literal(scalars: &[u32]) -> String {
    let escaped: String = scalars
        .iter()
        .map(|scalar| format!("\\u{{{scalar:X}}}"))
        .collect();

    format!("\"{escaped}\"")
}

/// Every case of Unicode's `GraphemeBreakTest.txt`, at the version `curios-unicode` pins, segments through `/std/Str/graphemes` into the clusters the database states: one program over every case, printing each one segmented otherwise.
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
    let program = fill!(
        r#"
        use /std/{Str, List, Bool, Spell, print};

        let cases: List({Str, List(Str)}) =
            [
                ${cases}];

        let otherwise: List({Str, List(Str)}) =
            List/filter(cases, (case) => Bool/not(Str/graphemes(case.0) == case.1));

        print(Str/flatten(List/map(otherwise, (case) => Str/concat(Spell/spell(case.0), "\n"))))
        "#,
        cases
    );

    let otherwise = printed(&program, []);

    assert!(otherwise.is_empty(), "segmented otherwise:\n{otherwise}");
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

    let printed = printed(
        PROPERTIES_PROGRAM,
        scalars.iter().map(|&scalar| u32::from(scalar).to_string()),
    );
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
