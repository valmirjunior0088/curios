use {
    super::{Origin, ReadOnly, Severity, Subject, diagnostics, rendered},
    crate::{Asked, Verdicts},
    curios_pipeline::{Cache, DEFAULT_STEP_BUDGET, Progress, check_units_with_prelude},
    curios_text::Overlay,
    std::{
        collections::BTreeMap,
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    },
};

fn of(text: &str) -> Vec<super::Diagnostic> {
    diagnostics(
        DEFAULT_STEP_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                label: "<stdin>".to_string(),
                text: text.to_string(),
            },
        },
        &Overlay::default(),
        None,
    )
}

#[test]
fn a_program_that_compiles_reports_nothing() {
    assert!(of("/std/print(\"hi\\n\")").is_empty());
}

#[test]
fn a_parse_failure_is_one_error_at_the_offset_the_parser_stopped() {
    let reports = of("/std/print(\"unclosed\n");
    let [report] = reports.as_slice() else {
        panic!("one diagnostic, got {reports:?}");
    };

    assert_eq!(report.severity, Severity::Error);
    let span = report
        .report
        .span
        .as_ref()
        .expect("a parse failure is located");
    assert_eq!(span.line_column(), (2, 1));
    assert!(
        report.render().contains("<stdin>:2:1"),
        "{}",
        report.render()
    );
}

#[test]
fn a_refused_type_is_one_error_at_the_term_refused() {
    let reports = of("let bad : /std/Nat = true;\n/std/print(\"\")");
    let [report] = reports.as_slice() else {
        panic!("one diagnostic, got {reports:?}");
    };

    assert_eq!(report.severity, Severity::Error);
    let span = report
        .report
        .span
        .as_ref()
        .expect("a type error is located");
    assert_eq!(span.line_column(), (1, 22));
    // The record renders as the compile path prints: message, then the snippet the span draws.
    assert!(
        report.render().contains("<stdin>:1:22"),
        "{}",
        report.render()
    );
}

#[test]
fn every_goal_is_its_own_record_at_its_own_occurrence() {
    let reports = of("let m : /std/Nat = ?;\nlet n : /std/Nat = ?;\n/std/print(\"\")");
    let [first, second] = reports.as_slice() else {
        panic!("two goals, got {reports:?}");
    };

    assert_eq!(first.severity, Severity::Goal);
    assert_eq!(second.severity, Severity::Goal);
    assert_eq!(first.report.span.as_ref().unwrap().line_column(), (1, 20));
    assert_eq!(second.report.span.as_ref().unwrap().line_column(), (2, 20));
    assert!(
        first.report.message.starts_with("goal `?`"),
        "{}",
        first.report.message
    );
    // The message carries no snippet — the span is the record's, and the transport draws or does not draw it.
    assert!(!first.report.message.contains("-->"));
}

/// A unit the overlay does not reach still comes from the store, however many units before it the overlay refused.
///
/// **The regression for placing and filing having been one decision.** [`ReadOnly`] drops the write, and used to drop the placement with it — but a slot is addressed after the units placed before it, so the first refused unit shifted every later address by one and one declined hit became a miss for the whole tail. Two mounted units are the smallest shape that can show it: the second is what the first's absence from the chain moves.
///
/// Read off the fold's own progress events, as the store's own tests are: whether a unit was reused is exactly the question a caller asks, and asserting on a slot name would pass just as happily with the chain broken.
#[test]
fn refusing_one_units_hit_does_not_refuse_the_units_after_it() {
    let root = mounted_project("read-only-chain");

    // Filed by an ordinary build, since a query reads a store it never writes.
    built(&root);

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "both units are the store's when the overlay reaches neither"
    );

    assert_eq!(
        folded(&root, &held(&root, "a/lib.crs")),
        ["compiling /alpha", "reused /beta"],
        "the overlay reaches /alpha alone, so /beta is still the store's"
    );

    fs::remove_dir_all(root).unwrap();
}

/// Two mountable packages and nothing else, at a directory of its own.
fn mounted_project(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!(
        "curios-wonder-{name}-{}-{millis}",
        std::process::id()
    ));

    for (directory, package) in [("a", "alpha"), ("b", "beta")] {
        write(
            &root,
            &format!("{directory}/curios.toml"),
            &format!("name = \"{package}\"\n"),
        );
        write(
            &root,
            &format!("{directory}/lib.crs"),
            &format!("use /std/{{Str}};\n\npub let said: Str =\n    \"{package}\";\n"),
        );
    }

    root
}

/// File both units the way an ordinary build does, through the store itself.
fn built(root: &Path) {
    let store = Verdicts::at(root.to_path_buf());

    check_units_with_prelude(
        DEFAULT_STEP_BUDGET,
        &mounted(root),
        Some(&store as &dyn Cache),
        |_| {},
    )
    .expect("two compiling units");
}

/// What the fold did to each of `root`'s units, checked through [`ReadOnly`] over `overlay` — the store `wonder` hands a query.
fn folded(root: &Path, overlay: &Overlay) -> Vec<String> {
    let units = super::overlaid(mounted(root), overlay);
    let store = Verdicts::at(root.to_path_buf());
    let read_only = ReadOnly {
        cache: &store,
        overlay,
    };

    let mut events = Vec::new();
    check_units_with_prelude(
        DEFAULT_STEP_BUDGET,
        &units,
        Some(&read_only as &dyn Cache),
        |progress| match progress {
            Progress::Compiling(prefix) => events.push(format!("compiling {}", prefix.join())),
            Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
            _ => {}
        },
    )
    .expect("two compiling units");

    events
}

/// Both packages, mounted in the order they are compiled in.
fn mounted(root: &Path) -> Vec<curios_text::RootSource> {
    curios_package::mounted(&[root.join("a"), root.join("b")]).expect("two mountable packages")
}

/// An overlay holding `path`'s own text: a document an editor has open and has not yet changed, which is a refused hit all the same — the store's re-read knows only the disk.
fn held(root: &Path, path: &str) -> Overlay {
    let path = root.join(path);
    let text = fs::read_to_string(&path).expect("a written module");

    Overlay::of(BTreeMap::from([(path, text)]))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// One fact is reported once, however many subjects reach it.
///
/// **The regression for a package's subjects overlapping.** `wonder diagnostics` with no target asks about the library and about every executable the package declares, and every executable is compiled against that library — so one unbound variable in it printed once per subject, three times in a package declaring two programs, and an agent walking its errors one at a time walked the same one three times.
///
/// Two subjects over one text stand in for that overlap. What the fix rests on is only that equal renderings are one fact, which the second half pins from the other side: two subjects saying different things still say both.
#[test]
fn one_fact_reached_by_two_subjects_is_rendered_once() {
    let asked = |text: &str| Asked {
        subject: Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                label: "<stdin>".to_string(),
                text: text.to_string(),
            },
        },
        store: None,
    };

    let same = rendered(
        vec![asked("/std/print(nope)"), asked("/std/print(nope)")],
        DEFAULT_STEP_BUDGET,
        &Overlay::default(),
    );
    let [one] = same.as_slice() else {
        panic!("two subjects, one fact, got {same:?}");
    };
    assert!(one.contains("unbound variable: nope"), "{one}");

    let different = rendered(
        vec![asked("/std/print(nope)"), asked("/std/print(other)")],
        DEFAULT_STEP_BUDGET,
        &Overlay::default(),
    );
    assert_eq!(
        different.len(),
        2,
        "two subjects, two facts, got {different:?}"
    );
}
