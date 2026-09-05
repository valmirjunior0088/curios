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

/// One rung of `program`, asked for the way the one-shot transport asks.
fn rung(name: &str, text: &str) -> Result<super::Rendering, super::Refusal> {
    match super::stage(
        DEFAULT_STEP_BUDGET,
        Vec::new(),
        Origin::Text {
            label: "<stdin>".to_string(),
            text: text.to_string(),
        },
        &Overlay::default(),
        None,
        name,
    ) {
        Ok(super::Reached::Rendered(rendering)) => Ok(rendering),
        Ok(super::Reached::Wasm(_)) => panic!("asked for a driver rung, reached the module"),
        Err(refusal) => Err(refusal),
    }
}

const REFUSED_LATER: &str = "use /std/{Nat};\nlet x : Nat = true;\n/std/print(\"ok\\n\")";

/// A rung the driver emitted is an answer, and a failure *after* it does not unmake one. Asking for `core` on a program that fails to elaborate is asking what the lowering produced, which is most of the reason to ask at all.
#[test]
fn a_rung_reached_before_the_failure_is_still_answered() {
    for name in ["text", "core"] {
        let rendering = rung(name, REFUSED_LATER)
            .unwrap_or_else(|_| panic!("{name} was observed before the refusal"));
        assert_eq!(rendering.name, name);
        assert!(!rendering.text.is_empty(), "{name} rendered nothing");
        assert!(
            !rendering.diagnostics.is_empty(),
            "{name} lost the diagnostics that stopped the compilation"
        );
    }
}

/// The other side of the same boundary: a rung the program never reached has not been answered, and stays a refusal. `ersd` sits past elaboration, which is where this program stops.
#[test]
fn a_rung_the_program_never_reached_is_refused() {
    let refusal = rung("ersd", REFUSED_LATER).expect_err("ersd is never reached");
    assert!(
        matches!(refusal, super::Refusal::Diagnostics(diagnostics) if !diagnostics.is_empty()),
        "expected the refusal to carry what stopped the program"
    );
}

/// A program that compiles answers with nothing beside the rendering, so the added channel stays empty on the ordinary path.
#[test]
fn a_rung_of_a_program_that_compiles_carries_no_diagnostics() {
    let rendering = rung("core", "/std/print(\"hi\\n\")").expect("core is reached");
    assert!(rendering.diagnostics.is_empty());
}

#[test]
fn a_parameterized_test_the_roster_cannot_draw_is_reported_through_the_test_program() {
    // The written program compiles — a test body is an item like any other — and the fault is in the test program's tail, where the `Property` goal is raised: the answer is that goal, at the declaration, exactly as `curios test` reports it. A drawable twin reports nothing, since its test program compiles too.
    let diagnostics = of(r#"
use /std/{Nat, Test};
test bounded(n: Nat, _p: Nat/Lt(n, 100)) =
    Test/check(n < 100);
/std/print("ran\n")
"#);
    assert_eq!(diagnostics.len(), 1);
    assert!(matches!(diagnostics[0].severity, Severity::Error));
    let rendered = diagnostics[0].render();
    assert!(
        rendered.contains("Property") && rendered.contains("Test/check(n < 100)"),
        "{rendered}"
    );

    let diagnostics = of(r#"
use /std/{Nat, Test};
test small(n: Nat) =
    Test/check(n < 100);
/std/print("ran\n")
"#);
    assert!(diagnostics.is_empty());
}

#[test]
fn a_library_test_the_roster_cannot_draw_is_reported_through_its_test_program() {
    // A library has no written program: it is checked through the `()` entry under the last unit's tests tail, the way `curios test` compiles it, so the goal lands on the declaration in `lib.crs`.
    let root = mounted_project("library-test-program");
    write(
        &root,
        "b/lib.crs",
        "use /std/{Nat, Test};\n\ntest bounded(n: Nat, _p: Nat/Lt(n, 100)) =\n    Test/check(n < 100);\n",
    );
    let diagnostics = diagnostics(
        DEFAULT_STEP_BUDGET,
        Subject::Unit {
            units: mounted(&root),
        },
        &Overlay::default(),
        None,
    );
    assert_eq!(diagnostics.len(), 1);
    let rendered = diagnostics[0].render();
    assert!(
        rendered.contains("Property") && rendered.contains("lib.crs"),
        "{rendered}"
    );
    fs::remove_dir_all(&root).unwrap();
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
    let reports = of("let _bad : /std/Nat = true;\n/std/print(\"\")");
    let [report] = reports.as_slice() else {
        panic!("one diagnostic, got {reports:?}");
    };

    assert_eq!(report.severity, Severity::Error);
    let span = report
        .report
        .span
        .as_ref()
        .expect("a type error is located");
    assert_eq!(span.line_column(), (1, 23));
    // The record renders as the compile path prints: message, then the snippet the span draws.
    assert!(
        report.render().contains("<stdin>:1:23"),
        "{}",
        report.render()
    );
}

/// A refusal from *below* the kernel is still a diagnostic: this query answers what `run` would say, and erasure is the last stage that says anything.
///
/// The check path used to stop at the kernel, on the claim that the stages under it only build the program. Erasure does more than build: it narrows every numeral into the erased carriers, refusing one that does not fit, and it hands the module to the erased verifier, which rejects the recursion classes the language does not admit. Both programs below were reported clean here and refused by `run`.
#[test]
fn a_refusal_below_the_kernel_is_still_reported() {
    let overflowing = of("let _f(n : /std/Nat) -> /std/Bool = n == 4294967296;\n/std/print(\"\")");
    let [report] = overflowing.as_slice() else {
        panic!("one diagnostic, got {overflowing:?}");
    };
    assert_eq!(report.severity, Severity::Error);
    assert!(
        report
            .render()
            .contains("overflows u32 at the erase boundary"),
        "{}",
        report.render()
    );

    // A mutual value group with no lambda between its members: no forcing order satisfies it, and the erased verifier is what says so.
    let knotted = of("let _a : /std/Nat = _b\nand _b : /std/Nat = _a;\n/std/print(\"\")");
    let [report] = knotted.as_slice() else {
        panic!("one diagnostic, got {knotted:?}");
    };
    assert_eq!(report.severity, Severity::Error);
    assert!(
        report
            .render()
            .contains("the erased module failed verification"),
        "{}",
        report.render()
    );
}

#[test]
fn every_goal_is_its_own_record_at_its_own_occurrence() {
    let reports = of("let _m : /std/Nat = ?;\nlet _n : /std/Nat = ?;\n/std/print(\"\")");
    let [first, second] = reports.as_slice() else {
        panic!("two goals, got {reports:?}");
    };

    assert_eq!(first.severity, Severity::Goal);
    assert_eq!(second.severity, Severity::Goal);
    assert_eq!(first.report.span.as_ref().unwrap().line_column(), (1, 21));
    assert_eq!(second.report.span.as_ref().unwrap().line_column(), (2, 21));
    assert!(
        first.report.message.starts_with("goal `?`"),
        "{}",
        first.report.message
    );
    // The message carries no snippet — the span is the record's, and the transport draws or does not draw it.
    assert!(!first.report.message.contains("-->"));
}

/// A lint is a record of its own severity, located at the word it is about, rendered as any report is.
#[test]
fn a_lint_is_its_own_record_at_the_word() {
    let reports = of("use /std/{Bool};\n/std/print(\"\")");
    let [report] = reports.as_slice() else {
        panic!("one lint, got {reports:?}");
    };

    assert_eq!(report.severity, Severity::Lint);
    assert_eq!(report.report.message, "unused import `Bool`; delete it");
    assert_eq!(report.report.span.as_ref().unwrap().line_column(), (1, 11));
    assert!(
        report.render().contains("<stdin>:1:11"),
        "{}",
        report.render()
    );
}

/// The lowering decides a lint, so a program that lowers reports its lints beside whatever elaboration then said — after it, since the verdict is what a reader acts on first.
#[test]
fn lints_are_reported_after_a_goal_and_after_an_error() {
    let reports = of("use /std/{Bool};\nlet _m : /std/Nat = ?;\n/std/print(\"\")");
    let severities = reports.iter().map(|r| r.severity).collect::<Vec<_>>();
    assert_eq!(severities, [Severity::Goal, Severity::Lint]);

    let reports = of("use /std/{Bool};\nlet _m : /std/Nat = true;\n/std/print(\"\")");
    let severities = reports.iter().map(|r| r.severity).collect::<Vec<_>>();
    assert_eq!(severities, [Severity::Error, Severity::Lint]);
}

/// A program that does not lower has nothing to read lints off: the error alone.
#[test]
fn a_program_that_does_not_lower_reports_the_error_alone() {
    let reports = of("use /std/{Nope};\nlet x : /std/Nat = 1;\n/std/print(\"\")");
    let severities = reports.iter().map(|r| r.severity).collect::<Vec<_>>();
    assert_eq!(severities, [Severity::Error]);
}

/// A library's lints are the unit's own, read off the unit the fold placed last — not the empty entry it is checked through.
#[test]
fn a_library_reports_its_own_lints() {
    let root = mounted_project("library-lints");
    write(
        &root,
        "b/lib.crs",
        "use /std/{Nat, Bool};\n\npub let one : Nat = 1;\n",
    );
    let diagnostics = diagnostics(
        DEFAULT_STEP_BUDGET,
        Subject::Unit {
            units: mounted(&root),
        },
        &Overlay::default(),
        None,
    );
    let [report] = diagnostics.as_slice() else {
        panic!("one lint, got {diagnostics:?}");
    };
    assert_eq!(report.severity, Severity::Lint);
    assert!(
        report.render().contains("lib.crs:1:16"),
        "{}",
        report.render()
    );
    fs::remove_dir_all(root).unwrap();
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
