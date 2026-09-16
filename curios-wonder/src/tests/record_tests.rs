//! Which records one program yields: every refusal, goal and lint it holds, each located at its own term, a lint after the verdict it stands beside, and nothing for a declaration whose only fault is reaching a refused one.

use {
    super::test_support::{mounted, mounted_project, write},
    crate::{Origin, Severity, Subject, diagnostics},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::Overlay,
};

fn of(text: &str) -> Vec<crate::Diagnostic> {
    diagnostics(
        DEFAULT_STEP_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            declares: None,
            origin: Origin::Text {
                label: "<stdin>".to_string(),
                text: text.to_string(),
            },
            unlinked: None,
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
    let reports = of("/std/print(\n");
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
/// The check path used to stop at the kernel, on the claim that the stages under it only build the program. Erasure does more than build: it narrows a dispatch key into the branch table `curios-ersd` carries, refusing one that does not fit, and it hands the module to the erased verifier, which rejects the recursion classes the language does not admit. Both programs below were reported clean here and refused by `run`.
#[test]
fn a_refusal_below_the_kernel_is_still_reported() {
    let overflowing = of(
        "let _f(n : /std/Nat) -> /std/Nat = match n | 0 => 1 | 4294967296 => 2 | _ => 0 end;\n/std/print(\"\")",
    );
    let [report] = overflowing.as_slice() else {
        panic!("one diagnostic, got {overflowing:?}");
    };
    assert_eq!(report.severity, Severity::Error);
    assert!(
        report.render().contains("does not fit a branch table"),
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
        report.render().contains("evaluate each other"),
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
}

/// Elaboration recovers past a refusal, so a file with two answers with two — each an error at its own term — and a declaration reaching a refused one is not in the answer.
#[test]
fn every_refusal_in_a_broken_file_is_listed() {
    let reports = of(
        "let _a : /std/Nat = true;\nlet _b : /std/Nat = _a;\nlet _c : /std/Nat = false;\n/std/print(\"\")",
    );
    let [first, second] = reports.as_slice() else {
        panic!("two refusals, got {reports:?}");
    };

    assert_eq!(first.severity, Severity::Error);
    assert_eq!(second.severity, Severity::Error);
    assert_eq!(first.report.span.as_ref().unwrap().line_column(), (1, 21));
    assert_eq!(second.report.span.as_ref().unwrap().line_column(), (3, 21));
}

#[test]
fn a_goal_beside_a_refusal_keeps_its_goal_severity() {
    let reports = of("let _a : /std/Nat = true;\nlet _m : /std/Nat = ?;\n/std/print(\"\")");
    let [refusal, goal] = reports.as_slice() else {
        panic!("a refusal and a goal, got {reports:?}");
    };

    assert_eq!(refusal.severity, Severity::Error);
    assert_eq!(refusal.report.span.as_ref().unwrap().line_column(), (1, 21));
    assert_eq!(goal.severity, Severity::Goal);
    assert_eq!(goal.report.span.as_ref().unwrap().line_column(), (2, 21));
}

/// A withheld witness declaration reports nothing of its own, and neither do its consumers: the miss they would report is the withholding's consequence, and the withholding's cause is already in the answer.
///
/// **The regression for a witness registering when its signature elaborates.** A *refused* witness registered before its body failed, so undoing it poisoned its key in place and a consumer meeting the poison stayed silent. A withheld one never registered, so there was no key to poison, and one unreadable declaration answered with two records — the second at a declaration with nothing wrong with it, pointing away from the one that has.
#[test]
fn a_withheld_witness_leaves_its_consumers_silent() {
    let reports = of(concat!(
        "use /std/{Nat, Show, Str};\n\n",
        "pub struct Meters: pub Type { Nat }\n\n",
        "pub let label(m: Meters) -> Str = ;\n\n",
        "satisfy Show(Meters) {\n    show(m) = label(m),\n}\n\n",
        "pub let render(m: Meters) -> Str = Show/show(m);\n\n",
        "/std/print(\"\")\n",
    ));

    let [record] = reports.as_slice() else {
        panic!("one record, got {reports:?}");
    };
    assert_eq!(record.severity, Severity::Error);
    assert!(
        record.report.message.contains("expected a term"),
        "{}",
        record.report.render()
    );
}

/// A parse failure inside one declaration is that declaration's record, and the file is read past it: the refusal after it is reported beside it, at its own term.
#[test]
fn a_broken_declaration_is_reported_beside_the_declarations_after_it() {
    let reports = of("let _a : /std/Nat = ;\nlet _c : /std/Nat = true;\n/std/print(\"\")");
    let [first, second] = reports.as_slice() else {
        panic!("two records, got {reports:?}");
    };

    assert_eq!(first.severity, Severity::Error);
    assert_eq!(first.report.span.as_ref().unwrap().line_column().0, 1);
    assert_eq!(second.severity, Severity::Error);
    assert_eq!(second.report.span.as_ref().unwrap().line_column(), (2, 21));
}
