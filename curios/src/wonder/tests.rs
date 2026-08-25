use {
    super::{Origin, Severity, Subject, diagnostics},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::Overlay,
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
