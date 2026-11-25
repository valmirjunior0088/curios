//! The language reference's examples check clean: every `crs` block of `documentation/language.md` is a whole program or module, asked about as `curios wonder -` asks about standard input, and reports nothing — no error, goal or lint — and is canonical, as `curios format --check -` asks. A form shown on its own, outside a program, is a `text` block, which nothing compiles.

use {
    crate::test_support::*,
    curios_common::{Kind, Source, SourceName, Style},
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    curios_surface::{Format, Formatted, WrittenIn},
    curios_wonder::{diagnostics, test_support::*},
    std::{fs, path::Path},
};

#[test]
fn every_crs_example_reports_nothing() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../documentation/language.md");
    let reference = fs::read_to_string(&path).expect("the reference is readable");
    let examples = fenced(&reference, "crs");
    assert!(!examples.is_empty(), "the reference holds examples");

    let mut wrong = Vec::new();
    for (line, example) in examples {
        let source = Source::named(SourceName::stdin(), example.as_str());
        match Formatted::from_source(&source, WrittenIn::Program, Format::default()) {
            Ok(Formatted::Unchanged(_)) => {}
            Ok(Formatted::Changed(canonical)) => wrong.push(format!(
                "the example at language.md:{line} is not canonical; the formatter writes it\n{canonical}"
            )),
            Err(report) => wrong.push(format!(
                "the example at language.md:{line} does not format\n{}",
                report.rendered(Kind::Error, &Style::plain())
            )),
        }
        let reports = diagnostics(
            DEFAULT_BUDGET,
            stdin_subject(&example, WrittenIn::Program),
            &Overlay::default(),
            None,
            &mut |_| {},
        );
        wrong.extend(reports.diagnostics.iter().map(|report| {
            format!(
                "the example at language.md:{line}\n{}",
                report.rendered(&Style::plain())
            )
        }));
    }

    assert!(
        wrong.is_empty(),
        "{} reports:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
}
