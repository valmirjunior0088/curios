//! The README's examples are what the compiler makes of them: every `crs` block of `README.md` is written as `curios format` writes it, and the error it shows, its one `text` block, is what `curios wonder` prints for the `crs` block before it.

use {
    super::test_support::*,
    curios_common::{Source, Style},
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    curios_surface::{Formatted, Width},
    curios_wonder::{Origin, Subject, diagnostics},
    std::{fs, path::Path},
};

/// The file the README has its reader save the error example in, which is the path `wonder` prints.
const ERROR_EXAMPLE: &str = "vector.crs";

fn readme() -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../README.md");
    fs::read_to_string(&path).expect("the README is readable")
}

#[test]
fn every_crs_block_is_written_as_format_writes_it() {
    let examples = fenced(&readme(), "crs");
    assert!(!examples.is_empty(), "the README holds examples");

    let mut unformatted = Vec::new();
    for (line, example) in examples {
        match Formatted::from_source(&Source::inline(example), Width::default()) {
            Ok(Formatted::Unchanged(_)) => {}
            Ok(Formatted::Changed(text)) => {
                unformatted.push(format!("README.md:{line} is formatted as\n\n{text}"));
            }
            Err(refusal) => unformatted.push(format!("README.md:{line}: {refusal}")),
        }
    }

    assert!(unformatted.is_empty(), "{}", unformatted.join("\n\n"));
}

#[test]
fn the_error_shown_is_what_wonder_prints() {
    let readme = readme();
    let Ok([(shown_at, shown)]) = <[_; 1]>::try_from(fenced(&readme, "text")) else {
        panic!("the README shows one error, in its one `text` block");
    };
    let (_, example) = fenced(&readme, "crs")
        .into_iter()
        .take_while(|(line, _)| *line < shown_at)
        .last()
        .expect("an example comes before the error it shows");

    let subject = Subject::Entry {
        units: Vec::new(),
        origin: Origin::Text {
            label: ERROR_EXAMPLE.to_owned(),
            text: example,
        },
        declares: None,
    };
    let reports = diagnostics(
        DEFAULT_BUDGET,
        subject,
        &Overlay::default(),
        None,
        &mut |_| {},
    );
    let printed: Vec<String> = reports
        .diagnostics
        .iter()
        .map(|report| report.rendered(&Style::plain()))
        .collect();

    // `wonder` prints its reports a blank line apart, the last ending its line.
    assert_eq!(format!("{}\n", printed.join("\n\n")), shown);
}
