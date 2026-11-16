//! What deciding the class of a written site spends of the lint's allowance, counted.

use {
    crate::tests::test_support::*,
    curios_elab::{DEFAULT_BUDGET, LINT_ALLOWANCE},
    curios_module::Overlay,
    curios_prelude::test_support::*,
    curios_profile::{Destination, fold_at, trace},
    curios_wonder::{Origin, Subject, diagnostics},
    std::fs::{create_dir_all, read_to_string},
};

/// **Every site the four corpora hold is classified inside [`LINT_ALLOWANCE`]**: the programs under `programs/`, the examples of `documentation/language.md`, and `/std`, its library and its test modules — compiled whole, since a fold handed no store declines the archived unit as a baseline — each asked about as `curios lint` asks, under a stream filed at `curios/.artifacts/allowance/`. A classification that ran out would answer no class and leave its site unreported, so the allowance is the least power of ten above the largest figure here.
///
/// cargo test --all-features --package curios --lib -- --ignored --nocapture classification_allowance_measurements
///
/// Counted, in units of the elaborator's budget, taken on top of `62915760a`: `classify` is what reducing a site's type to its class spent, `column` what inferring the type of a column a match pattern's `_` stands at spent.
///
/// | sample | sites | units at most |
/// | --- | ---: | ---: |
/// | `sites::classify` | 2 057 | 10 169 |
/// | `sites::column` | 568 | 13 255 |
///
/// **Both stay under a seventh of the allowance, and the largest is a column's**, whose type is inferred as the matrix reads it, where a site's class reads a type the elaborator already holds.
#[test]
#[ignore = "measurement, counted: run by hand, alone in its invocation"]
fn classification_allowance_measurements() {
    let directory = workspace().join("curios/.artifacts/allowance");
    create_dir_all(&directory).expect("capture directory");
    let stream = directory.join("stream.tsv");

    let reference = read_to_string(workspace().join("documentation/language.md"))
        .expect("the reference is readable");
    let mut subjects = programs(&workspace().join("programs"))
        .into_iter()
        .map(|path| Subject::Entry {
            units: Vec::new(),
            origin: Origin::File(path),
            declares: None,
        })
        .collect::<Vec<_>>();
    subjects.extend(
        fenced(&reference, "crs")
            .into_iter()
            .map(|(line, text)| Subject::Entry {
                units: Vec::new(),
                origin: Origin::Text {
                    label: format!("the example at language.md:{line}"),
                    text,
                },
                declares: None,
            }),
    );
    subjects.push(Subject::Unit {
        units: vec![std_source().tested()],
    });

    trace(Destination::File(stream.clone()), || {
        for subject in subjects {
            diagnostics(
                DEFAULT_BUDGET,
                subject,
                &Overlay::default(),
                None,
                &mut |_| {},
            );
        }
    })
    .expect("the stream opens");
    let report = fold_at(&stream).expect("the stream folds");

    for name in ["sites::classify", "sites::column"] {
        let (count, max) = report
            .samples
            .iter()
            .filter(|sample| sample.name == name)
            .fold((0, 0), |(count, max), sample| {
                (count + sample.count, max.max(sample.max))
            });
        println!("{name:<16} {count:>8} sites {max:>10} units at most");
        assert!(
            max < LINT_ALLOWANCE,
            "{name} spent {max} of the allowance {LINT_ALLOWANCE}"
        );
    }
}
