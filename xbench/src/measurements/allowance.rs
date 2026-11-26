//! What deciding the class of a written site spends of the lint's allowance, counted.

use {
    crate::{Measure, phase, read},
    curios::{TALLIES, test_support::*},
    curios_elab::{DEFAULT_BUDGET, LINT_ALLOWANCE},
    curios_module::Overlay,
    curios_prelude_build::std_source,
    curios_surface::WrittenIn,
    curios_wonder::{Origin, Subject, diagnostics},
    std::fs::read_to_string,
};

/// **Every site the four corpora hold is classified inside [`LINT_ALLOWANCE`]**: the programs under `programs/`, the examples of `documentation/language.md`, and `/std`, its library and its test modules — compiled whole, since a fold handed no cabinet declines the archived unit as a baseline — each asked about as `curios check` asks, all of them one phase whose fold is read. A classification that ran out would answer no class and leave its site unreported, so the allowance is the least power of ten above the largest figure here.
///
/// `cargo xbench measure classification_allowance_measurements` takes it: `<sample>/sites` how many sites a sample observed, and `<sample>/most` the most units one spent. Counted, in units of the elaborator's budget, taken on top of `62915760a`: `classify` is what reducing a site's type to its class spent, `column` what inferring the type of a column a match pattern's `_` stands at spent.
///
/// | sample | sites | units at most |
/// | --- | ---: | ---: |
/// | `sites::classify` | 2 057 | 10 169 |
/// | `sites::column` | 568 | 13 255 |
///
/// **Both stay under a seventh of the allowance, and the largest is a column's**, whose type is inferred as the matrix reads it, where a site's class reads a type the elaborator already holds.
pub fn classification_allowance_measurements() {
    let reference = read_to_string(workspace().join("documentation/language.md"))
        .expect("the reference is readable");
    let mut subjects = programs(&workspace().join("programs"))
        .into_iter()
        .map(|path| Subject::Entry {
            units: Vec::new(),
            origin: Origin::File(path),
            declares: None,
            package: None,
        })
        .collect::<Vec<_>>();
    subjects.extend(
        fenced(&reference, "crs")
            .into_iter()
            .map(|(_, text)| Subject::Entry {
                units: Vec::new(),
                origin: Origin::Text {
                    text,
                    written: WrittenIn::Program,
                },
                declares: None,
                package: None,
            }),
    );
    subjects.push(Subject::Unit {
        units: vec![std_source().tested()],
    });

    let report = phase(TALLIES, || {
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
    .profile;

    for name in ["sites::classify", "sites::column"] {
        let (count, max) = report
            .samples
            .iter()
            .filter(|sample| sample.name == name)
            .fold((0, 0), |(count, max), sample| {
                (count + sample.count, max.max(sample.max))
            });
        read(&format!("{name}/sites"), Measure::Count, None, count);
        read(&format!("{name}/most"), Measure::Count, None, max);
        assert!(
            max < LINT_ALLOWANCE,
            "{name} spent {max} of the allowance {LINT_ALLOWANCE}"
        );
    }
}
