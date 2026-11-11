//! The width a file is formatted within: which widths are admitted, how one is read, and that the laws hold at the narrowest over the sources of the tree.

use {
    super::{test_support::*, *},
    curios_common::Source,
};

#[test]
fn a_width_is_admitted_from_the_narrowest_through_the_line_width() {
    assert_eq!(Width::default().columns(), LINE_WIDTH);
    assert_eq!(Width::new(NARROWEST).map(Width::columns), Ok(NARROWEST));
    assert_eq!(Width::new(LINE_WIDTH).map(Width::columns), Ok(LINE_WIDTH));
    assert!(Width::new(NARROWEST - 1).is_err());
    assert!(Width::new(LINE_WIDTH + 1).is_err());
}

#[test]
fn a_width_is_read_from_its_digits_alone() {
    assert_eq!("72".parse::<Width>().map(Width::columns), Ok(72));
    for spelling in ["72px", "-72", "", "seventy"] {
        let refusal = spelling.parse::<Width>().expect_err(spelling);
        assert!(refusal.contains(&format!("{spelling:?}")), "{refusal}");
    }
    assert!(
        LINE_WIDTH
            .saturating_add(1)
            .to_string()
            .parse::<Width>()
            .is_err()
    );
}

#[test]
fn a_narrower_width_breaks_what_the_default_keeps_on_one_line() {
    let source =
        "let total: Nat =\n    combine(first_argument, second_argument, third_argument);\n";
    let narrowest = Width::new(NARROWEST).expect("the narrowest width is admitted");

    assert_eq!(formatted(source), source);
    let narrow = formatted_within(source, narrowest);
    assert_ne!(narrow, source);
    assert!(
        narrow.lines().all(|line| line.chars().count() <= NARROWEST),
        "{narrow}"
    );
    assert_eq!(formatted_within(&narrow, narrowest), narrow);
}

/// Every source of the tree formats at the narrowest width without refusal, keeping its program and its comments, and what that writes is written again unchanged: the laws hold there over every shape the corpus holds, which far more breaks put to the renderer than the default does.
#[test]
fn every_source_in_the_tree_formats_at_the_narrowest_width_and_holds_there() {
    let narrowest = Width::new(NARROWEST).expect("the narrowest width is admitted");
    let mut faults = Vec::new();

    for (path, text) in sources_of_the_tree() {
        let once = match Formatted::from_source(&Source::held(path.clone(), text), narrowest) {
            Ok(Formatted::Unchanged(text) | Formatted::Changed(text)) => text,
            Err(refusal) => {
                faults.push(format!("{}: {refusal}", path.display()));
                continue;
            }
        };
        match Formatted::from_source(&Source::held(path.clone(), once.as_str()), narrowest) {
            Ok(Formatted::Unchanged(_)) => {}
            Ok(Formatted::Changed(_)) => {
                faults.push(format!("{}: a second run changes it", path.display()))
            }
            Err(refusal) => faults.push(format!(
                "{}: its own output was refused: {refusal}",
                path.display()
            )),
        }
    }

    assert!(faults.is_empty(), "{}", faults.join("\n"));
}
