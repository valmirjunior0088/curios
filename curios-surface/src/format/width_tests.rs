//! The width a file is formatted within: which widths are admitted, how one is read, and that the laws hold at the narrowest over the sources of the tree.

use {
    super::{test_support::*, *},
    curios_common::Source,
};

#[test]
fn a_width_is_admitted_from_the_narrowest_through_the_line_width() {
    assert_eq!(Width::default().columns(), MAX_LINE_WIDTH);
    assert_eq!(
        Width::new(MIN_LINE_WIDTH).map(Width::columns),
        Ok(MIN_LINE_WIDTH)
    );
    assert_eq!(
        Width::new(MAX_LINE_WIDTH).map(Width::columns),
        Ok(MAX_LINE_WIDTH)
    );
    assert!(Width::new(MIN_LINE_WIDTH - 1).is_err());
    assert!(Width::new(MAX_LINE_WIDTH + 1).is_err());
}

#[test]
fn a_width_is_read_from_its_digits_alone() {
    assert_eq!("72".parse::<Width>().map(Width::columns), Ok(72));
    for spelling in ["72px", "-72", "", "seventy"] {
        let refusal = spelling.parse::<Width>().expect_err(spelling);
        assert!(refusal.contains(&format!("`{spelling}`")), "{refusal}");
    }
    assert!(
        MAX_LINE_WIDTH
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
    let narrowest = at(MIN_LINE_WIDTH);

    assert_eq!(formatted(source), source);
    let narrow = formatted_within(source, narrowest);
    assert_ne!(narrow, source);
    assert!(
        narrow
            .lines()
            .all(|line| line.chars().count() <= MIN_LINE_WIDTH),
        "{narrow}"
    );
    assert_eq!(formatted_within(&narrow, narrowest), narrow);
}

/// Every source of the tree formats at the narrowest line width without refusal, a match kept on one line by the defaults and at each extreme the settings admit, keeping its program and its comments, and what that writes is written again unchanged: the laws hold there over every shape the corpus holds, which far more breaks put to the renderer than the default does.
#[test]
fn every_source_in_the_tree_formats_at_the_narrowest_width_and_holds_there() {
    let mut faults = Vec::new();

    for (format, (path, text)) in narrowest_formats().into_iter().flat_map(|format| {
        sources_of_the_tree()
            .into_iter()
            .map(move |source| (format, source))
    }) {
        let once = match Formatted::from_source(
            &Source::held(path.clone(), text),
            WrittenIn::of(&path),
            format,
        ) {
            Ok(Formatted::Unchanged(text) | Formatted::Changed(text)) => text,
            Err(refusal) => {
                faults.push(format!(
                    "{}: {refusal}",
                    path.display(),
                    refusal = refusal.render()
                ));
                continue;
            }
        };
        match Formatted::from_source(
            &Source::held(path.clone(), once.as_str()),
            WrittenIn::of(&path),
            format,
        ) {
            Ok(Formatted::Unchanged(_)) => {}
            Ok(Formatted::Changed(_)) => {
                faults.push(format!("{}: a second run changes it", path.display()))
            }
            Err(refusal) => faults.push(format!(
                "{}: its own output was refused: {refusal}",
                path.display(),
                refusal = refusal.render()
            )),
        }
    }

    assert!(faults.is_empty(), "{}", faults.join("\n"));
}

/// A match is kept on one line within its format's flat match and broken past it: by its span, which `0` closes to every match, and by its arms, of which any count may be admitted.
#[test]
fn a_flat_match_keeps_or_breaks_a_match_by_its_span_and_its_arms() {
    let source = "let f(n: /std/Nat) -> /std/Nat =\n    match n | 0 => 1 | 1 => 2 | _ => n end;\n";
    let within = |width, arms| {
        let line_width = Width::default();
        let flat_match = FlatMatch::within(line_width, Some(width), Some(arms))
            .expect("the flat match is admitted");
        formatted_within(
            source,
            Format {
                line_width,
                flat_match,
            },
        )
    };
    let kept = |text: &str| {
        text.lines()
            .any(|line| line.trim_start().starts_with("match n |"))
    };

    assert!(kept(&formatted(source)), "{}", formatted(source));
    assert!(
        !kept(&within(0, DEFAULT_FLAT_MATCH_ARMS)),
        "no span keeps none"
    );
    assert!(
        !kept(&within(DEFAULT_FLAT_MATCH_WIDTH, 2)),
        "three arms past two"
    );
    assert!(
        kept(&within(DEFAULT_FLAT_MATCH_WIDTH, 3)),
        "three arms within three"
    );
    let four =
        "let f(n: /std/Nat) -> /std/Nat =\n    match n | 0 => 1 | 1 => 2 | 2 => 3 | _ => n end;\n";
    let wide = |arms| {
        let line_width = Width::default();
        formatted_within(
            four,
            Format {
                line_width,
                flat_match: FlatMatch::within(line_width, None, Some(arms))
                    .expect("the flat match is admitted"),
            },
        )
    };
    assert!(
        !kept(&wide(DEFAULT_FLAT_MATCH_ARMS)),
        "four arms past the default"
    );
    assert!(
        kept(&wide(usize::MAX)),
        "four arms within no count of its own"
    );
}
