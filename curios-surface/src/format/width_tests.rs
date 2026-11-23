//! The width and the indentation a file is formatted within: which are admitted, how each is read, and that the laws hold at each over the sources of the tree.

use {
    super::{test_support::*, *},
    curios_common::Source,
};

#[test]
fn a_width_is_one_of_the_widths_the_laws_are_held_at() {
    assert_eq!(Width::default().columns(), MAX_LINE_WIDTH);
    for columns in LINE_WIDTHS {
        assert_eq!(Width::new(columns).map(Width::columns), Ok(columns));
    }
    for columns in [MIN_LINE_WIDTH - 1, 60, MAX_LINE_WIDTH + 1] {
        let refusal = Width::new(columns).expect_err("a width between or past them");
        assert_eq!(
            refusal,
            format!("a line width is `50`, `80` or `100` columns, and `{columns}` is not")
        );
    }
}

#[test]
fn a_width_is_read_from_its_digits_alone() {
    assert_eq!("80".parse::<Width>().map(Width::columns), Ok(80));
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
fn an_indentation_is_one_of_the_two_the_laws_are_held_at() {
    assert_eq!(Indent::default().columns(), DEFAULT_INDENT_WIDTH);
    for columns in INDENT_WIDTHS {
        assert_eq!(Indent::new(columns).map(Indent::columns), Ok(columns));
    }
    for columns in [0, 3, 8] {
        assert_eq!(
            Indent::new(columns),
            Err(format!(
                "an indentation is `2` or `4` columns, and `{columns}` is not"
            ))
        );
    }
    assert_eq!("2".parse::<Indent>().map(Indent::columns), Ok(2));
    let refusal = "two".parse::<Indent>().expect_err("no digits");
    assert!(refusal.contains("`two`"), "{refusal}");
}

/// At two columns a file is indented two columns a level and broken where the default breaks it: a source canonical at four, its leading spaces halved.
#[test]
fn a_two_column_indentation_indents_each_level_by_two() {
    let source = "struct Pair: Type {\n    fst: /std/Nat,\n    snd: /std/Nat,\n}\n\nlet f(n: /std/Nat) -> /std/Nat =\n    match n\n    | 0 => 1\n    | 1 => 2\n    | 2 => 3\n    | _ =>\n        let m = n + 1;\n        m\n    end;\n";
    let halved = source
        .lines()
        .map(|line| {
            let written = line.trim_start();
            format!(
                "{}{written}\n",
                " ".repeat((line.len() - written.len()) / 2)
            )
        })
        .collect::<String>();
    let two = Format {
        indent_width: Indent::new(2).expect("the indentation is admitted"),
        ..Format::default()
    };

    assert_eq!(formatted(source), source);
    assert_eq!(formatted_within(source, two), halved);
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

/// Every source of the tree formats at every admitted line width and indentation without refusal, a match kept on one line by the defaults and at each extreme the settings admit, keeping its program and its comments, and what that writes is written again unchanged: the laws hold there over every shape the corpus holds, the narrowest putting far more breaks to the renderer than the default does.
#[test]
fn every_source_in_the_tree_formats_at_every_width_and_holds_there() {
    let faults = at_every_admitted_format(|format| {
        sources_of_the_tree()
            .into_iter()
            .filter_map(|(path, text)| {
                let once = match Formatted::from_source(
                    &Source::held(path.clone(), text),
                    WrittenIn::of(&path),
                    format,
                ) {
                    Ok(Formatted::Unchanged(text) | Formatted::Changed(text)) => text,
                    Err(refusal) => {
                        return Some(format!(
                            "{}: {refusal}",
                            path.display(),
                            refusal = refusal.render()
                        ));
                    }
                };
                match Formatted::from_source(
                    &Source::held(path.clone(), once.as_str()),
                    WrittenIn::of(&path),
                    format,
                ) {
                    Ok(Formatted::Unchanged(_)) => None,
                    Ok(Formatted::Changed(_)) => {
                        Some(format!("{}: a second run changes it", path.display()))
                    }
                    Err(refusal) => Some(format!(
                        "{}: its own output was refused: {refusal}",
                        path.display(),
                        refusal = refusal.render()
                    )),
                }
            })
            .collect::<Vec<_>>()
    })
    .concat();

    assert!(faults.is_empty(), "{}", faults.join("\n"));
}

/// A match is kept on one line within its format's flat match and broken past it: by its span, which `0` closes to every match, and by its arms.
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
                ..Format::default()
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
                ..Format::default()
            },
        )
    };
    assert!(
        !kept(&wide(DEFAULT_FLAT_MATCH_ARMS)),
        "four arms past the default"
    );
    assert!(
        kept(&wide(MAX_FLAT_MATCH_ARMS)),
        "four arms within the most"
    );
}

/// The bound on a flat match's arms is the most a line shows: the narrowest match the printer writes, one with no head, stays on a line [`MAX_LINE_WIDTH`] wide at [`MAX_FLAT_MATCH_ARMS`] arms, and at one more arm no line holds it, whatever its count admitted.
#[test]
fn the_most_arms_a_flat_match_keeps_is_the_most_a_line_shows() {
    let narrowest = |arms: usize| {
        let guarded = " | b => 0".repeat(arms - 1);
        format!("let b: /std/Bool = true;\n\nmatch{guarded} | _ => 0 end\n")
    };
    let line_width = Width::default();
    let at = |arms: usize| Format {
        line_width,
        flat_match: FlatMatch {
            width: MAX_LINE_WIDTH,
            arms,
        },
        ..Format::default()
    };
    let kept = |text: &str| {
        text.lines()
            .any(|line| line.starts_with("match |") && line.ends_with(" end"))
    };

    let most = narrowest(MAX_FLAT_MATCH_ARMS);
    assert!(
        kept(&formatted_within(&most, at(MAX_FLAT_MATCH_ARMS))),
        "{most}"
    );
    let past = formatted_within(
        &narrowest(MAX_FLAT_MATCH_ARMS + 1),
        at(MAX_FLAT_MATCH_ARMS + 1),
    );
    assert!(!kept(&past), "{past}");
    assert!(FlatMatch::within(line_width, None, Some(MAX_FLAT_MATCH_ARMS)).is_ok());
    let (setting, refusal) = FlatMatch::within(line_width, None, Some(MAX_FLAT_MATCH_ARMS + 1))
        .expect_err("one arm past the most");
    assert_eq!(setting, FlatMatchSetting::Arms);
    assert!(refusal.contains("at most 10 arms"), "{refusal}");
}
