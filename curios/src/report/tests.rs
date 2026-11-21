//! How the paths and the counts a report names are spelled, what a piece of a line holds beside its paint, how each drawing draws one record, and which drawing a command is made in.

use {
    super::*,
    std::{ffi::OsStr, path::Path},
};

/// A piece holds what its reader is shown and, beside it, what is kept: painted where the reader sees colour, the text itself in the kept form, and an escape sequence the text carries is the text's own in both.
#[test]
fn a_piece_is_built_as_its_reader_sees_it_and_plain() {
    let coloured = Style {
        here: None,
        painted: true,
    };
    let line = Piece::painted(&coloured, "├─", |style, text| style.dim(text))
        .then(" check; ")
        .then(Piece::painted(&coloured, "refused", |style, text| {
            style.troubled(text)
        }))
        .then("\n");
    assert_eq!(
        line.shown,
        "\x1b[2m├─\x1b[0m check; \x1b[31mrefused\x1b[0m\n"
    );
    assert_eq!(line.plain, "├─ check; refused\n");

    // Where the reader sees no colour the two forms are one.
    let unpainted = Piece::painted(&Style::plain(), "demo", |style, text| style.bold(text));
    assert_eq!(unpainted.shown, unpainted.plain);

    // A name that carries an escape sequence is kept as it was written, where stripping the painted line would take it for paint.
    let named = Piece::painted(&coloured, "a\x1b[1mb", |style, text| style.bold(text));
    assert_eq!(named.plain, "a\x1b[1mb");
    assert_eq!(named.shown, "\x1b[1ma\x1b[1mb\x1b[0m");

    let joined = Piece::joined(
        [
            Piece::from("1 passed"),
            Piece::painted(&coloured, "1 failed", |style, text| style.troubled(text)),
        ],
        ", ",
    );
    assert_eq!(joined.plain, "1 passed, 1 failed");
    assert_eq!(joined.shown, "1 passed, \x1b[31m1 failed\x1b[0m");
}

#[test]
fn only_what_went_wrong_is_worded_as_trouble() {
    for ending in ["refused", "failed", "trapped", "exited 0", "exited 7"] {
        assert!(trouble(ending), "{ending}");
    }
    for ending in ["passed", "1 goal", "3 goals"] {
        assert!(!trouble(ending), "{ending}");
    }
}

#[test]
fn a_count_is_spelled_with_its_thousands_apart() {
    assert_eq!(grouped(0), "0");
    assert_eq!(grouped(999), "999");
    assert_eq!(grouped(4312), "4,312");
    assert_eq!(grouped(1_000_000), "1,000,000");
    assert_eq!(grouped(12_345_678), "12,345,678");
}

/// A manifest beside the invocation goes unsaid, one above it is a climb from where the reader stands, and one off to the side is its own path.
#[test]
fn a_manifest_is_spelled_from_where_the_invocation_stands() {
    let manifest = Path::new("/work/app/curios.toml");

    assert_eq!(spelled_from(manifest, Path::new("/work/app")), None);
    assert_eq!(
        spelled_from(manifest, Path::new("/work/app/serve")).as_deref(),
        Some("../curios.toml")
    );
    assert_eq!(
        spelled_from(manifest, Path::new("/work/app/serve/deep")).as_deref(),
        Some("../../curios.toml")
    );
    assert_eq!(
        spelled_from(manifest, Path::new("/elsewhere")).as_deref(),
        Some("/work/app/curios.toml")
    );
}

/// The lines a node came to stand beneath its closing in one block: beside the trunk where its tree goes on, and in the trunk's column where the node was the last.
#[test]
fn a_nodes_lines_beneath_are_drawn_with_its_closing() {
    let mut trees = Trees::default();
    let tree = Opened::minted();
    let drawn = [
        Event::Named {
            tree,
            subject: "/app".to_string(),
            manifest: None,
        },
        Event::Began {
            tree,
            node: Node::Step(Step::Store),
            last: false,
        },
        Event::Closed {
            tree,
            closing: Closing::Ended("refused".to_string(), Duration::ZERO),
            beneath: vec![Piece::from("the cabinet is read-only")],
        },
        Event::Stood {
            tree,
            step: Step::Pin,
            last: true,
            stands: Piece::from("recorded"),
            beneath: vec![Piece::from("rev: a → b"), Piece::from("hash: c → d")],
        },
    ]
    .into_iter()
    .map(|event| trees.draw(event).plain)
    .collect::<String>();

    assert_eq!(
        drawn,
        "/app\n├─ store; refused in 0.0s\n│    the cabinet is read-only\n└─ pin; recorded\n     rev: a → b\n     hash: c → d\n"
    );
}

/// What `curios test` tells of a library checked and refused a place in the cabinet, then built into the program its tests run in, whose one test writes and fails.
fn record() -> Vec<Event> {
    let library = Opened::minted();
    let program = Opened::minted();
    let tenth = Duration::from_millis(100);

    vec![
        Event::Named {
            tree: library,
            subject: "/app".to_string(),
            manifest: Some("../curios.toml".to_string()),
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Check { tested: true }),
            last: false,
        },
        Event::Closed {
            tree: library,
            closing: Closing::Done(tenth),
            beneath: Vec::new(),
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Store),
            last: true,
        },
        Event::Closed {
            tree: library,
            closing: Closing::Ended("refused".to_string(), tenth),
            beneath: vec![Piece::from("the cabinet is read-only")],
        },
        Event::Left { tree: library },
        Event::Named {
            tree: program,
            subject: "/app".to_string(),
            manifest: None,
        },
        Event::Began {
            tree: program,
            node: Node::Test("/app/draws".to_string()),
            last: false,
        },
        Event::Tested {
            tree: program,
            ending: "failed".to_string(),
            taken: tenth,
            place: Some("lib.crs:3:6".to_string()),
            wrote: Wrote::Nothing,
            beneath: vec!["not equal".to_string()],
        },
        Event::Summed {
            tree: program,
            sum: Piece::from("0 passed, 1 failed"),
            taken: Some(tenth),
        },
    ]
}

/// Each drawing says every fact one record states: the subject and its manifest, each step and test, how each ended and what it took, what stands beneath, the tally.
#[test]
fn both_drawings_say_every_fact_of_one_record() {
    let mut trees = Trees::default();
    let mut lines = Lines::default();
    let treed = record()
        .into_iter()
        .map(|event| trees.draw(event).plain)
        .collect::<String>();
    let plain = record()
        .into_iter()
        .map(|event| lines.draw(event).plain)
        .collect::<String>();

    for fact in [
        "/app",
        "../curios.toml",
        "with its tests",
        "refused in 0.1s",
        "the cabinet is read-only",
        "/app/draws",
        "failed in 0.1s; lib.crs:3:6",
        "not equal",
        "0 passed, 1 failed",
    ] {
        assert!(treed.contains(fact), "`{fact}` drawn as trees:\n{treed}");
        assert!(plain.contains(fact), "`{fact}` drawn plain:\n{plain}");
    }
}

/// Two subjects open at once interleave a line each event, every line naming its subject and standing alone, and each subject's lines keep its own order.
#[test]
fn a_plain_line_names_its_subject_whatever_is_open_beside_it() {
    let mut lines = Lines::default();
    let library = Opened::minted();
    let program = Opened::minted();
    let tenth = Duration::from_millis(100);
    let drawn = [
        Event::Named {
            tree: library,
            subject: "/lib".to_string(),
            manifest: None,
        },
        Event::Named {
            tree: program,
            subject: "app".to_string(),
            manifest: None,
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Check { tested: true }),
            last: false,
        },
        Event::Began {
            tree: program,
            node: Node::Step(Step::Check { tested: false }),
            last: false,
        },
        Event::Closed {
            tree: program,
            closing: Closing::Ended("refused".to_string(), tenth),
            beneath: Vec::new(),
        },
        Event::Closed {
            tree: library,
            closing: Closing::Done(tenth),
            beneath: Vec::new(),
        },
    ]
    .into_iter()
    .map(|event| lines.draw(event).plain)
    .collect::<String>();

    assert_eq!(
        drawn,
        "Checking `/lib` with its tests\nChecking `app`\nChecked `app`: refused in 0.1s\nChecked `/lib` with its tests in 0.1s\n"
    );
}

/// The drawing is what the command line asks, else what the environment names, else live on a terminal that is no dumb one and that no continuous integration runs, and plain everywhere else.
#[test]
fn a_drawing_is_asked_then_named_then_read_off_standard_error() {
    let plain = Some(OsStr::new("plain"));
    let live = Some(OsStr::new("live"));
    let xterm = Some(OsStr::new("xterm-256color"));
    let dumb = Some(OsStr::new("dumb"));

    // Asked, whatever else is so.
    assert_eq!(
        Drawing::chosen(Some(Drawing::Plain), live, true, xterm, false),
        Ok(Drawing::Plain)
    );
    assert_eq!(
        Drawing::chosen(Some(Drawing::Live), plain, false, None, true),
        Ok(Drawing::Live)
    );
    // Named, on a terminal and in a pipe alike.
    assert_eq!(
        Drawing::chosen(None, plain, true, xterm, false),
        Ok(Drawing::Plain)
    );
    assert_eq!(
        Drawing::chosen(None, live, false, None, false),
        Ok(Drawing::Live)
    );
    assert_eq!(
        Drawing::chosen(None, Some(OsStr::new("tree")), true, xterm, false),
        Err("tree".to_string())
    );
    // Read off standard error.
    assert_eq!(
        Drawing::chosen(None, None, true, xterm, false),
        Ok(Drawing::Live)
    );
    assert_eq!(
        Drawing::chosen(None, None, true, None, false),
        Ok(Drawing::Live)
    );
    assert_eq!(
        Drawing::chosen(None, None, false, xterm, false),
        Ok(Drawing::Plain)
    );
    assert_eq!(
        Drawing::chosen(None, None, true, dumb, false),
        Ok(Drawing::Plain)
    );
    assert_eq!(
        Drawing::chosen(None, None, true, xterm, true),
        Ok(Drawing::Plain)
    );
}

/// A test's ending stands where what the test wrote left its node's line: in the tree on the node's own line where it wrote nothing, and beneath what it wrote, its mark in the quote's column, where it did, after the end of a line it left open; plain on a line of its own, after the end of a line it left open.
#[test]
fn a_tests_ending_stands_after_what_it_wrote() {
    let tree = Opened::minted();
    let tested = |wrote| Event::Tested {
        tree,
        ending: "passed".to_string(),
        taken: Duration::ZERO,
        place: None,
        wrote,
        beneath: Vec::new(),
    };
    let mut trees = Trees::default();
    assert_eq!(
        trees.draw(tested(Wrote::Nothing)).plain,
        "; ✓ passed in 0.0s\n"
    );
    assert_eq!(
        trees.draw(tested(Wrote::Lines)).plain,
        "│  ✓ passed in 0.0s\n"
    );
    assert_eq!(
        trees.draw(tested(Wrote::Open)).plain,
        "\n│  ✓ passed in 0.0s\n"
    );

    let mut lines = Lines::default();
    let mut ended = |wrote| {
        lines.draw(Event::Began {
            tree,
            node: Node::Test("/app/draws".to_string()),
            last: false,
        });
        lines.draw(tested(wrote)).plain
    };
    assert_eq!(ended(Wrote::Lines), "Tested `/app/draws`: passed in 0.0s\n");
    assert_eq!(
        ended(Wrote::Open),
        "\nTested `/app/draws`: passed in 0.0s\n"
    );
}
