//! How the paths and the counts a report names are spelled, what a piece of a line holds beside its paint, how each drawing draws one record, the live drawing frame by frame on a screen that reads the controls it writes, and which drawing a command is made in.

use {
    super::*,
    curios_native::test_support::*,
    std::{
        ffi::OsStr,
        path::Path,
        time::{Duration, Instant},
    },
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

/// A terminal of eighty columns and thirty rows.
const TERMINAL: (u16, u16) = (80, 30);

/// `tenths` of a second past `start`.
fn at(start: Instant, tenths: u64) -> Instant {
    start + Duration::from_millis(100 * tenths)
}

/// A unit's tree checked then stored, and a program's built over it: what `run` tells of a package, one subject open at a time, each event a tenth of a second after the one before.
fn one_at_a_time() -> Vec<Event> {
    let unit = Opened::minted();
    let program = Opened::minted();
    let step = |tree, step, last| Event::Began {
        tree,
        node: Node::Step(step),
        last,
    };
    let done = |tree| Event::Closed {
        tree,
        closing: Closing::Done(Duration::from_millis(100)),
        beneath: Vec::new(),
    };

    vec![
        Event::Planned { units: 1 },
        Event::Named {
            tree: unit,
            subject: "/app".to_string(),
            manifest: None,
        },
        step(unit, Step::Check { tested: false }, false),
        done(unit),
        Event::Settled,
        step(unit, Step::Store, true),
        done(unit),
        Event::Named {
            tree: program,
            subject: "app".to_string(),
            manifest: None,
        },
        step(program, Step::Check { tested: false }, false),
        done(program),
        step(program, Step::Build, false),
        done(program),
        step(program, Step::Emit, true),
        done(program),
    ]
}

/// The screen `events` leave, told a tenth of a second apart on `TERMINAL`, `finished` or interrupted after the last.
fn screen(events: Vec<Event>, finished: bool) -> Screen {
    let start = Instant::now();
    let mut live = Live::new(start);
    let mut screen = Screen::default();
    let mut tenths = 0;
    for event in events {
        tenths += 1;
        screen.write(&live.draw(event, at(start, tenths), TERMINAL).shown);
    }
    if finished {
        screen.write(&live.finish().shown);
    }

    screen
}

/// With one subject open at a time, the screen a finished command leaves is the tree drawing's bytes: the region gone, nothing it showed left behind.
#[test]
fn one_subject_at_a_time_leaves_the_trees_and_nothing_else() {
    let mut trees = Trees::default();
    let treed = one_at_a_time()
        .into_iter()
        .map(|event| trees.draw(event).plain)
        .collect::<String>();

    assert_eq!(screen(one_at_a_time(), true).shown(), treed);
}

/// Mid-run, the running node stands beneath what is settled, as it will read when it closes, its time after `;`, over the count of what is done; an interrupted command leaves that frame.
#[test]
fn a_running_node_reads_as_it_will_when_it_closes() {
    let events = one_at_a_time();
    // The plan, the unit named, its check begun.
    let begun = events[..3].to_vec();

    assert_eq!(
        screen(begun, false).shown(),
        "/app\n├─ check; 0.0s\n0 of 1 units; 0.3s\n"
    );
}

/// A subject that begins while an older one is open waits in the region, its tree so far beneath the older one's running node; it is written above, whole and in its turn, once the older one ends, even where it ended first.
#[test]
fn a_subject_that_ends_behind_an_older_one_is_written_in_its_turn() {
    let older = Opened::minted();
    let younger = Opened::minted();
    let named = |tree, subject: &str| Event::Named {
        tree,
        subject: subject.to_string(),
        manifest: None,
    };
    let check = |tree| Event::Began {
        tree,
        node: Node::Step(Step::Check { tested: false }),
        last: true,
    };
    let done = |tree| Event::Closed {
        tree,
        closing: Closing::Done(Duration::from_millis(100)),
        beneath: Vec::new(),
    };
    let start = Instant::now();
    let mut live = Live::new(start);
    let mut screen = Screen::default();
    for (event, tenths) in [
        (named(older, "/lib"), 0),
        (check(older), 1),
        (named(younger, "app"), 2),
        (check(younger), 3),
        (done(younger), 5),
    ] {
        screen.write(&live.draw(event, at(start, tenths), TERMINAL).shown);
    }
    assert_eq!(
        screen.shown(),
        "/lib\n└─ check; 0.4s\napp\n└─ check; 0.1s\n",
        "the younger subject ended, and waits, settled, in the region"
    );

    screen.write(&live.draw(done(older), at(start, 6), TERMINAL).shown);
    assert_eq!(
        screen.shown(),
        "/lib\n└─ check; 0.1s\n\napp\n└─ check; 0.1s\n",
        "both are written above in the order they opened, a blank line between, and no region is left"
    );

    // Both ended, so the next subject streams: what closes of it is written for good at once.
    let next = Opened::minted();
    let mut kept = String::new();
    for (event, tenths) in [(named(next, "/next"), 7), (check(next), 8), (done(next), 9)] {
        let frame = live.draw(event, at(start, tenths), TERMINAL);
        screen.write(&frame.shown);
        kept.push_str(&frame.above.plain);
    }
    assert_eq!(kept, "\n/next\n└─ check; 0.1s\n");
    assert_eq!(
        screen.shown(),
        "/lib\n└─ check; 0.1s\n\napp\n└─ check; 0.1s\n\n/next\n└─ check; 0.1s\n"
    );
}

/// Each tree is a block parted from what stands above it by one blank line, as is a line under no subject, and a blank line already there is not doubled.
#[test]
fn a_tree_is_parted_from_what_stands_above_it_by_one_blank_line() {
    let mut trees = Trees::default();
    let library = Opened::minted();
    let program = Opened::minted();
    let tenth = Duration::from_millis(100);
    let drawn = [
        Event::Named {
            tree: library,
            subject: "/app".to_string(),
            manifest: None,
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Check { tested: false }),
            last: true,
        },
        Event::Closed {
            tree: library,
            closing: Closing::Ended("refused".to_string(), tenth),
            beneath: Vec::new(),
        },
        Event::Apart,
        Event::Said {
            text: Piece::from("type mismatch\n└─ [error] lib.crs:1:1"),
        },
        Event::Named {
            tree: program,
            subject: "app".to_string(),
            manifest: None,
        },
        Event::Stood {
            tree: program,
            step: Step::Write,
            last: true,
            stands: Piece::from("app.wasm"),
            beneath: Vec::new(),
        },
        Event::Total {
            text: Piece::from("1 written"),
            taken: None,
        },
    ]
    .into_iter()
    .map(|event| trees.draw(event).plain)
    .collect::<String>();

    assert_eq!(
        drawn,
        "/app\n└─ check; refused in 0.1s\n\ntype mismatch\n└─ [error] lib.crs:1:1\n\napp\n└─ write; app.wasm\n\n1 written\n"
    );

    // As `check` parts its trees from the answer it writes on standard output, then counts it.
    let mut trees = Trees::default();
    let unit = Opened::minted();
    let linted = [
        Event::Named {
            tree: unit,
            subject: "/app".to_string(),
            manifest: None,
        },
        Event::Stood {
            tree: unit,
            step: Step::Format,
            last: true,
            stands: Piece::from("rewritten"),
            beneath: Vec::new(),
        },
        Event::Apart,
        Event::Total {
            text: Piece::from("2 lints"),
            taken: None,
        },
    ]
    .into_iter()
    .map(|event| trees.draw(event).plain)
    .collect::<String>();

    assert_eq!(linted, "/app\n└─ format; rewritten\n\n2 lints\n");
}

/// More open subjects than a third of the terminal holds are counted, not shown.
#[test]
fn a_region_holds_a_third_of_the_terminal_and_counts_the_rest() {
    let start = Instant::now();
    let mut live = Live::new(start);
    let mut screen = Screen::default();
    for subject in 0..8 {
        let tree = Opened::minted();
        let named = Event::Named {
            tree,
            subject: format!("/unit{subject}"),
            manifest: None,
        };
        let began = Event::Began {
            tree,
            node: Node::Step(Step::Check { tested: false }),
            last: true,
        };
        screen.write(&live.draw(named, start, (80, 12)).shown);
        screen.write(&live.draw(began, start, (80, 12)).shown);
    }

    // The streaming subject's line above, and a region of a third of the twelve rows beneath it.
    assert_eq!(
        screen.shown(),
        "/unit0\n└─ check; 0.0s\n/unit1\n└─ check; 0.0s\n+ 6 more\n"
    );
}

/// A running check shows behind `·` how far it has come, drawn at the clock's next tick rather than at each declaration; neither drawing writes any of it for good.
#[test]
fn a_running_check_shows_how_far_it_has_come_at_the_next_tick() {
    let start = Instant::now();
    let mut live = Live::new(start);
    let mut screen = Screen::default();
    let mut kept = String::new();
    let unit = Opened::minted();
    let walked = || Event::Walked {
        tree: unit,
        done: 1_416,
        of: 2_040,
        on: Some("/app/insert".to_string()),
    };
    for event in [
        Event::Named {
            tree: unit,
            subject: "/app".to_string(),
            manifest: None,
        },
        Event::Began {
            tree: unit,
            node: Node::Step(Step::Check { tested: false }),
            last: false,
        },
    ] {
        let frame = live.draw(event, start, TERMINAL);
        screen.write(&frame.shown);
        kept.push_str(&frame.above.plain);
    }

    let told = live.draw(walked(), at(start, 3), TERMINAL);
    assert_eq!(told.shown, "", "a walk's event waits for the clock");
    screen.write(&live.tick(at(start, 3), TERMINAL));
    assert_eq!(
        screen.shown(),
        "/app\n├─ check; 0.3s · 1,416 of 2,040 · `/app/insert`\n"
    );
    assert_eq!(kept, "/app\n");
    assert_eq!(Lines::default().draw(walked()).plain, "");
    assert_eq!(Trees::default().draw(walked()).plain, "");
}

/// A row wider than the terminal is cut at its end, `…` in its last column.
#[test]
fn a_row_wider_than_the_terminal_ends_in_an_ellipsis() {
    assert_eq!(cut("├─ check; 2.4s", 80), "├─ check; 2.4s");
    assert_eq!(cut("├─ check; 2.4s", 8), "├─ chec…");
    assert_eq!(cut("日本語", 4), "日…");
}

/// A region is drawn for the terminal's size as it reports it, and for eighty columns and twenty-four rows in any measure it reports as nothing or where there is no terminal to ask.
#[test]
fn a_terminal_that_reports_no_size_is_drawn_for_eighty_by_twenty_four() {
    assert_eq!(sized(Some((120, 40))), (120, 40));
    assert_eq!(sized(Some((0, 0))), (80, 24));
    assert_eq!(sized(Some((120, 0))), (120, 24));
    assert_eq!(sized(None), (80, 24));
}

/// The region is gone before what writes past the drawing, and nothing of it is ever written for good.
#[test]
fn the_region_is_cleared_before_a_program_writes_and_never_kept() {
    let events = one_at_a_time();
    let start = Instant::now();
    let mut live = Live::new(start);
    let mut screen = Screen::default();
    let mut kept = String::new();
    for (tenths, event) in (0..).zip(events[..3].iter().cloned()) {
        let frame = live.draw(event, at(start, tenths), TERMINAL);
        screen.write(&frame.shown);
        kept.push_str(&frame.above.plain);
    }
    assert!(screen.shown().contains("├─ check; "), "{}", screen.shown());
    assert_eq!(kept, "/app\n", "a running node is no line written for good");

    screen.write(&live.settle());
    screen.write("ok\n");
    assert_eq!(screen.shown(), "/app\nok\n");
}
