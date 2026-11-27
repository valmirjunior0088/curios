//! How the paths and the counts a report names are spelled, what a piece of a line holds beside its paint, how each drawing draws one record, the live drawing frame by frame on a screen that reads the controls it writes, and which drawing a command is made in.

use {
    super::*,
    curios_native::test_support::*,
    std::{
        env,
        ffi::OsStr,
        path::Path,
        process::{Command, Stdio},
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
            tested: false,
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
            tested: true,
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Check),
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
            tested: false,
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
        "· tests",
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
            tested: true,
        },
        Event::Named {
            tree: program,
            subject: "app".to_string(),
            manifest: None,
            tested: false,
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Check),
            last: false,
        },
        Event::Began {
            tree: program,
            node: Node::Step(Step::Check),
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
        "Checking `/lib` · tests\nChecking `app`\nChecked `app`: refused in 0.1s\nChecked `/lib` · tests in 0.1s\n"
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
            tested: false,
        },
        step(unit, Step::Check, false),
        done(unit),
        Event::Settled,
        step(unit, Step::Store, true),
        done(unit),
        Event::Named {
            tree: program,
            subject: "app".to_string(),
            manifest: None,
            tested: false,
        },
        step(program, Step::Check, false),
        done(program),
        step(program, Step::Build, false),
        done(program),
        step(program, Step::Emit, true),
        done(program),
    ]
}

/// The screen `events` leave, told a tenth of a second apart on `TERMINAL`, then `finished` a tenth after the last or interrupted there.
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
        screen.write(&live.finish(at(start, tenths + 1)).shown);
    }

    screen
}

/// The screen a finished command leaves is the tree drawing's bytes, then the tally frozen beneath a blank line: nothing the status showed is left behind.
#[test]
fn a_finished_command_leaves_the_trees_then_the_tally() {
    let mut trees = Trees::default();
    let treed = one_at_a_time()
        .into_iter()
        .map(|event| trees.draw(event).plain)
        .collect::<String>();

    assert_eq!(
        screen(one_at_a_time(), true).shown(),
        format!("{treed}\n1 of 1 units; 1.5s\n")
    );
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

/// A frame erases from the cursor, writes what is for good, then the status with autowrap off, each line clipped at the terminal's width, and brings the cursor back over the lines it drew to the start of the first: where what is written for good ends, which the next frame erases from.
#[test]
fn a_frame_writes_the_status_unwrapped_and_returns_to_its_start() {
    let start = Instant::now();
    let mut live = Live::new(start);
    let unit = Opened::minted();
    let narrow = (12, 24);
    let mut screen = Screen::default();
    for event in [
        Event::Planned { units: 2 },
        Event::Named {
            tree: unit,
            subject: "/app".to_string(),
            manifest: None,
            tested: false,
        },
    ] {
        screen.write(&live.draw(event, start, narrow).shown);
    }
    let began = live.draw(
        Event::Began {
            tree: unit,
            node: Node::Step(Step::Check),
            last: false,
        },
        start,
        narrow,
    );

    assert_eq!(
        began.shown,
        "\x1b[J/app\n\x1b[?7l├─ check; 0.\r\n0 of 2 units\x1b[?7h\x1b[1A\r"
    );
    assert_eq!(began.above.plain, "/app\n");
    screen.write(&began.shown);
    screen.write("x");
    assert_eq!(screen.shown(), "/app\nx─ check; 0.\n0 of 2 units\n");
    // Where the time shows, a tick redraws the status, erasing the last from where the cursor came back to.
    assert!(
        live.tick(at(start, 3), TERMINAL)
            .starts_with("\x1b[J\x1b[?7l")
    );
}

/// A status line is clipped by the cells its clusters take: a wide character that would cross the terminal's edge is left out whole.
#[test]
fn a_status_line_is_clipped_by_the_cells_its_clusters_take() {
    let start = Instant::now();
    let mut live = Live::new(start);
    let mut screen = Screen::default();
    let unit = Opened::minted();
    let size = (28, 24);
    for event in [
        Event::Named {
            tree: unit,
            subject: "/app".to_string(),
            manifest: None,
            tested: false,
        },
        Event::Began {
            tree: unit,
            node: Node::Step(Step::Check),
            last: false,
        },
        Event::Walked {
            tree: unit,
            done: 1,
            of: 2,
            on: Some("日本".to_string()),
        },
    ] {
        screen.write(&live.draw(event, start, size).shown);
    }
    screen.write(&live.tick(at(start, 3), size));

    assert_eq!(screen.shown(), "/app\n├─ check; 0.3s · 1 of 2 · `\n");
}

/// What a signal or a panic writes in the finish's place is what the finish would write then: the status erased, the open node's line ended, the tally frozen; once the command has finished, it is nothing.
#[test]
fn a_farewell_is_what_the_finish_would_write() {
    let begun = one_at_a_time()[..3].to_vec();
    let start = Instant::now();
    let mut interrupted = Live::new(start);
    let mut finished = Live::new(start);
    let mut screen = Screen::default();
    for (tenths, event) in (1..).zip(begun) {
        screen.write(
            &interrupted
                .draw(event.clone(), at(start, tenths), TERMINAL)
                .shown,
        );
        finished.draw(event, at(start, tenths), TERMINAL);
    }
    let farewell = interrupted.farewell(at(start, 5));
    let finish = finished.finish(at(start, 5));

    assert_eq!(farewell, finish.shown);
    screen.write(&farewell);
    assert_eq!(screen.shown(), "/app\n├─ check\n\n0 of 1 units; 0.5s\n");
    assert_eq!(finish.above.plain, "├─ check\n\n0 of 1 units; 0.5s\n");
    assert_eq!(finished.farewell(at(start, 6)), "");
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
            tested: false,
        },
        Event::Began {
            tree: library,
            node: Node::Step(Step::Check),
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
            tested: false,
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
            tested: false,
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
            tested: false,
        },
        Event::Began {
            tree: unit,
            node: Node::Step(Step::Check),
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

/// What the child half of [`a_panic_writes_the_farewell_ahead_of_what_it_says`] runs under, named by the variable its parent sets.
const PANICKED_LIVE: &str = "CURIOS_TEST_PANICKED_LIVE";

/// A panic in a live command erases the status and writes what the finish would have, the open node's line ended and the tally, ahead of what the panic says, which would otherwise land on the status where the cursor rests. A test cannot outlive the panic it means, so its child panics, run again from the test's own binary.
#[test]
fn a_panic_writes_the_farewell_ahead_of_what_it_says() {
    if env::var_os(PANICKED_LIVE).is_some() {
        transcribe(Drawing::Live);
        let unit = Opened::minted();
        for event in [
            Event::Planned { units: 1 },
            Event::Named {
                tree: unit,
                subject: "/app".to_string(),
                manifest: None,
                tested: false,
            },
            Event::Began {
                tree: unit,
                node: Node::Step(Step::Check),
                last: false,
            },
        ] {
            tell(event);
        }
        panic!("a panic on purpose");
    }

    let ended = Command::new(env::current_exe().unwrap())
        .args([
            "--exact",
            "report::tests::a_panic_writes_the_farewell_ahead_of_what_it_says",
            "--nocapture",
        ])
        .env(PANICKED_LIVE, "1")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .expect("the test runs again as a child");
    let mut screen = Screen::default();
    screen.write(&String::from_utf8_lossy(&ended.stderr));
    let shown = screen.shown();

    assert!(!ended.status.success(), "{shown}");
    let mut lines = shown.lines();
    assert_eq!(lines.next(), Some("/app"), "{shown}");
    assert_eq!(lines.next(), Some("├─ check"), "{shown}");
    assert_eq!(lines.next(), Some(""), "{shown}");
    assert!(
        lines
            .next()
            .is_some_and(|tally| tally.starts_with("0 of 1 units; ")),
        "{shown}"
    );
    // What the panic says follows, whatever blank line the hook that says it leads with.
    let mut said = lines.skip_while(|line| line.is_empty());
    assert!(
        said.next()
            .is_some_and(|line| line.starts_with("thread '") && line.contains("panicked at")),
        "{shown}"
    );
    assert_eq!(said.next(), Some("a panic on purpose"), "{shown}");
}

/// The status is drawn for the terminal's size as it reports it, and for eighty columns and twenty-four rows in any measure it reports as nothing or where there is no terminal to ask.
#[test]
fn a_terminal_that_reports_no_size_is_drawn_for_eighty_by_twenty_four() {
    assert_eq!(sized(Some((120, 40))), (120, 40));
    assert_eq!(sized(Some((0, 0))), (80, 24));
    assert_eq!(sized(Some((120, 0))), (120, 24));
    assert_eq!(sized(None), (80, 24));
}

/// The status is gone before what writes past the drawing, and nothing of it is ever written for good.
#[test]
fn the_status_is_cleared_before_a_program_writes_and_never_kept() {
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
