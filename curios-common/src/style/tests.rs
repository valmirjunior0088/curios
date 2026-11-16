//! How a path is spelled from where a reader stands, and what paint leaves beneath it.

use {
    super::*,
    crate::{Report, Said},
    std::path::Path,
};

/// A path is spelled by the climb from where the reader stands and the descent to it, and one that shares only the root with there is told whole.
#[test]
fn a_path_is_spelled_by_the_climb_and_the_descent_to_it() {
    let built = Path::new("/work/app/.curios/native/app/app");

    assert_eq!(
        between(built, Path::new("/work/app")),
        ".curios/native/app/app"
    );
    assert_eq!(
        between(built, Path::new("/work/app/serve/deep")),
        "../../.curios/native/app/app"
    );
    assert_eq!(
        between(built, Path::new("/work/other")),
        "../app/.curios/native/app/app"
    );
    assert_eq!(
        between(built, Path::new("/elsewhere")),
        "/work/app/.curios/native/app/app"
    );
    assert_eq!(between(Path::new("/work/app"), Path::new("/work/app")), ".");
}

#[test]
fn a_reader_standing_nowhere_is_spelled_every_path_as_it_is_held() {
    assert_eq!(
        Style::plain().spelled(Path::new("/work/app/lib.crs")),
        "/work/app/lib.crs"
    );
    assert_eq!(Style::plain().spelled(Path::new("lib.crs")), "lib.crs");
}

#[test]
fn paint_is_written_only_for_a_reader_who_sees_it() {
    let painted = Style {
        here: None,
        painted: true,
    };

    assert_eq!(Style::plain().troubled("error"), "error");
    assert_eq!(painted.troubled("error"), "\x1b[31merror\x1b[0m");
    assert_eq!(painted.bold("type mismatch"), "\x1b[1mtype mismatch\x1b[0m");
    assert_eq!(painted.dim("│"), "\x1b[2m│\x1b[0m");
}

/// A painted text reads back into the roles it was painted in, and what is plain between them stays plain.
#[test]
fn a_painted_text_reads_back_into_its_roles() {
    let painted = Style {
        here: None,
        painted: true,
    };
    let text = format!(
        "{} {}{}",
        painted.bold("refused"),
        painted.dim("│"),
        painted.troubled("[error]")
    );

    assert_eq!(
        runs(&text),
        [
            (Some(Role::Bold), "refused".to_string()),
            (None, " ".to_string()),
            (Some(Role::Dim), "│".to_string()),
            (Some(Role::Troubled), "[error]".to_string()),
        ]
    );
}

/// A report's runs, joined, are the report a reader who sees no colour reads: paint is a layer.
#[test]
fn a_reports_runs_join_to_its_plain_rendering() {
    let mut said = Said::line("something refused");
    said.stating("what was asked of it");
    let report = Report::of(None, said);
    let painted = Style {
        here: None,
        painted: true,
    };

    let read = runs(&report.rendered(Kind::Error, &painted));

    assert!(read.iter().any(|(role, _)| *role == Some(Role::Troubled)));
    assert_eq!(
        read.into_iter().map(|(_, text)| text).collect::<String>(),
        report.rendered(Kind::Error, &Style::plain())
    );
}
