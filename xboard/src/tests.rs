//! The board, held by the suite as `cargo xboard report` holds it.

use {
    super::*,
    std::{collections::BTreeSet, path::Path},
};

/// The suite fails where `cargo xboard report` would.
#[test]
fn the_board_holds() {
    let report = Report::run(PARTS);

    assert!(!report.fails(), "\n{report}");
}

/// The name a ticket's file takes from its title: lowercase, a backtick or an apostrophe dropped, and every other run of what is not a letter or a digit one `_`.
fn filed_as(title: &str) -> String {
    let mut name = String::new();
    for character in title
        .chars()
        .filter(|character| !matches!(character, '`' | '\''))
    {
        match character.is_ascii_alphanumeric() {
            true => name.push(character.to_ascii_lowercase()),
            false if !name.ends_with('_') => name.push('_'),
            false => {}
        }
    }

    name.trim_matches('_').to_string()
}

/// Each part's directory holds one file per ticket, named for its title, and nothing else: a ticket's file is found by reading its title, and a file no index names is a ticket the board never puts.
#[test]
fn every_ticket_is_filed_under_its_title() {
    for part in PARTS {
        let titled = part
            .tickets
            .iter()
            .map(|ticket| format!("{}.rs", filed_as(ticket.title)))
            .collect::<BTreeSet<_>>();
        let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join(part.name);
        let filed = match directory.read_dir() {
            Ok(entries) => entries
                .map(|entry| {
                    entry
                        .expect("a readable entry")
                        .file_name()
                        .to_string_lossy()
                        .into_owned()
                })
                .collect::<BTreeSet<_>>(),
            Err(_) => BTreeSet::new(),
        };

        assert_eq!(filed, titled, "the files of the part {}", part.name);
    }
}
