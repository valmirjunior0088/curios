//! How the paths and the counts a report names are spelled, and what a piece of a line holds beside its paint.

use {super::*, std::path::Path};

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
