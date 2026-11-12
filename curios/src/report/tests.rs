//! How the paths and the counts a report names are spelled, and what a painted line is beneath its paint.

use {super::*, std::path::Path};

#[test]
fn a_painted_line_stripped_of_its_codes_is_the_plain_one() {
    assert_eq!(plain("demo"), "demo");
    assert_eq!(
        plain("\x1b[2m├─\x1b[0m check; \x1b[31mrefused\x1b[0m \x1b[2min 0.3s\x1b[0m\n"),
        "├─ check; refused in 0.3s\n"
    );
    assert_eq!(plain("\x1b[1mdemo\x1b[0m"), "demo");
}

#[test]
fn only_what_went_wrong_is_worded_as_trouble() {
    for verdict in ["refused", "failed", "trapped", "exited 0", "exited 7"] {
        assert!(trouble(verdict), "{verdict}");
    }
    for verdict in ["passed", "1 goal", "3 goals"] {
        assert!(!trouble(verdict), "{verdict}");
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
