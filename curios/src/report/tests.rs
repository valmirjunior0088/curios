//! How the paths and the counts a report names are spelled.

use {super::*, std::path::Path};

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
