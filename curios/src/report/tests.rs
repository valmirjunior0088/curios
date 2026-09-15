//! How a build's header names the manifest that governs it.

use {super::spelled_from, std::path::Path};

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
