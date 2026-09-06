//! The one spelling two paths to a file share.

use {super::identity, std::path::Path};

/// A file that does not exist is spelled by its canonical parent and its name, and a bare name's parent is the current directory — not the empty path, which canonicalizes to nothing and left the relative name as given, so every caller read it as a path with no directory at all.
#[test]
fn a_bare_name_the_disk_does_not_hold_is_spelled_under_the_current_directory() {
    let here = std::env::current_dir().unwrap().canonicalize().unwrap();

    assert_eq!(
        identity(Path::new("curios-does-not-exist.crs")),
        here.join("curios-does-not-exist.crs")
    );
    assert_eq!(
        identity(Path::new("src/curios-does-not-exist.crs")),
        here.join("src").join("curios-does-not-exist.crs")
    );
}
