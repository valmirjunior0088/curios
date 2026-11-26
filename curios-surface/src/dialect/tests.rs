//! What a text is written in, read off a file's name.

use super::*;

/// A `.crsc` is Curios/Core, and every other name a program, one with no extension among them.
#[test]
fn a_file_is_core_text_by_its_crsc_extension_alone() {
    for (name, written) in [
        ("proof.crsc", WrittenIn::CoreText),
        ("dir/proof.crsc", WrittenIn::CoreText),
        ("main.crs", WrittenIn::Program),
        ("script", WrittenIn::Program),
        ("notes.txt", WrittenIn::Program),
        ("proof.crsc.crs", WrittenIn::Program),
    ] {
        assert_eq!(WrittenIn::of(Path::new(name)), written, "{name}");
    }
}

/// Each dialect's extension names it back, and no other extension names one.
#[test]
fn an_extension_names_the_dialect_whose_files_take_it() {
    for written in WrittenIn::EVERY {
        assert_eq!(WrittenIn::of_extension(written.extension()), Some(written));
    }
    assert_eq!(WrittenIn::of_extension("txt"), None);
}
