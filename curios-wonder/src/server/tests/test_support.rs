//! What the suites of `server` share: a server spoken to as an editor speaks to it, and the documents it holds.

use {
    crate::{server::*, *},
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

/// The document every job here is raised for.
pub fn scratch() -> PathBuf {
    PathBuf::from("/scratch.crs")
}

/// What the editor holds once `scratch` reads `text`.
pub fn overlay(text: &str) -> BTreeMap<PathBuf, String> {
    BTreeMap::from([(scratch(), text.to_string())])
}

/// A check of `program`, the program standing for a document: what the server's check answers and leaves behind it.
pub fn check(program: &str) -> Diagnosed {
    crate::checked(
        DEFAULT_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                text: program.to_string(),
                written: WrittenIn::Program,
            },
            declares: None,
            package: None,
        },
        &Overlay::default(),
        None,
        Path::new("<stdin>"),
        &mut |_| {},
    )
}

/// What a check of `program` leaves for a place to be answered from.
pub fn checked_program(program: &str) -> Placed {
    check(program).placed.expect("the program lowers")
}

/// The declaration named at the `nth` occurrence of `written` in `buffer`, by what a check of `program` left: by the path it is answered by.
pub fn named(program: &str, buffer: &str, written: &str, nth: usize) -> Option<String> {
    let (offset, _) = buffer
        .match_indices(written)
        .nth(nth)
        .expect("the buffer writes it");

    match answered_at(buffer, &checked_program(program), offset)?
        .0
        .stands
    {
        Stands::Declaration { described, .. } => Some(described.asked),
        Stands::Local { .. }
        | Stands::Field { .. }
        | Stands::Module { .. }
        | Stands::Term { .. } => None,
    }
}

pub const CHECKED: &str = "\
let one: /std/Nat = 1;
let two: /std/Nat = one;
let three: /std/Nat = two;
/std/print(\"\")
";
