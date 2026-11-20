//! The entrypoint: judged at the type it states, and refused when it states none.

use {
    super::test_support::*,
    crate::{test_support::*, *},
};

/// An entry stating no type is refused rather than inferred: elaboration writes the type it judged the body at, so an entry without one did not come through it, and inferring one would accept a program against a contract nobody checked. Mutation-checked: letting an untyped entry through accepts it.
#[test]
fn an_entry_stating_no_type_is_refused() {
    let verdicts = text_refusals("0");

    assert!(
        refuses(&verdicts, None, |error| matches!(
            error,
            Error::UntypedEntry
        )),
        "an entry with no type was certified: {verdicts:?}",
    );
}

/// An entry is checked against the type it states — the program contract a compile supplies, or an embedder's own — so a body that does not inhabit it is refused. The control states the body's own type.
#[test]
fn an_entry_is_checked_against_the_type_it_states() {
    let verdicts = text_refusals("0\n: #BoolType");
    assert!(
        refuses(&verdicts, None, |error| {
            matches!(error, Error::Mismatch { inferred, expected }
                if **inferred == term("#NatType")
                    && **expected == term("#BoolType"))
        }),
        "a `Nat` entry stated at `Bool` was certified: {verdicts:?}",
    );
    assert_eq!(text_refusals("0\n: #NatType"), Vec::new());
}
