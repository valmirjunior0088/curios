//! What another crate's suite reads of the reader of Core text and no reading does: its refusals by name, which the soundness board's records name a refusal by.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {crate::Refusal, curios_common::variants};

variants!(Refusal {
    ProgramForm,
    UnknownField,
    RepeatedField,
    RepeatedLevelPart,
    UnknownNode,
    Malformed,
    UnknownIntrinsic,
    RelativeName,
    NoScope,
    Undeclared,
});
