//! What the manifest suites share: a manifest parsed, or its refusal read.

use super::*;

/// The manifest `source` states, or the panic of it having been refused.
pub fn parse(source: &str) -> Manifest {
    source
        .parse()
        .expect("a manifest this test states is legal")
}

/// What the refusal `source` earns says, its tag and each fact beneath it, or the panic of it having been accepted.
pub fn refuse(source: &str) -> String {
    refused(source).said.to_string()
}

/// The refusal `source` earns, or the panic of it having been accepted.
pub fn refused(source: &str) -> Refused {
    source
        .parse::<Manifest>()
        .expect_err("a manifest this test states is refused")
}
