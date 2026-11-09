//! What another crate's suites read of how a unit is stored that nothing a build stores reads: a record of what was read, filed whole as one piece.

use {
    crate::{digests, reads_indexed},
    curios_common::Recorded,
    curios_core::Read,
};

/// The bytes a record of what was read is filed as where it is a constant of the executable, as the prelude's certification files it: its own part, the digest of each definition's reads and the one run holding them all, behind their lengths, so it is restored as a store's is and nothing of it is read until a walk asks.
pub fn reads_filed(recorded: &Recorded<Read>) -> Result<Vec<u8>, String> {
    let (table, kind) = reads_indexed(recorded)?;
    let digests = digests(kind.digests());
    let run = kind
        .objects(&|_| true)?
        .first()
        .map(|member| member.bytes.as_slice().to_vec())
        .unwrap_or_default();

    Ok(curios_archive::joined(&[
        &table[..],
        &digests[..],
        &run[..],
    ]))
}
