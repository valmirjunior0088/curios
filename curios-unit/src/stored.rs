//! The stored unit: a record of what a unit was compiled from, framed ahead of the unit in one file.
//!
//! Two producers write this format, and one sits below the other. The cabinet files a [`Unit`](crate::Unit) this way under a slot, from `curios-verdict` above the pipeline, and the fixed prelude's build images each of its roots this way ([`Unit::imaged`](crate::Unit::imaged)), from `curios-prelude`, below the pipeline and so below every cabinet. So the format is stated once, here, below both: what a record holds, how it is framed ahead of the artifact, and how the two are read back apart. What *verifies* a record — whether the files it names still hold their text, whether the units it names still contain what they did — is the cabinet's business and stays there. This crate says what a record is, never whether one may be believed.

#[cfg(test)]
mod tests;

use {
    curios_archive::{Bytes, parted},
    curios_common::{Blob, Digest, Source, SourceName, digest},
    std::{path::PathBuf, sync::Arc},
};

/// What a stored unit must still be true of to be believed.
///
/// Every field is a fact the compilation depended on and the address deliberately does not carry. Verification is all of them or nothing: a record that cannot be read, or that disagrees anywhere, is a miss.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Record {
    /// Each file the compilation read, by the name its package gives it, with the digest of the text that was parsed from it, in the order of the names ([`recorded`]). A name and no path: a record is the same bytes wherever its package stands, and whoever verifies it resolves each name through the packages of the source asking.
    pub reads: Vec<(SourceName, String)>,
    /// What each unit the compilation could have read contained: the units it may name and the ones those may name in turn, each by the prefixes it claims and the digest of the bytes its own slot holds, in the order of those prefixes. A unit outside them is not named, so what it holds decides nothing here.
    pub after: Vec<(String, String)>,
    /// The `foreign` declarations the unit's package said a module answers when it was compiled, each beside the manifest row that names it, as its source carried them. A claim is held to a declaration of the unit where the unit is compiled or restored, and a program's stored payload is taken with no unit compiled or restored at all: recorded here, a manifest whose claims have changed since is a disagreement, so the claims it holds now are held as any are.
    pub answers: Vec<(String, String)>,
    /// The digest of the stored unit's index, so a damaged unit that still deserializes is a miss rather than a belief, as the payload family's own digest makes a damaged artifact. Bytecheck confirms a unit's structure and nothing about its contents: a flipped byte inside a string reads back as a different string. The index names every blob the unit stores on its own by its digest, so this vouches for them too, each held to its pack's name where it is read. Concurrent filing is not its concern: a slot is one file renamed into place, after the blobs it names, so no write can leave a record beside a unit it was not made from.
    pub unit: String,
}

impl Record {
    /// The record of a unit compiled from `reads`, as [`recorded`] spells them, after units containing `after`, under a package claiming `answers`, whose own bytes digest to `unit`.
    pub fn of(
        reads: Vec<(SourceName, String)>,
        after: Vec<(String, String)>,
        answers: Vec<(String, String)>,
        unit: String,
    ) -> Self {
        Self {
            reads,
            after,
            answers,
            unit,
        }
    }
}

/// A read log as a record spells it: each file by the name its package gives it, with the digest of the text that was parsed from it, in the order of the names. `None` where a file was read for no package, which no record can name: a unit that read one has no place in a cabinet.
///
/// **A name resolves only to a file of the asking source's own packages**, which is what keeps a cabinet shared across projects from handing one project a unit compiled from another's files: a slot two projects address alike is believed only where this project's files hold the texts it was compiled from, and so is the unit this project would compile.
pub fn recorded(reads: &[(PathBuf, Arc<Source>)]) -> Option<Vec<(SourceName, String)>> {
    let mut recorded = reads
        .iter()
        .map(|(_, source)| Some((source.name()?.clone(), digest(source.text().as_bytes()))))
        .collect::<Option<Vec<_>>>()?;
    recorded.sort();

    Some(recorded)
}

/// What a slot file opens with, so a slot is told from anything else by its first bytes rather than by guessing where a record might end. Versioned by the compiler its address names rather than here: a slot another compiler framed is not found rather than found and misread.
const MAGIC: &[u8; 8] = b"crslot\0\0";

/// The bytes of one slot file: the magic, the record's length, the record, then the artifact.
///
/// The record goes first because it is the part every open decodes and the artifact is the part a payload probe never does; the length is ahead of it because an archive is read from its end and so does not know its own extent.
pub fn framed(record: &[u8], artifact: &[u8]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(MAGIC.len() + 8 + record.len() + artifact.len());
    bytes.extend_from_slice(MAGIC);
    bytes.extend_from_slice(&(record.len() as u64).to_le_bytes());
    bytes.extend_from_slice(record);
    bytes.extend_from_slice(artifact);

    bytes
}

/// How many bytes open a slot file ahead of its record: the magic, then the record's length.
pub const PREAMBLE: usize = MAGIC.len() + 8;

/// The length of the record a slot file frames, read off the file's first [`PREAMBLE`] bytes, or `None` where they open no slot.
///
/// The one place a slot's framing is read: [`segments`] reads a slot in hand through it, and a reader that decides whether to believe a slot reads its record alone through it, leaving the artifact unread behind a record that disagrees.
pub fn record_length(preamble: &[u8; PREAMBLE]) -> Option<usize> {
    let length = preamble.strip_prefix(MAGIC)?;
    let length = <[u8; 8]>::try_from(length).ok()?;

    usize::try_from(u64::from_le_bytes(length)).ok()
}

/// The record and the artifact of the slot file `bytes`, or `None` for bytes that are not one: something else entirely, or a slot truncated past its record.
///
/// The artifact segment is the same bytes the artifact is archived as on its own, so a reader that wants the unit and nothing about it takes the second half and decodes it as it would any archive.
pub fn segments(bytes: &[u8]) -> Option<(&[u8], &[u8])> {
    let (preamble, rest) = bytes.split_first_chunk::<PREAMBLE>()?;
    let length = record_length(preamble)?;

    (length <= rest.len()).then(|| rest.split_at(length))
}

/// The bytes of a unit's artifact: the unit as what is compiled after it restores it, then what it stores apart from itself, read by whoever asks — behind the first's length, since each half is an archive read from its end.
///
/// **Two halves by reader, in the one segment a record vouches for.** A slot's record digests its artifact, so both halves are believed together, and a slot stays one file replaced by one rename. What differs is who decodes which: opening a unit decodes the first and keeps the second as bytes.
///
/// What a unit stores apart is a part for each reader, every one behind its length ([`Unit::stored`](crate::Unit::stored), [`apart_parts`]): the digests of its lowered items, its erased forms and its elaborated items, each a blob of the cabinet, found where the unit is restored and read by whoever names its declaration or links it; where its declarations lie, read by a report placed in one of them; then what only a later compile of the same unit or a command about it reads: its import scopes, and its baseline half, a part for each stage that keeps a record, the text's, the elaborator's and erasure's.
pub fn halved(unit: &[u8], baseline: &[u8]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(8 + unit.len() + baseline.len());
    bytes.extend_from_slice(&(unit.len() as u64).to_le_bytes());
    bytes.extend_from_slice(unit);
    bytes.extend_from_slice(baseline);

    bytes
}

/// The two halves `halved` laid out in `bytes`, or `None` for bytes that are no such layout. The one place `halved`'s layout is read.
pub fn halves(bytes: &[u8]) -> Option<(&[u8], &[u8])> {
    let (length, rest) = bytes.split_first_chunk::<8>()?;
    let length = usize::try_from(u64::from_le_bytes(*length)).ok()?;

    (length <= rest.len()).then(|| rest.split_at(length))
}

/// The thirteen parts of what a unit stores apart from itself ([`Unit::stored`](crate::Unit::stored)), each a run of the buffer `stored` is in: the digests of its lowered items, of its erased forms, of its elaborated items, of what each declaration asked as it elaborated, of what erasing each item read, of the lowering's records and scans, of its import scopes and of each source's texts (`digests`), where its declarations lie, which a report reads, then what only a compile of the same unit or a command about it reads: its import scopes, the text's part of its baseline half, and the rest of the elaborator's and erasure's. `None` for bytes that are no such layout, one cut short among them: every part states its length, and together they are the bytes.
pub fn apart_parts(stored: &Bytes) -> Option<[Bytes; 13]> {
    parted(stored)?.try_into().ok()
}

/// `digests`, one after another: how an index names the blobs a unit stores on its own, by what each holds and nothing of where it is kept.
pub fn digests<'d>(digests: impl IntoIterator<Item = &'d Digest>) -> Vec<u8> {
    digests
        .into_iter()
        .flat_map(|digest| *digest.as_bytes())
        .collect()
}

/// The blobs `named` names ([`digests`]), each found by `find`. `Err` for bytes that are no list of digests, or one `find` holds no blob for.
pub fn found(named: &Bytes, find: &dyn Fn(&Digest) -> Option<Blob>) -> Result<Vec<Blob>, String> {
    let (digests, rest) = named.as_slice().as_chunks::<32>();
    if !rest.is_empty() {
        return Err("not a list of digests".to_string());
    }

    digests
        .iter()
        .map(|digest| {
            let digest = Digest::from_bytes(*digest);

            find(&digest).ok_or_else(|| format!("no blob {digest:?} the unit names is held"))
        })
        .collect()
}
