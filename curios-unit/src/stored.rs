//! The stored unit: a record of what a unit was compiled from, framed ahead of the unit in one file.
//!
//! Two producers write this format and neither may depend on the other. The store files a unit this way under a slot, from `curios-verdicts` above the pipeline; the fixed prelude is imaged this way by `curios-prelude-archive`'s build script, which sits below every store. So the format is stated once, here, below both: what a record holds, how it is framed ahead of the artifact, and how the two are read back apart. What *verifies* a record — whether the files it names still hold their text, whether the chain still agrees — is the store's business and stays there. This crate says what a record is, never whether one may be believed.

#[cfg(test)]
mod tests;

use {
    crate::Unit,
    curios_utilities::{Source, digest},
    std::{
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// What a stored unit must still be true of to be believed.
///
/// Every field is a fact the compilation depended on and the address deliberately does not carry. Verification is all of them or nothing: a record that cannot be read, or that disagrees anywhere, is a miss.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Record {
    /// Each file the compilation read, by canonical path, with the digest of the text that was parsed from it. Sorted, because `RootSource::reads` collects its vector out of a `BTreeMap`.
    pub reads: Vec<(String, String)>,
    /// What each predecessor contained, in fold order — the digest of the bytes its own slot holds. Ordered for the same reason the address orders their slots: two orders of one set are two lowerings.
    pub predecessors: Vec<String>,
    /// The stored unit's own digest, so a damaged unit that still deserializes is a miss rather than a belief, as the payload family's own digest makes a damaged artifact. Bytecheck confirms a unit's structure and nothing about its contents: a flipped byte inside a string reads back as a different string. It no longer stands between two compilers filing one slot at once — a slot is one file renamed into place, so no write can leave a record beside a unit it was not made from.
    pub unit: String,
}

impl Record {
    /// The record of a unit compiled from `reads` after predecessors containing `predecessors`, whose own bytes digest to `unit`.
    pub fn of(reads: Vec<(PathBuf, Rc<Source>)>, predecessors: Vec<String>, unit: String) -> Self {
        Self {
            reads: digested(reads),
            predecessors,
            unit,
        }
    }

    /// The directory the unit was compiled from: the parent of the shallowest read, which by the layout rule is the header beside the manifest — or `None` for a record of no reads, which is a unit supplied whole.
    ///
    /// Read off the record rather than written beside it. A record is the compiler's own account of what it read; a directory carried beside it would be a second account, and two accounts of one fact can disagree.
    pub fn directory(&self) -> Option<&Path> {
        self.reads
            .iter()
            .map(|(path, _)| Path::new(path))
            .min_by_key(|path| path.components().count())
            .and_then(Path::parent)
    }
}

/// A stored unit restored: the record beside the unit it vouches for.
///
/// Two values rather than one, because they are consumed apart: the unit is what a compilation is folded over, and the record is what the one consumer that asks where a unit came from reads.
pub struct Stored {
    pub record: Record,
    pub unit: Unit,
}

/// A read log as a record spells it: each file by canonical path, with the digest of the text that was parsed from it.
pub fn digested(reads: Vec<(PathBuf, Rc<Source>)>) -> Vec<(String, String)> {
    reads
        .into_iter()
        .map(|(path, text)| {
            (
                path.to_string_lossy().into_owned(),
                digest(text.text.as_bytes()),
            )
        })
        .collect()
}

/// Whether every file in `reads` lies under one of `directories`.
///
/// This is what keeps a shared store from admitting across projects. A slot's address carries no file contents, so two projects that each hold a package of one name, compiled by one compiler after one chain, address the same slot; without this, the second opens the first's record, finds the first's files unchanged on disk because nothing touched them, and is handed a unit compiled from source it has never seen. Checking containment rather than re-deriving the read set keeps the check exact: a git dependency is materialized once under the shared store and read from that same path by every project, so genuine sharing survives.
pub fn read_within(directories: &[&Path], reads: &[(String, String)]) -> bool {
    // Canonical on both sides, because a record's paths are canonical and a source's directories are however the manifest walk spelled them.
    let within = directories
        .iter()
        .map(|directory| {
            directory
                .canonicalize()
                .unwrap_or_else(|_| directory.to_path_buf())
        })
        .collect::<Vec<_>>();

    reads.iter().all(|(path, _)| {
        within
            .iter()
            .any(|directory| Path::new(path).starts_with(directory))
    })
}

/// What a slot file opens with, so a slot is told from anything else by its first bytes rather than by guessing where a record might end. Versioned in the address's schema tag rather than here: a slot written under an older framing is not found rather than found and misread.
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

/// The record and the artifact of the slot file `bytes`, or `None` for bytes that are not one: something else entirely, or a slot truncated past its record.
///
/// The one place a slot's framing is read. The artifact segment is the same bytes the artifact is archived as on its own, so a reader that wants the unit and nothing about it takes the second half and decodes it as it would any archive.
pub fn segments(bytes: &[u8]) -> Option<(&[u8], &[u8])> {
    let body = bytes.strip_prefix(MAGIC)?;
    let (length, rest) = body.split_first_chunk::<8>()?;
    let length = usize::try_from(u64::from_le_bytes(*length)).ok()?;

    (length <= rest.len()).then(|| rest.split_at(length))
}
