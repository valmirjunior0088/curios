//! One slot, one file: the framing that puts a record ahead of an artifact, the rename that replaces a slot whole, and the reader that takes a unit back out of one — or out of a bare archive, which is what the prelude image is.
//!
//! **A slot is one file, and it is replaced by one rename.** The record and the artifact are framed together ([`framed`]), written beside the slot and renamed into place, so no interrupted or concurrent write can leave a record beside an artifact it was not made from: a reader sees the old slot, the new one, or none. The artifact segment is the same bytes the artifact is archived as on its own, which is what lets `curios document` read a unit off a slot exactly as it reads one off the prelude image.

use {
    curios_unit::Unit,
    std::{fs, io, path::Path},
};

/// What a slot file opens with, so a slot is told from a bare archive by its first bytes rather than by guessing where a record might end. Versioned in the address's schema tag rather than here: a slot written under an older framing is not found rather than found and misread.
const MAGIC: &[u8; 8] = b"crslot\0\0";

/// File `record` and `artifact` as the slot at `slot`, replacing whatever it held.
///
/// **Written beside and renamed into place, which is the whole of it.** The file is complete before it bears the slot's name, and a rename replaces one name atomically, so a reader — this compiler or another filing the same slot at the same moment — sees the old slot, the new one, or none, and never a record beside an artifact it was not made from. The two-file scheme this replaced ordered its writes so that every interrupted state read as a miss, and needed the record to carry the artifact's digest to survive two compilers racing on one slot; one rename removes both cases rather than arguing about them. It is what every mature toolchain's cache does, and for this reason.
///
/// The staging name carries the process id, so two compilers staging one slot at once stage two files and the second rename simply wins. A staging file a crash leaves behind is never read, since it is not a slot's name, and is overwritten by the next write from the same process.
///
/// Shared by both families, so the argument holds in one place rather than twice: a payload slot and a unit slot differ in what they hold and in nothing about how it is put there.
pub(crate) fn replace(slot: &Path, record: &[u8], artifact: &[u8]) -> io::Result<()> {
    // Every failure names the slot it happened at: an error reading `Permission denied` alone leaves a reader guessing which of the five families under `.curios/` refused.
    let at = |error: io::Error| io::Error::other(format!("{}: {error}", slot.display()));

    let family = slot
        .parent()
        .ok_or_else(|| io::Error::other("a slot has a family"))
        .map_err(at)?;
    fs::create_dir_all(family).map_err(at)?;

    let staged = slot.with_extension(format!("{}.part", std::process::id()));
    fs::write(&staged, framed(record, artifact)).map_err(at)?;

    fs::rename(&staged, slot).map_err(|error| {
        // Best effort, as the whole write is: what matters is that nothing bearing the slot's name is half of anything.
        let _ = fs::remove_file(&staged);
        at(error)
    })
}

/// The bytes of one slot file: the magic, the record's length, the record, then the artifact.
///
/// The record goes first because it is the part every open decodes and the artifact is the part a payload probe never does; the length is ahead of it because an archive is read from its end and so does not know its own extent.
fn framed(record: &[u8], artifact: &[u8]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(MAGIC.len() + 8 + record.len() + artifact.len());
    bytes.extend_from_slice(MAGIC);
    bytes.extend_from_slice(&(record.len() as u64).to_le_bytes());
    bytes.extend_from_slice(record);
    bytes.extend_from_slice(artifact);

    bytes
}

/// The record and the artifact of the slot file `bytes`, or `None` for bytes that are not one: something else entirely, or a slot truncated past its record.
///
/// The one place a slot's framing is read, shared with [`archived_unit`], which takes the artifact and nothing else.
pub(crate) fn segments(bytes: &[u8]) -> Option<(&[u8], &[u8])> {
    let body = bytes.strip_prefix(MAGIC)?;
    let (length, rest) = body.split_first_chunk::<8>()?;
    let length = usize::try_from(u64::from_le_bytes(*length)).ok()?;

    (length <= rest.len()).then(|| rest.split_at(length))
}

/// The unit archived at `path`: a verdict slot under a store, or the prelude image. A slot frames a record ahead of the unit and the image is the unit alone, and the unit is archived the same way in both, so the one difference is where it starts. Validated before it is read, so a file that is not a unit is an error rather than undefined behaviour.
///
/// What is read is the unit and nothing about it: no record is checked, because no compilation is about to believe anything on its strength. `curios document` is the consumer, reading an interface off a unit that was filed without compiling it again.
pub fn archived_unit(path: &Path) -> Result<Unit, String> {
    let filed = fs::read(path).map_err(|error| format!("{}: {error}", path.display()))?;
    let bytes = segments(&filed).map_or(filed.as_slice(), |(_, artifact)| artifact);

    curios_archive::from_bytes::<Unit>(bytes)
        .map_err(|error| format!("{}: not an archived unit: {error}", path.display()))
}
