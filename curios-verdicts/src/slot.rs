//! One slot, one file: the rename that replaces a slot whole, and the read that opens one as far as its record. The framing itself, a record ahead of an artifact, is `curios-unit`'s.
//!
//! **A slot is one file, and it is replaced by one rename.** The record and the artifact are framed together (`curios_unit::framed`), written beside the slot and renamed into place, so no interrupted or concurrent write can leave a record beside an artifact it was not made from: a reader sees the old slot, the new one, or none.
//!
//! **A slot is read as far as its record first.** Whether a slot may be believed is decided by its record, which is small, and what is behind it is read only where the record agrees: a question about text an editor holds edited misses on the record alone, and the artifact behind it stays unread.

use {
    curios_unit::{PREAMBLE, framed, record_length},
    std::{
        fs::{self, File},
        io::{self, Read},
        path::Path,
    },
};

/// A slot opened as far as its record, the artifact behind it unread until something asks for it.
pub(crate) struct Opened {
    file: File,
    record: Vec<u8>,
}

impl Opened {
    /// The slot's record, as it was framed.
    pub(crate) fn record(&self) -> &[u8] {
        &self.record
    }

    /// The artifact behind the record, read now: `None` where it cannot be read.
    pub(crate) fn artifact(mut self) -> Option<Vec<u8>> {
        let mut artifact = Vec::new();
        self.file.read_to_end(&mut artifact).ok()?;

        Some(artifact)
    }
}

/// The slot at `slot`, read as far as its record: `None` where there is no file there, or the file opens no slot, or its record runs past its end.
pub(crate) fn opened(slot: &Path) -> Option<Opened> {
    let mut file = File::open(slot).ok()?;
    let size = file.metadata().ok()?.len();
    let mut preamble = [0; PREAMBLE];
    file.read_exact(&mut preamble).ok()?;
    let length = record_length(&preamble)?;
    // A length no file of this size could hold is a damaged slot, refused before it is allocated for.
    if u64::try_from(length).ok()? > size.saturating_sub(PREAMBLE as u64) {
        return None;
    }
    let mut record = vec![0; length];
    file.read_exact(&mut record).ok()?;

    Some(Opened { file, record })
}

/// File `record` and `artifact` as the slot at `slot`, replacing whatever it held.
///
/// **Written beside and renamed into place, which is the whole of it.** The file is complete before it bears the slot's name, and a rename replaces one name atomically, so a reader — this compiler or another filing the same slot at the same moment — sees the old slot, the new one, or none, and never a record beside an artifact it was not made from. A record and an artifact in two files would need their writes ordered so that every interrupted state reads as a miss, and the record to carry the artifact's digest to survive two compilers racing on one slot; one rename removes both cases rather than arguing about them.
///
/// The staging name carries the process id, so two compilers staging one slot at once stage two files and the second rename simply wins. A staging file a crash leaves behind is never read, since it is not a slot's name, and is overwritten by the next write from the same process.
///
/// Shared by both families, so the argument holds in one place rather than twice: a payload slot and a unit slot differ in what they hold and in nothing about how it is put there.
pub(crate) fn replace(slot: &Path, record: &[u8], artifact: &[u8]) -> io::Result<()> {
    // Every failure names the slot it happened at: an error reading `Permission denied` alone leaves a reader guessing which family under `.curios/` refused.
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
