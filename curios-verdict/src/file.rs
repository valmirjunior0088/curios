//! One record, one file: the rename that replaces an object's `unit` file or a payload whole, the read that opens one as far as its record, and the link that keeps the record a filing replaces. The framing itself, a record ahead of an artifact, is `curios-unit`'s.
//!
//! **Who filed a file is where it lies.** A file stands in an object under its compiler's name (`curios_package::Cabinet::objects_of`), so a reader opens what its own compiler filed and nothing another did, and a record another build laid out differently is never read: no file names its filer.
//!
//! **A file is replaced by one rename.** The record and the artifact are written beside the file and renamed into place, so no interrupted or concurrent write can leave a record beside an artifact it was not made from: a reader sees the old file, the new one, or none.
//!
//! **A file is read as far as its record first.** Whether a file may be believed is decided by its record, which is small, and what is behind it is read only where the record agrees: a question about text an editor holds edited misses on the record alone, and the artifact behind it stays unread.
//!
//! **What a filer stopped short of is swept where it was staged** ([`sweep`]): a staging file is never renamed by anyone but the process that wrote it, so one that process left is taken away once it is older than any filing takes — a unit's file's and a pack's by the collection of the object's directory they stand in, a payload's by the filing that lands a payload beside it.

use {
    curios_unit::{PREAMBLE, framed, record_length},
    std::{
        collections::BTreeSet,
        fs::{self, File},
        io::{self, Read},
        path::{Path, PathBuf},
        time::{Duration, SystemTime},
    },
};

/// What a staging file's name ends with: a file written whole beside the name it is renamed onto, which nothing reads under this one.
pub const STAGING: &str = "part";

/// How long a staging file stands before a filing beside it takes it away ([`sweep`]): longer than any filing takes, so a filer still writing one keeps it. The one time the cabinet keeps, and a crash's: no blob, record or object is kept or dropped by age.
pub const STALE: Duration = Duration::from_secs(24 * 60 * 60);

/// A file opened as far as its record, the artifact behind it unread until something asks for it.
pub struct Opened {
    file: File,
    record: Vec<u8>,
}

impl Opened {
    /// The file's record, as it was framed.
    pub fn record(&self) -> &[u8] {
        &self.record
    }

    /// The artifact behind the record, read now: `None` where it cannot be read.
    pub fn artifact(mut self) -> Option<Vec<u8>> {
        let mut artifact = Vec::new();
        self.file.read_to_end(&mut artifact).ok()?;

        Some(artifact)
    }
}

/// The file at `path`, read as far as its record: `None` where there is no file there, the file is not framed, or its record runs past its end.
pub fn opened(path: &Path) -> Option<Opened> {
    let mut file = File::open(path).ok()?;
    let size = file.metadata().ok()?.len();
    let mut preamble = [0; PREAMBLE];
    file.read_exact(&mut preamble).ok()?;
    let length = record_length(&preamble)?;
    // A length no file of this size could hold is a damaged file, refused before it is allocated for.
    if u64::try_from(length).ok()? > size.saturating_sub(PREAMBLE as u64) {
        return None;
    }
    let mut record = vec![0; length];
    file.read_exact(&mut record).ok()?;

    Some(Opened { file, record })
}

/// File `record` and `artifact` as the file at `path`, replacing whatever it held.
///
/// **Written beside and renamed into place, which is the whole of it.** The file is complete before it bears its name, and a rename replaces one name atomically, so a reader — this compiler or another filing the same file at the same moment — sees the old file, the new one, or none, and never a record beside an artifact it was not made from. A record and an artifact in two files would need their writes ordered so that every interrupted state reads as a miss, and the record to carry the artifact's digest to survive two compilers racing on one file; one rename removes both cases rather than arguing about them.
///
/// The staging name carries the process id, so two compilers staging one file at once stage two files and the second rename simply wins. A staging file a crash leaves behind is never read, since it bears no name a file is read under; the collection of an object takes one away in its directory, and the filing that lands a payload one beside it ([`sweep`]).
///
/// Shared by both families, so the argument holds in one place rather than twice: a payload and an object's `unit` file differ in what they hold and in nothing about how it is put there.
pub fn replace(path: &Path, record: &[u8], artifact: &[u8]) -> io::Result<()> {
    // Every failure names the file it happened at: an error reading `Permission denied` alone leaves a reader guessing which family under `.curios/` refused.
    let at = |error: io::Error| io::Error::other(format!("{}: {error}", path.display()));

    let family = path
        .parent()
        .ok_or_else(|| io::Error::other("a filed file has a directory"))
        .map_err(at)?;
    fs::create_dir_all(family).map_err(at)?;

    let staged = path.with_extension(format!("{}.{STAGING}", std::process::id()));
    fs::write(&staged, framed(record, artifact)).map_err(at)?;

    fs::rename(&staged, path).map_err(|error| {
        // Best effort, as the whole write is: what matters is that nothing bearing the file's name is half of anything.
        let _ = fs::remove_file(&staged);
        at(error)
    })
}

/// Keep the file at `path` under `aside` as well, before a filing replaces it: linked under a staging name and renamed onto `aside`, so the bytes it holds stay whole under the second name while the first takes another file, and a reader of `aside` sees the old file or this one. Nothing where no file is at `path`.
pub fn set_aside(path: &Path, aside: &Path) -> io::Result<()> {
    let staged = aside.with_extension(format!("{}.{STAGING}", std::process::id()));
    match fs::hard_link(path, &staged) {
        Ok(()) => fs::rename(&staged, aside).inspect_err(|_| {
            let _ = fs::remove_file(&staged);
        }),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error),
    }
}

/// Take away every staging file in `directory` last written [`STALE`] or more before `now`: what a filer that stopped short left, since no filing takes that long and no other process renames it into place.
pub fn sweep(directory: &Path, now: SystemTime) -> io::Result<()> {
    for entry in fs::read_dir(directory)?.flatten() {
        let staged = entry.path();
        let part = staged
            .extension()
            .is_some_and(|extension| extension == STAGING);
        if part && aged(&staged, now) {
            let _ = fs::remove_file(&staged);
        }
    }

    Ok(())
}

/// Take away every entry of `directory` but `kept`, a staging only once it is [`STALE`] at `now`: a name holding a dot, the extension or the prefix of what a filer, a fetch or a pin has yet to rename into place.
pub fn keep(directory: &Path, kept: impl IntoIterator<Item = PathBuf>, now: SystemTime) {
    let kept = kept.into_iter().collect::<BTreeSet<_>>();
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let staging = entry.file_name().to_string_lossy().contains('.');
        if kept.contains(&path) || (staging && !aged(&path, now)) {
            continue;
        }
        let _ = match entry.file_type() {
            Ok(kind) if kind.is_dir() => fs::remove_dir_all(&path),
            _ => fs::remove_file(&path),
        };
    }
}

/// Whether what is at `path` was last written [`STALE`] or more before `now`.
pub fn aged(path: &Path, now: SystemTime) -> bool {
    fs::metadata(path)
        .and_then(|metadata| metadata.modified())
        .is_ok_and(|written| now.duration_since(written).unwrap_or_default() >= STALE)
}
