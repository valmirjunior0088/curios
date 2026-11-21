//! The packs at one address, in its object's own directory beside the object's records: the blobs the object names, written before the object names them ([`Packs::write`]), found by name ([`Packs::find`]), and kept geometric and collected where a filing lands ([`Packs::collect`]).
//!
//! **Written whole and renamed into place, as a unit file is.** A pack is complete before it bears its name, and its name is the digest of its bytes, so a reader sees a pack whole or not at all, and two filers packing the same blobs write one file twice.
//!
//! **A pack is held to its name where a blob of it is read.** Its bytes are digested once, the first time anything of it is asked for, which vouches for every blob it holds. A pack whose bytes are not its name is damaged, since nothing renames a pack into place before it is whole: it is taken away, and a blob only it held is not found and is written again by the next filing that names it.
//!
//! **What a filing writes is decided by the packs' ends.** Which blobs a pack holds is read off its table ([`pack_names`]), so a filing reads no pack whole to know what it lacks.
//!
//! **Collected at every filing, with no clock.** A filing keeps the blobs its object's two records name, its own and the one it replaced. A pack at least half of whose blobs neither names is written again holding the rest, and taken away where it holds nothing else; and the smallest packs are merged into one, keeping what the records name, wherever each no longer holds at least twice the blobs of the one before it. So the address holds a logarithmic number of packs, a blob is written again a logarithmic number of times over its life, and every blob a rewrite carries is paid for by one that went. A blob no record names, where its pack is less than half such, stays until it is, and no compile takes it. The runs of each kind a rewrite holds are run again into one ([`repacked`]), so a pack keeps one table a kind however it was made.
//!
//! **A pack another filer merged away is looked for again.** The packs are listed once for a handle, and again where a blob is not found among them, so a merge that renamed what a reader was about to read is a second listing, never a miss.
//!
//! **What a filer stopped short of is swept where a filing lands**: a staging file [`STALE`](crate::STALE) old ([`sweep`]). A read writes nothing.
//!
//! Why an object's blobs are kept in packs at its address, merged so and collected so, is the crate `README.md`'s.

#[cfg(test)]
mod tests;

use {
    crate::{STAGING, sweep},
    curios_archive::Bytes,
    curios_common::{Blob, Digest},
    curios_unit::{Pack, pack_name, pack_names, packed, repacked},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        fs::{self, File},
        io,
        path::PathBuf,
        rc::Rc,
        time::SystemTime,
    },
};

/// What a pack file's name ends with.
const EXTENSION: &str = "pack";

/// How many times the blobs of the pack before it each pack holds, in a geometric address.
const FACTOR: usize = 2;

/// The packs of one address.
pub struct Packs {
    directory: PathBuf,
    /// The names of the packs as last listed. `None` before the first listing.
    listed: RefCell<Option<Vec<String>>>,
    /// What each pack holds as its table says, by the pack's name, `None` for one whose end is no table: a pack never changes under its name, so its end is read once for as long as the handle lives.
    names: RefCell<BTreeMap<String, Option<Rc<[Digest]>>>>,
    /// Each pack read whole so far, by its name, `None` for one that could not be read or was no pack.
    read: RefCell<BTreeMap<String, Option<Rc<Pack>>>>,
    /// The time this handle sweeps staging files by.
    now: SystemTime,
}

impl Packs {
    /// The packs kept in `directory`, swept of staging files at the time `now`.
    pub fn at(directory: PathBuf, now: SystemTime) -> Self {
        Self {
            directory,
            listed: RefCell::new(None),
            names: RefCell::new(BTreeMap::new()),
            read: RefCell::new(BTreeMap::new()),
            now,
        }
    }

    /// The blob named `digest`, where a pack of this address holds it.
    pub fn find(&self, digest: &Digest) -> Option<Blob> {
        if self.listed.borrow().is_none() {
            self.list();
        }
        if let Some(found) = self.probe(digest) {
            return Some(found);
        }
        // Not among the packs last listed: another filer may have merged them or added one since.
        self.list();
        self.probe(digest)
    }

    /// Which of `digests` no pack of this address holds, as the packs' tables say, in their order.
    pub fn missing<'d>(&self, digests: impl IntoIterator<Item = &'d Digest>) -> Vec<Digest> {
        let held = self
            .list()
            .iter()
            .filter_map(|name| self.names(name))
            .collect::<Vec<_>>();

        digests
            .into_iter()
            .filter(|digest| !held.iter().any(|names| names.binary_search(digest).is_ok()))
            .copied()
            .collect()
    }

    /// File `blobs` as one pack beside what the address holds. Nothing is written for no blobs. What keeps the address geometric, and drops what no record names, is [`Packs::collect`], which every filing runs once its record is in place.
    pub fn write(&self, blobs: &[Blob]) -> io::Result<()> {
        if blobs.is_empty() {
            return Ok(());
        }

        self.put(packed(blobs)).map(|_| ())
    }

    /// Collect this address at a filing, `kept` being what its object's two records name: a pack at least half of whose blobs `kept` leaves out is written again holding the rest, and taken away where it holds nothing else; then the smallest packs are merged, keeping what `kept` names, wherever they are no longer geometric; then a staging file [`STALE`](crate::STALE) old is swept. A blob `kept` leaves out stays where its pack is less than half such, and no compile takes it, since no record names it.
    pub fn collect(&self, kept: &BTreeSet<Digest>) -> io::Result<()> {
        for name in self.list() {
            let Some(names) = self.names(&name) else {
                continue;
            };
            let unnamed = names
                .iter()
                .filter(|digest| !kept.contains(*digest))
                .count();
            // At least half of it out, so every blob the rewrite carries is paid for by one that went.
            if unnamed > 0 && 2 * unnamed >= names.len() {
                self.rewrite(&name, kept)?;
            }
        }
        self.roll_up(kept)?;

        sweep(&self.directory, self.now)
    }

    /// The pack named `name` written again holding what `kept` names, and taken away where it holds nothing else. A pack that cannot be read is left where it is, and rewritten by a later filing that can read it.
    fn rewrite(&self, name: &str, kept: &BTreeSet<Digest>) -> io::Result<()> {
        let Some(pack) = self.pack(name) else {
            return Ok(());
        };
        let held = pack
            .blobs()
            .filter(|blob| kept.contains(&blob.digest))
            .collect::<Vec<_>>();
        let into = self.held(held)?;
        if into.as_deref() != Some(name) {
            let _ = fs::remove_file(self.path(name));
        }
        self.listed.borrow_mut().take();

        Ok(())
    }

    /// The smallest packs merged into one, wherever each no longer holds at least [`FACTOR`] times the blobs of the one before it, keeping what `kept` names, the merged ones taken away once the merge bears its name.
    fn roll_up(&self, kept: &BTreeSet<Digest>) -> io::Result<()> {
        let mut sized = self
            .list()
            .into_iter()
            .filter_map(|name| Some((self.names(&name)?.len(), name)))
            .collect::<Vec<_>>();
        sized.sort();
        let split = split(&sized.iter().map(|(count, _)| *count).collect::<Vec<_>>());
        if split < 2 {
            return Ok(());
        }

        let merged = sized[..split]
            .iter()
            .filter_map(|(_, name)| Some((name.clone(), self.pack(name)?)))
            .collect::<Vec<_>>();
        // A pack that could not be read is left where it is, and merged by a later filing that can read it.
        if merged.len() < 2 {
            return Ok(());
        }
        let into = self.held(
            merged
                .iter()
                .flat_map(|(_, pack)| pack.blobs())
                .filter(|blob| kept.contains(&blob.digest))
                .collect(),
        )?;
        for (name, _) in &merged {
            if into.as_deref() != Some(name.as_str()) {
                let _ = fs::remove_file(self.path(name));
            }
        }
        self.listed.borrow_mut().take();

        Ok(())
    }

    /// `blobs` filed as one pack, their runs of each kind run again into one ([`repacked`]), and its name: none for no blobs, which no pack holds.
    fn held(&self, blobs: Vec<Blob>) -> io::Result<Option<String>> {
        if blobs.is_empty() {
            return Ok(None);
        }

        self.put(repacked(blobs).map_err(io::Error::other)?)
            .map(Some)
    }

    /// The bytes of a pack filed under their name, replacing nothing: a pack of that name holds those bytes already. The pack is held as read, since its bytes are in hand.
    fn put(&self, bytes: Vec<u8>) -> io::Result<String> {
        let at =
            |error: io::Error| io::Error::other(format!("{}: {error}", self.directory.display()));
        fs::create_dir_all(&self.directory).map_err(at)?;
        let name = pack_name(&bytes);
        let path = self.path(&name);
        if !path.exists() {
            let staged =
                path.with_extension(format!("{EXTENSION}.{}.{STAGING}", std::process::id()));
            fs::write(&staged, &bytes).map_err(at)?;
            fs::rename(&staged, &path).map_err(|error| {
                let _ = fs::remove_file(&staged);
                at(error)
            })?;
        }
        let pack = Pack::read(Bytes::from(bytes)).map(Rc::new);
        self.read.borrow_mut().insert(name.clone(), pack);
        self.listed.borrow_mut().take();

        Ok(name)
    }

    /// The names of the packs the directory holds now, kept as the last listing.
    fn list(&self) -> Vec<String> {
        let names = fs::read_dir(&self.directory)
            .into_iter()
            .flatten()
            .filter_map(|entry| {
                let path = entry.ok()?.path();
                (path.extension()? == EXTENSION)
                    .then(|| path.file_stem()?.to_str().map(str::to_string))
                    .flatten()
            })
            .collect::<Vec<_>>();
        *self.listed.borrow_mut() = Some(names.clone());

        names
    }

    /// Every blob whose name opens with `prefix` that a pack of this address holds, each once: read out of each pack whose table names one.
    pub fn prefixed(&self, prefix: &[u8]) -> Vec<Blob> {
        let mut found = BTreeMap::new();
        for name in self.list() {
            let named = self.names(&name).is_some_and(|names| {
                let at = names.partition_point(|held| held.as_bytes()[..prefix.len()] < *prefix);
                names
                    .get(at)
                    .is_some_and(|held| held.as_bytes().starts_with(prefix))
            });
            if let Some(pack) = named.then(|| self.pack(&name)).flatten() {
                for blob in pack.prefixed(prefix) {
                    found.entry(blob.digest).or_insert(blob);
                }
            }
        }

        found.into_values().collect()
    }

    /// The blob named `digest`, among the packs last listed: read out of the pack whose table names it.
    fn probe(&self, digest: &Digest) -> Option<Blob> {
        let names = self.listed.borrow().clone().unwrap_or_default();
        names.iter().find_map(|name| {
            let named = self
                .names(name)
                .is_some_and(|names| names.binary_search(digest).is_ok());

            named.then(|| self.pack(name)?.get(digest)).flatten()
        })
    }

    /// The names of the blobs the pack named `name` holds, read off its end the first time they are asked for: `None` where it cannot be read or closes on no table.
    fn names(&self, name: &str) -> Option<Rc<[Digest]>> {
        if let Some(names) = self.names.borrow().get(name) {
            return names.clone();
        }
        let names = match self.read.borrow().get(name) {
            Some(Some(pack)) => Some(pack.blobs().map(|blob| blob.digest).collect()),
            _ => File::open(self.path(name))
                .ok()
                .and_then(|mut file| pack_names(&mut file))
                .map(Rc::from),
        };
        self.names
            .borrow_mut()
            .insert(name.to_string(), names.clone());

        names
    }

    /// The pack named `name`, read whole and held to its name the first time it is asked for: `None` where it cannot be read, and where its bytes are not its name or no pack, in which case it is damaged and taken away.
    fn pack(&self, name: &str) -> Option<Rc<Pack>> {
        if let Some(read) = self.read.borrow().get(name) {
            return read.clone();
        }
        let path = self.path(name);
        let read = fs::read(&path).ok().and_then(|bytes| {
            let pack = (pack_name(&bytes) == name)
                .then(|| Pack::read(Bytes::from(bytes)))
                .flatten();
            if pack.is_none() {
                let _ = fs::remove_file(&path);
                self.listed.borrow_mut().take();
            }

            pack.map(Rc::new)
        });
        self.read
            .borrow_mut()
            .insert(name.to_string(), read.clone());

        read
    }

    /// Where the pack named `name` is kept.
    fn path(&self, name: &str) -> PathBuf {
        self.directory.join(name).with_extension(EXTENSION)
    }
}

/// How many of the smallest packs of `counts`, sorted, to merge into one so that each pack left holds at least [`FACTOR`] times the blobs of the one before it: the packs from the top down to the first that holds fewer than that, and then each above them that holds fewer than that many times all of them together.
fn split(counts: &[usize]) -> usize {
    let Some(mut split) = (1..counts.len())
        .rev()
        .find(|at| counts[*at] < FACTOR * counts[at - 1])
        .map(|at| at + 1)
    else {
        return 0;
    };
    let mut total = counts[..split].iter().sum::<usize>();
    while split < counts.len() && counts[split] < FACTOR * total {
        total += counts[split];
        split += 1;
    }

    split
}
