//! The packs of one slot's address: the objects its slots name, written before a slot names them ([`Packs::write`]), found by name ([`Packs::find`]), and kept geometric.
//!
//! **Written whole and renamed into place, as a slot is.** A pack is complete before it bears its name, and its name is the digest of its bytes, so a reader sees a pack whole or not at all, and two filers packing the same objects write one file twice.
//!
//! **A pack is held to its name where an object of it is read.** Its bytes are digested once, the first time anything of it is asked for, which vouches for every object it holds. A pack whose bytes are not its name is damaged, since nothing renames a pack into place before it is whole: it is taken away, and an object only it held is not found and is written again by the next filing that names it.
//!
//! **What a filing writes is decided by the packs' ends.** Which objects a pack holds is read off its table ([`pack_names`]), so a filing reads no pack whole to know what it lacks.
//!
//! **Kept geometric where it is filed.** The filing that writes a pack merges the smallest packs of the address into one, wherever each no longer holds at least twice the objects of the one before it, as `git repack --geometric` rolls a repository's packs up: the address holds a logarithmic number of packs, and an object is written again a logarithmic number of times over its life. The elaborated declarations of the merged packs are consed again in one table ([`repacked`]), so a pack keeps one however it was made.
//!
//! **A pack another filer merged away is looked for again.** The packs are listed once for a handle, and again where an object is not found among them, so a merge that renamed what a reader was about to read is a second listing, never a miss.

#[cfg(test)]
mod tests;

use {
    curios_archive::Bytes,
    curios_unit::{Pack, pack_name, pack_names, packed, repacked},
    curios_utilities::{Digest, Object},
    std::{
        cell::RefCell,
        collections::BTreeMap,
        fs::{self, File},
        io,
        path::PathBuf,
        rc::Rc,
    },
};

/// What a pack file's name ends with.
const EXTENSION: &str = "pack";

/// How many times the objects of the pack before it each pack holds, in a geometric address.
const FACTOR: usize = 2;

/// The packs of one address.
pub(crate) struct Packs {
    directory: PathBuf,
    /// The names of the packs as last listed. `None` before the first listing.
    listed: RefCell<Option<Vec<String>>>,
    /// What each pack holds as its table says, by the pack's name, `None` for one whose end is no table: a pack never changes under its name, so its end is read once for as long as the handle lives.
    names: RefCell<BTreeMap<String, Option<Rc<[Digest]>>>>,
    /// Each pack read whole so far, by its name, `None` for one that could not be read or was no pack.
    read: RefCell<BTreeMap<String, Option<Rc<Pack>>>>,
}

impl Packs {
    /// The packs kept in `directory`.
    pub(crate) fn at(directory: PathBuf) -> Self {
        Self {
            directory,
            listed: RefCell::new(None),
            names: RefCell::new(BTreeMap::new()),
            read: RefCell::new(BTreeMap::new()),
        }
    }

    /// The object named `digest`, where a pack of this address holds it.
    pub(crate) fn find(&self, digest: &Digest) -> Option<Object> {
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
    pub(crate) fn missing<'d>(&self, digests: impl IntoIterator<Item = &'d Digest>) -> Vec<Digest> {
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

    /// File `objects` as one pack beside what the address holds, then merge the smallest packs where they are no longer geometric. Nothing is written for no objects.
    pub(crate) fn write(&self, objects: &[Object]) -> io::Result<()> {
        if objects.is_empty() {
            return Ok(());
        }
        self.put(packed(objects))?;

        self.roll_up()
    }

    /// The bytes of a pack filed under their name, replacing nothing: a pack of that name holds those bytes already. The pack is held as read, since its bytes are in hand.
    fn put(&self, bytes: Vec<u8>) -> io::Result<String> {
        let at =
            |error: io::Error| io::Error::other(format!("{}: {error}", self.directory.display()));
        fs::create_dir_all(&self.directory).map_err(at)?;
        let name = pack_name(&bytes);
        let path = self.path(&name);
        if !path.exists() {
            let staged = path.with_extension(format!("{EXTENSION}.{}.part", std::process::id()));
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

    /// The smallest packs merged into one, wherever each no longer holds at least [`FACTOR`] times the objects of the one before it, the merged ones taken away once the merge bears its name. Their elaborated declarations are consed again in one table ([`repacked`]).
    fn roll_up(&self) -> io::Result<()> {
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
        let merging = repacked(merged.iter().map(|(_, pack)| &**pack)).map_err(io::Error::other)?;
        let into = self.put(merging)?;
        for (name, _) in &merged {
            if *name != into {
                let _ = fs::remove_file(self.path(name));
            }
        }
        self.listed.borrow_mut().take();

        Ok(())
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

    /// The object named `digest`, among the packs last listed: read out of the pack whose table names it.
    fn probe(&self, digest: &Digest) -> Option<Object> {
        let names = self.listed.borrow().clone().unwrap_or_default();
        names.iter().find_map(|name| {
            let named = self
                .names(name)
                .is_some_and(|names| names.binary_search(digest).is_ok());

            named.then(|| self.pack(name)?.get(digest)).flatten()
        })
    }

    /// The names of the objects the pack named `name` holds, read off its end the first time they are asked for: `None` where it cannot be read or closes on no table.
    fn names(&self, name: &str) -> Option<Rc<[Digest]>> {
        if let Some(names) = self.names.borrow().get(name) {
            return names.clone();
        }
        let names = match self.read.borrow().get(name) {
            Some(Some(pack)) => Some(pack.objects().map(|object| object.digest).collect()),
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

/// How many of the smallest packs of `counts`, sorted, to merge into one so that each pack left holds at least [`FACTOR`] times the objects of the one before it: the packs from the top down to the first that holds fewer than that, and then each above them that holds fewer than that many times all of them together.
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
