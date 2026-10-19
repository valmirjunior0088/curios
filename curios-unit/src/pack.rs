//! A pack: objects of the store, each named by a digest ([`Object`]), kept together in one file named by the digest of its own ([`Pack`]).
//!
//! **Read from its end.** A pack closes on a table of the objects it holds, sorted by name, each with where its bytes lie, then how many there are and a magic: an object is found by a binary search of the table, and which objects a pack holds is read off its end alone ([`pack_names`]).
//!
//! **A function of what it holds.** Objects are laid out in the order of their names, each once, so two filers packing the same objects write the same bytes, under the same name.
//!
//! **An object is its bytes, or a member of them.** An entry names the run of the pack an object lies in, and where that run holds several objects archived together, which of them it is and what kind of run it is: the elaborated declarations of one pack share one table of nodes ([`curios_core::ItemRun`]), and the traces one table of names ([`curios_elab::TraceTable`]), so a node or a name two of them hold is stored and restored once, and each is still named by the digest of its form archived alone, which no pack's choice of neighbours moves. A merge of packs runs each kind's members again into one run of it ([`repacked`], [`rerun`]), so a pack keeps one table a kind however it was made.

#[cfg(test)]
mod tests;

use {
    curios_archive::Bytes,
    curios_core::{ItemRun, ReadEntry, ReadTable},
    curios_elab::{TraceTable, TracedEntry},
    curios_utilities::{Digest, Member, Object},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        io::{Read, Seek, SeekFrom},
        ops::Range,
    },
};

/// What a pack closes on, after its table and how many entries it holds.
const MAGIC: &[u8; 8] = b"crpack\0\0";

/// How many bytes close a pack after its table: how many entries the table holds, then the magic.
const END: usize = 8 + MAGIC.len();

/// The width of one entry of a pack's table: the object's name, the start and the length of the run it lies in, which member of the run it is, and the run's kind.
const ENTRY: usize = 32 + 8 + 8 + 4 + 1;

/// The member an entry names where its object is the whole of its run.
const WHOLE: u32 = u32::MAX;

/// The bytes of a pack holding `objects`, each once, in the order of their names: a run shared by several members is written once, where the first of them is.
pub fn packed<'o>(objects: impl IntoIterator<Item = &'o Object>) -> Vec<u8> {
    let mut objects = objects.into_iter().collect::<Vec<_>>();
    objects.sort_by_key(|object| object.digest);
    objects.dedup_by_key(|object| object.digest);

    let mut bytes = Vec::new();
    // Where each run handed in was written, by where it lies in memory: members of one run are handed in as the one run of one buffer.
    let mut written = HashMap::<(*const u8, usize), u64>::new();
    let mut table = Vec::with_capacity(objects.len());
    for object in &objects {
        let run = object.bytes.as_slice();
        let start = *written.entry((run.as_ptr(), run.len())).or_insert_with(|| {
            let start = bytes.len() as u64;
            bytes.extend_from_slice(run);
            start
        });
        table.push((
            object.digest,
            start,
            run.len() as u64,
            object.member.map_or(WHOLE, |member| member.index),
            object.member.map_or(0, |member| member.kind),
        ));
    }
    for (digest, start, length, member, kind) in &table {
        bytes.extend_from_slice(digest.as_bytes());
        bytes.extend_from_slice(&start.to_le_bytes());
        bytes.extend_from_slice(&length.to_le_bytes());
        bytes.extend_from_slice(&member.to_le_bytes());
        bytes.push(*kind);
    }
    bytes.extend_from_slice(&(table.len() as u64).to_le_bytes());
    bytes.extend_from_slice(MAGIC);

    bytes
}

/// The name of the pack `bytes` are: the digest of them, which is what a pack is held to where it is read.
pub fn pack_name(bytes: &[u8]) -> String {
    Digest::of(bytes).hex()
}

/// The names of the objects the pack `reader` holds, in their order, read off its end and nothing before its table: what tells whether an object is held without reading the pack. `None` where it closes on no table.
///
/// **Believed without the pack being held to its name**, which only reading it whole does: a damaged pack whose table still reads is taken to hold what it names, and is found damaged where one of its objects is read.
pub fn pack_names(reader: &mut (impl Read + Seek)) -> Option<Vec<Digest>> {
    let length = reader.seek(SeekFrom::End(0)).ok()?;
    let mut end = [0; END];
    reader.seek(SeekFrom::End(-(END as i64))).ok()?;
    reader.read_exact(&mut end).ok()?;
    let width = table_width(&end)?;
    if u64::try_from(width + END).ok()? > length {
        return None;
    }

    let mut table = vec![0; width];
    reader
        .seek(SeekFrom::End(-i64::try_from(width + END).ok()?))
        .ok()?;
    reader.read_exact(&mut table).ok()?;

    Some(table.chunks_exact(ENTRY).map(entry_name).collect())
}

/// How many bytes the table of a pack closing on `end` holds: `None` where `end` is not how a pack closes.
fn table_width(end: &[u8; END]) -> Option<usize> {
    let (count, magic) = end.split_first_chunk::<8>()?;
    if magic != MAGIC {
        return None;
    }

    usize::try_from(u64::from_le_bytes(*count))
        .ok()?
        .checked_mul(ENTRY)
}

/// The name an entry of a pack's table holds.
fn entry_name(entry: &[u8]) -> Digest {
    Digest::from_bytes(entry[..32].try_into().expect("thirty-two bytes"))
}

/// A pack read: its bytes, and where the table they close on lies.
pub struct Pack {
    bytes: Bytes,
    table: Range<usize>,
    count: usize,
}

impl Pack {
    /// The pack `bytes` are, or `None` for bytes that are no pack: no magic, a table running past them, or an entry naming bytes the pack does not hold.
    pub fn read(bytes: Bytes) -> Option<Self> {
        let (rest, end) = bytes.as_slice().split_last_chunk::<END>()?;
        let width = table_width(end)?;
        let start = rest.len().checked_sub(width)?;
        let pack = Self {
            table: start..rest.len(),
            count: width / ENTRY,
            bytes,
        };
        let runs_within = (0..pack.count).all(|index| {
            let (_, run, _) = pack.entry(index);
            run.end <= start
        });

        runs_within.then_some(pack)
    }

    /// How many objects this pack holds.
    pub fn len(&self) -> usize {
        self.count
    }

    /// Whether this pack holds no object.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// The object named `digest`, where this pack holds it: the run of the pack it lies in, and which member of the run it is, where the run holds several.
    pub fn get(&self, digest: &Digest) -> Option<Object> {
        let (mut low, mut high) = (0, self.count);
        while low < high {
            let middle = low + (high - low) / 2;
            let (held, run, member) = self.entry(middle);
            match held.cmp(digest) {
                std::cmp::Ordering::Equal => return self.object(held, run, member),
                std::cmp::Ordering::Less => low = middle + 1,
                std::cmp::Ordering::Greater => high = middle,
            }
        }
        None
    }

    /// Every object this pack holds, in the order of their names.
    pub fn objects(&self) -> impl Iterator<Item = Object> + '_ {
        (0..self.count).filter_map(|index| {
            let (digest, run, member) = self.entry(index);
            self.object(digest, run, member)
        })
    }

    /// The entry at `index` of the table: a name, the run of the pack it lies in, and its member.
    fn entry(&self, index: usize) -> (Digest, Range<usize>, Option<Member>) {
        let at = self.table.start + index * ENTRY;
        let entry = &self.bytes.as_slice()[at..at + ENTRY];
        let word = |from: usize| {
            u64::from_le_bytes(entry[from..from + 8].try_into().expect("eight bytes"))
        };
        let start = usize::try_from(word(32)).unwrap_or(usize::MAX);
        let length = usize::try_from(word(40)).unwrap_or(usize::MAX);
        let index = u32::from_le_bytes(entry[48..52].try_into().expect("four bytes"));
        let member = (index != WHOLE).then_some(Member {
            kind: entry[52],
            index,
        });

        (
            entry_name(entry),
            start..start.saturating_add(length),
            member,
        )
    }

    /// The object named `digest` lying in `run`, as `member` of it where it is one.
    fn object(&self, digest: Digest, run: Range<usize>, member: Option<Member>) -> Option<Object> {
        Some(Object {
            digest,
            bytes: self.bytes.run(run)?,
            member,
        })
    }
}

/// The bytes of one pack holding `objects`, each once, read out of other packs: an object stored alone as it is, and the members of runs of each kind run again into one run of it ([`rerun`]), so a pack made of others keeps one table a kind. `Err` is a run that is not what its kind says, or holds no member an entry names.
pub fn repacked(objects: impl IntoIterator<Item = Object>) -> Result<Vec<u8>, String> {
    let mut alone = Vec::new();
    let mut members = BTreeMap::<u8, Vec<Object>>::new();
    for object in objects {
        match object.member {
            Some(member) => members.entry(member.kind).or_default().push(object),
            None => alone.push(object),
        }
    }
    for (kind, members) in members {
        alone.extend(rerun(kind, &members)?);
    }

    Ok(packed(&alone))
}

/// `members`, members of runs of `kind`, as the members of one run of their kind, each once and in their order, each run they lie in read once: a pack merged or collected keeps one table of a kind, and objects a unit restored and must write again are written in a run of their own. `Err` is a kind no run is of, a run that is not what its kind says, or one that holds no member an entry names.
pub fn rerun(kind: u8, members: &[Object]) -> Result<Vec<Object>, String> {
    match kind {
        ItemRun::KIND => rerun_with(
            kind,
            members,
            |bytes| curios_archive::from_bytes::<ItemRun>(bytes).map(|run| run.items),
            |items| ItemRun::of(items.iter().copied()),
        ),
        TraceTable::KIND => rerun_with(kind, members, TraceTable::read, |entries| {
            TraceTable::run(
                entries
                    .iter()
                    .map(|entry: &&TracedEntry| (&entry.0, &entry.1)),
            )
        }),
        ReadTable::KIND => rerun_with(kind, members, ReadTable::read, |entries| {
            ReadTable::run(
                entries
                    .iter()
                    .map(|entry: &&ReadEntry| (&entry.0, &entry.1)),
            )
        }),
        other => Err(format!("no run is of kind {other}")),
    }
}

/// [`rerun`] for a kind whose runs `read` reads and `write` writes.
fn rerun_with<E>(
    kind: u8,
    members: &[Object],
    read: impl Fn(&[u8]) -> Result<Vec<E>, String>,
    write: impl Fn(&[&E]) -> Result<Vec<u8>, String>,
) -> Result<Vec<Object>, String> {
    // Each run read, by where it lies in memory: its members are read out of it once.
    let mut runs = HashMap::<(*const u8, usize), Vec<E>>::new();
    for object in members {
        let bytes = object.bytes.as_slice();
        if let std::collections::hash_map::Entry::Vacant(unread) =
            runs.entry((bytes.as_ptr(), bytes.len()))
        {
            unread.insert(read(bytes)?);
        }
    }

    let mut seen = BTreeSet::new();
    let mut entries = Vec::new();
    let mut digests = Vec::new();
    for object in members {
        if !seen.insert(object.digest) {
            continue;
        }
        let bytes = object.bytes.as_slice();
        let member = object
            .member
            .ok_or("an object alone is no member of a run")?;
        let entry = usize::try_from(member.index)
            .ok()
            .and_then(|index| runs[&(bytes.as_ptr(), bytes.len())].get(index))
            .ok_or_else(|| format!("a run holds no member {}", member.index))?;
        entries.push(entry);
        digests.push(object.digest);
    }
    if entries.is_empty() {
        return Ok(Vec::new());
    }

    let run = Bytes::from(write(&entries)?);
    digests
        .into_iter()
        .enumerate()
        .map(|(index, digest)| {
            Ok(Object {
                digest,
                bytes: run.clone(),
                member: Some(Member {
                    kind,
                    index: u32::try_from(index).map_err(|error| error.to_string())?,
                }),
            })
        })
        .collect()
}
