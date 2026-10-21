//! A unit as it is stored: its index, and the objects the index names, each entry of a kind a member of one run of it where a filing writes it.

use {
    crate::rerun,
    curios_archive::{Bytes, parted},
    curios_core::{Anchors, Global, HeldReads, Imports, ItemRun, Module, Read, ReadTable},
    curios_elab::{HeldTrace, TraceTable, Traces},
    curios_text::PreparedText,
    curios_utilities::{Digest, Facet, Member, Object, Recorded},
    std::{borrow::Cow, collections::BTreeSet, rc::Rc},
};

/// A unit as it is stored: its index, which every compilation after the unit restores and which names each object the unit stores on its own by its digest alone, and those objects ([`Indexed::objects`]).
///
/// **The index is a function of what was compiled**, since it names an object by what the object holds and never by where it is kept: a recompile files the index a whole compile files, and a record digests the index alone and so vouches for every object it names.
pub struct Indexed {
    pub index: Vec<u8>,
    /// The objects stored alone: the lowered items and the erased forms.
    pub(crate) alone: Vec<Object>,
    /// The entries stored as members of runs, a kind at a time.
    pub(crate) kinds: Vec<Kind>,
}

impl Indexed {
    /// The digest of every object the index names.
    pub fn digests(&self) -> impl Iterator<Item = &Digest> {
        self.alone
            .iter()
            .map(|object| &object.digest)
            .chain(self.kinds.iter().flat_map(Kind::digests))
    }

    /// The objects the index names that `wanted` asks for: each stored alone as it is, and the entries of each kind, each once, as the members of one run of it, so a node or a name two of them hold is stored once.
    pub fn objects(&self, wanted: impl Fn(&Digest) -> bool) -> Result<Vec<Object>, String> {
        let mut objects = self
            .alone
            .iter()
            .filter(|object| wanted(&object.digest))
            .cloned()
            .collect::<Vec<_>>();
        for kind in &self.kinds {
            objects.extend(kind.objects(&wanted)?);
        }

        Ok(objects)
    }
}

/// The entries of one kind a unit names, each by its digest, as a filing writes the ones the store lacks.
pub(crate) enum Kind {
    /// Entries a unit restored and holds as the objects it found them as: written again, where the store lacks them, as a run of their kind ([`rerun`]).
    Stored(Vec<Object>),
    /// The elaborated items of `core`, at each place with its digest.
    Items {
        core: Rc<Module>,
        named: Vec<(usize, Digest)>,
    },
    /// Declarations' traces, each with its digest and the name it is filed under.
    Traces(Vec<(Digest, Global, HeldTrace)>),
    /// What judging definitions read, each with its digest and the definition's name.
    Reads(Vec<(Digest, Global, HeldReads)>),
    /// The records and scans of a lowering's baseline half, each stored alone, by their digests.
    Lowering {
        text: Rc<PreparedText>,
        digests: Vec<Digest>,
    },
    /// The import scopes of a lowering, each body's imports and each module's offer stored alone, by their digests.
    Imports {
        text: Rc<PreparedText>,
        digests: Vec<Digest>,
    },
    /// Where a lowering's declarations lie, each source's texts stored alone, by their digests.
    Anchors {
        text: Rc<PreparedText>,
        digests: Vec<Digest>,
    },
}

impl Kind {
    /// The digest of each entry, in the order the index names them.
    pub(crate) fn digests(&self) -> Box<dyn Iterator<Item = &Digest> + '_> {
        match self {
            Kind::Stored(objects) => Box::new(objects.iter().map(|object| &object.digest)),
            Kind::Items { named, .. } => Box::new(named.iter().map(|(_, digest)| digest)),
            Kind::Traces(entries) => Box::new(entries.iter().map(|(digest, ..)| digest)),
            Kind::Reads(entries) => Box::new(entries.iter().map(|(digest, ..)| digest)),
            Kind::Lowering { digests, .. }
            | Kind::Imports { digests, .. }
            | Kind::Anchors { digests, .. } => Box::new(digests.iter()),
        }
    }

    /// The entries `wanted` asks for, each once, as the members of one run of their kind.
    fn objects(&self, wanted: &dyn Fn(&Digest) -> bool) -> Result<Vec<Object>, String> {
        let mut seen = BTreeSet::new();
        let mut fresh = |digest: &Digest| wanted(digest) && seen.insert(*digest);
        match self {
            Kind::Stored(objects) => {
                let wanted = objects
                    .iter()
                    .filter(|object| fresh(&object.digest))
                    .cloned()
                    .collect::<Vec<_>>();
                match wanted.first().and_then(|object| object.member) {
                    Some(member) => rerun(member.kind, &wanted),
                    None => Ok(wanted),
                }
            }
            Kind::Items { core, named } => {
                let named = named
                    .iter()
                    .filter(|(_, digest)| fresh(digest))
                    .collect::<Vec<_>>();
                if named.is_empty() {
                    return Ok(Vec::new());
                }
                let run = ItemRun::of(named.iter().map(|(place, _)| &core.items[*place]))?;

                members(ItemRun::KIND, named.iter().map(|(_, digest)| *digest), run)
            }
            Kind::Traces(entries) => {
                let entries = entries
                    .iter()
                    .filter(|(digest, ..)| fresh(digest))
                    .collect::<Vec<_>>();
                if entries.is_empty() {
                    return Ok(Vec::new());
                }
                let run = TraceTable::run(
                    entries
                        .iter()
                        .map(|(_, name, held)| (name, &***held, held.made())),
                )?;

                members(
                    TraceTable::KIND,
                    entries.iter().map(|(digest, ..)| *digest),
                    run,
                )
            }
            Kind::Lowering { text, digests } => {
                let wanted = digests
                    .iter()
                    .filter(|digest| fresh(digest))
                    .collect::<BTreeSet<_>>();

                text.baseline_objects(&|digest| wanted.contains(digest))
            }
            Kind::Imports { text, digests } => {
                let wanted = digests
                    .iter()
                    .filter(|digest| fresh(digest))
                    .collect::<BTreeSet<_>>();

                imports(text)?.objects(&|digest| wanted.contains(digest))
            }
            Kind::Anchors { text, digests } => {
                let wanted = digests
                    .iter()
                    .filter(|digest| fresh(digest))
                    .collect::<BTreeSet<_>>();

                anchors(text)?.objects(&|digest| wanted.contains(digest))
            }
            Kind::Reads(entries) => {
                let entries = entries
                    .iter()
                    .filter(|(digest, ..)| fresh(digest))
                    .collect::<Vec<_>>();
                if entries.is_empty() {
                    return Ok(Vec::new());
                }
                let run = ReadTable::run(
                    entries
                        .iter()
                        .map(|(_, name, held)| (name, &***held, held.made())),
                )?;

                members(
                    ReadTable::KIND,
                    entries.iter().map(|(digest, ..)| *digest),
                    run,
                )
            }
        }
    }
}

/// The objects `digests` name, in their order, as the members of the run of `kind` that `run` is.
fn members(
    kind: u8,
    digests: impl IntoIterator<Item = Digest>,
    run: Vec<u8>,
) -> Result<Vec<Object>, String> {
    let run = Bytes::from(run);
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

/// What a certified unit stores of what judging its definitions read, `recorded`: its own part and the kind of its entries, what it was restored from where it was and its own otherwise, each definition's reads by the digest they are stored under ([`curios_core::ReadRecord::digest`]).
pub(crate) fn reads_indexed(recorded: &Recorded<Read>) -> Result<(Cow<'_, [u8]>, Kind), String> {
    if let Some((table, entries)) = recorded.as_stored() {
        return Ok((
            Cow::Borrowed(table.as_slice()),
            Kind::Stored(entries.to_vec()),
        ));
    }
    let read = recorded.get()?;
    let table = curios_archive::to_bytes(read)?.to_vec();
    let entries = read
        .definitions()
        .map(|(name, held)| {
            let digest = held.digest(name)?;

            Ok((digest, *name, Rc::clone(held)))
        })
        .collect::<Result<_, String>>()?;

    Ok((Cow::Owned(table), Kind::Reads(entries)))
}

/// The bytes a record of what was read is filed as where it is a constant of the executable, as the prelude's certification files it: its own part, the digest of each definition's reads and the one run holding them all, behind their lengths, so it is restored as a store's is and nothing of it is read until a walk asks.
pub fn reads_filed(recorded: &Recorded<Read>) -> Result<Vec<u8>, String> {
    let (table, kind) = reads_indexed(recorded)?;
    let digests = crate::digests(kind.digests());
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

/// The record of what was read that `bytes` file ([`reads_filed`]): its own part, and each definition's reads a member of the one run, named by its digest. `None` for bytes that are no such thing.
pub fn reads_restored(bytes: &Bytes) -> Option<(Bytes, Vec<Object>)> {
    let [table, digests, run]: [Bytes; 3] = parted(bytes)?.try_into().ok()?;
    let (digests, rest) = digests.as_slice().as_chunks::<32>();
    if !rest.is_empty() {
        return None;
    }
    let entries = digests
        .iter()
        .enumerate()
        .map(|(index, digest)| {
            Some(Object {
                digest: Digest::from_bytes(*digest),
                bytes: run.clone(),
                member: Some(Member {
                    kind: ReadTable::KIND,
                    index: u32::try_from(index).ok()?,
                }),
            })
        })
        .collect::<Option<_>>()?;

    Some((table, entries))
}

/// What a unit stores of the lowering's baseline half `text` holds: its own part and the kind of its entries, what it was restored from where it was and its own otherwise.
pub(crate) fn lowering_indexed(text: &Rc<PreparedText>) -> Result<(Cow<'_, [u8]>, Kind), String> {
    if let Some((table, entries)) = text.baseline_stored() {
        return Ok((
            Cow::Borrowed(table.as_slice()),
            Kind::Stored(entries.to_vec()),
        ));
    }

    Ok((
        Cow::Owned(text.baseline_table()?),
        Kind::Lowering {
            text: Rc::clone(text),
            digests: text.baseline_digests()?,
        },
    ))
}

/// The import scopes `text` holds.
fn imports(text: &PreparedText) -> Result<&Imports, String> {
    text.imports_recorded().get()
}

/// What a unit stores of the import scopes `text` holds: their own part and the kind of their entries, what they were restored from where they were and their own otherwise.
pub(crate) fn imports_indexed(text: &Rc<PreparedText>) -> Result<(Cow<'_, [u8]>, Kind), String> {
    if let Some((table, entries)) = text.imports_recorded().as_stored() {
        return Ok((
            Cow::Borrowed(table.as_slice()),
            Kind::Stored(entries.to_vec()),
        ));
    }
    let held = imports(text)?;

    Ok((
        Cow::Owned(curios_archive::to_bytes(held)?.to_vec()),
        Kind::Imports {
            text: Rc::clone(text),
            digests: held.digests()?,
        },
    ))
}

/// Where the declarations `text` lowered lie.
fn anchors(text: &PreparedText) -> Result<&Anchors, String> {
    text.anchors_recorded().get()
}

/// What a unit stores of where the declarations `text` lowered lie: its own part and the kind of its entries, what it was restored from where it was and its own otherwise.
pub(crate) fn anchors_indexed(text: &Rc<PreparedText>) -> Result<(Cow<'_, [u8]>, Kind), String> {
    if let Some((table, entries)) = text.anchors_recorded().as_stored() {
        return Ok((
            Cow::Borrowed(table.as_slice()),
            Kind::Stored(entries.to_vec()),
        ));
    }
    let held = anchors(text)?;

    Ok((
        Cow::Owned(curios_archive::to_bytes(held)?.to_vec()),
        Kind::Anchors {
            text: Rc::clone(text),
            digests: held.digests()?,
        },
    ))
}

/// What a unit stores of the traces `recorded`: their own part and the kind of their entries, what they were restored from where they were and their own otherwise, each record by the digest it is stored under ([`curios_elab::TraceRecord::digest`]).
///
/// A record held is named as `facet`'s ([`curios_elab::TraceRecord::digest`]).
pub(crate) fn traces_indexed(
    recorded: &Recorded<Traces>,
    facet: Facet,
) -> Result<(Cow<'_, [u8]>, Kind), String> {
    if let Some((table, entries)) = recorded.as_stored() {
        return Ok((
            Cow::Borrowed(table.as_slice()),
            Kind::Stored(entries.to_vec()),
        ));
    }
    let traces = recorded.get()?;
    let table = curios_archive::to_bytes(traces)?.to_vec();
    let entries = traces
        .declarations()
        .map(|(name, held)| {
            let digest = held.digest(facet, name)?;

            Ok((digest, *name, Rc::clone(held)))
        })
        .collect::<Result<_, String>>()?;

    Ok((Cow::Owned(table), Kind::Traces(entries)))
}
