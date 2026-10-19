//! A unit as it is stored: its index, and the objects the index names, each entry of a kind a member of one run of it where a filing writes it.

use {
    crate::rerun,
    curios_archive::Bytes,
    curios_core::{Global, ItemRun, Module},
    curios_elab::{HeldTrace, TraceTable, Traces},
    curios_utilities::{Digest, Member, Object, Recorded},
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
}

impl Kind {
    /// The digest of each entry, in the order the index names them.
    pub(crate) fn digests(&self) -> Box<dyn Iterator<Item = &Digest> + '_> {
        match self {
            Kind::Stored(objects) => Box::new(objects.iter().map(|object| &object.digest)),
            Kind::Items { named, .. } => Box::new(named.iter().map(|(_, digest)| digest)),
            Kind::Traces(entries) => Box::new(entries.iter().map(|(digest, ..)| digest)),
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
                let run = TraceTable::run(entries.iter().map(|(_, name, held)| (name, &***held)))?;

                members(
                    TraceTable::KIND,
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

/// What a unit stores of the traces `recorded`: their own part and the kind of their entries, what they were restored from where they were and their own otherwise, each trace by the digest it is stored under ([`TraceTable::identity`]).
pub(crate) fn traces_indexed(recorded: &Recorded<Traces>) -> Result<(Cow<'_, [u8]>, Kind), String> {
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
            let digest = held.digest(|trace| TraceTable::identity(name, trace))?;

            Ok((digest, *name, Rc::clone(held)))
        })
        .collect::<Result<_, String>>()?;

    Ok((Cow::Owned(table), Kind::Traces(entries)))
}
