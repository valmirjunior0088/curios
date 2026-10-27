//! Who read what, of a unit's records read backwards.

use {
    super::{Asked, AskedItems, Part},
    curios_utilities::Persistent,
    std::collections::BTreeMap,
};

/// Which parts of a name an item's erasure read: a set over [`Part`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Parts(u8);

impl Parts {
    /// The set holding `part` alone.
    fn of(part: Part) -> Parts {
        Parts(1 << part as u8)
    }

    /// Both sets of parts.
    fn with(self, other: Parts) -> Parts {
        Parts(self.0 | other.0)
    }

    /// Each part the set holds, in a record's order.
    pub fn parts(self) -> impl Iterator<Item = Part> {
        Part::ALL
            .into_iter()
            .filter(move |part| self.0 & Parts::of(*part).0 != 0)
    }

    /// Whether the set holds `part`.
    pub fn holds(self, part: Part) -> bool {
        self.0 & Parts::of(part).0 != 0
    }
}

/// Who read what, of a unit's records read backwards: for each name, the items whose erasure read a part of it, by the name each declares first, with the parts it read. What an erasure over the unit finds the items an edit could reach by, from the edit, where asking every item's record asks the whole unit ([A compile costs what it reads](../../../documentation/design/compilation/a-compile-costs-what-it-reads.md)).
///
/// **Read off the records, never stored.** Made the first time an erasure over the unit asks, and kept with the unit; a unit erased over a baseline holds the baseline's with the edges of every item it erased again or dropped replaced ([`AskedBy::replace`]), so a session reads no record backwards twice.
///
/// **Looked up, never walked.** Its map is persistent and keyed by hash, so its order follows the process, and what reads it asks after one name at a time.
#[derive(Debug, Clone, Default)]
pub struct AskedBy {
    parts: Persistent<curios_core::Global, Vec<(curios_core::Global, Parts)>>,
}

/// Each name `asked` read a part of, with the parts it read of it, in the order of the names.
fn edges(asked: &Asked) -> Vec<(curios_core::Global, Parts)> {
    let mut edges = Vec::<(curios_core::Global, Parts)>::new();
    for (name, part) in asked.reads() {
        match edges.last_mut() {
            Some((held, parts)) if *held == name => *parts = parts.with(Parts::of(part)),
            _ => edges.push((name, Parts::of(part))),
        }
    }

    edges
}

impl AskedBy {
    /// Who read what, of `items`.
    pub fn of(items: &AskedItems) -> Self {
        curios_profile::profile!("asked_by::of");
        // Gathered whole first, so each name's list is built once and not copied at every reader a persistent insert would add.
        let mut gathered =
            BTreeMap::<curios_core::Global, Vec<(curios_core::Global, Parts)>>::new();
        for (reader, record) in items.records() {
            for (name, parts) in edges(record) {
                gathered.entry(name).or_default().push((*reader, parts));
            }
        }
        let mut by = AskedBy::default();
        for (name, readers) in gathered {
            by.parts.insert(name, readers);
        }

        by
    }

    /// Replace the edges of the item declaring `reader` first: those `earlier`, the record it held, added are taken out, and those `now`, the one it holds, adds are put in. `None` on either side is an item that held, or holds, no record.
    pub fn replace(
        &mut self,
        reader: curios_core::Global,
        earlier: Option<&Asked>,
        now: Option<&Asked>,
    ) {
        if let Some(earlier) = earlier {
            for (name, _) in edges(earlier) {
                let Some(held) = self.parts.get(&name) else {
                    continue;
                };
                let kept = held
                    .iter()
                    .filter(|(held, _)| *held != reader)
                    .copied()
                    .collect::<Vec<_>>();
                match kept.is_empty() {
                    true => {
                        self.parts.remove(&name);
                    }
                    false => self.parts.insert(name, kept),
                }
            }
        }
        if let Some(now) = now {
            for (name, parts) in edges(now) {
                let mut held = self.parts.get(&name).cloned().unwrap_or_default();
                held.push((reader, parts));
                self.parts.insert(name, held);
            }
        }
    }

    /// The items whose erasure read a part of `name`, each with the parts it read.
    pub fn of_name(&self, name: &curios_core::Global) -> &[(curios_core::Global, Parts)] {
        self.parts.get(name).map_or(&[], Vec::as_slice)
    }

    /// Whether some item's erasure read `part` of `name`.
    pub fn asks(&self, name: &curios_core::Global, part: Part) -> bool {
        self.of_name(name)
            .iter()
            .any(|(_, parts)| parts.holds(part))
    }

    /// Every name a part was read of, each once, in no stated order: what a unit in scope that moved is read again by.
    pub fn names(&self) -> impl Iterator<Item = &curios_core::Global> {
        self.parts.iter().map(|(name, _)| name)
    }
}
