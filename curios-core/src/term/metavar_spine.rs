//! A metavariable's spine: what each binder it was born under stands for at one occurrence, in binding order — the delayed substitution its solution is read through ([`Metavar::spine`](super::Metavar)).
//!
//! **Persistent, and carrying its facts.** The entries are a persistent vector, shared between every occurrence born under one scope and with the scope that holds them, so a birth costs nothing more than the one before it and an entry added for a binder costs a logarithm of the scope. Beside the entries the spine keeps what a node derives from its children — the hash, the reach, the containment flags, the free names and the rank's summary, each entry counted once at its rank — so a node holding a spine derives its own without reading it, and two spines are told apart by their hashes before their entries.
//!
//! **The identity is known as such.** A spine a metavariable is born with names each of its birth binders as itself, and a reader that would apply or invert it reads it as the identity without walking it ([`MetavarSpine::is_identity`]). Any spine a walk rewrote, or one built from a list, is not known to be one, which is the cautious answer: it is read entry by entry.
//!
//! Entries stand at their ranks, so two spines of one length that hold the same terms are one spine, whatever built them.

#[cfg(test)]
mod tests;

use {
    super::{
        Bound, Free, Frees, Names, Term,
        rank::{Ranked, SpineRanked},
    },
    curios_utilities::{Ordered, Sorted, Summary},
    std::{
        borrow::Borrow,
        fmt,
        hash::{Hash, Hasher},
    },
};

/// The deepest reach and the newest local of a run of a spine's entries, kept on each subtree of the entries: what the spine's own reach and newest local are, read off the whole, and what a rewrite reads to pass over a run whole ([`MetavarSpine::rewritten`]).
#[derive(Clone, Copy, Default)]
struct Reaches {
    reach: usize,
    newest_local: Option<u64>,
}

impl Summary<Term> for Reaches {
    fn of(entry: &Term) -> Self {
        Self {
            reach: entry.reach(),
            newest_local: entry.newest_local(),
        }
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            reach: self.reach.max(other.reach),
            newest_local: self.newest_local.max(other.newest_local),
        }
    }
}

/// What a node reads of its children beside what [`Reaches`] keeps, kept for a spine's entries together: the hash of each entry at its rank, combined so an entry added or removed moves it by that entry alone, and how many entries hold each containment.
#[derive(Clone, Copy, Default)]
struct Facts {
    hash: u64,
    metavar: usize,
    transient: usize,
    universe_meta: usize,
    universe_data: usize,
    group: usize,
}

impl Facts {
    /// `entry`'s share of the facts at `rank`: one side of [`Facts::add`] and [`Facts::remove`].
    fn of(rank: i64, entry: &Term) -> Self {
        Self {
            hash: placed_hash(rank, entry.get_or_init_hash()),
            metavar: usize::from(entry.has_metavar()),
            transient: usize::from(entry.has_transient()),
            universe_meta: usize::from(entry.has_universe_meta()),
            universe_data: usize::from(entry.has_universe_data()),
            group: usize::from(entry.has_group()),
        }
    }

    fn add(&mut self, share: Self) {
        self.hash = self.hash.wrapping_add(share.hash);
        self.metavar += share.metavar;
        self.transient += share.transient;
        self.universe_meta += share.universe_meta;
        self.universe_data += share.universe_data;
        self.group += share.group;
    }

    /// Take `share` out.
    fn remove(&mut self, share: Self) {
        self.hash = self.hash.wrapping_sub(share.hash);
        self.metavar -= share.metavar;
        self.transient -= share.transient;
        self.universe_meta -= share.universe_meta;
        self.universe_data -= share.universe_data;
        self.group -= share.group;
    }
}

/// An entry's hash mixed with its rank, so a spine's hash, a sum of these, tells two orders of one set of entries apart.
fn placed_hash(rank: i64, hash: u64) -> u64 {
    let mut mixed = hash ^ (rank as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);
    mixed = (mixed ^ (mixed >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    mixed = (mixed ^ (mixed >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    mixed ^ (mixed >> 31)
}

/// The free names a spine's entries hold: how many entries hold each, and the set of them, which a name enters with the first entry holding it and leaves with the last. Counted so that an entry taken out takes out only the names no other entry holds, as the containment counts beside it do.
#[derive(Clone)]
struct Held {
    counts: Sorted<Free, u32>,
    names: Frees,
}

impl Default for Held {
    fn default() -> Self {
        Self {
            counts: Sorted::new(),
            names: Frees::empty(),
        }
    }
}

impl Held {
    fn add(&mut self, entry: &Term) {
        for name in &entry.free_vars() {
            match self.counts.get(name).copied() {
                Some(count) => self.counts.insert(*name, count + 1),
                None => {
                    self.counts.insert(*name, 1);
                    self.names.insert(*name);
                }
            }
        }
    }

    fn remove(&mut self, entry: &Term) {
        for name in &entry.free_vars() {
            match self.counts.get(name).copied() {
                Some(1) => {
                    self.counts.remove(name);
                    self.names.remove(name);
                }
                Some(count) => self.counts.insert(*name, count - 1),
                None => unreachable!("an entry taken out was counted where it was put in"),
            }
        }
    }
}

/// A metavariable's spine. See the module documentation.
#[derive(Clone, Default)]
pub struct MetavarSpine {
    entries: Ordered<Term, Reaches>,
    facts: Facts,
    held: Held,
    /// The rank's summary of the entries, each placed at its rank: what a metavariable's node is ranked by beside its own identity.
    ranked: SpineRanked,
    /// Whether each entry is known to be its own birth binder: so for the spine a metavariable is born with, and an empty one; not for one a walk rewrote or one built from a list.
    identity: bool,
}

impl MetavarSpine {
    /// The spine of no binder, which every reader takes as the identity.
    pub fn new() -> Self {
        Self {
            identity: true,
            ..Self::default()
        }
    }

    /// The identity over the binders `self` names, one more bound after them: what a scope keeps beside its locals, so a metavariable born under them takes it as it stands.
    pub fn bound(&mut self, binder: &Free) {
        let entry = Term::free_var(binder);
        self.push(entry);
    }

    /// `entry` after every other. The identity stays one only where the entry is its own binder, which [`MetavarSpine::bound`] says.
    fn push(&mut self, entry: Term) {
        let rank = self.entries.len() as i64;
        self.facts.add(Facts::of(rank, &entry));
        self.held.add(&entry);
        self.ranked.changed(rank, entry.ranked(), true);
        self.entries.insert(rank, entry);
    }

    /// The spine over `binders`, each standing for itself: the identity a metavariable is born with, built again where a reader has only its binders.
    pub fn identity_over<'a>(binders: impl IntoIterator<Item = &'a Free>) -> Self {
        let mut spine = Self::new();
        for binder in binders {
            spine.bound(binder);
        }
        spine
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Whether every entry is known to be its own birth binder, so that applying the spine changes nothing and inverting it is the identity. False is the cautious answer for a spine nothing built as one.
    pub fn is_identity(&self) -> bool {
        self.identity
    }

    /// Whether the two are one version of one spine, and so hold the same entries, told without reading them.
    pub(crate) fn same_version(&self, other: &Self) -> bool {
        self.entries.same_version(&other.entries)
    }

    /// The entries in binding order.
    pub fn iter(&self) -> impl Iterator<Item = &Term> + Clone {
        self.entries.iter().map(|(_, entry)| entry)
    }

    /// The spine with each entry replaced by what `visit` makes of it, or `None` where it makes nothing new of any: a walk keeps a spine it leaves alone, identity and all. A run of entries whose deepest reach and newest local `passes` holds is passed over unvisited, so a capture of the binders just opened reads the entries that hold them and not the whole scope the metavariable was born in.
    pub(crate) fn rewritten(
        &self,
        passes: impl Fn(usize, Option<u64>) -> bool,
        mut visit: impl FnMut(&Term) -> Option<Term>,
    ) -> Option<Self> {
        let mut replaced = Vec::new();
        let entries = self.entries.rewritten_where(
            |reaches| passes(reaches.reach, reaches.newest_local),
            |rank, entry| {
                let new = visit(entry)?;
                replaced.push((rank, entry.clone(), new.clone()));
                Some(new)
            },
        )?;
        let mut spine = Self {
            entries,
            facts: self.facts,
            held: self.held.clone(),
            ranked: self.ranked.clone(),
            identity: false,
        };
        for (rank, old, new) in replaced {
            spine.facts.remove(Facts::of(rank, &old));
            spine.facts.add(Facts::of(rank, &new));
            spine.held.remove(&old);
            spine.held.add(&new);
            spine.ranked.changed(rank, old.ranked(), false);
            spine.ranked.changed(rank, new.ranked(), true);
        }

        Some(spine)
    }

    /// The rank's summary of the entries, kept as the spine is built and rewritten: what a metavariable's node is ranked by, read without reading an entry.
    pub(crate) fn ranked(&self) -> &Ranked {
        self.ranked.ranked()
    }

    pub(crate) fn reach(&self) -> usize {
        self.entries.summary().map_or(0, |reaches| reaches.reach)
    }

    /// The newest binder an entry holds free, by its index.
    pub(crate) fn newest_local(&self) -> Option<u64> {
        self.entries
            .summary()
            .and_then(|reaches| reaches.newest_local)
    }

    /// The free names its entries hold, kept as the spine is built and rewritten: a metavariable's free variables, read without reading an entry.
    pub fn free_vars(&self) -> Frees {
        self.held.names.clone()
    }

    /// Whether an entry holds any of `names` free: none does where every one is a local newer than the newest local an entry holds, and otherwise the spine's names answer.
    pub fn mentions_any(&self, names: &Names) -> bool {
        if names.is_empty() {
            return false;
        }
        if let Some(oldest) = names.oldest_local()
            && self.newest_local().is_none_or(|newest| newest < oldest)
        {
            return false;
        }
        self.held.names.holds_any(names)
    }

    /// Whether an entry holds a metavariable: what a walk looking for metavariables asks before it reads the spine, and passes over it where none does.
    pub fn has_metavar(&self) -> bool {
        self.facts.metavar > 0
    }

    pub(crate) fn has_transient(&self) -> bool {
        self.facts.transient > 0
    }

    pub(crate) fn has_universe_meta(&self) -> bool {
        self.facts.universe_meta > 0
    }

    pub(crate) fn has_universe_data(&self) -> bool {
        self.facts.universe_data > 0
    }

    pub(crate) fn has_group(&self) -> bool {
        self.facts.group > 0
    }
}

impl From<Vec<Term>> for MetavarSpine {
    /// A spine of `entries`, which nothing says is an identity unless it is empty.
    fn from(entries: Vec<Term>) -> Self {
        let mut spine = Self::new();
        spine.identity = entries.is_empty();
        for entry in entries {
            spine.push(entry);
        }
        spine
    }
}

impl PartialEq for MetavarSpine {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self.facts.hash == other.facts.hash
            && (self.same_version(other) || self.iter().eq(other.iter()))
    }
}

impl Eq for MetavarSpine {}

impl Hash for MetavarSpine {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.len());
        state.write_u64(self.facts.hash);
    }
}

impl fmt::Debug for MetavarSpine {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_list().entries(self.iter()).finish()
    }
}

/// A spine archived as its entries in binding order beside whether it is known to be the identity.
pub struct SpineEntries;

pub type AsSpineEntries = curios_archive::Via<SpineEntries>;

impl curios_archive::Proxy<MetavarSpine> for SpineEntries {
    type Archivable = (Vec<Term>, bool);

    fn to_archivable(spine: &MetavarSpine) -> impl Borrow<(Vec<Term>, bool)> {
        (spine.iter().cloned().collect(), spine.identity)
    }

    fn from_archivable((entries, identity): (Vec<Term>, bool)) -> Result<MetavarSpine, String> {
        let mut spine = MetavarSpine::from(entries);
        spine.identity = identity;

        Ok(spine)
    }
}
