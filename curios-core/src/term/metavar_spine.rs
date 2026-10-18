//! A metavariable's spine: what each binder it was born under stands for at one occurrence, in binding order — the delayed substitution its solution is read through ([`Metavar::spine`](super::Metavar)).
//!
//! **Persistent, and carrying its facts.** The entries are a persistent vector, shared between every occurrence born under one scope and with the scope that holds them, so a birth costs nothing more than the one before it and an entry added for a binder costs a logarithm of the scope. Beside the entries the spine keeps what a node derives from its children — the hash, the reach and the containment flags, each entry counted once — so a node holding a spine derives its own without reading it, and two spines are told apart by their hashes before their entries.
//!
//! **The identity is known as such.** A spine a metavariable is born with names each of its birth binders as itself, and a reader that would apply or invert it reads it as the identity without walking it ([`MetavarSpine::is_identity`]). Any spine a walk rewrote, or one built from a list, is not known to be one, which is the cautious answer: it is read entry by entry.
//!
//! Entries stand at their ranks, so two spines of one length that hold the same terms are one spine, whatever built them.

#[cfg(test)]
mod tests;

use {
    super::{Bound, Free, Term},
    curios_utilities::Ordered,
    std::{
        borrow::Borrow,
        fmt,
        hash::{Hash, Hasher},
    },
};

/// What a node reads of its children, kept for a spine's entries together: the hash of each entry at its rank, combined so an entry added or removed moves it by that entry alone, the deepest reach, and how many entries hold each containment.
#[derive(Clone, Copy, Default)]
struct Facts {
    hash: u64,
    reach: usize,
    newest_local: Option<u64>,
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
            reach: entry.reach(),
            newest_local: entry.newest_local(),
            metavar: usize::from(entry.has_metavar()),
            transient: usize::from(entry.has_transient()),
            universe_meta: usize::from(entry.has_universe_meta()),
            universe_data: usize::from(entry.has_universe_data()),
            group: usize::from(entry.has_group()),
        }
    }

    fn add(&mut self, share: Self) {
        self.hash = self.hash.wrapping_add(share.hash);
        self.reach = self.reach.max(share.reach);
        self.newest_local = self.newest_local.max(share.newest_local);
        self.metavar += share.metavar;
        self.transient += share.transient;
        self.universe_meta += share.universe_meta;
        self.universe_data += share.universe_data;
        self.group += share.group;
    }

    /// Take `share` out, `reach` and the newest local excepted: a maximum cannot be taken back, so a rewrite reads both off the entries again ([`MetavarSpine::rewritten`]).
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

/// A metavariable's spine. See the module documentation.
#[derive(Clone, Default)]
pub struct MetavarSpine {
    entries: Ordered<Term>,
    facts: Facts,
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

    /// The spine with each entry replaced by what `visit` makes of it, or `None` where it makes nothing new of any: a walk keeps a spine it leaves alone, identity and all.
    pub(crate) fn rewritten(&self, mut visit: impl FnMut(&Term) -> Option<Term>) -> Option<Self> {
        let mut rewritten = None::<Self>;
        for (rank, entry) in self.entries.iter() {
            let Some(new) = visit(entry) else {
                continue;
            };
            let spine = rewritten.get_or_insert_with(|| Self {
                identity: false,
                ..self.clone()
            });
            spine.facts.remove(Facts::of(rank, entry));
            spine.facts.add(Facts::of(rank, &new));
            spine.entries.insert(rank, new);
        }
        let mut rewritten = rewritten?;
        let (reach, newest_local) = rewritten.iter().fold((0, None), |(reach, newest), entry| {
            (reach.max(entry.reach()), newest.max(entry.newest_local()))
        });
        rewritten.facts.reach = reach;
        rewritten.facts.newest_local = newest_local;

        Some(rewritten)
    }

    pub(crate) fn reach(&self) -> usize {
        self.facts.reach
    }

    /// The newest binder an entry holds free, by its index.
    pub(crate) fn newest_local(&self) -> Option<u64> {
        self.facts.newest_local
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
