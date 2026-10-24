//! What a term is ranked by among the atoms of one reading: its shape, then its identities by age, each by the positions it holds — a summary a node carries, made of its children's.
//!
//! **An identity is compared, never read as a value.** A binder, a metavariable, a universe metavariable and a quality metavariable are each a position in a counter its declaration mints from, and how far the counter had run when an identity was minted follows every walk that minted before it: a mismatch's display built and discarded, a closed run answered from a memo or made again, a probe that opened a binder. So a rank reads an identity only by comparing it with another of its kind, which is its age, the one property of a counter's value no identity minted since moves. For locals that stand in one term, the older is the one whose scope encloses the other's or comes before it in a telescope, since a scope is opened from the body of the one enclosing it: so age orders locals by where their binders stand, and orders a metavariable, which has no binder, the same way.
//!
//! **A node's summary is made of its children's at the cost of what it adds**, as its free-variable set is ([`Frees`](super::Frees)): its structure, every identity a placeholder, with the index of the child whose map it took; a map from each identity, by kind and age, to a hash of the positions it holds; and the wrapping sum of those hashes. The largest child's map is taken as it stands, and each entry of every other child and each identity of the node's own is joined into it under the structure, as Maziarz, Ellis, Lawrence, Fitzgibbon and Peyton Jones join position trees under a structure's tag ([Hashing Modulo Alpha-Equivalence](https://arxiv.org/abs/2105.02856), §4.8): a term's summaries cost n log² n over its nodes, and a node two atoms share is summarized once. The sum, and not an exclusive or, since two identities of one node can hold one position, two universe metavariables at one offset of one level. A metavariable's summary is its spine's, which the spine keeps as it is built and rewritten with each entry's positions placed at its rank, joined with the metavariable's own identity.
//!
//! **A term's shape is one hash of its structure and its sum**, so `f(x, y)` and `f(y, z)` have one shape and `f(x, x)` another, and it is the rank's key. Two terms of one shape are ordered by their maps read in age order to the first entry held apart, an identity before another by its age and one identity by the hash of its positions, so `f(x, y)` and `f(y, x)` are told apart by where `x` stands: the summary is its own tie ([`Tie`]), and no term is read at a comparison.
//!
//! A rank orders and never identifies: two terms of one rank are one term up to a collision of the hashes, which costs at most the order two atoms are put in.

#[cfg(test)]
mod tests;

use {
    super::{Subterm, Term},
    crate::{
        Bound, Free, Instance, InstanceHead, LevelHead, MaskedLevels, Metavar, Quality, Visit,
    },
    curios_algebra::Tie,
    curios_utilities::Sorted,
    std::{
        any::Any,
        cmp::Ordering,
        collections::hash_map::DefaultHasher,
        hash::{Hash, Hasher},
        rc::Rc,
    },
};

/// The kind an identity is of, and the first key it is ordered by: two identities of two kinds are told apart by their kinds, and only two of one kind are compared by age.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Kind {
    Binder,
    Metavariable,
    Universe,
    Quality,
}

/// An identity by its kind and its age.
type Identity = (Kind, u64);

/// What a node carries of its rank. See the module documentation.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct Ranked {
    /// The structure, every identity a placeholder: the tag the node's own identities and the smaller children's entries are joined under.
    structure: u64,
    /// The rank's key: the structure and the sum in one hash.
    shape: u64,
    /// Each identity the term holds, at a hash of the positions it holds.
    positions: Sorted<Identity, u64>,
    /// The wrapping sum of the position hashes, which drops the identities' names.
    sum: u64,
}

const LOCAL: u64 = 1;
const METAVARIABLE: u64 = 2;
const NODE: u64 = 3;
const PARAM: u64 = 4;
const META_OFFSET: u64 = 5;
const META_QUALITY: u64 = 6;
const HERE: u64 = 7;
const LEVEL: u64 = 8;
const SORT: u64 = 9;
const CHILD: u64 = 10;
const OWN: u64 = 11;
const ALONE: u64 = 12;
const BOTH: u64 = 13;
const SPINE_STRUCTURE: u64 = 14;
const SPINE_ENTRY: u64 = 15;
const INSTANCE_OF_LOCAL: u64 = 16;

/// `parts` folded into one hash, in order.
fn mix(parts: &[u64]) -> u64 {
    let mut mixed = 0x9e37_79b9_7f4a_7c15u64;
    for part in parts {
        mixed = (mixed ^ part).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        mixed ^= mixed >> 31;
    }
    mixed
}

fn hashed(value: &impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

/// A node's own structure, every identity a placeholder, beside the identities it holds itself with where each stands in it: a free local, a metavariable's id, the universe metavariables of its levels by slot and offset, the quality metavariables of its sorts by slot.
fn own(subterm: &Subterm) -> (u64, Vec<(Identity, u64)>) {
    match subterm {
        Subterm::Var(var) => match var.as_free().and_then(Free::local_index) {
            Some(index) => (mix(&[LOCAL]), vec![((Kind::Binder, index), HERE)]),
            None => (hashed(subterm), Vec::new()),
        },
        Subterm::Metavar(metavar) => (
            mix(&[
                METAVARIABLE,
                hashed(&metavar.origin),
                metavar.spine.len() as u64,
            ]),
            vec![((Kind::Metavariable, metavar.id.0 as u64), HERE)],
        ),
        _ => {
            let mut visit = Visit::masking_levels(|_, _| None, Term::prop(), true);
            let masked = subterm.traverse(&mut visit);
            let MaskedLevels {
                levels, qualities, ..
            } = visit.take_masked_levels();
            let mut parts = Vec::new();
            let mut identities = Vec::new();
            // An instance's variable head is this node's own identity, as a bare `Var`'s is, and the masked node would spell it by its value.
            match subterm {
                Subterm::Instance(Instance {
                    head: InstanceHead::Var(var),
                    ..
                }) if var.as_free().and_then(Free::local_index).is_some() => {
                    parts.push(INSTANCE_OF_LOCAL);
                    let index = var
                        .as_free()
                        .and_then(Free::local_index)
                        .expect("the head is a local");
                    identities.push(((Kind::Binder, index), HERE));
                }
                _ => parts.push(mix(&[NODE, hashed(&masked)])),
            }
            for (slot, (_, level)) in levels.iter().enumerate() {
                parts.push(u64::from(level.constant));
                let mut metas = Vec::new();
                for (head, offset) in &level.atoms {
                    match head {
                        LevelHead::Param(param) => {
                            parts.push(mix(&[PARAM, param.0 as u64, u64::from(*offset)]));
                        }
                        LevelHead::Meta(meta) => metas.push((*offset, meta.0 as u64)),
                    }
                }
                metas.sort_unstable();
                for (offset, id) in metas {
                    parts.push(mix(&[META_OFFSET, u64::from(offset)]));
                    identities.push((
                        (Kind::Universe, id),
                        mix(&[LEVEL, slot as u64, u64::from(offset)]),
                    ));
                }
            }
            for (slot, quality) in qualities.iter().enumerate() {
                match quality {
                    Quality::Meta(meta) => {
                        parts.push(META_QUALITY);
                        identities
                            .push(((Kind::Quality, meta.0 as u64), mix(&[SORT, slot as u64])));
                    }
                    other => parts.push(hashed(other)),
                }
            }
            (mix(&parts), identities)
        }
    }
}

/// A map and its sum as they are joined into.
#[derive(Default)]
struct Joined {
    positions: Sorted<Identity, u64>,
    sum: u64,
}

impl Joined {
    /// `identity` at `position`, joined under `tag` with what the map already holds of it.
    fn place(&mut self, tag: u64, identity: Identity, position: u64) {
        let placed = match self.positions.get(&identity) {
            Some(held) => {
                self.sum = self.sum.wrapping_sub(mix(&[*held]));
                mix(&[tag, BOTH, *held, position])
            }
            None => mix(&[tag, ALONE, position]),
        };
        self.sum = self.sum.wrapping_add(mix(&[placed]));
        self.positions.insert(identity, placed);
    }

    /// `entries` joined in under `tag`, the smaller map into the larger: the entries are placed in a map of their own, and the two maps are unioned so the paths to the keys both hold are copied and every other node is shared.
    fn join(&mut self, tag: u64, entries: impl Iterator<Item = (Identity, u64)>) {
        let mut small = Joined::default();
        for (identity, position) in entries {
            small.place(tag, identity, position);
        }
        if small.positions.is_empty() {
            return;
        }
        let mut sum = self.sum.wrapping_add(small.sum);
        let positions = self
            .positions
            .union_with(&small.positions, |_, held, placed| {
                sum = sum
                    .wrapping_sub(mix(&[*held]))
                    .wrapping_sub(mix(&[*placed]));
                let both = mix(&[tag, BOTH, *held, *placed]);
                sum = sum.wrapping_add(mix(&[both]));
                Some(both)
            });
        self.positions = positions;
        self.sum = sum;
    }
}

/// The summary of a node whose own structure and identities are `own` and whose children's summaries are `children`, in order: the largest child's map taken as it stands, every other child's entries and the node's own identities joined into it under the structure.
fn joined(own: (u64, Vec<(Identity, u64)>), children: &[&Ranked]) -> Ranked {
    let (own_structure, identities) = own;
    let largest = children
        .iter()
        .enumerate()
        .max_by_key(|(_, child)| child.positions.len())
        .map(|(index, _)| index);
    let mut parts = Vec::with_capacity(children.len() + 3);
    parts.push(own_structure);
    parts.push(children.len() as u64);
    parts.push(largest.map_or(u64::MAX, |index| index as u64));
    parts.extend(children.iter().map(|child| child.structure));
    let structure = mix(&parts);

    let mut joined = match largest {
        Some(index) => Joined {
            positions: children[index].positions.clone(),
            sum: children[index].sum,
        },
        None => Joined::default(),
    };
    for (index, child) in children.iter().enumerate() {
        if Some(index) == largest {
            continue;
        }
        joined.join(
            structure,
            child
                .positions
                .iter()
                .map(|(identity, position)| (*identity, mix(&[CHILD, index as u64, *position]))),
        );
    }
    joined.join(
        structure,
        identities
            .into_iter()
            .map(|(identity, position)| (identity, mix(&[OWN, position]))),
    );

    Ranked {
        structure,
        shape: mix(&[structure, joined.sum]),
        positions: joined.positions,
        sum: joined.sum,
    }
}

impl Ranked {
    /// The summary of a term that holds no identity: its structural hash is its structure, and it holds no positions.
    pub(super) fn closed(hash: u64) -> Self {
        Ranked {
            structure: hash,
            shape: mix(&[hash, 0]),
            ..Ranked::default()
        }
    }

    /// The summary of `subterm`, every child's filled: a node's own structure and identities joined with its children's summaries in order.
    pub(super) fn of(subterm: &Subterm) -> Self {
        let mut children = Vec::new();
        subterm.any_child_term(&mut |child| {
            children.push(Rc::clone(child.ranked_filled()));
            false
        });
        let children = children.iter().map(|child| &**child).collect::<Vec<_>>();
        joined(own(subterm), &children)
    }

    /// The summary of a metavariable's node: its spine's, kept as the spine is built, joined with the metavariable's own identity.
    pub(super) fn of_metavar(metavar: &Metavar) -> Self {
        joined(
            own(&Subterm::Metavar(metavar.clone())),
            &[metavar.spine.ranked()],
        )
    }

    /// The rank's key.
    pub(super) fn shape(&self) -> u64 {
        self.shape
    }
}

/// The summary of a spine's entries, each placed at its rank, kept as the spine is built and rewritten: a spine is a sequence, so an entry's positions stand at its rank and an entry taken out takes out what it put in, and an identity leaves the map with the last entry holding it, counted as the spine's names are.
#[derive(Clone, Debug, Default)]
pub(super) struct SpineRanked {
    ranked: Ranked,
    /// How many entries hold each identity.
    holding: Sorted<Identity, u32>,
}

impl SpineRanked {
    pub(super) fn ranked(&self) -> &Ranked {
        &self.ranked
    }

    /// `entry`'s summary placed at `rank`, put in or taken out.
    pub(super) fn changed(&mut self, rank: i64, entry: &Ranked, put_in: bool) {
        let share = mix(&[SPINE_STRUCTURE, rank as u64, entry.structure]);
        self.ranked.structure = match put_in {
            true => self.ranked.structure.wrapping_add(share),
            false => self.ranked.structure.wrapping_sub(share),
        };
        for (identity, position) in entry.positions.iter() {
            let contribution = mix(&[SPINE_ENTRY, rank as u64, *position]);
            let held = self.ranked.positions.get(identity).copied();
            if let Some(held) = held {
                self.ranked.sum = self.ranked.sum.wrapping_sub(mix(&[held]));
            }
            let holding = self.holding.get(identity).copied().unwrap_or(0);
            let (holding, hash) = match put_in {
                true => (holding + 1, held.unwrap_or(0).wrapping_add(contribution)),
                false => (
                    holding - 1,
                    held.expect("an entry taken out was put in")
                        .wrapping_sub(contribution),
                ),
            };
            if holding == 0 {
                self.holding.remove(identity);
                self.ranked.positions.remove(identity);
            } else {
                self.holding.insert(*identity, holding);
                self.ranked.sum = self.ranked.sum.wrapping_add(mix(&[hash]));
                self.ranked.positions.insert(*identity, hash);
            }
        }
        self.ranked.shape = mix(&[self.ranked.structure, self.ranked.sum]);
    }
}

/// Two atoms of one shape, ordered by their identities in the order of their ages, each identity by the hash of the positions it holds: read off the two maps to the first entry held apart, no term touched.
impl Tie for Ranked {
    fn tie(&self, other: &dyn Tie) -> Ordering {
        let Some(other) = other.as_any().downcast_ref::<Ranked>() else {
            return Ordering::Equal;
        };
        let mut this = self.positions.iter();
        let mut that = other.positions.iter();
        loop {
            match (this.next(), that.next()) {
                (None, None) => return Ordering::Equal,
                (None, Some(_)) => return Ordering::Less,
                (Some(_), None) => return Ordering::Greater,
                (Some((this_identity, this_position)), Some((that_identity, that_position))) => {
                    match this_identity
                        .cmp(that_identity)
                        .then_with(|| this_position.cmp(that_position))
                    {
                        Ordering::Equal => {}
                        ordering => return ordering,
                    }
                }
            }
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
