//! Which of an earlier walk's verdicts a walk adopts.
//!
//! A unit compiled over a baseline is judged by a walk that is handed the baseline whole — the module an earlier walk judged and the record it filed — and decides for itself, declaration by declaration, which of that walk's verdicts stand. A verdict is adopted where the declaration's terms are the baseline's and every declaration its own filed reads name stands in the part that was read: a signature, with its scheme, its registry entries and its verdict, or a body. Every other declaration is judged. No caller says which declarations are kept, so the trusted base rests on no closure it did not compute, and the record a verdict is held to is the one this crate filed ([Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)).
//!
//! **In the order a module is stored.** A declaration follows what it reads, so every name its record holds is settled, adopted or judged, before the declaration is asked about. A read of a name not settled by then is one the order does not account for, and the declaration is judged.
//!
//! **A read of a unit in scope stands where the unit is the one that was read, or the part read digests as it did.** A record names what a judgment read, so a read of this unit is compared, the baseline's declaration against the one in hand. Of a unit in scope there is no earlier declaration in hand, so a walk records the digest of each part it read of one (`read_digest`) and a later walk digests the part again, unless whoever mounted the unit says it is the one the baseline's walk read (`Globals::standing`). That the scope is the one the earlier walk judged in is what a baseline's address says — a unit is offered as a baseline only after the units it was compiled after — and it is the one claim a caller makes of a baseline.
//!
//! **What a declaration's own terms decide is decided of its terms.** Its universe contexts, the levels and binders it mentions and what it spells of elaboration's own syntax are held to their rules by the walk that first met those terms, so a declaration whose terms are the baseline's is passed over by every such check whether or not its verdict is adopted.

use {
    super::{held, induct_alike, struct_alike},
    curios_core::{
        Certification, Global, InductDecl, Item, Module, Nominal, Places, Polarity, Reads, Term,
        Totality,
    },
    std::collections::{BTreeMap, HashMap},
};

#[cfg(feature = "archive")]
use {
    super::Globals,
    curios_core::{Sharing, StructDecl},
    curios_utilities::{Qualifier, digest},
};

/// An earlier walk over another text of the unit a walk judges: the module it judged, where that module holds each name, and the record it filed.
#[derive(Clone, Copy)]
pub struct Baseline<'a> {
    pub module: &'a Module,
    pub places: &'a Places,
    pub certification: &'a Certification,
}

impl<'a> Baseline<'a> {
    /// Whether the entry `item` carries under `name` is the one `held`, the baseline's item, carries under it: the same declaration, by everything the certifier reads of one, or none on both sides.
    fn entries_alike(name: &Global, item: &Item, held: &Item) -> bool {
        let (this, that) = (item.nominal(name), held.nominal(name));
        let alike = |this: Option<&InductDecl>, that: Option<&InductDecl>| match (this, that) {
            (None, None) => true,
            (Some(this), Some(that)) => induct_alike(this, that),
            _ => false,
        };
        let records = match (
            this.and_then(Nominal::record),
            that.and_then(Nominal::record),
        ) {
            (None, None) => true,
            (Some(this), Some(that)) => struct_alike(this, that),
            _ => false,
        };

        records
            && alike(
                this.and_then(Nominal::inductive),
                that.and_then(Nominal::inductive),
            )
    }

    /// Whether `item`, of `module`, is the baseline's own: the same names bound to the same terms under the same universe context, a group being its members' one fixed point, each marked a proof or not as the baseline's is, and the same registry entries under each name. Elaboration's other stamps take no part, as they take none where an environment is asked whether a declaration is its own; the proof mark does, being the one an environment reads a name by.
    pub(crate) fn holds(&self, item: &Item) -> bool {
        let names = item.declared_names();
        let Some(place) = names.first().and_then(|name| self.places.place(name)) else {
            return false;
        };
        let held = &self.module.items[place.item()];
        let terms = match (item, held) {
            (Item::Let(this), Item::Let(that)) => {
                this.name == that.name
                    && this.universe_context == that.universe_context
                    && this.type_ == that.type_
                    && this.body == that.body
                    && this.proof == that.proof
            }
            (Item::Rec(this), Item::Rec(that)) => {
                this.group == that.group
                    && this.definitions.len() == that.definitions.len()
                    && this
                        .definitions
                        .iter()
                        .zip(&that.definitions)
                        .all(|(this, that)| this.name == that.name && this.proof == that.proof)
            }
            _ => false,
        };

        terms
            && names
                .iter()
                .all(|name| Self::entries_alike(name, item, held))
    }
}

/// The digest of what a read reads of `name`, a declaration a unit in `globals` holds: of its body, what it unfolds to, where `body`, and otherwise of its signature, which is its type, its universe context, the registry entry it carries as [`induct_alike`] and [`struct_alike`] read one, and the certifier's verdict on it, its totality and its entry's polarity vector. `Ok(None)` where no unit holds the name.
///
/// **The certifier's own, of what [`Stood`] says of a declaration of the walk's unit.** A walk records it of each part it read of another unit, and a walk over that walk's unit as a baseline digests the part again where the unit it reads is not the one that was read: the terms are the ones in its scope and the function is this one, so a verdict adopted across units rests on nothing another component computed. Each term is consed alone (`Sharing`), so it is digested under no position and under its binders' names: two parts that digest alike are one to every judgment, and two that differ in a name alone are judged again, which costs a judgment and admits nothing.
#[cfg(feature = "archive")]
pub(crate) fn read_digest(
    globals: &Globals<'_>,
    name: &Global,
    body: bool,
) -> Result<Option<String>, String> {
    let Some(held) = globals.held(name) else {
        return Ok(None);
    };
    let sharing = Sharing::new();
    if body {
        let unfolds = held.value.map(|value| sharing.share(value));

        return curios_archive::to_bytes(&unfolds).map(|bytes| Some(digest(&bytes)));
    }

    // An entry as the certifier reads one: what elaboration alone records is left out.
    let family = globals.induct_decl(name).map(|declaration| InductDecl {
        module: Qualifier::default(),
        rep_public: false,
        polarities: Vec::new(),
        ..declaration.shared(&sharing)
    });
    let record = globals.struct_decl(name).map(|declaration| StructDecl {
        module: Qualifier::default(),
        rep_public: false,
        polarities: Vec::new(),
        ..declaration.shared(&sharing)
    });
    let part = (
        sharing.share(held.type_),
        held.universes.clone(),
        family,
        record,
        globals.partial(name),
        globals.polarities(name).map(<[Polarity]>::to_vec),
    );

    curios_archive::to_bytes(&part).map(|bytes| Some(digest(&bytes)))
}

/// How a declaration a walk has settled came out against the baseline's under its name: whether everything a signature read reads of it is the baseline's — its type, its universe context, its registry entries and the certifier's verdict on it — and whether what it unfolds to is. A proof unfolds to nothing whatever its body, so an edit to one moves no read of it.
#[derive(Clone, Copy)]
struct Stood {
    signature: bool,
    body: bool,
}

/// What a walk over a baseline has settled so far, and so which of the baseline's verdicts it may still adopt.
pub(crate) struct Adopting<'a> {
    baseline: Baseline<'a>,
    stood: HashMap<Global, Stood>,
}

impl<'a> Adopting<'a> {
    /// A walk over `baseline` that has settled nothing yet.
    pub(crate) fn over(baseline: Baseline<'a>) -> Self {
        Self {
            baseline,
            stood: HashMap::new(),
        }
    }

    /// The baseline being walked over.
    pub(crate) fn baseline(&self) -> Baseline<'a> {
        self.baseline
    }

    /// The baseline's record of each name `item` declares, in the order the item declares them, where its verdict on `item` stands; `None` where it does not, and the walk judges the item.
    ///
    /// `item`'s terms are the baseline's ([`Baseline::holds`]). The verdict stands where the record covers each name `item` declares and files a vector for each registry entry `module` holds under one, and every name that record read — of this unit, settled before it, or of a unit in scope, which `abroad` answers for, told whether the read was of the name's body — stands in the part read.
    pub(crate) fn adopts(
        &self,
        item: &Item,
        abroad: impl Fn(&Global, bool) -> bool,
    ) -> Option<Vec<(Totality, &'a Reads)>> {
        let names = item.declared_names();
        let certification = self.baseline.certification;
        let stands = |read: &Global, body: bool| match self.stood.get(read) {
            Some(stood) if body => stood.body,
            Some(stood) => stood.signature,
            // Another unit's, or no one's.
            None => abroad(read, body),
        };

        names
            .iter()
            .map(|name| {
                let totality = certification.totality(name)?;
                let reads = certification.reads(name)?;
                let vectored =
                    item.nominal(name).is_none() || certification.polarities(name).is_some();

                (vectored
                    && reads
                        .signatures
                        .iter()
                        .all(|read| names.contains(&read) || stands(read, false))
                    && reads
                        .bodies
                        .iter()
                        .all(|read| names.contains(&read) || stands(read, true)))
                .then_some((totality, reads))
            })
            .collect()
    }

    /// Record `item` as adopted: everything read of it is the baseline's.
    pub(crate) fn adopted(&mut self, item: &Item) {
        for name in item.declared_names() {
            self.stood.insert(
                *name,
                Stood {
                    signature: true,
                    body: true,
                },
            );
        }
    }

    /// Record how `item`, judged by this walk, came out against the baseline's declaration under each of its names: `partial` is the walk's classification of a name once the item is classified, and `polarities` the vectors it has filed.
    pub(crate) fn judged(
        &mut self,
        item: &Item,
        partial: impl Fn(&Global) -> bool,
        polarities: &BTreeMap<Global, Vec<Polarity>>,
    ) {
        for (index, name) in item.declared_names().into_iter().enumerate() {
            let stood = match held(self.baseline.module, self.baseline.places, name) {
                None => Stood {
                    signature: false,
                    body: false,
                },
                Some(held) => {
                    let certification = self.baseline.certification;
                    let verdict = partial(name)
                        == (certification.totality(name) != Some(Totality::Total))
                        && polarities.get(name).map(Vec::as_slice)
                            == certification.polarities(name);
                    let (same_type, same_body, same_universes) = match item {
                        Item::Let(definition) => (
                            definition.type_ == *held.type_,
                            (!definition.proof).then_some(&definition.body) == held.value,
                            definition.universe_context == *held.universes,
                        ),
                        Item::Rec(rec) => (
                            rec.group.member_type(index) == *held.type_,
                            (!rec.definitions[index].proof)
                                .then_some(&Term::rec_proj(rec.group.clone(), index))
                                == held.value,
                            rec.group.universe_context() == held.universes,
                        ),
                    };

                    Stood {
                        signature: same_type
                            && same_universes
                            && verdict
                            && self.baseline.places.place(name).is_some_and(|place| {
                                let earlier = &self.baseline.module.items[place.item()];

                                Baseline::entries_alike(name, item, earlier)
                            }),
                        body: same_body,
                    }
                }
            };
            self.stood.insert(*name, stood);
        }
    }
}
