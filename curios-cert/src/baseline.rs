//! Which of an earlier walk's verdicts a walk adopts.
//!
//! A unit compiled over a baseline is judged by a walk that is handed the baseline whole — the module an earlier walk judged and the record it filed — and decides for itself, declaration by declaration, which of that walk's verdicts stand. A verdict is adopted where the declaration's terms are the baseline's and every declaration its own filed reads name stands in the part that was read: a signature, with its scheme, its registry entries and its verdict, or a body. Every other declaration is judged. No caller says which declarations are kept, so the trusted base rests on no closure it did not compute, and the record a verdict is held to is the one this crate filed ([Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)).
//!
//! **In the order a module is stored.** A declaration follows what it reads, so every name its record holds is settled, adopted or judged, before the declaration is asked about. A read of a name not settled by then is one the order does not account for, and the declaration is judged.
//!
//! **Asked only where an edit could have reached.** The record read backwards says which declarations read each name, so the walk finds from the edit which declarations a moved one could reach, and asks the reads of those alone; one none reaches is not visited, every declaration its reads name standing: the walk reads it where the baseline holds it once it has passed its place, as it would read one it adopted there, and its record stands as filed.
//!
//! **A read of a unit in scope stands where the unit is the one that was read, or the part read digests as it did.** A record names what a judgment read, so a read of this unit is compared, the baseline's declaration against the one in hand. Of a unit in scope there is no earlier declaration in hand, so a walk records the digest of each part it read of one (`read_digest`) and a later walk digests the part again, unless whoever mounted the unit says it is the one the baseline's walk read (`Globals::standing`). That the scope is the one the earlier walk judged in is what a baseline's address says — a unit is offered as a baseline only after the units it was compiled after — and it is the one claim a caller makes of a baseline.
//!
//! **What a declaration's own terms decide is decided of its terms.** Its universe contexts, the levels and binders it mentions and what it spells of elaboration's own syntax are held to their rules by the walk that first met those terms, so a declaration whose terms are the baseline's is passed over by every such check whether or not its verdict is adopted.

use {
    super::{Globals, Held, held, induct_alike, struct_alike},
    curios_core::{
        Certification, Concluded, Global, InductDecl, Item, ItemRun, Module, Nominal, Places,
        Polarity, Reads, Sharing, StructDecl, Totality, UniverseContext, Variance,
    },
    curios_utilities::{Digest, Qualifier, digest},
    std::collections::{BTreeMap, HashMap},
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

/// The digest of what a read reads of `name`, a declaration a unit in `globals` holds: of its body, what it unfolds to, where `body`, and otherwise of its signature, which is its type, its universe context by what it is compared by ([`UniverseContext::inequalities`]), the registry entry it carries as [`induct_alike`] and [`struct_alike`] read one, and the certifier's verdict on it, its totality and its entry's polarity vector. `Ok(None)` where no unit holds the name.
///
/// **The certifier's own, of what [`Stood`] says of a declaration of the walk's unit.** A walk records it of each part it read of another unit, and a walk over that walk's unit as a baseline digests the part again where the unit it reads is not the one that was read: the terms are the ones in its scope and the function is this one, so a verdict adopted across units rests on nothing another component computed. Each term is consed alone (`Sharing`), so it is digested under no position and under its binders' names: two parts that digest alike are one to every judgment, and two that differ in a name alone are judged again, which costs a judgment and admits nothing.
pub(crate) fn read_digest(
    globals: &Globals<'_>,
    name: &Global,
    body: bool,
) -> Result<Option<String>, String> {
    let Some(held) = globals.held(name) else {
        return Ok(None);
    };
    let read = Readable {
        held,
        family: globals.induct_decl(name),
        record: globals.struct_decl(name),
        partial: globals.partial(name),
        polarities: globals.polarities(name),
        variances: globals.variances(name),
    };

    read.digest(body).map(Some)
}

/// [`read_digest`] of `name` as `module` holds it where `places` says, under the verdict `partial` and the vectors `polarities` and `variances`: what a walk reads of a declaration of its own unit, as it settled it or as the baseline holds it. `Ok(None)` where the module holds no such name.
pub(crate) fn held_digest(
    module: &Module,
    places: &Places,
    name: &Global,
    (partial, polarities, variances): (bool, Option<&[Polarity]>, Option<&[Variance]>),
    body: bool,
) -> Result<Option<String>, String> {
    let Some(held) = held(module, places, name) else {
        return Ok(None);
    };
    let read = Readable {
        held,
        family: module.induct_decl(places, name),
        record: module.struct_decl(places, name),
        partial,
        polarities,
        variances,
    };

    read.digest(body).map(Some)
}

/// What a read reads of a declaration: its terms, the registry entries its former carries, and the certifier's verdict on it.
struct Readable<'a> {
    held: Held<'a>,
    family: Option<&'a InductDecl>,
    record: Option<&'a StructDecl>,
    partial: bool,
    polarities: Option<&'a [Polarity]>,
    variances: Option<&'a [Variance]>,
}

impl Readable<'_> {
    /// The digest of what a read reads of the declaration: what it unfolds to, where `body`, and otherwise its signature. See [`read_digest`].
    fn digest(&self, body: bool) -> Result<String, String> {
        let sharing = Sharing::new();
        if body {
            let unfolds = self.held.value.map(|value| sharing.share(value));

            return curios_archive::to_bytes(&unfolds).map(|bytes| digest(&bytes));
        }

        // An entry as the certifier reads one: what elaboration alone records is left out, and its scheme is held by what it is compared by.
        let family = self.family.map(|declaration| {
            let read = InductDecl {
                universe_context: UniverseContext::empty(),
                module: Qualifier::default(),
                rep_public: false,
                polarities: Vec::new(),
                variances: Vec::new(),
                ..declaration.shared(&sharing)
            };

            (read, declaration.universe_context.inequalities())
        });
        let record = self.record.map(|declaration| {
            let read = StructDecl {
                universe_context: UniverseContext::empty(),
                module: Qualifier::default(),
                rep_public: false,
                polarities: Vec::new(),
                variances: Vec::new(),
                ..declaration.shared(&sharing)
            };

            (read, declaration.universe_context.inequalities())
        });
        let part = (
            sharing.share(self.held.type_),
            self.held.universes.inequalities(),
            family,
            record,
            self.partial,
            self.polarities.map(<[Polarity]>::to_vec),
            self.variances.map(<[Variance]>::to_vec),
        );

        curios_archive::to_bytes(&part).map(|bytes| digest(&bytes))
    }
}

/// Complete what judging each definition of `module` read, held where `places` says and concluded as `certification` records, where no filing has ([`curios_core::ReadRecord::complete`]): with the identity of the item judged, the certifier's own digest of each part read — of a declaration of the unit, over the unit as it stands, and of another unit's, the digest the walk recorded of it, none of a fixed unit's — and the verdict. What a store's filing asks of the certifier before the unit is filed, so a later walk at its address finds the verdict by the item it judged, and nothing outside the certifier computes any part of what that walk compares.
pub fn complete(module: &Module, places: &Places, certification: &Certification) {
    let mut globals = Globals::default();
    globals.mount(module, places, certification);
    for (name, record) in certification.records() {
        if record.completed() {
            continue;
        }
        record.complete(concluded(
            module,
            places,
            certification,
            &globals,
            name,
            record,
        ));
    }
}

/// What judging `name`, which read `reads`, concluded and read, as a filing completes its record: `None` where no record of it can be completed.
fn concluded(
    module: &Module,
    places: &Places,
    certification: &Certification,
    globals: &Globals<'_>,
    name: &Global,
    reads: &Reads,
) -> Option<Concluded> {
    let place = places.place(name)?.item();
    let input = match module.items.part(place) {
        Some(stored) => stored.digest,
        None => ItemRun::identity(&module.items[place]).ok()?,
    };
    let read = |read: &Global, body: bool| -> Option<Option<Option<[u8; 32]>>> {
        if places.place(read).is_some() {
            return read_digest(globals, read, body)
                .ok()
                .map(|digest| Some(digest.as_deref().and_then(bytes_of)));
        }
        let recorded = certification.abroad(read).and_then(|read| match body {
            true => read.body.as_deref(),
            false => read.signature.as_deref(),
        });

        Some(recorded.map(bytes_of))
    };
    let signatures = reads
        .signatures
        .iter()
        .map(|signature| read(signature, false))
        .collect::<Option<Vec<_>>>()?;
    let bodies = reads
        .bodies
        .iter()
        .map(|body| read(body, true))
        .collect::<Option<Vec<_>>>()?;

    Some(Concluded {
        input,
        signatures,
        bodies,
        totality: certification.totality(name)?,
        vectors: certification
            .polarities(name)
            .zip(certification.variances(name))
            .map(|(polarities, variances)| (polarities.to_vec(), variances.to_vec())),
    })
}

/// The thirty-two bytes a digest spells.
pub(crate) fn bytes_of(digest: &str) -> Option<[u8; 32]> {
    Digest::from_hex(digest).map(|digest| *digest.as_bytes())
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
        let stands = |read: &Global, body: bool| match self.stood.get(read) {
            Some(stood) if body => stood.body,
            Some(stood) => stood.signature,
            // Another unit's, or no one's.
            None => abroad(read, body),
        };
        let recorded = self.recorded(item)?;

        recorded
            .iter()
            .all(|(_, reads)| {
                reads
                    .signatures
                    .iter()
                    .all(|read| names.contains(&read) || stands(read, false))
                    && reads
                        .bodies
                        .iter()
                        .all(|read| names.contains(&read) || stands(read, true))
            })
            .then_some(recorded)
    }

    /// The baseline's record of each name `item` declares, in the order the item declares them, where it covers each and files a vector for each registry entry `module` holds under one; `None` where it does not, and the walk judges the item. What it read is not asked: [`Adopting::adopts`] asks it, and a declaration no edit can have reached takes its record as it stands.
    pub(crate) fn recorded(&self, item: &Item) -> Option<Vec<(Totality, &'a Reads)>> {
        let certification = self.baseline.certification;

        item.declared_names()
            .iter()
            .map(|name| {
                let vectored = item.nominal(name).is_none()
                    || (certification.polarities(name).is_some()
                        && certification.variances(name).is_some());

                vectored
                    .then(|| Some((certification.totality(name)?, certification.reads(name)?)))
                    .flatten()
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

    /// Record how `item`, judged by this walk where `module` holds it at the places `places` gives, came out against the baseline's declaration under each of its names: `partial` is the walk's classification of a name once the item is classified, and `polarities` and `variances` the vectors it has filed.
    ///
    /// **Told by the digest a read records** ([`held_digest`]), of the declaration as the walk settled it and as the baseline holds it: a part that digests alike is one to every judgment, and one that differs in a binder's name alone is told apart, so a record carried from the baseline digests what a walk that judged every declaration would.
    pub(crate) fn judged(
        &mut self,
        (module, places): (&Module, &Places),
        item: &Item,
        partial: impl Fn(&Global) -> bool,
        polarities: &BTreeMap<Global, Vec<Polarity>>,
        variances: &BTreeMap<Global, Vec<Variance>>,
    ) {
        let baseline = self.baseline;
        let certification = baseline.certification;
        for name in item.declared_names() {
            let now = |body| {
                let verdict = (
                    partial(name),
                    polarities.get(name).map(Vec::as_slice),
                    variances.get(name).map(Vec::as_slice),
                );
                held_digest(module, places, name, verdict, body)
                    .ok()
                    .flatten()
            };
            let then = |body| {
                let verdict = (
                    certification.totality(name) != Some(Totality::Total),
                    certification.polarities(name),
                    certification.variances(name),
                );
                held_digest(baseline.module, baseline.places, name, verdict, body)
                    .ok()
                    .flatten()
            };
            let alike = |body| now(body).is_some_and(|now| Some(now) == then(body));
            let stood = Stood {
                signature: alike(false),
                body: alike(true),
            };
            self.stood.insert(*name, stood);
        }
    }
}
