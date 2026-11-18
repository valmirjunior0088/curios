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
    curios_common::{Digest, Qualifier, digest},
    curios_core::{
        Applications, Certification, Concluded, Definition, DefinitionKind, Definitions, Global,
        InductDecl, Item, ItemRun, Nominal, Nominals, Places, Polarity, Reads, RecDefinition,
        RecItem, Sharing, StructDecl, Totality, Variance,
    },
    std::collections::{BTreeMap, HashMap},
};

/// A unit as its walk filed it: the module the walk accepted, where that module holds each name, and the record the walk filed of it. How the certifier reads a unit it does not judge again: a unit in scope (`Globals::mount`), a baseline a walk over another text of the unit adopts verdicts from, and a unit a stage outside the certifier digests a part of ([`part_digest`]).
///
/// **The record answers for every name the module holds**: a totality for each definition and both vectors for each family and record, as every walk's record does, and a unit is made of its walk's accepted answer whole (`curios_unit::Unit`). So each is read here per name, and a miss is a construction bug of the compiler's own, read as one; bytes no walk wrote, a store's damaged or a forger's, are outside the claim ([the soundness board](../../documentation/design/soundness/the-soundness-board.md)).
#[derive(Clone, Copy)]
pub struct Filed<'a> {
    pub definitions: &'a Definitions,
    pub places: &'a Places,
    pub certification: &'a Certification,
}

/// What a read of a filed record that misses a name the unit holds is.
const ANSWERS: &str = "a filed record answers for every name its unit holds";

impl<'a> Filed<'a> {
    /// Whether the definition `name`, which this unit holds, is not known to terminate.
    pub fn partial(&self, name: &Global) -> bool {
        self.certification.totality(name).expect(ANSWERS) == Totality::Partial
    }

    /// The polarity vector of the family or record this unit holds under `name`: `None` where it holds none there.
    pub fn polarities(&self, name: &Global) -> Option<&'a [Polarity]> {
        self.vectored(name)
            .then(|| self.certification.polarities(name).expect(ANSWERS))
    }

    /// The variance vector of the family or record this unit holds under `name`: `None` where it holds none there.
    pub fn variances(&self, name: &Global) -> Option<&'a [Variance]> {
        self.vectored(name)
            .then(|| self.certification.variances(name).expect(ANSWERS))
    }

    /// How the definition `name`, which this unit holds, applies its parameters, where it applies one every use of which is seen.
    pub fn applications(&self, name: &Global) -> Option<&'a Applications> {
        self.certification.applications(name)
    }

    /// What a signature read of `name`, a definition this unit holds, reads beside its terms.
    pub fn beside(&self, name: &Global) -> Beside<'a> {
        (
            self.partial(name),
            self.polarities(name),
            self.variances(name),
            self.applications(name),
        )
    }

    /// Whether this unit holds a family or a record under `name`, whose vectors its record files.
    fn vectored(&self, name: &Global) -> bool {
        self.definitions
            .nominal(self.places, name)
            .is_some_and(|nominal| nominal.inductive().is_some() || nominal.record().is_some())
    }

    /// Whether the entry `item` holds under `name` is the one `held`, the baseline's item, holds under it: the same declaration, by everything the certifier reads of one, or none on both sides.
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

    /// Whether `item`, of `module`, is the baseline's own: the same names bound to the same terms under the same universe context, a group being its members' one fixed point, and the same registry entries under each name. Elaboration's stamps take no part, its proof marks and value sorts among them: the walk states those of every item it judges or takes by its own reading of the terms (`certifier::stating`), so an item whose terms are the baseline's is stated as the baseline's was.
    ///
    /// Each form is compared by a function that destructures it whole ([`definition_alike`], [`group_alike`], [`nominals_alike`]), so a field added to the representation is classified there or does not compile. That is the rule and not a courtesy: comparing by the fields someone thought of is how the `rec` form came to be compared by its group and its members' names and marks, and silently not by the universe context its members' levels mean, which admitted Hurkens' paradox through a group adopted under constraints no walk assumed.
    pub(crate) fn holds(&self, item: &Item) -> bool {
        let names = item.declared_names();
        let Some(place) = names.first().and_then(|name| self.places.place(name)) else {
            return false;
        };
        let held = &self.definitions.items[place.item()];
        let terms = match (item, held) {
            (Item::Let(this), Item::Let(that)) => definition_alike(this, that),
            (Item::Rec(this), Item::Rec(that)) => group_alike(this, that),
            (Item::Nominal(this), Item::Nominal(that)) => nominals_alike(this, that),
            _ => false,
        };

        terms
            && names
                .iter()
                .all(|name| Self::entries_alike(name, item, held))
    }
}

/// Whether a `let` item is the baseline's: everything a judgment reads of a name in scope, the definition destructured whole so that what is left out is left out on purpose.
///
/// What is read is the type, the body, the universe context and whether the declaration wrote the definition, which every statement the walk makes of the definition is read off. What is not: what else a kind says, which no judgment reads; the island, which is elaboration's record; and the proof mark and the value sort, which the walk states of the definition itself.
fn definition_alike(this: &Definition, that: &Definition) -> bool {
    let Definition {
        name,
        universe_context,
        type_,
        body,
        kind,
        island: _,
        proof: _,
        value_sort: _,
    } = this;

    *name == that.name
        && *universe_context == that.universe_context
        && *type_ == that.type_
        && *body == that.body
        && authored(kind) == authored(&that.kind)
}

/// Whether a declaration wrote a definition of `kind`, which is what the walk reads of a kind where it states the definition a proof ([`check_definition`](crate::check_definition)).
fn authored(kind: &DefinitionKind) -> bool {
    matches!(kind, DefinitionKind::Authored)
}

/// Whether a nominal declaration is the baseline's by its names: the same names in the same order, each entry compared under its name by [`Filed::entries_alike`], which is everything its made formers and constructors are read off.
fn nominals_alike(this: &Nominals, that: &Nominals) -> bool {
    let names = |nominals: &Nominals| {
        nominals
            .entries()
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
    };

    names(this) == names(that)
}

/// Whether a `rec` item is the baseline's: the universe context its members' levels mean, the group's own scopes, which hold every member's type and body, and each member by what is read of one.
///
/// The context is the item's and not the group's, positionally the parameters of whatever declaration holds it, and `globals::held` publishes it to every reader of a member — so a group adopted under a context no walk assumed leaves its members' products quantifying where a reader's instance says rather than where the declaration's own constraints allow.
fn group_alike(this: &RecItem, that: &RecItem) -> bool {
    this.universe_context() == that.universe_context()
        && this.group() == that.group()
        && this.exports().len() == that.exports().len()
        && this
            .exports()
            .iter()
            .zip(that.exports())
            .all(|(this, that)| member_alike(this, that))
}

/// Whether a group's member is the baseline's. Its terms are the group's, so what is left is the name it is exported under and whether the declaration wrote it; the rest is what [`definition_alike`] leaves out, for the reasons stated there.
fn member_alike(this: &RecDefinition, that: &RecDefinition) -> bool {
    let RecDefinition {
        name,
        kind,
        island: _,
        proof: _,
    } = this;

    *name == that.name && authored(kind) == authored(&that.kind)
}

/// The digest of what a read reads of `name`, a declaration a unit in `globals` holds: of its body, what it unfolds to, where `body`, and otherwise of its signature, which is its type, its universe context, the registry entry it carries as [`induct_alike`] and [`struct_alike`] read one, the variance vector it carries aside, which no judgment of another unit reads, and the certifier's verdict on it, its totality and its entry's polarity vector. `Ok(None)` where no unit holds the name.
///
/// **The certifier's own, of what [`Stood`] says of a declaration of the walk's unit.** A walk records it of each part it read of another unit, and a walk over that walk's unit as a baseline digests the part again where the unit it reads is not the one that was read: the terms are the ones in its scope and the function is this one, so a verdict adopted across units rests on nothing another component computed. Each term is consed alone (`Sharing`), so it is digested under no position and under its binders' names: two parts that digest alike are one to every judgment, and two that differ in a name alone are judged again, which costs a judgment and admits nothing.
pub fn read_digest(
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
        applications: globals.applications(name),
    };

    read.digest(body).map(Some)
}

/// [`read_digest`] of `name` as `module` holds it where `places` says, under the verdict `partial` and the vectors `polarities` and `variances`: what a walk reads of a declaration of its own unit, as it settled it or as the baseline holds it. `Ok(None)` where the module holds no such name.
pub fn held_digest(
    definitions: &Definitions,
    places: &Places,
    name: &Global,
    (partial, polarities, variances, applications): Beside<'_>,
    body: bool,
) -> Result<Option<String>, String> {
    let Some(held) = held(definitions, places, name) else {
        return Ok(None);
    };
    let read = Readable {
        held,
        family: definitions.induct_decl(places, name),
        record: definitions.struct_decl(places, name),
        partial,
        polarities,
        variances,
        applications,
    };

    read.digest(body).map(Some)
}

/// What a signature read of a definition reads beside its terms: whether it is not known to terminate, the vectors of the entry it is the former of, and how it applies its parameters.
pub type Beside<'a> = (
    bool,
    Option<&'a [Polarity]>,
    Option<&'a [Variance]>,
    Option<&'a Applications>,
);

/// The certifier's digest of what a read reads of `name`, a declaration `unit` holds, under the verdict and vectors its record files for it: of its body where `body`, and otherwise of its signature, as a walk digests a part it read of a unit in scope (`read_digest`). It is what a stage that reads declarations through the certifier records of a read and asks the read again by, so what it compares is the certifier's own digest. `Ok(None)` where the unit holds no such name.
pub fn part_digest(unit: Filed<'_>, name: &Global, body: bool) -> Result<Option<String>, String> {
    if unit.places.place(name).is_none() {
        return Ok(None);
    }

    held_digest(unit.definitions, unit.places, name, unit.beside(name), body)
}

/// What a read reads of a declaration: its terms, the registry entry it is the former of, and the certifier's verdict on it.
struct Readable<'a> {
    held: Held<'a>,
    family: Option<&'a InductDecl>,
    record: Option<&'a StructDecl>,
    partial: bool,
    polarities: Option<&'a [Polarity]>,
    variances: Option<&'a [Variance]>,
    applications: Option<&'a Applications>,
}

impl Readable<'_> {
    /// The digest of what a read reads of the declaration: what it unfolds to, where `body`, and otherwise its signature. See [`read_digest`].
    fn digest(&self, body: bool) -> Result<String, String> {
        let sharing = Sharing::new();
        if body {
            // The sort a definition states for its value goes with what it unfolds to, since `Certifier::value_sort` records a body read where `Sort::of` asks a publication.
            let unfolds = (
                self.held.value.map(|value| sharing.share(value)),
                self.held.value_sort.map(|sort| sharing.share(sort)),
            );

            return curios_archive::to_bytes(&unfolds).map(|bytes| digest(&bytes));
        }

        // An entry as the certifier reads one: what elaboration alone records is left out.
        let family = self.family.map(|declaration| {
            let shared = declaration.shared(&sharing);
            InductDecl::new(
                shared.universe_context().clone(),
                shared.arity().clone(),
                shared.constructors().clone(),
                shared.result_sort().clone(),
                Qualifier::default(),
                false,
            )
        });
        let record = self.record.map(|declaration| StructDecl {
            module: Qualifier::default(),
            rep_public: false,
            polarities: Vec::new(),
            variances: Vec::new(),
            ..declaration.shared(&sharing)
        });
        let part = (
            sharing.share(self.held.type_),
            // Held by both parts: the publication is read by `Sort::of` with nothing run, and a reader's own value sort is stated through it, so a verdict may rest on it whichever read the asking walk recorded, and covering it twice refuses an adoption that would have stood rather than standing on one that would not.
            self.held.value_sort.map(|sort| sharing.share(sort)),
            self.held.universes.clone(),
            family,
            record,
            self.partial,
            self.polarities.map(<[Polarity]>::to_vec),
            self.variances.map(<[Variance]>::to_vec),
            self.applications.cloned(),
        );

        curios_archive::to_bytes(&part).map(|bytes| digest(&bytes))
    }
}

/// Complete what judging each definition of `module` read, held where `places` says and concluded as `certification` records, where no filing has ([`curios_core::ReadRecord::complete`]): with the identity of the item judged, the certifier's own digest of each part read — of a declaration of the unit, over the unit as it stands, and of another unit's, the digest the walk recorded of it, none of a fixed unit's — and the verdict. What a store's filing asks of the certifier before the unit is filed, so a later walk at its address finds the verdict by the item it judged, and nothing outside the certifier computes any part of what that walk compares.
pub fn complete(unit: Filed<'_>) {
    curios_profile::profile!("judgments::complete");
    let mut globals = Globals::default();
    globals.mount(unit);
    let mut completing = Completing {
        unit,
        globals,
        digests: HashMap::new(),
        identities: HashMap::new(),
    };
    for (name, record) in unit.certification.records() {
        if record.completed() {
            continue;
        }
        record.complete(completing.concluded(name, record));
    }
}

/// What a filing completes a walk's records with: the unit, as the walk filed it and mounted, and what the walk recorded, each part read digested and each item named once however many records ask.
struct Completing<'a> {
    unit: Filed<'a>,
    globals: Globals<'a>,
    /// What each part of the unit read digests to, by its name and whether it is the body: `None` where it will not digest.
    digests: HashMap<(Global, bool), Option<Option<[u8; 32]>>>,
    /// The identity of each item judged, by its place: `None` where it will not archive.
    identities: HashMap<usize, Option<Digest>>,
}

impl Completing<'_> {
    /// What judging `name`, which read `reads`, concluded and read: `None` where no record of it can be completed.
    fn concluded(&mut self, name: &Global, reads: &Reads) -> Option<Concluded> {
        let Filed {
            definitions,
            places,
            certification,
        } = self.unit;
        let place = places.place(name)?.item();
        let input =
            *self
                .identities
                .entry(place)
                .or_insert_with(|| match definitions.items.part(place) {
                    Some(stored) => Some(stored.digest),
                    None => ItemRun::identity(&definitions.items[place]).ok(),
                });
        let signatures = reads
            .signatures
            .iter()
            .map(|signature| self.read(signature, false))
            .collect::<Option<Vec<_>>>()?;
        let bodies = reads
            .bodies
            .iter()
            .map(|body| self.read(body, true))
            .collect::<Option<Vec<_>>>()?;

        Some(Concluded {
            input: input?,
            signatures,
            bodies,
            totality: certification.totality(name).expect(ANSWERS),
            applications: self.unit.applications(name).cloned().unwrap_or_default(),
            vectors: self
                .unit
                .polarities(name)
                .zip(self.unit.variances(name))
                .map(|(polarities, variances)| (polarities.to_vec(), variances.to_vec())),
        })
    }

    /// What a record holds of a read of `read`, its body where `body` and its signature otherwise: of a declaration of the unit, the certifier's digest of it now; of another unit's, the digest the walk recorded; of a fixed unit's, nothing. `None` where it will not digest.
    fn read(&mut self, read: &Global, body: bool) -> Option<Option<Option<[u8; 32]>>> {
        if self.unit.places.place(read).is_some() {
            let globals = &self.globals;
            let digest = *self.digests.entry((*read, body)).or_insert_with(|| {
                read_digest(globals, read, body)
                    .ok()
                    .map(|digest| digest.as_deref().and_then(bytes_of))
            });

            return digest.map(Some);
        }
        let recorded = self
            .unit
            .certification
            .abroad(read)
            .and_then(|read| match body {
                true => read.body.as_deref(),
                false => read.signature.as_deref(),
            });

        Some(recorded.map(bytes_of))
    }
}

/// The thirty-two bytes a digest spells.
pub fn bytes_of(digest: &str) -> Option<[u8; 32]> {
    Digest::from_hex(digest).map(|digest| *digest.as_bytes())
}

/// How a declaration a walk has settled came out against the baseline's under its name: whether everything a signature read reads of it is the baseline's — its type, its universe context, its registry entries and the certifier's verdict on it — and whether what it unfolds to is. A proof unfolds to nothing whatever its body, so an edit to one moves no read of it.
#[derive(Clone, Copy)]
struct Stood {
    signature: bool,
    body: bool,
}

/// What a walk over a baseline has settled so far, and so which of the baseline's verdicts it may still adopt.
pub struct Adopting<'a> {
    baseline: Filed<'a>,
    stood: HashMap<Global, Stood>,
}

impl<'a> Adopting<'a> {
    /// A walk over `baseline` that has settled nothing yet.
    pub fn over(baseline: Filed<'a>) -> Self {
        Self {
            baseline,
            stood: HashMap::new(),
        }
    }

    /// The baseline being walked over.
    pub fn baseline(&self) -> Filed<'a> {
        self.baseline
    }

    /// The baseline's record of each name `item` declares, in the order the item declares them, where its verdict on `item` stands; `None` where it does not, and the walk judges the item.
    ///
    /// `item`'s terms are the baseline's ([`Filed::holds`]). The verdict stands where every name the baseline's record read of it — of this unit, settled before it, or of a unit in scope, which `abroad` answers for, told whether the read was of the name's body — stands in the part read.
    pub fn adopts(
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
        let recorded = self.recorded(item);

        recorded
            .iter()
            .all(|(_, reads)| {
                reads
                    .signatures
                    .iter()
                    .all(|read| names.contains(read) || stands(read, false))
                    && reads
                        .bodies
                        .iter()
                        .all(|read| names.contains(read) || stands(read, true))
            })
            .then_some(recorded)
    }

    /// The baseline's record of each name `item`, one of the baseline's items, declares, in the order the item declares them: the record answers for each ([`Filed`]). What it read is not asked: [`Adopting::adopts`] asks it, and a declaration no edit can have reached takes its record as it stands.
    fn recorded(&self, item: &Item) -> Vec<(Totality, &'a Reads)> {
        let certification = self.baseline.certification;

        item.declared_names()
            .iter()
            .map(|name| {
                (
                    certification.totality(name).expect(ANSWERS),
                    certification.reads(name).expect(ANSWERS),
                )
            })
            .collect()
    }

    /// Record `item` as adopted: everything read of it is the baseline's.
    pub fn adopted(&mut self, item: &Item) {
        for name in item.declared_names() {
            self.stood.insert(
                name,
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
    pub fn judged(
        &mut self,
        (definitions, places): (&Definitions, &Places),
        item: &Item,
        partial: impl Fn(&Global) -> bool,
        polarities: &BTreeMap<Global, Vec<Polarity>>,
        variances: &BTreeMap<Global, Vec<Variance>>,
        applications: &BTreeMap<Global, Applications>,
    ) {
        let baseline = self.baseline;
        let certification = baseline.certification;
        for name in item.declared_names() {
            let now = |body| {
                let verdict = (
                    partial(&name),
                    polarities.get(&name).map(Vec::as_slice),
                    variances.get(&name).map(Vec::as_slice),
                    applications.get(&name),
                );
                held_digest(definitions, places, &name, verdict, body)
                    .ok()
                    .flatten()
            };
            let then = |body| {
                let verdict = (
                    certification.totality(&name) != Some(Totality::Total),
                    certification.polarities(&name),
                    certification.variances(&name),
                    certification.applications(&name),
                );
                held_digest(baseline.definitions, baseline.places, &name, verdict, body)
                    .ok()
                    .flatten()
            };
            let alike = |body| now(body).is_some_and(|now| Some(now) == then(body));
            let stood = Stood {
                signature: alike(false),
                body: alike(true),
            };
            self.stood.insert(name, stood);
        }
    }
}
