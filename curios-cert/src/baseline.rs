//! Which of an earlier walk's verdicts a walk adopts.
//!
//! A unit compiled over a baseline is judged by a walk that is handed the baseline whole — the module an earlier walk judged and the record it filed — and decides for itself, declaration by declaration, which of that walk's verdicts stand. A verdict is adopted where the declaration's terms are the baseline's and every declaration its own filed reads name stands in the part that was read: a signature, with its scheme, its registry entries and its verdict, or a body. Every other declaration is judged. No caller says which declarations are kept, so the trusted base rests on no closure it did not compute, and the record a verdict is held to is the one this crate filed ([Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)).
//!
//! **In the order a module is stored.** A declaration follows what it reads, so every name its record holds is settled, adopted or judged, before the declaration is asked about. A read of a name not settled by then is one the order does not account for, and the declaration is judged.
//!
//! **A read of a unit in scope stands on the baseline's address.** A record names what a judgment read and not what the name held, so a read of this unit is compared, the baseline's declaration against the one in hand, and a read of a unit in scope cannot be: it stands wherever the name is in scope. That the scope is the one the earlier walk judged in is what a baseline's address says — a unit is offered as a baseline only after the units it was compiled after — and it is the one claim a caller makes of a baseline.
//!
//! **What a declaration's own terms decide is decided of its terms.** Its universe contexts, the levels and binders it mentions and what it spells of elaboration's own syntax are held to their rules by the walk that first met those terms, so a declaration whose terms are the baseline's is passed over by every such check whether or not its verdict is adopted.

use {
    super::{held, induct_alike, struct_alike},
    curios_core::{
        Certification, Certified, Global, Item, Module, Places, Polarity, Term, Totality,
    },
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
    /// Whether the registry entries `module` holds under `name` are the baseline's: each registry holding the same declaration under it, by everything the certifier reads of one, or holding none on both sides.
    fn entries_alike(&self, name: &Global, module: &Module) -> bool {
        let inducts = match (
            module.induct_decls.get(name),
            self.module.induct_decls.get(name),
        ) {
            (None, None) => true,
            (Some(this), Some(that)) => induct_alike(this, that),
            _ => false,
        };
        let structs = match (
            module.struct_decls.get(name),
            self.module.struct_decls.get(name),
        ) {
            (None, None) => true,
            (Some(this), Some(that)) => struct_alike(this, that),
            _ => false,
        };

        inducts && structs
    }

    /// Whether `item`, of `module`, is the baseline's own: the same names bound to the same terms under the same universe context, a group being its members' one fixed point, each marked a proof or not as the baseline's is, and the same registry entries under each name. Elaboration's other stamps take no part, as they take none where an environment is asked whether a declaration is its own; the proof mark does, being the one an environment reads a name by.
    pub(crate) fn holds(&self, item: &Item, module: &Module) -> bool {
        let names = item.declared_names();
        let Some(place) = names.first().and_then(|name| self.places.place(name)) else {
            return false;
        };
        let terms = match (item, &self.module.items[place.item()]) {
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

        terms && names.iter().all(|name| self.entries_alike(name, module))
    }
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
    /// `item`'s terms are the baseline's ([`Baseline::holds`]). The verdict stands where the record covers each name `item` declares and files a vector for each registry entry `module` holds under one, and every name that record read — of this unit, settled before it, or of a unit in scope, which `in_scope` answers for — stands in the part read.
    pub(crate) fn adopts(
        &self,
        item: &Item,
        module: &Module,
        in_scope: impl Fn(&Global) -> bool,
    ) -> Option<Vec<&'a Certified>> {
        let names = item.declared_names();
        let certification = self.baseline.certification;
        let stands = |read: &Global, part: fn(&Stood) -> bool| match self.stood.get(read) {
            Some(stood) => part(stood),
            // A unit in scope is the one the baseline's walk read: a baseline is offered after the same units.
            None => in_scope(read),
        };

        names
            .iter()
            .map(|name| {
                let certified = certification.certified(name)?;
                let vectored = (!module.induct_decls.contains_key(name)
                    && !module.struct_decls.contains_key(name))
                    || certification.polarities(name).is_some();
                let reads = &certified.reads;

                (vectored
                    && reads
                        .signatures
                        .iter()
                        .all(|read| names.contains(&read) || stands(read, |stood| stood.signature))
                    && reads
                        .bodies
                        .iter()
                        .all(|read| names.contains(&read) || stands(read, |stood| stood.body)))
                .then_some(certified)
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
        module: &Module,
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
                            && self.baseline.entries_alike(name, module),
                        body: same_body,
                    }
                }
            };
            self.stood.insert(*name, stood);
        }
    }
}
