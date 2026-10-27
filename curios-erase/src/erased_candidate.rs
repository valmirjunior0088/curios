//! An item's erasure as a store keeps it to be found by the item: its record, completed where it is filed with the item's identity, the digest of every part it read and the erased forms it made ([`ErasedUnit::complete`]).
//!
//! **Found by the item it erased.** A completed record is named by the item's identity ([`curios_core::ItemRun::identity`]), which says what the item means and nothing of where it was written, so an item elaborated again to what it was, or taken from a store, finds the records of every erasure of it.
//!
//! **Taken where every part it read digests as it did.** An item's erased form is a function of its item and of what its erasure read ([`ErasedOver`]): the parts of the declarations around it, how each name it uses is bound in the erased form that declares it, and the forms its evaluation ran, whole. A completed record holds a digest of each, as the certifier's holds one of each part its judgment read (`curios_core::Concluded`), so it is told standing by what is erased now and never by what its erasure read then.

use {
    crate::{
        AskedRecord, AskedTable, ErasedOver, ErasedUnit, HeldAsked, Made, Part, Read, bytes_of,
        declared_digest, erased_digest,
    },
    curios_utilities::{Candidates, Digest, Facet},
    std::{collections::HashMap, rc::Rc},
};

impl ErasedOver<'_> {
    /// Whether every part `asked`, the erasure of the item declaring `names` where `made` was filed, read digests now as it did: `unit` holding the erased forms of the items before it.
    pub fn stands(
        &self,
        names: &[&curios_core::Global],
        made: &Made,
        asked: &AskedRecord,
        unit: &ErasedUnit,
    ) -> bool {
        asked
            .reads()
            .zip(&made.reads)
            .all(|(read, then)| self.read_now(names, read, unit) == Some(*then))
    }
}

impl ErasedUnit {
    /// Complete the record of the erasure of each item `module` holds, where `places` holds each name and `certification` is the record its walk filed, that no filing has: the item's identity, from `items`, what its erasure read, and its erased forms, from `forms`, each by the item's place. Nothing completes one that read a part that will not digest, or one of another unit that the unit records nothing of.
    pub fn complete(
        &self,
        module: &curios_core::Module,
        places: &curios_core::Places,
        certification: &curios_core::Certification,
        items: &HashMap<usize, Digest>,
        forms: &HashMap<usize, Digest>,
    ) {
        curios_profile::profile!("erasures::complete");
        // What each part read digests to, made once however many items read it.
        let mut digests = HashMap::<(curios_core::Global, Part), Option<Read>>::new();
        for place in module.items.places() {
            let names = module.items.names(place);
            let Some(held) = names.first().and_then(|first| self.asked(first)) else {
                continue;
            };
            if held.completed() {
                continue;
            }
            let (Some(identity), Some(form)) = (items.get(&place), forms.get(&place)) else {
                held.complete(None);
                continue;
            };

            let reads = held
                .reads()
                .map(|read| match names.contains(&&read.0) {
                    true => Some(None),
                    false => *digests
                        .entry(read)
                        .or_insert_with(|| self.read_then((module, places, certification), read)),
                })
                .collect::<Option<Vec<_>>>();
            held.complete(reads.map(|reads| Made {
                input: *identity,
                reads,
                forms: *form,
            }));
        }
    }

    /// What an item read of `part` of `name`, as a completed record holds it ([`Read`]), off this unit, its module held where `places` says under `certification`, and what its erasure recorded of other units: `None` where it will not digest.
    fn read_then(
        &self,
        (module, places, certification): (
            &curios_core::Module,
            &curios_core::Places,
            &curios_core::Certification,
        ),
        (name, part): (curios_core::Global, Part),
    ) -> Option<Read> {
        let digest = match (part, self.place(&name), places.place(&name)) {
            (Part::Binding | Part::Forms, Some(place), _) => {
                erased_digest(self, place, (name, part))
            }
            (Part::Signature | Part::Body | Part::Kind, _, Some(_)) => {
                declared_digest(module, places, certification, (name, part))
            }
            _ => return self.recorded_read(&name, part),
        };

        digest
            .ok()
            .map(|digest| Some(digest.as_deref().and_then(bytes_of)))
    }

    /// What the unit's erasure recorded of `part` of `name`, a declaration of another unit: nothing of a fixed unit's, which it records nothing of.
    fn recorded_read(&self, name: &curios_core::Global, part: Part) -> Option<Read> {
        Some(
            self.asked_items()
                .abroad(name, part)
                .map(|digest| digest.as_deref().and_then(bytes_of)),
        )
    }
}

/// The completed records of the erasure of the item declaring `name` first, made from `input`, as `candidates` holds them, each with the digest it is stored under: what an erasure looks among before it erases the item.
pub(super) fn found(
    candidates: &dyn Candidates,
    input: &Digest,
    name: &curios_core::Global,
) -> Vec<HeldAsked> {
    candidates
        .candidates(Facet::Erasure, input)
        .into_iter()
        .filter_map(|object| {
            let member = object
                .member
                .filter(|member| member.kind == AskedTable::KIND)?;
            let (named, asked, made) =
                AskedTable::member(object.bytes.as_slice(), member.index).ok()?;
            let made =
                made.filter(|made| made.input == *input && made.reads.len() == asked.len())?;

            (named == *name)
                .then(|| Rc::new(AskedRecord::restored(asked, Some(made), object.digest)))
        })
        .collect()
}
