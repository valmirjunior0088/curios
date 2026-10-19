//! An item's erasure as a store keeps it to be found by the item: its trace, completed where it is filed with the item's identity, the digest of every part and binding it read and the erased form it made ([`ErasedUnit::complete`]).
//!
//! **Found by the item it erased.** A completed record is named by the item's identity ([`curios_core::ItemRun::identity`]), which says what the item means and nothing of where it was written, so an item elaborated again to what it was, or taken from a store, finds the records of every erasure of it.
//!
//! **Taken where every part and binding it read digests as it did.** An item's erased form is a function of its item and of what its erasure read ([`ErasedOver`]): the parts of the declarations around it, how each name it uses is bound in the erased form that declares it, and the forms its evaluation ran, whole. A completed record holds a digest of each, as the elaboration's holds one of each part it read ([`crate::Candidating`]), so it is told standing by what is erased now and never by what its erasure read then.

use {
    super::{ErasedOver, ErasedUnit},
    crate::{Answered, Made, Question, Read, bytes_of, part_digest},
    curios_core::{Global, Module, Places, Trace},
    curios_utilities::Digest,
    std::collections::HashMap,
};

impl ErasedOver<'_> {
    /// Whether every question of `asked`, the erasure of the item declaring `names` where `made` was filed, is a part asked, and every part and binding it read digests now as it did: `unit` holding the erased forms of the items before it.
    pub(super) fn stands(
        &self,
        names: &[&Global],
        made: &Made,
        asked: &Trace<Question, Answered>,
        unit: &ErasedUnit,
    ) -> bool {
        asked
            .asked()
            .iter()
            .zip(&made.reads)
            .all(|((question, answered), read)| {
                *answered == Answered::Part && self.read_now(names, question, unit) == Some(*read)
            })
    }
}

impl ErasedUnit {
    /// Complete the record of the erasure of each item `module` holds, where `places` holds each name, that no filing has: the item's identity, from `items`, what its erasure read, and its erased form, from `forms`, each by the item's place. Nothing completes one whose erasure asked something of the unit as a whole, or read a part or a binding that will not digest, or one of another unit that the unit records nothing of.
    pub fn complete(
        &self,
        module: &Module,
        places: &Places,
        items: &HashMap<usize, Digest>,
        forms: &HashMap<usize, Digest>,
    ) {
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

            let mut reads = Some(Vec::with_capacity(held.len()));
            for (question, answered) in held.asked() {
                let read = match answered {
                    Answered::Part => self.read_then(module, places, &names, question),
                    _ => None,
                };
                match (read, &mut reads) {
                    (Some(read), Some(reads)) => reads.push(read),
                    _ => reads = None,
                }
            }
            held.complete(reads.map(|reads| Made {
                input: *identity,
                reads,
                outputs: vec![*form],
                credited: Vec::new(),
            }));
        }
    }

    /// What the item declaring `names` read of what `question` asks, as a completed record holds it ([`Read`]), off this unit, its module `module` held where `places` says, and what its erasure recorded of other units: `None` where it will not digest.
    fn read_then(
        &self,
        module: &Module,
        places: &Places,
        names: &[&Global],
        question: &Question,
    ) -> Option<Read> {
        let Some(subject) = question
            .subject()
            .filter(|subject| !names.contains(subject))
        else {
            return Some(None);
        };
        let digest = match (question, self.place(subject)) {
            (Question::Erased(_), Some(place)) => self.item(place).read_digest(subject),
            (Question::Ran(_), Some(place)) => self.form_digest(place),
            (Question::Erased(_) | Question::Ran(_), None) => return self.recorded_read(question),
            (part, _) => match places.place(subject) {
                Some(_) => {
                    return part_digest(module, places, part)
                        .ok()
                        .map(|digest| Some(digest.as_deref().and_then(bytes_of)));
                }
                None => return self.recorded_read(question),
            },
        };

        digest.ok().map(|digest| Some(bytes_of(&digest)))
    }

    /// What the unit's erasure recorded of the part or binding `question` asks of another unit: nothing of a fixed unit's, which it records nothing of.
    fn recorded_read(&self, question: &Question) -> Option<Read> {
        Some(
            self.foreign()
                .digest(question)
                .map(|digest| digest.as_deref().and_then(bytes_of)),
        )
    }
}
