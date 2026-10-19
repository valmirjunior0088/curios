//! What erasing an item made, filed for a later compile that elaborates the item alike: a candidate, holding the erased form, what its erasure asked, and the digest of every part and binding it read ([`ErasedUnit::erasure_candidate`]).
//!
//! **Found by the item it erased.** A candidate is named by the item's identity ([`curios_core::ItemRun::identity`]), which says what the item means and nothing of where it was written, so an item elaborated again to what it was, or taken from a store, finds the candidates of every erasure of it.
//!
//! **Taken where every part and binding it read digests as it did.** An item's erased form is a function of its item and of what its erasure read ([`ErasedOver`]): the parts of the declarations around it, how each name it uses is bound in the erased form that declares it, and the forms its evaluation ran, whole. A candidate holds a digest of each, as the elaboration's holds one of each part it read ([`crate::Candidating`]), so it is told standing by what is erased now and never by what its erasure read then.

use {
    super::{ErasedOver, ErasedUnit},
    crate::{Answered, Question, Read, TraceTable, bytes_of, part_digest},
    curios_core::{Global, Module, Places, Trace},
    curios_utilities::{Digest, Facet, Object, candidate},
};

/// An item's erasure as a store keeps it. See the module documentation.
#[curios_archive::archived]
struct ErasureCandidate {
    /// The identity of the item erased, whole, of which the candidate's name holds a prefix.
    input: [u8; 32],
    /// The digest of the erased form, the object a unit stores it as.
    form: [u8; 32],
    /// What its erasure asked, as a run of one trace ([`TraceTable::run`]).
    trace: Vec<u8>,
    /// What each question read, in the trace's order ([`Read`]): nothing of a part of the item's own or of a fixed unit's.
    reads: Vec<Read>,
}

/// An erasure candidate read back: the digest of its erased form, its trace and what each question read.
pub(super) struct ErasedCandidate {
    pub(super) form: Digest,
    pub(super) trace: Trace<Question, Answered>,
    pub(super) reads: Vec<Read>,
}

impl ErasedCandidate {
    /// The candidate `bytes` hold of the erasure of the item named `input`, first naming `name`: `None` for bytes that hold no such candidate.
    pub(super) fn read(bytes: &[u8], input: &Digest, name: &Global) -> Option<Self> {
        let candidate = curios_archive::from_bytes::<ErasureCandidate>(bytes).ok()?;
        if candidate.input != *input.as_bytes() {
            return None;
        }
        let [(traced, trace)] =
            <[_; 1]>::try_from(TraceTable::read(&candidate.trace).ok()?).ok()?;
        if traced != *name || trace.len() != candidate.reads.len() {
            return None;
        }

        Some(Self {
            form: Digest::from_bytes(candidate.form),
            trace,
            reads: candidate.reads,
        })
    }

    /// Whether every question of this candidate, of the item declaring `names`, is a part asked and every part and binding it read digests now as it did: `unit` holding the erased forms of the items before it, which `over` reads the rest by.
    pub(super) fn stands(
        &self,
        over: &ErasedOver<'_>,
        names: &[&Global],
        unit: &ErasedUnit,
    ) -> bool {
        self.trace
            .asked()
            .iter()
            .zip(&self.reads)
            .all(|((question, answered), read)| {
                *answered == Answered::Part && over.read_now(names, question, unit) == Some(*read)
            })
    }
}

impl ErasedUnit {
    /// The candidate of the erasure of the item `module` holds at `place`, where `places` holds each name, as a store keeps it: `identity` being the item's, and `form` the digest of its erased form. `None` where none is made of it: an item this unit holds no trace of, whose erasure asked something of the unit as a whole, or read a part or a binding that will not digest, or one of another unit that the unit records nothing of.
    pub fn erasure_candidate(
        &self,
        module: &Module,
        places: &Places,
        place: usize,
        identity: Digest,
        form: Digest,
    ) -> Result<Option<Object>, String> {
        let item = &module.items[place];
        let names = item.declared_names();
        let Some(trace) = names.first().and_then(|first| self.asked(first)) else {
            return Ok(None);
        };

        let mut reads = Vec::with_capacity(trace.len());
        for (question, answered) in trace.asked() {
            if *answered != Answered::Part {
                return Ok(None);
            }
            let Some(read) = self.read_then(module, places, &names, question) else {
                return Ok(None);
            };
            reads.push(read);
        }
        let made = ErasureCandidate {
            input: *identity.as_bytes(),
            form: *form.as_bytes(),
            trace: TraceTable::run([(names[0], &***trace)])?,
            reads,
        };

        let bytes = curios_archive::to_bytes(&made)?.to_vec();
        Ok(Some(candidate(Facet::Erasure, &identity, bytes)))
    }

    /// What the item declaring `names` read of what `question` asks, as a candidate records it ([`Read`]), off this unit, its module `module` held where `places` says, and what its erasure recorded of other units: `None` where it will not digest.
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
