//! What a judgment asks of the declarations around it, noted as it asks.
//!
//! A declaration is a function of what it reads, and it reads another declaration through one of a context's doors alone: a name's type with its scheme, what the name unfolds to, its kind, whether it is total, the vector it filed, a registry entry, the witness standing at a key, and whether the unit's lemmas are on offer. Each door notes its question here, answered or not, so the questions an attempt put are in hand where the attempt stands: a later compile of the same unit keeps what the declaration elaborated to where every one of them is answered as it was.
//!
//! **A question holds its answer only where the answer is no declaration's part.** What a name's signature or body is, the declaration holds, in the unit the trace is kept with, so asking again is comparing that part and nothing is copied here. Which witness stands at a key, whether one is declared there and whether lemmas are on offer are answered by the unit as a whole, and each is noted with what it answered.
//!
//! **Asked once, in the order first asked.** A judgment asks after one name thousands of times, and the trace keeps the first.
//!
//! **Erasure asks through the same doors.** An item is erased by a reader of finished terms (`Retyping`), which is a context, so what erasing it asks of the declarations around it is noted here as an elaboration's is; and it asks one thing more, of another item's erased form, which is a question of its own.
//!
//! **Of the declarations around it, so not of itself.** What a declaration asks of a name it declares — a recursive call's signature, the former a constructor is checked against, the witness it declares found at its own key — is answered by the attempt under way, and stands wherever the declaration does.

use {
    curios_core::{Global, Trace, WitnessKey},
    std::{
        cell::RefCell,
        collections::HashSet,
        hash::{Hash, Hasher},
        mem,
    },
};

/// One question a judgment put to its environment about another declaration, naming a declaration by `N` and a witness key by `K`: the name and the key themselves where a judgment asks, and each one's place in a unit's table where a unit is stored ([`Traces`](crate::Traces)).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Question<N = Global, K = WitnessKey> {
    /// The type a name is declared at, with its universe scheme.
    Signature(N),
    /// What a name unfolds to.
    Body(N),
    /// The kind of definition a name is.
    Kind(N),
    /// Whether a definition is total.
    Totality(N),
    /// The variance vector a definition filed.
    Variances(N),
    /// An inductive family's registry entry.
    Induct(N),
    /// A record's registry entry.
    Struct(N),
    /// A concept's registry entry.
    Concept(N),
    /// The witness standing at a concept and a key.
    Witness(N, K),
    /// Whether a witness is declared at a concept and a key.
    WitnessDeclared(N, K),
    /// Whether the unit's lemmas are on offer to a proof the elaborator writes.
    Lemmas,
    /// The erased form of the item declaring a name, as an item that names it reads it: how the name is bound where it is erased, and the row it declares. Erasure's alone.
    Erased(N),
    /// The erased form of the item declaring a name, whole: what an item evaluated where it is erased read of each item it was linked after, since evaluating it runs what they define. Erasure's alone.
    Ran(N),
}

impl<N, K> Question<N, K> {
    /// The name the question is about, where it is about one.
    pub fn subject(&self) -> Option<&N> {
        match self {
            Question::Signature(name)
            | Question::Body(name)
            | Question::Kind(name)
            | Question::Totality(name)
            | Question::Variances(name)
            | Question::Induct(name)
            | Question::Struct(name)
            | Question::Concept(name)
            | Question::Witness(name, _)
            | Question::WitnessDeclared(name, _)
            | Question::Erased(name)
            | Question::Ran(name) => Some(name),
            Question::Lemmas => None,
        }
    }

    /// The question with its name spelled by `name` and its key by `key`, or the first refusal of either: how a trace is written into a unit's table and read back out of it.
    #[cfg(feature = "archive")]
    pub(crate) fn spelled<M, L, E>(
        self,
        mut name: impl FnMut(N) -> Result<M, E>,
        mut key: impl FnMut(K) -> Result<L, E>,
    ) -> Result<Question<M, L>, E> {
        Ok(match self {
            Question::Signature(subject) => Question::Signature(name(subject)?),
            Question::Body(subject) => Question::Body(name(subject)?),
            Question::Kind(subject) => Question::Kind(name(subject)?),
            Question::Totality(subject) => Question::Totality(name(subject)?),
            Question::Variances(subject) => Question::Variances(name(subject)?),
            Question::Induct(subject) => Question::Induct(name(subject)?),
            Question::Struct(subject) => Question::Struct(name(subject)?),
            Question::Concept(subject) => Question::Concept(name(subject)?),
            Question::Witness(subject, at) => Question::Witness(name(subject)?, key(at)?),
            Question::WitnessDeclared(subject, at) => {
                Question::WitnessDeclared(name(subject)?, key(at)?)
            }
            Question::Lemmas => Question::Lemmas,
            Question::Erased(subject) => Question::Erased(name(subject)?),
            Question::Ran(subject) => Question::Ran(name(subject)?),
        })
    }
}

/// What a [`Question`] was answered.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Answered<N = Global> {
    /// The part asked of the declaration named, there or not: the declaration's to hold, and read again off the unit the trace is kept with.
    Part,
    /// The witness found at the key, by its name, or none.
    Witness(Option<N>),
    /// Yes or no.
    Whether(bool),
}

impl<N> Answered<N> {
    /// The answer with its name spelled by `name`, or its refusal.
    #[cfg(feature = "archive")]
    pub(crate) fn spelled<M, E>(
        self,
        name: impl FnOnce(N) -> Result<M, E>,
    ) -> Result<Answered<M>, E> {
        Ok(match self {
            Answered::Part => Answered::Part,
            Answered::Witness(found) => Answered::Witness(found.map(name).transpose()?),
            Answered::Whether(whether) => Answered::Whether(whether),
        })
    }
}

/// A question as the repeat check holds it, hashed by where its subject's one allocation lies. A name's own hash is over its text, so that every process orders alike, which a set that is never iterated has no use for and a door asked at every occurrence of a name cannot pay.
#[derive(Debug, PartialEq, Eq)]
struct Once(Question);

impl Hash for Once {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(&self.0).hash(state);
        let module = match self.0.subject() {
            Some(Global::Authored(path)) => Some(path),
            Some(Global::Witness(id)) => Some(id.module()),
            None => None,
        };
        module.map(|path| path.segments().as_ptr()).hash(state);
    }
}

/// The trace of the judgment under way: its questions in the order each was first asked, with what each was answered.
#[derive(Debug, Default)]
pub(crate) struct Traced {
    /// The names the judgment's own declaration declares, whose parts it asks of itself.
    own: Vec<Global>,
    seen: RefCell<HashSet<Once>>,
    trace: RefCell<Trace<Question, Answered>>,
}

impl Traced {
    /// Begin the trace of a judgment of the declaration declaring `own`: the next question is its first.
    pub(crate) fn begin(&mut self, own: &[&Global]) {
        self.own.clear();
        self.own.extend(own.iter().copied());
        self.take();
    }

    /// Note that `question` was asked and answered `answered`, where it has not been asked since the trace was begun and is not the declaration's of itself: of a part of a name it declares, or answered by a witness it declares.
    pub(crate) fn note(&self, question: Question, answered: Answered) {
        if !self.seen.borrow_mut().insert(Once(question)) {
            return;
        }
        let own = match answered {
            Answered::Part => question
                .subject()
                .is_some_and(|subject| self.own.contains(subject)),
            Answered::Witness(Some(found)) => self.own.contains(&found),
            Answered::Witness(None) | Answered::Whether(_) => false,
        };
        if !own {
            self.trace.borrow_mut().record(question, answered);
        }
    }

    /// Whether `question` has been asked since the trace was begun.
    pub(crate) fn noted(&self, question: Question) -> bool {
        self.seen.borrow().contains(&Once(question))
    }

    /// The trace so far, taken: the next question is the first of another.
    pub(crate) fn take(&mut self) -> Trace<Question, Answered> {
        self.seen.get_mut().clear();
        mem::take(self.trace.get_mut())
    }
}
