//! What a task asked of what it read, in the order it asked: the record its result is reused by.
//!
//! What one task made of one input — a declaration's lowered form of its text, its elaboration of that form — is the same result again where its input is the same and everything it asked is answered as it was. A [`Trace`] holds the second half: each question with the answer it was given. What a question is belongs to the stage that asks it, so the type is generic over both and knows one rule.

/// The questions one task put to what it read, each with its answer, in the order they were asked.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Trace<Q, A> {
    asked: Vec<(Q, A)>,
}

impl<Q, A> Default for Trace<Q, A> {
    fn default() -> Self {
        Self { asked: Vec::new() }
    }
}

impl<Q, A> Trace<Q, A> {
    /// Record that `question` was answered `answer`, after everything recorded so far.
    pub fn record(&mut self, question: Q, answer: A) {
        self.asked.push((question, answer));
    }

    /// Whether every question is `answered` as it was, asked again in the order it was made: the first that is not ends the asking, since what was asked after it was asked of an answer that no longer stands.
    pub fn stands(&self, mut answered: impl FnMut(&Q, &A) -> bool) -> bool {
        self.asked
            .iter()
            .all(|(question, answer)| answered(question, answer))
    }
}
