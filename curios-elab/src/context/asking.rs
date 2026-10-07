//! The questions a judgment's reduction puts to conversion.
//!
//! Reduction is plain or it asks. At a stuck fold or a missed equation it may put a question to the elaborator's conversion, whose answer rests on the solutions and the level constraints that stand; a question reduction puts to conversion is itself answered by plain reduction, which is what ends the regress, and a shared analysis reads a term plainly too, since totality may rest on no verdict of conversion's. [`Asking`] is the state of that protocol: whether reduction is plain now, the two atoms a classing is asking about, and the counts a remembered reduct is held against.

use curios_core::Term;

/// Where a context stands in the protocol by which reduction asks conversion.
#[derive(Debug, Default)]
pub(crate) struct Asking {
    /// Whether reduction is plain: it asks the elaborator's conversion nothing.
    plain: bool,
    /// The two atoms a classing is asking conversion about; `None` where no classing is asking.
    asked: Option<(Term, Term)>,
    /// How many questions a judgment's reduction has put to conversion.
    questions: u64,
    /// How many of every question asked with nothing committed were declined for a commit — what a remembered reduct is held against.
    declined: u64,
}

impl Asking {
    /// Whether reduction is plain.
    pub(crate) fn plain(&self) -> bool {
        self.plain
    }

    /// Make reduction plain or asking, answering what it was: the two halves of the bracket that reads a term plainly.
    pub(crate) fn set_plain(&mut self, plain: bool) -> bool {
        std::mem::replace(&mut self.plain, plain)
    }

    /// A judgment's reduction is putting a question to conversion. Its answer rests on the level constraints that stand, so a scope that is rolled back with constraints withdrawn after one was asked inside it clears what was reduced meanwhile.
    pub(crate) fn note_question(&mut self) {
        self.questions += 1;
    }

    /// How many questions reduction has put to conversion.
    pub(crate) fn questions(&self) -> u64 {
        self.questions
    }

    /// A question asked with nothing committed was answered no because answering yes would have committed a solution or a level constraint. The answer can change once the solver moves, so no reduct taken across it is remembered.
    pub(crate) fn note_declined(&mut self) {
        self.declined += 1;
    }

    /// How many questions have been declined for a commit.
    pub(crate) fn declined(&self) -> u64 {
        self.declined
    }

    /// The two atoms a classing is asking conversion about, which a classing made meanwhile is held inside (`curios_core::Classes::of`).
    pub(crate) fn asked(&self) -> Option<&(Term, Term)> {
        self.asked.as_ref()
    }

    /// Enter or leave a classing's question about two atoms, answering what stood before.
    pub(crate) fn ask(&mut self, asked: Option<(Term, Term)>) -> Option<(Term, Term)> {
        std::mem::replace(&mut self.asked, asked)
    }
}
