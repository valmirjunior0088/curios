//! The questions a judgment's reduction puts to conversion.
//!
//! Reduction is plain or it asks. At a stuck fold or a missed equation it may put a question to the elaborator's conversion, whose answer rests on the solutions and the level constraints that stand; a question reduction puts to conversion is itself answered by plain reduction, which is what ends the regress, and a shared analysis reads a term plainly too, since totality may rest on no verdict of conversion's. [`Asking`] is the state of that protocol: whether reduction is plain now, the two atoms a classing is asking about, whether conversion is answering a question with nothing committed, and the counts a remembered reduct is held against.

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
    /// Whether a level equation has been withheld inside the question conversion is answering with nothing committed; `None` where it is answering none.
    withheld: Option<bool>,
    /// How many pairs of heads conversion has met inside their own unfolding and left apart: a verdict reached across one rests on what was being unfolded there.
    apart: u64,
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

    /// A pair of heads was met inside its own unfolding and left apart.
    pub(crate) fn note_apart(&mut self) {
        self.apart += 1;
    }

    /// How many pairs have been left apart inside their own unfolding.
    pub(crate) fn apart(&self) -> u64 {
        self.apart
    }

    /// Enter or leave a question asked with nothing committed, answering what stood before: one is entered with `Some(false)`, and what leaving it reads back says whether a level equation was withheld inside it.
    pub(crate) fn withhold(&mut self, withheld: Option<bool>) -> Option<bool> {
        std::mem::replace(&mut self.withheld, withheld)
    }

    /// Whether conversion is answering a question asked with nothing committed, where it joins no two levels.
    pub(crate) fn withholding(&self) -> bool {
        self.withheld.is_some()
    }

    /// A level equation was withheld inside the question conversion is answering.
    pub(crate) fn note_withheld(&mut self) {
        if let Some(withheld) = &mut self.withheld {
            *withheld = true;
        }
    }

    /// The two atoms a classing is asking conversion about, which a classing made meanwhile is held inside (`curios_analysis::Classes::of`).
    pub(crate) fn asked(&self) -> Option<&(Term, Term)> {
        self.asked.as_ref()
    }

    /// Enter or leave a classing's question about two atoms, answering what stood before.
    pub(crate) fn ask(&mut self, asked: Option<(Term, Term)>) -> Option<(Term, Term)> {
        std::mem::replace(&mut self.asked, asked)
    }
}
