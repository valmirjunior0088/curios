//! A probe: one program, or one module written as Core text, put to the checkers as a proof of `False`, off the board, and what they said of it.

#[cfg(test)]
mod tests;

use {
    crate::{Answer, Checker, Proof, false_name, refusers},
    std::fmt,
};

/// What the checkers said of a proof put to them at `False` and filed nowhere: the question a hunt asks before there is a ticket to write. The proof is put exactly as the board puts a witness of its form, so what a probe shows admitted is what a ticket filed from it shows admitted.
#[derive(Debug)]
pub struct Probe {
    answer: Answer,
    /// What the proof's form put it to.
    asked: [Checker; 2],
}

impl Probe {
    /// Put `proof` to what its form puts it to, its term as a proof of `False`.
    pub fn put(proof: &Proof) -> Probe {
        Probe {
            answer: proof.answer(),
            asked: proof.asked(),
        }
    }

    /// Whether both checkers admitted the term at `False`: a flaw, and a ticket to file.
    pub fn admitted(&self) -> bool {
        self.answer.admitted()
    }

    /// What `checker` made of the proof. The certifier is not asked where nothing built it a module to judge.
    fn said(&self, checker: Checker) -> &'static str {
        let refused = self
            .answer
            .refusals()
            .iter()
            .any(|refusal| refusal.by() == Some(checker));

        match (refused, checker) {
            (true, _) => "refused",
            (false, Checker::Certifier) if !self.answer.certifier_asked() => "not asked",
            (false, _) => "admitted",
        }
    }
}

/// The verdict in the report's words, what each step the proof was put to made of it, and every refusal as its step reports it.
impl fmt::Display for Probe {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.admitted() {
            true => writeln!(formatter, "ADMITTED   at `{}`", false_name().faithful())?,
            false => writeln!(formatter, "REFUSED    by {}", refusers(&self.answer))?,
        }

        for checker in self.asked {
            writeln!(
                formatter,
                "           {:<10}  {}",
                checker.to_string(),
                self.said(checker)
            )?;
        }

        for refusal in self.answer.refusals() {
            writeln!(formatter, "\n{}", refusal.to_string().trim_end())?;
        }

        Ok(())
    }
}
