//! A probe: one program put to the checkers as a proof of `False`, off the board, and what they said of it.

#[cfg(test)]
mod tests;

use {
    crate::{Answer, Checker, false_name, put_program, refusers},
    std::fmt,
};

/// What the checkers said of a program put to them at `False` and filed nowhere: the question a hunt asks before there is a ticket to write. The program is put exactly as the board puts a program witness, so what a probe shows admitted is what a ticket filed from it shows admitted.
#[derive(Debug)]
pub struct Probe {
    answer: Answer,
}

impl Probe {
    /// Put the program `source` to both checkers, its final term as a proof of `False`.
    pub fn put(source: &str) -> Probe {
        Probe {
            answer: put_program(source),
        }
    }

    /// Whether both checkers admitted the term at `False`: a flaw, and a ticket to file.
    pub fn admitted(&self) -> bool {
        self.answer.admitted()
    }

    /// What `checker` made of the program. The kernel is not asked where the elaborator built it no module to judge.
    fn said(&self, checker: Checker) -> &'static str {
        let refused = self
            .answer
            .refusals()
            .iter()
            .any(|refusal| refusal.by() == checker);

        match (refused, checker) {
            (true, _) => "refused",
            (false, Checker::Kernel) if !self.answer.kernel_asked() => "not asked",
            (false, _) => "admitted",
        }
    }
}

/// The verdict in the report's words, what each checker made of the program, and every refusal as a compilation prints it.
impl fmt::Display for Probe {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.admitted() {
            true => writeln!(formatter, "ADMITTED   at `{}`", false_name().faithful())?,
            false => writeln!(formatter, "REFUSED    by {}", refusers(&self.answer))?,
        }

        for checker in [Checker::Elaborator, Checker::Kernel] {
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
