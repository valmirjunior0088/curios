//! A report: every ticket on a board put to the checkers, where each one stands for it, and what was found in each part.

#[cfg(test)]
mod tests;

use {
    crate::{Answer, Part, Refusal, Status, Ticket, Witness},
    std::fmt,
};

/// Where a ticket stands once its witnesses have been put.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Standing {
    /// Fixed, and every witness is refused as it expects.
    Holds,
    /// Fixed, and a witness is not refused as it expects: the flaw is back, or what held it shut is gone.
    Regressed(Vec<String>),
    /// Open, and some witness is not refused as it expects, as an open flaw's is not.
    Open,
    /// Open, and every witness is refused as it expects: something closed the flaw and the ticket does not say what.
    Shut,
}

impl Standing {
    /// Whether a run fails on this standing.
    pub fn fails(&self) -> bool {
        matches!(self, Standing::Regressed(_) | Standing::Shut)
    }

    fn label(&self) -> &'static str {
        match self {
            Standing::Holds => "HOLDS",
            Standing::Regressed(_) => "REGRESSED",
            Standing::Open => "OPEN",
            Standing::Shut => "SHUT",
        }
    }
}

/// Where a ticket stands for what its witnesses answered.
pub(crate) fn standing(status: Status, witnesses: &[Witness], answers: &[Answer]) -> Standing {
    let problems = witnesses
        .iter()
        .zip(answers)
        .filter_map(|(witness, answer)| {
            witness
                .unmet(answer)
                .map(|problem| format!("{}: {problem}", witness.what))
        })
        .collect::<Vec<_>>();

    match (status, problems.is_empty()) {
        (Status::Open, true) => Standing::Shut,
        (Status::Open, false) => Standing::Open,
        (Status::Fixed, true) => Standing::Holds,
        (Status::Fixed, false) => Standing::Regressed(problems),
    }
}

/// One line of a report: a ticket, with what its witnesses answered and where it stands.
#[derive(Debug)]
pub struct Row {
    pub part: &'static Part,
    pub ticket: &'static Ticket,
    pub answers: Vec<Answer>,
    pub standing: Standing,
}

impl Row {
    /// Whether the kernel judged any witness of the ticket. A program the elaborator refuses while it is still building the module never reaches it, so a ticket holding only such programs says nothing of the trusted base.
    pub fn reaches_kernel(&self) -> bool {
        self.answers.iter().any(Answer::kernel_asked)
    }
}

/// Every ticket of a board with where it stands.
#[derive(Debug)]
pub struct Report {
    board: &'static [&'static Part],
    pub rows: Vec<Row>,
}

impl Report {
    /// Put every witness of every ticket on `board` to the checkers.
    pub fn run(board: &'static [&'static Part]) -> Report {
        let mut rows = Vec::new();

        for part in board {
            for ticket in part.tickets {
                let answers = ticket
                    .witnesses
                    .iter()
                    .map(|witness| witness.proof.answer())
                    .collect::<Vec<_>>();

                let standing = standing(ticket.status, ticket.witnesses, &answers);

                rows.push(Row {
                    part,
                    ticket,
                    answers,
                    standing,
                });
            }
        }

        Report { board, rows }
    }

    /// Whether any ticket regressed or was closed without saying so.
    pub fn fails(&self) -> bool {
        self.rows.iter().any(|row| row.standing.fails())
    }

    fn count(&self, wanted: fn(&Standing) -> bool) -> usize {
        self.rows.iter().filter(|row| wanted(&row.standing)).count()
    }
}

/// Who refused a witness and by which error, as a report notes it beside the witness.
fn refusers(answer: &Answer) -> String {
    match answer.admitted() {
        true => "admitted".to_string(),
        false => {
            let mut named = answer
                .refusals()
                .iter()
                .map(Refusal::named)
                .collect::<Vec<_>>();
            named.dedup();

            named.join(", ")
        }
    }
}

impl fmt::Display for Report {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            formatter,
            "xboard: {} ticket(s) — {} holding, {} open, {} regressed, {} shut without saying so",
            self.rows.len(),
            self.count(|standing| *standing == Standing::Holds),
            self.count(|standing| *standing == Standing::Open),
            self.count(|standing| matches!(standing, Standing::Regressed(_))),
            self.count(|standing| *standing == Standing::Shut),
        )?;

        let mut rows = self.rows.iter().collect::<Vec<_>>();
        rows.sort_by_key(|row| (!row.standing.fails(), row.ticket.found_on));

        for row in rows {
            writeln!(
                formatter,
                "\n  {:<9}  {}  {}",
                row.standing.label(),
                row.ticket.found_on,
                row.ticket.title
            )?;
            writeln!(formatter, "             part     {}", row.part.name)?;
            for (witness, answer) in row.ticket.witnesses.iter().zip(&row.answers) {
                writeln!(formatter, "             {witness} [{}]", refusers(answer))?;
            }
            if let Standing::Regressed(problems) = &row.standing {
                for problem in problems {
                    writeln!(formatter, "             !        {problem}")?;
                }
            }
        }

        writeln!(formatter, "\nparts: {}", self.board.len())?;

        for part in self.board.iter().filter(|part| !part.tickets.is_empty()) {
            write!(
                formatter,
                "  {} — {} ticket(s)",
                part.name,
                part.tickets.len()
            )?;

            match part.tickets.iter().map(|ticket| ticket.found_on).max() {
                Some(last) => writeln!(formatter, ", the last found {last}")?,
                None => writeln!(formatter)?,
            }
        }

        let unreached = self
            .rows
            .iter()
            .filter(|row| !row.reaches_kernel())
            .collect::<Vec<_>>();
        if !unreached.is_empty() {
            writeln!(formatter, "\nnot put to the kernel: {}", unreached.len())?;

            for row in unreached {
                writeln!(formatter, "  {}", row.ticket.title)?;
            }
        }

        Ok(())
    }
}
