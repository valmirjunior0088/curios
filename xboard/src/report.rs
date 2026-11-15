//! A report: every ticket on a board put to the checkers, where each one stands for it, and what was found in each part.

#[cfg(test)]
mod tests;

use {
    crate::{Answer, Filed, Part, Refusal, Status, Ticket, Witness},
    std::fmt,
};

/// Where a ticket stands once its witnesses have been put.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Standing {
    /// Fixed, and every witness is refused as it expects.
    Holds,
    /// Fixed, and a witness is not refused as it expects: the flaw is back, or what held it shut is gone.
    Regressed(Vec<String>),
    /// Open, and every witness shows the flaw or what closes it: each is admitted or refused as it expects, and one is admitted.
    Open,
    /// Open, and every witness is refused as it expects: something closed the flaw and the ticket does not say what.
    Shut,
    /// Open, and a witness is refused by an error it does not expect: it no longer shows the flaw, or something closed the flaw another way, and the ticket says neither.
    Unshown(Vec<String>),
    /// Archived, and every witness fails before any checker reads it.
    Archived,
    /// Archived, and a witness is read: what the archive says is gone from the language is back, and the ticket belongs on the board.
    Revived(Vec<String>),
}

impl Standing {
    /// Whether a run fails on this standing.
    pub fn fails(&self) -> bool {
        matches!(
            self,
            Standing::Regressed(_) | Standing::Shut | Standing::Unshown(_) | Standing::Revived(_)
        )
    }

    fn label(&self) -> &'static str {
        match self {
            Standing::Holds => "HOLDS",
            Standing::Regressed(_) => "REGRESSED",
            Standing::Open => "OPEN",
            Standing::Shut => "SHUT",
            Standing::Unshown(_) => "UNSHOWN",
            Standing::Archived => "ARCHIVED",
            Standing::Revived(_) => "REVIVED",
        }
    }
}

/// Where a ticket filed as `filed` stands for what its witnesses answered.
pub fn standing(filed: &Filed, witnesses: &[Witness], answers: &[Answer]) -> Standing {
    let status = match filed {
        Filed::Live(status) => *status,
        Filed::Archived(_) => return archived(witnesses, answers),
    };
    // Each witness not refused as it expects, with whether it was admitted.
    let unmet = witnesses
        .iter()
        .zip(answers)
        .filter_map(|(witness, answer)| {
            witness
                .unmet(answer)
                .map(|problem| (answer.admitted(), format!("{}: {problem}", witness.what)))
        })
        .collect::<Vec<_>>();
    let refused_otherwise = unmet
        .iter()
        .filter(|(admitted, _)| !admitted)
        .map(|(_, problem)| problem.clone())
        .collect::<Vec<_>>();

    match (status, unmet.is_empty(), refused_otherwise.is_empty()) {
        (Status::Open, true, _) => Standing::Shut,
        (Status::Open, false, true) => Standing::Open,
        (Status::Open, false, false) => Standing::Unshown(refused_otherwise),
        (Status::Fixed, true, _) => Standing::Holds,
        (Status::Fixed, false, _) => {
            Standing::Regressed(unmet.into_iter().map(|(_, problem)| problem).collect())
        }
    }
}

/// Where an archived ticket stands for what its witnesses answered: each must fail before any checker reads it.
fn archived(witnesses: &[Witness], answers: &[Answer]) -> Standing {
    let read = witnesses
        .iter()
        .zip(answers)
        .filter(|(_, answer)| !answer.unread())
        .map(|(witness, answer)| format!("{}: read, and {}", witness.what, refusers(answer)))
        .collect::<Vec<_>>();

    match read.is_empty() {
        true => Standing::Archived,
        false => Standing::Revived(read),
    }
}

/// One line of a report: a ticket, with what its witnesses answered and where it stands.
#[derive(Debug)]
pub struct Row<'b> {
    /// The name of the part it is filed under.
    part: &'b str,
    ticket: &'b Ticket,
    answers: Vec<Answer>,
    standing: Standing,
}

impl<'b> Row<'b> {
    /// Put every witness of `ticket`, filed under the part named `part`, to the checkers.
    pub fn put(part: &'b str, ticket: &'b Ticket) -> Row<'b> {
        let answers = ticket
            .witnesses
            .iter()
            .map(Witness::answer)
            .collect::<Vec<_>>();
        let standing = standing(&ticket.filed, &ticket.witnesses, &answers);

        Row {
            part,
            ticket,
            answers,
            standing,
        }
    }

    /// Whether the certifier judged any witness of the ticket. A program the elaborator refuses while it is still building the module never reaches it, so a ticket holding only such programs says nothing of the trusted base.
    pub fn reaches_certifier(&self) -> bool {
        self.answers.iter().any(Answer::certifier_asked)
    }

    /// Whether any witness of the ticket was admitted at `False`.
    pub fn admitted(&self) -> bool {
        self.answers.iter().any(Answer::admitted)
    }
}

/// Where the ticket stands, the part it is filed under, each witness with who refused it, and what keeps a failing ticket from standing.
impl fmt::Display for Row<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            formatter,
            "  {:<9}  {}  {}",
            self.standing.label(),
            self.ticket.found_on,
            self.ticket.title
        )?;
        writeln!(formatter, "             part     {}", self.part)?;
        if let Filed::Archived(reason) = &self.ticket.filed {
            writeln!(formatter, "             reason   {reason}")?;
        }
        for (witness, answer) in self.ticket.witnesses.iter().zip(&self.answers) {
            writeln!(formatter, "             {witness} [{}]", refusers(answer))?;
        }
        if let Standing::Regressed(problems)
        | Standing::Unshown(problems)
        | Standing::Revived(problems) = &self.standing
        {
            for problem in problems {
                writeln!(formatter, "             !        {problem}")?;
            }
        }

        Ok(())
    }
}

/// Every ticket of a board with where it stands.
#[derive(Debug)]
pub struct Report<'b> {
    board: &'b [Part],
    rows: Vec<Row<'b>>,
}

impl<'b> Report<'b> {
    /// Put every witness of every ticket on `board` to the checkers.
    pub fn run(board: &'b [Part]) -> Report<'b> {
        let rows = board
            .iter()
            .flat_map(|part| {
                part.tickets
                    .iter()
                    .map(|ticket| Row::put(&part.name, ticket))
            })
            .collect();

        Report { board, rows }
    }

    /// Whether any ticket regressed, was closed without saying so, stands open on a witness refused by an error it does not expect, or is archived with a witness read.
    pub fn fails(&self) -> bool {
        self.rows.iter().any(|row| row.standing.fails())
    }

    fn count(&self, wanted: fn(&Standing) -> bool) -> usize {
        self.rows.iter().filter(|row| wanted(&row.standing)).count()
    }
}

/// Who refused a witness and by which error, as a report notes it beside the witness.
pub fn refusers(answer: &Answer) -> String {
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

impl fmt::Display for Report<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            formatter,
            "xboard: {} ticket(s) — {} holding, {} open, {} regressed, {} shut without saying so, {} open and refused otherwise; {} archived, {} of them read",
            self.rows
                .iter()
                .filter(|row| matches!(row.ticket.filed, Filed::Live(_)))
                .count(),
            self.count(|standing| *standing == Standing::Holds),
            self.count(|standing| *standing == Standing::Open),
            self.count(|standing| matches!(standing, Standing::Regressed(_))),
            self.count(|standing| *standing == Standing::Shut),
            self.count(|standing| matches!(standing, Standing::Unshown(_))),
            self.rows
                .iter()
                .filter(|row| matches!(row.ticket.filed, Filed::Archived(_)))
                .count(),
            self.count(|standing| matches!(standing, Standing::Revived(_))),
        )?;

        let mut rows = self.rows.iter().collect::<Vec<_>>();
        rows.sort_by_key(|row| (!row.standing.fails(), row.ticket.found_on));

        for row in rows {
            write!(formatter, "\n{row}")?;
        }

        writeln!(formatter, "\nparts: {}", self.board.len())?;

        for part in self.board.iter().filter(|part| !part.tickets.is_empty()) {
            let live = part
                .tickets
                .iter()
                .filter(|ticket| matches!(ticket.filed, Filed::Live(_)))
                .count();
            write!(formatter, "  {} — {live} ticket(s)", part.name)?;
            if live < part.tickets.len() {
                write!(formatter, " and {} archived", part.tickets.len() - live)?;
            }

            match part.tickets.iter().map(|ticket| ticket.found_on).max() {
                Some(last) => writeln!(formatter, ", the last found {last}")?,
                None => writeln!(formatter)?,
            }
            writeln!(formatter, "    {}", part.description)?;
        }

        // An archived ticket's witnesses are read by no checker, the certifier least of all.
        let unreached = self
            .rows
            .iter()
            .filter(|row| matches!(row.ticket.filed, Filed::Live(_)) && !row.reaches_certifier())
            .collect::<Vec<_>>();
        if !unreached.is_empty() {
            writeln!(formatter, "\nnot put to the certifier: {}", unreached.len())?;

            for row in unreached {
                writeln!(formatter, "  {}", row.ticket.title)?;
            }
        }

        Ok(())
    }
}
