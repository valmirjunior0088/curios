//! What is written on the board: a part of the judgment, the tickets filed under it, and the witnesses each carries.

use {
    crate::{Date, Expected},
    curios_core::{Module, Term},
    std::fmt,
};

/// One part of the judgment: a question, stated as the documentation of its constant, with a ticket for every flaw that answered it wrongly. A part claims nothing beyond its question, and it is split once it holds enough tickets about one rule to name that rule by what they show.
#[derive(Debug)]
pub struct Part {
    pub name: &'static str,
    pub tickets: &'static [Ticket],
}

/// Where a ticket stands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    /// The flaw is in the tree: some witness is admitted at `False`.
    Open,
    /// The flaw is closed: every witness is refused as it expects. The commit that closed it is the one that wrote this status, which git answers for.
    Fixed,
}

/// One flaw: a closed term the checkers admitted at `/std/Bool/False`. Nothing else is a ticket — not a misjudgment no proof of `False` was built from, not a refusal of something sound, not a program compiled to do what its source does not say.
///
/// A whole one, written as a board file writes it and put through a run, is `EXAMPLE` in this crate's `report/tests.rs`.
#[derive(Debug)]
pub struct Ticket {
    pub title: &'static str,
    /// The day it was found.
    pub found_on: Date,
    pub status: Status,
    /// The proofs of `False` the flaw admitted: each is refused while the ticket is fixed.
    pub witnesses: &'static [Witness],
}

/// One term put to the checkers at `False` on every run. Admitted, it is the flaw; refused as it expects, it is what holds the flaw shut.
#[derive(Debug, Clone, Copy)]
pub struct Witness {
    /// What is being put, in a few words.
    pub what: &'static str,
    pub proof: Proof,
    /// What refuses it once its flaw is closed, by every checker listed and by the error each names: the rule is named, or a witness broken in some other way would pass. Written by [`refused!`](crate::refused).
    pub expect: &'static [Expected],
}

/// The proof of `False` a witness alleges, in the form it is put. Either way it supplies a term and the type it is put at, `/std/Bool/False`, is fixed beneath it: nothing a witness says of itself is read.
#[derive(Debug, Clone, Copy)]
pub enum Proof {
    /// A source program whose tail is the term, put to the compiler as a compilation puts it: the elaborator, then the kernel over the module it built.
    Program(&'static str),
    /// A module built by hand and the term it closes with, put to the kernel alone with the prelude in scope, through the walk a compilation puts a program to.
    Module(fn() -> (Module, Term)),
}

impl Proof {
    fn kind(&self) -> &'static str {
        match self {
            Proof::Program(_) => "program",
            Proof::Module(_) => "module",
        }
    }
}

/// How a witness reads on the board: the form of its proof, what it puts, and what refuses it.
impl fmt::Display for Witness {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{:<7}  {} — refused by ",
            self.proof.kind(),
            self.what
        )?;

        for (index, expected) in self.expect.iter().enumerate() {
            if index > 0 {
                formatter.write_str(" and ")?;
            }
            write!(formatter, "{expected}")?;
        }

        Ok(())
    }
}
