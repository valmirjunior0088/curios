//! What is written on the board: a part of the judgment, the tickets filed under it, and the witnesses each carries, as the loader reads them off their records.

use {
    crate::{Checker, Date, Expected},
    serde::Deserialize,
    std::fmt,
};

/// One part of the judgment: a question, stated as its description, with a ticket for every flaw that answered it wrongly. A part claims nothing beyond its question, and it is split once it holds enough tickets about one rule to name that rule by what they show.
#[derive(Debug)]
pub(crate) struct Part {
    /// Its directory's name.
    pub(crate) name: String,
    pub(crate) description: String,
    pub(crate) tickets: Vec<Ticket>,
}

/// Where a ticket stands, by the word its record states it in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum Status {
    /// The flaw is in the tree: some witness is admitted at `False`.
    Open,
    /// The flaw is closed: every witness is refused as it expects. The commit that closed it is the one that wrote this status, which git answers for.
    Fixed,
}

/// One flaw: a closed term the checkers admitted at `/std/Bool/False`. Nothing else is a ticket — not a misjudgment no proof of `False` was built from, not a refusal of something sound, not a program compiled to do what its source does not say.
///
/// A whole one, as its directory holds it and put through a run, is `EXAMPLE` in this crate's `report/tests.rs`.
#[derive(Debug)]
pub(crate) struct Ticket {
    pub(crate) title: String,
    /// The day it was found.
    pub(crate) found_on: Date,
    pub(crate) status: Status,
    /// The proofs of `False` the flaw admitted: each is refused while the ticket is fixed.
    pub(crate) witnesses: Vec<Witness>,
}

/// One term put to the checkers at `False` on every run. Admitted, it is the flaw; refused as it expects, it is what holds the flaw shut.
#[derive(Debug)]
pub(crate) struct Witness {
    /// What is being put, in a few words.
    pub(crate) what: String,
    pub(crate) proof: Proof,
    /// What refuses it once its flaw is closed, by every checker listed and by the error each names, and, for an error every conversion raises, where it stands: the rule is named, or a witness broken in some other way would pass.
    pub(crate) expect: Vec<Expected>,
}

/// The proof of `False` a witness alleges, by its text and the form it is written in, which says how it is put. Either way it supplies a term and the type it is put at, `/std/Bool/False`, is fixed beneath it: nothing a witness says of itself is read.
#[derive(Debug, Clone)]
pub(crate) enum Proof {
    /// A source program whose tail is the term, put to the compiler as a compilation puts it: the elaborator, then the kernel over the module it built.
    Program(String),
    /// A module built by hand, written as Core text closing with the term and stating no type for it: read over the prelude, and put to the kernel alone with the prelude in scope, through the walk a compilation puts a program to.
    Module(String),
}

impl Proof {
    pub(crate) fn kind(&self) -> &'static str {
        match self {
            Proof::Program(_) => "program",
            Proof::Module(_) => "module",
        }
    }

    /// What the proof is put to, in the order it meets them: the step that builds its module, then the kernel.
    pub(crate) fn asked(&self) -> [Checker; 2] {
        match self {
            Proof::Program(_) => [Checker::Elaborator, Checker::Kernel],
            Proof::Module(_) => [Checker::Reader, Checker::Kernel],
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
