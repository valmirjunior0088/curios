//! What is written on the board: a part of the judgment, the tickets filed under it, and the witnesses each carries, as the loader reads them off their records.

use {
    crate::{Checker, Date, Expected},
    curios_surface::WrittenIn,
    serde::Deserialize,
    std::fmt,
};

/// One part of the judgment: a question, stated as its description, with a ticket for every flaw that answered it wrongly. A part claims nothing beyond its question, and it is split once it holds enough tickets about one rule to name that rule by what they show.
#[derive(Debug)]
pub struct Part {
    /// Its directory's name.
    pub name: String,
    pub description: String,
    pub tickets: Vec<Ticket>,
    /// The tickets the archive files under it.
    pub archived: Vec<Archived>,
}

/// Where a ticket stands, by the word its record states it in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Status {
    /// The flaw is in the tree: some witness is admitted at `False`.
    Open,
    /// The flaw is closed: every witness is refused as it expects. The commit that closed it is the one that wrote this status, which git answers for.
    Fixed,
}

/// One flaw: a closed term the checkers admitted at `/std/Bool/False`. Nothing else is a ticket — not a misjudgment no proof of `False` was built from, not a refusal of something sound, not a program compiled to do what its source does not say.
///
/// A whole one, as its directory holds it and put through a run, is `EXAMPLE` in this crate's `report/tests.rs`.
#[derive(Debug)]
pub struct Ticket {
    pub title: String,
    /// The day it was found.
    pub found_on: Date,
    pub status: Status,
    /// The proofs of `False` the flaw admitted: each is refused as it expects while the ticket is fixed.
    pub witnesses: Vec<Witness>,
}

/// A ticket in the archive, `xboard/archive/`: its title, the day it was found and the reason its record states, which is all the loader reads of it. Its witnesses stay in its directory as they were filed, and nothing puts them.
#[derive(Debug)]
pub struct Archived {
    pub title: String,
    /// The day it was found.
    pub found_on: Date,
    /// Why it is archived: the construct its witnesses are written in is gone from the language, or a construction no type of the trusted base can hold.
    pub reason: String,
}

/// One term put to the checkers at `False` on every run, at the end of a history: the steps it states walked over a store of their own, then the proof put over the prelude and every unit they compiled. Admitted, it is the flaw; refused as it expects, it is what holds the flaw shut.
#[derive(Debug)]
pub struct Witness {
    /// What is being put, in a few words.
    pub what: String,
    /// The steps walked before the proof is put: none where it is put over the prelude alone.
    pub steps: Vec<Step>,
    pub proof: Proof,
    /// What refuses it once its flaw is closed, by every checker listed and by the error each names, and, for an error every conversion raises, where it stands: the rule is named, or a witness broken in some other way would pass.
    pub expect: Vec<Expected>,
}

/// One step of a witness's history, walked as `curios_verdict::test_support::History` walks it.
#[derive(Debug, Clone)]
pub enum Step {
    /// The unit claiming `mount` stepped to `text`, written as `written`, and every unit built: compiled over its earlier step where it had one, as a recompile is over its baseline.
    Unit {
        mount: String,
        written: WrittenIn,
        text: String,
    },
    /// The unit claiming `mount` mounted by no later build, as a dependency taken out of a manifest is: a unit standing in for an archived root unmounted gives the root its place back.
    Unmount { mount: String },
}

impl Step {
    /// What the step is put to: the checkers of the text it compiles, and nothing for a unit unmounted, whose later builds compile what earlier steps wrote.
    fn asked(&self) -> &'static [Checker] {
        match self {
            Step::Unit {
                written: WrittenIn::Program,
                ..
            } => &[Checker::Elaborator, Checker::Certifier],
            Step::Unit {
                written: WrittenIn::CoreText,
                ..
            } => &[Checker::Certifier],
            Step::Unmount { .. } => &[],
        }
    }
}

impl Witness {
    /// The form it is put as, as a report shows it: a history where steps come before the proof, and the proof's form otherwise.
    pub fn kind(&self) -> &'static str {
        match self.steps.is_empty() {
            true => self.proof.kind(),
            false => "history",
        }
    }

    /// What any step of it is put to, the proof's included, in the order a program meets them.
    pub fn asked(&self) -> Vec<Checker> {
        let asked = self
            .steps
            .iter()
            .flat_map(|step| step.asked().iter().copied())
            .chain(self.proof.asked().iter().copied())
            .collect::<Vec<_>>();

        [Checker::Elaborator, Checker::Certifier]
            .into_iter()
            .filter(|checker| asked.contains(checker))
            .collect()
    }
}

/// The proof of `False` a witness alleges, by its text and the form it is written in, which says how it is put, over the units in scope: the prelude, and every unit its history compiled after it. Either way it supplies a term and the type it is put at, `/std/Bool/False`, is fixed beneath it: nothing a witness says of itself is read.
#[derive(Debug, Clone)]
pub enum Proof {
    /// A source program whose tail is the term, put to the compiler as a compilation puts it: the elaborator, then the certifier over the module it built.
    Program(String),
    /// A module built by hand, written as Core text closing with the term and stating no type for it: read over the units in scope, and put to the certifier alone with them in scope, through the walk a compilation puts a program to.
    Module(String),
}

impl Proof {
    pub fn kind(&self) -> &'static str {
        match self {
            Proof::Program(_) => "program",
            Proof::Module(_) => "module",
        }
    }

    /// What the proof is put to, in the order it meets them: for a program the elaborator that builds its module, then the certifier; for a module the certifier alone.
    pub fn asked(&self) -> &'static [Checker] {
        match self {
            Proof::Program(_) => &[Checker::Elaborator, Checker::Certifier],
            Proof::Module(_) => &[Checker::Certifier],
        }
    }
}

/// How a witness reads on the board: the form of its proof, what it puts, and what refuses it where it expects a refusal.
impl fmt::Display for Witness {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:<7}  {}", self.kind(), self.what)?;
        if self.expect.is_empty() {
            return Ok(());
        }
        formatter.write_str(" — refused by ")?;

        for (index, expected) in self.expect.iter().enumerate() {
            if index > 0 {
                formatter.write_str(" and ")?;
            }
            write!(formatter, "{expected}")?;
        }

        Ok(())
    }
}
