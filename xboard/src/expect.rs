//! The refusal a witness expects once its flaw is closed, and whether what the checkers said meets it.

use {
    crate::{Answer, Witness, words},
    curios_common::{Qualifier, Variants},
    curios_core::Global,
    curios_pipeline::{Checker, Refusal, standing_at},
    std::fmt,
};

/// One checker's refusal, as a witness expects it: by the checker, by the error it raises where one is named, and where it stands where that is named.
#[derive(Debug, Clone)]
pub struct Expected {
    pub by: Checker,
    /// The error, by the name its checker's crate states for it ([`Checker::errors`]); none where any error of the checker meets it, which is how an open ticket expects a refusal no error names yet.
    pub error: Option<&'static str>,
    pub within: Option<Within>,
}

/// Where an expected refusal stands: the declaration it is raised in, by its absolute path, or the term a program closes with. A mismatch is every conversion's refusal, so one named where the flaw's construction stands is not met by one raised anywhere else.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Within {
    Declaration(String),
    Entry,
}

impl Within {
    /// Whether a refusal standing at `name` stands here.
    fn holds(&self, name: Option<&Global>) -> bool {
        match (self, name) {
            (Within::Entry, None) => true,
            (Within::Declaration(path), Some(Global::Authored(qualifier))) => {
                *qualifier == Qualifier::from(path.trim_start_matches('/').split('/'))
            }
            _ => false,
        }
    }
}

impl Expected {
    /// Whether `refusal` is the one expected.
    fn meets(&self, refusal: &Refusal) -> bool {
        let named = |name: &str| self.error.is_none_or(|error| error == name);
        let stands =
            |name: Option<&Global>| self.within.as_ref().is_none_or(|within| within.holds(name));

        match (self.by, refusal) {
            (Checker::Elaborator, Refusal::Elaborator { error, .. }) => {
                error.each().any(|member| {
                    named(member.raised().variant_name()) && stands(member.declared_in())
                })
            }
            (Checker::Certifier, Refusal::Certifier { refusal, .. }) => {
                named(refusal.error.variant_name()) && stands(refusal.name.as_ref())
            }
            _ => false,
        }
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            None => write!(formatter, "the {}", self.by)?,
            Some(error) => write!(formatter, "the {}'s `{error}`", self.by)?,
        }
        match &self.within {
            None => Ok(()),
            Some(Within::Entry) => write!(formatter, " {}", standing_at(None)),
            Some(Within::Declaration(path)) => write!(formatter, " in `{path}`"),
        }
    }
}

/// What `answer` says nearest to a refusal by `by`, on one line, with what said it: that checker's first refusal, else the first any checker made, else where the witness stopped. A certifier's refusal is quoted in its own words, which its report places beneath a heading.
fn nearest(answer: &Answer, by: Checker) -> String {
    let (named, reported) = match answer {
        Answer::Stopped(stop) => (stop.at.clone(), stop.reported.clone()),
        Answer::Put(put) => {
            let refusals = put.refusals();
            match refusals
                .iter()
                .find(|refusal| refusal.by() == by)
                .or(refusals.first())
            {
                Some(
                    refusal @ Refusal::Certifier {
                        refusal: certified, ..
                    },
                ) => (refusal.named(), words(certified)),
                Some(refusal) => (refusal.named(), refusal.to_string()),
                None => return "admitted".to_string(),
            }
        }
    };

    format!(
        "{named} says {}",
        reported.lines().next().unwrap_or_default()
    )
}

impl Witness {
    /// What keeps `answer` from being the refusal the witness expects, or nothing.
    pub fn unmet(&self, answer: &Answer) -> Option<String> {
        if answer.admitted() {
            return Some("admitted".to_string());
        }
        let refusals = answer.refusals();

        self.expect
            .iter()
            .find(|expected| !refusals.iter().any(|refusal| expected.meets(refusal)))
            .map(|expected| {
                let asked = expected.by != Checker::Certifier || answer.certifier_asked();
                let nearest = nearest(answer, expected.by);

                match asked {
                    true => format!("refused, but not by {expected}: the {nearest}"),
                    false => format!(
                        "refused, but not by {expected}, which was not asked: the {nearest}"
                    ),
                }
            })
    }
}
