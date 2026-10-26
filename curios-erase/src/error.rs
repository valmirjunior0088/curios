//! What erasure refuses a program for, and what its reader refused it for.
//!
//! **Erasure's own refusals are what only the erased program shows.** Two recursion rules `curios-anf`'s verifier holds a fragment to — values whose initializers evaluate one another, and an initializer that performs an effect — each named by the source names erasure minted the verifier's identities for, and a natural case key past what a branch table holds. Everything else erasure is refused for is its reader's, wrapped as the reader raised it and reported in the reader's words: a type the reader could not read, a budget it spent.

use {
    crate::ErasedNames,
    curios_elab::Reading,
    curios_num::Natural,
    curios_utilities::{Note, Report},
    std::{collections::BTreeMap, rc::Rc},
};

/// A member or a callee of a refusal the erased verifier raised: the name erasure minted it for, or the erased program's own name for it where erasure minted it for none.
#[derive(Debug, Clone)]
pub enum ErasedName {
    Named(curios_core::Free),
    Erased(String),
}

impl ErasedName {
    /// This name as its reader writes it: a declaration as resolution reaches it, a local binder by its label.
    fn spelled(&self, spelling: &curios_core::Spelling) -> String {
        match self {
            ErasedName::Named(curios_core::Free::Global(global)) => spelling.symbol(global),
            ErasedName::Named(local) => local.hint().unwrap_or("_").to_string(),
            ErasedName::Erased(name) => name.clone(),
        }
    }
}

/// Why erasure refused a program.
#[derive(Debug)]
pub enum Error {
    /// What erasure's reader refused, as it raised it.
    Reading(Box<curios_elab::Error>),
    /// Computed recursive members whose initializers evaluate one another, so no order of forcing them by need can initialize them. Each step is a member by its source name and the call it reads the next member through, when the read goes through one; a single step is a member evaluating itself.
    EvaluationCycle {
        steps: Vec<(ErasedName, Option<ErasedName>)>,
    },
    /// A computed recursive member whose initializer performs an effect, directly or through the named call, which forcing it by need could not keep in its place.
    InitializerPerformsEffect {
        member: ErasedName,
        through: Option<ErasedName>,
    },
    /// A natural-dispatch case key that does not fit `curios-anf`'s `NatCase`. A `Nat` value is unbounded through every erased stage; a branch table's key is not.
    NatCaseKeyOverflow { value: Natural },
    /// An error of erasure's own, at the term erasure was erasing.
    Located {
        extent: curios_core::Extent,
        error: Box<Error>,
    },
}

impl From<curios_core::UniverseError> for Error {
    fn from(error: curios_core::UniverseError) -> Self {
        curios_elab::Error::from(error).into()
    }
}

impl From<curios_elab::Error> for Error {
    fn from(error: curios_elab::Error) -> Self {
        Self::Reading(Box::new(error))
    }
}

impl Error {
    pub fn nat_case_key_overflow(value: Natural) -> Self {
        Self::NatCaseKeyOverflow { value }
    }

    /// The refusal of a read of `member` from another item, where `member`'s own initializer reads it: the one-step cycle the verifier would name had the two been erased together.
    pub fn evaluates_itself(member: curios_core::Free) -> Self {
        Self::EvaluationCycle {
            steps: vec![(ErasedName::Named(member), None)],
        }
    }

    /// The refusal a program earns from the erased verifier, framed at the refused member's initializer when `extents` records one — a cycle at its first member. Only the verifier's recursion rules come back as an error: a malformed module is a compiler fault, which the verifier panics on rather than returning.
    pub fn refused_by_verifier(
        error: curios_anf::VerifyError,
        extents: &BTreeMap<curios_anf::ValueId, curios_core::Extent>,
        names: &ErasedNames,
    ) -> Self {
        // A member and a callee by the name erasure minted each for, and by the erased program's own where it minted it for none: a lambda written in place, a fragment linked past the names of its items.
        let member = |value: curios_anf::ValueId, erased: String| match names.values.get(&value) {
            Some(name) => ErasedName::Named(*name),
            None => ErasedName::Erased(erased),
        };
        let callee = |call: curios_anf::CycleCall| match names.functions.get(&call.function) {
            Some(name) => ErasedName::Named(*name),
            None => ErasedName::Erased(call.name),
        };

        match error {
            curios_anf::VerifyError::EvaluationCycle { steps } => {
                let extent = steps
                    .first()
                    .and_then(|step| extents.get(&step.value))
                    .cloned();
                Self::EvaluationCycle {
                    steps: steps
                        .into_iter()
                        .map(|step| (member(step.value, step.member), step.through.map(callee)))
                        .collect(),
                }
                .at_opt(extent)
            }
            curios_anf::VerifyError::InitializerPerformsEffect {
                member: erased,
                value,
                through,
            } => Self::InitializerPerformsEffect {
                member: member(value, erased),
                through: through.map(callee),
            }
            .at_opt(extents.get(&value).cloned()),
        }
    }

    /// Record where this error arose: the first extent stamped wins, as the elaborator's does, and a reader's error is stamped as the reader's own.
    pub fn at(self, extent: curios_core::Extent) -> Self {
        match self {
            Self::Reading(error) => Self::Reading(Box::new(error.at(extent))),
            Self::Located { .. } => self,
            error => Self::Located {
                extent,
                error: Box::new(error),
            },
        }
    }

    pub fn at_opt(self, extent: Option<curios_core::Extent>) -> Self {
        match extent {
            Some(extent) => self.at(extent),
            None => self,
        }
    }

    /// Render this error for its reader: [`Error::reports`], rendered.
    pub fn format(&self, reading: &Reading<'_>) -> String {
        Report::render_all(&self.reports(reading))
    }

    /// This error as its reports: a reader's error as the reader reports it, and one of erasure's own as one report at its extent, spelled for a reader standing outside every declaration.
    pub fn reports(&self, reading: &Reading<'_>) -> Vec<Report> {
        let (extent, refusal) = match self {
            Self::Reading(error) => return error.reports(reading),
            Self::Located { extent, error } => (Some(extent), &**error),
            refusal => (None, refusal),
        };
        let spelling = reading.spelling().for_reader(curios_core::ReaderPosition {
            owner: None,
            witnesses: Rc::default(),
        });

        vec![Report {
            span: extent.and_then(|extent| extent.resolved(reading.anchors)),
            message: refusal.message(&spelling),
            notes: refusal.notes(&spelling, reading.anchors),
        }]
    }

    /// The sentence one of erasure's own refusals says.
    fn message(&self, spelling: &curios_core::Spelling) -> String {
        match self {
            Self::NatCaseKeyOverflow { value } => {
                format!("natural dispatch key {value} does not fit a branch table")
            }
            Self::EvaluationCycle { steps } => {
                let quoted = steps
                    .iter()
                    .map(|(member, _)| format!("`{}`", member.spelled(spelling)))
                    .collect::<Vec<_>>();
                let listed = match quoted.split_last() {
                    Some((last, rest)) if !rest.is_empty() => {
                        format!("{} and {last}", rest.join(", "))
                    }
                    _ => quoted.join(", "),
                };
                let calls = steps
                    .iter()
                    .filter_map(|(_, through)| through.as_ref())
                    .map(|callee| format!("`{}`", callee.spelled(spelling)))
                    .collect::<Vec<_>>();
                let through = match calls.as_slice() {
                    [] => String::new(),
                    [call] => format!(" through a call to {call}"),
                    _ => format!(" through calls to {}", calls.join(", ")),
                };
                match steps.len() {
                    1 => format!(
                        "{listed} evaluates itself{through} as it is initialized; a value may mention itself only under a lambda, where it is computed the first time it is read"
                    ),
                    _ => format!(
                        "{listed} evaluate each other{through} as they are initialized; values may mention one another only under a lambda, where each is computed the first time it is read"
                    ),
                }
            }
            Self::InitializerPerformsEffect { member, through } => {
                let through = match through {
                    Some(callee) => {
                        format!(" through a call to `{}`", callee.spelled(spelling))
                    }
                    None => String::new(),
                };
                format!(
                    "the initializer of `{}` performs an effect{through}; a recursive value is computed when it is first read, or never, so its initializer may perform none",
                    member.spelled(spelling)
                )
            }
            Self::Reading(_) | Self::Located { .. } => {
                unreachable!("a reader's error and a location are reported through what they hold")
            }
        }
    }

    /// The further places one of erasure's own refusals is about: a value read where it evaluates itself is written in its declaration.
    fn notes(
        &self,
        spelling: &curios_core::Spelling,
        anchors: &[&curios_core::Anchors],
    ) -> Vec<Note> {
        match self {
            Self::EvaluationCycle { steps } => match steps.as_slice() {
                [(named @ ErasedName::Named(curios_core::Free::Global(member)), None)] => {
                    declared_here(member, named, spelling, anchors)
                }
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }
}

/// The note that `member` is declared where its label stands, where it stands anywhere a reader can see.
fn declared_here(
    member: &curios_core::Global,
    named: &ErasedName,
    spelling: &curios_core::Spelling,
    anchors: &[&curios_core::Anchors],
) -> Vec<Note> {
    curios_core::Anchors::label(anchors, member)
        .map(|span| Note {
            span,
            message: format!("`{}` is declared here", named.spelled(spelling)),
        })
        .into_iter()
        .collect()
}
