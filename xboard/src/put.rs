//! A witness put to the checkers at `False`, through the product's put ([`put_program`], [`put_core_text`]), and what they said: what `curios wonder --false` answers of the same text, the history before it the board's own.

#[cfg(test)]
mod tests;

use {
    crate::{Proof, Step, Witness},
    curios_common::{Report, Source},
    curios_elab::DEFAULT_BUDGET,
    curios_module::{RootSource, SYNTAX},
    curios_pipeline::{
        CompileError, Put, Refusal, Unmade, certifier_said, put_core_text, put_program,
    },
    curios_surface::{Entrypoint, ReadError},
    curios_unit::Predecessors,
    curios_verdict::test_support::*,
};

/// What putting a witness to the checkers answered.
#[derive(Debug)]
pub enum Answer {
    /// What the checkers said of the proof put at `False`, or of a step's unit the certifier refused, which stops a history there.
    Put(curios_pipeline::Answer),
    /// Kept from the checkers' judgment at `False`: a text no checker read, the proof's or a step's, or a history stopped at a step by what names no checker's error.
    Stopped(Stop),
}

/// What kept a witness from the checkers' judgment at `False`, as it was reported. No expectation meets it: on the board it is a text that has rotted, or a fix that refuses an earlier unit by no checker's error, which a witness writes as Core text instead.
#[derive(Debug)]
pub struct Stop {
    /// Where it stopped, as a report names it: `text` for one no checker read, or the step of the unit it stopped at.
    pub at: String,
    pub reported: String,
    /// Whether the certifier judged a unit on the way: one a step wrote, admitted before erasure refused it.
    pub judged: bool,
}

impl Answer {
    /// Every refusal a checker made: none for a stop.
    pub fn refusals(&self) -> &[Refusal] {
        match self {
            Answer::Put(answer) => answer.refusals(),
            Answer::Stopped(_) => &[],
        }
    }

    /// Whether every checker accepted the term at `False`: a forgery.
    pub fn admitted(&self) -> bool {
        match self {
            Answer::Put(answer) => answer.admitted(),
            Answer::Stopped(_) => false,
        }
    }

    /// Whether the certifier judged the witness: always for a module put, for a program only where the elaborator built one to hand it, and for a history stopped by erasure at the unit it admitted.
    pub fn certifier_asked(&self) -> bool {
        match self {
            Answer::Put(answer) => answer.certifier_asked(),
            Answer::Stopped(stop) => stop.judged,
        }
    }

    /// Who refused the witness and by which error, as a report notes it beside the witness, or where it stopped.
    pub fn refusers(&self) -> String {
        match self {
            Answer::Put(answer) => answer.refusers(),
            Answer::Stopped(stop) => stop.at.clone(),
        }
    }
}

/// Put `proof` at `False` over `predecessors` through the product's put. `Err` is its text put to no checker, as it was reported: a program that does not parse or lower, or Core text that does not read or closes with no term.
fn put_proof(predecessors: Predecessors<'_>, proof: &Proof) -> Result<Put, String> {
    let put = match proof {
        Proof::Program(text) => {
            let entrypoint = Entrypoint::parse(&Source::inline(text.as_str()))
                .map_err(|error| error.report().render())?;

            put_program(
                DEFAULT_BUDGET,
                predecessors,
                &SYNTAX,
                &entrypoint,
                &RootSource::none(),
            )
            .map(Some)
        }
        Proof::Module(text) => put_core_text(
            DEFAULT_BUDGET,
            predecessors,
            &SYNTAX,
            &Source::inline(text.as_str()),
        ),
    };

    put.map_err(String::from)?
        .ok_or_else(|| "the text closes with no term to put".to_string())
}

/// The reader's refusal, at the place in the text it stands where it names one.
fn located(error: &ReadError) -> String {
    match &error.span {
        Some(span) => Report::at(span.clone(), error.to_string()).render(),
        None => error.to_string(),
    }
}

/// Walk `steps` over a cabinet of their own, then put `proof` to the checkers at `False` over the prelude and every unit they compiled: the history a witness is. With no step, the proof is put over the prelude alone, and nothing is written anywhere.
pub fn put(steps: &[Step], proof: &Proof) -> Answer {
    let mut history = History::new();
    for step in steps {
        let walked = match step {
            Step::Unit {
                mount,
                written,
                text,
            } => history.step(mount, *written, text),
            Step::Unmount { mount } => history.unmounted(mount),
        };
        if let Err(stopped) = walked {
            return halted(stopped);
        }
    }

    history
        .built(|predecessors| match put_proof(predecessors, proof) {
            Ok(put) => Answer::Put(put.answer),
            Err(reported) => unread(reported),
        })
        .unwrap_or_else(halted)
}

/// A text no checker read, as it was reported.
fn unread(reported: String) -> Answer {
    Answer::Stopped(Stop {
        at: "text".to_string(),
        reported,
        judged: false,
    })
}

/// What a history that stopped before its proof answered: the refusal of the step it stopped at, as the checker that made it raised it where one did.
fn halted(Stopped { mount, why }: Stopped) -> Answer {
    let at = |reported: String| format!("in the step of `/{mount}`: {reported}");
    let step = |reported: String, judged: bool| {
        Answer::Stopped(Stop {
            at: format!("the step of `/{mount}`"),
            reported,
            judged,
        })
    };
    match why {
        Unbuilt::Unparsed(reported) => unread(at(reported)),
        Unbuilt::Read(error) => unread(at(located(&error))),
        Unbuilt::Unmade(Unmade::Certifier(verdict)) => {
            Answer::Put(curios_pipeline::Answer::Judged(vec![Refusal::Certifier {
                reported: CompileError::Failure(vec![Report::unlocated(at(certifier_said(
                    &verdict,
                )))]),
                refusal: *verdict,
            }]))
        }
        Unbuilt::Unmade(Unmade::Erased(error)) => step(format!("{error:?}"), true),
        Unbuilt::Compiled(error) => step(error.to_string(), false),
        Unbuilt::Stored(reported) => step(reported, false),
    }
}

impl Proof {
    /// Put the proof at `False` over the prelude alone, as `curios wonder --false` puts it: `Err` is its text, put to no checker, as it was reported.
    pub fn put(&self) -> Result<Put, String> {
        History::new()
            .built(|predecessors| put_proof(predecessors, self))
            .expect("a history of no step builds")
    }
}

impl Witness {
    /// Put the witness to the checkers at `False`: its history walked, then its proof.
    pub fn answer(&self) -> Answer {
        put(&self.steps, &self.proof)
    }
}
