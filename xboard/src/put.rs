//! A witness put to the checkers at `False`, and what they said.

#[cfg(test)]
mod tests;

use {
    crate::{Proof, Step, Witness},
    curios_cert::Verdict,
    curios_common::{Report, test_support::Variants},
    curios_core::{Definitions, Global, Program, Term, Universes, Zonked, ZonkedRefusal},
    curios_module::{RootSource, SYNTAX},
    curios_pipeline::{
        DEFAULT_BUDGET, EntryTail, Examined, examine_entrypoint, recheck, test_support::*,
    },
    curios_surface::{CoreEntry, CoreFile, FormScope, ReadError, Spelling, to_core},
    curios_unit::Predecessors,
    curios_verdict::test_support::*,
    serde::Deserialize,
    std::fmt,
};

/// The name the compiler's own registry holds `/std/Bool/False` under: its type former's, and the key its registry entry stands at.
pub fn false_name() -> Global {
    Global::Authored(SYNTAX.proof.false_type.qualifier())
}

/// `/std/Bool/False` as an elaborated program mentions it, by [`false_name`]: what a module built by hand is put at, what its terms name `False` by, and the type the pipeline states for a program put as a proof.
pub fn false_type() -> Term {
    Term::instance(false_name(), Universes::default())
}

/// What a witness is put to: the checkers, and the reader that builds a module written as Core text in the elaborator's place. A record names one by the word it is displayed as.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Checker {
    /// `curios-elab`, which builds the module of a program. A program the grammar refuses is counted here: no module exists for the kernel either way.
    Elaborator,
    /// `curios-surface`'s reader, which builds the module Core text writes. Text that does not parse is counted here, as a program the grammar refuses is the elaborator's.
    Reader,
    /// `curios-cert` and the layer it judges through: the trusted base.
    Kernel,
}

impl Checker {
    /// Every error the checker raises, by the name its crate states for it once, in a match no variant is missing from.
    pub fn errors(self) -> &'static [&'static str] {
        match self {
            Checker::Elaborator => <curios_elab::Error as Variants>::NAMES,
            Checker::Reader => <curios_surface::Refusal as Variants>::NAMES,
            Checker::Kernel => <curios_cert::Error as Variants>::NAMES,
        }
    }
}

impl fmt::Display for Checker {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Checker::Elaborator => "elaborator",
            Checker::Reader => "reader",
            Checker::Kernel => "kernel",
        })
    }
}

/// One refusal a witness met, as the checker that made it raised it.
#[derive(Debug)]
pub enum Refusal {
    /// The program never reached the elaborator's judgment: its text did not parse or did not lower. Held as the compilation reports it, since what is raised there is a message.
    Text(String),
    /// Core text that does not parse, or closes otherwise than with a term stating no type: a text that has rotted, which no expectation meets. Held as the reader reports it.
    Unread(String),
    /// The reader's refusal of a text that parses, with where it stands in the text.
    Read { error: ReadError, reported: String },
    /// The elaborator's error, with the diagnostic a compilation prints for it.
    Elaborator {
        error: curios_elab::Error,
        reported: String,
    },
    /// A module carrying what elaboration should have solved, refused by the evidence the kernel's walk takes a module with. Reported, and never what a witness expects: the kernel refuses such a module itself, and that refusal is the one a fix is held by.
    Unfinished(ZonkedRefusal),
    /// The kernel's refusal, and the item or registry entry it refused.
    Kernel(Verdict),
    /// A history stopped at the step of the unit claiming `mount` by what names no checker's error, held as it was reported: a program refused as a build reports it, a unit erasure refused, or the history's store. No expectation meets it, so a fix that refuses a unit before the proof writes that unit as Core text.
    Stopped { mount: String, reported: String },
}

/// Where a refusal stands, as a report says it: in the declaration it names by its path, or at the closing term.
pub fn standing_at(name: Option<&Global>) -> String {
    match name {
        None => "at the closing term".to_string(),
        Some(Global::Authored(path)) => {
            format!("in `/{}`", path.iter().collect::<Vec<_>>().join("/"))
        }
        Some(Global::Witness(_)) => "in a witness".to_string(),
    }
}

impl Refusal {
    /// The checker that made it, where the refusal names one.
    pub fn by(&self) -> Option<Checker> {
        match self {
            Refusal::Text(_) | Refusal::Elaborator { .. } => Some(Checker::Elaborator),
            Refusal::Unread(_) | Refusal::Read { .. } => Some(Checker::Reader),
            Refusal::Unfinished(_) | Refusal::Kernel(_) => Some(Checker::Kernel),
            Refusal::Stopped { .. } => None,
        }
    }

    /// The checker and the errors it raised, by variant and by where each stands: what a report notes beside a witness, and what an expectation that names an error is written from.
    pub fn named(&self) -> String {
        match self {
            Refusal::Text(_) => "text".to_string(),
            Refusal::Unread(_) => "reader".to_string(),
            Refusal::Read { error, .. } => format!("reader `{}`", error.refusal.variant_name()),
            Refusal::Elaborator { error, .. } => {
                let mut variants = error
                    .each()
                    .map(|member| {
                        format!(
                            "`{}` {}",
                            member.raised().variant_name(),
                            standing_at(member.declared_in())
                        )
                    })
                    .collect::<Vec<_>>();
                variants.dedup();

                format!("elaborator {}", variants.join(" "))
            }
            Refusal::Unfinished(_) => "kernel, as unfinished".to_string(),
            Refusal::Kernel(verdict) => format!(
                "kernel `{}` {}",
                verdict.error.variant_name(),
                standing_at(verdict.name.as_ref())
            ),
            Refusal::Stopped { mount, .. } => format!("the step of `/{mount}`"),
        }
    }
}

/// What the refusal says, as a compilation prints it.
impl fmt::Display for Refusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Refusal::Text(reported)
            | Refusal::Unread(reported)
            | Refusal::Read { reported, .. }
            | Refusal::Elaborator { reported, .. }
            | Refusal::Stopped { reported, .. } => formatter.write_str(reported),
            Refusal::Unfinished(refusal) => write!(formatter, "{refusal}"),
            Refusal::Kernel(verdict) => {
                write!(
                    formatter,
                    "{}",
                    verdict.error.rendered(&Spelling::default())
                )
            }
        }
    }
}

/// What putting a witness to the checkers answered.
#[derive(Debug)]
pub enum Answer {
    /// Refused before a module existed for the kernel to judge: by the program's text, by the elaborator while it was building one, or by the reader of Core text; or a history stopped by what names no checker, which says nothing of whether the kernel was asked.
    Unbuilt(Refusal),
    /// Judged by the kernel, and for a program by the elaborator before it, with every refusal either made: none where the term was admitted at `False`.
    Judged(Vec<Refusal>),
}

impl Answer {
    /// Every refusal the witness met.
    pub fn refusals(&self) -> &[Refusal] {
        match self {
            Answer::Unbuilt(refusal) => std::slice::from_ref(refusal),
            Answer::Judged(refusals) => refusals,
        }
    }

    /// Whether every checker accepted the term at `False`: a forgery.
    pub fn admitted(&self) -> bool {
        self.refusals().is_empty()
    }

    /// Whether the kernel judged the witness: always for a module, and for a program only where the elaborator built one to hand it.
    pub fn kernel_asked(&self) -> bool {
        matches!(self, Answer::Judged(_))
    }
}

/// Put a source program to both checkers as a proof of `False`, over `predecessors`: the elaborator, then the kernel over the module it built. The pipeline states the type; nothing here or in the program chooses it.
///
/// The elaborator's erasure obligations are reported rather than raised, so a program only it refuses still yields a module for the kernel to judge. A refusal while the module is being built leaves the kernel nothing to judge.
fn put_program(predecessors: Predecessors<'_>, source: &str) -> Answer {
    let entrypoint = match source.parse::<curios_surface::Entrypoint>() {
        Ok(entrypoint) => entrypoint,
        Err(error) => return Answer::Unbuilt(Refusal::Text(format!("{error:?}"))),
    };

    match examine_entrypoint(
        DEFAULT_BUDGET,
        predecessors,
        &SYNTAX,
        &entrypoint,
        &RootSource::none(),
        EntryTail::False,
    ) {
        Err(refused) => Answer::Unbuilt(match refused.error {
            Some(error) => Refusal::Elaborator {
                error: *error,
                reported: refused.reported.into(),
            },
            None => Refusal::Text(refused.reported.into()),
        }),
        Ok(Examined {
            obligations,
            verdicts,
        }) => Answer::Judged(
            obligations
                .into_iter()
                .map(|obligation| Refusal::Elaborator {
                    error: obligation.error,
                    reported: Report::render_all(&obligation.reported),
                })
                .chain(verdicts.into_iter().map(Refusal::Kernel))
                .collect(),
        ),
    }
}

/// Put a module built by hand to the kernel alone, with `predecessors` in scope, as a program closing with `body` stated at `type_`.
fn put_module(
    predecessors: Predecessors<'_>,
    definitions: Definitions,
    body: Term,
    type_: Term,
) -> Answer {
    let program = Program {
        definitions,
        entrypoint: curios_core::Entrypoint {
            body,
            type_: Some(type_),
        },
    };

    // The walk takes a program only with the evidence that elaboration finished it, so one carrying what elaboration should have solved is refused here, by the layer the kernel judges through.
    Answer::Judged(match Zonked::project(&program) {
        Err(refusal) => vec![Refusal::Unfinished(refusal)],
        Ok(program) => recheck(&program, DEFAULT_BUDGET, predecessors, &SYNTAX)
            .into_iter()
            .map(Refusal::Kernel)
            .collect(),
    })
}

/// Read `text` as Core text over `predecessors`, and put the module it writes to the kernel alone, with them in scope, as a program closing with its term at `False`. The text states no type for its term: the board states it.
fn put_core_text(predecessors: Predecessors<'_>, text: &str) -> Answer {
    let unread = |reported: String| Answer::Unbuilt(Refusal::Unread(reported));
    let file = match text.parse::<CoreFile>() {
        Ok(file) => file,
        Err(error) => return unread(error.format()),
    };
    match &file.entry {
        Some(CoreEntry { type_: None, .. }) => {}
        Some(CoreEntry { type_: Some(_), .. }) => {
            return unread(
                "the text states the type its term is put at, which the board states".to_string(),
            );
        }
        None => return unread("the text closes with no term to put".to_string()),
    }

    match to_core(&file, Some(&FormScope::over(SYNTAX, predecessors.cores()))) {
        Err(error) => Answer::Unbuilt(Refusal::Read {
            reported: located(&error),
            error,
        }),
        Ok((definitions, entry)) => {
            let body = entry.expect("an entry the text writes is read").body;

            put_module(predecessors, definitions, body, false_type())
        }
    }
}

/// The reader's refusal, at the place in the text it stands where it names one.
fn located(error: &ReadError) -> String {
    match &error.span {
        Some(span) => Report::at(span.clone(), error.to_string()).render(),
        None => error.to_string(),
    }
}

/// Walk `steps` over a store of their own, then put `proof` to the checkers at `False` over the prelude and every unit they compiled: the history a witness is. With no step, the proof is put over the prelude alone, and nothing is written anywhere.
pub fn put(steps: &[Step], proof: &Proof) -> Answer {
    let mut history = History::new();
    for step in steps {
        let walked = match step {
            Step::Unit {
                mount,
                written,
                text,
            } => history.step(mount, *written, text),
            Step::Withhold { mount, names } => history.withhold(mount, names.as_deref()),
        };
        if let Err(stopped) = walked {
            return halted(stopped);
        }
    }

    history
        .built(|predecessors| match proof {
            Proof::Program(source) => put_program(predecessors, source),
            Proof::Module(text) => put_core_text(predecessors, text),
        })
        .unwrap_or_else(halted)
}

/// What a history that stopped before its proof answered: the refusal of the step it stopped at, as the checker that made it raised it where one did.
fn halted(Stopped { mount, why }: Stopped) -> Answer {
    let at = |reported: String| format!("in the step of `/{mount}`: {reported}");
    match why {
        Unbuilt::Unparsed(reported) => Answer::Unbuilt(Refusal::Unread(at(reported))),
        Unbuilt::Read(error) => Answer::Unbuilt(Refusal::Read {
            reported: at(located(&error)),
            error: *error,
        }),
        Unbuilt::Unmade(Unmade::Unfinished(refusal)) => {
            Answer::Judged(vec![Refusal::Unfinished(*refusal)])
        }
        Unbuilt::Unmade(Unmade::Kernel(verdict)) => Answer::Judged(vec![Refusal::Kernel(*verdict)]),
        Unbuilt::Unmade(Unmade::Erased(reported)) => {
            Answer::Judged(vec![Refusal::Stopped { mount, reported }])
        }
        Unbuilt::Compiled(error) => Answer::Unbuilt(Refusal::Stopped {
            mount,
            reported: error.to_string(),
        }),
        Unbuilt::Stored(reported) => Answer::Unbuilt(Refusal::Stopped { mount, reported }),
    }
}

impl Proof {
    /// Put the proof to the checkers at `False` over the prelude alone.
    pub fn answer(&self) -> Answer {
        put(&[], self)
    }
}

impl Witness {
    /// Put the witness to the checkers at `False`: its history walked, then its proof.
    pub fn answer(&self) -> Answer {
        put(&self.steps, &self.proof)
    }
}
