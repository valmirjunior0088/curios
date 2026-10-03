//! A witness put to the checkers at `False`, and what they said.

#[cfg(test)]
mod tests;

use {
    crate::Proof,
    curios_cert::Verdict,
    curios_core::{Free, Global, Module, Program, Term, Zonked, ZonkedRefusal},
    curios_pipeline::{DEFAULT_STEP_BUDGET, Examined, examine_with_prelude, recheck_with_prelude},
    curios_text::{RootSource, SYNTAX},
    std::fmt,
};

/// `/std/Bool/False` as an elaborated program mentions it, by the name the compiler's own registry holds: what a module built by hand is put at, what its terms name `False` by, and the type the pipeline states for a program put as a proof.
pub fn false_type() -> Term {
    Term::free_var(&Free::from(&Global::Authored(
        SYNTAX.proof.false_type.qualifier(),
    )))
}

/// One of the two checkers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Checker {
    /// `curios-elab`, which builds the module. A program the grammar refuses is counted here: no module exists for the kernel either way.
    Elaborator,
    /// `curios-cert` and the layer it judges through: the trusted base.
    Kernel,
}

impl fmt::Display for Checker {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Checker::Elaborator => "elaborator",
            Checker::Kernel => "kernel",
        })
    }
}

/// One refusal a witness met, as the checker that made it raised it.
#[derive(Debug)]
pub enum Refusal {
    /// The program never reached the elaborator's judgment: its text did not parse or did not lower. Held as the compilation reports it, since what is raised there is a message.
    Text(String),
    /// The elaborator's error, with the diagnostic a compilation prints for it.
    Elaborator {
        error: curios_elab::Error,
        reported: String,
    },
    /// A module carrying what elaboration should have solved, refused by the evidence the kernel's walk takes a module with. Reported, and never what a witness expects: the kernel refuses such a module itself, and that refusal is the one a fix is held by.
    Unfinished(ZonkedRefusal),
    /// The kernel's refusal, and the item or registry entry it refused.
    Kernel(Verdict),
}

/// The error an elaborator's report wraps in where it was raised: what a witness names.
pub(crate) fn raised(error: &curios_elab::Error) -> &curios_elab::Error {
    let mut error = error;

    while let curios_elab::Error::Located { error: inner, .. }
    | curios_elab::Error::InDeclaration { error: inner, .. }
    | curios_elab::Error::InUnreachableArm { error: inner, .. }
    | curios_elab::Error::InScope { error: inner, .. } = error
    {
        error = inner;
    }

    error
}

/// The variant `error` is, as its `Debug` spells it.
fn variant(error: &impl fmt::Debug) -> String {
    let spelled = format!("{error:?}");

    spelled
        .split(['(', ' ', '{'])
        .next()
        .unwrap_or_default()
        .to_string()
}

impl Refusal {
    /// The checker that made it.
    pub fn by(&self) -> Checker {
        match self {
            Refusal::Text(_) | Refusal::Elaborator { .. } => Checker::Elaborator,
            Refusal::Unfinished(_) | Refusal::Kernel(_) => Checker::Kernel,
        }
    }

    /// The checker and the errors it raised, by variant: what a report notes beside a witness, and what an expectation that names an error is written from.
    pub fn named(&self) -> String {
        match self {
            Refusal::Text(_) => "text".to_string(),
            Refusal::Elaborator { error, .. } => {
                let mut variants = error
                    .each()
                    .map(|member| format!("`{}`", variant(raised(member))))
                    .collect::<Vec<_>>();
                variants.dedup();

                format!("elaborator {}", variants.join(" "))
            }
            Refusal::Unfinished(_) => "kernel, as unfinished".to_string(),
            Refusal::Kernel(verdict) => format!("kernel `{}`", variant(&verdict.error)),
        }
    }
}

/// What the refusal says, as a compilation prints it.
impl fmt::Display for Refusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Refusal::Text(reported) | Refusal::Elaborator { reported, .. } => {
                formatter.write_str(reported)
            }
            Refusal::Unfinished(refusal) => write!(formatter, "{refusal}"),
            Refusal::Kernel(verdict) => write!(formatter, "{}", verdict.error),
        }
    }
}

/// What putting a witness to the checkers answered.
#[derive(Debug)]
pub enum Answer {
    /// Refused before a module existed for the kernel to judge: by the program's text, or by the elaborator while it was building one.
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

/// Put a source program to both checkers as a proof of `False`: the elaborator, then the kernel over the module it built. The pipeline states the type; nothing here or in the program chooses it.
///
/// The elaborator's erasure obligations are reported rather than raised, so a program only it refuses still yields a module for the kernel to judge. A refusal while the module is being built leaves the kernel nothing to judge.
fn put_program(source: &str) -> Answer {
    let entrypoint = match source.parse::<curios_text::Entrypoint>() {
        Ok(entrypoint) => entrypoint,
        Err(error) => return Answer::Unbuilt(Refusal::Text(format!("{error:?}"))),
    };

    match examine_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none()) {
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
                    reported: obligation.reported,
                })
                .chain(verdicts.into_iter().map(Refusal::Kernel))
                .collect(),
        ),
    }
}

/// Put a module built by hand to the kernel alone, with the prelude in scope, as a program closing with `body` stated at `type_`.
fn put_module(module: Module, body: Term, type_: Term) -> Answer {
    let program = Program {
        module,
        entry: curios_core::Entrypoint {
            body,
            type_: Some(type_),
        },
    };

    // The walk takes a program only with the evidence that elaboration finished it, so one carrying what elaboration should have solved is refused here, by the layer the kernel judges through.
    Answer::Judged(match Zonked::project(&program) {
        Err(refusal) => vec![Refusal::Unfinished(refusal)],
        Ok(program) => recheck_with_prelude(&program, DEFAULT_STEP_BUDGET)
            .into_iter()
            .map(Refusal::Kernel)
            .collect(),
    })
}

impl Proof {
    /// Put the proof to the checkers at `False`.
    pub fn answer(&self) -> Answer {
        match self {
            Proof::Program(source) => put_program(source),
            Proof::Module(build) => {
                let (module, proof) = build();

                put_module(module, proof, false_type())
            }
        }
    }
}
