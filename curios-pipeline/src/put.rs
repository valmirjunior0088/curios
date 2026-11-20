//! A closing term put at `False`, and what the checkers said of it: the question `wonder --false` asks of a subject and the soundness board asks of every witness, answered by one function, so a ticket's answer is what the product answers of the same text.
//!
//! **A program meets both checkers, as a compilation does, and Core text the certifier alone.** A program goes through [`examine_entrypoint`] with its closing term at `False`: the elaborator, its erasure obligations reported rather than raised so that a program only it refuses still reaches the certifier, then the certifier over the module it built. A text of Core text goes through the door ([`read_core_text`]), its closing term put at `False` by the reader's rule, and its module and term to the certifier's walk. Each refusal is its checker's own error, named as its crate names it ([`Variants`]) with where it stood, and reported as a compilation reports it, placed where the text writes the declaration it is about, or the term it closes with ([`program_refusal`]).
//!
//! **A subject that does not lower, or a text that does not read, was put to no checker**: the question is not answered, and its report is handed back as a compilation's would be — a refusal by the grammar is no refusal by a checker, as the Lean Kernel Arena tells a checker declining an input from one rejecting it.

#[cfg(test)]
mod tests;

use {
    crate::{
        CompileError, EntryTail, Examined, Refused, Refuted, examine_entrypoint, globals,
        program_refusal, read_core_text,
    },
    curios_cert::certify_program,
    curios_common::{Report, Source, SyntaxRegistry, Variants},
    curios_core::{Global, Program, Term, Universes},
    curios_module::RootSource,
    curios_surface::{Entrypoint, FormScope, Spelling},
    curios_unit::Predecessors,
    std::{fmt, sync::Arc},
};

/// What judges a term put at `False`: the two checkers, each by the word `wonder --false` and the soundness board name it by.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Checker {
    /// `curios-elab`, which builds the module of a program.
    Elaborator,
    /// `curios-cert` and the layer it judges through: the trusted base.
    Certifier,
}

impl Checker {
    /// What a program is put to: both checkers, the elaborator first.
    pub const PROGRAM: &'static [Checker] = &[Checker::Elaborator, Checker::Certifier];

    /// What a text of Core text is put to: the certifier alone, nothing being left to an elaborator.
    pub const CORE_TEXT: &'static [Checker] = &[Checker::Certifier];

    /// Every error the checker raises, by the name its crate states for it once, in a match no variant is missing from.
    pub fn errors(self) -> &'static [&'static str] {
        match self {
            Checker::Elaborator => <curios_elab::Error as Variants>::NAMES,
            Checker::Certifier => <curios_cert::Error as Variants>::NAMES,
        }
    }
}

impl fmt::Display for Checker {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Checker::Elaborator => "elaborator",
            Checker::Certifier => "certifier",
        })
    }
}

/// One refusal a term put at `False` met, as the checker that made it raised it.
#[derive(Debug)]
pub enum Refusal {
    /// The elaborator's error, with the diagnostic a compilation prints for it.
    Elaborator {
        error: curios_elab::Error,
        reported: String,
    },
    /// The certifier's refusal, of the item or registry entry it names, with the report a compilation makes of it.
    Certifier {
        refusal: curios_cert::Refusal,
        reported: String,
    },
}

impl Refusal {
    /// The refusal in its checker's own words: the elaborator's diagnostic, which opens with them, and the certifier's error on no place ([`certifier_said`]).
    pub fn said(&self) -> String {
        match self {
            Refusal::Elaborator { reported, .. } => reported.clone(),
            Refusal::Certifier { refusal, .. } => certifier_said(refusal),
        }
    }

    /// The checker that made it.
    pub fn by(&self) -> Checker {
        match self {
            Refusal::Elaborator { .. } => Checker::Elaborator,
            Refusal::Certifier { .. } => Checker::Certifier,
        }
    }

    /// The checker and the errors it raised, by variant and by where each stands: what an answer names beside its verdict, and what a ticket's expectation is written from.
    pub fn named(&self) -> String {
        match self {
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
            Refusal::Certifier { refusal, .. } => format!(
                "certifier `{}` {}",
                refusal.error.variant_name(),
                standing_at(refusal.name.as_ref())
            ),
        }
    }
}

/// The certifier's refusal in its own words: its error, on no place, which a report of it places beneath a heading.
pub fn certifier_said(refusal: &curios_cert::Refusal) -> String {
    refusal.error.rendered(&Spelling::default()).to_string()
}

/// What the refusal says, as a compilation prints it.
impl fmt::Display for Refusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Refusal::Elaborator { reported, .. } | Refusal::Certifier { reported, .. } => {
                formatter.write_str(reported)
            }
        }
    }
}

/// Where a refusal stands, as an answer says it: in the declaration it names by its path, or at the closing term.
pub fn standing_at(name: Option<&Global>) -> String {
    match name {
        None => "at the closing term".to_string(),
        Some(Global::Authored(path)) => {
            format!("in `/{}`", path.iter().collect::<Vec<_>>().join("/"))
        }
        Some(Global::Witness(_)) => "in a witness".to_string(),
    }
}

/// What the checkers said of a term put at `False`.
#[derive(Debug)]
pub enum Answer {
    /// Refused by the elaborator while it was building a module, before one existed for the certifier to judge.
    Unbuilt(Refusal),
    /// Judged by the certifier, and for a program by the elaborator before it, with every refusal either made: none where the term was admitted at `False`.
    Judged(Vec<Refusal>),
}

impl Answer {
    /// Every refusal the term met.
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

    /// Whether the certifier judged the term: always for Core text, and for a program only where the elaborator built it a module to hand on.
    pub fn certifier_asked(&self) -> bool {
        matches!(self, Answer::Judged(_))
    }

    /// The checkers that refused and the errors each raised, each named once, in the order they were met: `admitted` where none did.
    pub fn refusers(&self) -> String {
        match self.admitted() {
            true => "admitted".to_string(),
            false => {
                let mut named = self
                    .refusals()
                    .iter()
                    .map(Refusal::named)
                    .collect::<Vec<_>>();
                named.dedup();

                named.join(", ")
            }
        }
    }
}

/// A term put at `False` and what the checkers said of it ([`Answer`]), with what its dialect put it to and the name `False` is declared at.
#[derive(Debug)]
pub struct Put {
    pub answer: Answer,
    pub asked: &'static [Checker],
    pub empty: Global,
}

impl Put {
    /// What `checker` made of the term: refused, admitted, or not asked, where nothing built the certifier a module to judge.
    pub fn said(&self, checker: Checker) -> &'static str {
        let refused = self
            .answer
            .refusals()
            .iter()
            .any(|refusal| refusal.by() == checker);

        match (refused, checker) {
            (true, _) => "refused",
            (false, Checker::Certifier) if !self.answer.certifier_asked() => "not asked",
            (false, _) => "admitted",
        }
    }
}

/// The verdict as the answer's first word, `ADMITTED` or `REFUSED` and by whom; beneath it what each checker the term was put to made of it; then every refusal as its checker reports it.
impl fmt::Display for Put {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.answer.admitted() {
            true => writeln!(formatter, "ADMITTED   at `{}`", self.empty.faithful())?,
            false => writeln!(formatter, "REFUSED    by {}", self.answer.refusers())?,
        }

        for &checker in self.asked {
            writeln!(
                formatter,
                "           {:<10}  {}",
                checker.to_string(),
                self.said(checker)
            )?;
        }

        for refusal in self.answer.refusals() {
            writeln!(formatter, "\n{}", refusal.to_string().trim_end())?;
        }

        Ok(())
    }
}

/// The name `False` is declared at, by the registry `syntax`.
fn empty(syntax: &SyntaxRegistry) -> Global {
    Global::Authored(syntax.proof.false_type.qualifier())
}

/// Put the closing term of the program `entrypoint`, its modules read through `loader`, at `False` over `predecessors`: the elaborator, then the certifier over the module it built ([`examine_entrypoint`]). `Err` is a program that does not lower, put to no checker.
pub fn put_program(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
) -> Result<Put, CompileError> {
    let answer = match examine_entrypoint(
        budget,
        predecessors,
        syntax,
        entrypoint,
        loader,
        EntryTail::False,
    ) {
        Err(Refused {
            error: Some(error),
            reported,
        }) => Answer::Unbuilt(Refusal::Elaborator {
            error: *error,
            reported: reported.to_string(),
        }),
        Err(Refused {
            error: None,
            reported,
        }) => return Err(reported),
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
                .chain(verdicts.into_iter().map(|Refuted { verdict, reported }| {
                    Refusal::Certifier {
                        refusal: verdict,
                        reported: Report::render_all(&reported),
                    }
                }))
                .collect(),
        ),
    };

    Ok(Put {
        answer,
        asked: Checker::PROGRAM,
        empty: empty(syntax),
    })
}

/// Put the term the Core text `source` closes with at `False` over `predecessors`, through the door, its module and term to the certifier alone. `Ok(None)` is a text closing with no term, a module and no proof; `Err` a text that does not read, put to no checker.
pub fn put_core_text(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &Arc<Source>,
) -> Result<Option<Put>, CompileError> {
    let scope = FormScope::over(*syntax, predecessors.cores());
    let at = Term::instance(empty(syntax), Universes::default());
    let read = read_core_text(source, &scope, &at)
        .map_err(|unread| CompileError::Failure(vec![unread.report()]))?;
    let Some(entrypoint) = read.entry else {
        return Ok(None);
    };
    let program = Program {
        definitions: read.definitions,
        entrypoint,
    };
    let (rechecked, _) = certify_program(&program, budget, &globals(predecessors), *syntax);
    let anchors = std::iter::once(&read.anchors)
        .chain(predecessors.anchors())
        .collect::<Vec<_>>();
    let lexicons = predecessors.lexicons();

    Ok(Some(Put {
        answer: Answer::Judged(
            rechecked
                .into_refusals()
                .into_iter()
                .map(|verdict| Refusal::Certifier {
                    reported: program_refusal(
                        &verdict,
                        &program.definitions,
                        &lexicons,
                        &anchors,
                        syntax,
                    )
                    .to_string(),
                    refusal: verdict,
                })
                .collect(),
        ),
        asked: Checker::CORE_TEXT,
        empty: empty(syntax),
    }))
}
