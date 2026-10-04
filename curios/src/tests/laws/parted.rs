//! The rows where a law stops: each row a checker puts on the other side than its seed derives, with what each checker says and the finding that holds why.
//!
//! One table, held equal to what each audit finds: a row that starts parting fails its audit until it is listed, and a listed row that stops parting fails it until the entry is deleted, which is what the commit that closes its finding does. A row both checkers refuse is no disagreement between them and is listed all the same, since its seed derives that it holds.

use super::{ARMS, Answers, Context};

/// The audit that states a row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Audit {
    Seeds,
    Typed,
    Arms,
    Heads,
    Types,
    Reversed,
    Chained,
    Substituted,
}

/// One row a checker puts on the other side than derived.
pub(super) struct Parted {
    pub(super) audit: Audit,
    pub(super) row: String,
    pub(super) answers: Answers,
    /// The finding, by its heading in `documentation/roadmap/01-soundness/00-findings.md`.
    pub(super) finding: &'static str,
}

const ETA_BY_TYPE: &str = "The elaborator fires Π and Σ eta by the goal's type only between two terms no structural rule claims.";

/// Two neutrals at a type eta carries to a unit: a record of units, by its projections, and a function into a unit, by its application.
const CARRIED_TO_A_UNIT: &[&str] = &[
    "unit eta at a record of units, two neutrals",
    "unit eta at a function into a unit, two neutrals",
];

/// The one typed context that hands its two sides over as one stuck shape: two calls of a definition, which unfold to two stuck matches.
const UNFOLDED: &str = "a definition's argument";

/// Every row listed.
pub(super) fn parted() -> Vec<Parted> {
    let elaborator_refuses = Answers {
        elaborator: false,
        kernel: true,
    };
    let names = |contexts: &[Context]| {
        contexts
            .iter()
            .map(|context| context.name)
            .collect::<Vec<_>>()
    };
    let arms = names(ARMS);
    // A row's name, as the audit that states it spells it.
    let under = |seeds: &[&str], contexts: &[&str]| {
        seeds
            .iter()
            .flat_map(|seed| {
                contexts
                    .iter()
                    .map(move |context| format!("{seed} under {context}"))
            })
            .collect::<Vec<_>>()
    };

    let mut parted = Vec::new();
    let mut list = |audit, rows: Vec<String>, answers, finding| {
        parted.extend(rows.into_iter().map(|row| Parted {
            audit,
            row,
            answers,
            finding,
        }));
    };

    // Eta by the goal's type, ahead of structure. The kernel projects two sides at a record and applies two at a function whatever their shapes, and so reaches the unit that decides the goal. The elaborator compares two stuck matches arm against arm at `Type`, where two neutrals at such a type stay apart: each match is the literal there, and the two are not each other.
    list(
        Audit::Arms,
        under(CARRIED_TO_A_UNIT, &arms),
        elaborator_refuses,
        ETA_BY_TYPE,
    );
    list(
        Audit::Typed,
        under(CARRIED_TO_A_UNIT, &[UNFOLDED]),
        elaborator_refuses,
        ETA_BY_TYPE,
    );

    parted
}

/// Hold what `audit` found — each row a checker put on the other side than derived, with what each said — equal to the rows the table lists for it.
pub(super) fn hold_to_the_table(audit: Audit, found: &[(String, Answers)]) {
    let listed = parted()
        .into_iter()
        .filter(|parted| parted.audit == audit)
        .collect::<Vec<_>>();
    let mut failures = Vec::new();
    for (row, answers) in found {
        match listed.iter().find(|parted| parted.row == *row) {
            Some(parted) if parted.answers == *answers => {}
            Some(parted) => failures.push(format!(
                "`{row}` is listed as {}, and is now {answers}",
                parted.answers
            )),
            None => failures.push(format!("`{row}` is {answers}, and is not listed")),
        }
    }
    for parted in &listed {
        if !found.iter().any(|(row, _)| *row == parted.row) {
            failures.push(format!(
                "`{}` is listed under \"{}\", and both checkers now put it where its seed derives",
                parted.row, parted.finding
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
