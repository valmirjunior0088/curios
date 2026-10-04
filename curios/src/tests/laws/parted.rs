//! The rows where a law stops: each row a checker puts on the other side than its seed derives, with what each checker says and the finding that holds why.
//!
//! One table, held equal to what each audit finds: a row that starts parting fails its audit until it is listed, and a listed row that stops parting fails it until the entry is deleted, which is what the commit that closes its finding does. A row both checkers refuse is no disagreement between them and is listed all the same, since its seed derives that it holds.

use super::Answers;

/// The audit that states a row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Audit {
    Seeds,
}

/// One row a checker puts on the other side than derived.
pub(super) struct Parted {
    pub(super) audit: Audit,
    pub(super) row: String,
    pub(super) answers: Answers,
    /// The finding, by its heading in `documentation/roadmap/01-soundness/00-findings.md`.
    pub(super) finding: &'static str,
}

const UNIT_ETA: &str = "The two checkers part on unit eta, and a program reaches it.";

/// Every row listed.
pub(super) fn parted() -> Vec<Parted> {
    let elaborator_alone = Answers {
        elaborator: true,
        kernel: false,
    };
    [
        "unit eta, the literal",
        "unit eta, two neutrals",
        "unit eta at a struct, two neutrals",
    ]
    .into_iter()
    .map(|row| Parted {
        audit: Audit::Seeds,
        row: row.to_owned(),
        answers: elaborator_alone,
        finding: UNIT_ETA,
    })
    .collect()
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
