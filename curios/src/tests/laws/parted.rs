//! The rows where a law stops: each row a checker puts on the other side than its seed derives, with what each checker says and the finding that holds why.
//!
//! One table, held equal to what each audit finds: a row that starts parting fails its audit until it is listed, and a listed row that stops parting fails it until the entry is deleted, which is what the commit that closes its finding does. A row both checkers refuse is no disagreement between them and is listed all the same, since its seed derives that it holds.

use super::Answers;

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

/// One row a checker puts on the other side than derived: the audit that states it, its name as that audit spells it, what each checker says, and the finding that holds why, by its heading in `documentation/roadmap/01-soundness/00-findings.md`.
pub(super) type Parted = (Audit, String, Answers, &'static str);

/// Every row listed. There is none: both checkers put every row where its seed derives.
pub(super) fn parted() -> Vec<Parted> {
    Vec::new()
}

/// Hold what `audit` found — each row a checker put on the other side than derived, with what each said — equal to the rows the table lists for it.
pub(super) fn hold_to_the_table(audit: Audit, found: &[(String, Answers)]) {
    let listed = parted()
        .into_iter()
        .filter(|(stated_by, ..)| *stated_by == audit)
        .collect::<Vec<_>>();
    let mut failures = Vec::new();
    for (row, answers) in found {
        match listed.iter().find(|(_, listed, ..)| listed == row) {
            Some((_, _, listed, _)) if listed == answers => {}
            Some((_, _, listed, _)) => failures.push(format!(
                "`{row}` is listed as {listed}, and is now {answers}"
            )),
            None => failures.push(format!("`{row}` is {answers}, and is not listed")),
        }
    }
    for (_, row, _, finding) in &listed {
        if !found.iter().any(|(found, _)| found == row) {
            failures.push(format!(
                "`{row}` is listed under \"{finding}\", and both checkers now put it where its seed derives"
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
