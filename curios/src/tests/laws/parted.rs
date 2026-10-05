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

/// One row a checker puts on the other side than derived.
pub(super) struct Parted {
    pub(super) audit: Audit,
    pub(super) row: String,
    pub(super) answers: Answers,
    /// The finding, by its heading in `documentation/roadmap/01-soundness/00-findings.md`.
    pub(super) finding: &'static str,
}

const POSED_AS_TWO_CALLS: &str = "Two calls of one definition are compared by their spines only where the pair reaches a checker as two calls.";

/// Two calls of one definition that differ in a proof its body eliminates: at `Nat`, applied once more, and at a function and a record type.
const THROUGH: &str = "irrelevance through a definition";
const CURRIED: &str = "irrelevance through a definition's curried call";
const INTO_A_FUNCTION: &str = "irrelevance through a definition, at a function type";
const INTO_A_RECORD: &str = "irrelevance through a definition, at a record type";

/// Every row listed.
pub(super) fn parted() -> Vec<Parted> {
    let elaborator_refuses = Answers {
        elaborator: false,
        kernel: true,
    };
    let kernel_refuses = Answers {
        elaborator: true,
        kernel: false,
    };
    let both_refuse = Answers::both(false);
    let every = [THROUGH, CURRIED, INTO_A_FUNCTION, INTO_A_RECORD];
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

    // Two calls of one definition converge by their spines where the pair is posed as two calls. Wherever reduction reaches a call first it unfolds, the two proofs become a stuck elimination's scrutinees, and a scrutinee is compared at `Type`, where nothing says they are proofs of one proposition.
    //
    // The kernel alone: its eta by the goal's type applies a lambda and projects a tuple literal, and forces what that yields through the call; the elaborator opens the literal and still meets two calls.
    list(
        Audit::Typed,
        under(&every, &["a lambda's body", "a tuple's component"]),
        kernel_refuses,
        POSED_AS_TWO_CALLS,
    );
    // Both: reducing what the call sits in unfolds it before the two are paired.
    list(
        Audit::Typed,
        under(&every, &["a let's value", "a list's element"]),
        both_refuse,
        POSED_AS_TWO_CALLS,
    );
    list(
        Audit::Heads,
        under(
            &[THROUGH, CURRIED],
            &["a match's scrutinee", "an operation's operand"],
        ),
        both_refuse,
        POSED_AS_TWO_CALLS,
    );
    list(
        Audit::Heads,
        under(&[INTO_A_RECORD], &["a projection's head"]),
        both_refuse,
        POSED_AS_TWO_CALLS,
    );
    // The elaborator alone: read by a proof by reflexivity, whose implicit the solver commits as the first call's reduct, where the kernel, put the claim with the calls as written, compares their curried spines.
    list(
        Audit::Heads,
        under(&[INTO_A_FUNCTION], &["an application's head"]),
        elaborator_refuses,
        POSED_AS_TWO_CALLS,
    );
    list(
        Audit::Seeds,
        vec![CURRIED.to_owned()],
        elaborator_refuses,
        POSED_AS_TWO_CALLS,
    );
    list(
        Audit::Reversed,
        vec![format!("{CURRIED}, reversed")],
        elaborator_refuses,
        POSED_AS_TWO_CALLS,
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
