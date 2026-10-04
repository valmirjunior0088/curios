//! The rows where a law stops: each row a checker puts on the other side than its seed derives, with what each checker says and the finding that holds why.
//!
//! One table, held equal to what each audit finds: a row that starts parting fails its audit until it is listed, and a listed row that stops parting fails it until the entry is deleted, which is what the commit that closes its finding does. A row both checkers refuse is no disagreement between them and is listed all the same, since its seed derives that it holds.

use super::{ARMS, Answers, Context, TYPED, TYPES};

/// The audit that states a row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Audit {
    Seeds,
    Typed,
    Arms,
    Heads,
    Types,
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

const ETA_AT_TYPE: &str =
    "The kernel refuses Π and Σ eta under a child it compares at `Type`, and a program reaches it.";

/// The seeds eta decides by a literal's shape alone: a lambda that is no forwarder, and a tuple literal, each against a neutral.
const BY_THE_LITERAL: &[&str] = &["function eta past a fold", "record eta"];

const UNIT_LITERAL: &[&str] = &["unit eta, the literal"];

/// Two neutrals at a type with one inhabitant, which nothing but the type equates.
const UNIT_NEUTRALS: &[&str] = &[
    "unit eta, two neutrals",
    "unit eta at a struct, two neutrals",
];

/// The typed context whose hole the kernel reaches untyped all the same: comparing two calls of a definition at a record, it projects them before it compares their spines, and unfolded they are stuck matches whose arms it compares at `Type`.
const UNFOLDED: &str = "a definition's argument";

/// Every row listed.
pub(super) fn parted() -> Vec<Parted> {
    let kernel_refuses = Answers {
        elaborator: true,
        kernel: false,
    };
    let both_refuse = Answers::both(false);
    let names = |contexts: &[Context]| {
        contexts
            .iter()
            .map(|context| context.name)
            .collect::<Vec<_>>()
    };
    let (typed, arms, types) = (names(TYPED), names(ARMS), names(TYPES));
    let folded = typed
        .iter()
        .copied()
        .filter(|context| *context != UNFOLDED)
        .collect::<Vec<_>>();

    let mut parted = Vec::new();
    let mut list = |audit, seeds: &[&str], contexts: Option<&[&str]>, answers, finding| {
        for seed in seeds {
            let rows = match contexts {
                None => vec![(*seed).to_owned()],
                Some(contexts) => contexts
                    .iter()
                    .map(|context| format!("{seed} under {context}"))
                    .collect(),
            };
            parted.extend(rows.into_iter().map(|row| Parted {
                audit,
                row,
                answers,
                finding,
            }));
        }
    };

    // Unit eta: the elaborator equates any two terms at a type with no fields, by the literal or by the type, and the kernel has no such rule. Where neither checker has the type — an arm, and a definition's argument once unfolded — two neutrals part from their seed in both.
    for units in [UNIT_LITERAL, UNIT_NEUTRALS] {
        list(Audit::Seeds, units, None, kernel_refuses, UNIT_ETA);
        list(Audit::Types, units, Some(&types), kernel_refuses, UNIT_ETA);
    }
    list(
        Audit::Typed,
        UNIT_LITERAL,
        Some(&typed),
        kernel_refuses,
        UNIT_ETA,
    );
    list(
        Audit::Arms,
        UNIT_LITERAL,
        Some(&arms),
        kernel_refuses,
        UNIT_ETA,
    );
    list(
        Audit::Typed,
        UNIT_NEUTRALS,
        Some(&folded),
        kernel_refuses,
        UNIT_ETA,
    );
    list(
        Audit::Typed,
        UNIT_NEUTRALS,
        Some(&[UNFOLDED]),
        both_refuse,
        UNIT_ETA,
    );
    list(
        Audit::Arms,
        UNIT_NEUTRALS,
        Some(&arms),
        both_refuse,
        UNIT_ETA,
    );

    // Eta by the literal: the elaborator fires it at any goal type, and the kernel only where the goal's type is the function or the record.
    list(
        Audit::Arms,
        BY_THE_LITERAL,
        Some(&arms),
        kernel_refuses,
        ETA_AT_TYPE,
    );
    list(
        Audit::Typed,
        BY_THE_LITERAL,
        Some(&[UNFOLDED]),
        kernel_refuses,
        ETA_AT_TYPE,
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
