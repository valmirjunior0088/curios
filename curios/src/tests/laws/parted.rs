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

const UNIT_ETA: &str = "The two checkers part on unit eta, and a program reaches it.";

const ETA_AT_TYPE: &str =
    "The kernel refuses Π and Σ eta under a child it compares at `Type`, and a program reaches it.";

/// The seeds eta decides by a literal's shape alone: a lambda that is no forwarder, and a tuple literal, each against a neutral.
const BY_THE_LITERAL: &[&str] = &["function eta past a fold", "record eta"];

/// A literal of a type with one inhabitant against a neutral, which the elaborator takes by the literal's shape.
const UNIT_LITERALS: &[&str] = &[
    "unit eta, the literal",
    "unit eta at a record of units, the literal",
];

/// Two neutrals at a type with no fields, which nothing but the type equates.
const UNIT_NEUTRALS: &[&str] = &[
    "unit eta, two neutrals",
    "unit eta at a struct, two neutrals",
];

/// Two neutrals at a record of units, which the elaborator refuses wherever it meets them: it compares their projections at `Type`, where it compared the literal's fields at no type at all.
const RECORD_NEUTRALS: &[&str] = &["unit eta at a record of units, two neutrals"];

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
    // A row's name, as the audit that states it spells it.
    let alone = |seeds: &[&str], law: &str| {
        seeds
            .iter()
            .map(|seed| format!("{seed}{law}"))
            .collect::<Vec<_>>()
    };
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

    // Unit eta. The elaborator equates a literal of a type with one inhabitant with anything, by the literal's shape, and two neutrals at a type with no fields by the type; the kernel has neither rule. Two neutrals part from their seed in both checkers where neither has the type — an arm, and a definition's argument once unfolded — and at a record of units wherever they stand, which is where the elaborator's conversion is not transitive: each neutral is the literal, and the two are not each other.
    for (audit, law) in [(Audit::Seeds, ""), (Audit::Reversed, ", reversed")] {
        list(audit, alone(UNIT_LITERALS, law), kernel_refuses, UNIT_ETA);
        list(audit, alone(UNIT_NEUTRALS, law), kernel_refuses, UNIT_ETA);
        list(audit, alone(RECORD_NEUTRALS, law), both_refuse, UNIT_ETA);
    }
    list(
        Audit::Chained,
        vec![
            "unit eta, the literal chained with unit eta, two neutrals".to_owned(),
            "unit eta at a record of units, the literal chained with unit eta at a record of units, two neutrals".to_owned(),
        ],
        kernel_refuses,
        UNIT_ETA,
    );
    list(
        Audit::Substituted,
        alone(
            &["unit eta, two neutrals", RECORD_NEUTRALS[0]],
            ", at a compound term",
        ),
        kernel_refuses,
        UNIT_ETA,
    );
    for (audit, contexts) in [
        (Audit::Typed, &typed),
        (Audit::Arms, &arms),
        (Audit::Types, &types),
    ] {
        list(
            audit,
            under(UNIT_LITERALS, contexts),
            kernel_refuses,
            UNIT_ETA,
        );
        list(
            audit,
            under(RECORD_NEUTRALS, contexts),
            both_refuse,
            UNIT_ETA,
        );
    }
    list(
        Audit::Types,
        under(UNIT_NEUTRALS, &types),
        kernel_refuses,
        UNIT_ETA,
    );
    list(
        Audit::Typed,
        under(UNIT_NEUTRALS, &folded),
        kernel_refuses,
        UNIT_ETA,
    );
    list(
        Audit::Typed,
        under(UNIT_NEUTRALS, &[UNFOLDED]),
        both_refuse,
        UNIT_ETA,
    );
    list(
        Audit::Arms,
        under(UNIT_NEUTRALS, &arms),
        both_refuse,
        UNIT_ETA,
    );

    // Eta by the literal: the elaborator fires it at any goal type, and the kernel only where the goal's type is the function or the record.
    list(
        Audit::Arms,
        under(BY_THE_LITERAL, &arms),
        kernel_refuses,
        ETA_AT_TYPE,
    );
    list(
        Audit::Typed,
        under(BY_THE_LITERAL, &[UNFOLDED]),
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
