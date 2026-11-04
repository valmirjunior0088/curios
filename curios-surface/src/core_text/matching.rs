//! A match, in the surface's spellings of its cases.
//!
//! **A match is `match head` and one arm per case, closed by `end`**: a boolean match's `| false` and `| true`, a dispatch's literal keys and its `| _`, a family's arms by their constructors' tags with their payloads' binders under their marks and its catch-all `| _`, and a fold's `| 0` and `| p + 1; ih`, `| []` and `| [h, ..t]; ih`, `| x[]` and `| x[h, ..t]; ih`. **A written motive is `: (binders) => motive` and builds the family it states**; a match with none states the goal it is checked against, `#{result = Ambient(T)}`. What a match's cases hold beside their arms is set in its `cases` field: the instance a family's match states, `#{cases = {stated = T}}`, and a list fold's element type, `#{cases = {carrier = {elem = T}}}`, which no arm writes.

use {
    super::{Decoration, nested},
    crate::{Frame, label_terms, plicity_mark, sub},
    curios_core::{Carrier, Cases, InductArm, Match, MatchResult, Scope, Term, Three},
    curios_print::{Printer, flat, indent, pure},
};

/// A match: its head and motive, then each arm, then `end`.
pub fn match_written(
    Match {
        head,
        result,
        cases,
    }: Match,
    frame: Frame,
) -> Printer {
    let mut fields = Vec::<Decoration>::new();
    let motive = match result {
        MatchResult::Family(motive) => motive_written(motive, frame),
        MatchResult::Ambient(goal) => {
            fields.push((
                "result",
                flat([pure("Ambient("), sub(goal, frame), pure(")")]),
            ));
            pure("")
        }
    };
    let mut held = Vec::<Decoration>::new();
    let arms = match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => vec![
            arm(pure("false"), false_case, frame),
            arm(pure("true"), true_case, frame),
        ],
        Cases::Switch { cases, default } => cases
            .into_iter()
            .map(|(key, body)| arm(pure(key.to_string()), body, frame))
            .chain([arm(pure("_"), default, frame)])
            .collect(),
        Cases::Induct {
            cases,
            default,
            stated,
        } => {
            if let Some(stated) = stated {
                held.push(("stated", sub(stated, frame)));
            }
            cases
                .into_iter()
                .map(|(tag, induct_arm)| constructor_arm(&tag.as_string(), induct_arm, frame))
                .chain(default.map(|default| arm(pure("_"), default, frame)))
                .collect()
        }
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                let ((predecessor, hypothesis), step) = frame.open_two(cons_case);
                vec![
                    arm(pure("0"), empty_case, frame),
                    arm(
                        pure(format!(
                            "{} + 1; {}",
                            frame.spelling.word(&predecessor),
                            frame.spelling.word(&hypothesis)
                        )),
                        step,
                        frame,
                    ),
                ]
            }
            Carrier::Bytes {
                empty_case,
                cons_case,
            } => vec![
                arm(pure("x[]"), empty_case, frame),
                cons_arm("x[", cons_case, frame),
            ],
            Carrier::List {
                elem,
                empty_case,
                cons_case,
            } => {
                held.push(("carrier", nested(vec![("elem", sub(elem, frame))])));
                vec![
                    arm(pure("[]"), empty_case, frame),
                    cons_arm("[", cons_case, frame),
                ]
            }
        },
    };
    if !held.is_empty() {
        fields.push(("cases", nested(held)));
    }
    let elimination = flat([
        pure("match "),
        sub(head, frame),
        motive,
        flat(arms),
        pure("\nend"),
    ]);

    frame.decorated(fields, elimination, true)
}

/// A written motive: its binders, then the family it states.
fn motive_written(motive: Scope<curios_core::Many>, frame: Frame) -> Printer {
    let labels = frame.labels(motive.label_iter());
    let terms = label_terms(&labels);
    let opened = motive.open(&terms.iter().collect::<Vec<_>>());
    let binders = labels
        .iter()
        .map(|label| frame.spelling.word(label))
        .collect::<Vec<_>>()
        .join(", ");

    flat([pure(format!(": ({binders}) => ")), sub(opened, frame)])
}

/// One arm: its pattern, then its body indented under it.
fn arm(pattern: Printer, body: Term, frame: Frame) -> Printer {
    flat([
        pure("\n| "),
        pattern,
        pure(" =>\n"),
        indent(sub(body, frame)),
    ])
}

/// A family's arm: its constructor's tag over its payload's binders, each under its mark.
fn constructor_arm(tag: &str, induct_arm: InductArm, frame: Frame) -> Printer {
    let labels = frame.labels(induct_arm.label_iter());
    let terms = label_terms(&labels);
    let binders = labels
        .iter()
        .enumerate()
        .map(|(index, label)| {
            let mark = plicity_mark(induct_arm.plicities().get(index));
            format!("{mark}{}", frame.spelling.word(label))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let body = induct_arm.open(&terms.iter().collect::<Vec<_>>());

    arm(pure(format!("{tag}({binders})")), body, frame)
}

/// A packed or list fold's step: its head and tail under the bracket that opens it, and its hypothesis after `;`.
fn cons_arm(open: &str, cons_case: Scope<Three>, frame: Frame) -> Printer {
    let ((head, tail, hypothesis), step) = frame.open_three(cons_case);
    let pattern = format!(
        "{open}{}, ..{}]; {}",
        frame.spelling.word(&head),
        frame.spelling.word(&tail),
        frame.spelling.word(&hypothesis)
    );

    arm(pure(pattern), step, frame)
}
