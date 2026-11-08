//! A match, in the surface's spellings of its cases.
//!
//! **A match is `match head` and one arm per case, closed by `end`**: a boolean match's `| false` and `| true`, a dispatch's literal keys and its `| _`, a family's arms by their constructors' tags with their payloads' binders under their marks and its catch-all `| _`, and a fold's `| 0` and `| p + 1; ih`, `| []` and `| [h, ..t]; ih`, `| x[]` and `| x[h, ..t]; ih`. **A written motive is `: (binders) => motive` and builds the family it states**; a match with none states the goal it is checked against, `#{result = Ambient(T)}`. What a match's cases hold beside their arms is set in its `cases` field: the instance a family's match states, `#{cases = {stated = T}}`, and a list fold's element type, `#{cases = {carrier = {elem = T}}}`, which no arm writes.

use {
    super::{
        Decoration, Fields, ReadError, Reader, Refusal, Tree, called, core_read, malformed, nested,
        tree, value, value_fields, value_term, value_variant, word,
    },
    crate::{
        BytesPattern, CoreText, Frame, FuncParam, ListPattern, MatchPattern, MatrixArm, NatLiteral,
        NatPattern, Pattern, Radix, label_terms, sub,
    },
    curios_common::Plicity,
    curios_core::{
        Atom, Carrier, Cases, Free, InductArm, Many, Match, MatchResult, Scope, Term, Three, Two,
    },
};

/// A match: its head and motive, then each arm.
pub fn match_written(
    Match {
        head,
        result,
        cases,
    }: Match,
    frame: Frame,
) -> Tree {
    let mut fields = Vec::<Decoration>::new();
    let motive = match result {
        MatchResult::Family(motive) => Some(motive_written(motive, frame)),
        MatchResult::Ambient(goal) => {
            fields.push((
                "result",
                value(called(word("Ambient"), vec![sub(goal, frame)])),
            ));
            None
        }
    };
    let mut held = Vec::<Decoration>::new();
    let arms = match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => vec![
            arm(MatchPattern::Bool(false), false_case, frame),
            arm(MatchPattern::Bool(true), true_case, frame),
        ],
        Cases::Switch { cases, default } => cases
            .into_iter()
            .map(|(key, body)| {
                arm(
                    MatchPattern::Nat(NatPattern::Lit(NatLiteral(key, Radix::Dec(0)))),
                    body,
                    frame,
                )
            })
            .chain([arm(binder("_"), default, frame)])
            .collect(),
        Cases::Induct {
            cases,
            default,
            stated,
        } => {
            if let Some(stated) = stated {
                held.push(("stated", value(sub(stated, frame))));
            }
            cases
                .into_iter()
                .map(|(tag, induct_arm)| constructor_arm(&tag.as_string(), induct_arm, frame))
                .chain(default.map(|default| arm(binder("_"), default, frame)))
                .collect()
        }
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                let ((predecessor, hypothesis), step) = frame.open_two(cons_case);
                vec![
                    arm(MatchPattern::Nat(NatPattern::Zero), empty_case, frame),
                    arm(
                        MatchPattern::Nat(NatPattern::Succ {
                            pred_label: crate::Label::from(frame.spelling.word(&predecessor)),
                            ih: Some(Pattern::Binder(crate::Label::from(
                                frame.spelling.word(&hypothesis),
                            ))),
                        }),
                        step,
                        frame,
                    ),
                ]
            }
            Carrier::Bytes {
                empty_case,
                cons_case,
            } => {
                let ((head, tail, hypothesis), step) = frame.open_three(cons_case);
                vec![
                    arm(MatchPattern::Bytes(BytesPattern::End), empty_case, frame),
                    arm(
                        MatchPattern::Bytes(BytesPattern::Atom {
                            head_label: crate::Label::from(frame.spelling.word(&head)),
                            tail_label: crate::Label::from(frame.spelling.word(&tail)),
                            ih: Some(Pattern::Binder(crate::Label::from(
                                frame.spelling.word(&hypothesis),
                            ))),
                        }),
                        step,
                        frame,
                    ),
                ]
            }
            Carrier::List {
                elem,
                empty_case,
                cons_case,
            } => {
                held.push(("carrier", nested(vec![("elem", value(sub(elem, frame)))])));
                let ((head, tail, hypothesis), step) = frame.open_three(cons_case);
                vec![
                    arm(MatchPattern::List(ListPattern::Nil), empty_case, frame),
                    arm(
                        MatchPattern::List(ListPattern::Cons {
                            head_label: crate::Label::from(frame.spelling.word(&head)),
                            tail_label: crate::Label::from(frame.spelling.word(&tail)),
                            ih: Some(Pattern::Binder(crate::Label::from(
                                frame.spelling.word(&hypothesis),
                            ))),
                        }),
                        step,
                        frame,
                    ),
                ]
            }
        },
    };
    if !held.is_empty() {
        fields.push(("cases", nested(held)));
    }
    let elimination = tree(crate::Subterm::Match(crate::Match {
        head: sub(head, frame),
        motive,
        arms,
    }));

    frame.decorated(fields, elimination)
}

/// A binder pattern by its word.
fn binder(word: &str) -> MatchPattern {
    MatchPattern::Binder(crate::Label::from(word))
}

/// A written motive: the function over its binders the family it states is.
fn motive_written(motive: Scope<curios_core::Many>, frame: Frame) -> Tree {
    let labels = frame.labels(motive.label_iter());
    let terms = label_terms(&labels);
    let opened = motive.open(&terms.iter().collect::<Vec<_>>());
    let params = labels
        .iter()
        .map(|label| FuncParam {
            plicity: Plicity::Explicit,
            pattern: Pattern::Binder(crate::Label::from(frame.spelling.word(label))),
            annotation: None,
        })
        .collect();

    tree(crate::Subterm::Func(crate::Func {
        params,
        body: sub(opened, frame),
    }))
}

/// One arm: its pattern, then its body.
fn arm(pattern: MatchPattern, body: Term, frame: Frame) -> MatrixArm<crate::Shown> {
    MatrixArm {
        pattern,
        body: sub(body, frame),
        span: None,
    }
}

/// A family's arm: its constructor's tag over its payload's binders, each under its mark.
fn constructor_arm(tag: &str, induct_arm: InductArm, frame: Frame) -> MatrixArm<crate::Shown> {
    let labels = frame.labels(induct_arm.label_iter());
    let terms = label_terms(&labels);
    let args = labels
        .iter()
        .enumerate()
        .map(|(index, label)| {
            let mark = induct_arm
                .plicities()
                .get(index)
                .copied()
                .unwrap_or(Plicity::Explicit);
            (mark, binder(&frame.spelling.word(label)))
        })
        .collect();
    let body = induct_arm.open(&terms.iter().collect::<Vec<_>>());

    arm(
        MatchPattern::Variant {
            tag: tag.to_string(),
            args,
        },
        body,
        frame,
    )
}

/// A match: its head, its motive where one is written, and its cases, read off the spelling its arms are written in — a boolean match's `false` and `true`, a dispatch's keys and its `_`, a family's tags and its `_`, a fold's `0` and `p + 1; ih`, `[]` and `[h, ..t]; ih`, `x[]` and `x[h, ..t]; ih`. A match with no motive states the goal it is checked against, `#{result = Ambient(T)}`, and what its cases hold beside their arms is set in `cases`.
pub fn match_read(
    crate::Match { head, motive, arms }: &crate::Match<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let span = head.span();
    let refused = |what: &'static str| ReadError::at(Refusal::Malformed(what), span);
    let head_read = core_read(head, reader)?;
    let result = match (motive, fields.take("result")) {
        (Some(motive), None) => MatchResult::Family(motive_read(motive, reader)?),
        (None, Some(value)) => match value_variant(value)? {
            ("Ambient", held) => match held.as_slice() {
                [goal] => MatchResult::Ambient(core_read(goal, reader)?),
                _ => return Err(malformed("`Ambient(T)`", value)),
            },
            _ => return Err(malformed("`Ambient(T)`", value)),
        },
        (Some(_), Some(value)) => return Err(malformed("a motive or a goal, not both", value)),
        (None, None) => {
            return Err(refused(
                "a match's motive, `: (…) => P`, or its goal, `#{result = Ambient(T)}`",
            ));
        }
    };
    let (mut stated, mut carrier) = (None, None);
    if let Some(value) = fields.take("cases") {
        let mut held = value_fields(value)?;
        stated = held.take("stated").map(value_term).transpose()?;
        if let Some(carried) = held.take("carrier") {
            let mut carried = value_fields(carried)?;
            carrier = carried.take("elem").map(value_term).transpose()?;
            carried.finish()?;
        }
        held.finish()?;
    }
    let patterns = arms.iter().map(|arm| &arm.pattern).collect::<Vec<_>>();
    let cases = match patterns.as_slice() {
        [MatchPattern::Bool(false), MatchPattern::Bool(true)] => Cases::Bool {
            false_case: core_read(&arms[0].body, reader)?,
            true_case: core_read(&arms[1].body, reader)?,
        },
        [
            MatchPattern::Nat(NatPattern::Zero),
            MatchPattern::Nat(NatPattern::Succ { pred_label, ih }),
        ] => {
            let hypothesis =
                binder_word(ih.as_ref()).ok_or_else(|| refused("a fold's hypothesis, `; ih`"))?;
            Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: core_read(&arms[0].body, reader)?,
                    cons_case: scope_read(
                        &[pred_label.as_str(), hypothesis],
                        &arms[1].body,
                        reader,
                    )
                    .map(|(frees, body)| Scope::close(Two, &[&frees[0], &frees[1]], body))?,
                },
            }
        }
        [
            MatchPattern::List(ListPattern::Nil),
            MatchPattern::List(ListPattern::Cons {
                head_label,
                tail_label,
                ih,
            }),
        ] => {
            let hypothesis =
                binder_word(ih.as_ref()).ok_or_else(|| refused("a fold's hypothesis, `; ih`"))?;
            let elem = carrier.take().ok_or_else(|| {
                refused("a list fold's element, `#{cases = {carrier = {elem = T}}}`")
            })?;
            Cases::FreeMonoid {
                carrier: Carrier::List {
                    elem: core_read(&elem, reader)?,
                    empty_case: core_read(&arms[0].body, reader)?,
                    cons_case: three_read(
                        [head_label.as_str(), tail_label.as_str(), hypothesis],
                        &arms[1].body,
                        reader,
                    )?,
                },
            }
        }
        [
            MatchPattern::Bytes(BytesPattern::End),
            MatchPattern::Bytes(BytesPattern::Atom {
                head_label,
                tail_label,
                ih,
            }),
        ] => {
            let hypothesis =
                binder_word(ih.as_ref()).ok_or_else(|| refused("a fold's hypothesis, `; ih`"))?;
            Cases::FreeMonoid {
                carrier: Carrier::Bytes {
                    empty_case: core_read(&arms[0].body, reader)?,
                    cons_case: three_read(
                        [head_label.as_str(), tail_label.as_str(), hypothesis],
                        &arms[1].body,
                        reader,
                    )?,
                },
            }
        }
        [.., MatchPattern::Binder(default)]
            if default.as_str() == "_"
                && patterns[..patterns.len() - 1].iter().all(|pattern| {
                    matches!(
                        pattern,
                        MatchPattern::Nat(NatPattern::Zero | NatPattern::Lit(_))
                    )
                })
                && patterns.len() > 1 =>
        {
            let (keyed, default) = arms.split_at(arms.len() - 1);
            Cases::Switch {
                cases: keyed
                    .iter()
                    .map(|arm| {
                        let key = match &arm.pattern {
                            MatchPattern::Nat(NatPattern::Lit(literal)) => literal.0.clone(),
                            _ => curios_num::Natural::zero(),
                        };
                        Ok((key, core_read(&arm.body, reader)?))
                    })
                    .collect::<Result<_, ReadError>>()?,
                default: core_read(&default[0].body, reader)?,
            }
        }
        _ => {
            let mut cases = Vec::new();
            let mut default = None;
            for (index, arm) in arms.iter().enumerate() {
                match &arm.pattern {
                    MatchPattern::Variant { tag, args } if default.is_none() => {
                        let mut words = Vec::with_capacity(args.len());
                        let mut marks = Vec::with_capacity(args.len());
                        for (mark, arg) in args {
                            let MatchPattern::Binder(word) = arg else {
                                return Err(refused(
                                    "a constructor's arm over its payload's binders",
                                ));
                            };
                            words.push(word.as_str());
                            marks.push(*mark);
                        }
                        let (frees, body) = scope_read(&words, &arm.body, reader)?;
                        let binders = frees.iter().collect::<Vec<_>>();
                        cases.push((
                            Atom::from(tag.as_str()),
                            InductArm::new(
                                Scope::close(Many(binders.len()), &binders, body),
                                marks,
                            ),
                        ));
                    }
                    MatchPattern::Binder(word)
                        if word.as_str() == "_" && index + 1 == arms.len() =>
                    {
                        default = Some(core_read(&arm.body, reader)?);
                    }
                    _ => return Err(refused("an arm in a spelling of a match Core holds")),
                }
            }
            Cases::Induct {
                cases,
                default,
                stated: stated
                    .take()
                    .map(|stated| core_read(&stated, reader))
                    .transpose()?,
            }
        }
    };
    if stated.is_some() || carrier.is_some() {
        return Err(refused("`cases` fields this match's cases hold"));
    }

    Ok(Term::from(curios_core::Subterm::Match(Match {
        head: head_read,
        result,
        cases,
    })))
}

/// A written motive, `(i, x) => P`: its binders, which state no domain, over the family it states.
fn motive_read(
    motive: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Scope<Many>, ReadError> {
    let refused = || {
        ReadError::at(
            Refusal::Malformed("a motive, `(binders) => P`"),
            motive.span(),
        )
    };
    let crate::Subterm::Func(crate::Func { params, body }) = motive.as_subterm() else {
        return Err(refused());
    };
    let words = params
        .iter()
        .map(|param| match (&param.pattern, &param.annotation) {
            (Pattern::Binder(word), None) => Ok(word.as_str()),
            _ => Err(refused()),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let (frees, body) = scope_read(&words, body, reader)?;
    let binders = frees.iter().collect::<Vec<_>>();

    Ok(Scope::close(Many(binders.len()), &binders, body))
}

/// The word a fold's hypothesis binds, where it is one.
fn binder_word(pattern: Option<&crate::Pattern>) -> Option<&str> {
    match pattern? {
        Pattern::Binder(word) => Some(word.as_str()),
        Pattern::Tuple(_) | Pattern::Struct { .. } => None,
    }
}

/// `body` read under binders for `words`, with the locals they were opened at.
fn scope_read(
    words: &[&str],
    body: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<(Vec<Free>, Term), ReadError> {
    reader.binding(words, |reader, frees| {
        Ok((frees.to_vec(), core_read(body, reader)?))
    })
}

/// A fold's step over its head, its tail and its hypothesis.
fn three_read(
    words: [&str; 3],
    body: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Scope<Three>, ReadError> {
    let (frees, body) = scope_read(&words, body, reader)?;

    Ok(Scope::close(
        Three,
        &[&frees[0], &frees[1], &frees[2]],
        body,
    ))
}
