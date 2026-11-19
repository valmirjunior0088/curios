//! A match, in the surface's spellings of its cases.
//!
//! **A match is `match head` and one arm per case, closed by `end`**: a boolean match's `| false` and `| true`, a dispatch's literal keys and its `| _`, a family's arms by their constructors' tags with their payloads' binders under their marks and its catch-all `| _`, and a case split's `| 0` and `| p + 1`, `| []` and `| [h, ..t]`, `| x[]` and `| x[h, ..t]`. **A match states the goal its arms inhabit, `#{goal = T}`.** What a match's cases hold beside their arms is set in its `cases` field: the instance a family's match states, `#{cases = {stated = T}}`, and a list split's element type, `#{cases = {carrier = {elem = T}}}`, which no arm writes.

use {
    super::{
        Decoration, Fields, LabelRead, ReadError, Reader, Refusal, core_read, nested,
        stated_proofs, tree, value, value_fields, value_term,
    },
    crate::{
        Binder, CoreText, DecoratorField, Elements, Frame, Label, MatchPattern, MatrixArm,
        NatLiteral, NumLit, Radix, Rendered, label_terms, natural_pattern, sub,
    },
    curios_common::Plicity,
    curios_core::{
        Arity, Atom, Carrier, Cases, Free, InductArm, Many, Match, One, Scope, Term, Two,
    },
    curios_num::Natural,
    std::{array, iter},
};

/// A match: its head and goal, then each arm.
pub fn match_written<D: Rendered>(
    Match { head, goal, cases }: Match,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    // Each part written in the order the reader meets it, the head first, so a node a part shares with a later one is named where the reader reads it.
    let head = sub(head, frame);
    let mut fields = Vec::<Decoration<D>>::new();
    fields.push(("goal", value(sub(goal, frame))));
    let mut held = Vec::<Decoration<D>>::new();
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
            .map(|(key, body)| arm(natural_pattern(key), body, frame))
            .chain(iter::once_with(|| {
                arm(binder(Vec::new(), "_"), default, frame)
            }))
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
                .chain(
                    default
                        .into_iter()
                        .map(|default| arm(binder(Vec::new(), "_"), default, frame)),
                )
                .collect()
        }
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                let [predecessor_core] = cores(cons_case.label_iter(), frame);
                let (predecessor, step) = frame.open_one(cons_case);
                vec![
                    arm(natural_pattern(Natural::zero()), empty_case, frame),
                    arm(
                        MatchPattern::Successor {
                            pred_label: decorated(predecessor_core, &predecessor, frame),
                            step: NatLiteral(Natural::one(), Radix::Dec(0)),
                        },
                        step,
                        frame,
                    ),
                ]
            }
            Carrier::Bytes {
                empty_case,
                cons_case,
            } => {
                let [head_core, tail_core] = cores(cons_case.label_iter(), frame);
                let ((head, tail), step) = frame.open_two(cons_case);
                vec![
                    arm(
                        MatchPattern::Bytes(Elements::Closed(Vec::new())),
                        empty_case,
                        frame,
                    ),
                    arm(
                        MatchPattern::Bytes(Elements::cons(
                            decorated(head_core, &head, frame),
                            decorated(tail_core, &tail, frame),
                        )),
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
                let [head_core, tail_core] = cores(cons_case.label_iter(), frame);
                let ((head, tail), step) = frame.open_two(cons_case);
                vec![
                    arm(
                        MatchPattern::List(Elements::Closed(Vec::new())),
                        empty_case,
                        frame,
                    ),
                    arm(
                        MatchPattern::List(Elements::cons(
                            decorated(head_core, &head, frame),
                            decorated(tail_core, &tail, frame),
                        )),
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
    let elimination = tree(crate::Subterm::Match(crate::Match { head, arms }));

    frame.decorated(fields, elimination)
}

/// A binder pattern by its word, under what the decorator states of its label.
fn binder<D: Rendered>(core: Vec<DecoratorField<D>>, word: &str) -> MatchPattern<D> {
    MatchPattern::Binder(Binder {
        core,
        label: Label::from(word),
    })
}

/// The binder a render opened at `local`, by its word, under what the decorator states of its label.
fn decorated<D: Rendered>(
    core: Vec<DecoratorField<D>>,
    local: &Free,
    frame: Frame<'_, D>,
) -> Binder<D> {
    Binder {
        core,
        label: Label::from(frame.spelling.word(local)),
    }
}

/// What a whole write states of each binder of a scope of `N` binders beside its word, in order.
fn cores<'l, D: Rendered, const N: usize>(
    labels: impl Iterator<Item = Option<&'l curios_core::Label>>,
    frame: Frame<'_, D>,
) -> [Vec<DecoratorField<D>>; N] {
    let mut labels = labels.map(|label| frame.label_fields(label));
    array::from_fn(|_| labels.next().unwrap_or_default())
}

/// One arm: its pattern, then its body.
fn arm<D: Rendered>(pattern: MatchPattern<D>, body: Term, frame: Frame<'_, D>) -> MatrixArm<D> {
    MatrixArm::of(pattern, sub(body, frame))
}

/// A family's arm: its constructor's tag over its payload's binders, each under its mark.
fn constructor_arm<D: Rendered>(
    tag: &str,
    induct_arm: InductArm,
    frame: Frame<'_, D>,
) -> MatrixArm<D> {
    let mut cores = induct_arm
        .label_iter()
        .map(|label| frame.label_fields(label))
        .collect::<Vec<_>>()
        .into_iter();
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
            (
                mark,
                binder(
                    cores.next().unwrap_or_default(),
                    &frame.spelling.word(label),
                ),
            )
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

/// A match: its head, its goal, and its cases, read off the spelling its arms are written in — a boolean match's `false` and `true`, a dispatch's keys and its `_`, a family's tags and its `_`, a case split's `0` and `p + 1`, `[]` and `[h, ..t]`, `x[]` and `x[h, ..t]`. A match states the goal its arms inhabit, `#{goal = T}`, and what its cases hold beside their arms is set in `cases`.
pub fn match_read(
    crate::Match { head, arms }: &crate::Match<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let span = head.span();
    let refused = |what: &'static str| ReadError::at(Refusal::Malformed(what), span);
    // An elimination's arms are its cases; a guard stands only in a match before its patterns are compiled, `#{node = Matrix}`.
    if arms.iter().any(|arm| !arm.guards.is_empty()) {
        return Err(refused("an elimination's arm, which writes no guard"));
    }
    let head_read = core_read(head, reader)?;
    let goal = match fields.take("goal") {
        Some(value) => core_read(&value_term(value)?, reader)?,
        None => return Err(refused("a match's goal, `#{goal = T}`")),
    };
    let (mut stated, mut carrier) = (None, None);
    if let Some(value) = fields.take("cases") {
        let mut held = value_fields(value)?;
        // Read where it is taken, before the arms, the order the writer meets it in.
        stated = held
            .take("stated")
            .map(|stated| core_read(&value_term(stated)?, reader))
            .transpose()?;
        if let Some(carried) = held.take("carrier") {
            let mut carried = value_fields(carried)?;
            carrier = carried.take("elem").map(value_term).transpose()?;
            carried.finish()?;
        }
        held.finish()?;
    }
    let patterns = arms
        .iter()
        .map(|arm| match arm.alternatives.as_slice() {
            [alternative] => Ok(&alternative.pattern),
            _ => Err(refused("an arm of one pattern")),
        })
        .collect::<Result<Vec<_>, ReadError>>()?;
    let cases = match patterns.as_slice() {
        [MatchPattern::Bool(false), MatchPattern::Bool(true)] => Cases::Bool {
            false_case: core_read(&arms[0].body, reader)?,
            true_case: core_read(&arms[1].body, reader)?,
        },
        [
            MatchPattern::Numeral(zero),
            MatchPattern::Successor { pred_label, step },
        ] if natural_key(zero).is_some_and(|zero| zero.is_zero())
            && step.0 == Natural::one() =>
        {
            let labels = [LabelRead::binder(pred_label)?];
            Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: core_read(&arms[0].body, reader)?,
                    cons_case: scope_read(&labels, &arms[1].body, reader).map(
                        |(frees, body)| {
                            proving_scope(Scope::close(One, &[&frees[0]], body), &labels)
                        },
                    )?,
                },
            }
        }
        [
            MatchPattern::List(Elements::Closed(empty)),
            MatchPattern::List(cons),
        ] if empty.is_empty() && cons.as_cons().is_some() => {
            let (head_label, tail_label) = cons.as_cons().expect("the arm is a cons, as read");
            let labels = [
                LabelRead::binder(head_label)?,
                LabelRead::binder(tail_label)?,
            ];
            let elem = carrier.take().ok_or_else(|| {
                refused("a list split's element, `#{cases = {carrier = {elem = T}}}`")
            })?;
            Cases::FreeMonoid {
                carrier: Carrier::List {
                    elem: core_read(&elem, reader)?,
                    empty_case: core_read(&arms[0].body, reader)?,
                    cons_case: two_read(&labels, &arms[1].body, reader)?,
                },
            }
        }
        [
            MatchPattern::Bytes(Elements::Closed(empty)),
            MatchPattern::Bytes(cons),
        ] if empty.is_empty() && cons.as_cons().is_some() => {
            let (head_label, tail_label) = cons.as_cons().expect("the arm is a cons, as read");
            let labels = [
                LabelRead::binder(head_label)?,
                LabelRead::binder(tail_label)?,
            ];
            Cases::FreeMonoid {
                carrier: Carrier::Bytes {
                    empty_case: core_read(&arms[0].body, reader)?,
                    cons_case: two_read(&labels, &arms[1].body, reader)?,
                },
            }
        }
        [.., MatchPattern::Binder(default)]
            if is_bare_wildcard(default)
                && patterns[..patterns.len() - 1].iter().all(|pattern| {
                    matches!(pattern, MatchPattern::Numeral(numeral) if natural_key(numeral).is_some())
                })
                && patterns.len() > 1 =>
        {
            let (keyed, default) = arms.split_at(arms.len() - 1);
            Cases::Switch {
                cases: keyed
                    .iter()
                    .zip(&patterns)
                    .map(|(arm, pattern)| {
                        let MatchPattern::Numeral(numeral) = pattern else {
                            unreachable!("every arm before a dispatch's catch-all is a key");
                        };
                        let key = natural_key(numeral).expect("a dispatch's key is a natural");
                        Ok((key, core_read(&arm.body, reader)?))
                    })
                    .collect::<Result<_, ReadError>>()?,
                default: core_read(&default[0].body, reader)?,
            }
        }
        _ => {
            let mut cases = Vec::new();
            let mut default = None;
            for (index, (arm, pattern)) in arms.iter().zip(&patterns).enumerate() {
                match pattern {
                    MatchPattern::Variant { tag, args } if default.is_none() => {
                        let mut labels = Vec::with_capacity(args.len());
                        let mut marks = Vec::with_capacity(args.len());
                        for (mark, arg) in args {
                            let MatchPattern::Binder(binder) = arg else {
                                return Err(refused(
                                    "a constructor's arm over its payload's binders",
                                ));
                            };
                            labels.push(LabelRead::binder(binder)?);
                            marks.push(*mark);
                        }
                        let body = many_read(&labels, &arm.body, reader)?;
                        cases.push((Atom::from(tag.as_str()), InductArm::new(body, marks)));
                    }
                    MatchPattern::Binder(binder)
                        if is_bare_wildcard(binder) && index + 1 == arms.len() =>
                    {
                        default = Some(core_read(&arm.body, reader)?);
                    }
                    _ => return Err(refused("an arm in a spelling of a match Core holds")),
                }
            }
            Cases::Induct {
                cases,
                default,
                stated: stated.take(),
            }
        }
    };
    if stated.is_some() || carrier.is_some() {
        return Err(refused("`cases` fields this match's cases hold"));
    }

    Ok(Term::from(curios_core::Subterm::Match(Match {
        head: head_read,
        goal,
        cases,
    })))
}

/// The natural a key spelled as a numeral is: an unsigned numeral's value, and none for a signed one, which is no natural's.
fn natural_key(numeral: &NumLit) -> Option<Natural> {
    (!numeral.sign.is_marked()).then(|| numeral.magnitude.clone())
}

/// Whether `binder` is a default arm's `_` under no decorator: a default binds nothing in Core, so nothing is stated of its label.
fn is_bare_wildcard(binder: &Binder<CoreText>) -> bool {
    binder.as_str() == "_" && binder.core.is_empty()
}

/// `body` read under binders for `labels`, with the locals they were opened at.
fn scope_read(
    labels: &[LabelRead<'_>],
    body: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<(Vec<Free>, Term), ReadError> {
    reader.binding(labels, |reader, frees| {
        Ok((frees.to_vec(), core_read(body, reader)?))
    })
}

/// `scope`, read over `labels`, its binders stating whether each binds a proof where a label states one does.
fn proving_scope<A: Arity>(scope: Scope<A>, labels: &[LabelRead<'_>]) -> Scope<A> {
    match stated_proofs(labels) {
        Some(proofs) => scope.with_proofs(&proofs),
        None => scope,
    }
}

/// `body` read under binders for `labels`, closed over them however many they are.
fn many_read(
    labels: &[LabelRead<'_>],
    body: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Scope<Many>, ReadError> {
    let (frees, body) = scope_read(labels, body, reader)?;
    let binders = frees.iter().collect::<Vec<_>>();

    Ok(proving_scope(
        Scope::close(Many(binders.len()), &binders, body),
        labels,
    ))
}

/// A sequence split's cons arm over its head and its tail.
fn two_read(
    labels: &[LabelRead<'_>],
    body: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Scope<Two>, ReadError> {
    let (frees, body) = scope_read(labels, body, reader)?;

    Ok(proving_scope(
        Scope::close(Two, &[&frees[0], &frees[1]], body),
        labels,
    ))
}
