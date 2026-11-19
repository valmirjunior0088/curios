//! The forms only elaboration builds, each in the surface spelling it was read from under the node it is.
//!
//! An operator is `#{node = Infix} (a + b)`, a numeral elaboration has not settled `#{node = NumLit} 3`, a `!` site the rest of its region called over the action's result, `#{node = Bang} (k)((action)!)`, an ascription `#{node = Ascribed, type_ = T} e`, the body a derived witness asks for `#{node = Derive} ?`, a name resolution has not settled `#{node = Name} /path`, a concept's method named by its path `#{node = Method} /Show/show`, and a match before its patterns are compiled `#{node = Matrix} match head | pattern => body … end`, each row's body the function over its pattern's binders it is and each place of the pattern written by the word that function gives its binder, a bare `_` a wildcard, which is no place, and `#{node = Binder} _` a nameless place, each row an arm of its own and `#{node = Matrix, arms = [2, 1]}` counting the rows of each arm where one holds more than one. A program writes none of them in Core text, which no elaborator reads; a witness writes one to see it refused. A reader is shown each as it was written, an operator with its operands parenthesized as a reader reads them, an ascription as the term and its type bracketed, and the derived body as `derive`.

use {
    super::{
        Decoration, Fields, ReadError, Reader, Refusal, bracketed, call, core_read,
        decoration_fields, numeral, term_global, tree, value, value_list, value_number, value_term,
        value_word, word, word_value,
    },
    crate::{
        Alternative, CoreText, Frame, MatchPattern, MatrixArm, NatPattern, ProofLiteral, Rendered,
        sub, written_pattern,
    },
    curios_common::{Plicity, Sign},
    curios_core::{
        Arm, Ascribed, Atom, Bang, Elements, Func, Global, Infix, Matrix, Method, NumLit, Pattern,
        Row, Slot, Subterm, Term, Transient,
    },
};

/// An elaboration-only form under the node it is.
pub fn transient_written<D: Rendered>(transient: Transient, frame: Frame<'_, D>) -> crate::Term<D> {
    let node = |name: &'static str| vec![("node", word_value(name))];
    if let Some(leave) = frame.reader() {
        match transient {
            Transient::Infix(Infix { op, left, right }) => {
                return tree(crate::Subterm::Infix(crate::Infix {
                    op,
                    left: sub(left, frame),
                    right: sub(right, frame),
                }));
            }
            // No surface form states a type beside a bare term, so a reader is shown the pair bracketed.
            Transient::Ascribed(Ascribed { term, type_ }) => {
                return tree(crate::Subterm::Core(D::ascribed(
                    leave,
                    sub(term, frame),
                    sub(type_, frame),
                )));
            }
            Transient::Derive => return tree(crate::Subterm::Derive),
            transient => return transient_written_as_core(transient, frame, node),
        }
    }

    transient_written_as_core(transient, frame, node)
}

/// [`transient_written`] for every form whose reader's spelling is its Core text's without the decorator.
fn transient_written_as_core<D: Rendered>(
    transient: Transient,
    frame: Frame<'_, D>,
    node: impl Fn(&'static str) -> Vec<Decoration<D>>,
) -> crate::Term<D> {
    match transient {
        Transient::Infix(Infix { op, left, right }) => frame.decorated(
            node("Infix"),
            tree(crate::Subterm::Infix(crate::Infix {
                op,
                left: sub(left, frame),
                right: sub(right, frame),
            })),
        ),
        Transient::NumLit(NumLit::Number { magnitude, sign }) => {
            frame.decorated(node("NumLit"), numeral(magnitude, sign))
        }
        Transient::NumLit(NumLit::Character(character)) => frame.decorated(
            node("NumLit"),
            tree(crate::Subterm::ProofLiteral(ProofLiteral::Char(
                character.into(),
            ))),
        ),
        // The rest of the region called over the action's result, as the `!` it is: `k(action!)`.
        Transient::Bang(Bang {
            action,
            continuation,
        }) => frame.decorated(
            node("Bang"),
            call(
                sub(continuation, frame),
                vec![crate::Argument {
                    term: tree(crate::Subterm::Bang(sub(action, frame))),
                    plicity: Plicity::Explicit,
                }],
            ),
        ),
        Transient::Ascribed(Ascribed { term, type_ }) => {
            let mut fields = node("Ascribed");
            fields.push(("type_", value(sub(type_, frame))));
            frame.decorated(fields, sub(term, frame))
        }
        Transient::Derive => frame.decorated(node("Derive"), tree(crate::Subterm::Goal)),
        Transient::Name(name) => frame.decorated(node("Name"), frame.global(&name)),
        Transient::Method(Method { concept, field }) => {
            let path = match concept.qualifier() {
                Some(concept) => frame
                    .spelling
                    .symbol(&Global::Authored(concept.with(&field))),
                None => format!("{}/{field}", frame.spelling.symbol(&concept)),
            };
            frame.decorated(node("Method"), word(path))
        }
        Transient::Matrix(matrix) => {
            let mut fields = node("Matrix");
            // An arm of one alternative is what the form builds, each arm Core text writes being one row.
            if matrix.arms.iter().any(|arm| arm.alternatives.len() > 1) {
                let counts = matrix
                    .arms
                    .iter()
                    .map(|arm| value(numeral(arm.alternatives.len(), Sign::Unmarked)))
                    .collect();
                fields.push(("arms", bracketed(counts)));
            }
            frame.decorated(fields, matrix_written(matrix, frame))
        }
    }
}

/// A match as it was written: its head, then each arm as its alternatives over the names their rows' bodies bind, and the guards and the body the first row's opening is — in Core text, each row an arm of its own, its pattern over places and each guard and its body as the function over its binders it is, each over the domain Core holds for it, the field `arms` saying how many rows each arm holds where one holds more than one. A reader is shown a match over `()` whose rows write an unnamed `_`, every one but the last guarded, as the match with no head it was lowered from.
fn matrix_written<D: Rendered>(
    Matrix { head, arms }: Matrix,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    let unheaded = matches!(&*head, Subterm::Tuple(tuple) if tuple.fields.is_empty());
    let arms = match frame.core_text() {
        true => arms
            .into_iter()
            .flat_map(|arm| arm.alternatives)
            .map(
                |Row {
                     pattern,
                     guards,
                     body,
                 }| {
                    let guards = guards
                        .into_iter()
                        .map(|guard| match guard {
                            curios_core::Guard::Holds(condition) => (None, sub(condition, frame)),
                            curios_core::Guard::Binds { pattern, value } => {
                                (Some(pattern), sub(value, frame))
                            }
                        })
                        .collect::<Vec<_>>();
                    let body = sub(body, frame);
                    let places = &mut row_places(&body).into_iter();

                    MatrixArm {
                        alternatives: vec![Alternative {
                            pattern: written_pattern(&pattern, places, place),
                            span: None,
                        }],
                        guards: guards
                            .into_iter()
                            .map(|(pattern, term)| match pattern {
                                None => crate::Guard::Holds(term),
                                Some(pattern) => crate::Guard::Binds {
                                    pattern: Box::new(Alternative {
                                        pattern: written_pattern(&pattern, places, place),
                                        span: None,
                                    }),
                                    value: term,
                                },
                            })
                            .collect(),
                        body,
                    }
                },
            )
            .collect::<Vec<_>>(),
        false => arms
            .into_iter()
            .map(|arm| {
                let mut first = None;
                let alternatives = arm
                    .alternatives
                    .into_iter()
                    .map(|row| {
                        let (pattern, guards, body) = row_read_back(row, frame);
                        first.get_or_insert((guards, body));

                        Alternative {
                            pattern,
                            span: None,
                        }
                    })
                    .collect::<Vec<_>>();
                let (guards, body) = first.expect("an arm writes at least one alternative");

                MatrixArm {
                    alternatives,
                    guards,
                    body,
                }
            })
            .collect::<Vec<_>>(),
    };

    if unheaded
        && !frame.core_text()
        && let Some(headless) = crate::HeadlessMatch::of_arms(&arms)
    {
        return tree(crate::Subterm::HeadlessMatch(headless));
    }

    tree(crate::Subterm::Match(crate::Match {
        head: sub(head, frame),
        arms,
    }))
}

/// A row as a reader is shown it: its pattern over the names its body binds, each guard over the names written before it, and the body under them all — every binder opened once, under one label, as `lambda_parameters` opens a lambda's, so a guard and the body read one name alike.
/// The words a row's places are written by in Core text: its function's as the function is written, each parameter's word without its decorator. A function written as no lambda, a node met again under sharing, states none, and the places are written nameless.
fn row_places<D: Rendered>(body: &crate::Term<D>) -> Vec<crate::Binder<D>> {
    match body.as_subterm() {
        crate::Subterm::Func(crate::Func { params, .. }) => params
            .iter()
            .map(|param| match &param.pattern {
                crate::Pattern::Binder(binder) => binder.label.clone().into(),
                crate::Pattern::Tuple(_) | crate::Pattern::Struct { .. } => "_".into(),
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// A pattern's own binder as Core text writes its place: by its word, and a nameless one under `#{node = Binder}`, since a bare `_` there is a wildcard.
fn place<D: Rendered>(binder: crate::Binder<D>) -> crate::Binder<D> {
    match binder.label.as_str() {
        "_" => crate::Binder {
            core: decoration_fields(vec![("node", word_value("Binder"))]),
            label: binder.label,
        },
        _ => binder,
    }
}

fn row_read_back<D: Rendered>(
    Row {
        pattern,
        guards,
        body,
    }: Row,
    frame: Frame<'_, D>,
) -> (crate::MatchPattern<D>, Vec<crate::Guard<D>>, crate::Term<D>) {
    let Subterm::Func(Func { telescope }) = &*body else {
        unreachable!("a row's body is a function over its binders");
    };
    let mut cursor = telescope.cursor();
    let mut opened = Vec::with_capacity(telescope.len());
    let mut names = Vec::with_capacity(telescope.len());
    while cursor.entry().is_some() {
        let label = frame.label(cursor.label());
        let core = frame.label_fields(cursor.label());
        // Asked before the cursor moves past the binder it asks about.
        let named = label.hint().is_some() || cursor.binder_used();
        names.push(crate::Binder {
            core,
            label: crate::Label::from(match named {
                true => frame.spelling.label(&label),
                false => "_".to_string(),
            }),
        });
        cursor.advance(Term::free_var(&label));
        opened.push(Term::free_var(&label));
    }
    let body = sub(cursor.body().expect("a cursor past every entry"), frame);

    let at = |function: &Term, place: usize| {
        let Subterm::Func(Func { telescope }) = &**function else {
            unreachable!("a row's guards are functions over the binders before them");
        };
        sub(
            telescope.open(&opened[..place].iter().collect::<Vec<_>>()),
            frame,
        )
    };
    let mut names = names.into_iter();
    let mut place = pattern.binders();
    let written = written_pattern(&pattern, &mut names, std::convert::identity);
    let guards = guards
        .iter()
        .map(|guard| match guard {
            curios_core::Guard::Holds(condition) => crate::Guard::Holds(at(condition, place)),
            curios_core::Guard::Binds { pattern, value } => {
                let value = at(value, place);
                place += pattern.binders();

                crate::Guard::Binds {
                    pattern: Box::new(Alternative {
                        pattern: written_pattern(pattern, &mut names, std::convert::identity),
                        span: None,
                    }),
                    value,
                }
            }
        })
        .collect();

    (written, guards, body)
}

/// An operator, `#{node = Infix} (a + b)`: its operands, each a term of Core text.
pub fn infix_read(
    operand: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let crate::Subterm::Infix(crate::Infix { op, left, right }) = operand.as_subterm() else {
        return Err(ReadError::at(
            Refusal::Malformed("an operator, `(a + b)`"),
            operand.span(),
        ));
    };

    Ok(Term::from(Subterm::Transient(Transient::Infix(Infix {
        op: *op,
        left: core_read(left, reader)?,
        right: core_read(right, reader)?,
    }))))
}

/// An elaboration-only form other than an operator under the node `node` names, in the spelling it was read from.
pub fn node_transient_read(
    node: &str,
    operand: &crate::Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let refused = |what: &'static str| ReadError::at(Refusal::Malformed(what), operand.span());
    let transient = match (node, operand.as_subterm()) {
        (
            "NumLit",
            crate::Subterm::NumLit(crate::NumLit {
                magnitude, sign, ..
            }),
        ) => Transient::NumLit(NumLit::Number {
            magnitude: magnitude.clone(),
            sign: *sign,
        }),
        ("NumLit", crate::Subterm::ProofLiteral(ProofLiteral::Char(character))) => {
            Transient::NumLit(NumLit::Character(character.value))
        }
        ("NumLit", _) => return Err(refused("a numeral or a character")),
        ("Bang", crate::Subterm::Apply(crate::Apply { head, arguments })) => {
            match arguments.as_slice() {
                [action] => match action.term.as_subterm() {
                    crate::Subterm::Bang(action) => Transient::Bang(Bang {
                        action: core_read(action, reader)?,
                        continuation: core_read(head, reader)?,
                    }),
                    _ => return Err(refused("a `!` site, `(k)((action)!)`")),
                },
                _ => return Err(refused("a `!` site, `(k)((action)!)`")),
            }
        }
        ("Bang", _) => return Err(refused("a `!` site, `(k)((action)!)`")),
        ("Ascribed", _) => {
            let type_ = fields
                .take("type_")
                .ok_or_else(|| refused("an ascription's type, `#{type_ = T}`"))?;
            Transient::Ascribed(Ascribed {
                term: core_read(operand, reader)?,
                type_: core_read(&value_term(type_)?, reader)?,
            })
        }
        ("Derive", crate::Subterm::Goal) => Transient::Derive,
        ("Derive", _) => return Err(refused("`?`, the body a derived witness asks for")),
        ("Name", _) => Transient::Name(
            term_global(operand).ok_or_else(|| refused("a name by its absolute path"))?,
        ),
        ("Method", crate::Subterm::Name(name)) if name.is_abs() && !name.is_single() => {
            Transient::Method(Method {
                concept: Global::Authored(name.qualifier().without_last()),
                field: name.last().to_string(),
            })
        }
        ("Method", _) => return Err(refused("a method by its path, `/C/field`")),
        ("Matrix", crate::Subterm::Match(crate::Match { head, arms })) => {
            let rows = arms
                .iter()
                .map(|arm| {
                    let [alternative] = arm.alternatives.as_slice() else {
                        return Err(refused(
                            "an arm of one pattern, the rows an arm holds stated by `arms`",
                        ));
                    };
                    let mut places = Vec::new();
                    let pattern = pattern_read(&alternative.pattern, operand, &mut places)?;
                    let mut over =
                        |term: &crate::Term<CoreText>, binders: usize, what: &'static str| {
                            let read = core_read(term, reader)?;
                            match &*read {
                                Subterm::Func(Func { telescope }) if telescope.len() == binders => {
                                    Ok(read)
                                }
                                _ => Err(refused(what)),
                            }
                        };
                    let mut binders = pattern.binders();
                    let guards = arm
                        .guards
                        .iter()
                        .map(|guard| match guard {
                            crate::Guard::Holds(condition) => Ok(curios_core::Guard::Holds(over(
                                condition,
                                binders,
                                "a guard, the function over the binders written before it",
                            )?)),
                            crate::Guard::Binds { pattern, value } => {
                                let value = over(
                                    value,
                                    binders,
                                    "a guard, the function over the binders written before it",
                                )?;
                                let pattern = pattern_read(&pattern.pattern, operand, &mut places)?;
                                binders += pattern.binders();

                                Ok(curios_core::Guard::Binds { pattern, value })
                            }
                        })
                        .collect::<Result<Vec<_>, ReadError>>()?;
                    let body = over(
                        &arm.body,
                        binders,
                        "a row's body, the function over its pattern's and its guards' binders",
                    )?;
                    if function_words(&arm.body).is_some_and(|words| words != places) {
                        return Err(refused(
                            "a row's places, each written as the word the row's function gives its binder",
                        ));
                    }

                    Ok(Row {
                        pattern,
                        guards,
                        body,
                    })
                })
                .collect::<Result<Vec<_>, ReadError>>()?;
            let counts = match fields.take("arms") {
                Some(counts) => value_list(counts)?
                    .iter()
                    .map(|count| match value_number(count)? {
                        0 => Err(refused("an arm of at least one row")),
                        count => usize::try_from(count)
                            .map_err(|_| refused("an arm of at least one row")),
                    })
                    .collect::<Result<Vec<_>, ReadError>>()?,
                None => vec![1; rows.len()],
            };
            if counts.iter().sum::<usize>() != rows.len() {
                return Err(refused("as many rows as `arms` counts"));
            }
            let mut rows = rows.into_iter();
            let arms = counts
                .into_iter()
                .map(|count| Arm {
                    alternatives: rows.by_ref().take(count).collect(),
                })
                .collect();

            Transient::Matrix(Matrix {
                head: core_read(head, reader)?,
                arms,
            })
        }
        ("Matrix", _) => return Err(refused("a match")),
        _ => unreachable!("the node names an elaboration-only form"),
    };

    Ok(Term::from(Subterm::Transient(transient)))
}

/// A row's pattern as Core holds it, each binder a place among the row's whose word `places` gathers in the order the pattern writes them, and a bare `_` a wildcard, which is no place.
fn pattern_read(
    pattern: &MatchPattern<CoreText>,
    at: &crate::Term<CoreText>,
    places: &mut Vec<String>,
) -> Result<Pattern, ReadError> {
    Ok(match pattern {
        MatchPattern::Binder(binder) => match own_place_read(binder, at)? {
            Some(word) => {
                places.push(word);

                Pattern::Binder
            }
            None => Pattern::WILDCARD,
        },
        MatchPattern::Variant { tag, args } => Pattern::Variant {
            tag: Atom::from(tag.as_str()),
            args: args
                .iter()
                .map(|(mark, arg)| Ok((*mark, pattern_read(arg, at, places)?)))
                .collect::<Result<_, ReadError>>()?,
        },
        MatchPattern::Tuple(fields) => Pattern::Tuple(
            fields
                .iter()
                .map(|field| Ok((field.label.clone(), pattern_read(&field.value, at, places)?)))
                .collect::<Result<_, ReadError>>()?,
        ),
        MatchPattern::Struct { .. } => return Err(unresolved(at)),
        MatchPattern::Bool(value) => Pattern::Bool(*value),
        MatchPattern::Char(character) => Pattern::Character(character.value),
        MatchPattern::Str(literal) => Pattern::Str(literal.value.clone()),
        MatchPattern::Nat(NatPattern::Zero) => Pattern::Zero,
        MatchPattern::Nat(NatPattern::Succ { pred_label, step }) => Pattern::Succ {
            predecessor: slot_read(pred_label, at, places)?,
            step: step.0.clone(),
        },
        MatchPattern::Nat(NatPattern::Lit(literal)) => Pattern::Literal(literal.0.clone()),
        MatchPattern::List(elements) => Pattern::List(elements_read(elements, at, places)?),
        MatchPattern::Bytes(elements) => Pattern::Bytes(elements_read(elements, at, places)?),
    })
}

/// A run's elements, each read as [`pattern_read`] reads a pattern, then its rest where it is open, in the order the run writes them.
fn elements_read(
    elements: &crate::Elements<CoreText>,
    at: &crate::Term<CoreText>,
    places: &mut Vec<String>,
) -> Result<Elements, ReadError> {
    Ok(match elements {
        crate::Elements::Closed(elements) => Elements::Closed(
            elements
                .iter()
                .map(|element| pattern_read(element, at, places))
                .collect::<Result<_, ReadError>>()?,
        ),
        crate::Elements::Open { first, then, rest } => {
            let first = Box::new(pattern_read(first, at, places)?);
            let then = then
                .iter()
                .map(|element| pattern_read(element, at, places))
                .collect::<Result<_, ReadError>>()?;
            let rest = slot_read(rest, at, places)?;

            Elements::Open { first, then, rest }
        }
    })
}

/// A pattern's own binder, or a successor's predecessor or an open run's rest, where a bare `_` is a wildcard and no place: a word is a place, and `#{node = Binder} _` a nameless one. A decorator states nothing else of a place, whose label is the row's function's.
fn own_place_read(
    binder: &crate::Binder<CoreText>,
    at: &crate::Term<CoreText>,
) -> Result<Option<String>, ReadError> {
    if binder.core.is_empty() {
        return Ok((binder.label.as_str() != "_").then(|| binder.label.to_string()));
    }
    let mut fields = Fields::of(&binder.core, binder.label.span())?;
    let node = fields.take("node").map(value_word).transpose()?;
    fields.finish()?;

    match node {
        Some("Binder") if binder.label.as_str() == "_" => Ok(Some(binder.label.to_string())),
        _ => Err(misplaced(binder, at)),
    }
}

/// A successor's predecessor or an open run's rest, read as a pattern's own binder is: a place whose word `places` gathers, or a wildcard.
fn slot_read(
    binder: &crate::Binder<CoreText>,
    at: &crate::Term<CoreText>,
    places: &mut Vec<String>,
) -> Result<Slot, ReadError> {
    Ok(match own_place_read(binder, at)? {
        Some(word) => {
            places.push(word);

            Slot::Binder
        }
        None => Slot::Wildcard,
    })
}

/// The refusal of a decorator before a place, which states nothing the pattern holds: a place's label is the row's function's.
fn misplaced(binder: &crate::Binder<CoreText>, at: &crate::Term<CoreText>) -> ReadError {
    ReadError::at(
        Refusal::Malformed("a row's place, whose label the row's body states"),
        binder.label.span().or(at.span()),
    )
}

/// The words a row's function, as written, gives its binders: `None` where it is written as no lambda, as a node met again under sharing is.
fn function_words(body: &crate::Term<CoreText>) -> Option<Vec<String>> {
    match body.as_subterm() {
        crate::Subterm::Func(crate::Func { params, .. }) => Some(
            params
                .iter()
                .map(|param| match &param.pattern {
                    crate::Pattern::Binder(binder) => binder.label.to_string(),
                    crate::Pattern::Tuple(_) | crate::Pattern::Struct { .. } => "_".to_string(),
                })
                .collect(),
        ),
        _ => None,
    }
}

/// The refusal of a struct pattern in a row: what its head resolved to is Core's and no form of Core text's.
fn unresolved(at: &crate::Term<CoreText>) -> ReadError {
    ReadError::at(
        Refusal::Malformed(
            "a row's pattern other than a struct's, whose resolution Core text writes no form of",
        ),
        at.span(),
    )
}
