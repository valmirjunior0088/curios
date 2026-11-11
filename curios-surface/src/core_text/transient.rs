//! The forms only elaboration builds, each in the surface spelling it was read from under the node it is.
//!
//! An operator is `#{node = Infix} (a + b)`, a numeral elaboration has not settled `#{node = NumLit} 3`, a `!` site the rest of its region called over the action's result, `#{node = Bang} (k)((action)!)`, an ascription `#{node = Ascribed, type_ = T} e`, the body a derived witness asks for `#{node = Derive} ?`, a name resolution has not settled `#{node = Name} /path`, a concept's method named by its path `#{node = Method} /Show/show`, and a match before its patterns are compiled `#{node = Matrix} match head | pattern => body … end`, each row's pattern over places and its body the function over the pattern's binders it is, each row an arm of its own and `#{node = Matrix, arms = [2, 1]}` counting the rows of each arm where one holds more than one. A program writes none of them in Core text, which no elaborator reads; a witness writes one to see it refused. A reader is shown each as it was written, an operator with its operands parenthesized as a reader reads them, an ascription as the term and its type bracketed, and the derived body as `derive`.

use {
    super::{
        Decoration, Fields, ReadError, Reader, Refusal, Tree, bracketed, call, core_read, numeral,
        shown, term_global, tree, value, value_number, value_term, value_unmarked, word,
        word_value,
    },
    crate::{
        Alternative, BytesPattern, CoreText, Frame, ListPattern, MatchPattern, MatrixArm,
        NatPattern, ProofLiteral, ShownForm, lambda_parameters, sub, written_pattern,
    },
    curios_common::{Plicity, Sign},
    curios_core::{
        Arm, Ascribed, Atom, Bang, Bind, Func, Global, Infix, Matrix, Method, NumLit, Pattern, Row,
        Subterm, Term, Transient,
    },
};

/// An elaboration-only form under the node it is.
pub fn transient_written(transient: Transient, frame: Frame) -> Tree {
    let node = |name: &'static str| vec![("node", word_value(name))];
    if !frame.core_text() {
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
                return shown(ShownForm::Ascribed {
                    term: sub(term, frame),
                    type_: sub(type_, frame),
                });
            }
            Transient::Derive => return tree(crate::Subterm::Derive),
            transient => return transient_written_as_core(transient, frame, node),
        }
    }

    transient_written_as_core(transient, frame, node)
}

/// [`transient_written`] for every form whose reader's spelling is its Core text's without the decorator.
fn transient_written_as_core(
    transient: Transient,
    frame: Frame,
    node: impl Fn(&'static str) -> Vec<Decoration>,
) -> Tree {
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
            tree(crate::Subterm::ProofLiteral(ProofLiteral::Char(character))),
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

/// A match as it was written: its head and motive, then each arm as its alternatives over the names their rows' bodies bind and the body the first row's opening is — in Core text, each row an arm of its own, its pattern over places and its body as the function over its binders it is, each over the domain Core holds for it, the field `arms` saying how many rows each arm holds where one holds more than one.
fn matrix_written(Matrix { head, motive, arms }: Matrix, frame: Frame) -> Tree {
    let motive = motive.map(|motive| sub(motive, frame));
    let arms = match frame.core_text() {
        true => arms
            .into_iter()
            .flat_map(|arm| arm.alternatives)
            .map(|Row { pattern, body }| {
                let places = &mut std::iter::repeat_with(|| crate::Label::from("_".to_string()));

                MatrixArm::of(written_pattern(&pattern, places), sub(body, frame))
            })
            .collect::<Vec<_>>(),
        false => arms
            .into_iter()
            .map(|arm| {
                let mut body = None;
                let alternatives = arm
                    .alternatives
                    .into_iter()
                    .map(|Row { pattern, body: row }| {
                        let Subterm::Func(Func { telescope }) = &*row else {
                            unreachable!("a row's body is a function over its binders");
                        };
                        let mut params = Vec::with_capacity(telescope.len());
                        let opened = lambda_parameters(
                            telescope.cursor(),
                            &telescope.marks(),
                            frame,
                            &mut params,
                        );
                        let mut names = params.into_iter().map(|param| match param.pattern {
                            crate::Pattern::Binder(name) => name,
                            crate::Pattern::Tuple(_) | crate::Pattern::Struct { .. } => {
                                unreachable!("a lambda's parameter is written as a name")
                            }
                        });
                        body.get_or_insert(opened);

                        Alternative {
                            pattern: written_pattern(&pattern, &mut names),
                            span: None,
                        }
                    })
                    .collect::<Vec<_>>();

                MatrixArm {
                    alternatives,
                    body: body.expect("an arm writes at least one alternative"),
                }
            })
            .collect::<Vec<_>>(),
    };

    tree(crate::Subterm::Match(crate::Match {
        head: sub(head, frame),
        motive,
        arms,
    }))
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
            Transient::NumLit(NumLit::Character(*character))
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
        ("Matrix", crate::Subterm::Match(crate::Match { head, motive, arms })) => {
            let rows = arms
                .iter()
                .map(|arm| {
                    let [alternative] = arm.alternatives.as_slice() else {
                        return Err(refused(
                            "an arm of one pattern, the rows an arm holds stated by `arms`",
                        ));
                    };
                    let pattern = pattern_read(&alternative.pattern, operand)?;
                    let body = core_read(&arm.body, reader)?;
                    match &*body {
                        Subterm::Func(Func { telescope })
                            if telescope.len() == pattern.binders() =>
                        {
                            Ok(Row { pattern, body })
                        }
                        _ => Err(refused(
                            "a row's body, the function over its pattern's binders",
                        )),
                    }
                })
                .collect::<Result<Vec<_>, ReadError>>()?;
            let counts = match fields.take("arms") {
                Some(counts) => value_unmarked(counts)?
                    .into_iter()
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
                motive: motive
                    .as_ref()
                    .map(|motive| core_read(motive, reader))
                    .transpose()?,
                arms,
            })
        }
        ("Matrix", _) => return Err(refused("a match")),
        _ => unreachable!("the node names an elaboration-only form"),
    };

    Ok(Term::from(Subterm::Transient(transient)))
}

/// A row's pattern as Core holds it, each binder a place among the row's.
fn pattern_read(pattern: &MatchPattern, at: &crate::Term<CoreText>) -> Result<Pattern, ReadError> {
    let fields = |fields: &[crate::MatchPatternField]| {
        fields
            .iter()
            .map(|field| Ok((field.label.clone(), pattern_read(&field.value, at)?)))
            .collect::<Result<Vec<_>, ReadError>>()
    };
    Ok(match pattern {
        MatchPattern::Binder(_) => Pattern::Binder,
        MatchPattern::Variant { tag, args } => Pattern::Variant {
            tag: Atom::from(tag.as_str()),
            args: args
                .iter()
                .map(|(mark, arg)| Ok((*mark, pattern_read(arg, at)?)))
                .collect::<Result<_, ReadError>>()?,
        },
        MatchPattern::Tuple(written) => Pattern::Tuple(fields(written)?),
        MatchPattern::Struct { .. } => return Err(unresolved(at)),
        MatchPattern::Bool(value) => Pattern::Bool(*value),
        MatchPattern::Char(character) => Pattern::Character(*character),
        MatchPattern::Nat(NatPattern::Zero) => Pattern::Zero,
        MatchPattern::Nat(NatPattern::Succ { ih, .. }) => {
            Pattern::Succ(bind_read(ih.as_ref(), at)?)
        }
        MatchPattern::Nat(NatPattern::Lit(literal)) => Pattern::Literal(literal.0.clone()),
        MatchPattern::List(ListPattern::Nil) => Pattern::Nil,
        MatchPattern::List(ListPattern::Cons { ih, .. }) => {
            Pattern::Cons(bind_read(ih.as_ref(), at)?)
        }
        MatchPattern::Bytes(BytesPattern::End) => Pattern::End,
        MatchPattern::Bytes(BytesPattern::Atom { ih, .. }) => {
            Pattern::Atom(bind_read(ih.as_ref(), at)?)
        }
    })
}

/// What a fold's hypothesis is bound by, where one is written.
fn bind_read(
    pattern: Option<&crate::Pattern>,
    at: &crate::Term<CoreText>,
) -> Result<Option<Bind>, ReadError> {
    fn bind(pattern: &crate::Pattern, at: &crate::Term<CoreText>) -> Result<Bind, ReadError> {
        match pattern {
            crate::Pattern::Binder(_) => Ok(Bind::Binder),
            crate::Pattern::Tuple(fields) => Ok(Bind::Fields(
                fields
                    .iter()
                    .map(|field| Ok((field.label.clone(), bind(&field.value, at)?)))
                    .collect::<Result<_, ReadError>>()?,
            )),
            crate::Pattern::Struct { .. } => Err(unresolved(at)),
        }
    }
    pattern.map(|pattern| bind(pattern, at)).transpose()
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
