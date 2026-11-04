//! The forms only elaboration builds, each in the surface spelling it was read from under the node it is.
//!
//! An operator is `#{node = Infix} (a + b)`, a numeral elaboration has not settled `#{node = NumLit} 3`, a `!` site the rest of its region called over the action's result, `#{node = Bang} (k)((action)!)`, an ascription `#{node = Ascribed, type_ = T} e`, the body a derived witness asks for `#{node = Derive} ?`, a name resolution has not settled `#{node = Name} /path`, a concept's method named by its path `#{node = Method} /Show/show`, and a match before its patterns are compiled `#{node = Matrix} match head | pattern => body … end`, each row's pattern over places and its body the function over the pattern's binders it is. A program writes none of them in Core text, which no elaborator reads; a witness writes one to see it refused. A reader is shown each as it was written, an operator with its operands parenthesized as a reader reads them, an ascription as the term and its type bracketed, and the derived body as `derive`.

use {
    super::{
        Decoration, Fields, ReadError, Reader, Refusal, core_read, reader_infix, standalone_head,
        term_global, value_term,
    },
    crate::{
        BytesPattern, CoreText, Frame, ListPattern, MatchPattern, NatPattern, ProofLiteral,
        escape_character, lambda_parameters, sub, written_pattern,
    },
    curios_core::{
        Ascribed, Atom, Bang, Bind, Func, Global, Infix, Matrix, Method, NumLit, Pattern, Row,
        Subterm, Term, Transient,
    },
    curios_print::{Printer, flat, indent, line, pure},
};

/// An elaboration-only form under the node it is.
pub fn transient_written(transient: Transient, frame: Frame) -> Printer {
    let node = |name: &'static str| vec![("node", pure(name))];
    if !frame.core_text() {
        match transient {
            Transient::Infix(Infix { op, left, right }) => {
                return reader_infix(op.symbol(), left, right, frame);
            }
            // No surface form states a type beside a bare term, so a reader is shown the pair bracketed.
            Transient::Ascribed(Ascribed { term, type_ }) => {
                return flat([
                    pure("("),
                    sub(term, frame),
                    pure(": "),
                    sub(type_, frame),
                    pure(")"),
                ]);
            }
            Transient::Derive => return pure("derive"),
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
) -> Printer {
    match transient {
        Transient::Infix(Infix { op, left, right }) => frame.decorated(
            node("Infix"),
            flat([
                sub(left, frame),
                pure(format!(" {} ", op.symbol())),
                sub(right, frame),
            ]),
            false,
        ),
        Transient::NumLit(NumLit::Number { magnitude, sign }) => frame.decorated(
            node("NumLit"),
            pure(format!("{}{magnitude}", sign.symbol())),
            true,
        ),
        Transient::NumLit(NumLit::Character(character)) => frame.decorated(
            node("NumLit"),
            pure(format!("'{}'", escape_character(character))),
            true,
        ),
        // The rest of the region called over the action's result, as the `!` it is: `(k)((action)!)`.
        Transient::Bang(Bang {
            action,
            continuation,
        }) => frame.decorated(
            node("Bang"),
            flat([
                pure("("),
                sub(continuation, frame),
                pure(")(("),
                sub(action, frame),
                pure(")!)"),
            ]),
            true,
        ),
        Transient::Ascribed(Ascribed { term, type_ }) => {
            let mut fields = node("Ascribed");
            fields.push(("type_", sub(type_, frame)));
            // The term is the operand as it stands only where it writes no decorator of its own, which a second decorator would not attach to.
            let primary = standalone_head(&term);
            frame.decorated(fields, sub(term, frame), primary)
        }
        Transient::Derive => frame.decorated(node("Derive"), pure("?"), true),
        Transient::Name(name) => {
            frame.decorated(node("Name"), pure(frame.spelling.symbol(&name)), true)
        }
        Transient::Method(Method { concept, field }) => {
            let path = match concept.qualifier() {
                Some(concept) => frame
                    .spelling
                    .symbol(&Global::Authored(concept.with(&field))),
                None => format!("{}/{field}", frame.spelling.symbol(&concept)),
            };
            frame.decorated(node("Method"), pure(path), true)
        }
        Transient::Matrix(matrix) => {
            frame.decorated(node("Matrix"), matrix_written(matrix, frame), true)
        }
    }
}

/// A match as it was written: its head and motive, then each row as its pattern over the names its body binds — in Core text, its pattern over places and its body as the function over its binders it is, each over the domain Core holds for it.
fn matrix_written(Matrix { head, motive, rows }: Matrix, frame: Frame) -> Printer {
    let motive = match motive {
        Some(motive) => flat([pure(": "), sub(motive, frame)]),
        None => pure(""),
    };
    let rows = rows
        .into_iter()
        .map(|Row { pattern, body }| {
            if frame.core_text() {
                let places = &mut std::iter::repeat_with(|| "_".to_string());
                return flat([
                    pure("\n| "),
                    pure(written_pattern(&pattern, places)),
                    pure(" =>"),
                    indent(flat([line(), sub(body, frame)])),
                ]);
            }
            let Subterm::Func(Func { telescope }) = &*body else {
                unreachable!("a row's body is a function over its binders");
            };
            let mut names = Vec::with_capacity(telescope.len());
            let body = lambda_parameters(
                telescope.cursor(),
                &telescope.marks(),
                frame,
                &mut names,
                &mut Vec::new(),
            );

            flat([
                pure("\n| "),
                pure(written_pattern(&pattern, &mut names.into_iter())),
                pure(" =>"),
                body,
            ])
        })
        .collect::<Vec<_>>();

    flat([
        pure("match "),
        sub(head, frame),
        motive,
        flat(rows),
        pure("\nend"),
    ])
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
                    let pattern = pattern_read(&arm.pattern, operand)?;
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
                .collect::<Result<_, ReadError>>()?;
            Transient::Matrix(Matrix {
                head: core_read(head, reader)?,
                motive: motive
                    .as_ref()
                    .map(|motive| core_read(motive, reader))
                    .transpose()?,
                rows,
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
