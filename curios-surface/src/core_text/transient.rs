//! The forms only elaboration builds, each in the surface spelling it was read from under the node it is.
//!
//! An operator is `#{node = Infix} (a + b)`, a numeral elaboration has not settled `#{node = NumLit} 3`, a `!` site `#{node = Bang} (action!; rest)`, an ascription `#{node = Ascribed, type_ = T} e`, the body a derived witness asks for `#{node = Derive} ?`, a name resolution has not settled `#{node = Name} /path`, a concept's method named by its path `#{node = Method} /Show/show`, and a match before its patterns are compiled `#{node = Matrix} match head | pattern => body … end`. A program writes none of them in Core text, which no elaborator reads; a witness writes one to see it refused. A reader is shown each as it was written, an operator with its operands parenthesized as a reader reads them, an ascription as the term and its type bracketed, and the derived body as `derive`.

use {
    super::{Decoration, reader_infix},
    crate::{Frame, escape_character, lambda_parameters, sub, written_pattern},
    curios_core::{
        Ascribed, Bang, Func, Global, Infix, Matrix, Method, NumLit, Row, Subterm, Transient,
    },
    curios_print::{Printer, flat, pure},
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
        Transient::Bang(Bang {
            action,
            continuation,
        }) => frame.decorated(
            node("Bang"),
            flat([sub(action, frame), pure("!; "), sub(continuation, frame)]),
            false,
        ),
        Transient::Ascribed(Ascribed { term, type_ }) => {
            let mut fields = node("Ascribed");
            fields.push(("type_", sub(type_, frame)));
            let primary = matches!(&*term, Subterm::Instance(_) | Subterm::Var(_));
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

/// A match as it was written: its head and motive, then each row as its pattern over the names its body binds.
fn matrix_written(Matrix { head, motive, rows }: Matrix, frame: Frame) -> Printer {
    let motive = match motive {
        Some(motive) => flat([pure(": "), sub(motive, frame)]),
        None => pure(""),
    };
    let rows = rows
        .into_iter()
        .map(|Row { pattern, body }| {
            let Subterm::Func(Func { telescope }) = &*body else {
                unreachable!("a row's body is a function over its binders");
            };
            let mut names = Vec::with_capacity(telescope.len());
            let body = lambda_parameters(telescope.cursor(), &telescope.marks(), frame, &mut names);

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
