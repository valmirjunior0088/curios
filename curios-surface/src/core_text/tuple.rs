//! A tuple type, a tuple, and a projection.
//!
//! **A tuple type is `{a: A, B}`** and **a tuple `(a = x, y)`**, `()` the empty one. **`(h).0` builds a tuple's projection and `(h).label` one by a label no declaration resolved**, and a projection out of a declared record states the record and the field's place, `#{field = Struct(/Pair/Pair, 0)} (h).0`, as one by place no declaration resolved states `Position`.

use {
    crate::{Frame, listed, sub},
    curios_core::{Field, Proj, Term, Tuple, TupleType},
    curios_print::{Printer, flat, indent, pure},
};

/// A tuple type: each field's type, under its label where it has one a reader wrote or a later field reads.
pub fn tuple_type_written(TupleType { telescope }: TupleType, frame: Frame) -> Printer {
    let mut items = Vec::with_capacity(telescope.len());
    let mut cursor = telescope.cursor();
    while let Some((_, type_)) = cursor.entry() {
        let raw = cursor.label();
        let label = frame.label(raw);
        frame.mint.note_tuple_label(&label);
        let named = match raw {
            Some(name) => name.hint().is_some() || cursor.binder_used(),
            None => false,
        };
        let typed = sub(type_, frame);
        items.push(indent(match named {
            true => flat([pure(frame.spelling.label(&label)), pure(": "), typed]),
            false => typed,
        }));
        cursor.advance(Term::free_var(&label));
    }

    listed("{".into(), false, items, "}")
}

/// A tuple: each field's value, under its label where it carries one, a lone unlabelled one before a comma, which no parenthesized term ends with.
pub fn tuple_written(Tuple { fields, names }: Tuple, frame: Frame) -> Printer {
    let mut names = names.into_iter().chain(std::iter::repeat(None));
    let lone = fields.len() == 1;
    let fields = fields
        .into_iter()
        .map(|field| match names.next().flatten() {
            Some(name) => flat([pure(name), pure(" = "), sub(field, frame)]),
            None if lone => flat([sub(field, frame), pure(",")]),
            None => sub(field, frame),
        })
        .collect();

    listed("(".into(), false, fields, ")")
}

/// A projection: its head between parentheses, then the field it reads.
pub fn projection_written(Proj { head, field }: Proj, frame: Frame) -> Printer {
    let read = |field: String| flat([pure("("), sub(head, frame), pure(format!(").{field}"))]);
    match field {
        Field::Tuple(index) => read(index.to_string()),
        Field::Label(label) => read(label),
        Field::Position(index) => frame.decorated(
            vec![("field", pure(format!("Position({index})")))],
            read(index.to_string()),
            true,
        ),
        Field::Struct(record, index) => frame.decorated(
            vec![(
                "field",
                pure(format!(
                    "Struct({}, {index})",
                    frame.spelling.symbol(&record)
                )),
            )],
            read(index.to_string()),
            true,
        ),
    }
}
