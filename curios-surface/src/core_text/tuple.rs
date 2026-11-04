//! A tuple type, a tuple, and a projection.
//!
//! **A tuple type is `{a: A, B}`** and **a tuple `(a = x, y)`**, `()` the empty one. **`(h).0` builds a tuple's projection and `(h).label` one by a label no declaration resolved**, and a projection out of a declared record states the record and the field's place, `#{field = Struct(/Pair/Pair, 0)} (h).0`, as one by place no declaration resolved states `Position`.

use {
    super::{
        Fields, ReadError, Reader, Refusal, core_read, malformed, telescope_read, term_global,
        term_number, value_variant,
    },
    crate::{CoreText, Frame, listed, sub},
    curios_common::Plicity,
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

/// A tuple type: each field's type under the fields before it, under its label where it has one, which is the field's own and no binder's hint alone.
pub fn tuple_type_read(
    crate::TupleType { fields }: &crate::TupleType<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let entries = fields
        .iter()
        .map(|field| match &field.func_params {
            Some(_) => Err(ReadError::at(
                Refusal::ProgramForm("a field's signature sugar"),
                field.type_.span(),
            )),
            None => Ok((
                Plicity::Explicit,
                field.label.as_ref().map_or("_", |label| label.as_str()),
                &field.type_,
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let (entries, ()) = telescope_read(&entries, reader, &mut |_, _| Ok(()))?;

    Ok(Term::tuple_type(
        entries
            .into_iter()
            .map(|(_, binder, type_)| (binder, type_)),
    ))
}

/// A tuple: each field's value, under the label it was written with where it carries one; a tuple with none carries no names.
pub fn tuple_read(
    crate::Tuple { fields }: &crate::Tuple<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let mut values = Vec::with_capacity(fields.len());
    let mut names = Vec::with_capacity(fields.len());
    for field in fields {
        if field.func_params.is_some() {
            return Err(ReadError::at(
                Refusal::ProgramForm("a field's definition sugar"),
                field.value.span(),
            ));
        }
        values.push(core_read(&field.value, reader)?);
        names.push(field.label.clone());
    }
    if names.iter().all(Option::is_none) {
        names.clear();
    }

    Ok(Term::from(curios_core::Subterm::Tuple(Tuple {
        fields: values,
        names,
    })))
}

/// A projection: a tuple's slot, `(h).0`, or a label no declaration resolved, `(h).label`; a structure's slot and a position no declaration resolved are stated by `field`.
pub fn projection_read(
    crate::Proj { head, field }: &crate::Proj<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let head = core_read(head, reader)?;
    let field = match (fields.take("field"), field) {
        (None, crate::Field::Index(index)) => Field::Tuple(*index),
        (None, crate::Field::Label(label)) => Field::Label(label.clone()),
        (Some(value), crate::Field::Index(index)) => {
            let slot = |held: &crate::Term<CoreText>| {
                term_number(held)
                    .and_then(|slot| usize::try_from(slot).ok())
                    .filter(|slot| slot == index)
                    .ok_or_else(|| malformed("the slot the projection reads", value))
            };
            match value_variant(value)? {
                ("Position", held) => match held.as_slice() {
                    [position] => Field::Position(slot(position)?),
                    _ => return Err(malformed("`Position(n)`", value)),
                },
                ("Struct", held) => match held.as_slice() {
                    [record, place] => Field::Struct(
                        term_global(record)
                            .ok_or_else(|| malformed("a structure's name", value))?,
                        slot(place)?,
                    ),
                    _ => return Err(malformed("`Struct(/R, n)`", value)),
                },
                _ => return Err(malformed("`Position(n)` or `Struct(/R, n)`", value)),
            }
        }
        (Some(value), crate::Field::Label(_)) => {
            return Err(malformed("a projection by place under `field`", value));
        }
    };

    Ok(Term::from(curios_core::Subterm::Proj(Proj { head, field })))
}
