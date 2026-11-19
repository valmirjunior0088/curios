//! A tuple type, a tuple, and a projection.
//!
//! **A tuple type is `{a: A, B}`** and **a tuple `(a = x, y)`**, `()` the empty one. **`(h).0` builds a tuple's projection and `(h).label` one by a label no declaration resolved**, and a projection out of a declared record states the record and the field's place, `#{field = Struct(/Pair/Pair, 0)} (h).0`, as one by place no declaration resolved states `Position`.

use {
    super::{
        Fields, LabelRead, ReadError, Reader, Refusal, called, core_read, malformed, numeral,
        proving_telescope, telescope_read, term_global, term_number, tree, value, value_variant,
        word,
    },
    crate::{CoreText, Frame, Rendered, TupleField, TupleTypeParam, sub},
    curios_common::Plicity,
    curios_core::{Field, Proj, Subterm, Term, Tuple, TupleType},
    std::iter,
};

/// A tuple type: each field's type, under its label where it has one a reader wrote or a later field reads.
pub fn tuple_type_written<D: Rendered>(
    TupleType { telescope }: TupleType,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    let mut fields = Vec::with_capacity(telescope.len());
    let mut cursor = telescope.cursor();
    while let Some((_, type_)) = cursor.entry() {
        let raw = cursor.label();
        let label = frame.label(raw);
        let core = frame.label_fields(raw);
        frame.mint.note_tuple_label(&label);
        let named = match raw {
            Some(name) => name.hint().is_some() || cursor.binder_used(),
            None => false,
        };
        let type_ = sub(type_, frame);
        // What a decorator states of a field's binder stands before a label, so an unlabelled one that states something is written as `_`, which reads back as no hint.
        let word = match named {
            true => Some(frame.spelling.label(&label)),
            false => (!core.is_empty()).then(|| "_".to_string()),
        };
        fields.push(TupleTypeParam {
            core,
            label: word.map(crate::Label::from),
            func_params: None,
            type_,
        });
        cursor.advance(Term::free_var(&label));
    }

    tree(crate::Subterm::TupleType(crate::TupleType { fields }))
}

/// A tuple: each field's value, under its label where it carries one; the printer writes a lone unlabelled one before a comma, which no parenthesized term ends with.
pub fn tuple_written<D: Rendered>(
    Tuple { fields, names }: Tuple,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    let mut names = names.into_iter().chain(iter::repeat(None));
    let fields = fields
        .into_iter()
        .map(|field| TupleField {
            label: names.next().flatten(),
            func_params: None,
            value: sub(field, frame),
        })
        .collect();

    tree(crate::Subterm::Tuple(crate::Tuple { fields }))
}

/// A projection: its head, then the field it reads, under the record and place it reads where a declaration resolved it or none did.
pub fn projection_written<D: Rendered>(
    Proj { head, field }: Proj,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    let read = |field: crate::Field| {
        tree(crate::Subterm::Proj(crate::Proj {
            head: sub(head, frame),
            field,
        }))
    };
    match field {
        Field::Tuple(index) => read(crate::Field::Index(index)),
        Field::Label(label) => read(crate::Field::Label(label)),
        Field::Position(index) => frame.decorated(
            vec![(
                "field",
                value(called(word("Position"), vec![numeral(index)])),
            )],
            read(crate::Field::Index(index)),
        ),
        Field::Struct(record, index) => frame.decorated(
            vec![(
                "field",
                value(called(
                    word("Struct"),
                    vec![frame.global(&record), numeral(index)],
                )),
            )],
            read(crate::Field::Index(index)),
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
            None => {
                let word = field.label.as_ref().map_or("_", |label| label.as_str());
                let label = LabelRead::of(&field.core, word, field.type_.span())?;
                Ok((Plicity::Explicit, label, &field.type_))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    let (read, ()) = telescope_read(&entries, reader, &mut |_, _| Ok(()))?;
    let built = Term::tuple_type(read.into_iter().map(|(_, binder, type_)| (binder, type_)));
    let Subterm::TupleType(TupleType { telescope }) = Term::unwrap_or_clone(built) else {
        unreachable!("a tuple type was built");
    };

    Ok(Term::from(Subterm::TupleType(TupleType {
        telescope: proving_telescope(telescope, &entries),
    })))
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
