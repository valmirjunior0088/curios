//! A nominal family, a constructor's value, a record type, and a record literal.
//!
//! **A call of a former's definition builds that call**, so a family applied as a node of its own is the same call under `#{node = InductType}`, its parameters and then its indices each a call, marked as its declaration marks them; a record type is `#{node = StructType}` over its former's call. **A constructor's value is the call of its constructor's definition**, named under its family, under `#{node = Variant}`, its family's parameters under `@` ahead of its payload. **A record literal is the former's call and its fields between braces**, every field in declaration order, its parameters written where it has any; one elaboration has not rebuilt states the entries it was written with, `#{entries = [Field(a), Use]}`. A certified string is the literal it stands for where the spelling names the declaration that certifies one.

use {
    super::{
        Decoration, Fields, ReadError, Reader, Refusal, bracketed, core_read, malformed,
        term_global, term_word, universe_suffix, universes_read, universes_written, value_unmarked,
        value_variant,
    },
    crate::{
        CoreText, Frame, StructLitEntry, TupleField, listed, nominal_arguments, string_literal, sub,
    },
    curios_common::Plicity,
    curios_core::{
        Atom, Global, InductType, Struct, StructEntry, StructType, Term, Universes, Variant,
    },
    curios_print::{Printer, flat, pure},
};

/// The fields a nominal node's decorator sets: the node it is, and the instance it stands at where it states one.
fn nominal_fields(node: &'static str, universes: &Universes, frame: Frame) -> Vec<Decoration> {
    let mut fields = Vec::<Decoration>::from([("node", pure(node))]);
    if let Some(universes) = universes_written(universes, frame) {
        fields.push(("universes", universes));
    }

    fields
}

/// A nominal declaration's name as it is written: in a reader's spelling, with the instance it stands at after it.
fn nominal_name(name: &Global, universes: &Universes, frame: Frame) -> String {
    let symbol = frame.spelling.symbol(name);
    match frame.core_text() {
        true => symbol,
        false => format!("{symbol}{}", universe_suffix(universes, frame)),
    }
}

/// A family as the call of its former: its parameters, then its indices where it has any; in a reader's spelling, each list only where it holds any, and the bare name where it has neither.
pub fn family_written(
    InductType {
        name,
        universes,
        params,
        indices,
    }: InductType,
    frame: Frame,
) -> Printer {
    let marks = frame
        .spelling
        .nominal_marks(&name, params.len() + indices.len());
    let offset = params.len();
    let mut call = vec![pure(nominal_name(&name, &universes, frame))];
    // Core text writes the parameters' list even where it is empty, so the indices are the second list wherever there are any.
    if !params.is_empty() || frame.core_text() {
        call.push(listed(
            "(".into(),
            false,
            nominal_arguments(params, marks, 0, frame),
            ")",
        ));
    }
    if !indices.is_empty() {
        call.push(listed(
            "(".into(),
            false,
            nominal_arguments(indices, marks, offset, frame),
            ")",
        ));
    }

    frame.decorated(
        nominal_fields("InductType", &universes, frame),
        flat(call),
        true,
    )
}

/// A constructor's value as the call of its constructor's definition: its family's parameters under `@`, then its payload.
pub fn variant_written(
    Variant {
        name,
        universes,
        params,
        tag,
        payload,
    }: Variant,
    frame: Frame,
) -> Printer {
    // A reader writes a constructor's value as the constructor's call, its family's parameters left for inference.
    if !frame.core_text() {
        let constructor = format!(
            "{}/{}",
            nominal_name(&name, &universes, frame),
            tag.as_string()
        );
        return match payload.is_empty() {
            true => pure(constructor),
            false => listed(
                format!("{constructor}("),
                false,
                payload.into_iter().map(|field| sub(field, frame)).collect(),
                ")",
            ),
        };
    }
    let constructor = match name {
        Global::Authored(family) => frame
            .spelling
            .symbol(&Global::Authored(family.with(tag.as_string().as_str()))),
        Global::Witness(_) => format!("{}/{}", frame.spelling.symbol(&name), tag.as_string()),
    };
    let arguments = params
        .into_iter()
        .map(|param| flat([pure("@"), sub(param, frame)]))
        .chain(payload.into_iter().map(|field| sub(field, frame)))
        .collect();

    frame.decorated(
        nominal_fields("Variant", &universes, frame),
        flat([pure(constructor), listed("(".into(), false, arguments, ")")]),
        true,
    )
}

/// A record type as the call of its former.
pub fn record_written(
    StructType {
        name,
        universes,
        params,
    }: StructType,
    frame: Frame,
) -> Printer {
    let marks = frame.spelling.nominal_marks(&name, params.len());
    if !frame.core_text() && params.is_empty() {
        return pure(nominal_name(&name, &universes, frame));
    }
    let call = flat([
        pure(nominal_name(&name, &universes, frame)),
        listed(
            "(".into(),
            false,
            nominal_arguments(params, marks, 0, frame),
            ")",
        ),
    ]);

    frame.decorated(nominal_fields("StructType", &universes, frame), call, true)
}

/// A record literal: its former's call where it states parameters, and its fields between braces.
pub fn literal_written(
    Struct {
        name,
        universes,
        params,
        fields,
        entries,
    }: Struct,
    frame: Frame,
) -> Printer {
    if let Some(literal) = string_literal(&name, &fields, frame.spelling) {
        return pure(literal);
    }
    let marks = frame.spelling.nominal_marks(&name, params.len());
    // A reader writes a literal by its record's name, its parameters left for inference.
    let head = match params.is_empty() || !frame.core_text() {
        true => pure(nominal_name(&name, &universes, frame)),
        false => flat([
            pure(frame.spelling.symbol(&name)),
            listed(
                "(".into(),
                false,
                nominal_arguments(params, marks, 0, frame),
                ")",
            ),
        ]),
    };
    let literal = flat([
        head,
        pure(" "),
        listed(
            "{".into(),
            true,
            fields
                .into_iter()
                .map(|field: Term| sub(field, frame))
                .collect(),
            "}",
        ),
    ]);
    let mut decoration = Vec::<Decoration>::new();
    if let Some(universes) = universes_written(&universes, frame) {
        decoration.push(("universes", universes));
    }
    if !entries.is_empty() {
        decoration.push((
            "entries",
            bracketed(entries.iter().map(entry_written).collect()),
        ));
    }

    frame.decorated(decoration, literal, true)
}

/// How a literal elaboration has not rebuilt wrote one of its entries, as Core names it.
fn entry_written(entry: &StructEntry) -> Printer {
    let label = |label: &Option<String>| match label {
        Some(label) => format!("Some({label})"),
        None => "None".to_string(),
    };
    pure(match entry {
        StructEntry::Field(written) => format!("Field({})", label(written)),
        StructEntry::Implicit(written) => format!("Implicit({})", label(written)),
        StructEntry::Use => "Use".to_string(),
        StructEntry::Spread => "Spread".to_string(),
    })
}

/// The instance a nominal node stands at: none, or what `universes` states.
fn nominal_universes(fields: &mut Fields<'_>) -> Result<Universes, ReadError> {
    match fields.take("universes") {
        Some(value) => universes_read(value),
        None => Ok(Universes::default()),
    }
}

/// A call's head and its arguments' terms, each read under whatever mark it is written with: a nominal node's arguments are marked as its declaration marks them where the spelling knows it, and the mark is no part of the node.
fn call_parts<'t>(
    term: &'t crate::Term<CoreText>,
    what: &'static str,
) -> Result<(&'t crate::Term<CoreText>, Vec<&'t crate::Term<CoreText>>), ReadError> {
    match term.as_subterm() {
        crate::Subterm::Apply(crate::Apply { head, arguments }) => Ok((
            head,
            arguments.iter().map(|argument| &argument.term).collect(),
        )),
        _ => Err(ReadError::at(Refusal::Malformed(what), term.span())),
    }
}

/// Each of `terms` read.
fn terms_read(
    terms: &[&crate::Term<CoreText>],
    reader: &mut Reader<'_>,
) -> Result<Vec<Term>, ReadError> {
    terms.iter().map(|term| core_read(term, reader)).collect()
}

/// A family as a node of its own, `#{node = InductType} /F(params)(indices)`: its former's call over its parameters, then over its indices where it has any.
pub fn family_read(
    operand: &crate::Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    const CALL: &str = "a family's call, `/F(params)` and its indices' after it";
    let (head, outer) = call_parts(operand, CALL)?;
    let (name, params, indices) = match term_global(head) {
        Some(name) => (name, outer, Vec::new()),
        None => {
            let (former, params) = call_parts(head, CALL)?;
            let name = term_global(former)
                .ok_or_else(|| ReadError::at(Refusal::Malformed(CALL), former.span()))?;
            (name, params, outer)
        }
    };
    let universes = nominal_universes(fields)?;

    Ok(Term::from(curios_core::Subterm::InductType(InductType {
        name,
        universes,
        params: terms_read(&params, reader)?,
        indices: terms_read(&indices, reader)?,
    })))
}

/// A constructor's value, `#{node = Variant} /F/tag(@params, payload)`: its family's parameters under `@`, then its payload.
pub fn variant_read(
    operand: &crate::Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    const CALL: &str = "a constructor's call, `/F/tag(@params, payload)`";
    let refused = || ReadError::at(Refusal::Malformed(CALL), operand.span());
    let crate::Subterm::Apply(crate::Apply { head, arguments }) = operand.as_subterm() else {
        return Err(refused());
    };
    let Some(Global::Authored(constructor)) = term_global(head) else {
        return Err(refused());
    };
    if constructor.is_root() {
        return Err(refused());
    }
    let hidden = arguments
        .iter()
        .take_while(|argument| argument.plicity == Plicity::Implicit)
        .count();
    if arguments[hidden..]
        .iter()
        .any(|argument| argument.plicity != Plicity::Explicit)
    {
        return Err(refused());
    }
    let (params, payload) = arguments.split_at(hidden);
    let universes = nominal_universes(fields)?;

    Ok(Term::from(curios_core::Subterm::Variant(Variant {
        name: Global::Authored(constructor.without_last()),
        universes,
        params: params
            .iter()
            .map(|argument| core_read(&argument.term, reader))
            .collect::<Result<_, _>>()?,
        tag: Atom::from(constructor.last()),
        payload: payload
            .iter()
            .map(|argument| core_read(&argument.term, reader))
            .collect::<Result<_, _>>()?,
    })))
}

/// A record type as a node of its own, `#{node = StructType} /R(params)`: its former's call.
pub fn record_read(
    operand: &crate::Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    const CALL: &str = "a record's call, `/R(params)`";
    let (head, params) = call_parts(operand, CALL)?;
    let name =
        term_global(head).ok_or_else(|| ReadError::at(Refusal::Malformed(CALL), head.span()))?;
    let universes = nominal_universes(fields)?;

    Ok(Term::from(curios_core::Subterm::StructType(StructType {
        name,
        universes,
        params: terms_read(&params, reader)?,
    })))
}

/// A record literal: its former's call where it states parameters, and every field between braces in declaration order, unlabelled; the entries it was written with where elaboration has not rebuilt it.
pub fn literal_read(
    crate::StructLit {
        head,
        params,
        entries,
    }: &crate::StructLit<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let refused = |what: &'static str| ReadError::at(Refusal::Malformed(what), head.span());
    if !head.is_abs() {
        return Err(refused("a literal's absolute name"));
    }
    let name = Global::Authored(*head.qualifier());
    let params = match params {
        Some(arguments) => arguments
            .iter()
            .map(|argument| core_read(&argument.term, reader))
            .collect::<Result<_, _>>()?,
        None => Vec::new(),
    };
    let values = entries
        .iter()
        .map(|entry| match entry {
            StructLitEntry::Field(TupleField {
                label: None,
                func_params: None,
                value,
            }) => core_read(value, reader),
            _ => Err(refused("a literal's field, unlabelled and in its place")),
        })
        .collect::<Result<_, _>>()?;
    let universes = nominal_universes(fields)?;
    let written = match fields.take("entries") {
        Some(value) => value_unmarked(value)?
            .into_iter()
            .map(entry_read)
            .collect::<Result<_, _>>()?,
        None => Vec::new(),
    };

    Ok(Term::from(curios_core::Subterm::Struct(Struct {
        name,
        universes,
        params,
        fields: values,
        entries: written,
    })))
}

/// How a literal elaboration has not rebuilt wrote one of its entries, as Core names it.
fn entry_read(value: &crate::CoreValue) -> Result<StructEntry, ReadError> {
    let label = |held: &[&crate::Term<CoreText>]| match held {
        [written] if term_word(written) == Some("None") => Ok(None),
        [written] => match written.as_subterm() {
            crate::Subterm::Apply(crate::Apply { head, arguments })
                if term_word(head) == Some("Some") =>
            {
                match arguments.as_slice() {
                    [label] => term_word(&label.term)
                        .map(|label| Some(label.to_string()))
                        .ok_or_else(|| malformed("an entry's label", value)),
                    _ => Err(malformed("an entry's label", value)),
                }
            }
            _ => Err(malformed("`None` or `Some(label)`", value)),
        },
        _ => Err(malformed("an entry's label", value)),
    };
    match value_variant(value)? {
        ("Field", held) => Ok(StructEntry::Field(label(&held)?)),
        ("Implicit", held) => Ok(StructEntry::Implicit(label(&held)?)),
        ("Use", held) if held.is_empty() => Ok(StructEntry::Use),
        ("Spread", held) if held.is_empty() => Ok(StructEntry::Spread),
        _ => Err(malformed("a literal's entry, as Core names it", value)),
    }
}
