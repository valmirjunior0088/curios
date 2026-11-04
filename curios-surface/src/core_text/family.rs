//! A nominal family, a constructor's value, a record type, and a record literal.
//!
//! **A call of a former's definition builds that call**, so a family applied as a node of its own is the same call under `#{node = InductType}`, its parameters and then its indices each a call, marked as its declaration marks them; a record type is `#{node = StructType}` over its former's call. **A constructor's value is the call of its constructor's definition**, named under its family, under `#{node = Variant}`, its family's parameters under `@` ahead of its payload. **A record literal is the former's call and its fields between braces**, every field in declaration order, its parameters written where it has any; one elaboration has not rebuilt states the entries it was written with, `#{entries = [Field(a), Use]}`. A certified string is the literal it stands for where the spelling names the declaration that certifies one.

use {
    super::{Decoration, bracketed, universe_suffix, universes_written},
    crate::{Frame, listed, nominal_arguments, string_literal, sub},
    curios_core::{Global, InductType, Struct, StructEntry, StructType, Term, Universes, Variant},
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

/// A family as the call of its former: its parameters, then its indices, each a call where it has any; in a reader's spelling, the bare name where it has neither.
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
    if !params.is_empty() || (indices.is_empty() && frame.core_text()) {
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
