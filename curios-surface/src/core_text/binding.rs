//! A local `let` block and a local recursive group.
//!
//! **`let x: T = v; tail` builds a block**, and consecutive `let`s one block of them all, as Core holds them; a block whose tail is a block of its own writes it between parentheses. **`let f: T = b and g: U = c; tail` builds a group**, and a group of one member is that `let` under `#{node = Rec}`, a member being in scope of its own body.

use {
    crate::{Frame, label_terms, sub},
    curios_core::{Bound, Let, Rec, Subterm},
    curios_print::{Printer, flat, indent, pure, sep_flat},
};

/// A block: each binding under the ones before it, then its tail.
pub fn let_written(Let { bindings, tail }: Let, frame: Frame) -> Printer {
    let labels = frame.labels(tail.label_iter());
    let terms = label_terms(&labels);
    let terms = terms.iter().collect::<Vec<_>>();
    let lines = bindings
        .iter()
        .enumerate()
        .map(|(index, binding)| {
            let type_ = binding.type_().release(&terms[..index]);
            let value = binding.value().release(&terms[..index]);
            flat([
                pure("let "),
                pure(frame.spelling.word(&labels[index])),
                pure(": "),
                sub(type_, frame),
                pure(" =\n"),
                indent(sub(value, frame)),
                pure(";\n"),
            ])
        })
        .collect::<Vec<_>>();
    let body = tail.open(&terms);
    let body = match &*body {
        Subterm::Let(_) => flat([pure("("), sub(body, frame), pure(")")]),
        _ => sub(body, frame),
    };

    flat([flat(lines), body])
}

/// A group: each member under every member's name, then its tail.
pub fn rec_written(Rec { group, tail }: Rec, frame: Frame) -> Printer {
    let labels = frame.labels(tail.label_iter());
    let terms = label_terms(&labels);
    let terms = terms.iter().collect::<Vec<_>>();
    let members = group
        .iter()
        .cloned()
        .enumerate()
        .map(|(index, member)| {
            flat([
                pure(frame.spelling.word(&labels[index])),
                pure(": "),
                sub(member.type_.open(&terms), frame),
                pure(" =\n"),
                indent(sub(member.body.open(&terms), frame)),
            ])
        })
        .collect::<Vec<_>>();
    let single = members.len() == 1;
    let written = flat([
        pure("let "),
        sep_flat(members, || pure("\nand ")),
        pure(";\n"),
        sub(tail.open(&terms), frame),
    ]);

    match single {
        true => frame.decorated(vec![("node", pure("Rec"))], written, false),
        false => written,
    }
}
