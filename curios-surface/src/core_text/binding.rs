//! A local `let` block and a local recursive group.
//!
//! **`let x: T = v; tail` builds a block**, and consecutive `let`s one block of them all, as Core holds them; a block whose tail is a block of its own writes it between parentheses. **`let f: T = b and g: U = c; tail` builds a group**, and a group of one member is that `let` under `#{node = Rec}`, a member being in scope of its own body.

use {
    super::{ReadError, Reader, Refusal, core_read},
    crate::{CoreText, Frame, LetBinding, LetGroup, LetSignature, Pattern, label_terms, sub},
    curios_core::{Bound, Free, Let, Many, Rec, Scope, Subterm, Term},
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

/// A run of `let` statements and its tail: consecutive bindings one block, a group of several members a group whose tail is what follows it, and a block written between parentheses a block of its own.
pub fn let_read(
    crate::Let { groups, tail }: &crate::Let<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    statements_read(groups, tail, reader)
}

/// A single-member group, `#{node = Rec} (let f: T = b; tail)`: the `let` whose member is in scope of its own body.
pub fn rec_read(
    operand: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    match operand.as_subterm() {
        crate::Subterm::Let(crate::Let { groups, tail }) => match groups.split_first() {
            Some((group, rest)) => group_read(group, rest, tail, reader),
            None => Err(refused(operand)),
        },
        _ => Err(refused(operand)),
    }
}

/// The refusal of what is no `let` where a group is written.
fn refused(term: &crate::Term<CoreText>) -> ReadError {
    ReadError::at(Refusal::Malformed("a group's `let`"), term.span())
}

/// `groups` and then `tail`, as the nodes they write.
fn statements_read(
    groups: &[LetGroup<CoreText>],
    tail: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let singles = groups
        .iter()
        .take_while(|group| group.members.len() == 1)
        .count();
    match singles {
        0 => match groups.split_first() {
            Some((group, rest)) => group_read(group, rest, tail, reader),
            None => core_read(tail, reader),
        },
        _ => {
            let (block, rest) = groups.split_at(singles);
            let members = block
                .iter()
                .map(|group| &group.members[0])
                .collect::<Vec<_>>();
            block_read(&members, &mut Vec::new(), rest, tail, reader)
        }
    }
}

/// A block's bindings from `members` on, each type and value read under the bindings before it, which `read` holds; then what follows the block under all of them.
fn block_read(
    members: &[&LetBinding<CoreText>],
    read: &mut Vec<(Free, Term, Term)>,
    rest: &[LetGroup<CoreText>],
    tail: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let Some((member, later)) = members.split_first() else {
        let tail = statements_read(rest, tail, reader)?;
        return Ok(block(std::mem::take(read), tail));
    };
    let (word, type_, value) = member_parts(member, tail)?;
    let type_ = core_read(type_, reader)?;
    let value = core_read(value, reader)?;
    reader.binding(&[word], |reader, frees| {
        read.push((frees[0], type_, value));
        block_read(later, read, rest, tail, reader)
    })
}

/// A group of `group`'s members, each type and body read under every member, then what follows it under them.
fn group_read(
    group: &LetGroup<CoreText>,
    rest: &[LetGroup<CoreText>],
    tail: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let parts = group
        .members
        .iter()
        .map(|member| member_parts(member, tail))
        .collect::<Result<Vec<_>, _>>()?;
    let words = parts.iter().map(|(word, _, _)| *word).collect::<Vec<_>>();
    reader.binding(&words, |reader, frees| {
        let items = parts
            .iter()
            .zip(frees)
            .map(|((_, type_, value), free)| {
                Ok((*free, core_read(type_, reader)?, core_read(value, reader)?))
            })
            .collect::<Result<Vec<_>, ReadError>>()?;
        let tail = statements_read(rest, tail, reader)?;

        Ok(Term::rec(items, tail))
    })
}

/// A member's word, type and value, as Core text writes one: `let x: T = v`.
fn member_parts<'t>(
    member: &'t LetBinding<CoreText>,
    tail: &crate::Term<CoreText>,
) -> Result<
    (
        &'t str,
        &'t crate::Term<CoreText>,
        &'t crate::Term<CoreText>,
    ),
    ReadError,
> {
    let refused = |form: &'static str| ReadError::at(Refusal::ProgramForm(form), tail.span());
    let Pattern::Binder(word) = &member.binder else {
        return Err(refused("a `let`'s pattern"));
    };
    match &member.signature {
        LetSignature::Name {
            type_: Some(type_),
            body,
        } => Ok((word.as_str(), type_, body)),
        LetSignature::Name { type_: None, .. } => Err(refused("a `let` that states no type")),
        LetSignature::Func { .. } => Err(refused("a `let`'s definition sugar")),
    }
}

/// One block of `items`, each closed over the bindings before it, over `tail` closed over all of them, which stays a node of its own where it is a block: a block written as a tail is one the text wrote apart.
fn block(items: Vec<(Free, Term, Term)>, tail: Term) -> Term {
    let binders = items
        .iter()
        .map(|(binder, _, _)| binder)
        .collect::<Vec<_>>();
    let closed = |term: &Term, count: usize| match count {
        0 => term.clone(),
        _ => Scope::close(Many(count), &binders[..count], term.clone())
            .body()
            .clone(),
    };
    let bindings = items
        .iter()
        .enumerate()
        .map(|(index, (_, type_, value))| {
            curios_core::LetBinding::new(closed(type_, index), closed(value, index))
        })
        .collect();

    Term::from(Subterm::Let(Let {
        bindings,
        tail: Scope::close(Many(binders.len()), &binders, tail),
    }))
}
