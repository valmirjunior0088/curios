//! A local `let` block and a local recursive group.
//!
//! **`let x: T = v; tail` builds a block**, and consecutive `let`s one block of them all, as Core holds them; a block whose tail is a block of its own writes it between parentheses. **`let f: T = b and g: U = c; tail` builds a group**, and a group of one member is that `let` under `#{node = Rec}`, a member being in scope of its own body.

use {
    super::{LabelRead, ReadError, Reader, Refusal, core_read, stated_proofs, tree, word_value},
    crate::{
        Binder, CoreText, DecoratorField, Frame, Label, LetBinding, LetGroup, LetSignature,
        Pattern, Rendered, WordAt, label_terms, sub,
    },
    curios_core::{Bound, Free, Let, Many, Rec, RecGroup, Scope, Subterm, Term},
};

/// One binding of a block or a group: its word under what the decorator states of its label, its type and its value.
fn binding<D: Rendered>(
    core: Vec<DecoratorField<D>>,
    word: String,
    type_: crate::Term<D>,
    body: crate::Term<D>,
) -> LetBinding<D> {
    LetBinding {
        word: WordAt::default(),
        binder: Pattern::Binder(Binder {
            core,
            label: Label::from(word),
        }),
        signature: LetSignature::Name {
            type_: Some(type_),
            body,
        },
    }
}

/// A block: each binding under the ones before it, then its tail. The printer writes a tail that is a block of its own between parentheses, since a run of statements reads as one block.
pub fn let_written<D: Rendered>(block: Let, frame: Frame<'_, D>) -> crate::Term<D> {
    let (bindings, tail) = block.into_parts();
    let mut cores = tail.label_iter().map(|label| frame.label_fields(label));
    let labels = frame.labels(tail.label_iter());
    let terms = label_terms(&labels);
    let terms = terms.iter().collect::<Vec<_>>();
    let groups = bindings
        .iter()
        .enumerate()
        .map(|(index, each)| {
            let type_ = each.type_().release(&terms[..index]);
            let value = each.value().release(&terms[..index]);
            LetGroup {
                members: vec![binding(
                    cores.next().unwrap_or_default(),
                    frame.spelling.word(&labels[index]),
                    sub(type_, frame),
                    sub(value, frame),
                )],
            }
        })
        .collect::<Vec<_>>();
    let tail = sub(tail.open(&terms), frame);

    tree(crate::Subterm::Let(crate::Let { groups, tail }))
}

/// A group: each member under every member's name, then its tail; a group of one member under `#{node = Rec}`, which a block of that one binding would otherwise be.
pub fn rec_written<D: Rendered>(block: Rec, frame: Frame<'_, D>) -> crate::Term<D> {
    let (group, tail) = block.into_parts();
    let mut cores = tail.label_iter().map(|label| frame.label_fields(label));
    let labels = frame.labels(tail.label_iter());
    let terms = label_terms(&labels);
    let terms = terms.iter().collect::<Vec<_>>();
    let members = group
        .iter()
        .cloned()
        .enumerate()
        .map(|(index, member)| {
            binding(
                cores.next().unwrap_or_default(),
                frame.spelling.word(&labels[index]),
                sub(member.type_.open(&terms), frame),
                sub(member.body.open(&terms), frame),
            )
        })
        .collect::<Vec<_>>();
    let single = members.len() == 1;
    let written = tree(crate::Subterm::Let(crate::Let {
        groups: vec![LetGroup { members }],
        tail: sub(tail.open(&terms), frame),
    }));

    match single {
        true => frame.decorated(vec![("node", word_value("Rec"))], written),
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
fn block_read<'t>(
    members: &[&'t LetBinding<CoreText>],
    read: &mut Vec<(Free, LabelRead<'t>, Term, Term)>,
    rest: &[LetGroup<CoreText>],
    tail: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let Some((member, later)) = members.split_first() else {
        let tail = statements_read(rest, tail, reader)?;
        return Ok(block(std::mem::take(read), tail));
    };
    let (label, type_, value) = member_parts(member, tail)?;
    let type_ = core_read(type_, reader)?;
    let value = core_read(value, reader)?;
    reader.binding(&[label], |reader, frees| {
        read.push((frees[0], label, type_, value));
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
    let labels = parts.iter().map(|(label, _, _)| *label).collect::<Vec<_>>();
    reader.binding(&labels, |reader, frees| {
        let items = parts
            .iter()
            .zip(frees)
            .map(|((_, type_, value), free)| {
                Ok((*free, core_read(type_, reader)?, core_read(value, reader)?))
            })
            .collect::<Result<Vec<_>, ReadError>>()?;
        let tail = statements_read(rest, tail, reader)?;
        let group = RecGroup::closing(items);
        let tail = Scope::close(Many(frees.len()), &frees.iter().collect::<Vec<_>>(), tail);
        let block = match stated_proofs(&labels) {
            Some(proofs) => Rec::new(group.with_proofs(&proofs), tail.with_proofs(&proofs)),
            None => Rec::new(group, tail),
        };

        Ok(Term::from(Subterm::Rec(block)))
    })
}

/// A member's label, type and value, as Curios/Core writes one: `let x: T = v`.
fn member_parts<'t>(
    member: &'t LetBinding<CoreText>,
    tail: &crate::Term<CoreText>,
) -> Result<
    (
        LabelRead<'t>,
        &'t crate::Term<CoreText>,
        &'t crate::Term<CoreText>,
    ),
    ReadError,
> {
    let refused = |form: &'static str| ReadError::at(Refusal::ProgramForm(form), tail.span());
    let Pattern::Binder(binder) = &member.binder else {
        return Err(refused("a `let`'s pattern"));
    };
    match &member.signature {
        LetSignature::Name {
            type_: Some(type_),
            body,
        } => Ok((LabelRead::binder(binder)?, type_, body)),
        LetSignature::Name { type_: None, .. } => Err(refused("a `let` that states no type")),
        LetSignature::Func { .. } | LetSignature::Lambda { .. } => {
            Err(refused("a `let`'s definition sugar"))
        }
    }
}

/// One block of `items`, each closed over the bindings before it, over `tail` closed over all of them, which stays a node of its own where it is a block: a block written as a tail is one the text wrote apart. Its binders state whether each binds a proof where an item's label states one does.
fn block(items: Vec<(Free, LabelRead<'_>, Term, Term)>, tail: Term) -> Term {
    let binders = items
        .iter()
        .map(|(binder, _, _, _)| binder)
        .collect::<Vec<_>>();
    let labels = items
        .iter()
        .map(|(_, label, _, _)| *label)
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
        .map(|(index, (_, _, type_, value))| {
            curios_core::LetBinding::new(closed(type_, index), closed(value, index))
        })
        .collect();
    let tail = Scope::close(Many(binders.len()), &binders, tail);
    let tail = match stated_proofs(&labels) {
        Some(proofs) => tail.with_proofs(&proofs),
        None => tail,
    };

    Term::from(Subterm::Let(Let::new(bindings, tail)))
}
