//! A function type, a function, a call, and a call across the wire.
//!
//! **A function is written as it binds**, its parameters under their marks — `@x`, `use w` — each over the domain Core holds for it, and a type former's η-form as the lambda it is. **A call is its head and its arguments under their marks**, the head parenthesized where it is a lambda or carries a decorator of its own. **A foreign call is the call of its `foreign` declaration under `#{node = Foreign}`**, the type a row that does not return is handed under `@`: the row it is made through is the one that declaration states.

use {
    super::{
        LabelRead, ReadError, Reader, Refusal, call, core_read, stated_proofs, term_global, tree,
        word, word_value,
    },
    crate::{
        CoreText, Frame, Pattern, Rendered, call_arguments, lambda_parameters, marked_arguments,
        parameter_types, sub,
    },
    curios_common::Plicity,
    curios_core::{
        Apply, Bound, Foreign, FormerEta, Free, Func, FuncType, Subterm, Telescope, Term,
        Universes, former_eta,
    },
};

/// A function type: its parameters, then what it returns.
pub fn function_type_written<D: Rendered>(
    FuncType { telescope }: FuncType,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    let plicities = telescope.marks();
    let mut params = Vec::with_capacity(telescope.len());
    let output = parameter_types(telescope.cursor(), &plicities, frame, &mut params);

    tree(crate::Subterm::FuncType(crate::FuncType { params, output }))
}

/// A function: its parameters, always between parentheses, each over its domain in Curios/Core, then its body.
pub fn function_written<D: Rendered>(
    Func { telescope }: Func,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    // A reader is shown a type former's η-form as the former it stands for, the lambda a reader would contract anyway.
    if !frame.core_text()
        && let Some(former) = former_eta(&telescope)
    {
        return former_written(former, frame);
    }
    let plicities = telescope.marks();
    let mut params = Vec::with_capacity(telescope.len());
    let body = lambda_parameters(telescope.cursor(), &plicities, frame, &mut params);

    tree(crate::Subterm::Func(crate::Func { params, body }))
}

/// A type former a lambda is the η-form of, as a reader writes it: its name alone where the binders took every argument, and its call over what they left otherwise, each argument under the declaration's mark.
fn former_written<D: Rendered>(former: FormerEta, frame: Frame<'_, D>) -> crate::Term<D> {
    match former {
        FormerEta::Intrinsic(name) => word(frame.spelling.intrinsic_symbol(name)),
        FormerEta::Nominal(name, prefix, arity) => {
            let marks = frame.spelling.nominal_marks(&name, arity);
            let reference = Term::instance(name, Universes::default());
            let term = match prefix.is_empty() {
                true => reference,
                false => Term::apply_marked(
                    reference,
                    prefix.into_iter().enumerate().map(|(index, argument)| {
                        let mark = marks.and_then(|marks| marks.get(index)).copied();
                        (mark.unwrap_or(Plicity::Explicit), argument)
                    }),
                ),
            };

            sub(term, frame)
        }
    }
}

/// A call: its head, then its arguments under their marks. The printer parenthesizes a head no call takes as it stands: a lambda, which would take the arguments into its body, and a decorated node, whose decorator would be read over the call.
pub fn apply_written<D: Rendered>(
    Apply { head, arguments }: Apply,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    call(sub(head, frame), call_arguments(arguments, frame))
}

/// A call across the wire: the call of the declaration that states its row.
pub fn foreign_written<D: Rendered>(
    Foreign {
        function,
        declared,
        arguments: mut implicits,
    }: Foreign,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    let explicits = implicits.split_off(usize::from(function.diverges()));
    let call = call(
        frame.global(&declared),
        marked_arguments(implicits, explicits, frame),
    );

    frame.decorated(vec![("node", word_value("Foreign"))], call)
}

/// A function type: each parameter under its mark, by its word where it has one, then what it returns, each type under the parameters before it.
pub fn function_type_read(
    crate::FuncType { params, output }: &crate::FuncType<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let entries = params
        .iter()
        .map(parameter_entry)
        .collect::<Result<Vec<_>, ReadError>>()?;
    let (read, output) =
        telescope_read(&entries, reader, &mut |reader, _| core_read(output, reader))?;

    Ok(proving(Term::func_type_marked(read, output), &entries))
}

/// A function: each parameter under its mark and by its word, over its domain, then its body.
pub fn function_read(
    crate::Func { params, body }: &crate::Func<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let entries = params
        .iter()
        .map(|param| {
            let label = match &param.pattern {
                Pattern::Binder(binder) => LabelRead::binder(binder)?,
                Pattern::Tuple(_) | Pattern::Struct { .. } | Pattern::Variant { .. } => {
                    return Err(ReadError::at(
                        Refusal::ProgramForm("a parameter's pattern"),
                        body.span(),
                    ));
                }
            };
            let domain = param.annotation.as_ref().ok_or_else(|| {
                ReadError::at(
                    Refusal::Malformed("a parameter over its domain, `x: T`"),
                    body.span(),
                )
            })?;
            Ok((param.plicity, label, domain))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let (read, body) = telescope_read(&entries, reader, &mut |reader, _| core_read(body, reader))?;

    Ok(proving(Term::func_marked(read, body), &entries))
}

/// A parameter written as a function type's is, as the telescope entry it reads as: its mark, its binder's label by its word or `_`, and its type.
pub fn parameter_entry(
    param: &crate::FuncTypeParam<CoreText>,
) -> Result<TelescopeEntry<'_>, ReadError> {
    let word = param.label.as_deref().unwrap_or("_");
    let label = LabelRead::of(&param.core, word, param.type_.span())?;

    Ok((param.plicity, label, &param.type_))
}

/// Whether each of `entries` binds a proof, where one of them states that it does.
fn entries_proofs(entries: &[TelescopeEntry<'_>]) -> Option<Vec<bool>> {
    let labels = entries
        .iter()
        .map(|(_, label, _)| *label)
        .collect::<Vec<_>>();

    stated_proofs(&labels)
}

/// `telescope`, read over `entries`, its binders stating whether each binds a proof where an entry's decorator states one does.
pub fn proving_telescope<B: Bound>(
    telescope: Telescope<B>,
    entries: &[TelescopeEntry<'_>],
) -> Telescope<B> {
    match entries_proofs(entries) {
        Some(proofs) => telescope.with_proofs(&proofs),
        None => telescope,
    }
}

/// `term`, a function or a function type read over `entries`, its binders stating whether each binds a proof where an entry's decorator states one does.
fn proving(term: Term, entries: &[TelescopeEntry<'_>]) -> Term {
    let Some(proofs) = entries_proofs(entries) else {
        return term;
    };
    match Term::unwrap_or_clone(term) {
        Subterm::Func(Func { telescope }) => Term::from(Subterm::Func(Func {
            telescope: telescope.with_proofs(&proofs),
        })),
        Subterm::FuncType(FuncType { telescope }) => Term::from(Subterm::FuncType(FuncType {
            telescope: telescope.with_proofs(&proofs),
        })),
        _ => unreachable!("a function or a function type was read"),
    }
}

/// A telescope's entry as the text writes it: its mark, its binder's label, and its type.
pub type TelescopeEntry<'t> = (Plicity, LabelRead<'t>, &'t crate::Term<CoreText>);

/// A telescope's entry as it is read: its mark, the local it is opened at, which a telescope's builder closes over, and its type.
pub type OpenedEntry = (Plicity, Free, Term);

/// What a telescope's reader reads under all of its binders, handed the entries read.
pub type Beneath<'r, R> = dyn FnMut(&mut Reader<'_>, &[OpenedEntry]) -> Result<R, ReadError> + 'r;

/// The entries of a telescope, each type read under the binders before it, then what `rest` reads under all of them.
pub fn telescope_read<R>(
    entries: &[TelescopeEntry<'_>],
    reader: &mut Reader<'_>,
    rest: &mut Beneath<'_, R>,
) -> Result<(Vec<OpenedEntry>, R), ReadError> {
    let mut read = Vec::with_capacity(entries.len());
    let rest = entries_read(entries, &mut read, reader, rest)?;

    Ok((read, rest))
}

/// [`telescope_read`] from `entries` on, the entries before them in `read`.
fn entries_read<R>(
    entries: &[TelescopeEntry<'_>],
    read: &mut Vec<OpenedEntry>,
    reader: &mut Reader<'_>,
    rest: &mut Beneath<'_, R>,
) -> Result<R, ReadError> {
    let Some(((mark, label, type_), later)) = entries.split_first() else {
        return rest(reader, read);
    };
    let type_ = core_read(type_, reader)?;
    reader.binding(&[*label], |reader, frees| {
        read.push((*mark, frees[0], type_));
        entries_read(later, read, reader, rest)
    })
}

/// A call across the wire, `#{node = Foreign} /f(@T, x)`: the call of the declaration that states its row, the type a row that does not return is handed under `@`, then its operands.
pub fn foreign_read(
    operand: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let refused = || {
        ReadError::at(
            Refusal::Malformed("the call of a foreign row's declaration"),
            operand.span(),
        )
    };
    let crate::Subterm::Apply(crate::Apply { head, arguments }) = operand.as_subterm() else {
        return Err(refused());
    };
    let declared = term_global(head).ok_or_else(refused)?;
    let function = reader.row(&declared).cloned().ok_or_else(|| {
        ReadError::at(
            Refusal::Undeclared(format!("{}", declared.faithful())),
            head.span(),
        )
    })?;
    let handed = usize::from(function.diverges());
    let mut operands = Vec::with_capacity(arguments.len());
    for (index, argument) in arguments.iter().enumerate() {
        let mark = match index < handed {
            true => Plicity::Implicit,
            false => Plicity::Explicit,
        };
        if argument.plicity != mark {
            return Err(refused());
        }
        operands.push(core_read(&argument.term, reader)?);
    }

    Ok(Term::foreign(function, declared, operands))
}
