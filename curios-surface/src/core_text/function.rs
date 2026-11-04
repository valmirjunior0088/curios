//! A function type, a function, a call, and a call across the wire.
//!
//! **A function is written as it binds**, its parameters under their marks — `@x`, `use w` — each over the domain Core holds for it, and a type former's η-form as the lambda it is. **A call is its head and its arguments under their marks**, the head parenthesized where it is a lambda or carries a decorator of its own. **A foreign call is the call of its `foreign` declaration under `#{node = Foreign}`**, the type a row that does not return is handed under `@`: the row it is made through is the one that declaration states.

use {
    super::{ReadError, Reader, Refusal, core_read, term_global, writes_operands},
    crate::{
        CoreText, Frame, Pattern, call_arguments, lambda_parameters, listed, marked_arguments,
        parameter_types, sub,
    },
    curios_common::Plicity,
    curios_core::{
        Apply, Field, Foreign, FormerEta, Free, Func, FuncType, Intrinsic, Subterm, Term,
        Universes, former_eta,
    },
    curios_print::{Printer, flat, group, pure},
};

/// A function type: its parameters, then what it returns.
pub fn function_type_written(FuncType { telescope }: FuncType, frame: Frame) -> Printer {
    let plicities = telescope.marks();
    let mut printers = Vec::with_capacity(telescope.len());
    let output = parameter_types(telescope.cursor(), &plicities, frame, &mut printers);

    flat([
        listed("(".into(), false, printers, ")"),
        pure(" -> "),
        output,
    ])
}

/// A function: its parameters, always between parentheses, then its body.
pub fn function_written(Func { telescope }: Func, frame: Frame) -> Printer {
    // A reader is shown a type former's η-form as the former it stands for, the lambda a reader would contract anyway.
    if !frame.core_text()
        && let Some(former) = former_eta(&telescope)
    {
        return former_written(former, frame);
    }
    let plicities = telescope.marks();
    let mut marked = Vec::with_capacity(telescope.len());
    let mut domains = Vec::with_capacity(telescope.len());
    let body = lambda_parameters(
        telescope.cursor(),
        &plicities,
        frame,
        &mut marked,
        &mut domains,
    );
    // Core text writes each parameter's domain, which Core holds; a reader leaves it to the lambda's type.
    let parameters = match frame.core_text() {
        true => listed(
            "(".into(),
            false,
            marked
                .into_iter()
                .zip(domains)
                .map(|(parameter, domain)| flat([pure(parameter), pure(": "), domain]))
                .collect(),
            ")",
        ),
        false => pure(format!("({})", marked.join(", "))),
    };

    group(flat([parameters, pure(" =>"), body]))
}

/// A type former a lambda is the η-form of, as a reader writes it: its name alone where the binders took every argument, and its call over what they left otherwise, each argument under the declaration's mark.
fn former_written(former: FormerEta, frame: Frame) -> Printer {
    match former {
        FormerEta::Intrinsic(name) => pure(frame.spelling.intrinsic_symbol(name)),
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

/// A call: its head, parenthesized where it is no operand a call takes as it stands, then its arguments.
pub fn apply_written(Apply { head, arguments }: Apply, frame: Frame) -> Printer {
    let callee = match standalone_head(&head) {
        true => sub(head, frame),
        false => flat([pure("("), sub(head, frame), pure(")")]),
    };

    flat([
        callee,
        listed("(".into(), false, call_arguments(arguments, frame), ")"),
    ])
}

/// Whether `head` is a primary a call can follow as it is written: a name, a call, a projection, a variable, an intrinsic. A lambda would take the arguments into its body, and a decorated node would take the decorator over the call.
pub fn standalone_head(head: &Term) -> bool {
    match &**head {
        Subterm::Instance(instance) => instance.universes.is_empty(),
        Subterm::Var(var) => var.as_bound().is_none(),
        Subterm::Apply(_) => true,
        Subterm::Proj(projection) => matches!(projection.field, Field::Tuple(_) | Field::Label(_)),
        Subterm::Intrinsic(intrinsic) => {
            !matches!(intrinsic, Intrinsic::Nat(_)) && writes_operands(intrinsic)
        }
        _ => false,
    }
}

/// A call across the wire: the call of the declaration that states its row.
pub fn foreign_written(
    Foreign {
        function,
        declared,
        arguments: mut implicits,
    }: Foreign,
    frame: Frame,
) -> Printer {
    let explicits = implicits.split_off(usize::from(function.diverges()));
    let call = flat([
        pure(frame.spelling.symbol(&declared)),
        listed(
            "(".into(),
            false,
            marked_arguments(implicits, explicits, frame),
            ")",
        ),
    ]);

    frame.decorated(vec![("node", pure("Foreign"))], call, true)
}

/// A function type: each parameter under its mark, by its word where it has one, then what it returns, each type under the parameters before it.
pub fn function_type_read(
    crate::FuncType { params, output }: &crate::FuncType<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let entries = params
        .iter()
        .map(|param| {
            (
                param.plicity,
                param.label.as_deref().unwrap_or("_"),
                &param.type_,
            )
        })
        .collect::<Vec<_>>();
    let (entries, output) =
        telescope_read(&entries, reader, &mut |reader, _| core_read(output, reader))?;

    Ok(Term::func_type_marked(entries, output))
}

/// A function: each parameter under its mark and by its word, over its domain, then its body.
pub fn function_read(
    crate::Func { params, body }: &crate::Func<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let entries = params
        .iter()
        .map(|param| {
            let word = match &param.pattern {
                Pattern::Binder(label) => label.as_str(),
                Pattern::Tuple(_) | Pattern::Struct { .. } => {
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
            Ok((param.plicity, word, domain))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let (entries, body) =
        telescope_read(&entries, reader, &mut |reader, _| core_read(body, reader))?;

    Ok(Term::func_marked(entries, body))
}

/// A telescope's entry as it is read: its mark, the local it is opened at, which a telescope's builder closes over, and its type.
pub type OpenedEntry = (Plicity, Free, Term);

/// What a telescope's reader reads under all of its binders, handed the entries read.
pub type Beneath<'r, R> = dyn FnMut(&mut Reader<'_>, &[OpenedEntry]) -> Result<R, ReadError> + 'r;

/// The entries of a telescope, each type read under the binders before it, then what `rest` reads under all of them.
pub fn telescope_read<R>(
    entries: &[(Plicity, &str, &crate::Term<CoreText>)],
    reader: &mut Reader<'_>,
    rest: &mut Beneath<'_, R>,
) -> Result<(Vec<OpenedEntry>, R), ReadError> {
    let mut read = Vec::with_capacity(entries.len());
    let rest = entries_read(entries, &mut read, reader, rest)?;

    Ok((read, rest))
}

/// [`telescope_read`] from `entries` on, the entries before them in `read`.
fn entries_read<R>(
    entries: &[(Plicity, &str, &crate::Term<CoreText>)],
    read: &mut Vec<OpenedEntry>,
    reader: &mut Reader<'_>,
    rest: &mut Beneath<'_, R>,
) -> Result<R, ReadError> {
    let Some(((mark, word, type_), later)) = entries.split_first() else {
        return rest(reader, read);
    };
    let type_ = core_read(type_, reader)?;
    reader.binding(&[word], |reader, frees| {
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
