//! A function type, a function, a call, and a call across the wire.
//!
//! **A function is written as it binds**, its parameters under their marks — `@x`, `use w` — and a type former's η-form as the lambda it is. **A call is its head and its arguments under their marks**, the head parenthesized where it is a lambda or carries a decorator of its own. **A foreign call is the call of its `foreign` declaration under `#{node = Foreign}`**, the type a row that does not return is handed under `@`: the row it is made through is the one that declaration states.

use {
    crate::{
        Frame, call_arguments, lambda_parameters, listed, marked_arguments, parameter_types, sub,
    },
    curios_common::Plicity,
    curios_core::{
        Apply, Field, Foreign, FormerEta, Func, FuncType, Intrinsic, Subterm, Term, Universes,
        former_eta,
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
    let body = lambda_parameters(telescope.cursor(), &plicities, frame, &mut marked);

    group(flat([
        pure(format!("({})", marked.join(", "))),
        pure(" =>"),
        body,
    ]))
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
fn standalone_head(head: &Term) -> bool {
    match &**head {
        Subterm::Instance(instance) => instance.universes.is_empty(),
        Subterm::Var(var) => var.as_bound().is_none(),
        Subterm::Apply(_) => true,
        Subterm::Proj(projection) => matches!(projection.field, Field::Tuple(_) | Field::Label(_)),
        Subterm::Intrinsic(intrinsic) => !matches!(intrinsic, Intrinsic::Nat(_)),
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
