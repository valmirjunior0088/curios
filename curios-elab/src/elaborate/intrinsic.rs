use {
    super::{Context, Error, Mode, elaborate, expect},
    crate::reduce_with,
    curios_core::{Intrinsic, Operand, Produced, Subterm, Term, Var, Visit},
};

fn list_type(elem: Term) -> Term {
    Subterm::Intrinsic(Intrinsic::ListType(elem)).into()
}

/// Check every element of a `List` literal against an already-determined element type, returning the rebuilt elements. Shared by the two ways the element type is fixed: borrowed from `expected` when checking, or a fresh metavar when inferring (see [`elaborate_intrinsic`] and [`synth_intrinsic`]'s `List` arm).
fn check_list_elems(
    context: &mut Context,
    elems: &[Term],
    elem_type: &Term,
) -> Result<Vec<Term>, Error> {
    let mut elaborated = Vec::with_capacity(elems.len());

    for elem in elems {
        elaborated.push(elaborate(context, elem, Mode::Check(elem_type.clone()))?.0);
    }

    Ok(elaborated)
}

/// Synthesize an intrinsic's type, checking *and rebuilding* its operands. Mirrors the old `infer_intrinsic`, but every operand obligation goes through `elaborate(Check)` and the elaborated operand is kept, so the returned `Intrinsic` is the authoritative (rebuilt) one that flows on to `zonk`/`erase`.
/// Rebuild `intrinsic` with its first `done.len()` traversed operands replaced, leaving the rest as written.
///
/// The partial rebuild is what makes the walk below streaming rather than one pass: an operand's declared type may mention an operand before it — `List/get`'s list is at `List(element)`, a division's bound at `0 < divisor` — and elaboration *changes* what it touches, so a type read off the un-elaborated node would be checked against a spelling that no longer exists.
fn with_elaborated(intrinsic: &Intrinsic, done: Vec<Term>) -> Intrinsic {
    // Taken by value because the rewrite hook is boxed `'static` and cannot borrow what it yields.
    let mut ready = done.into_iter();

    intrinsic.traverse(&mut Visit::rewriting(
        |_, _: &Var| None,
        Box::new(move |_, _| ready.next()),
    ))
}

/// Elaborate every operand against the type this operation demands of it, and hand back the rebuilt node with its result type.
///
/// The demands are `Intrinsic::signature`'s, which is also what the kernel checks against — so elaboration and re-checking cannot disagree about an operand's type, and neither restates the other. What stays here is the half a table cannot state: `Mode` is elaboration's, a type operand is established through `check_is_sort` so a metavariable in that position gets a classifier, and a parameterized former's sort is read off the rebuilt node.
fn synth_intrinsic(
    context: &mut Context,
    intrinsic: &Intrinsic,
) -> Result<(Intrinsic, Term), Error> {
    let mut done: Vec<Term> = Vec::new();

    loop {
        let current = with_elaborated(intrinsic, done.clone());
        let signature = current.signature(&context.syntax());
        let operands = current.operands();

        // The table and the traversal are two statements of one operand list; zipping them is only sound while they agree, and a disagreement is this crate's bug rather than the term's.
        debug_assert_eq!(
            operands.len(),
            signature.operands.len(),
            "`signature` and `operands` disagree about {intrinsic:?}",
        );

        let Some(demand) = signature.operands.get(done.len()) else {
            let type_ = match signature.produced {
                Produced::Fixed(type_) => type_,
                Produced::Sort => crate::sort_term(context, &Term::intrinsic(current.clone()))?,
            };

            return Ok((current, type_));
        };

        let operand = operands[done.len()].clone();
        done.push(match demand {
            Operand::At(type_) => elaborate(context, &operand, Mode::Check(type_.clone()))?.0,
            Operand::IsType => crate::check_is_sort(context, &operand)?.0,
            Operand::Function { domain, codomain } => {
                let binder = context.fresh(Some("x"));
                let expected = Term::func_type([(binder, domain.clone())], codomain.clone());

                elaborate(context, &operand, Mode::Check(expected))?.0
            }
        });
    }
}

pub(crate) fn elaborate_intrinsic(
    context: &mut Context,
    term: &Term,
    intrinsic: &Intrinsic,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `List` is bidirectional. Checking against a concrete `List(T)`, it borrows the element type from `expected` — definitional, so each element is checked against the known type (better errors, and numeric element literals pick the right numeric type). Any other expected shape — a stuck `?M(?A)` awaiting the flex-apply imitation included — falls through to `synth_intrinsic`, which mints a fresh element-type metavar; the check-after-infer unification then equates `List(?T)` with the expected type (pinning `?M := List`, or reporting the genuine mismatch).
    if let (
        Intrinsic::List {
            element: _,
            items: elems,
        },
        Mode::Check(expected),
    ) = (intrinsic, &mode)
        && let Subterm::Intrinsic(Intrinsic::ListType(elem_type)) =
            &*reduce_with(context, expected)?
    {
        let elaborated = check_list_elems(context, elems, elem_type)?;

        return Ok((
            Term::intrinsic(Intrinsic::List {
                element: elem_type.clone(),
                items: elaborated,
            }),
            expected.clone(),
        ));
    }

    // `ListConcat` mirrors `List`'s bidirectionality — and must, because the lowering of a spread list literal `[a, ..xs, b]` mints a fresh metavar for the element-type slot. Checking against a concrete `List(T)`, solve the slot against `expected` FIRST (`expect` unifies `List(slot)` with it), so the operands — the literal chunks especially — elaborate against the known element type instead of default-solving it from the first element. Any other expected shape falls through to `synth_intrinsic`, exactly as for `List`.
    if let (
        Intrinsic::ListConcat {
            element: type_slot,
            operands,
        },
        Mode::Check(expected),
    ) = (intrinsic, &mode)
        && let Subterm::Intrinsic(Intrinsic::ListType(_)) = &*reduce_with(context, expected)?
    {
        let type_slot = crate::check_is_sort(context, type_slot)?.0;
        expect(context, term, &list_type(type_slot.clone()), expected)?;

        let mut elaborated = Vec::with_capacity(operands.len());
        for operand in operands {
            elaborated.push(elaborate(context, operand, Mode::Check(expected.clone()))?.0);
        }

        return Ok((
            Term::intrinsic(Intrinsic::ListConcat {
                element: type_slot,
                operands: elaborated,
            }),
            expected.clone(),
        ));
    }

    let (intrinsic, type_) = synth_intrinsic(context, intrinsic)?;

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::intrinsic(intrinsic), type_))
}
