use super::*;

pub(super) fn elaborate_tuple(
    context: &mut Context,
    tuple: &Tuple,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Tuple { fields, names } = tuple;

    let expected = match mode {
        Mode::Check(expected) => expected,
        // Synthesis position: infer each field independently and form the *non-dependent* product. No field type can mention an earlier field (each is inferred in isolation), so the telescope is non-dependent — a dependent Σ-type only ever arises from a checking expectation. This is what lets an un-annotated `let (a, b) = (x, y)` infer `{ typeof x, typeof y }` instead of demanding an annotation.
        Mode::Infer => {
            let mut elaborated = Vec::with_capacity(fields.len());
            let mut field_types = Vec::with_capacity(fields.len());
            for field in fields {
                let (field, field_type) = elaborate(context, field, Mode::Infer)?;
                elaborated.push(field);
                field_types.push((context.fresh(None), field_type));
            }
            return Ok((Term::tuple(elaborated), Term::tuple_type(field_types)));
        }
    };

    let type_telescope = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::TupleType(TupleType { telescope }) => telescope,
        Subterm::Metavar(_) if !context.parking_suppressed() => {
            return park_checking(context, term, &expected);
        }
        _ => {
            return Err(Error::not_a_tuple_type(expected.clone()));
        }
    };

    if fields.len() != type_telescope.len() {
        return Err(Error::tuple_arity_mismatch(
            type_telescope.len(),
            fields.len(),
        ));
    }

    // Written field names are checked positionally against the expected type's labels and then dropped — the rebuilt literal is name-free. Reordering is deliberately not supported: in a dependent telescope the written order is the checking order.
    let labels = type_telescope.labels();
    for (position, name) in names.iter().enumerate() {
        let Some(name) = name else { continue };
        let expected_label = labels.get(position).copied().unwrap_or_default();
        if expected_label != name {
            return Err(Error::tuple_field_name_mismatch(
                name.clone(),
                expected_label.to_string(),
                position,
            ));
        }
    }

    let sources: Vec<FieldSource> = fields.iter().map(FieldSource::Written).collect();
    let mut elaborated = Vec::with_capacity(fields.len());
    check_dependent_fields(context, type_telescope, &sources, term, &mut elaborated)?;

    Ok((Term::tuple(elaborated), expected))
}

pub(super) fn elaborate_metavar(
    context: &mut Context,
    metavar: &Metavar,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let id = metavar.id;

    match mode {
        // Birth: freeze the local context as Γ and record the type the hole is checked against. Births happen once per id, but a re-traversal in the same mode is idempotent — re-check the recorded type against the (identical) `expected`.
        Mode::Check(expected) => {
            if let Some(entry) = context.metavar_entry(id) {
                let result = entry.result.clone();
                // `into_core` mints a hole *bare* (`Term::metavar`), and the birth arm below rebuilds it over its frozen Γ — but that rebuild lands in the *returned* term, not in the node the traversal read. A node reached a second time in checking position therefore arrives bare again, and returning it unchanged puts a spineless copy of a birthed hole into a compared type, which is exactly what `solve`'s spine-arity invariant forbids. Re-attach the identity spine here so both traversals hand the same term downstream.
                //
                // Only the bare case is repaired: an occurrence that already carries a spine may be carrying a *substituted* one, opened under binders the birth Γ does not have, and that is the delayed substitution the whole representation exists to keep.
                let restored =
                    (metavar.spine.is_empty() && !entry.telescope.is_empty()).then(|| {
                        entry
                            .telescope
                            .iter()
                            .map(|(name, _)| Term::free_var(name))
                            .collect::<Vec<_>>()
                    });
                expect(context, term, &result, &expected)?;

                let Some(spine) = restored else {
                    return Ok((term.clone(), expected));
                };
                let rebuilt = Term::metavar_birthed(id, metavar.origin.clone(), spine);
                let rebuilt = match term.span() {
                    Some(span) => rebuilt.with_span(span),
                    None => rebuilt,
                };
                Ok((rebuilt, expected))
            } else {
                // Rebuild the hole with the identity spine over its frozen telescope: the rebuilt term is what flows downstream, so every surviving occurrence carries the delayed substitution. Telescope and spine are the shared per-Γ snapshot.
                let (telescope, spine) = context.identity_snapshot();
                context.birth_metavar(id, telescope, expected.clone());

                let rebuilt = Term::metavar_birthed(id, metavar.origin.clone(), spine);
                let rebuilt = match term.span() {
                    Some(span) => rebuilt.with_span(span),
                    None => rebuilt,
                };
                Ok((rebuilt, expected))
            }
        }
        // A hole in synthesis position has no type to offer — unless it was already born in a checking position, in which case report that type.
        Mode::Infer => match context.metavar_entry(id) {
            Some(entry) => Ok((term.clone(), entry.result.clone())),
            // A written goal, though, must survive to zonk's report rather than die here: a fresh *unmarked* metavariable stands in as its type (unmarked so only the goal itself reports; the report then shows the stand-in — `?N` — if nothing ever pins it). Birth mirrors the checking arm, with the stand-in as `result`.
            None if matches!(metavar.origin, Some(MetavarOrigin::Goal)) => {
                let classifier = context.fresh_classifier_type("inferred goal classifier");
                let result = context.fresh_unmarked_metavar(classifier, term.span());
                let (telescope, spine) = context.identity_snapshot();
                context.birth_metavar(id, telescope, result.clone());

                let rebuilt = Term::metavar_birthed(id, metavar.origin.clone(), spine);
                let rebuilt = match term.span() {
                    Some(span) => rebuilt.with_span(span),
                    None => rebuilt,
                };
                Ok((rebuilt, result))
            }
            None => Err(Error::CannotInfer),
        },
    }
}

/// Check `args` pointwise against the dependent telescope `signature` — each arg under the earlier ones — collecting the rebuilt args and returning the telescope's terminal, opened at those args. The caller checks arity first; the arity error differs by site. The given-args counterpart to `check_telescope_entries`.
pub(super) fn check_args_against<B: Bound>(
    context: &mut Context,
    signature: Telescope<B>,
    args: &[Term],
) -> Result<(Vec<Term>, B), Error> {
    let mut elaborated = Vec::with_capacity(args.len());
    let terminal = signature.walk(args, |_, arg, ty| -> Result<(), Error> {
        elaborated.push(check(context, arg, ty.clone())?);
        Ok(())
    })?;
    Ok((elaborated, terminal))
}

/// Whether the (reduced) expectation is headed by an unsolved metavariable — including a stuck application of one. Such an expectation may yet be solved to the reference's own hidden-headed function type, so insertion must not fire on it.
fn flexible(context: &Context, term: &Term) -> bool {
    match &**term {
        Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
        Subterm::Apply(apply) => flexible(context, &apply.head),
        _ => false,
    }
}

/// Implicit-eta on the check turnaround. A reference whose type leads with an implicit binder, checked against a concrete *explicit* function type, has its leading implicits inserted as metavariables and is eta-expanded over the remaining explicit binders — so a bare `cat : (@T, Lst T, Lst T) -> Lst T` is accepted where `(Lst B, Lst B) -> Lst B` is expected, instead of demanding `(l, r) => cat(l, r)`. Implicit insertion is otherwise an application-site mechanism (`elaborate_apply`); this is the one extension into value position. Producing a full lambda (rather than a partial application) keeps erase/CPS untouched: the output is an ordinary closure over a saturated call.
///
/// Fires only for `Var`/`Proj` heads, and only where a hidden-headed function type could never convert anyway: against a ground explicit-arrow expectation, and against a *rigid non-arrow* expectation — plicity is part of function identity, so both configurations are guaranteed plicity mismatches, and inserting can only rescue programs that were errors. A flexible (metavar-headed) expectation must not fire, since it may still be solved to the reference's own polymorphic type; the expected-not-hidden gate likewise preserves polymorphic-value assignment (`let f : (@z : A) -> … = …` keeps its implicit). It is purely additive: when it does not fire, or the inserted shape does not convert, behavior is as before.
pub(super) fn insert_implicits_on_check(
    context: &mut Context,
    term: &Term,
    rebuilt: Term,
    type_: Term,
    expected: &Term,
) -> Result<(Term, Term), Error> {
    if !matches!(&**term, Subterm::Var(_) | Subterm::Proj(_)) {
        return Ok((rebuilt, type_));
    }

    let inferred = reduce_with(context, &type_)?;
    let Subterm::FuncType(ift) = &*inferred else {
        return Ok((rebuilt, type_));
    };
    if matches!(ift.plicities.first(), Some(Plicity::Explicit) | None) {
        return Ok((rebuilt, type_));
    }

    let expected_reduced = reduce_with(context, expected)?;
    let fires = match &*expected_reduced {
        Subterm::FuncType(eft) => !matches!(
            eft.plicities.first(),
            Some(Plicity::Implicit) | Some(Plicity::Witness)
        ),
        _ => !flexible(context, &expected_reduced),
    };
    if !fires {
        return Ok((rebuilt, type_));
    }

    let ift = ift.clone();
    let func_label = match &**term {
        Subterm::Var(var) => var.unwrap().to_string(),
        _ => "<function>".to_string(),
    };

    // Walk the head's telescope: implicit binders become fresh metavariables (the inserted arguments), witness binders fresh metavariables with resolution goals, explicit binders fresh lambda parameters (the eta variables). `head_args` records all in telescope order so the body re-applies the head fully saturated; `open` threads the dependent substitution so a later binder mentioning an earlier one is instantiated.
    let mut head_args: Vec<(Plicity, Term)> = Vec::new();
    let mut binders: Vec<(Free, Term)> = Vec::new();
    let output = context.with_frame(|context| -> Result<Term, Error> {
        let mut tele = ift.telescope.clone();
        let mut plicities = ift.plicities.iter();
        loop {
            match tele {
                Telescope::Done(output) => break Ok(*output),
                Telescope::Cons(domain, rest) => match plicities.next() {
                    Some(&plicity @ (Plicity::Implicit | Plicity::Witness)) => {
                        let arg = insert_auto_argument(
                            context,
                            plicity,
                            &domain,
                            rest.first_hint(),
                            &func_label,
                            term,
                        )?;
                        tele = rest.open(&[&arg]);
                        head_args.push((plicity, arg));
                    }
                    Some(Plicity::Explicit) => {
                        let label = context.fresh(rest.first_hint());
                        context.assume(&label, &domain);
                        let var = Term::free_var(&label);
                        tele = rest.open(&[&var]);
                        binders.push((label, domain));
                        head_args.push((Plicity::Explicit, var));
                    }
                    None => unreachable!("plicities parallel the telescope"),
                },
            }
        }
    })?;

    let body = Term::apply_marked(rebuilt, head_args);

    // No explicit binders to eta over (an all-implicit curried prefix, e.g. `(@A, @B) -> …`): the implicit-saturated application *is* the value, and its type is the opened output. Erasure drops the implicit arguments anyway.
    if binders.is_empty() {
        return Ok((body, output));
    }

    let func_type = Term::func_type(binders.clone(), output);
    Ok((Term::func(binders, body), func_type))
}
