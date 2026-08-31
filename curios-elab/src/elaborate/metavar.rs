use super::*;

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
                // `into_core` mints a hole *bare* (`Term::hole`), and the birth arm below rebuilds it over its frozen Γ — but that rebuild lands in the *returned* term, not in the node the traversal read. A node reached a second time in checking position therefore arrives bare again, and returning it unchanged puts a spineless copy of a birthed hole into a compared type, which is exactly what `solve`'s spine-arity invariant forbids. Re-attach the identity spine here so both traversals hand the same term downstream.
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
            // A written goal, though, must survive to zonk's report rather than die here: a fresh silent hole stands in as its type (a hole so only the goal itself reports; the report then shows the stand-in — `?N` — if nothing ever pins it). Birth mirrors the checking arm, with the stand-in as `result`.
            None if matches!(metavar.origin, MetavarOrigin::Goal) => {
                let classifier = context.fresh_classifier_type("inferred goal classifier");
                let result = context.fresh_hole_metavar(classifier, term.span());
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

/// Whether the (reduced) expectation is headed by an unsolved metavariable — including a stuck application of one. Such an expectation may yet be solved to the reference's own hidden-headed function type, so insertion must not fire on it.
fn flexible(context: &Context, term: &Term) -> bool {
    match &**term {
        Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
        Subterm::Apply(apply) => flexible(context, &apply.head),
        _ => false,
    }
}

/// Implicit-eta on the check turnaround. A reference whose type leads with an implicit binder, checked against a concrete *explicit* function type, has its leading implicits inserted as metavariables and is eta-expanded over the remaining explicit binders — so a bare `cat : (@T, List T, List T) -> List T` is accepted where `(List B, List B) -> List B` is expected, instead of demanding `(l, r) => cat(l, r)`. Implicit insertion is otherwise an application-site mechanism (`elaborate_apply`); this is the one extension into value position. Producing a full lambda (rather than a partial application) keeps erase/CPS untouched: the output is an ordinary closure over a saturated call.
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
    if matches!(ift.plicities().first(), Some(Plicity::Explicit) | None) {
        return Ok((rebuilt, type_));
    }

    let expected_reduced = reduce_with(context, expected)?;
    let fires = match &*expected_reduced {
        Subterm::FuncType(eft) => !matches!(
            eft.plicities().first(),
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
        let mut plicities = ift.plicities().iter();
        let mut auto_premises = 0usize;
        loop {
            match tele {
                Telescope::Done(output) => break Ok(*output),
                Telescope::Cons(domain, rest) => match plicities.next() {
                    Some(&plicity @ (Plicity::Implicit | Plicity::Witness)) => {
                        let premise = super::premise_label(auto_premises);
                        if matches!(plicity, Plicity::Witness) {
                            auto_premises += 1;
                        }
                        let arg = insert_auto_argument(
                            context,
                            plicity,
                            &domain,
                            rest.first_hint(),
                            &func_label,
                            term,
                            premise,
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

/// The canonical inhabitant of `type_`, when it has exactly one and no argument is needed to build it.
///
/// **This is not proof search.** The answer is unique, so there is no choice to make, nothing to backtrack over, and no incompleteness to reason about — it is the introduction-side counterpart of the singleton condition [`singleton_eliminable`](super::match_::singleton_eliminable) applies to elimination, and of proof irrelevance, which already holds that every inhabitant of a proposition is interchangeable. This rule only says that where exactly one exists and it takes no arguments, the elaborator may write it down.
///
/// Recognised narrowly: the goal must reduce to `/syn`'s trivially true proposition. That covers every *decided* proposition — one defined as a `match` on a machine comparison, whose in-range branch is this type — which is the whole family `/sys`'s preconditions are drawn from. A user's own single-nullary-constructor proposition is not discharged, and widening to it means reading the constructor telescope apart from the declaration parameters it leads with; the narrow form needs neither and is what every caller here wants.
/// A *spent* budget is not an answer. Reduction failing to reach `True` because it ran out of steps is not evidence that the proposition is untrue, so the failure propagates instead of collapsing into `None`: the caller's fallback mints a hole, and a hole minted here reports as an uninferred implicit against a caller who may well have established the bound. That reads as the user's fault at the argument rather than as the resource limit it is, and it is the one place where an exhausted budget could be mistaken for a judgment.
pub(super) fn trivially_inhabited(
    context: &mut Context,
    type_: &Term,
) -> Result<Option<Term>, Error> {
    let truth = context.syntax().proof.true_type.qualifier();
    let qed = context.syntax().proof.true_qed.qualifier();

    let reduced = crate::reduce_with(context, type_)?;
    let Subterm::InductType(induct) = &*reduced else {
        return Ok(None);
    };

    Ok(
        (induct.name == curios_core::Global::Authored(truth)).then(|| {
            Term::apply(
                Term::var(curios_core::Var::free(Free::global(qed))),
                Vec::<Term>::new(),
            )
        }),
    )
}
