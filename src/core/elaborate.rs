use {
    super::{
        Apply, Atom, Carrier, Cases, Context, Definition, Error, Field, Func, FuncType,
        ImplicitOrigin, Inductive, InductiveParam, Invert, Item, Let, Many, Match, Metavar, Module,
        MotivePattern,
        MotiveSlot, Nat, ParkedWork, Plicity, Prim, Proj, Quantity, Rec, Scope, Struct, StructType,
        Structure,
        Subterm, Telescope, Term, Three, Tuple, TupleType, Two, InductiveType, Var, Variant,
        case_target_indices,
        check, check_motive, convert_with, drain_parked, elaborate_prim, expect, invert_indices,
        is_prop, reduce_with, refine_head, sort_term,
    },
    std::collections::{BTreeMap, BTreeSet, VecDeque},
};

/// The elaboration mode (§6). `Infer` synthesizes a type; `Check(expected)`
/// drives the term against a known type, hitting `expect` at each synthesizable
/// node's turnaround and consuming `expected` directly at naturally-checked
/// nodes (`Func`, `Tuple`, `Metavar`).
#[derive(Debug, Clone)]
pub enum Mode {
    Infer,
    Check(Term),
}

fn elaborate_func_type(context: &mut Context, ft: &FuncType) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<Term>,
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(output) => check(context, &output, Term::type_()),
            Telescope::Cons(ty, rest) => {
                let domain = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                // Assume the *rebuilt* domain: insertion saturates applications
                // during elaboration, and a lowered (under-applied) type leaking
                // into later reduction would open a telescope at the wrong arity.
                context.assume(&name, &domain);
                domains.push((name, domain));
                walk(context, rest.open(&[&x]), domains)
            }
        }
    }

    let mut domains = Vec::new();
    let output = context.with_frame(|context| walk(context, ft.telescope.clone(), &mut domains))?;

    let rebuilt = Term::func_type_quantified(
        ft.plicities
            .iter()
            .zip(&ft.quantities)
            .zip(domains)
            .map(|((&plicity, &quantity), (label, domain))| (plicity, quantity, label, domain)),
        output,
    );

    Ok((rebuilt, Term::type_()))
}

/// A binder's user-facing name. The head's function type is the *rebuilt* one,
/// whose binders were re-closed under `fresh`-minted labels (`T#1`); reports
/// should name the binder as written, and `#` cannot occur in an identifier.
fn binder_name(label: &str) -> String {
    match label.split_once('#') {
        Some(("", _)) => "_".to_string(),
        Some((name, _)) => name.to_string(),
        None => label.to_string(),
    }
}

fn elaborate_apply(
    context: &mut Context,
    apply: &Apply,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Apply {
        head,
        params,
        plicities,
    } = apply;

    // Insertion provenance: name the applied function in the uninferred-
    // implicit report. Heads are references in practice; anything else gets a
    // placeholder (the span still locates the call).
    let func_label = match &**head {
        Subterm::Var(var) => var.unwrap().to_string(),
        _ => "<function>".to_string(),
    };

    let (mut head, head_type) = elaborate(context, head, Mode::Infer)?;
    let mut head_type = reduce_with(context, &head_type)?;

    // The two call-site queues: plain arguments fill explicit binders in
    // telescope order, `@`-arguments fill implicit binders in telescope order,
    // matched independently — the relative position of an `@`-argument among
    // the plain ones carries no meaning.
    let mut plain: VecDeque<Term> = VecDeque::new();
    let mut marked: VecDeque<Term> = VecDeque::new();
    for (plicity, param) in plicities.iter().zip(params) {
        match plicity {
            Plicity::Explicit => plain.push_back(param.clone()),
            Plicity::Implicit => marked.push_back(param.clone()),
        }
    }

    // All-implicit telescopes (the curried `bind` shape, e.g.
    // `(@A, @B) -> (M A, A -> M B) -> M B`): when the head telescope has zero
    // explicit slots but plain arguments were given, saturate it — `@`-queue
    // first, fresh metavariables for the rest — reduce the output, and
    // re-target the plain arguments at the next telescope. This fires *only*
    // with zero explicit slots, so application stays arity-strict everywhere
    // else (this is deliberately not general partial application).
    let ft = loop {
        let ft = match &*head_type {
            Subterm::FuncType(ft) => ft.clone(),
            other => return Err(Error::not_a_function(other.clone())),
        };

        let all_implicit =
            !ft.plicities.is_empty() && ft.plicities.iter().all(|p| matches!(p, Plicity::Implicit));
        if !all_implicit || plain.is_empty() {
            break ft;
        }

        let mut args = Vec::with_capacity(ft.plicities.len());
        let mut tele = ft.telescope.clone();
        let output = loop {
            match tele {
                Telescope::Done(output) => break *output,
                Telescope::Cons(ty, rest) => {
                    let arg = match marked.pop_front() {
                        Some(arg) => check(context, &arg, ty.clone())?,
                        None => {
                            let binder = binder_name(rest.first_label().unwrap_or("_"));
                            context.fresh_metavar(
                                ty.clone(),
                                term.span(),
                                ImplicitOrigin {
                                    func: func_label.clone(),
                                    binder,
                                },
                            )
                        }
                    };
                    tele = rest.open(&[&arg]);
                    args.push(arg);
                }
            }
        };

        head = Term::apply_marked(head, args.into_iter().map(|a| (Plicity::Implicit, a)));
        head_type = reduce_with(context, &output)?;
    };

    // Arity is checked per queue: plain arguments must exactly cover the
    // explicit slots; `@`-arguments may undershoot the implicit slots (the
    // remainder is inserted) but never overshoot them.
    let explicit_slots = ft
        .plicities
        .iter()
        .filter(|p| matches!(p, Plicity::Explicit))
        .count();
    let implicit_slots = ft.plicities.len() - explicit_slots;

    if plain.len() != explicit_slots {
        return Err(Error::wrong_number_of_arguments(
            explicit_slots,
            plain.len(),
        ));
    }
    if marked.len() > implicit_slots {
        return Err(Error::too_many_implicits(implicit_slots, marked.len()));
    }

    // Materialize the saturated argument vector, threading the dependent
    // substitution so each inserted metavariable is born at its binder's
    // *instantiated* type. The walk below re-checks the inserted metavariables
    // idempotently (`elaborate_metavar` re-checks the recorded type).
    let mut full_args = Vec::with_capacity(ft.plicities.len());
    {
        let mut tele = ft.telescope.clone();
        for plicity in &ft.plicities {
            let Telescope::Cons(ty, rest) = tele else {
                unreachable!("plicities parallel the telescope");
            };
            let arg = match plicity {
                Plicity::Explicit => plain.pop_front().expect("arity checked above"),
                Plicity::Implicit => match marked.pop_front() {
                    Some(arg) => arg,
                    None => {
                        let binder = binder_name(rest.first_label().unwrap_or("_"));
                        context.fresh_metavar(
                            ty.clone(),
                            term.span(),
                            ImplicitOrigin {
                                func: func_label.clone(),
                                binder,
                            },
                        )
                    }
                },
            };
            tele = rest.open(&[&arg]);
            full_args.push(arg);
        }
    }
    let params = &full_args;

    // Result-directed argument order (§6). An introduction form (tuple,
    // lambda) is checked-only: it can't be elaborated against a parameter type that
    // reduces to a bare, unsolved metavar — there is no structure to drive it. In
    // `Check` mode we postpone exactly those arguments, unify the application's
    // result type against `expected` (which pins the metavars — both those a sibling
    // argument would witness and phantom ones the expected type alone carries), then
    // re-check the postponed arguments against their now-refined types. Synthesizable
    // arguments (`Var`/`Apply`/`Proj`/literals) are never postponed: they run first
    // and feed that very unification, so this only reorders the checked-only forms
    // and is otherwise byte-for-byte the previous left-to-right walk. If the result
    // unification fails to pin a postponed argument's type, the re-check fails with
    // the same error as before — no new acceptance, graceful degradation.
    let checking = matches!(mode, Mode::Check(_));

    // The metavars the result type carries — exactly the ones `expect(output, expected)`
    // can pin. A continuation lambda whose codomain still mentions one of these is
    // postponed (see `blocked_on_metavar`) so its body is checked only after that
    // unification refines the codomain. Opening over the raw args is pure substitution
    // (no birth/solve), so this is just an early read of the result type.
    let arg_refs = params.iter().collect::<Vec<&Term>>();
    let result_metavars = ft.telescope.clone().open(&arg_refs).metavars();

    // Whether the expected type is fully ground. The codomain postponement is only a
    // win when `expect(output, expected)` actually *grounds* the result metavar; if
    // `expected` itself carries an unsolved metavar, that turnaround is flex-flex and
    // the metavar must instead be grounded by the continuation's body — so postponing
    // it would strand the metavar (flex-flex-under-constructor) rather than refine it.
    // When expected is not ground we fall back to the eager (current) behavior.
    let expected_ground = match &mode {
        Mode::Check(expected) => expected
            .metavars()
            .iter()
            .all(|&id| transitively_ground(context, id)),
        Mode::Infer => false,
    };

    // The telescope is opened with the *rebuilt* argument at every eager
    // slot, so later entry types and the output carry rebuilt spellings only
    // — a lowered copy spliced into the output would smuggle a birthed hole's
    // bare node past its rebuild (and a lowered term toward the reducer).
    // A postponed intro form stays lowered for now; its holes are unbirthed,
    // and its rebuilt form lands after the output `expect` pins its metas.
    // An argument to an erased (`Zero`) parameter sits in an erased position, so
    // it is checked at ρ scaled by the binder's quantity — this is what lets an
    // erased variable be *passed* to an erased parameter (category c).
    let quantities = ft.quantities.clone();

    // Walk the telescope, checking each argument against its (dependent) domain
    // and opening the rest with the elaborated form. A checked-only intro form
    // blocked on a metavar is postponed — its slot keeps the raw term for now —
    // but the moment a *later* synthesizable argument grounds the metavar it was
    // waiting on (e.g. `subst`'s `p : Eq(x, y)` grounds the motive's domain), it
    // is re-checked and the remaining telescope re-opened through its elaborated
    // form. Otherwise a sibling whose type mentions it (`subst`'s `v : P x`) or
    // the result (`P y`) would reduce through a raw term whose un-inserted
    // implicits (like `Eq`'s `@A`) panic the reducer. Arguments still genuinely
    // blocked at the end (a continuation awaiting a codomain metavar) are settled
    // after the result `expect`, as before.
    let original = ft.telescope.clone();
    let mut elaborated: Vec<Term> = Vec::with_capacity(params.len());
    let mut postponed: Vec<usize> = Vec::new();
    let mut tele = original.clone();
    let mut index = 0;
    let output = loop {
        let (ty, rest) = match tele {
            Telescope::Done(body) => break *body,
            Telescope::Cons(ty, rest) => (ty, rest),
        };
        let quantity = quantities.get(index).copied().unwrap_or(Quantity::Omega);
        let term = if checking
            && blocked_on_metavar(context, &params[index], &ty, &result_metavars, expected_ground)?
        {
            postponed.push(index);
            params[index].clone()
        } else {
            context.with_quantity(quantity, |context| check(context, &params[index], ty.clone()))?
        };
        elaborated.push(term);

        // Re-check any postponed argument whose block this slot just cleared.
        let mut resolved = false;
        let mut cursor = 0;
        while cursor < postponed.len() {
            let slot = postponed[cursor];
            let slot_ty = original
                .clone()
                .nth(slot, |k| elaborated[k].clone())
                .expect("postponed slot is within the telescope");
            if blocked_on_metavar(context, &params[slot], &slot_ty, &result_metavars, expected_ground)?
            {
                cursor += 1;
            } else {
                let quantity = quantities.get(slot).copied().unwrap_or(Quantity::Omega);
                elaborated[slot] =
                    context.with_quantity(quantity, |context| check(context, &params[slot], slot_ty))?;
                postponed.remove(cursor);
                resolved = true;
            }
        }

        // Re-open from the top through the (possibly updated) prefix so later
        // slot types carry the elaborated forms; otherwise just advance.
        tele = match resolved {
            false => rest.open(&[&elaborated[index]]),
            true => {
                let mut reopened = original.clone();
                for term in &elaborated {
                    reopened = match reopened {
                        Telescope::Cons(_, rest) => rest.open(&[term]),
                        Telescope::Done(_) => unreachable!("the prefix is within the telescope"),
                    };
                }
                reopened
            }
        };
        index += 1;
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &output, expected)?;
        for &slot in &postponed {
            let slot_ty = original
                .clone()
                .nth(slot, |k| elaborated[k].clone())
                .expect("postponed slot is within the telescope");
            let quantity = quantities.get(slot).copied().unwrap_or(Quantity::Omega);
            elaborated[slot] =
                context.with_quantity(quantity, |context| check(context, &params[slot], slot_ty))?;
        }
    }

    // The rebuilt application is fully saturated; each argument's mark is its
    // binder's plicity (inserted metavariables recorded like any other
    // argument), so re-elaborating the rebuilt node is stable: both queues
    // then match their slots exactly and nothing is minted twice.
    Ok((
        Term::apply_marked(head, ft.plicities.iter().copied().zip(elaborated)),
        output,
    ))
}

/// Whether `arg` is a checked-only introduction form (tuple, lambda, array
/// literal) that cannot be elaborated yet because the type structure it needs is
/// an unsolved metavar — a tuple or array literal whose whole expected type, or a
/// lambda whose expected *domain*, reduces to one. (A lambda only needs its domain
/// known: the body, which may project the parameter, can't be checked against an
/// unknown domain; its codomain may stay a metavar. An array literal borrows its
/// element type from `expected`, so it needs the expected head — `Arr _` — to be
/// known.) Synthesizable forms return `false`: they have a turnaround of their own
/// and must run eagerly so their solutions feed the result unification.
fn blocked_on_metavar(
    context: &mut Context,
    arg: &Term,
    ty: &Term,
    result_metavars: &BTreeSet<usize>,
    expected_ground: bool,
) -> Result<bool, Error> {
    let is_lambda = matches!(&**arg, Subterm::Func(_));
    let is_arr = matches!(&**arg, Subterm::Prim(Prim::Arr(_)));
    let is_tuple = matches!(&**arg, Subterm::Tuple(_));
    if !is_lambda && !is_arr && !is_tuple {
        return Ok(false);
    }
    let reduced = reduce_with(context, ty)?;
    Ok(match &*reduced {
        // A tuple/array/lambda whose whole expected type is an unsolved metavar.
        Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
        Subterm::FuncType(FuncType { telescope, .. }) if is_lambda => match telescope {
            Telescope::Cons(domain, _) => {
                // A lambda whose expected *domain* is an unsolved metavar: its body may
                // need the domain's structure (to project the parameter), so postpone it
                // until a sibling argument (e.g. `p : Parse(A)`) pins the domain.
                let domain_blocked = match &*reduce_with(context, domain)? {
                    Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
                    _ => false,
                };
                // ...or a lambda whose *codomain* still carries an unsolved metavar that
                // the result type will pin: postpone until `expect(output, expected)`
                // solves it, so the body is checked against the refined codomain. This is
                // the `let !`-continuation case — `(x) => …` checked against
                // `?dom => Parse(?B)`, where `?dom` is already pinned by the bind's action
                // but `?B` (the bind's own result type) is solved only by the turnaround.
                // Gating on `result_metavars` keeps it to metavars `expect` will address;
                // gating on `expected_ground` ensures that turnaround actually grounds
                // `?B` (vs. a flex-flex alias that the eager body must ground instead).
                domain_blocked
                    || (expected_ground
                        && reduced.metavars().iter().any(|id| {
                            result_metavars.contains(id) && context.metavar_solution(*id).is_none()
                        }))
            }
            Telescope::Done(_) => false,
        },
        _ => false,
    })
}

/// Whether metavar `id` is solved *all the way down*: solved, and every metavar in its
/// solution is itself transitively ground. `metavar_solution` only sees one level, and
/// a solution can still embed unsolved metavars, so `expected_ground` needs this
/// transitive view to be sure the turnaround will actually pin a result metavar rather
/// than alias it flex-flex. Terminates: the occurs check forbids cyclic solutions.
fn transitively_ground(context: &Context, id: usize) -> bool {
    match context.metavar_solution(id) {
        None => false,
        Some(solution) => solution
            .metavars()
            .iter()
            .all(|&inner| transitively_ground(context, inner)),
    }
}

fn elaborate_tuple_type(context: &mut Context, tt: &TupleType) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &mut Vec<(String, Term)>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let field = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                // The *rebuilt* field type, as in `elaborate_func_type`.
                context.assume(&name, &field);
                fields.push((name, field));
                walk(context, rest.open(&[&x]), fields)
            }
        }
    }

    // Labels are part of the type's identity and the target of `.label`
    // resolution, so they must be unique and survive the rebuild verbatim
    // (the walk gensyms its binders to keep nested frames collision-free;
    // `relabel` restores the written names afterwards).
    let source_labels = tt.telescope.labels();
    for (position, label) in source_labels.iter().enumerate() {
        if !label.is_empty() && source_labels[..position].contains(label) {
            return Err(Error::duplicate_tuple_label(label.to_string()));
        }
    }

    let mut fields = Vec::new();
    context.with_frame(|context| walk(context, tt.telescope.clone(), &mut fields))?;

    let telescope = Telescope::build(fields, ()).relabel(&source_labels);

    let rebuilt: Term = Subterm::TupleType(TupleType {
        telescope,
        // Field order is preserved through the walk and relabel, so the input
        // quantities still align one-for-one.
        quantities: tt.quantities.clone(),
    })
    .into();

    // A tuple type's sort: `Prop` when every field is a proposition (`{}` is the
    // base case), `Type` otherwise — so a record of proofs checks against `Prop`.
    let sort = sort_term(context, &rebuilt)?;
    Ok((rebuilt, sort))
}

/// Infer and rebuild a match scrutinee, requiring its reduced type to be the
/// given primitive type. The authoritative analogue of `expect_prim_head` (kept
/// for `erase`): it returns the rebuilt head alongside its reduced type.
fn elaborate_prim_head(
    context: &mut Context,
    head: &Term,
    expected: Prim,
) -> Result<(Term, Term), Error> {
    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    match expected {
        Prim::NatType if matches!(&*head_type, Subterm::Prim(Prim::NatType)) => {
            Ok((head, head_type))
        }
        Prim::BlnType if matches!(&*head_type, Subterm::Prim(Prim::BlnType)) => {
            Ok((head, head_type))
        }
        Prim::BinType if matches!(&*head_type, Subterm::Prim(Prim::BinType)) => {
            Ok((head, head_type))
        }
        Prim::NatType => Err(Error::not_nat_type(head_type)),
        Prim::BlnType => Err(Error::not_bln_type(head_type)),
        Prim::BinType => Err(Error::not_bin_type(head_type)),
        _ => unreachable!("elaborate_prim_head supports only NatType, BlnType and BinType"),
    }
}

/// When a match is elaborated in checking mode, solve its motive against the
/// expected type *before* the arms are checked. An omitted motive is a constant
/// scope wrapping a fresh metavar (`text::to_core::elaborate`), so `motive.open`
/// is that bare metavar and this pins it to `expected` up front — checking-only
/// arms (tuples, constructors) then see a concrete target instead of an
/// unsolved hole, and a result mentioning an enclosing type variable is taken
/// straight from `expected` rather than inverted out of an arm. For an explicit
/// motive it is the same consistency check that the `Check` turnaround would
/// otherwise run post-hoc on the match's type (`elaborate_subterm`), only earlier.
fn seed_motive(
    context: &mut Context,
    term: &Term,
    motive: &Scope<Many>,
    head: &Term,
    mode: &Mode,
) -> Result<(), Error> {
    if let Mode::Check(expected) = mode {
        expect(context, term, &motive.open(&[head]), expected)?;
    }

    Ok(())
}

fn elaborate_nat_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, Prim::NatType)?;

    // Everything below opens the *rebuilt* motive: insertion saturates
    // applications during elaboration, and a lowered (under-applied) motive
    // body reaching the reducer would open a telescope at the wrong arity.
    let motive = check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let zero_elaborated = check(
        context,
        zero_case,
        motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let succ_body = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&pred_label))]),
        );

        check(
            context,
            &succ_case.open(&[
                &Term::var(Var::free(&pred_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::var(Var::free(&pred_label)),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
        )
    })?;

    let succ_elaborated =
        Scope::close(Two, &[pred_label.as_str(), ih_label.as_str()], succ_body);

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        // `Nat` is the free monoid on one payload-less generator: its cons arm binds
        // just (predecessor, ih), so the carrier is `Nat` and the head is absent.
        cases: Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: zero_elaborated,
                cons_case: succ_elaborated,
            },
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_arr_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Arr` carries an element type the eliminator must read off the scrutinee
    // (unlike `Nat`, whose carrier is parameterless) — infer the head, then
    // demand its type is `Arr(elem)`.
    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;
    let elem = match &*head_type {
        Subterm::Prim(Prim::ArrType(elem)) => elem.clone(),
        _ => return Err(Error::not_arr_type(head_type)),
    };

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &head_type, motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let empty_value: Term = Subterm::Prim(Prim::Arr(vec![])).into();
    let empty_elaborated = check(context, empty_case, motive.open(&[&empty_value]))?;

    let head_label = context.fresh(cons_case.first_label());
    let tail_label = context.fresh(cons_case.second_label());
    let ih_label = context.fresh(cons_case.third_label());

    let cons_body = context.with_frame(|context| {
        context.assume(&head_label, &elem);
        context.assume(&tail_label, &head_type);
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&tail_label))]),
        );

        // The cons value `head :: tail`, encoded as the monoid operation on a
        // singleton and the tail (no separate prepend primitive).
        let cons_value: Term = Subterm::Prim(Prim::ArrConcat(
            elem.clone(),
            vec![
                Subterm::Prim(Prim::Arr(vec![Term::var(Var::free(&head_label))])).into(),
                Term::var(Var::free(&tail_label)),
            ],
        ))
        .into();

        check(
            context,
            &cons_case.open(&[
                &Term::var(Var::free(&head_label)),
                &Term::var(Var::free(&tail_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            motive.open(&[&cons_value]),
        )
    })?;

    let cons_elaborated = Scope::close(
        Three,
        &[
            head_label.as_str(),
            tail_label.as_str(),
            ih_label.as_str(),
        ],
        cons_body,
    );

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::FreeMonoid {
            carrier: Carrier::Arr {
                elem,
                empty_case: empty_elaborated,
                cons_case: cons_elaborated,
            },
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_bin_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Bin` is a parameterless carrier (like `Nat`/`Bln`), so the scrutinee's type
    // is just `Bin` — no element type to read off the head as `Arr` needs.
    let (head_elaborated, head_type) = elaborate_prim_head(context, head, Prim::BinType)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &head_type, motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let empty_value: Term = Subterm::Prim(Prim::Bin(vec![])).into();
    let empty_elaborated = check(context, empty_case, motive.open(&[&empty_value]))?;

    let head_label = context.fresh(cons_case.first_label());
    let tail_label = context.fresh(cons_case.second_label());
    let ih_label = context.fresh(cons_case.third_label());

    let cons_body = context.with_frame(|context| {
        // A `Bin`'s generator is a single byte, typed as `Nat`.
        context.assume(&head_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(&tail_label, &head_type);
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&tail_label))]),
        );

        // The cons value `head :: tail`, encoded as the monoid operation on the
        // singleton `[head]` and the tail. A `Bin` literal holds only concrete
        // bytes, so the singleton of the symbolic byte `head` is `append(\\, head)`
        // (a byte appended to the empty bytestring), not a `Bin` literal.
        let singleton: Term = Subterm::Prim(Prim::BinAppend(
            Subterm::Prim(Prim::Bin(vec![])).into(),
            Term::var(Var::free(&head_label)),
        ))
        .into();
        let cons_value: Term = Subterm::Prim(Prim::BinConcat(vec![
            singleton,
            Term::var(Var::free(&tail_label)),
        ]))
        .into();

        check(
            context,
            &cons_case.open(&[
                &Term::var(Var::free(&head_label)),
                &Term::var(Var::free(&tail_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            motive.open(&[&cons_value]),
        )
    })?;

    let cons_elaborated = Scope::close(
        Three,
        &[
            head_label.as_str(),
            tail_label.as_str(),
            ih_label.as_str(),
        ],
        cons_body,
    );

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::FreeMonoid {
            carrier: Carrier::Bin {
                empty_case: empty_elaborated,
                cons_case: cons_elaborated,
            },
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_switch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, Prim::NatType)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let mut cases_elaborated = BTreeMap::new();
    for (n, body) in cases {
        let body = context.with_frame(|context| {
            refine_head(
                context,
                head,
                &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
            );
            check(
                context,
                body,
                motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]),
            )
        })?;
        cases_elaborated.insert(*n, body);
    }

    let default_elaborated = check(context, default, motive.open(&[&head_elaborated]))?;

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::Switch {
            cases: cases_elaborated,
            default: default_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    match cases {
        Cases::Bln {
            false_case,
            true_case,
        } => elaborate_bln_match(context, head, motive, false_case, true_case, term, mode),
        Cases::Switch { cases, default } => {
            elaborate_switch(context, head, motive, cases, default, term, mode)
        }
        Cases::Inductive { cases, pattern } => {
            elaborate_inductive_match(context, head, motive, cases, pattern.as_ref(), term, mode)
        }
        Cases::FreeMonoid {
            carrier:
                Carrier::Nat {
                    empty_case,
                    cons_case,
                },
        } => elaborate_nat_match(context, head, motive, empty_case, cons_case, term, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::Arr {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_arr_match(context, head, motive, empty_case, cons_case, term, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bin {
                    empty_case,
                    cons_case,
                },
        } => elaborate_bin_match(context, head, motive, empty_case, cons_case, term, mode),
    }
}

#[allow(clippy::too_many_arguments)]
fn elaborate_bln_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, Prim::BlnType)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &Subterm::Prim(Prim::BlnType).into(), motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let false_elaborated = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into());
        check(
            context,
            false_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let true_elaborated = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into());
        check(
            context,
            true_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::Bln {
            false_case: false_elaborated,
            true_case: true_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_proj(context: &mut Context, proj: &Proj) -> Result<(Term, Term), Error> {
    let Proj { head, field } = proj;

    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    // Both tuples and structs project; the field telescope is the tuple type's
    // own, or the struct's (instantiated at the head type's parameters). The
    // per-field quantities ride alongside (relevance check below). A struct
    // additionally enforces representation privacy here (§7).
    let (telescope, quantities) = match &*head_type {
        Subterm::TupleType(TupleType {
            telescope,
            quantities,
        }) => (telescope.clone(), quantities.clone()),
        Subterm::StructType(StructType { name, params }) => {
            let Some(structure) = context.structure(name).cloned() else {
                return Err(Error::unbound_variable(Term::var(Var::free(name))));
            };

            // The use-site module is the enclosing item's qualifier prefix
            // (`Context::island`, set per item by `elaborate_module`);
            // the island model grants no descendant access, so the check is
            // exact qualifier equality.
            if !structure.rep_public && context.island() != structure.module {
                let field = match field {
                    Field::Index(index) => index.to_string(),
                    Field::Label(label) => label.clone(),
                };
                return Err(Error::private_field(name.clone(), field));
            }

            (structure.fields_at(params), structure.field_quantities.clone())
        }
        other => return Err(Error::not_a_tuple(other.clone())),
    };

    // A label projection resolves to its position here and is rebuilt
    // positionally — nothing below elaboration ever sees a label. Lookup is
    // unambiguous because duplicate labels are rejected when the tuple type
    // itself elaborates.
    let index = match field {
        Field::Index(index) => *index,
        Field::Label(label) => {
            let labels = telescope.labels();
            match labels.iter().position(|l| l == label) {
                Some(index) => index,
                None => {
                    return Err(Error::unknown_tuple_label(
                        label.clone(),
                        labels
                            .iter()
                            .filter(|l| !l.is_empty())
                            .map(|l| l.to_string())
                            .collect(),
                    ));
                }
            }
        }
    };

    if index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(index, telescope.len()));
    }

    // Relevance (QTT): an erased field is dropped from the runtime rep, so
    // projecting it in a runtime (ω) position is the same leak the `Var` check
    // catches for binders — it would read past the collapsed value. Empty
    // `quantities` (legacy all-ω construction) never trips this.
    if quantities.get(index) == Some(&Quantity::Zero)
        && context.current_quantity() == Quantity::Omega
    {
        return Err(Error::erased_variable_at_runtime(Term::proj(
            head.clone(),
            index,
        )));
    }

    let field_type = telescope
        .nth(index, |j| Term::proj(head.clone(), j))
        .expect("index in range");

    Ok((Term::proj(head, index), field_type))
}

/// Type a primitive inductive type against its registry entry: the parameters
/// and indices are checked pointwise (dependently) as one flat argument list
/// through the declaration's full index telescope (whose leading binders are
/// the parameters), and the whole node is a `Type`.
fn elaborate_inductive_type(context: &mut Context, ut: &InductiveType) -> Result<(Term, Term), Error> {
    let InductiveType {
        name,
        params,
        indices,
    } = ut;

    let Some(inductive) = context.inductive(name).cloned() else {
        return Err(Error::unbound_variable(Term::var(Var::free(name))));
    };

    let args: Vec<Term> = params.iter().chain(indices.iter()).cloned().collect();

    if args.len() != inductive.indices.len() {
        return Err(Error::wrong_number_of_arguments(
            inductive.indices.len(),
            args.len(),
        ));
    }

    let mut elaborated = Vec::with_capacity(args.len());
    inductive.indices.walk(&args, |_, arg, ty| {
        elaborated.push(check(context, arg, ty.clone())?);
        Ok(())
    })?;

    Ok((
        Term::inductive_type(
            name,
            elaborated[..params.len()].iter().cloned(),
            elaborated[params.len()..].iter().cloned(),
        ),
        inductive.result_sort,
    ))
}

/// Type a primitive constructor value against its registry signature: the
/// instantiated parameters and the payload are checked through the
/// constructor's full telescope, whose terminal gives the constructed
/// `InductiveType`.
fn elaborate_variant(
    context: &mut Context,
    uc: &Variant,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let Variant {
        name,
        params,
        tag,
        payload,
    } = uc;

    let Some(inductive) = context.inductive(name).cloned() else {
        return Err(Error::unbound_variable(Term::var(Var::free(name))));
    };

    let Some(signature) = inductive.constructors.get(tag).map(|c| c.telescope.clone()) else {
        return Err(Error::match_case_missing(term.clone(), tag.clone()));
    };

    let args: Vec<Term> = params.iter().chain(payload.iter()).cloned().collect();

    if args.len() != signature.len() {
        return Err(Error::wrong_number_of_arguments(
            signature.len(),
            args.len(),
        ));
    }

    // Payload quantities (params excluded); an arg to an erased payload field
    // sits in an erased position, so it is checked at ρ scaled by that quantity
    // — this lets an erased variable be injected into an erased field
    // (category c, the variant analogue of `elaborate_apply`).
    let quantities = inductive
        .constructors
        .get(tag)
        .map(|c| c.quantities.clone())
        .unwrap_or_default();
    let n_params = params.len();

    let mut elaborated = Vec::with_capacity(args.len());
    let output = signature.walk(&args, |slot, arg, ty| {
        let quantity = match slot.checked_sub(n_params) {
            Some(payload_index) => {
                quantities.get(payload_index).copied().unwrap_or(Quantity::Omega)
            }
            None => Quantity::Omega,
        };
        elaborated.push(context.with_quantity(quantity, |context| check(context, arg, ty.clone()))?);
        Ok(())
    })?;

    let rebuilt = Term::variant(
        name,
        elaborated[..params.len()].iter().cloned(),
        tag.clone(),
        elaborated[params.len()..].iter().cloned(),
    );

    Ok((rebuilt, output))
}

/// Type a struct type against its registry entry: the parameters are checked
/// pointwise (dependently) through the parameter telescope, and the whole node
/// is a `Type`. The struct analogue of `elaborate_inductive_type`, with no indices.
///
/// An *empty* parameter list on a parameterized struct is the inferred-head form
/// (the type struct destructuring gives its temp): mint one fresh metavariable
/// per declared parameter, exactly as the bare-name struct literal does
/// (`elaborate_struct`), so the head can be solved by unification against the
/// scrutinee's type.
fn elaborate_struct_type(
    context: &mut Context,
    st: &StructType,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let StructType { name, params } = st;

    let Some(structure) = context.structure(name).cloned() else {
        return Err(match context.assumption(name) {
            Some(found) => Error::not_a_struct_type(found.clone()),
            None => Error::unbound_variable(Term::var(Var::free(name))),
        });
    };

    if params.is_empty() && !structure.params.is_empty() {
        let mut resolved = Vec::with_capacity(structure.params.len());
        let mut tele = structure.params.clone();
        while let Telescope::Cons(ty, rest) = tele {
            let binder = binder_name(rest.first_label().unwrap_or("_"));
            let arg = context.fresh_metavar(
                ty.clone(),
                term.span(),
                ImplicitOrigin {
                    func: name.clone(),
                    binder,
                },
            );
            tele = rest.open(&[&arg]);
            resolved.push(arg);
        }
        return Ok((Term::struct_type(name, resolved), structure.result_sort.clone()));
    }

    if params.len() != structure.params.len() {
        return Err(Error::struct_arity_mismatch(
            name.clone(),
            structure.params.len(),
            params.len(),
        ));
    }

    let mut elaborated = Vec::with_capacity(params.len());
    structure.params.walk(params, |_, arg, ty| {
        elaborated.push(check(context, arg, ty.clone())?);
        Ok(())
    })?;

    Ok((Term::struct_type(name, elaborated), structure.result_sort))
}

/// Type a struct literal against its registry entry (§3.3). The struct's `name`
/// makes it self-describing, so this synthesizes (like `elaborate_variant`,
/// not the purely-checked `elaborate_tuple`): the parameters come from the
/// written head — a bare-name head mints one fresh metavariable per parameter,
/// solved by the field checks (and, in `Check` mode, the `expect` turnaround
/// unifying the result `StructType` against the expected type) — and the fields
/// are checked in declaration order through the (dependent) field telescope.
fn elaborate_struct(context: &mut Context, s: &Struct, term: &Term) -> Result<(Term, Term), Error> {
    let Struct {
        name,
        params,
        fields,
        names,
    } = s;

    let Some(structure) = context.structure(name).cloned() else {
        return Err(match context.assumption(name) {
            Some(found) => Error::not_a_struct_type(found.clone()),
            None => Error::unbound_variable(Term::var(Var::free(name))),
        });
    };

    // Construction privacy (§7): a private-representation struct may be built
    // only within its declaring module. Checked here (alongside projection
    // privacy in `elaborate_proj`) via `island`, set per item by
    // `elaborate_module`.
    if !structure.rep_public && context.island() != structure.module {
        return Err(Error::private_representation(name.clone()));
    }

    // A written-but-wrong parameter count is an error; an *empty* list is the
    // bare-name head, which mints one fresh metavariable per parameter.
    if !params.is_empty() && params.len() != structure.params.len() {
        return Err(Error::struct_arity_mismatch(
            name.clone(),
            structure.params.len(),
            params.len(),
        ));
    }

    // Resolve the parameters, threading the (dependent) parameter telescope so
    // each minted metavariable is born at its binder's instantiated type.
    let mut written = params.iter();
    let mut resolved = Vec::with_capacity(structure.params.len());
    let mut tele = structure.params.clone();
    while let Telescope::Cons(ty, rest) = tele {
        let arg = match written.next() {
            Some(arg) => check(context, arg, ty.clone())?,
            None => {
                let binder = binder_name(rest.first_label().unwrap_or("_"));
                context.fresh_metavar(
                    ty.clone(),
                    term.span(),
                    ImplicitOrigin {
                        func: name.clone(),
                        binder,
                    },
                )
            }
        };
        tele = rest.open(&[&arg]);
        resolved.push(arg);
    }

    // Instantiate the field telescope at the resolved parameters.
    let field_telescope = structure.fields_at(&resolved);

    if fields.len() != field_telescope.len() {
        return Err(Error::wrong_number_of_fields(
            name.clone(),
            field_telescope.len(),
            fields.len(),
        ));
    }

    // Written field names are checked positionally against the declared labels
    // and then dropped — the rebuilt literal is name-free. Reordering is not
    // supported: in a dependent telescope the written order is the check order.
    let labels = field_telescope.labels();
    for (position, written) in names.iter().enumerate() {
        let Some(written) = written else { continue };
        let declared = labels.get(position).copied().unwrap_or_default();
        if declared != written {
            return Err(Error::unknown_struct_field(
                name.clone(),
                written.clone(),
                labels
                    .iter()
                    .filter(|l| !l.is_empty())
                    .map(|l| l.to_string())
                    .collect(),
            ));
        }
    }

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        quantities: &[Quantity],
        idx: usize,
        elaborated: &mut Vec<Term>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                // Open the rest of the dependent telescope with the *elaborated*
                // field, not the raw surface term: the elaborated form carries
                // label projections rebuilt positionally (and implicits inserted),
                // whereas a raw `Field::Label` substituted into a later field type
                // would panic once that type is reduced (e.g. `Task(b.A)` arising
                // from a field typed `Task(A)` in `{ A : Type, t : Task(A) }`).
                // An erased field's value sits in an erased position (ρ scaled by
                // its quantity), so an erased binder may fill it.
                let quantity = quantities.get(idx).copied().unwrap_or(Quantity::Omega);
                let head = context.with_quantity(quantity, |context| check(context, &fields[0], ty))?;
                let rest = rest.open(&[&head]);
                elaborated.push(head);
                walk(context, rest, &fields[1..], quantities, idx + 1, elaborated)
            }
        }
    }

    let mut elaborated = Vec::with_capacity(fields.len());
    walk(
        context,
        field_telescope,
        fields,
        &structure.field_quantities,
        0,
        &mut elaborated,
    )?;

    Ok((
        Term::struct_(name, resolved.clone(), elaborated),
        Term::struct_type(name, resolved),
    ))
}

/// The primitive eliminator's typing rule. Arm binders are
/// typed directly from the constructor's registry telescope instantiated at
/// the scrutinee type's parameters — no projections from a stuck payload —
/// and each arm's binder count is statically checked against that telescope.
///
/// With a plain motive (no pattern) the discipline is constant/scrutinee-only:
/// arms check against `motive(variant)`, the match has type `motive(head)`,
/// and any indices ride along inertly. The annotated type-pattern motive
/// (Rung A of the indexed-inductive ladder) additionally binds the scrutinee's
/// indices: each arm checks against the motive at *that case's* target
/// indices, and the whole match types at the scrutinee's *actual* indices.
#[allow(clippy::too_many_arguments)]
/// Whether a single-constructor proposition admits large elimination: every
/// payload binder must be non-informative — a proposition itself, or *forced* by
/// appearing in the constructor's index targets (recovered from the scrutinee's
/// type, as `Eq`'s `refl(z) : (z, z)` recovers `z` from its indices).
fn singleton_eliminable(
    context: &mut Context,
    inductive: &Inductive,
    tag: &Atom,
    params: &[Term],
) -> Result<bool, Error> {
    let Some(payload) = inductive.instantiate(tag, params) else {
        return Ok(false);
    };

    // Open the payload telescope under fresh binders, collecting each binder's
    // (name, type) and the terminal whose indices are the constructor's targets.
    let mut binders: Vec<(String, Term)> = Vec::new();
    let mut telescope = payload;
    let terminal = loop {
        match telescope {
            Telescope::Cons(ty, rest) => {
                let name = context.fresh(rest.first_label());
                telescope = rest.open(&[&Term::var(Var::free(&name))]);
                binders.push((name, ty));
            }
            Telescope::Done(terminal) => break *terminal,
        }
    };

    // A binder is forced iff it occurs in the terminal's index expressions.
    let forced = terminal.free_vars();
    for (name, ty) in &binders {
        if !forced.contains(name) && !is_prop(context, ty)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn elaborate_inductive_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    pattern: Option<&MotivePattern>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A match with no arms is a vacuous elimination — either of an empty inductive
    // (`Void`) or of one whose every constructor inversion-clashes at the
    // scrutinee's indices. Such a match compiles to unreachable code that never
    // inspects the scrutinee, so the scrutinee occupies an *erased* position: an
    // erased witness of falsity (e.g. an erased `Void` threaded from a proof) may
    // discharge it into a relevant result. A match with arms still inspects the
    // scrutinee at the ambient resource.
    let head_quantity = match cases.is_empty() {
        true => Quantity::Zero,
        false => Quantity::Omega,
    };
    let (head_elaborated, head_type) =
        context.with_quantity(head_quantity, |context| elaborate(context, head, Mode::Infer))?;
    let head_type = reduce_with(context, &head_type)?;

    let (name, params, actual_indices) = match &*head_type {
        Subterm::InductiveType(InductiveType {
            name,
            params,
            indices,
        }) => (name.clone(), params.clone(), indices.clone()),
        other => return Err(Error::not_a_inductive_type(other.clone())),
    };

    let Some(inductive) = context.inductive(&name).cloned() else {
        return Err(Error::unbound_variable(Term::var(Var::free(&name))));
    };

    let (motive_elaborated, pattern_elaborated, plan) = match pattern {
        None => (check_motive(context, &head_type, motive)?, None, vec![]),
        Some(pattern) => {
            let (motive_elaborated, pattern_elaborated, plan) =
                check_inductive_motive(context, &inductive, &name, &params, motive, pattern)?;
            (motive_elaborated, Some(pattern_elaborated), plan)
        }
    };

    // The match's own type: the motive at the scrutinee itself — and, for a
    // pattern motive, at the scrutinee's actual parameters and indices. Opened
    // from the *rebuilt* motive, as in `elaborate_nat_match`.
    let result_args = plan
        .iter()
        .map(|slot| match slot {
            SlotPlan::Param(i) => params[*i].clone(),
            SlotPlan::Index(j) => actual_indices[*j].clone(),
        })
        .collect::<Vec<_>>();
    let result_refs = result_args
        .iter()
        .chain([&head_elaborated])
        .collect::<Vec<_>>();
    let result_type = motive_elaborated.open(&result_refs);

    // The seed (`seed_motive`'s job, generalized over the pattern binders):
    // in checking mode, pin the motive — a bare metavar when elided — to the
    // expected type before the arms are checked.
    if let Mode::Check(expected) = &mode {
        expect(context, term, &result_type, expected)?;
    }

    // Large-elimination guard: a strict proposition may not be eliminated into a
    // relevant (data) result — observing which inhabitant it was would break
    // proof irrelevance. Permitted only by *empty* elimination (no constructors)
    // or *singleton* elimination (one constructor whose payload is entirely
    // non-informative: each binder a proposition, or forced by the indices).
    if is_prop(context, &head_type)? && !is_prop(context, &result_type)? {
        // Count the *covered* constructors (the written arms), not the
        // inductive's global ones: index inversion may leave only one applicable
        // — a `cont`-scan `Utf8` admits only `more` — making an otherwise
        // multi-constructor proposition singleton in this context. (An
        // under-covered match is still caught by the coverage check below.)
        let permitted = match cases.len() {
            0 => true,
            1 => {
                let tag = cases.keys().next().expect("one covered constructor");
                singleton_eliminable(context, &inductive, tag, &params)?
            }
            _ => false,
        };
        if !permitted {
            return Err(Error::large_elim_of_prop(name.clone()));
        }
    }

    // Every written arm must name a constructor; coverage is decided per
    // constructor below — a missing arm is legal iff inversion proves it
    // impossible (Rung C).
    if cases
        .keys()
        .any(|tag| !inductive.constructors.contains_key(tag))
    {
        return Err(Error::match_arity_mismatch(
            inductive.constructors.len(),
            cases.len(),
        ));
    }

    let mut cases_elaborated = BTreeMap::new();
    for tag in inductive.constructors.keys() {
        let Some(scope) = cases.get(tag) else {
            // An unindexed inductive has nothing to invert: every arm is
            // reachable and a missing one is plainly missing.
            if actual_indices.is_empty() {
                return Err(Error::match_case_missing(term.clone(), tag.clone()));
            }

            // Rung C — checker-verified omission: a missing arm is accepted
            // iff first-order inversion of the scrutinee's actual indices
            // against this case's targets finds a *definite* clash. The arm
            // is then pruned (erase fills its slot with an unreachable
            // body); anything short of definite keeps the arm mandatory.
            let telescope = inductive
                .instantiate(tag, &params)
                .expect("constructor instantiates at its inductive's parameters");

            let labels = (0..telescope.len())
                .map(|_| context.fresh(None))
                .collect::<Vec<_>>();
            let vars = labels
                .iter()
                .map(|label| Term::var(Var::free(label)))
                .collect::<Vec<_>>();
            let ix_c = case_target_indices(telescope, &vars);

            match invert_indices(context, &actual_indices, &ix_c, &labels)? {
                Invert::Impossible => continue,
                Invert::Solved(_) => {
                    return Err(Error::missing_arm_not_impossible(tag.clone()));
                }
            }
        };

        let telescope = inductive
            .instantiate(tag, &params)
            .expect("constructor instantiates at its inductive's parameters");

        // Static arity check: the arm's binder count must equal the
        // constructor's payload arity.
        let arity = telescope.len();
        if scope.arity() != arity {
            return Err(Error::ctor_arity_mismatch(
                tag.clone(),
                arity,
                scope.arity(),
            ));
        }

        // Open the telescope with fresh names paralleling the arm's binder
        // labels; each binder is assumed at its declared (dependent) type.
        let hints = scope
            .label_iter()
            .map(|l| l.map(str::to_string))
            .collect::<Vec<_>>();
        let labels = hints
            .iter()
            .map(|hint| context.fresh(hint.as_deref()))
            .collect::<Vec<_>>();
        let vars = labels
            .iter()
            .map(|label| Term::var(Var::free(label)))
            .collect::<Vec<_>>();

        // Payload quantities, aligned with the arm's binders — an erased
        // payload binder is assumed at `Zero`, so referencing it in a runtime
        // position of the arm body is caught by the `Var` check (it is dropped
        // from the variant tuple, so a runtime use would dangle).
        let arm_quantities = inductive
            .constructors
            .get(tag)
            .map(|c| c.quantities.clone())
            .unwrap_or_default();

        let body_elaborated = context.with_frame(|context| {
            let mut telescope = telescope;
            for (i, (label, var)) in labels.iter().zip(&vars).enumerate() {
                match telescope {
                    Telescope::Cons(ty, rest) => {
                        let quantity =
                            arm_quantities.get(i).copied().unwrap_or(Quantity::Omega);
                        context.assume_quantified(label, &ty, quantity);
                        telescope = rest.open(&[var]);
                    }
                    Telescope::Done(_) => unreachable!("arity checked above"),
                }
            }

            // This case's target indices: the terminal of its (instantiated,
            // opened) signature states them over the payload binders.
            let ix_c = match &telescope {
                Telescope::Done(terminal) => match &***terminal {
                    Subterm::InductiveType(InductiveType { indices, .. }) => indices.clone(),
                    _ => unreachable!("constructor terminal is its inductive type"),
                },
                Telescope::Cons(..) => unreachable!("arity checked above"),
            };

            // Refinement propagates `head := ctor_val` to other occurrences of
            // the scrutinee in the arm body; the binder types themselves came
            // from the telescope above.
            let ctor_val = Term::variant(name.clone(), params.clone(), tag.clone(), vars.clone());
            refine_head(context, head, &ctor_val);

            // Rung B — definitional learning: a scrutinee index that is a
            // stable key (`refine_head`'s Var/Proj restriction) reduces,
            // inside this arm, to the case's target index — the same
            // counterfactual, frame-scoped move as `head := ctor_val`.
            // Refinements never justify the typing (the motive application
            // does); they are convertibility aids, so context hypotheses
            // mentioning the key reduce at the arm's index.
            for (actual, target) in actual_indices.iter().zip(&ix_c) {
                refine_head(context, actual, target);
            }

            // Rung C — inversion, arm side: a scrutinee index in constructor
            // form pins arm binders to forced values (`m + 1 ~ n + 1` pins
            // `m := n`), registered as the same frame-scoped reducts. A
            // definite clash here means the arm is unreachable; it was
            // written, so it is simply checked as is.
            if let Invert::Solved(solutions) =
                invert_indices(context, &actual_indices, &ix_c, &labels)?
            {
                for (label, solution) in solutions {
                    context.refine(&label, &solution);
                }
            }

            // The motive at this case: pattern binders take the actual
            // parameters and the case's target indices; the scrutinee takes
            // the constructed value.
            let arm_args = plan
                .iter()
                .map(|slot| match slot {
                    SlotPlan::Param(i) => params[*i].clone(),
                    SlotPlan::Index(j) => ix_c[*j].clone(),
                })
                .collect::<Vec<_>>();
            let arm_refs = arm_args.iter().chain([&ctor_val]).collect::<Vec<_>>();
            let expected = motive_elaborated.open(&arm_refs);

            let var_refs = vars.iter().collect::<Vec<_>>();
            check(context, &scope.open(&var_refs), expected)
        })?;

        let label_strs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        cases_elaborated.insert(
            tag.clone(),
            Scope::close(Many(arity), &label_strs, body_elaborated),
        );
    }

    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive: motive_elaborated,
        cases: Cases::Inductive {
            cases: cases_elaborated,
            pattern: pattern_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// How each binder of an annotated motive scope (scrutinee excluded) maps to
/// the inductive's flat argument list: a parameter position (opened with the
/// actual parameter everywhere) or an index position (opened with the case's
/// target index in arms, the scrutinee's actual index for the match itself).
enum SlotPlan {
    Param(usize),
    Index(usize),
}

/// Check the annotated type-pattern motive of an inductive match: validate the
/// pattern against the registry (parameter slots verbatim-or-binder, index
/// slots binder-only), then check the motive body as a type family over the
/// index binders and the scrutinee.
///
/// Index binders are assumed at the registry's index telescope instantiated
/// at the scrutinee's actual parameters, each later type opened with the
/// earlier binder; the scrutinee binder is assumed at
/// `InductiveType(name, params, index-vars)`. No unification anywhere — the
/// eliminator's discipline.
fn check_inductive_motive(
    context: &mut Context,
    inductive: &Inductive,
    name: &str,
    params: &[Term],
    motive: &Scope<Many>,
    pattern: &MotivePattern,
) -> Result<(Scope<Many>, MotivePattern, Vec<SlotPlan>), Error> {
    let n_params = inductive.params.len();
    let n_indices = inductive.indices.len() - n_params;

    if pattern.name != name {
        return Err(Error::motive_wrong_inductive(
            pattern.name.clone(),
            name.to_string(),
        ));
    }

    if pattern.slots.len() != n_params + n_indices {
        return Err(Error::motive_pattern_arity(
            n_params + n_indices,
            pattern.slots.len(),
        ));
    }

    // Each parameter's declared type at the actual parameters, for checking
    // verbatim slots.
    let mut param_types = Vec::with_capacity(n_params);
    inductive.params.clone().walk(params, |_, _, ty| {
        param_types.push(ty.clone());
        Ok(())
    })?;

    let mut plan = Vec::new();
    let mut slots_elaborated = Vec::new();

    for (position, slot) in pattern.slots.iter().enumerate() {
        match (slot, position < n_params) {
            // A parameter slot binder is legal — it binds the actual
            // parameter, which the scrutinee's type fixes anyway.
            (MotiveSlot::Binder, true) => {
                plan.push(SlotPlan::Param(position));
                slots_elaborated.push(MotiveSlot::Binder);
            }
            (MotiveSlot::Binder, false) => {
                plan.push(SlotPlan::Index(position - n_params));
                slots_elaborated.push(MotiveSlot::Binder);
            }
            // A verbatim parameter must be the actual parameter — written
            // out for the reader, checked for the checker.
            (MotiveSlot::Term(t), true) => {
                let elaborated = check(context, t, param_types[position].clone())?;
                if !convert_with(context, &elaborated, &params[position])? {
                    return Err(Error::motive_param_mismatch(
                        elaborated,
                        params[position].clone(),
                    ));
                }
                slots_elaborated.push(MotiveSlot::Term(elaborated));
            }
            // An index slot states nothing — it binds. Constraining an index
            // is the declaration's job (case targets), not the motive's.
            (MotiveSlot::Term(t), false) => {
                return Err(Error::motive_index_slot_not_binder(t.clone()));
            }
        }
    }

    // The lowering closes one motive binder per binder slot, then the
    // scrutinee; a mismatch is a lowering bug, not user error.
    assert_eq!(
        plan.len() + 1,
        motive.arity(),
        "motive scope arity matches its pattern's binder slots"
    );

    let hints = motive
        .label_iter()
        .map(|l| l.map(str::to_string))
        .collect::<Vec<_>>();
    let labels = hints
        .iter()
        .map(|hint| context.fresh(hint.as_deref()))
        .collect::<Vec<_>>();

    let motive_elaborated = context.with_frame(|context| {
        // Peel the parameter binders off the full index telescope, leaving
        // the index types at the actual parameters.
        let mut ix_telescope = inductive.indices.clone();
        for param in params {
            ix_telescope = match ix_telescope {
                Telescope::Cons(_, rest) => rest.open(&[param]),
                Telescope::Done(_) => unreachable!("index telescope leads with the parameters"),
            };
        }

        let mut index_vars = Vec::with_capacity(n_indices);
        for (slot, label) in plan.iter().zip(&labels) {
            let var = Term::var(Var::free(label));
            match slot {
                // A parameter binder is an *alias* of the actual parameter —
                // defined, not assumed, so the motive body's uses of it are
                // convertible with the index types (which are instantiated at
                // the actual parameters directly).
                SlotPlan::Param(i) => context.define_assuming(label, &param_types[*i], &params[*i]),
                SlotPlan::Index(_) => {
                    ix_telescope = match ix_telescope {
                        Telescope::Cons(ty, rest) => {
                            context.assume(label, &ty);
                            rest.open(&[&var])
                        }
                        Telescope::Done(_) => {
                            unreachable!("index slot count equals the index telescope's")
                        }
                    };
                    index_vars.push(var);
                }
            }
        }

        let scrutinee_label = labels.last().expect("motive binds at least the scrutinee");
        context.assume(
            scrutinee_label,
            &Term::inductive_type(name, params.to_vec(), index_vars),
        );

        let var_terms = labels
            .iter()
            .map(|label| Term::var(Var::free(label)))
            .collect::<Vec<_>>();
        let var_refs = var_terms.iter().collect::<Vec<_>>();
        let body = elaborate(context, &motive.open(&var_refs), Mode::Check(Term::type_()))?.0;

        let label_strs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        Ok(Scope::close(Many(labels.len()), &label_strs, body))
    })?;

    Ok((
        motive_elaborated,
        MotivePattern {
            name: name.to_string(),
            slots: slots_elaborated,
        },
        plan,
    ))
}

fn elaborate_let(context: &mut Context, let_: &Let, mode: Mode) -> Result<(Term, Term), Error> {
    let Let { type_, body, tail } = let_;

    // A bare metavar annotation is the lowering of a typeless local `let x = e`
    // (equivalently `let x : _ = e`): infer the body's type instead of checking
    // the body against the hole. This is what lets a lambda/tuple/atom body —
    // which `check` against an unsolved hole would reject — be bound without an
    // annotation. Otherwise check the body against the (possibly partial)
    // annotation, as before.
    let (type_elaborated, body_elaborated, assumed) = match &**type_ {
        Subterm::Metavar(_) => {
            let (body_elaborated, inferred) = elaborate(context, body, Mode::Infer)?;
            (inferred.clone(), body_elaborated, inferred)
        }
        // The body is checked against — and the binder assumed at — the
        // *rebuilt* annotation: insertion saturates applications during
        // elaboration, and a lowered (under-applied) type reaching the
        // reducer would open a telescope at the wrong arity.
        _ => {
            let type_elaborated = check(context, type_, Term::type_())?;
            let body_elaborated = check(context, body, type_elaborated.clone())?;
            (type_elaborated.clone(), body_elaborated, type_elaborated)
        }
    };

    let label = context.fresh(tail.first_label());

    // Propagate `mode` into the frame so a `Check(expected)` turnaround happens
    // where the let binding is in scope; `expected` is from the outer scope and
    // does not mention the bound name, so comparing inside the frame is sound.
    // The binding is `define`d with the *rebuilt* body: insertion saturates
    // applications during elaboration, and the tail's type-level evaluation
    // must not reduce through the lowered (under-applied) original.
    let (tail_elaborated, tail_type) = context.with_frame(|context| {
        context.define_assuming(&label, &assumed, &body_elaborated);

        let (tail_elaborated, tail_type) =
            elaborate(context, &tail.open(&[&Term::var(Var::free(&label))]), mode)?;

        Ok::<_, Error>((tail_elaborated, reduce_with(context, &tail_type)?))
    })?;

    let rebuilt = Term::let_(label, type_elaborated, body_elaborated, tail_elaborated);

    Ok((rebuilt, tail_type))
}

fn elaborate_rec(context: &mut Context, rec: &Rec, mode: Mode) -> Result<(Term, Term), Error> {
    let Rec { items, tail } = rec;

    let labels = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = labels
        .iter()
        .map(Var::free)
        .map(Term::var)
        .collect::<Vec<_>>();

    let label_terms = label_terms.iter().collect::<Vec<_>>();

    let items = items
        .iter()
        .map(|(type_, body)| (type_.open(&label_terms), body.open(&label_terms)))
        .collect::<Vec<_>>();

    let tail = tail.open(&label_terms);

    let (types_elaborated, bodies_elaborated, tail_elaborated, tail_type) =
        context.with_frame(|context| {
            for (label, (type_, _)) in labels.iter().zip(items.iter()) {
                context.assume(label, type_);
            }

            let mut types_elaborated = Vec::with_capacity(items.len());
            for (type_, _) in &items {
                types_elaborated.push(check(context, type_, Term::type_())?);
            }

            // Upgrade the assumptions to the *rebuilt* signatures before any
            // body is checked: insertion saturates applications during
            // elaboration, and a lowered (under-applied) type reaching the
            // reducer would open a telescope at the wrong arity. The lowered
            // forms were only needed above, while the signatures checked each
            // other.
            for (label, type_) in labels.iter().zip(&types_elaborated) {
                context.reassume(label, type_);
            }

            for (label, (_, body)) in labels.iter().zip(items.iter()) {
                context.define(label, body);
            }

            let mut bodies_elaborated = Vec::with_capacity(items.len());
            for ((_, body), type_) in items.iter().zip(&types_elaborated) {
                bodies_elaborated.push(check(context, body, type_.clone())?);
            }

            // Re-define with the rebuilt bodies before the tail: insertion
            // saturates applications during elaboration, and the tail's
            // type-level evaluation must not reduce through the lowered
            // (under-applied) originals.
            for (label, body) in labels.iter().zip(&bodies_elaborated) {
                context.define(label, body);
            }

            let (tail_elaborated, tail_type) = elaborate(context, &tail, mode)?;

            Ok::<_, Error>((
                types_elaborated,
                bodies_elaborated,
                tail_elaborated,
                reduce_with(context, &tail_type)?,
            ))
        })?;

    let triples = labels
        .into_iter()
        .zip(types_elaborated)
        .zip(bodies_elaborated)
        .map(|((label, type_), body)| (label, type_, body));

    Ok((Term::rec(triples, tail_elaborated), tail_type))
}

fn elaborate_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Func { telescope } = func;

    match mode {
        Mode::Check(expected) => elaborate_func_check(context, telescope, term, expected),
        Mode::Infer => elaborate_func_infer(context, telescope),
    }
}

/// Park a whole *checking problem* (§8): a checked-only introduction form
/// met an expected type whose structure is still an unsolved metavariable —
/// possibly pinned by a constraint parked moments ago. A fresh placeholder
/// metavariable stands in the rebuilt tree; once the expected type's metas
/// solve, the problem re-checks under its frozen frame and the placeholder is
/// solved with the rebuilt term (the spine machinery splices it wherever the
/// occurrence travelled).
fn park_checking(
    context: &mut Context,
    term: &Term,
    expected: &Term,
) -> Result<(Term, Term), Error> {
    let (placeholder, stand_in) = context.fresh_placeholder(expected.clone(), term.span());
    context.park(
        ParkedWork::Checking {
            term: term.clone(),
            expected: expected.clone(),
            placeholder,
        },
        term.clone(),
    );

    Ok((stand_in, expected.clone()))
}

/// Check a lambda against an expected function type. Walk the lambda's own
/// telescope (whose `Done` is the body) alongside the expected type's telescope
/// (whose `Done` is the output type) in lockstep. Each parameter's domain is
/// taken from the expected type; the lambda's own domain — a hole when the
/// annotation was omitted, or the annotation itself — is unified against it via
/// `expect`, which solves the hole (or checks the annotation). The rebuilt lambda
/// then *carries* the expected domain rather than the hole, so re-closing it (and
/// every enclosing binder) captures any free names the domain mentions — this is
/// what keeps nested lambda domains de-Bruijn-correct for `zonk`/`erase` (§9).
fn elaborate_func_check(
    context: &mut Context,
    telescope: &Telescope<Term>,
    term: &Term,
    expected: Term,
) -> Result<(Term, Term), Error> {
    let ft = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::FuncType(ft) => ft,
        Subterm::Metavar(_) if !context.parking_suppressed() => {
            return park_checking(context, term, &expected);
        }
        _ => return Err(Error::not_a_function_type(expected.clone())),
    };

    if telescope.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            ft.telescope.len(),
            telescope.len(),
        ));
    }

    fn walk(
        context: &mut Context,
        term: &Term,
        body: Telescope<Term>,
        type_: Telescope<Term>,
        quantities: &[Quantity],
        idx: usize,
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match (body, type_) {
            (Telescope::Done(body), Telescope::Done(output)) => check(context, &body, *output),
            (Telescope::Cons(domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                // Unify the *rebuilt* annotation against the expected domain:
                // `expect` reduces both sides, and a lowered (under-applied)
                // domain would open a telescope at the wrong arity. An omitted
                // annotation is a hole either way — `check` births it and
                // `expect` then solves it to the expected domain, as before.
                let domain = check(context, &domain, Term::type_())?;
                expect(context, term, &domain, &type_)?;
                let name = context.fresh(body_rest.first_label());
                let x = Term::var(Var::free(&name));
                // Assume the parameter at the expected type's quantity, so the
                // body (checked at `Omega`) cannot reference an erased binder.
                let quantity = quantities.get(idx).copied().unwrap_or(Quantity::Omega);
                context.assume_quantified(&name, &type_, quantity);
                domains.push((name, type_.clone()));
                walk(
                    context,
                    term,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    quantities,
                    idx + 1,
                    domains,
                )
            }
            // Arities were checked equal above.
            _ => unreachable!("function/type telescope arity mismatch"),
        }
    }

    let mut domains = Vec::new();
    let body = context.with_frame(|context| {
        walk(
            context,
            term,
            telescope.clone(),
            ft.telescope,
            &ft.quantities,
            0,
            &mut domains,
        )
    })?;

    Ok((Term::func(domains, body), expected))
}

/// Synthesize a function type from a lambda's own domain annotations — the mirror
/// of `elaborate_func_type`. Walk the telescope, elaborating each domain against
/// `Type`, assuming the parameter, and inferring the body at `Done`. A domain
/// that stays an unconstrained hole (the bare `(x) => …` sugar, or `(x : _)`)
/// offers nothing to synthesize from, so inference fails — exactly as a bare
/// lambda in inference position did before annotations existed. The rebuilt lambda
/// and its type share the same closed domains, so both stay de-Bruijn-correct.
fn elaborate_func_infer(
    context: &mut Context,
    telescope: &Telescope<Term>,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        body: Telescope<Term>,
        domains: &mut Vec<(String, Term)>,
    ) -> Result<(Term, Term), Error> {
        match body {
            Telescope::Done(body) => elaborate(context, &body, Mode::Infer),
            Telescope::Cons(domain, body_rest) => {
                let domain = check(context, &domain, Term::type_())?;

                if matches!(&*reduce_with(context, &domain)?, Subterm::Metavar(_)) {
                    return Err(Error::CannotInfer);
                }

                let name = context.fresh(body_rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &domain);
                domains.push((name, domain));
                walk(context, body_rest.open(&[&x]), domains)
            }
        }
    }

    let mut domains = Vec::new();
    let (body, output) =
        context.with_frame(|context| walk(context, telescope.clone(), &mut domains))?;

    Ok((
        Term::func(domains.clone(), body),
        Term::func_type(domains, output),
    ))
}

fn elaborate_tuple(
    context: &mut Context,
    tuple: &Tuple,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Tuple { fields, names } = tuple;

    let expected = match mode {
        Mode::Check(expected) => expected,
        // Synthesis position: infer each field independently and form the
        // *non-dependent* product. No field type can mention an earlier field
        // (each is inferred in isolation), so the telescope is non-dependent —
        // a dependent Σ-type only ever arises from a checking expectation. This
        // is what lets an un-annotated `let (a, b) = (x, y)` infer `{ typeof x,
        // typeof y }` instead of demanding an annotation.
        Mode::Infer => {
            let mut elaborated = Vec::with_capacity(fields.len());
            let mut field_types = Vec::with_capacity(fields.len());
            for field in fields {
                let (field, field_type) = elaborate(context, field, Mode::Infer)?;
                elaborated.push(field);
                field_types.push((String::new(), field_type));
            }
            return Ok((Term::tuple(elaborated), Term::tuple_type(field_types)));
        }
    };

    let (type_telescope, quantities) = match Term::unwrap_or_clone(reduce_with(context, &expected)?)
    {
        Subterm::TupleType(TupleType {
            telescope,
            quantities,
        }) => (telescope, quantities),
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

    // Written field names are checked positionally against the expected
    // type's labels and then dropped — the rebuilt literal is name-free.
    // Reordering is deliberately not supported: in a dependent telescope the
    // written order is the checking order.
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

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        quantities: &[Quantity],
        idx: usize,
        elaborated: &mut Vec<Term>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                // Open the rest of the dependent telescope with the *elaborated*
                // field, not the raw surface term: the elaborated form carries
                // label projections rebuilt positionally (and implicits inserted),
                // whereas a raw `Field::Label` substituted into a later field type
                // would panic once that type is reduced (e.g. `Task(b.A)` arising
                // from a field typed `Task(A)` in `{ A : Type, t : Task(A) }`).
                // An erased field's value is checked at ρ scaled by its quantity,
                // so an erased binder may fill an erased (subset-witness) field.
                let quantity = quantities.get(idx).copied().unwrap_or(Quantity::Omega);
                let head = context.with_quantity(quantity, |context| check(context, &fields[0], ty))?;
                let rest = rest.open(&[&head]);
                elaborated.push(head);
                walk(context, rest, &fields[1..], quantities, idx + 1, elaborated)
            }
        }
    }

    let mut elaborated = Vec::with_capacity(fields.len());
    walk(context, type_telescope, fields, &quantities, 0, &mut elaborated)?;

    Ok((Term::tuple(elaborated), expected))
}

fn elaborate_metavar(
    context: &mut Context,
    metavar: &Metavar,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let id = metavar.id;

    match mode {
        // Birth (§5): freeze the local context as Γ and record the type the hole
        // is checked against. Births happen once per id, but a re-traversal in
        // the same mode is idempotent — re-check the recorded type against the
        // (identical) `expected`.
        Mode::Check(expected) => {
            if context.metavar_entry(id).is_some() {
                let result = context.metavar_entry(id).unwrap().result.clone();
                expect(context, term, &result, &expected)?;
                Ok((term.clone(), expected))
            } else {
                // Rebuild the hole with the identity spine over its frozen
                // telescope: the rebuilt term is what flows downstream, so
                // every surviving occurrence carries the delayed substitution.
                // Telescope and spine are the shared per-Γ snapshot.
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
        // A hole in synthesis position has no type to offer — unless it was
        // already born in a checking position, in which case report that type.
        Mode::Infer => match context.metavar_entry(id) {
            Some(entry) => Ok((term.clone(), entry.result.clone())),
            None => Err(Error::CannotInfer),
        },
    }
}

/// Rebuild a registry entry's `params`/`indices` telescopes with *elaborated*
/// types. `to_core` records the declaration's lowered spellings, and a lowered
/// type must never leak into later reduction: implicit insertion saturates
/// applications during elaboration, and an under-applied index type (e.g.
/// `Eq(0, 0)` against `Eq`'s 3-ary type constructor) would open a telescope at
/// the wrong arity the first time `reduce` meets the registry copy.
///
/// Called from `elaborate_module_rec` after the group's signatures are
/// reassumed rebuilt and *before* any body is checked — index types may
/// mention the group's own members (resolved through the assumed signatures),
/// and the type-constructor bodies' `InductiveType` nodes check their arguments
/// against this very telescope. A name with no registry entry is an ordinary
/// binding; no-op.
fn elaborate_inductive_indices(context: &mut Context, name: &str) -> Result<(), Error> {
    let Some(inductive) = context.inductive(name).cloned() else {
        return Ok(());
    };

    let n_params = inductive.params.len();
    let labels = inductive
        .indices
        .labels()
        .iter()
        .map(|label| label.to_string())
        .collect::<Vec<_>>();

    // Walk the full (params-first) index telescope, checking each entry type
    // against `Type` under the earlier binders — the same gensym-then-relabel
    // discipline as `elaborate_tuple_type`.
    let mut entries = Vec::new();
    context.with_frame(|context| {
        let mut telescope = inductive.indices.clone();
        loop {
            match telescope {
                Telescope::Done(_) => break Ok::<_, Error>(()),
                Telescope::Cons(ty, rest) => {
                    let rebuilt = check(context, &ty, Term::type_())?;
                    let label = context.fresh(rest.first_label());
                    context.assume(&label, &rebuilt);
                    telescope = rest.open(&[&Term::var(Var::free(&label))]);
                    entries.push((label, rebuilt));
                }
            }
        }
    })?;

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let params =
        Telescope::build(entries[..n_params].iter().cloned(), ()).relabel(&label_refs[..n_params]);
    let indices = Telescope::build(entries, ()).relabel(&label_refs);

    context.register_inductive(
        name,
        Inductive {
            params,
            indices,
            constructors: inductive.constructors,
            result_sort: inductive.result_sort,
        },
    );

    Ok(())
}

/// Rebuild a registry entry's constructor signatures with *elaborated* types —
/// the second phase of the registry rebuild (see
/// [`elaborate_inductive_indices`]). Payload types may apply the inductive group's
/// type constructors, so this runs from `elaborate_module_rec` only after the
/// group's rebuilt bodies are defined; each terminal — the constructed
/// `InductiveType` normal form — routes through `elaborate_inductive_type`, which
/// checks the parameters and the case's target indices against the
/// already-rebuilt index telescope and returns another `InductiveType` node, the
/// shape `case_target_indices` and the match elaborators rely on.
fn elaborate_inductive_constructors(context: &mut Context, name: &str) -> Result<(), Error> {
    let Some(inductive) = context.inductive(name).cloned() else {
        return Ok(());
    };

    let mut constructors = BTreeMap::new();
    for (tag, param) in &inductive.constructors {
        let signature = &param.telescope;
        let labels = signature
            .labels()
            .iter()
            .map(|label| label.to_string())
            .collect::<Vec<_>>();

        let (entries, terminal) = context.with_frame(|context| {
            let mut telescope = signature.clone();
            let mut entries = Vec::new();
            loop {
                match telescope {
                    Telescope::Done(terminal) => {
                        let terminal = check(context, &terminal, Term::type_())?;
                        break Ok::<_, Error>((entries, terminal));
                    }
                    Telescope::Cons(ty, rest) => {
                        let rebuilt = check(context, &ty, Term::type_())?;
                        let label = context.fresh(rest.first_label());
                        context.assume(&label, &rebuilt);
                        telescope = rest.open(&[&Term::var(Var::free(&label))]);
                        entries.push((label, rebuilt));
                    }
                }
            }
        })?;

        let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        constructors.insert(
            tag.clone(),
            InductiveParam {
                telescope: Telescope::build(entries, terminal).relabel(&label_refs),
                quantities: param.quantities.clone(),
            },
        );
    }

    context.register_inductive(
        name,
        Inductive {
            params: inductive.params,
            indices: inductive.indices,
            constructors,
            result_sort: inductive.result_sort,
        },
    );

    Ok(())
}

/// Rebuild a struct's registry telescopes with *elaborated* types, so the field
/// types `erase` and later construction sites consult are saturated (implicit
/// insertion) and reduce correctly — the struct analogue of
/// [`elaborate_inductive_indices`], over the single (params-first) field
/// telescope. Called from `elaborate_module_let` once the type-former is
/// defined (field types may mention the struct itself and earlier items). A
/// name with no registry entry is an ordinary binding; no-op.
fn elaborate_structure(context: &mut Context, name: &str) -> Result<(), Error> {
    let Some(structure) = context.structure(name).cloned() else {
        return Ok(());
    };

    let n_params = structure.params.len();
    let labels = structure
        .fields
        .labels()
        .iter()
        .map(|label| label.to_string())
        .collect::<Vec<_>>();

    let mut entries = Vec::new();
    context.with_frame(|context| {
        let mut telescope = structure.fields.clone();
        loop {
            match telescope {
                Telescope::Done(_) => break Ok::<_, Error>(()),
                Telescope::Cons(ty, rest) => {
                    let rebuilt = check(context, &ty, Term::type_())?;
                    let label = context.fresh(rest.first_label());
                    context.assume(&label, &rebuilt);
                    telescope = rest.open(&[&Term::var(Var::free(&label))]);
                    entries.push((label, rebuilt));
                }
            }
        }
    })?;

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let params =
        Telescope::build(entries[..n_params].iter().cloned(), ()).relabel(&label_refs[..n_params]);
    let fields = Telescope::build(entries, ()).relabel(&label_refs);

    context.register_structure(
        name,
        Structure {
            params,
            fields,
            field_quantities: structure.field_quantities.clone(),
            result_sort: structure.result_sort,
            module: structure.module,
            rep_public: structure.rep_public,
        },
    );

    Ok(())
}

/// Type-check a single non-recursive top-level definition, `define` it into the
/// *current* (persistent base) frame, and return its rebuilt form. The flat
/// analogue of `elaborate_let`'s per-binding work, minus the `with_frame`/tail
/// recursion: the binding must stay in scope for every later item and the
/// entrypoint body. The *rebuilt* body is `define`d (implicit insertion makes
/// the lowered one no longer interchangeable; see the comment below), and the
/// rebuilt `Definition` flows on to `zonk`/`erase`.
fn elaborate_module_let(context: &mut Context, def: &Definition) -> Result<Definition, Error> {
    let type_ = check(context, &def.type_, Term::type_())?;
    let body = check(context, &def.body, type_.clone())?;

    // Define the *rebuilt* body at the *rebuilt* type, not the lowered ones:
    // implicit-argument insertion saturates applications during elaboration,
    // and the untyped reducer (type-level evaluation in later items' types)
    // would meet a lowered form's under-applied calls and open a telescope at
    // the wrong arity. Pre-insertion the two were interchangeable; no longer.
    context.define_assuming(&def.name, &type_, &body);

    // A struct's type-former lowers to a standalone `let`; rebuild its registry
    // telescopes now that the former is defined (no-op for an ordinary let).
    elaborate_structure(context, &def.name)?;

    Ok(Definition {
        name: def.name.clone(),
        type_,
        body,
    })
}

/// Type-check a top-level `rec` group, `define` every member into the current
/// frame, and return their rebuilt forms. The flat analogue of `elaborate_rec` —
/// assume all signatures, check the types, define all bodies, then check the
/// bodies — but with no de Bruijn open/close: members already reference each
/// other by free name.
fn elaborate_module_rec(
    context: &mut Context,
    defs: &[Definition],
) -> Result<Vec<Definition>, Error> {
    for def in defs {
        context.assume(&def.name, &def.type_);
    }

    let mut types = Vec::with_capacity(defs.len());
    for def in defs {
        types.push(check(context, &def.type_, Term::type_())?);
    }

    // Upgrade the assumptions to the *rebuilt* signatures before any body is
    // checked (see `elaborate_rec`): a lowered (under-applied) type must not
    // leak into later reduction. The lowered forms were only needed above,
    // while the signatures checked each other.
    for (def, type_) in defs.iter().zip(&types) {
        context.reassume(&def.name, type_);
    }

    // An inductive's type bindings always lower as one `rec` group whose member
    // names are the registry keys. Rebuild the registry index telescopes here
    // — after the rebuilt signatures are assumed (index types may mention the
    // group), before any body's `InductiveType` node checks against them.
    for def in defs {
        elaborate_inductive_indices(context, &def.name)?;
    }

    for def in defs {
        context.define(&def.name, &def.body);
    }

    let mut bodies = Vec::with_capacity(defs.len());
    for (def, type_) in defs.iter().zip(&types) {
        bodies.push(check(context, &def.body, type_.clone())?);
    }

    // Re-define every member with its rebuilt body: insertion saturates
    // applications during elaboration, and later items' type-level evaluation
    // must not reduce through the lowered (under-applied) originals. The
    // originals were only needed above, while the members checked each other.
    for (def, body) in defs.iter().zip(&bodies) {
        context.define(&def.name, body);
    }

    // Registry rebuild, phase two: constructor payload types may apply the
    // group's type constructors, so their signatures (and `InductiveType`
    // terminals) elaborate only now that the rebuilt bodies are defined.
    for def in defs {
        elaborate_inductive_constructors(context, &def.name)?;
    }

    Ok(defs
        .iter()
        .zip(types)
        .zip(bodies)
        .map(|((def, type_), body)| Definition {
            name: def.name.clone(),
            type_,
            body,
        })
        .collect())
}

/// Elaborate a whole [`Module`] (§9). Each top-level item is checked and `define`d
/// *cumulatively in the persistent base frame* — never a popped `with_frame` —
/// so every definition stays in scope for later items, the entrypoint `body`, and
/// (through `mode`) its type annotation. Returns the rebuilt module (lambda
/// domains solved, binders re-closed) alongside the body's type, reduced through
/// the accumulated definitions.
///
/// Elaboration is authoritative: the returned module — not the lowered input — is
/// what `zonk_module` then makes meta-free for `erase`.
pub fn elaborate_module(
    context: &mut Context,
    module: &Module,
    metavar_floor: usize,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    // Seed the context's inductive registry before any item is checked: a
    // inductive's type-constructor and value-constructor definitions reference
    // their own registry entry (`elaborate_inductive_type` / `elaborate_variant`).
    for (name, inductive) in &module.inductives {
        context.register_inductive(name, inductive.clone());
    }

    // Likewise seed the struct registry — `elaborate_struct`/`elaborate_proj`
    // consult it (and `elaborate_structure` rebuilds each entry's telescopes).
    for (name, structure) in &module.structures {
        context.register_structure(name, structure.clone());
    }

    // Implicit-argument insertion mints metavariables during elaboration;
    // floor the counter above `to_core`'s (which returns the count alongside
    // the lowered module) so the id spaces never collide.
    context.seed_metavars(metavar_floor);

    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        // The use-site module for the struct projection privacy check (§7) is
        // the qualifier prefix of the item's name (a `rec` group shares one);
        // the entrypoint body below runs under the root module.
        let item_module = match item {
            Item::Let(def) => module_of(&def.name),
            Item::Rec(defs) => defs.first().map(|d| module_of(&d.name)).unwrap_or(""),
        };
        context.set_island(item_module.to_string());

        items.push(match item {
            Item::Let(def) => Item::Let(elaborate_module_let(context, def)?),
            Item::Rec(defs) => Item::Rec(elaborate_module_rec(context, defs)?),
        });
        // Constraints parked during this item must resolve within it: drain
        // here so an unresolvable one is attributed to its own definition and
        // frozen frames do not accumulate across items (§8).
        drain_parked(context)?;
    }

    context.set_island(String::new());
    let (body, body_type) = elaborate(context, &module.body, mode)?;
    drain_parked(context)?;
    let body_type = reduce_with(context, &body_type)?;

    // The output module carries the *rebuilt* registry entries (pulled back
    // from the context, where the per-group rebuild re-registered them), so
    // `zonk_module` and `erase` see elaborated telescopes. An entry whose
    // declaring item was pruned keeps its lowered form — nothing consults it.
    let inductives = module
        .inductives
        .keys()
        .map(|name| {
            let inductive = context
                .inductive(name)
                .expect("every module entry was registered above")
                .clone();
            (name.clone(), inductive)
        })
        .collect();

    // Same for the struct registry: pull back the entries rebuilt by
    // `elaborate_structure` so `zonk_module`/`erase` see elaborated telescopes.
    let structures = module
        .structures
        .keys()
        .map(|name| {
            let structure = context
                .structure(name)
                .expect("every module entry was registered above")
                .clone();
            (name.clone(), structure)
        })
        .collect();

    let module = Module {
        items,
        inductives,
        structures,
        type_: module.type_.clone(),
        body,
    };

    Ok((module, body_type))
}

/// The module of a flat item: its name's qualifier prefix (`Foo/Bar` for
/// `Foo/Bar/f`; the empty string for a root-level `f`). The struct projection
/// privacy check compares this against the struct's declaring module.
fn module_of(name: &str) -> &str {
    match name.rfind('/') {
        Some(slash) => &name[..slash],
        None => "",
    }
}

/// Implicit-eta on the check turnaround. A reference whose type leads with an
/// implicit binder, checked against a concrete *explicit* function type, has its
/// leading implicits inserted as metavariables and is eta-expanded over the
/// remaining explicit binders — so a bare `Arr/concat` is accepted where
/// `(Arr B, Arr B) -> Arr B` is expected, instead of demanding
/// `(l, r) => concat(l, r)`. Implicit insertion is otherwise an application-site
/// mechanism (`elaborate_apply`); this is the one extension into value position.
/// Producing a full lambda (rather than a partial application) keeps erase/CPS
/// untouched: the output is an ordinary closure over a saturated call.
///
/// Fires only for `Var`/`Proj` heads against a ground explicit-arrow expectation;
/// every other shape returns the term unchanged for the ordinary `expect`. The
/// expected-not-implicit gate preserves polymorphic-value assignment
/// (`let f : (@z : A) -> … = …` keeps its implicit). It is purely additive: when
/// it does not fire, or the inserted shape does not convert, behavior is as before.
fn insert_implicits_on_check(
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
    if ift.plicities.first() != Some(&Plicity::Implicit) {
        return Ok((rebuilt, type_));
    }

    let expected_reduced = reduce_with(context, expected)?;
    let expected_explicit = matches!(
        &*expected_reduced,
        Subterm::FuncType(eft) if eft.plicities.first() != Some(&Plicity::Implicit)
    );
    if !expected_explicit {
        return Ok((rebuilt, type_));
    }

    let ift = ift.clone();
    let span = term.span();
    let func_label = match &**term {
        Subterm::Var(var) => var.unwrap().to_string(),
        _ => "<function>".to_string(),
    };

    // Walk the head's telescope: implicit binders become fresh metavariables (the
    // inserted arguments), explicit binders become fresh lambda parameters (the
    // eta variables). `head_args` records both in telescope order so the body
    // re-applies the head fully saturated; `open` threads the dependent
    // substitution so a later binder mentioning an earlier one is instantiated.
    let mut head_args: Vec<(Plicity, Term)> = Vec::new();
    let mut binders: Vec<(String, Term)> = Vec::new();
    let output = context.with_frame(|context| {
        let mut tele = ift.telescope.clone();
        let mut plicities = ift.plicities.iter();
        loop {
            match tele {
                Telescope::Done(output) => break *output,
                Telescope::Cons(domain, rest) => match plicities.next() {
                    Some(Plicity::Implicit) => {
                        let binder = binder_name(rest.first_label().unwrap_or("_"));
                        let metavar = context.fresh_metavar(
                            domain,
                            span.clone(),
                            ImplicitOrigin {
                                func: func_label.clone(),
                                binder,
                            },
                        );
                        tele = rest.open(&[&metavar]);
                        head_args.push((Plicity::Implicit, metavar));
                    }
                    Some(Plicity::Explicit) => {
                        let label = context.fresh(rest.first_label());
                        context.assume(&label, &domain);
                        let var = Term::var(Var::free(&label));
                        tele = rest.open(&[&var]);
                        binders.push((label, domain));
                        head_args.push((Plicity::Explicit, var));
                    }
                    None => unreachable!("plicities parallel the telescope"),
                },
            }
        }
    });

    let body = Term::apply_marked(rebuilt, head_args);

    // No explicit binders to eta over (an all-implicit curried prefix, e.g.
    // `(@A, @B) -> …`): the implicit-saturated application *is* the value, and its
    // type is the opened output. Erasure drops the implicit arguments anyway.
    if binders.is_empty() {
        return Ok((body, output));
    }

    let func_type = Term::func_type(binders.clone(), output);
    Ok((Term::func(binders, body), func_type))
}

pub fn elaborate(context: &mut Context, term: &Term, mode: Mode) -> Result<(Term, Term), Error> {
    // A term checked against `Type` *is* a type (or type family) — an erased
    // position. Lower the ambient resource to `Zero` so an erased binder may be
    // referenced here (in an index, a domain, a motive). This single chokepoint
    // covers every `check(.., Term::type_())` and `Mode::Check(Term::type_())`
    // site; the `current_quantity` guard keeps the recursion one level deep.
    if let Mode::Check(expected) = &mode
        && matches!(&**expected, Subterm::Type)
        && context.current_quantity() != Quantity::Zero
    {
        return context.with_quantity(Quantity::Zero, |context| {
            elaborate(context, term, mode.clone())
        });
    }

    let result = elaborate_subterm(context, term, mode);

    // Carry the source span onto the rebuilt term as well as onto any error, so
    // downstream passes keep reporting against the original syntax.
    match term.span() {
        Some(span) => result
            .map(|(term, type_)| (term.with_span(span.clone()), type_))
            .map_err(|error| error.at(span)),
        None => result,
    }
}

fn elaborate_subterm(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // Synthesizable nodes compute their type and hit the `expect` turnaround in
    // `Check` mode; naturally-checked nodes (and the mode-propagating `Let`/`Rec`)
    // consume `mode` directly and return early. Every arm returns the rebuilt
    // term — binders re-closed, lambda domains solved (§9).
    let (rebuilt, type_) = match &**term {
        Subterm::Type => (term.clone(), Term::type_()),
        Subterm::Prop => (term.clone(), Term::type_()),
        Subterm::Prim(prim) => return elaborate_prim(context, term, prim, mode),
        Subterm::Match(m) => return elaborate_match(context, m, term, mode),
        Subterm::FuncType(ft) => elaborate_func_type(context, ft)?,
        Subterm::Apply(apply) => return elaborate_apply(context, apply, term, mode),
        Subterm::TupleType(tt) => elaborate_tuple_type(context, tt)?,
        Subterm::Proj(proj) => elaborate_proj(context, proj)?,
        Subterm::Let(let_) => return elaborate_let(context, let_, mode),
        Subterm::Rec(rec) => return elaborate_rec(context, rec, mode),
        Subterm::Var(var) => match context.assumption(var.unwrap()) {
            Some(type_) => {
                // The relevance check: an erased (`Zero`) binder referenced while
                // the ambient resource is `Omega` (a runtime position) is the one
                // and only place the discipline is enforced.
                if context.assumption_quantity(var.unwrap()) == Quantity::Zero
                    && context.current_quantity() == Quantity::Omega
                {
                    return Err(Error::erased_variable_at_runtime(Term::var(var.clone())));
                }
                (term.clone(), type_.clone())
            }
            None => return Err(Error::unbound_variable(Term::var(var.clone()))),
        },
        Subterm::Func(func) => return elaborate_func(context, func, term, mode),
        Subterm::Tuple(tuple) => return elaborate_tuple(context, tuple, term, mode),
        Subterm::Metavar(metavar) => return elaborate_metavar(context, metavar, term, mode),
        Subterm::InductiveType(ut) => elaborate_inductive_type(context, ut)?,
        Subterm::Variant(uc) => elaborate_variant(context, uc, term)?,
        Subterm::StructType(st) => elaborate_struct_type(context, st, term)?,
        Subterm::Struct(s) => elaborate_struct(context, s, term)?,
    };

    if let Mode::Check(expected) = &mode {
        let (rebuilt, type_) = insert_implicits_on_check(context, term, rebuilt, type_, expected)?;
        expect(context, term, &type_, expected)?;
        return Ok((rebuilt, type_));
    }

    Ok((rebuilt, type_))
}
