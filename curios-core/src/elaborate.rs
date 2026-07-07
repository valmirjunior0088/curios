use {
    super::{
        Apply, Bound, Context, Error, Field, Func, FuncType, ImplicitOrigin, InductiveType, Infix,
        Let, Metavar, MetavarId, Nat, NumLit, ParkedWork, Prim, Proj, Rec, Struct, StructEntry,
        StructType, Structure, Subterm, Telescope, Term, Tuple, TupleType, Var, Variant,
        WitnessOrigin, attempt_witness_goal, check, elaborate_match, elaborate_prim, expect,
        operator_concept, reduce_with, sort_term,
    },
    curios_base::{Flt, Int, NumOp, Plicity},
    num_bigint::BigInt,
    num_traits::ToPrimitive,
    std::collections::{BTreeSet, VecDeque},
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
        plicities: &[Plicity],
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(output) => check(context, &output, Term::type_()),
            Telescope::Cons(ty, rest) => {
                let domain = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::free_var(&name);
                // Assume the *rebuilt* domain: insertion saturates applications
                // during elaboration, and a lowered (under-applied) type leaking
                // into later reduction would open a telescope at the wrong arity.
                // A `use` binder additionally joins the witness scope: the rest
                // of the type may itself need resolution through it.
                match plicities.get(domains.len()) {
                    Some(Plicity::Witness) => context.assume_witness(&name, &domain),
                    _ => context.assume(&name, &domain),
                }
                domains.push((name, domain));
                walk(context, rest.open(&[&x]), plicities, domains)
            }
        }
    }

    let mut domains = Vec::new();
    let output = context
        .with_frame(|context| walk(context, ft.telescope.clone(), &ft.plicities, &mut domains))?;

    let rebuilt = Term::func_type_marked(
        ft.plicities
            .iter()
            .zip(domains)
            .map(|(&plicity, (label, domain))| (plicity, label, domain)),
        output,
    );

    Ok((rebuilt, Term::type_()))
}

/// Fill an omitted non-explicit slot: an implicit binder gets a fresh
/// metavariable; a witness binder gets a fresh metavariable *plus* a
/// resolution goal, attempted eagerly (solved now, parked on a flex key, or
/// deferred on a missing table entry). `origin` is the application node — the
/// span anchor for the goal.
fn insert_auto_argument(
    context: &mut Context,
    plicity: Plicity,
    type_: &Term,
    label: Option<&str>,
    func: &str,
    origin: &Term,
) -> Result<Term, Error> {
    let binder = binder_name(label.unwrap_or("_"));

    match plicity {
        Plicity::Implicit => Ok(context.fresh_metavar(
            type_.clone(),
            origin.span(),
            ImplicitOrigin {
                func: func.to_string(),
                binder,
            },
        )),
        Plicity::Witness => {
            let provenance = WitnessOrigin {
                func: func.to_string(),
                binder,
            };
            let (id, metavar) =
                context.fresh_witness_metavar(type_.clone(), origin.span(), provenance.clone());
            attempt_witness_goal(context, id, type_, provenance, origin)?;
            Ok(metavar)
        }
        Plicity::Explicit => unreachable!("explicit slots are never auto-filled"),
    }
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

    // The three call-site queues: plain arguments fill explicit binders in
    // telescope order, `@`-arguments fill implicit binders, `use`-arguments
    // fill witness binders — each matched independently, so the relative
    // position of a marked argument among the plain ones carries no meaning.
    let mut plain: VecDeque<Term> = VecDeque::new();
    let mut marked: VecDeque<Term> = VecDeque::new();
    let mut used: VecDeque<Term> = VecDeque::new();
    for (plicity, param) in plicities.iter().zip(params) {
        match plicity {
            Plicity::Explicit => plain.push_back(param.clone()),
            Plicity::Implicit => marked.push_back(param.clone()),
            Plicity::Witness => used.push_back(param.clone()),
        }
    }

    // All-auto telescopes (the curried `bind` shape, e.g.
    // `(@A, @B) -> (M A, A -> M B) -> M B`, or a method wrapper's
    // `(@A, use w) -> …`): when the head telescope has zero explicit slots but
    // plain arguments were given, saturate it — marked queues first, fresh
    // metavariables (and witness goals) for the rest — reduce the output, and
    // re-target the plain arguments at the next telescope. This fires *only*
    // with zero explicit slots, so application stays arity-strict everywhere
    // else (this is deliberately not general partial application).
    let ft = loop {
        let ft = match &*head_type {
            Subterm::FuncType(ft) => ft.clone(),
            other => return Err(Error::not_a_function(other.clone())),
        };

        let all_auto = !ft.plicities.is_empty()
            && ft.plicities.iter().all(|p| !matches!(p, Plicity::Explicit));
        if !all_auto || plain.is_empty() {
            break ft;
        }

        let mut args = Vec::with_capacity(ft.plicities.len());
        let mut tele = ft.telescope.clone();
        for plicity in &ft.plicities {
            let Telescope::Cons(ty, rest) = tele else {
                unreachable!("plicities parallel the telescope");
            };
            let queue = match plicity {
                Plicity::Implicit => &mut marked,
                Plicity::Witness => &mut used,
                Plicity::Explicit => unreachable!("all-auto telescope"),
            };
            let arg = match queue.pop_front() {
                Some(arg) => check(context, &arg, ty.clone())?,
                None => insert_auto_argument(
                    context,
                    *plicity,
                    &ty,
                    rest.first_label(),
                    &func_label,
                    term,
                )?,
            };
            tele = rest.open(&[&arg]);
            args.push((*plicity, arg));
        }
        let Telescope::Done(output) = tele else {
            unreachable!("plicities parallel the telescope");
        };

        head = Term::apply_marked(head, args);
        head_type = reduce_with(context, &output)?;
    };

    // Arity is checked per queue: plain arguments must exactly cover the
    // explicit slots; `@`- and `use`-arguments may undershoot their slots (the
    // remainder is inserted/resolved) but never overshoot them.
    let explicit_slots = ft
        .plicities
        .iter()
        .filter(|p| matches!(p, Plicity::Explicit))
        .count();
    let implicit_slots = ft
        .plicities
        .iter()
        .filter(|p| matches!(p, Plicity::Implicit))
        .count();
    let witness_slots = ft
        .plicities
        .iter()
        .filter(|p| matches!(p, Plicity::Witness))
        .count();

    if plain.len() != explicit_slots {
        return Err(Error::wrong_number_of_arguments(
            explicit_slots,
            plain.len(),
        ));
    }
    if marked.len() > implicit_slots {
        return Err(Error::too_many_implicits(implicit_slots, marked.len()));
    }
    if used.len() > witness_slots {
        return Err(Error::too_many_witness_args(witness_slots, used.len()));
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
                    None => insert_auto_argument(
                        context,
                        *plicity,
                        &ty,
                        rest.first_label(),
                        &func_label,
                        term,
                    )?,
                },
                Plicity::Witness => match used.pop_front() {
                    Some(arg) => arg,
                    None => insert_auto_argument(
                        context,
                        *plicity,
                        &ty,
                        rest.first_label(),
                        &func_label,
                        term,
                    )?,
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
        let term = if checking
            && blocked_on_metavar(
                context,
                &params[index],
                &ty,
                &result_metavars,
                expected_ground,
            )? {
            postponed.push(index);
            params[index].clone()
        } else {
            check(context, &params[index], ty.clone())?
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
            if blocked_on_metavar(
                context,
                &params[slot],
                &slot_ty,
                &result_metavars,
                expected_ground,
            )? {
                cursor += 1;
            } else {
                elaborated[slot] = check(context, &params[slot], slot_ty)?;
                postponed.remove(cursor);
                resolved = true;
            }
        }

        // Re-open from the top through the (possibly updated) prefix so later
        // slot types carry the elaborated forms; otherwise just advance.
        tele = match resolved {
            false => rest.open(&[&elaborated[index]]),
            true => original.clone().open_params(&elaborated),
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
            elaborated[slot] = check(context, &params[slot], slot_ty)?;
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

/// Whether `arg` is a checked-only introduction form (tuple, lambda, list
/// literal) that cannot be elaborated yet because the type structure it needs is
/// an unsolved metavar — a tuple or list literal whose whole expected type, or a
/// lambda whose expected *domain*, reduces to one. (A lambda only needs its domain
/// known: the body, which may project the parameter, can't be checked against an
/// unknown domain; its codomain may stay a metavar. A list literal borrows its
/// element type from `expected`, so it needs the expected head — `Lst _` — to be
/// known.) Synthesizable forms return `false`: they have a turnaround of their own
/// and must run eagerly so their solutions feed the result unification.
fn blocked_on_metavar(
    context: &mut Context,
    arg: &Term,
    ty: &Term,
    result_metavars: &BTreeSet<MetavarId>,
    expected_ground: bool,
) -> Result<bool, Error> {
    let is_lambda = matches!(&**arg, Subterm::Func(_));
    let is_list = matches!(&**arg, Subterm::Prim(Prim::Lst(_)));
    let is_tuple = matches!(&**arg, Subterm::Tuple(_));
    if !is_lambda && !is_list && !is_tuple {
        return Ok(false);
    }
    let reduced = reduce_with(context, ty)?;
    Ok(match &*reduced {
        // A tuple/list/lambda whose whole expected type is an unsolved metavar.
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
fn transitively_ground(context: &Context, id: MetavarId) -> bool {
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
                let x = Term::free_var(&name);
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

    let rebuilt: Term = Subterm::TupleType(TupleType { telescope }).into();

    // A tuple type's sort: `Prop` when every field is a proposition (`{}` is the
    // base case), `Type` otherwise — so a record of proofs checks against `Prop`.
    let sort = sort_term(context, &rebuilt)?;
    Ok((rebuilt, sort))
}

fn elaborate_proj(context: &mut Context, proj: &Proj) -> Result<(Term, Term), Error> {
    let Proj { head, field } = proj;

    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    // Both tuples and structs project; the field telescope is the tuple type's
    // own, or the struct's (instantiated at the head type's parameters). A struct
    // additionally enforces representation privacy here (§7).
    let telescope = match &*head_type {
        Subterm::TupleType(TupleType { telescope }) => telescope.clone(),
        Subterm::StructType(StructType { name, params }) => {
            let Some(structure) = context.structure(name).cloned() else {
                return Err(Error::unbound_variable(Term::free_var(name)));
            };

            // The use-site module is the enclosing item's qualifier prefix
            // (`Context::island`, set per item by `elaborate_module`);
            // the island model grants no descendant access, so the check is
            // exact qualifier equality.
            if !structure.rep_public && *context.island() != structure.module {
                let field = match field {
                    Field::Index(index) => index.to_string(),
                    Field::Label(label) => label.clone(),
                };
                return Err(Error::private_field(name.clone(), field));
            }

            structure.fields_at(params)
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
                    // A concept's superclass fields carry a minted internal
                    // label and are not projectable by name — never surface
                    // them among the available fields.
                    let supers: Vec<usize> = match &*head_type {
                        Subterm::StructType(StructType { name, .. }) => context
                            .concept(name)
                            .map(|concept| concept.supers.iter().map(|(i, _)| *i).collect())
                            .unwrap_or_default(),
                        _ => Vec::new(),
                    };
                    return Err(Error::unknown_tuple_label(
                        label.clone(),
                        labels
                            .iter()
                            .enumerate()
                            .filter(|(i, l)| !l.is_empty() && !supers.contains(i))
                            .map(|(_, l)| l.to_string())
                            .collect(),
                    ));
                }
            }
        }
    };

    if index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(index, telescope.len()));
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
fn elaborate_inductive_type(
    context: &mut Context,
    ut: &InductiveType,
) -> Result<(Term, Term), Error> {
    let InductiveType {
        name,
        params,
        indices,
    } = ut;

    let Some(inductive) = context.inductive(name).cloned() else {
        return Err(Error::unbound_variable(Term::free_var(name)));
    };

    let args: Vec<Term> = params.iter().chain(indices.iter()).cloned().collect();

    if args.len() != inductive.indices.len() {
        return Err(Error::wrong_number_of_arguments(
            inductive.indices.len(),
            args.len(),
        ));
    }

    let (elaborated, ()) = check_args_against(context, inductive.indices, &args)?;

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
        return Err(Error::unbound_variable(Term::free_var(name)));
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

    let (elaborated, output) = check_args_against(context, signature, &args)?;

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
            None => Error::unbound_variable(Term::free_var(name)),
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
        return Ok((
            Term::struct_type(name, resolved),
            structure.result_sort.clone(),
        ));
    }

    if params.len() != structure.params.len() {
        return Err(Error::struct_arity_mismatch(
            name.clone(),
            structure.params.len(),
            params.len(),
        ));
    }

    let (elaborated, ()) = check_args_against(context, structure.params, params)?;

    Ok((Term::struct_type(name, elaborated), structure.result_sort))
}

/// Where one field position's value comes from: a written term to check, or —
/// for a concept's `use`-marked field with no written fill — a witness goal to
/// mint at the position's instantiated type.
enum FieldSource<'a> {
    Written(&'a Term),
    Resolve { func: String, binder: String },
}

/// Check each positional field against its type in a dependent field telescope,
/// pushing the elaborated fields onto `elaborated`. Shared by struct and tuple
/// literal elaboration. The rest of the telescope is opened with the
/// *elaborated* field, not the raw surface term: the elaborated form carries
/// label projections rebuilt positionally (and implicits inserted), whereas a
/// raw `Field::Label` substituted into a later field type would panic once that
/// type is reduced (e.g. `Task(b.A)` arising from a field typed `Task(A)` in
/// `{ A : Type, t : Task(A) }`). A `Resolve` source mints a witness metavar
/// plus an eagerly-attempted resolution goal — the `insert_auto_argument`
/// pattern — anchored at `origin`, and the metavar threads the telescope like
/// any elaborated field.
fn check_dependent_fields(
    context: &mut Context,
    tele: Telescope<()>,
    sources: &[FieldSource],
    origin: &Term,
    elaborated: &mut Vec<Term>,
) -> Result<(), Error> {
    match tele {
        Telescope::Done(_) => Ok(()),
        Telescope::Cons(ty, rest) => {
            let head = match &sources[0] {
                FieldSource::Written(field) => check(context, field, ty)?,
                FieldSource::Resolve { func, binder } => {
                    let provenance = WitnessOrigin {
                        func: func.clone(),
                        binder: binder.clone(),
                    };
                    let (id, metavar) = context.fresh_witness_metavar(
                        ty.clone(),
                        origin.span(),
                        provenance.clone(),
                    );
                    attempt_witness_goal(context, id, &ty, provenance, origin)?;
                    metavar
                }
            };
            let rest = rest.open(&[&head]);
            elaborated.push(head);
            check_dependent_fields(context, rest, &sources[1..], origin, elaborated)
        }
    }
}

/// Type a struct literal against its registry entry (§3.3). The struct's `name`
/// makes it self-describing, so this synthesizes (like `elaborate_variant`,
/// not the purely-checked `elaborate_tuple`): the parameters come from the
/// written head — a bare-name head mints one fresh metavariable per parameter,
/// solved by the field checks (and, in `Check` mode, the `expect` turnaround
/// unifying the result `StructType` against the expected type) — and the fields
/// are checked in declaration order through the (dependent) field telescope.
fn elaborate_struct(
    context: &mut Context,
    s: &Struct,
    term: &Term,
    mode: &Mode,
) -> Result<(Term, Term), Error> {
    let Struct {
        name,
        params,
        fields,
        entries,
    } = s;

    let Some(structure) = context.structure(name).cloned() else {
        return Err(match context.assumption(name) {
            Some(found) => Error::not_a_struct_type(found.clone()),
            None => Error::unbound_variable(Term::free_var(name)),
        });
    };

    // Construction privacy (§7): a private-representation struct may be built
    // only within its declaring module. Checked here (alongside projection
    // privacy in `elaborate_proj`) via `island`, set per item by
    // `elaborate_module`.
    if !structure.rep_public && *context.island() != structure.module {
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

    // A `..base` spread takes its own path: the base is let-bound in a fresh
    // frame and every unwritten position copies from it. At most one spread,
    // and it must be the first entry.
    match entries
        .iter()
        .filter(|e| matches!(e, StructEntry::Spread))
        .count()
    {
        0 => {}
        1 if matches!(entries[0], StructEntry::Spread) => {
            return elaborate_struct_spread(context, &structure, s, term, mode);
        }
        1 => return Err(Error::spread_not_first(name.clone())),
        _ => return Err(Error::multiple_spreads(name.clone())),
    }

    let resolved = resolve_struct_params(context, name, &structure, params, term)?;
    seed_struct_expectation(context, name, &resolved, term, mode)?;

    // Instantiate the field telescope at the resolved parameters.
    let field_telescope = structure.fields_at(&resolved);

    // A concept's `use`-marked (superclass) fields leave the positional field
    // sequence, exactly like witness slots at call sites: plain written fields
    // pair with the plain positions, explicit `use <term>` entries pair with
    // the `use` positions in declaration order (no skipping), and every
    // remaining `use` position becomes a witness-resolution goal. Note the
    // check order is telescope order, not written order — the same model as
    // call-site witness arguments.
    let use_positions: Vec<usize> = match context.concept(name) {
        Some(concept) => concept.supers.iter().map(|(index, _)| *index).collect(),
        None => Vec::new(),
    };
    debug_assert!(use_positions.windows(2).all(|w| w[0] < w[1]));

    // Partition the written entries; an empty entry list is all-plain-unlabeled
    // (the internal normal form).
    let mut plain: Vec<(Option<&str>, &Term)> = Vec::new();
    let mut fills: Vec<&Term> = Vec::new();
    if entries.is_empty() {
        plain.extend(fields.iter().map(|field| (None, field)));
    } else {
        for (entry, field) in entries.iter().zip(fields) {
            match entry {
                StructEntry::Field(label) => plain.push((label.as_deref(), field)),
                StructEntry::Use => fills.push(field),
                StructEntry::Spread => unreachable!("a spread literal takes the spread path"),
            }
        }
    }

    if !fills.is_empty() && context.concept(name).is_none() {
        return Err(Error::use_entry_outside_concept(name.clone()));
    }

    if fills.len() > use_positions.len() {
        return Err(Error::too_many_use_entries(
            name.clone(),
            use_positions.len(),
            fills.len(),
        ));
    }

    // Superclass fields are anonymous, so no written label can target one: a
    // labeled entry naming a former superclass is just an unknown field, caught
    // by the positional validation below.
    let labels = field_telescope.labels();
    let plain_labels: Vec<&str> = labels
        .iter()
        .enumerate()
        .filter(|(position, _)| !use_positions.contains(position))
        .map(|(_, label)| *label)
        .collect();

    if plain.len() != plain_labels.len() {
        return Err(Error::wrong_number_of_fields(
            name.clone(),
            plain_labels.len(),
            plain.len(),
        ));
    }

    // Written field names are checked positionally against the declared labels
    // and then dropped — the rebuilt literal is name-free. Reordering is not
    // supported: in a dependent telescope the written order is the check order.
    for (position, (written, _)) in plain.iter().enumerate() {
        let Some(written) = written else { continue };
        let declared = plain_labels.get(position).copied().unwrap_or_default();
        if declared != *written {
            return Err(Error::unknown_struct_field(
                name.clone(),
                (*written).to_string(),
                plain_labels
                    .iter()
                    .filter(|l| !l.is_empty())
                    .map(|l| l.to_string())
                    .collect(),
            ));
        }
    }

    // Merge into one source per declared position: `use` positions consume the
    // written fills first, then fall back to resolution; plain positions
    // consume the plain values (counts validated above).
    let mut plain_values = plain.iter().map(|(_, field)| *field);
    let mut fill_values = fills.iter().copied();
    let mut sources = Vec::with_capacity(field_telescope.len());
    for position in 0..field_telescope.len() {
        if use_positions.contains(&position) {
            sources.push(match fill_values.next() {
                Some(fill) => FieldSource::Written(fill),
                // A `use` position is an anonymous superclass field; its minted
                // internal label must never surface, so the goal's provenance
                // names it `_` (the goal itself already shows the concept).
                None => FieldSource::Resolve {
                    func: name.clone(),
                    binder: "_".to_string(),
                },
            });
        } else {
            let field = plain_values
                .next()
                .expect("plain field count was validated against the telescope");
            sources.push(FieldSource::Written(field));
        }
    }

    let mut elaborated = Vec::with_capacity(sources.len());
    check_dependent_fields(context, field_telescope, &sources, term, &mut elaborated)?;

    Ok((
        Term::struct_(name, resolved.clone(), elaborated),
        Term::struct_type(name, resolved),
    ))
}

/// Resolve a struct literal's head parameters, threading the (dependent)
/// parameter telescope so each minted metavariable is born at its binder's
/// instantiated type: written arguments are checked, omitted ones minted fresh.
fn resolve_struct_params(
    context: &mut Context,
    name: &str,
    structure: &Structure,
    params: &[Term],
    term: &Term,
) -> Result<Vec<Term>, Error> {
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
                        func: name.to_string(),
                        binder,
                    },
                )
            }
        };
        tele = rest.open(&[&arg]);
        resolved.push(arg);
    }
    Ok(resolved)
}

/// Seed omitted parameters from the checking expectation *before* the fields
/// elaborate: a field checked against a type carrying an unsolved parameter
/// metavariable can strand flex-flex constraints (e.g. a `match` tail's
/// inferred motive against `Result({Nat, ?P}, Str)`) that nothing wakes.
/// Only a same-named struct expectation seeds — anything else falls through
/// to the dispatch-level `expect`, preserving implicit insertion and the
/// ordinary mismatch diagnostics.
fn seed_struct_expectation(
    context: &mut Context,
    name: &str,
    resolved: &[Term],
    term: &Term,
    mode: &Mode,
) -> Result<(), Error> {
    if let Mode::Check(expected) = mode
        && let Subterm::StructType(StructType {
            name: expected_name,
            ..
        }) = Term::unwrap_or_clone(reduce_with(context, expected)?)
        && expected_name == name
    {
        let seeded = Term::struct_type(name, resolved.to_vec());
        expect(context, term, &seeded, expected)?;
    }
    Ok(())
}

/// The `..base` spread path of a struct literal: the base is elaborated once
/// and let-bound in a fresh frame, written overrides claim their declared
/// positions by label — an order-preserving subsequence of the field
/// telescope, so written order stays check order — explicit `use <term>`
/// fills pair with the concept's `use`-marked positions as in the plain path,
/// and every remaining position, plain and `use` alike, copies from the base
/// by positional projection (a superclass field is *copied*, not re-resolved).
///
/// The parameters are minted *inside* the frame: an omitted parameter's
/// metavariable may need to solve to a projection of the bound base (e.g.
/// `?A := b.A`), which is only in scope there. The result type is reduced
/// before the frame closes — the `elaborate_let` discipline — so occurrences
/// of the binder unfold to the base before escaping the rebuilt
/// `let b = base; Name { … }`, which downstream stages see as existing nodes.
fn elaborate_struct_spread(
    context: &mut Context,
    structure: &Structure,
    s: &Struct,
    term: &Term,
    mode: &Mode,
) -> Result<(Term, Term), Error> {
    let Struct {
        name,
        params,
        fields,
        entries,
    } = s;

    // The base must be a value of this very struct: positional projections
    // would happily copy from a structurally-matching tuple or a same-shaped
    // foreign struct otherwise. Its *parameters* may differ from the
    // literal's — the parameter-changing update — since every copied field is
    // checked against the new instantiated field type anyway.
    let (base, base_type) = elaborate(context, &fields[0], Mode::Infer)?;
    let base_type = reduce_with(context, &base_type)?;
    if !matches!(
        &*base_type,
        Subterm::StructType(StructType { name: base_name, .. }) if base_name == name
    ) {
        return Err(Error::spread_base_type_mismatch(
            name.clone(),
            base_type.clone(),
        ));
    }

    let label = context.fresh(Some("base"));

    let (rebuilt, result_type) = context.with_frame(|context| {
        context.define_assuming(&label, &base_type, &base);

        let resolved = resolve_struct_params(context, name, structure, params, term)?;
        seed_struct_expectation(context, name, &resolved, term, mode)?;

        let field_telescope = structure.fields_at(&resolved);

        let use_positions: Vec<usize> = match context.concept(name) {
            Some(concept) => concept.supers.iter().map(|(index, _)| *index).collect(),
            None => Vec::new(),
        };
        debug_assert!(use_positions.windows(2).all(|w| w[0] < w[1]));

        // Partition the overrides (everything after the spread). Positional
        // values are ambiguous across the spread's gaps, so every plain
        // override must be labeled.
        let mut plain: Vec<(&str, &Term)> = Vec::new();
        let mut fills: Vec<&Term> = Vec::new();
        for (entry, field) in entries[1..].iter().zip(&fields[1..]) {
            match entry {
                StructEntry::Field(Some(written)) => plain.push((written, field)),
                StructEntry::Field(None) => {
                    return Err(Error::unlabeled_spread_override(name.clone()));
                }
                StructEntry::Use => fills.push(field),
                StructEntry::Spread => unreachable!("spread multiplicity was validated"),
            }
        }

        if !fills.is_empty() && context.concept(name).is_none() {
            return Err(Error::use_entry_outside_concept(name.clone()));
        }

        if fills.len() > use_positions.len() {
            return Err(Error::too_many_use_entries(
                name.clone(),
                use_positions.len(),
                fills.len(),
            ));
        }

        let labels = field_telescope.labels();
        let plain_positions: Vec<(usize, &str)> = labels
            .iter()
            .enumerate()
            .filter(|(position, _)| !use_positions.contains(position))
            .map(|(position, label)| (position, *label))
            .collect();

        // Overrides claim declared positions by label, as an order-preserving
        // subsequence of the telescope: a label found ahead of the cursor
        // claims its position; found only behind, it is repeated or out of
        // order; found nowhere, it is unknown.
        let mut overrides: Vec<Option<&Term>> = vec![None; field_telescope.len()];
        let mut cursor = 0;
        for (written, field) in plain {
            let listed = || {
                plain_positions
                    .iter()
                    .filter(|(_, l)| !l.is_empty())
                    .map(|(_, l)| l.to_string())
                    .collect::<Vec<_>>()
            };
            match plain_positions[cursor..]
                .iter()
                .position(|(_, declared)| *declared == written)
            {
                Some(ahead) => {
                    let (position, _) = plain_positions[cursor + ahead];
                    overrides[position] = Some(field);
                    cursor += ahead + 1;
                }
                None if plain_positions[..cursor]
                    .iter()
                    .any(|(_, declared)| *declared == written) =>
                {
                    return Err(Error::spread_override_out_of_order(
                        name.clone(),
                        written.to_string(),
                        listed(),
                    ));
                }
                None => {
                    return Err(Error::unknown_struct_field(
                        name.clone(),
                        written.to_string(),
                        listed(),
                    ));
                }
            }
        }

        // Explicit `use` fills pair with the `use` positions in declaration
        // order (no skipping), exactly as in the plain path.
        let mut fill_values = fills.iter().copied();
        for position in &use_positions {
            match fill_values.next() {
                Some(fill) => overrides[*position] = Some(fill),
                None => break,
            }
        }

        // One value per declared position: the override where written, a
        // positional projection of the bound base everywhere else.
        let values: Vec<Term> = overrides
            .iter()
            .enumerate()
            .map(|(position, override_)| match override_ {
                Some(field) => (*field).clone(),
                None => Term::proj(Term::free_var(&label), position),
            })
            .collect();

        let sources: Vec<FieldSource> = values.iter().map(FieldSource::Written).collect();
        let mut elaborated = Vec::with_capacity(sources.len());
        check_dependent_fields(context, field_telescope, &sources, term, &mut elaborated)?;

        // Reduce inside the frame, where the binder is defined: occurrences of
        // it in the result type unfold to the base before escaping the `let`.
        let result_type = reduce_with(context, &Term::struct_type(name.clone(), resolved.clone()))?;

        Ok::<_, Error>((Term::struct_(name, resolved, elaborated), result_type))
    })?;

    Ok((Term::let_(label, base_type, base, rebuilt), result_type))
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
            elaborate(context, &tail.open(&[&Term::free_var(&label)]), mode)?;

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

/// Resolve a polymorphic numeric literal ([`NumLit`]) to a concrete scalar
/// primitive. In `Check` mode the expected type pins the choice; an expected
/// type that is still a bare unsolved metavar — and `Infer` mode — fall back to
/// the literal's shape default (`Int` when a sign was written, else `Nat`), and
/// the closing `expect` then solves that metavar to the chosen type. The literal
/// resolves *eagerly*: deferring it would strand downstream elaboration that
/// needs the type immediately (a projection off the literal's type, say). The
/// operator (`elaborate_infix`) pins its operand type from the non-literal side
/// first, so a literal there sees a concrete type and `1 + flt` still works.
/// Decimal literals never reach here; they parse straight to `Flt`.
fn elaborate_num_lit(
    context: &mut Context,
    num_lit: &NumLit,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let nat_type: Term = Subterm::Prim(Prim::NatType).into();
    let int_type: Term = Subterm::Prim(Prim::IntType).into();
    let flt_type: Term = Subterm::Prim(Prim::FltType).into();

    // A written sign rules out `Nat`, so the default lands on `Int`.
    let default_type: Term = if num_lit.signed {
        int_type.clone()
    } else {
        nat_type.clone()
    };

    let target = match &mode {
        Mode::Check(expected) => {
            let reduced = reduce_with(context, expected)?;
            match &*reduced {
                // Nothing concrete to resolve against yet — commit to the shape
                // default; the closing `expect` solves the metavar to it.
                Subterm::Metavar(Metavar { id, .. }) if context.metavar_solution(*id).is_none() => {
                    Term::unwrap_or_clone(default_type.clone())
                }
                _ => Term::unwrap_or_clone(reduced),
            }
        }
        Mode::Infer => Term::unwrap_or_clone(default_type.clone()),
    };

    let (prim, type_) = match &target {
        Subterm::Prim(Prim::NatType) if !num_lit.negative => {
            (Prim::Nat(Nat::new(num_lit.magnitude.clone())), nat_type)
        }
        Subterm::Prim(Prim::IntType) => {
            let magnitude = BigInt::from(num_lit.magnitude.clone());
            let value = if num_lit.negative {
                -magnitude
            } else {
                magnitude
            };
            (Prim::Int(Int::new(value)), int_type)
        }
        Subterm::Prim(Prim::FltType) => {
            let magnitude = num_lit.magnitude.to_f32().unwrap_or(f32::INFINITY);
            let value = if num_lit.negative {
                -magnitude
            } else {
                magnitude
            };
            (Prim::Flt(Flt::from_f32(value)), flt_type)
        }
        // A concrete expected type that is non-numeric — or `Nat` for a negative
        // literal — has no realization: report against the literal's own shape.
        _ => {
            let Mode::Check(expected) = &mode else {
                unreachable!("Infer-mode target is always the Nat/Int shape default");
            };
            let inferred = if num_lit.negative {
                int_type
            } else {
                default_type
            };
            return Err(Error::type_mismatch(inferred, expected.clone()));
        }
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::prim(prim), type_))
}

/// The shape default for an infix operator whose operand type nothing pinned:
/// any signed/negative literal operand forces `Int`, otherwise `Nat`.
fn infix_default_type(infix: &Infix) -> Prim {
    let signed = |operand: &Term| matches!(&**operand, Subterm::NumLit(num_lit) if num_lit.signed);

    if signed(&infix.left) || signed(&infix.right) {
        Prim::IntType
    } else {
        Prim::NatType
    }
}

/// Elaborate an infix operator ([`Infix`]) as a concept method call. A fresh
/// operand-type metavar `?T` is pinned by the non-literal operands first (or,
/// for arithmetic operators, by the expected result type), then defaulted from
/// the operand literals if nothing constrains it; only then are the literal
/// operands checked — against a `?T` that is already concrete, so they never
/// force it to their own default. That ordering is what lets `1 + flt` resolve
/// to `Flt` rather than a `Nat`/`Flt` mismatch.
///
/// Dispatch is then **one path**: every operator except `&&`/`||` desugars to
/// a projection of a witness of its `/syn` concept ([`operator_concept`]) —
/// `a + b` ≙ `Add/add(a, b)`, primitives included, resolved by the same
/// engine that fills `use` slots (so `no witness of Add(Point)` is the single
/// error vocabulary, and what an operator means at a type is entirely a
/// question of which witnesses exist). `&&`/`||` stay hardcoded on `Bln`:
/// short-circuit operators are control flow, not overloads. `!=` rebuilds as
/// `BlnXor(Eql/eql(a, b), true)` — no `BlnNot` prim exists. The node never
/// survives elaboration; witness projections over the statically-known
/// primitive witnesses collapse back to bare `Prim` code in the backend (see
/// the codegen parity tests).
fn elaborate_infix(
    context: &mut Context,
    infix: &Infix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let bln_type: Term = Subterm::Prim(Prim::BlnType).into();

    // `?T`: the operand type shared by both sides.
    let (operand_id, operand_type) = context.fresh_placeholder(Term::type_(), term.span());

    // An arithmetic operator returns its operand type, so an expected result
    // type pins `?T` straight away; a comparison returns `Bln`, which says
    // nothing about the operands, so only the operands can pin it.
    if !infix.op.result_is_bln()
        && let Mode::Check(expected) = &mode
    {
        expect(context, term, &operand_type, expected)?;
    }

    let left_is_literal = matches!(&*infix.left, Subterm::NumLit(_));
    let right_is_literal = matches!(&*infix.right, Subterm::NumLit(_));

    // Phase 1: the non-literal operands pin `?T` from their own types.
    let mut left = match left_is_literal {
        false => Some(elaborate(context, &infix.left, Mode::Check(operand_type.clone()))?.0),
        true => None,
    };
    let mut right = match right_is_literal {
        false => Some(elaborate(context, &infix.right, Mode::Check(operand_type.clone()))?.0),
        true => None,
    };

    // Nothing pinned `?T` — every non-literal operand left it open. Default from
    // the operand shapes so the literal operands have a concrete type to take.
    if context.metavar_solution(operand_id).is_none() {
        let default = infix_default_type(infix);
        context.solve_metavar(operand_id, Subterm::Prim(default).into());
    }

    // Phase 2: the literal operands resolve against the now-concrete `?T`.
    if left_is_literal {
        left = Some(elaborate(context, &infix.left, Mode::Check(operand_type.clone()))?.0);
    }
    if right_is_literal {
        right = Some(elaborate(context, &infix.right, Mode::Check(operand_type.clone()))?.0);
    }

    let left = left.unwrap();
    let right = right.unwrap();

    // `&&`/`||`: hardcoded to the `Bln` short-circuit primitives, the one
    // deliberate exception to concept dispatch.
    let Some((concept_name, field_name)) = operator_concept(infix.op) else {
        let head = Term::unwrap_or_clone(reduce_with(context, &operand_type)?);
        if !matches!(head, Subterm::Prim(Prim::BlnType)) {
            return Err(Error::operator_undefined(
                infix.op.symbol().to_string(),
                head,
            ));
        }
        let build = match infix.op {
            NumOp::And => Prim::BlnAnd,
            _ => Prim::BlnOr,
        };
        let rebuilt = Term::prim(build(left, right));
        if let Mode::Check(expected) = &mode {
            expect(context, term, &bln_type, expected)?;
        }
        return Ok((rebuilt, bln_type));
    };

    // The concept registry entry — absent only in an exotic embedding that
    // elaborates without the embedded prelude, where the operator has nothing to
    // dispatch through.
    let Some(concept) = context.concept(concept_name).cloned() else {
        let head = Term::unwrap_or_clone(reduce_with(context, &operand_type)?);
        return Err(Error::operator_undefined(
            infix.op.symbol().to_string(),
            head,
        ));
    };

    // Projection is positional over the *instantiated* field telescope
    // (`Structure::fields_at` peels the leading parameter binders, exactly as
    // `elaborate_proj` resolves a label), so the method's position among the
    // concept's fields is the index — no parameter offset.
    let projection_index = concept
        .fields
        .iter()
        .position(|field| field == field_name)
        .expect("the syn operator concepts declare their table fields");

    // Mint and attempt the witness goal exactly like an omitted `use`
    // argument: it resolves, parks on a flex operand type, or defers to a
    // later witness registration, and a definite miss reports
    // `no witness of Add(Point)` — the single operator error vocabulary.
    let goal = Term::struct_type(concept_name, vec![operand_type.clone()]);
    let provenance = WitnessOrigin {
        func: infix.op.symbol().to_string(),
        binder: field_name.to_string(),
    };
    let (slot, witness) =
        context.fresh_witness_metavar(goal.clone(), term.span(), provenance.clone());
    attempt_witness_goal(context, slot, &goal, provenance, term)?;

    let call = Term::apply(Term::proj(witness, projection_index), [left, right]);
    // No `BlnNot` prim exists; `!=` is the xor-negated equality.
    let rebuilt = match infix.op {
        NumOp::Neq => Term::prim(Prim::BlnXor(call, Term::prim(Prim::Bln(true)))),
        _ => call,
    };

    let result_type = if infix.op.result_is_bln() {
        bln_type
    } else {
        operand_type
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &result_type, expected)?;
    }

    Ok((rebuilt, result_type))
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
        plicities: &[Plicity],
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
                let x = Term::free_var(&name);
                // A binder the expected type marks `use` joins the witness
                // scope: resolution inside the body finds it there.
                match plicities.get(domains.len()) {
                    Some(Plicity::Witness) => context.assume_witness(&name, &type_),
                    _ => context.assume(&name, &type_),
                }
                domains.push((name, type_.clone()));
                walk(
                    context,
                    term,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    plicities,
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
            &ft.plicities,
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
                let x = Term::free_var(&name);
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

    let sources: Vec<FieldSource> = fields.iter().map(FieldSource::Written).collect();
    let mut elaborated = Vec::with_capacity(fields.len());
    check_dependent_fields(context, type_telescope, &sources, term, &mut elaborated)?;

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

/// Check `args` pointwise against the dependent telescope `signature` — each arg
/// under the earlier ones — collecting the rebuilt args and returning the
/// telescope's terminal, opened at those args. The caller checks arity first; the
/// arity error differs by site. The given-args counterpart to
/// `check_telescope_entries`.
fn check_args_against<B: Bound>(
    context: &mut Context,
    signature: Telescope<B>,
    args: &[Term],
) -> Result<(Vec<Term>, B), Error> {
    let mut elaborated = Vec::with_capacity(args.len());
    let terminal = signature.walk(args, |_, arg, ty| {
        elaborated.push(check(context, arg, ty.clone())?);
        Ok(())
    })?;
    Ok((elaborated, terminal))
}

/// Implicit-eta on the check turnaround. A reference whose type leads with an
/// implicit binder, checked against a concrete *explicit* function type, has its
/// leading implicits inserted as metavariables and is eta-expanded over the
/// remaining explicit binders — so a bare `Lst/concat` is accepted where
/// `(Lst B, Lst B) -> Lst B` is expected, instead of demanding
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
    if matches!(ift.plicities.first(), Some(Plicity::Explicit) | None) {
        return Ok((rebuilt, type_));
    }

    let expected_reduced = reduce_with(context, expected)?;
    let expected_explicit = matches!(
        &*expected_reduced,
        Subterm::FuncType(eft) if !matches!(
            eft.plicities.first(),
            Some(Plicity::Implicit) | Some(Plicity::Witness)
        )
    );
    if !expected_explicit {
        return Ok((rebuilt, type_));
    }

    let ift = ift.clone();
    let func_label = match &**term {
        Subterm::Var(var) => var.unwrap().to_string(),
        _ => "<function>".to_string(),
    };

    // Walk the head's telescope: implicit binders become fresh metavariables
    // (the inserted arguments), witness binders fresh metavariables with
    // resolution goals, explicit binders fresh lambda parameters (the eta
    // variables). `head_args` records all in telescope order so the body
    // re-applies the head fully saturated; `open` threads the dependent
    // substitution so a later binder mentioning an earlier one is instantiated.
    let mut head_args: Vec<(Plicity, Term)> = Vec::new();
    let mut binders: Vec<(String, Term)> = Vec::new();
    let output = context.with_frame(|context| {
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
                            rest.first_label(),
                            &func_label,
                            term,
                        )?;
                        tele = rest.open(&[&arg]);
                        head_args.push((plicity, arg));
                    }
                    Some(Plicity::Explicit) => {
                        let label = context.fresh(rest.first_label());
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

    // No explicit binders to eta over (an all-implicit curried prefix, e.g.
    // `(@A, @B) -> …`): the implicit-saturated application *is* the value, and its
    // type is the opened output. Erasure drops the implicit arguments anyway.
    if binders.is_empty() {
        return Ok((body, output));
    }

    let func_type = Term::func_type(binders.clone(), output);
    Ok((Term::func(binders, body), func_type))
}

pub(crate) fn elaborate(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
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
            Some(type_) => (term.clone(), type_.clone()),
            None => return Err(Error::unbound_variable(Term::var(var.clone()))),
        },
        Subterm::Func(func) => return elaborate_func(context, func, term, mode),
        Subterm::Tuple(tuple) => return elaborate_tuple(context, tuple, term, mode),
        Subterm::Infix(infix) => return elaborate_infix(context, infix, term, mode),
        Subterm::NumLit(num_lit) => return elaborate_num_lit(context, num_lit, term, mode),
        Subterm::Metavar(metavar) => return elaborate_metavar(context, metavar, term, mode),
        Subterm::InductiveType(ut) => elaborate_inductive_type(context, ut)?,
        Subterm::Variant(uc) => elaborate_variant(context, uc, term)?,
        Subterm::StructType(st) => elaborate_struct_type(context, st, term)?,
        Subterm::Struct(s) => elaborate_struct(context, s, term, &mode)?,
    };

    if let Mode::Check(expected) = &mode {
        let (rebuilt, type_) = insert_implicits_on_check(context, term, rebuilt, type_, expected)?;
        expect(context, term, &type_, expected)?;
        return Ok((rebuilt, type_));
    }

    Ok((rebuilt, type_))
}
