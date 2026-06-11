use {
    super::{
        Apply, Atom, Cases, Context, Error, Field, Func, Item, Let, Many, Match, Module,
        MotivePattern, MotiveSlot, Nat, Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple,
        TupleType, Two,
        UnionType, Var, Variant, erase_prim, expect_prim_head, infer, reduce_with, refine_head,
    },
    crate::ersd,
    std::collections::BTreeMap,
};

fn erase_func(
    context: &mut Context,
    func: &Func,
    _term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Func { telescope } = func;

    let ft = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::FuncType(ft) => ft,
        // Elaborate already checked this function against a function type (§9).
        _ => unreachable!("erase: function checked against non-function type"),
    };

    // Walk the lambda's telescope (whose `Done` is the body) alongside the
    // checked function type's telescope (whose `Done` is the output type),
    // generating a fresh name per parameter and recording the candidate flag
    // from each expected domain. The lambda's own domains are erased away.
    fn walk(
        context: &mut Context,
        body: Telescope<Term>,
        type_: Telescope<Term>,
        names: &mut Vec<String>,
        candidates: &mut Vec<bool>,
    ) -> Result<(Term, Term), Error> {
        match (body, type_) {
            (Telescope::Done(body), Telescope::Done(output)) => Ok((*body, *output)),
            (Telescope::Cons(_domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                // The flag is read before the binder is assumed: a parameter's type
                // never depends on the parameter itself.
                candidates.push(is_candidate(context, &type_)?);
                let name = context.fresh(body_rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &type_);
                names.push(name);
                walk(
                    context,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    names,
                    candidates,
                )
            }
            _ => unreachable!("erase: function/type telescope arity mismatch"),
        }
    }

    let mut param_names = Vec::new();
    let mut candidates = Vec::new();
    let (erased_body, captures) = context.with_frame(|context| {
        let (body_opened, output_type) = walk(
            context,
            telescope.clone(),
            ft.telescope,
            &mut param_names,
            &mut candidates,
        )?;

        // Captures are the body's free variables other than the lambda's own
        // parameters (which appear as fresh frees once the body is opened). The
        // candidate flag rides from here — the last point a binder's type is
        // known — down to `cont`, where the optimizer specializes function-typed
        // arguments.
        let captures = body_opened
            .free_vars()
            .into_iter()
            .filter(|name| !param_names.contains(name))
            .map(|name| {
                let type_ = infer(context, &Term::var(Var::free(&name)))?;
                let candidate = is_candidate(context, &type_)?;
                Ok(ersd::Argument { name, candidate })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let erased_body = erase(context, &body_opened, &output_type)?;
        Ok::<_, Error>((erased_body, captures))
    })?;

    let params = param_names
        .into_iter()
        .zip(candidates)
        .map(|(name, candidate)| ersd::Argument { name, candidate })
        .collect();

    Ok(ersd::Subterm::Func(ersd::Func {
        captures,
        params,
        body: erased_body,
    })
    .into())
}

/// Whether an argument of type `type_` is a specialization candidate, after
/// reduction. Three erased-to-trivial shapes qualify, each a compile-time constant
/// the specializer can bake in:
///
/// - a **function type** — a first-class closure value, devirtualizable;
/// - **`Type`** — an erased type argument (a unit at runtime);
/// - the **empty tuple type `{}`** — an erased unit argument.
///
/// Reduction matters: an aliased or computed type only exposes its head in
/// weak-head normal form.
fn is_candidate(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    Ok(match &*reduce_with(context, type_)? {
        Subterm::FuncType(_) | Subterm::Type => true,
        Subterm::TupleType(tuple_type) => tuple_type.telescope.is_empty(),
        _ => false,
    })
}

fn erase_apply(
    context: &mut Context,
    apply: &Apply,
    _term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let Apply { head, params, .. } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    // Elaborate already checked the head is a function applied to the right
    // number of arguments (§9); here the shape is re-derived only to lower.
    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft,
        _ => unreachable!("erase: applied a non-function"),
    };

    assert_eq!(
        params.len(),
        ft.telescope.len(),
        "erase: application arity disagrees with the function type",
    );

    let mut erased_params = Vec::with_capacity(params.len());
    ft.telescope.clone().walk(params, |arg, ty| {
        erased_params.push(erase(context, arg, ty)?);
        Ok(())
    })?;
    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::Apply(ersd::Apply {
        head: erased_head,
        params: erased_params,
    })
    .into())
}

fn erase_tuple(context: &mut Context, tuple: &Tuple, expected: &Term) -> Result<ersd::Term, Error> {
    let Tuple { fields, .. } = tuple;

    // Elaborate already checked this tuple against a tuple type of matching
    // arity (§9); the telescope is re-derived here only to lower the fields.
    let type_telescope = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::TupleType(TupleType { telescope }) => telescope,
        _ => unreachable!("erase: tuple checked against non-tuple type"),
    };

    assert_eq!(
        fields.len(),
        type_telescope.len(),
        "erase: tuple width disagrees with the tuple type",
    );

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        erased: &mut Vec<ersd::Term>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let head = &fields[0];
                erased.push(erase(context, head, &ty)?);
                walk(context, rest.open(&[head]), &fields[1..], erased)
            }
        }
    }

    let mut erased_fields = Vec::<ersd::Term>::new();
    walk(context, type_telescope, fields, &mut erased_fields)?;

    Ok(ersd::Subterm::Tuple(ersd::Tuple {
        fields: erased_fields,
    })
    .into())
}

fn erase_nat_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, term, Prim::NatType)?;

    let erased_zero_case = erase(
        context,
        zero_case,
        &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let erased_succ_case = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&pred_label))]),
        );

        erase(
            context,
            &succ_case.open(&[
                &Term::var(Var::free(&pred_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            &motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::var(Var::free(&pred_label)),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::NatMatch(ersd::NatMatch::Induction {
        head: erased_head,
        zero_case: erased_zero_case,
        pred: pred_label,
        ih: ih_label,
        succ_case: erased_succ_case,
    })
    .into())
}

fn erase_switch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, term, Prim::NatType)?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let case_expected = motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]);
            context.with_frame(|context| {
                refine_head(
                    context,
                    head,
                    &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
                );
                erase(context, body, &case_expected).map(|e| (*n, e))
            })
        })
        .collect::<Result<BTreeMap<_, _>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head]))?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head,
        cases: erased_cases,
        default: erased_default,
    })
    .into())
}

fn erase_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    match cases {
        Cases::Bln {
            false_case,
            true_case,
        } => erase_bln_match(context, head, motive, false_case, true_case, term, expected),
        Cases::Nat {
            zero_case,
            succ_case,
        } => erase_nat_match(context, head, motive, zero_case, succ_case, term, expected),
        Cases::Switch { cases, default } => {
            erase_switch(context, head, motive, cases, default, term, expected)
        }
        Cases::Union { cases, pattern } => {
            erase_union_match(context, head, motive, cases, pattern.as_ref(), expected)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn erase_bln_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, term, Prim::BlnType)?;

    let erased_false = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into());
        erase(
            context,
            false_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let erased_true = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into());
        erase(
            context,
            true_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head,
        cases: BTreeMap::from([(0, erased_false)]),
        default: erased_true,
    })
    .into())
}

fn erase_proj(
    context: &mut Context,
    proj: &Proj,
    _term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let Proj { head, field } = proj;
    // Labels are resolved by elaborate; erase runs strictly downstream.
    let Field::Index(index) = field else {
        unreachable!("unresolved label projection reached erase");
    };

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    // Elaborate already checked the head is a tuple and the index is in range
    // (§9); the type is re-derived here only to lower the head. (The
    // projection-through-a-stuck-union-payload workaround that used to live
    // here — `projectable_at` — died with the tagged-tuple encoding: a union
    // payload is no longer reached by projecting a structural pair, so a
    // projection's head type is always a `TupleType` again.)
    match &*head_type {
        Subterm::TupleType(TupleType { telescope }) => {
            assert!(*index < telescope.len(), "erase: projected a non-tuple");
        }
        _ => unreachable!("erase: projected a non-tuple"),
    }

    Ok(ersd::Subterm::Proj(ersd::Proj {
        head: erase(context, head, &head_type)?,
        index: *index,
    })
    .into())
}

/// Lower a primitive constructor value to its flat sum-of-products runtime
/// representation: a single allocation `(tag_index, payload...)` with the
/// payload inlined after the tag. The tag's runtime
/// index is the constructor's position in sorted (registry key) order.
fn erase_variant(
    context: &mut Context,
    uc: &Variant,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let Variant {
        name,
        params,
        tag,
        payload,
    } = uc;

    let inductive = context
        .inductive(name)
        .cloned()
        .expect("erase: constructor names a registered inductive");
    let index = inductive
        .tag_index(tag)
        .expect("erase: constructor tag registered with its inductive");
    let mut telescope = inductive
        .instantiate(tag, params)
        .expect("erase: constructor instantiates at its inductive's parameters");

    // Erase the payload against the constructor telescope's (dependent) types,
    // inline after the tag.
    let mut fields = Vec::with_capacity(payload.len() + 1);
    fields.push(ersd::Subterm::Atom(ersd::Atom { index }).into());
    for value in payload {
        match telescope {
            Telescope::Cons(ty, rest) => {
                fields.push(erase(context, value, &ty)?);
                telescope = rest.open(&[value]);
            }
            Telescope::Done(_) => unreachable!("erase: constructor arity checked by elaborate"),
        }
    }

    Ok(ersd::Subterm::Tuple(ersd::Tuple { fields }).into())
}

/// Lower the primitive eliminator: an index dispatch on the scrutinee's tag
/// (field 0), each arm rebinding its payload binders to the flat record's
/// remaining fields (`head.(i + 1)`). Downstream stages
/// (`cont`/`optm`/`wasm`) see only generic tuples, projections, and an
/// index-dispatched match.
fn erase_union_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    pattern: Option<&MotivePattern>,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let (name, params, actual_indices) = match &*head_type {
        Subterm::UnionType(UnionType {
            name,
            params,
            indices,
        }) => (name.clone(), params.clone(), indices.clone()),
        _ => unreachable!("erase: union match scrutinee checked by elaborate"),
    };

    let inductive = context
        .inductive(&name)
        .cloned()
        .expect("erase: scrutinee type names a registered inductive");

    let scrutinee_label = context.fresh(Some("scrutinee"));

    // The pattern's binder slots, positionally (validated by elaborate):
    // `true` marks a parameter position (opened with the actual parameter),
    // `false` an index position (opened with the case's target index).
    let binder_slots: Vec<(bool, usize)> = pattern
        .map(|p| {
            let n_params = inductive.params.len();
            p.slots
                .iter()
                .enumerate()
                .filter_map(|(position, slot)| match slot {
                    MotiveSlot::Binder if position < n_params => Some((true, position)),
                    MotiveSlot::Binder => Some((false, position - n_params)),
                    MotiveSlot::Term(_) => None,
                })
                .collect()
        })
        .unwrap_or_default();

    let cases_erased = inductive
        .constructors
        .keys()
        .map(|tag| {
            // A tag with no arm was pruned by elaborate (Rung C verified the
            // case impossible at the scrutinee's indices). Its dispatch slot
            // still exists positionally, but reaching it is a compiler bug or
            // corrupted runtime tag, so lower it to a real trap.
            let Some(scope) = cases.get(tag) else {
                return Ok(ersd::Subterm::Unreachable.into());
            };
            let telescope = inductive
                .instantiate(tag, &params)
                .expect("erase: constructor instantiates at its inductive's parameters");

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

            context.with_frame(|context| {
                let mut telescope = telescope;
                for (label, var) in labels.iter().zip(&vars) {
                    match telescope {
                        Telescope::Cons(ty, rest) => {
                            context.assume(label, &ty);
                            telescope = rest.open(&[var]);
                        }
                        Telescope::Done(_) => {
                            unreachable!("erase: constructor arity checked by elaborate")
                        }
                    }
                }

                // This case's target indices, for opening a pattern motive.
                let ix_c = match &telescope {
                    Telescope::Done(terminal) => match &***terminal {
                        Subterm::UnionType(UnionType { indices, .. }) => indices.clone(),
                        _ => unreachable!("erase: constructor terminal is its union type"),
                    },
                    Telescope::Cons(..) => {
                        unreachable!("erase: constructor arity checked by elaborate")
                    }
                };

                let ctor_val =
                    Term::variant(name.clone(), params.clone(), tag.clone(), vars.clone());
                refine_head(context, head, &ctor_val);

                // Rung B, mirrored from elaborate: key-shaped scrutinee
                // indices reduce to the case's targets inside the arm, so
                // types erased here converge the same way they checked.
                for (actual, target) in actual_indices.iter().zip(&ix_c) {
                    refine_head(context, actual, target);
                }

                let arm_args = binder_slots
                    .iter()
                    .map(|&(is_param, i)| match is_param {
                        true => params[i].clone(),
                        false => ix_c[i].clone(),
                    })
                    .collect::<Vec<_>>();
                let arm_refs = arm_args.iter().chain([&ctor_val]).collect::<Vec<_>>();
                let expected = motive.open(&arm_refs);
                let var_refs = vars.iter().collect::<Vec<_>>();
                let body = erase(context, &scope.open(&var_refs), &expected)?;

                // Bind each payload binder to its flat-record slot:
                // `let x_i = scrutinee.(i + 1); …` (innermost-last, so fold in
                // reverse). Projections read the let-bound scrutinee — never a
                // re-erased copy of the head term, which would re-execute an
                // effectful scrutinee once per arm.
                labels
                    .iter()
                    .enumerate()
                    .rev()
                    .try_fold(body, |tail, (i, label)| {
                        Ok(ersd::Subterm::Let(ersd::Let {
                            name: label.clone(),
                            body: ersd::Subterm::Proj(ersd::Proj {
                                head: ersd::Subterm::Name(ersd::Name::from(
                                    scrutinee_label.as_str(),
                                ))
                                .into(),
                                index: i + 1,
                            })
                            .into(),
                            tail,
                        })
                        .into())
                    })
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    // The head term is erased (and thus evaluated) exactly once, shared by
    // the tag dispatch and every arm's payload projections.
    Ok(ersd::Subterm::Let(ersd::Let {
        name: scrutinee_label.clone(),
        body: erase(context, head, &head_type)?,
        tail: ersd::Subterm::Match(ersd::Match {
            head: ersd::Subterm::Proj(ersd::Proj {
                head: ersd::Subterm::Name(ersd::Name::from(scrutinee_label.as_str())).into(),
                index: 0,
            })
            .into(),
            cases: cases_erased,
        })
        .into(),
    })
    .into())
}

fn erase_let(context: &mut Context, let_: &Let, expected: &Term) -> Result<ersd::Term, Error> {
    let Let {
        type_: body_type,
        body,
        tail,
    } = let_;

    let name = context.fresh(tail.first_label());
    let erased_body = erase(context, body, body_type)?;
    let var_term = Term::var(Var::free(&name));
    let tail = tail.open(&[&var_term]);

    let tail = context.with_frame(|context| {
        context.define_assuming(&name, body_type, body);

        erase(context, &tail, expected)
    })?;

    Ok(ersd::Subterm::Let(ersd::Let {
        name,
        body: erased_body,
        tail,
    })
    .into())
}

fn erase_rec(context: &mut Context, rec: &Rec, expected: &Term) -> Result<ersd::Term, Error> {
    let Rec { items, tail } = rec;

    let names = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = names
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

    let erased = context.with_frame(|context| {
        for (name, (type_, _)) in names.iter().zip(items.iter()) {
            context.assume(name, type_);
        }

        for (name, (_, body)) in names.iter().zip(items.iter()) {
            context.define(name, body);
        }

        let erased_items = items
            .iter()
            .map(|(type_, body)| erase(context, body, type_))
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(ersd::Rec {
            names,
            items: erased_items,
            tail: erase(context, &tail, expected)?,
        })
    })?;

    Ok(ersd::Subterm::Rec(erased).into())
}

/// Erase a whole meta-free [`Module`] to an [`ersd::Module`] (§9). Each top-level
/// item is erased and `define`d *cumulatively in the persistent base frame* (no
/// `with_frame`), so later items, the entrypoint body, and the type annotations
/// all reduce through the accumulated definitions; then the entrypoint `body` is
/// erased against `expected`. The flat analogue of `erase_let`/`erase_rec`, minus
/// the de Bruijn open/close — top-level cross-references are already free `Var`s,
/// which erase to `ersd::Name`.
pub fn erase_module(
    context: &mut Context,
    module: &Module,
    expected: &Term,
) -> Result<ersd::Module, Error> {
    // Erase runs with its own `Context` (see `run::compile`); seed its
    // inductive registry from the module before any item consults it.
    for (name, inductive) in &module.inductives {
        context.register_inductive(name, inductive.clone());
    }

    let mut items = Vec::with_capacity(module.items.len());

    for item in &module.items {
        match item {
            Item::Let(def) => {
                let body = erase(context, &def.body, &def.type_)?;
                context.define_assuming(&def.name, &def.type_, &def.body);

                items.push(ersd::Item::Let {
                    name: def.name.clone(),
                    body,
                });
            }
            Item::Rec(defs) => {
                for def in defs {
                    context.assume(&def.name, &def.type_);
                }

                for def in defs {
                    context.define(&def.name, &def.body);
                }

                let names: Vec<String> = defs.iter().map(|def| def.name.clone()).collect();

                let erased = defs
                    .iter()
                    .map(|def| erase(context, &def.body, &def.type_))
                    .collect::<Result<Vec<_>, Error>>()?;

                items.push(ersd::Item::Rec {
                    names,
                    items: erased,
                });
            }
        }
    }

    let body = erase(context, &module.body, expected)?;

    Ok(ersd::Module { items, body })
}

pub fn erase(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    // Attach this term's span to *any* error from erasing it. The dispatch lives
    // in `erase_subterm` so that its `?` short-circuits (e.g. a conversion
    // mismatch from `expect`) still flow through this wrapper rather than
    // escaping `erase` unspanned.
    let result = erase_subterm(context, term, expected);

    match term.span() {
        Some(span) => result.map_err(|error| error.at(span)),
        None => result,
    }
}

fn erase_subterm(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    match &**term {
        Subterm::Prim(prim) => erase_prim(context, term, prim, expected),
        Subterm::Match(m) => erase_match(context, m, term, expected),
        // Type formers all erase to a runtime unit; they carry nothing to lower
        // and were already checked by `elaborate`.
        Subterm::Type | Subterm::FuncType(_) | Subterm::TupleType(_) | Subterm::UnionType(_) => {
            Ok(ersd::Subterm::Erased.into())
        }
        Subterm::Variant(uc) => erase_variant(context, uc, expected),
        Subterm::Func(func) => erase_func(context, func, term, expected),
        Subterm::Apply(apply) => erase_apply(context, apply, term, expected),
        Subterm::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Subterm::Proj(proj) => erase_proj(context, proj, term, expected),
        Subterm::Let(let_) => erase_let(context, let_, expected),
        Subterm::Rec(lr) => erase_rec(context, lr, expected),
        Subterm::Var(var) => Ok(ersd::Subterm::Name(ersd::Name::from(var.unwrap())).into()),
        // Erase runs downstream of zonking, on a meta-free term (§9).
        Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erase"),
    }
}
